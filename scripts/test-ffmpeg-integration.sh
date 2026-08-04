#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

script_directory="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd -- "${script_directory}/.." && pwd)"
ffmpeg_binary="${FFMPEG_BINARY:-$(command -v ffmpeg || true)}"
ffprobe_binary="${FFPROBE_BINARY:-$(command -v ffprobe || true)}"
perl_binary="${PERL_BINARY:-$(command -v perl || true)}"
bridge_binary="${BRIDGE_BINARY:-${project_root}/build/ts-codec-bridge.elf}"
tsreadex_binary="${TSREADEX_BINARY:-}"
av1_elementary_verifier="${project_root}/scripts/verify-av1-ts-elementary-stream.sh"
temporary_directory="$(mktemp -d /tmp/tscodecbridge-ffmpeg.XXXXXX)"
keep_artifacts="${KEEP_FFMPEG_ARTIFACTS:-0}"
matrix_case_count=0
stream_anchor_success_case_count=0
declare -A matrix_cases=()

cleanup() {
    if [[ "${keep_artifacts}" == '1' ]]; then
        printf 'Retained FFmpeg integration artifacts: %s\n' \
            "${temporary_directory}" >&2
    else
        rm -rf -- "${temporary_directory}"
    fi
}
trap cleanup EXIT

if [[ "${keep_artifacts}" != '0' && "${keep_artifacts}" != '1' ]]; then
    printf 'KEEP_FFMPEG_ARTIFACTS must be 0 or 1: %s\n' \
        "${keep_artifacts}" >&2
    exit 2
fi

test -n "${ffmpeg_binary}"
test -n "${ffprobe_binary}"
test -n "${perl_binary}"
test -x "${ffmpeg_binary}"
test -x "${ffprobe_binary}"
test -x "${perl_binary}"
test -x "${bridge_binary}"
test -x "${av1_elementary_verifier}"
if [[ -n "${tsreadex_binary}" ]]; then
    test -x "${tsreadex_binary}"
fi

"${ffmpeg_binary}" -version >/dev/null
"${ffprobe_binary}" -version >/dev/null
test "$("${bridge_binary}" --version)" = '0.1.0'
test "$("${bridge_binary}" --mapping-version)" = '1'

assert_audio_streams() {
    local transport_stream="$1"
    local expected_codec="$2"
    local observed

    observed="$(
        "${ffprobe_binary}" \
            -v error \
            -select_streams a \
            -show_entries stream=index,codec_name \
            -of csv=p=0 \
            "${transport_stream}" |
            sed '/^$/d' |
            sort -u
    )"
    test "${observed}" = \
        "$(printf '1,%s\n2,%s' "${expected_codec}" "${expected_codec}")"
}

assert_video_stream() {
    local transport_stream="$1"
    local codec="$2"
    local observed

    case "${codec}" in
        avc)
            observed="$(
                "${ffprobe_binary}" \
                    -v error \
                    -select_streams v:0 \
                    -show_entries stream=codec_name \
                    -of csv=p=0 \
                    "${transport_stream}" |
                    sed '/^$/d' |
                    sort -u
            )"
            test "${observed}" = 'h264'
            ;;
        hevc)
            observed="$(
                "${ffprobe_binary}" \
                    -v error \
                    -select_streams v:0 \
                    -show_entries stream=codec_name \
                    -of csv=p=0 \
                    "${transport_stream}" |
                    sed '/^$/d' |
                    sort -u
            )"
            test "${observed}" = 'hevc'
            ;;
        vp9 | av1)
            local expected_tag
            if [[ "${codec}" == 'vp9' ]]; then
                expected_tag='VP09'
            else
                expected_tag='AV01'
            fi
            observed="$(
                "${ffprobe_binary}" \
                    -v error \
                    -select_streams d \
                    -show_entries stream=index,codec_tag_string \
                    -of csv=p=0 \
                    "${transport_stream}" |
                    sed '/^$/d' |
                    sort -u
            )"
            test "${observed}" = "0,${expected_tag}"
            ;;
        *)
            printf 'Unknown video codec in probe assertion: %s\n' \
                "${codec}" >&2
            return 1
            ;;
    esac
}

assert_video_frame_rate() {
    local transport_stream="$1"
    local expected_frame_rate="$2"
    local observed

    observed="$(
        "${ffprobe_binary}" \
            -v error \
            -select_streams v:0 \
            -show_entries stream=avg_frame_rate \
            -of default=noprint_wrappers=1:nokey=1 \
            "${transport_stream}" |
            sed '/^$/d' |
            sort -u
    )"
    test "${observed}" = "${expected_frame_rate}"
}

assert_fixed_muxrate_and_pcr_period() {
    local case_name="$1"
    local transport_stream="$2"
    local expected_muxrate_kbps="$3"
    local pcr_period_milliseconds="$4"

    "${perl_binary}" - \
        "${transport_stream}" \
        "${expected_muxrate_kbps}" \
        "${pcr_period_milliseconds}" <<'PERL'
use strict;
use warnings;

my ($path, $expected_muxrate_kbps, $pcr_period_milliseconds) = @ARGV;
die "expected muxrate is not a positive integer\n"
    unless defined($expected_muxrate_kbps) && $expected_muxrate_kbps =~ /\A[1-9][0-9]*\z/;
die "PCR period is not a positive integer\n"
    unless defined($pcr_period_milliseconds) && $pcr_period_milliseconds =~ /\A[1-9][0-9]*\z/;

my $packet_size = 188;
my $packet_bits = $packet_size * 8;
my $pcr_ticks_per_second = 27_000_000;
my $expected_muxrate_bps = $expected_muxrate_kbps * 1000;
open my $stream, q{<}, $path or die "$path: $!\n";
binmode $stream;

my @pcr_samples;
my $packet_index = 0;
while (1) {
    my $read_count = read($stream, my $packet, $packet_size);
    die "$path: $!\n" unless defined $read_count;
    last if $read_count == 0;
    die "$path: trailing TS packet bytes=$read_count\n"
        unless $read_count == $packet_size;
    die "$path: TS sync lost at packet=$packet_index\n"
        unless ord(substr($packet, 0, 1)) == 0x47;

    my $adaptation_field_control = ord(substr($packet, 3, 1)) & 0x30;
    if ($adaptation_field_control == 0x20 || $adaptation_field_control == 0x30) {
        my $adaptation_field_length = ord(substr($packet, 4, 1));
        if ($adaptation_field_length >= 1) {
            my $adaptation_flags = ord(substr($packet, 5, 1));
            if ($adaptation_flags & 0x10) {
                die "$path: PCR is truncated at packet=$packet_index\n"
                    if $adaptation_field_length < 7;
                my @bytes = map { ord($_) } split //, substr($packet, 6, 6);
                my $pcr_base = (((($bytes[0] << 8) | $bytes[1]) << 8 | $bytes[2]) << 8 | $bytes[3]);
                $pcr_base = ($pcr_base << 1) | ($bytes[4] >> 7);
                my $pcr_extension = (($bytes[4] & 1) << 8) | $bytes[5];
                my $pcr = $pcr_base * 300 + $pcr_extension;
                my $pid = ((ord(substr($packet, 1, 1)) & 0x1f) << 8)
                    | ord(substr($packet, 2, 1));
                push @pcr_samples, [$packet_index, $pid, $pcr];
            }
        }
    }
    ++$packet_index;
}

die "$path: fewer than two PCR values\n" unless @pcr_samples >= 2;
my $pcr_pid = $pcr_samples[0][1];
my $maximum_interval_ticks = 0;
my $maximum_muxrate_residual = 0;
for my $index (1 .. $#pcr_samples) {
    my ($previous_packet, $previous_pid, $previous_pcr) = @{$pcr_samples[$index - 1]};
    my ($current_packet, $current_pid, $current_pcr) = @{$pcr_samples[$index]};
    die "$path: PCR PID changed from $previous_pid to $current_pid\n"
        unless $current_pid == $previous_pid;
    my $pcr_delta = $current_pcr - $previous_pcr;
    die "$path: PCR did not advance at packet=$current_packet\n"
        unless $pcr_delta > 0;
    $maximum_interval_ticks = $pcr_delta
        if $pcr_delta > $maximum_interval_ticks;

    my $packet_delta = $current_packet - $previous_packet;
    my $ideal_product = $packet_delta * $packet_bits * $pcr_ticks_per_second;
    my $actual_product = $pcr_delta * $expected_muxrate_bps;
    my $residual = abs($actual_product - $ideal_product);
    $maximum_muxrate_residual = $residual
        if $residual > $maximum_muxrate_residual;
}

# PCR clock is derived from CBR packet positions.  The 20ms retransmission
# target may be scheduled after up to three complete 188-byte TS slots; this
# is the bounded mux quantization, not a relaxation of the Anchor 50ms limit.
my $target_interval_ticks = $pcr_period_milliseconds * 27_000;
my $three_packet_tolerance_ticks = int(
    (3 * $packet_bits * $pcr_ticks_per_second + $expected_muxrate_bps - 1)
    / $expected_muxrate_bps
);
my $maximum_allowed_interval_ticks = $target_interval_ticks
    + $three_packet_tolerance_ticks;
die sprintf(
    "%s: PCR interval ticks=%d exceeds target=%d plus three-packet CBR quantization=%d\n",
    $path,
    $maximum_interval_ticks,
    $target_interval_ticks,
    $three_packet_tolerance_ticks,
)
    if $maximum_interval_ticks > $maximum_allowed_interval_ticks;
die sprintf(
    "%s: fixed muxrate residual=%d exceeds one PCR tick at rate=%d\n",
    $path,
    $maximum_muxrate_residual,
    $expected_muxrate_bps,
)
    if $maximum_muxrate_residual > $expected_muxrate_bps;

printf(
    "Verified CBR/PCR: pcr_pid=0x%04X samples=%d muxrate_kbps=%d target_ms=%d max_interval_ticks=%d max_interval_ms=%.6f quantization_limit_ticks=%d\n",
    $pcr_pid,
    scalar(@pcr_samples),
    $expected_muxrate_kbps,
    $pcr_period_milliseconds,
    $maximum_interval_ticks,
    $maximum_interval_ticks / 27_000,
    $maximum_allowed_interval_ticks,
);
PERL
    printf 'Verified fixed muxrate/PCR contract: case=%s muxrate_kbps=%s pcr_period_ms=%s.\n' \
        "${case_name}" \
        "${expected_muxrate_kbps}" \
        "${pcr_period_milliseconds}"
}

assert_stream_anchor_ffmpeg_boundary() {
    local case_name="$1"
    local transport_stream="$2"
    local anchor_stderr="${temporary_directory}/${case_name}-stream-anchor.stderr"

    if "${ffmpeg_binary}" -hide_banner -encoders 2>/dev/null |
        awk '$2 == "timed_id3" { found = 1 } END { exit(found ? 0 : 1); }'
    then
        printf '%s\n' \
            'FFmpeg now exposes a timed_id3 encoder; add a safe real Stream Anchor source before accepting this integration test.' >&2
        return 1
    fi

    if "${bridge_binary}" \
        --stream-anchor-v1 \
        < "${transport_stream}" \
        > /dev/null \
        2> "${anchor_stderr}"
    then
        printf 'Stream Anchor unexpectedly accepted FFmpeg input without timed-ID3: case=%s\n' \
            "${case_name}" >&2
        return 1
    fi
    grep -Fq 'STREAM_ANCHOR_TIMED_ID3_STREAM_MISSING' "${anchor_stderr}"
    printf '%s\n' \
        'Stream Anchor boundary: FFmpeg has no timed_id3 encoder, so real HEVC/AAC input is verified to fail closed without the source marker; successful 24-byte PRIV owner/payload finalization remains covered by synthetic stream-anchor tests.'
}

assert_stream_anchor_id3_transition() {
    local case_name="$1"
    local encoded_stream="$2"
    local finalized_stream="$3"
    local expected_generation_id="$4"
    local encoded_id3="${temporary_directory}/${case_name}-encoded.id3"
    local finalized_id3="${temporary_directory}/${case_name}-finalized.id3"

    "${ffmpeg_binary}" -hide_banner -loglevel error -y \
        -i "${encoded_stream}" -map 0:d:0 -c copy -f data "${encoded_id3}"
    "${ffmpeg_binary}" -hide_banner -loglevel error -y \
        -i "${finalized_stream}" -map 0:d:0 -c copy -f data "${finalized_id3}"

    grep -aFq 'com.konomitv-bs4k.stream-anchor-source.v1' "${encoded_id3}"
    if grep -aFq 'com.konomitv-bs4k.stream-anchor.v1' "${encoded_id3}"; then
        printf 'Final Stream Anchor owner existed before Bridge: case=%s\n' "${case_name}" >&2
        return 1
    fi
    grep -aFq 'com.konomitv-bs4k.stream-anchor.v1' "${finalized_id3}"
    if grep -aFq 'com.konomitv-bs4k.stream-anchor-source.v1' "${finalized_id3}"; then
        printf 'Source Stream Anchor owner remained after Bridge: case=%s\n' "${case_name}" >&2
        return 1
    fi

    "${perl_binary}" - "${finalized_id3}" "${expected_generation_id}" <<'PERL'
use strict;
use warnings;

my ($path, $expected_generation) = @ARGV;
open my $stream, q{<}, $path or die "$path: $!\n";
binmode $stream;
local $/;
my $data = <$stream>;
my $position = 0;
my $anchor_count = 0;
my $expected_sequence = 0;
my $previous_source_time;

sub syncsafe_u32 {
    my ($octets) = @_;
    my @bytes = unpack('C4', $octets);
    die "$path: invalid synchsafe integer\n" if grep { $_ & 0x80 } @bytes;
    return (($bytes[0] << 21) | ($bytes[1] << 14) | ($bytes[2] << 7) | $bytes[3]);
}

sub network_u64 {
    my ($octets) = @_;
    my ($high, $low) = unpack('NN', $octets);
    return $high * 4294967296 + $low;
}

while ($position < length($data)) {
    die "$path: truncated ID3 header\n" if length($data) - $position < 10;
    die "$path: invalid ID3 signature\n" unless substr($data, $position, 3) eq 'ID3';
    my $tag_size = syncsafe_u32(substr($data, $position + 6, 4));
    my $tag_end = $position + 10 + $tag_size;
    die "$path: truncated ID3 tag\n" if $tag_end > length($data);
    my $frame_position = $position + 10;
    while ($frame_position < $tag_end) {
        last if substr($data, $frame_position, 1) eq "\0";
        die "$path: truncated ID3 frame\n" if $tag_end - $frame_position < 10;
        my $frame_id = substr($data, $frame_position, 4);
        my $frame_size = syncsafe_u32(substr($data, $frame_position + 4, 4));
        my $payload_start = $frame_position + 10;
        my $frame_end = $payload_start + $frame_size;
        die "$path: ID3 frame exceeds tag\n" if $frame_end > $tag_end;
        if ($frame_id eq 'PRIV') {
            my $payload = substr($data, $payload_start, $frame_size);
            my $owner_end = index($payload, "\0");
            die "$path: PRIV owner is unterminated\n" if $owner_end < 0;
            my $owner = substr($payload, 0, $owner_end);
            if ($owner eq 'com.konomitv-bs4k.stream-anchor.v1') {
                my $anchor_payload = substr($payload, $owner_end + 1);
                die "$path: Stream Anchor payload is not 24 bytes\n"
                    unless length($anchor_payload) == 24;
                my @prefix = unpack('C4', substr($anchor_payload, 0, 4));
                die "$path: Stream Anchor version/flags/reserved are invalid\n"
                    unless join(',', @prefix) eq '1,0,0,0';
                my $generation = network_u64(substr($anchor_payload, 4, 8));
                my $sequence = unpack('N', substr($anchor_payload, 12, 4));
                my $source_time = network_u64(substr($anchor_payload, 16, 8));
                die "$path: generation mismatch actual=$generation expected=$expected_generation\n"
                    unless $generation == $expected_generation;
                die "$path: sequence mismatch actual=$sequence expected=$expected_sequence\n"
                    unless $sequence == $expected_sequence;
                die "$path: source time did not advance\n"
                    if defined($previous_source_time) && $source_time <= $previous_source_time;
                $previous_source_time = $source_time;
                ++$expected_sequence;
                ++$anchor_count;
            }
        }
        $frame_position = $frame_end;
    }
    $position = $tag_end;
}
die "$path: no finalized Stream Anchor frames\n" unless $anchor_count > 0;
printf("Verified finalized Stream Anchor frames: count=%d generation=%s.\n", $anchor_count, $expected_generation);
PERL
}

run_stream_anchor_success_case() {
    local latency_mode="$1"
    local case_name="stream-anchor-hevc-24000-1001-aac-${latency_mode}"
    local source="${temporary_directory}/hevc-24000-1001-aac-${latency_mode}-input.ts"
    local marked="${temporary_directory}/${case_name}-marked.ts"
    local encoded="${temporary_directory}/${case_name}-encoded.ts"
    local finalized="${temporary_directory}/${case_name}-finalized.ts"
    local bridge_stderr="${temporary_directory}/${case_name}-bridge.stderr"
    local generation_id
    local -a latency_options=()

    if [[ -z "${tsreadex_binary}" ]]; then
        return 0
    fi
    case "${latency_mode}" in
        normal) ;;
        low) latency_options=(-flush_packets 1) ;;
        *) return 1 ;;
    esac
    if [[ "${latency_mode}" == 'normal' ]]; then
        generation_id=1311768467463790320
    else
        generation_id=1311768467463790321
    fi

    "${tsreadex_binary}" \
        -n -1 -A 1 -c 5 -u 1 -d 9 -g "${generation_id}" \
        "${source}" > "${marked}"
    "${ffmpeg_binary}" \
        -hide_banner -loglevel error -y \
        -f mpegts -i "${marked}" \
        -map 0:v:0 -map 0:a? -map 0:d? -ignore_unknown \
        -c:v libx265 -preset ultrafast -x265-params log-level=error \
        -pix_fmt yuv420p -g 24 -b:v 600k -maxrate 800k -bufsize 800k \
        -c:a copy -c:d copy \
        -f mpegts -muxrate 2200k -pcr_period 20 \
        "${latency_options[@]}" \
        "${encoded}"
    if ! "${bridge_binary}" \
        --video-codec passthrough \
        --audio-codec aac \
        --stream-anchor-v1 \
        < "${encoded}" \
        > "${finalized}" \
        2> "${bridge_stderr}"
    then
        cat "${bridge_stderr}" >&2
        return 1
    fi
    rm -f -- "${bridge_stderr}"

    test -s "${finalized}"
    test "$(wc -c < "${encoded}")" -eq "$(wc -c < "${finalized}")"
    assert_video_stream "${finalized}" hevc
    assert_audio_streams "${finalized}" aac
    assert_video_frame_rate "${finalized}" 24000/1001
    assert_fixed_muxrate_and_pcr_period "${case_name}-encoded" "${encoded}" 2200 20
    assert_fixed_muxrate_and_pcr_period "${case_name}-finalized" "${finalized}" 2200 20
    assert_stream_anchor_id3_transition \
        "${case_name}" "${encoded}" "${finalized}" "${generation_id}"
    assert_strict_transport "${case_name}" "${finalized}"
    stream_anchor_success_case_count=$((stream_anchor_success_case_count + 1))
    printf 'Stream Anchor success path passed: %s.\n' "${case_name}"
}

assert_strict_transport() {
    local case_name="$1"
    local transport_stream="$2"
    local revalidated="${temporary_directory}/${case_name}-revalidated.ts"

    "${bridge_binary}" \
        --pass-through \
        < "${transport_stream}" \
        > "${revalidated}"
    cmp "${transport_stream}" "${revalidated}"
}

assert_av1_elementary_stream() {
    local case_name="$1"
    local transport_stream="$2"
    local elementary_stream="${temporary_directory}/${case_name}-av1.es"

    "${ffmpeg_binary}" \
        -hide_banner \
        -loglevel error \
        -y \
        -i "${transport_stream}" \
        -map 0:d:0 \
        -c copy \
        -f data \
        "${elementary_stream}"
    "${av1_elementary_verifier}" "${elementary_stream}"
}

run_generated_case() {
    local case_name="$1"
    local codec="$2"
    local audio_codec="$3"
    local latency_mode="$4"
    local video_size="$5"
    local video_rate="$6"
    local duration="$7"
    local video_frames="$8"
    local gop_size="$9"
    local video_bitrate="${10}"
    local video_bitrate_max="${11}"
    local transport_rate_kbps="${12}"
    local audio_bitrate="${13}"
    local pixel_format="${14:-yuv420p}"
    local input="${temporary_directory}/${case_name}-input.ts"
    local output="${temporary_directory}/${case_name}-output.ts"
    local bridge_stderr="${temporary_directory}/${case_name}-bridge.stderr"
    local video_encoder
    local -a video_options=()
    local -a audio_options=()
    local -a latency_options=()
    local -a bridge_options=()

    case "${codec}" in
        avc)
            video_encoder='libx264'
            video_options=(-preset ultrafast)
            ;;
        hevc)
            video_encoder='libx265'
            video_options=(-preset ultrafast -x265-params log-level=error)
            ;;
        vp9)
            video_encoder='libvpx-vp9'
            video_options=(
                -cpu-used 8
                -deadline realtime
                -row-mt 1
                -lag-in-frames 0
                -auto-alt-ref 0
                -profile:v 0
            )
            ;;
        av1)
            video_encoder='libaom-av1'
            video_options=(
                -cpu-used 8
                -usage realtime
                -row-mt 1
                -lag-in-frames 0
                -profile:v 0
            )
            ;;
        *)
            printf 'Unknown video codec: %s\n' "${codec}" >&2
            return 1
            ;;
    esac

    case "${audio_codec}" in
        aac)
            audio_options=(-c:a aac -b:a "${audio_bitrate}k")
            ;;
        opus)
            audio_options=(
                -c:a libopus
                -application audio
                -b:a "${audio_bitrate}k"
            )
            ;;
        *)
            printf 'Unknown audio codec: %s\n' "${audio_codec}" >&2
            return 1
            ;;
    esac

    case "${latency_mode}" in
        normal)
            ;;
        low)
            latency_options=(-flush_packets 1)
            ;;
        *)
            printf 'Unknown latency mode: %s\n' "${latency_mode}" >&2
            return 1
            ;;
    esac

    "${ffmpeg_binary}" \
        -hide_banner \
        -loglevel error \
        -y \
        -f lavfi \
        -i "testsrc2=size=${video_size}:rate=${video_rate}:duration=${duration}" \
        -f lavfi \
        -i "sine=frequency=1000:sample_rate=48000:duration=${duration}" \
        -f lavfi \
        -i "sine=frequency=1400:sample_rate=48000:duration=${duration}" \
        -map 0:v:0 \
        -map 1:a:0 \
        -map 2:a:0 \
        -c:v "${video_encoder}" \
        -pix_fmt "${pixel_format}" \
        "${video_options[@]}" \
        -g "${gop_size}" \
        -b:v "${video_bitrate}k" \
        -maxrate "${video_bitrate_max}k" \
        -bufsize "${video_bitrate_max}k" \
        "${audio_options[@]}" \
        -ar 48000 \
        -frames:v "${video_frames}" \
        -shortest \
        -f mpegts \
        -muxrate "${transport_rate_kbps}k" \
        -pcr_period 20 \
        "${latency_options[@]}" \
        "${input}"

    if [[ "${codec}" == 'avc' || "${codec}" == 'hevc' ]]; then
        if [[ "${audio_codec}" == 'aac' ]]; then
            bridge_options=(--pass-through)
        else
            bridge_options=(
                --video-codec passthrough
                --audio-codec opus
            )
        fi
    else
        bridge_options=(
            --video-codec "${codec}"
            --audio-codec "${audio_codec}"
        )
        if [[ "${codec}" == 'av1' ]]; then
            bridge_options+=(
                --transport-rate-kbps "${transport_rate_kbps}"
            )
        fi
    fi

    if ! "${bridge_binary}" \
        "${bridge_options[@]}" \
        < "${input}" \
        > "${output}" \
        2> "${bridge_stderr}"
    then
        cat "${bridge_stderr}" >&2
        printf 'FFmpeg integration failed: case=%s video=%s audio=%s latency=%s input=%s muxrate_kbps=%s pcr_period_ms=20 bridge_args=' \
            "${case_name}" \
            "${codec}" \
            "${audio_codec}" \
            "${latency_mode}" \
            "${input}" \
            "${transport_rate_kbps}" >&2
        printf ' %q' "${bridge_options[@]}" >&2
        printf '\n' >&2
        return 1
    fi
    rm -f -- "${bridge_stderr}"

    test -s "${output}"
    test "$(( $(wc -c < "${output}") % 188 ))" -eq 0
    test "$(wc -c < "${output}")" -eq "$(wc -c < "${input}")"
    if [[ "${bridge_options[0]}" == '--pass-through' ]]; then
        cmp "${input}" "${output}"
    elif [[ "${codec}" == 'vp9' || "${codec}" == 'av1' ]]; then
        if cmp -s "${input}" "${output}"; then
            printf 'Bridge output is byte-identical for semantic case: %s\n' \
                "${case_name}" >&2
            return 1
        fi
    fi

    assert_video_stream "${output}" "${codec}"
    assert_audio_streams "${output}" "${audio_codec}"
    assert_strict_transport "${case_name}" "${output}"
    if [[ "${codec}" == 'av1' ]]; then
        assert_av1_elementary_stream "${case_name}" "${output}"
    fi

    printf 'FFmpeg integration passed: %s (%s bytes)\n' \
        "${case_name}" "$(wc -c < "${output}")"
}

run_matrix_case() {
    local codec="$1"
    local audio_codec="$2"
    local latency_mode="$3"
    local case_name="matrix-${codec}-${audio_codec}-${latency_mode}"
    local transport_rate_kbps=2200

    # 320x180を自動levelでencodeするとAV1 sequence headerはLevel 2.0となる。
    # このlevelのRx=1.65Mbit/sを超えるTS muxrateを宣言すると、正しいT-STD
    # modelが入力を拒否するため、KonomiTVの240p production clampと同じ値にする。
    if [[ "${codec}" == 'av1' ]]; then
        transport_rate_kbps=1650
    fi

    if [[ -n "${matrix_cases[${case_name}]+x}" ]]; then
        printf 'Duplicate FFmpeg matrix case: %s\n' "${case_name}" >&2
        return 1
    fi
    matrix_cases["${case_name}"]=1
    matrix_case_count=$((matrix_case_count + 1))

    run_generated_case \
        "${case_name}" \
        "${codec}" \
        "${audio_codec}" \
        "${latency_mode}" \
        320x180 10 0.8 8 5 600 800 "${transport_rate_kbps}" 96
}

run_hevc_24000_1001_aac_pcr_case() {
    local latency_mode="$1"
    local case_name="hevc-24000-1001-aac-${latency_mode}"
    local transport_rate_kbps=2200
    local input="${temporary_directory}/${case_name}-input.ts"

    # Anchor入力の実運用条件を、通常/低遅延で別々に生成する。
    # 48 frame = 24000/1001fpsの約2秒で、HEVCの3753/3754 tick cadenceを持つ。
    run_generated_case \
        "${case_name}" \
        hevc \
        aac \
        "${latency_mode}" \
        320x180 24000/1001 2.1 48 24 600 800 "${transport_rate_kbps}" 96
    assert_video_frame_rate "${input}" 24000/1001
    assert_fixed_muxrate_and_pcr_period \
        "${case_name}" \
        "${input}" \
        "${transport_rate_kbps}" \
        20
    assert_stream_anchor_ffmpeg_boundary "${case_name}" "${input}"
}

# 4映像codec × 2音声codec × 通常・低遅延を、それぞれ別の入力生成と
# Bridge実行で検証する。片方の遅延モードの成果物は流用しない。
for matrix_codec in avc hevc vp9 av1; do
    for matrix_audio_codec in aac opus; do
        for matrix_latency_mode in normal low; do
            run_matrix_case \
                "${matrix_codec}" \
                "${matrix_audio_codec}" \
                "${matrix_latency_mode}"
        done
    done
done
test "${matrix_case_count}" -eq 16
test "${#matrix_cases[@]}" -eq 16
printf 'Verified independent FFmpeg matrix cases: %s.\n' \
    "${matrix_case_count}"

# 能力probeより高い実運用境界と10bit AV1を、最低16条件に追加して検証する。
run_generated_case production-av1-1080p av1 opus normal \
    1440x1080 30 0.4 12 30 2100 3150 4550 192
run_generated_case production-av1-240p-8bit-normal av1 opus normal \
    426x240 30 1.0 30 30 315 420 1650 192 yuv420p
run_generated_case production-av1-240p-10bit-normal av1 opus normal \
    426x240 30 1.0 30 30 315 420 1650 192 yuv420p10le
run_generated_case production-av1-240p-8bit-low av1 opus low \
    426x240 30 1.0 30 30 315 420 1650 192 yuv420p
run_generated_case production-av1-240p-10bit-low av1 opus low \
    426x240 30 1.0 30 30 315 420 1650 192 yuv420p10le

run_hevc_24000_1001_aac_pcr_case normal
run_hevc_24000_1001_aac_pcr_case low
run_stream_anchor_success_case normal
run_stream_anchor_success_case low
if [[ -n "${tsreadex_binary}" ]]; then
    test "${stream_anchor_success_case_count}" -eq 2
    printf 'Verified independent Stream Anchor success paths: %s.\n' \
        "${stream_anchor_success_case_count}"
else
    printf '%s\n' \
        'Stream Anchor success path skipped: TSREADEX_BINARY was not supplied; fail-closed boundaries remain verified.'
fi

if "${bridge_binary}" \
    --video-codec vp9 \
    --audio-codec opus \
    --transport-rate-kbps 2200 \
    < "${temporary_directory}/matrix-vp9-opus-normal-input.ts" \
    > /dev/null \
    2> "${temporary_directory}/vp9-rate-error.txt"
then
    printf '%s\n' \
        'VP9 unexpectedly accepted --transport-rate-kbps.' >&2
    exit 1
fi
grep -Fq -- \
    '--transport-rate-kbps is only valid with --video-codec av1' \
    "${temporary_directory}/vp9-rate-error.txt"

printf 'FFmpeg integration tests passed: %s matrix cases, HEVC 24000/1001fps AAC normal/low PCR cases, and %s Stream Anchor success cases.\n' \
    "${matrix_case_count}" \
    "${stream_anchor_success_case_count}"
