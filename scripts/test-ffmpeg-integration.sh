#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

script_directory="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd -- "${script_directory}/.." && pwd)"
ffmpeg_binary="${FFMPEG_BINARY:-ffmpeg}"
ffprobe_binary="${FFPROBE_BINARY:-ffprobe}"
bridge_binary="${BRIDGE_BINARY:-${project_root}/build/ts-codec-bridge.elf}"
av1_elementary_verifier="${project_root}/scripts/verify-av1-ts-elementary-stream.sh"
expected_ffmpeg_sha256="${FFMPEG_SHA256:-5599c2bc61d987c7336ad4a7bf4b917fa8b260822fbb65c8b069188931339f76}"
expected_ffprobe_sha256="${FFPROBE_SHA256:-877ca4ba5b63cabde0378cb39af3be12e449b6079c2d975b37aaab20f758c191}"
temporary_directory="$(mktemp -d /tmp/tscodecbridge-ffmpeg.XXXXXX)"
keep_artifacts="${KEEP_FFMPEG_ARTIFACTS:-0}"
matrix_case_count=0
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

test -x "${ffmpeg_binary}"
test -x "${ffprobe_binary}"
test -x "${bridge_binary}"
test -x "${av1_elementary_verifier}"

test "$(sha256sum "${ffmpeg_binary}" | cut -d' ' -f1)" = \
    "${expected_ffmpeg_sha256}"
test "$(sha256sum "${ffprobe_binary}" | cut -d' ' -f1)" = \
    "${expected_ffprobe_sha256}"
"${ffmpeg_binary}" -version 2>&1 |
    grep -Fq 'ffmpeg version n8.1.2 '
"${ffprobe_binary}" -version 2>&1 |
    grep -Fq 'ffprobe version n8.1.2 '
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

printf 'Fixed FFmpeg 8.1.2 integration tests passed: %s matrix cases.\n' \
    "${matrix_case_count}"
