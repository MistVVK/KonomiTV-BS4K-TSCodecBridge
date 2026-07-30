#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

# TS packet境界の前後とstdoutの完全一致を確認する。
script_directory="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd -- "${script_directory}/.." && pwd)"
bridge_binary="${BRIDGE_BINARY:-${project_root}/build/ts-codec-bridge.elf}"
temporary_directory="$(mktemp -d /tmp/tscodecbridge-test.XXXXXX)"

cleanup() {
    rm -rf -- "${temporary_directory}"
}
trap cleanup EXIT

test -x "${bridge_binary}"

valid_fixture="${project_root}/tests/fixtures/valid-vp9-opus.ts"
valid_aac_fixture="${project_root}/tests/fixtures/valid-av1-aac.ts"

printf '%s\n' 'Testing saved executable pass-through byte exactness...'
for fixture in "${valid_fixture}" "${valid_aac_fixture}"; do
    output_path="${temporary_directory}/$(basename "${fixture}").pass.ts"
    "${bridge_binary}" --pass-through < "${fixture}" > "${output_path}"
    cmp "${fixture}" "${output_path}"
done

empty_input="${temporary_directory}/empty.ts"
empty_output="${temporary_directory}/empty.out.ts"
dd if=/dev/null of="${empty_input}" status=none
"${bridge_binary}" --pass-through < "${empty_input}" > "${empty_output}"
cmp "${empty_input}" "${empty_output}"

for byte_count in 1 187 189 2067; do
    input_path="${temporary_directory}/truncated-${byte_count}.ts"
    output_path="${temporary_directory}/truncated-${byte_count}.out.ts"
    dd if="${valid_fixture}" \
        of="${input_path}" \
        bs=1 \
        count="${byte_count}" \
        status=none
    if "${bridge_binary}" --pass-through \
        < "${input_path}" \
        > "${output_path}" \
        2> "${output_path}.stderr"; then
        printf 'Truncated pass-through unexpectedly succeeded: %s\n' \
            "${byte_count}" >&2
        exit 1
    fi
done

if "${bridge_binary}" --pass-through \
    < "${project_root}/tests/fixtures/corrupt-sync.ts" \
    > "${temporary_directory}/corrupt-sync.ts.pass.stdout" \
    2> "${temporary_directory}/corrupt-sync.ts.pass.stderr"; then
    printf '%s\n' \
        'Invalid pass-through sync byte unexpectedly succeeded.' >&2
    exit 1
fi

printf '%s\n' 'Testing saved executable version output...'
version_output="$("${bridge_binary}" --version)"
printf 'Observed version output: <%s>\n' "${version_output}"
test "${version_output}" = '0.1.0'
printf '%s\n' 'Testing saved executable mapping version output...'
mapping_version_output="$("${bridge_binary}" --mapping-version)"
printf 'Observed mapping version output: <%s>\n' \
    "${mapping_version_output}"
test "${mapping_version_output}" = '1'
printf '%s\n' 'Testing saved executable help output...'
"${bridge_binary}" --help | grep -Fqx \
    'Usage: ts-codec-bridge.elf [OPTIONS]'
"${bridge_binary}" --help | grep -Fq \
    -- '--mapping-version'

printf '%s\n' 'Testing saved executable invalid arguments...'
if "${bridge_binary}" --invalid \
    > "${temporary_directory}/invalid.stdout" \
    2> "${temporary_directory}/invalid.stderr"; then
    printf '%s\n' 'Invalid CLI arguments unexpectedly succeeded.' >&2
    exit 1
fi
test ! -s "${temporary_directory}/invalid.stdout"
grep -Fq 'ts-codecbridge: error:' \
    "${temporary_directory}/invalid.stderr"

printf '%s\n' 'Testing duplicate mapping version arguments...'
if "${bridge_binary}" --mapping-version --mapping-version \
    > "${temporary_directory}/duplicate.stdout" \
    2> "${temporary_directory}/duplicate.stderr"; then
    printf '%s\n' 'Duplicate mapping version unexpectedly succeeded.' >&2
    exit 1
fi
test ! -s "${temporary_directory}/duplicate.stdout"
grep -Fq -- '--mapping-version may be specified only once' \
    "${temporary_directory}/duplicate.stderr"

printf '%s\n' 'Testing validated production fast path...'
"${bridge_binary}" \
    < "${valid_fixture}" \
    > "${temporary_directory}/validated.ts"
cmp "${valid_fixture}" "${temporary_directory}/validated.ts"

for corrupt_name in \
    corrupt-sync.ts \
    corrupt-truncated-packet.ts \
    corrupt-video-cc.ts; do
    if "${bridge_binary}" \
        < "${project_root}/tests/fixtures/${corrupt_name}" \
        > "${temporary_directory}/${corrupt_name}.stdout" \
        2> "${temporary_directory}/${corrupt_name}.stderr"; then
        printf 'Corrupt default input unexpectedly succeeded: %s\n' \
            "${corrupt_name}" >&2
        exit 1
    fi
done

cp "${valid_fixture}" "${temporary_directory}/afc-zero.ts"
printf '\000' |
    dd of="${temporary_directory}/afc-zero.ts" \
        bs=1 seek=3 conv=notrunc status=none
if "${bridge_binary}" \
    < "${temporary_directory}/afc-zero.ts" \
    > "${temporary_directory}/afc-zero.stdout" \
    2> "${temporary_directory}/afc-zero.stderr"; then
    printf '%s\n' 'AFC zero default input unexpectedly succeeded.' >&2
    exit 1
fi
if "${bridge_binary}" --pass-through \
    < "${temporary_directory}/afc-zero.ts" \
    > "${temporary_directory}/afc-zero.pass.stdout" \
    2> "${temporary_directory}/afc-zero.pass.stderr"; then
    printf '%s\n' \
        'AFC zero pass-through input unexpectedly succeeded.' >&2
    exit 1
fi

cp "${valid_fixture}" "${temporary_directory}/bad-adaptation.ts"
printf '\267' |
    dd of="${temporary_directory}/bad-adaptation.ts" \
        bs=1 seek=4 conv=notrunc status=none
if "${bridge_binary}" \
    < "${temporary_directory}/bad-adaptation.ts" \
    > "${temporary_directory}/bad-adaptation.stdout" \
    2> "${temporary_directory}/bad-adaptation.stderr"; then
    printf '%s\n' \
        'Invalid adaptation field default input unexpectedly succeeded.' \
        >&2
    exit 1
fi

printf '%s\n' 'Testing saved executable semantic fixture paths...'
if "${bridge_binary}" \
    --video-codec av1 \
    --audio-codec aac \
    < "${project_root}/tests/fixtures/valid-av1-aac.ts" \
    > "${temporary_directory}/av1-missing-rate.stdout" \
    2> "${temporary_directory}/av1-missing-rate.stderr"; then
    printf '%s\n' \
        'AV1 unexpectedly accepted a missing transport rate.' >&2
    exit 1
fi
test ! -s "${temporary_directory}/av1-missing-rate.stdout"
grep -Fq -- \
    '--transport-rate-kbps is required with --video-codec av1' \
    "${temporary_directory}/av1-missing-rate.stderr"

if "${bridge_binary}" \
    --video-codec vp9 \
    --audio-codec opus \
    --transport-rate-kbps 2200 \
    < "${project_root}/tests/fixtures/valid-vp9-opus.ts" \
    > "${temporary_directory}/vp9-forbidden-rate.stdout" \
    2> "${temporary_directory}/vp9-forbidden-rate.stderr"; then
    printf '%s\n' \
        'VP9 unexpectedly accepted a transport rate.' >&2
    exit 1
fi
test ! -s "${temporary_directory}/vp9-forbidden-rate.stdout"
grep -Fq -- \
    '--transport-rate-kbps is only valid with --video-codec av1' \
    "${temporary_directory}/vp9-forbidden-rate.stderr"

printf '%s\n' \
    'Testing semantic path integrity on non-target transport PIDs...'
cp "${valid_fixture}" "${temporary_directory}/semantic-tei.ts"
printf '\301' |
    dd of="${temporary_directory}/semantic-tei.ts" \
        bs=1 seek="$((3 * 188 + 1))" conv=notrunc status=none
cp "${valid_fixture}" "${temporary_directory}/semantic-scrambled.ts"
printf '\260' |
    dd of="${temporary_directory}/semantic-scrambled.ts" \
        bs=1 seek="$((5 * 188 + 3))" conv=notrunc status=none

conflicting_duplicate="${temporary_directory}/semantic-conflict.ts"
dd if="${valid_fixture}" of="${conflicting_duplicate}" \
    bs=188 count=6 status=none
dd if="${valid_fixture}" of="${conflicting_duplicate}" \
    bs=188 skip=5 seek=6 count=1 conv=notrunc status=none
printf '\110' |
    dd of="${conflicting_duplicate}" \
        bs=1 seek="$((6 * 188 + 187))" conv=notrunc status=none
dd if="${valid_fixture}" of="${conflicting_duplicate}" \
    bs=188 skip=6 seek=7 conv=notrunc status=none

for corrupt_name in \
    semantic-tei.ts \
    semantic-scrambled.ts \
    semantic-conflict.ts; do
    if "${bridge_binary}" --pass-through \
        < "${temporary_directory}/${corrupt_name}" \
        > "${temporary_directory}/${corrupt_name}.pass.stdout" \
        2> "${temporary_directory}/${corrupt_name}.pass.stderr"; then
        printf 'Corrupt pass-through input unexpectedly succeeded: %s\n' \
            "${corrupt_name}" >&2
        exit 1
    fi
    if "${bridge_binary}" \
        --video-codec vp9 \
        --audio-codec opus \
        < "${temporary_directory}/${corrupt_name}" \
        > "${temporary_directory}/${corrupt_name}.stdout" \
        2> "${temporary_directory}/${corrupt_name}.stderr"; then
        printf 'Corrupt semantic input unexpectedly succeeded: %s\n' \
            "${corrupt_name}" >&2
        exit 1
    fi
done

for corrupt_name in \
    corrupt-pat-null-program-pid.ts \
    corrupt-pat-reserved-program-pid.ts \
    corrupt-pat-duplicate-program-number.ts \
    corrupt-pmt-null-elementary-pid.ts \
    corrupt-pmt-zero-program-number.ts \
    corrupt-pmt-reserved-elementary-pid.ts \
    corrupt-pmt-reserved-pcr-pid.ts \
    corrupt-pmt-duplicate-elementary-pid.ts \
    corrupt-opus-lacing.ts \
    corrupt-opus-zero-pes-length.ts \
    corrupt-pes-truncated-escr.ts; do
    if "${bridge_binary}" \
        --video-codec vp9 \
        --audio-codec opus \
        < "${project_root}/tests/fixtures/${corrupt_name}" \
        > "${temporary_directory}/${corrupt_name}.stdout" \
        2> "${temporary_directory}/${corrupt_name}.stderr"; then
        printf 'Corrupt semantic input unexpectedly succeeded: %s\n' \
            "${corrupt_name}" >&2
        exit 1
    fi
done

exact_duplicate="${temporary_directory}/semantic-exact-duplicate.ts"
dd if="${valid_fixture}" of="${exact_duplicate}" \
    bs=188 count=6 status=none
dd if="${valid_fixture}" of="${exact_duplicate}" \
    bs=188 skip=5 seek=6 count=1 conv=notrunc status=none
dd if="${valid_fixture}" of="${exact_duplicate}" \
    bs=188 skip=6 seek=7 conv=notrunc status=none
"${bridge_binary}" \
    --video-codec vp9 \
    --audio-codec opus \
    < "${exact_duplicate}" \
    > "${temporary_directory}/semantic-exact-duplicate.out.ts"
test -s "${temporary_directory}/semantic-exact-duplicate.out.ts"
test "$(( $(wc -c \
    < "${temporary_directory}/semantic-exact-duplicate.out.ts") % 188 ))" \
    -eq 0

"${bridge_binary}" \
    --video-codec vp9 \
    --audio-codec opus \
    < "${project_root}/tests/fixtures/valid-vp9-opus.ts" \
    > "${temporary_directory}/vp9-opus-output.ts"
"${bridge_binary}" \
    --video-codec av1 \
    --audio-codec aac \
    --transport-rate-kbps 2200 \
    < "${project_root}/tests/fixtures/valid-av1-aac.ts" \
    > "${temporary_directory}/av1-aac-output.ts"

export PROJECT_ROOT="${project_root}"
export VP9_EXECUTABLE_OUTPUT="${temporary_directory}/vp9-opus-output.ts"
export AV1_EXECUTABLE_OUTPUT="${temporary_directory}/av1-aac-output.ts"
export CL_SOURCE_REGISTRY="${project_root}//:"
export ASDF_OUTPUT_TRANSLATIONS="/:${temporary_directory}/asdf-cache/"
sbcl --noinform \
    --disable-debugger \
    --non-interactive \
    --load "${script_directory}/verify-executable-fixtures.lisp"

printf '%s\n' 'Saved executable tests passed.'
