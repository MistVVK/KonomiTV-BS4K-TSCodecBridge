#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

# test systemのFASLは一時directoryへ隔離する。
script_directory="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd -- "${script_directory}/.." && pwd)"
cache_directory="$(mktemp -d /tmp/tscodecbridge-asdf.XXXXXX)"
test_log="${cache_directory}/test.log"
expected_test_count="${EXPECTED_TEST_COUNT:-}"

cleanup() {
    rm -rf -- "${cache_directory}"
}
trap cleanup EXIT

export PROJECT_ROOT="${project_root}"
export CL_SOURCE_REGISTRY="${project_root}//:"
export ASDF_OUTPUT_TRANSLATIONS="/:${cache_directory}/"

if [[ -n "${expected_test_count}" ]] &&
    [[ ! "${expected_test_count}" =~ ^[1-9][0-9]*$ ]]
then
    printf 'EXPECTED_TEST_COUNT must be a positive integer: %s\n' \
        "${expected_test_count}" >&2
    exit 2
fi

printf '%s\n' 'Running ASDF tests...'
sbcl --noinform \
    --disable-debugger \
    --non-interactive \
    --load "${script_directory}/test.lisp" \
    2>&1 |
    tee "${test_log}"

if [[ -n "${expected_test_count}" ]]; then
    grep -Fqx \
        "Tests: ${expected_test_count} passed, 0 failed" \
        "${test_log}"
    printf 'Verified exact Bridge test count: %s.\n' \
        "${expected_test_count}"
fi
