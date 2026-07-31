#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

script_directory="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd -- "${script_directory}/.." && pwd)"
expected_test_count="${EXPECTED_TEST_COUNT:-201}"

run_gate() {
    local gate_name="$1"
    shift
    printf '\n=== Release gate: %s ===\n' "${gate_name}"
    "$@"
}

cd "${project_root}"

run_gate \
    'SBCL clean compile' \
    bash "${script_directory}/compile.sh"
run_gate \
    'SBLint and Mallet warning-free lint' \
    bash "${script_directory}/lint.sh"
run_gate \
    "exactly ${expected_test_count} unit tests" \
    env EXPECTED_TEST_COUNT="${expected_test_count}" \
    bash "${script_directory}/test.sh"
run_gate \
    'fixture manifest and SHA-256' \
    bash "${script_directory}/verify-fixture-manifest.sh"
run_gate \
    'saved executable build' \
    bash "${script_directory}/build.sh"
run_gate \
    'saved executable fixtures and fail-closed boundary' \
    bash "${script_directory}/test-executable.sh"
run_gate \
    'Docker runtime distribution static audit' \
    bash "${script_directory}/test-docker-distribution-static.sh"
run_gate \
    'large-PES latency and allocation gate' \
    bash "${script_directory}/benchmark-large-pes.sh"
run_gate \
    'fixed FFmpeg 8.1.2 sixteen-case integration matrix' \
    bash "${script_directory}/test-ffmpeg-integration.sh"
run_gate \
    'pass-through, VP9, and AV1 600-second transport, latency, allocation, and RSS soak' \
    env SOAK_MODE=all SOAK_DURATION_SECONDS=600 \
    bash "${script_directory}/soak.sh"

printf '\nRelease check passed with %s unit tests, the 16-case FFmpeg matrix, and all 600-second soak modes.\n' \
    "${expected_test_count}"
