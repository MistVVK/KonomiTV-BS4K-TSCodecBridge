#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

script_directory="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd -- "${script_directory}/.." && pwd)"

cd "${project_root}"

# 公開候補 commit の短時間 gate、正規 runtime image、24時間保持 gate を
# 一つの最終入口へ固定する。runtime build は clean commit を必須とする。
bash "${script_directory}/release-check.sh"
bash "${script_directory}/build-runtime-image.sh"
bash "${script_directory}/test-runtime-image.sh"
SOAK_MODE=av1 SOAK_DURATION_SECONDS=86400 bash "${script_directory}/soak.sh"

printf '%s\n' \
    'Final release check passed, including the runtime image and 24-hour soak.'
