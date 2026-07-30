#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

# 保存実行形式以外のASDF生成物は一時directoryへ隔離する。
script_directory="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd -- "${script_directory}/.." && pwd)"
cache_directory="$(mktemp -d /tmp/tscodecbridge-asdf.XXXXXX)"
output_path="${project_root}/build/ts-codec-bridge.elf"

cleanup() {
    rm -rf -- "${cache_directory}"
}
trap cleanup EXIT

mkdir -p -- "${project_root}/build"
export PROJECT_ROOT="${project_root}"
export OUTPUT_PATH="${output_path}"
export CL_SOURCE_REGISTRY="${project_root}//:"
export ASDF_OUTPUT_TRANSLATIONS="/:${cache_directory}/"

printf '%s\n' 'Building saved SBCL executable...'
sbcl --noinform \
    --disable-debugger \
    --non-interactive \
    --load "${script_directory}/build.lisp"

test -x "${output_path}"
printf 'Built %s\n' "${output_path}"
