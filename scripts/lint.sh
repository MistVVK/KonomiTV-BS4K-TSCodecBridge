#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

# lint toolはホストへ外部導入し、リポジトリには収録しない。
script_directory="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd -- "${script_directory}/.." && pwd)"
cache_directory="$(mktemp -d /tmp/tscodecbridge-asdf.XXXXXX)"
mallet_binary="${MALLET_BINARY:-$(command -v mallet || true)}"
sblint_source_directory="${SBLINT_SOURCE_DIR:-}"

cleanup() {
    rm -rf -- "${cache_directory}"
}
trap cleanup EXIT

test -n "${mallet_binary}"
test -x "${mallet_binary}"

# 共通 bin 上の Mallet から installer の tool root を逆算し、同じ場所の
# SBLint source を既定値として使う。別々に導入した場合だけ明示指定する。
if [[ -z "${sblint_source_directory}" ]]; then
    resolved_mallet_binary="$(realpath "${mallet_binary}")"
    lint_tool_root="$(dirname "$(dirname "$(dirname "${resolved_mallet_binary}")")")"
    sblint_source_directory="${lint_tool_root}/sblint"
fi
test -f "${sblint_source_directory}/sblint.asd"

export PROJECT_ROOT="${project_root}"
export SBLINT_SOURCE_DIR="${sblint_source_directory}"
export CL_SOURCE_REGISTRY="${project_root}//:${sblint_source_directory}//:"
export ASDF_OUTPUT_TRANSLATIONS="/:${cache_directory}/"

printf '%s\n' 'Running SBLint...'
sbcl --noinform \
    --disable-debugger \
    --non-interactive \
    --load "${script_directory}/run-sblint.lisp"

printf '%s\n' 'Running Mallet strict lint...'
"${mallet_binary}" --strict --fail-on warning "${project_root}"
printf '%s\n' 'All lint gates passed.'
