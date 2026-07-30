#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

# lint toolはbuilderへ外部導入し、リポジトリには収録しない。
script_directory="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd -- "${script_directory}/.." && pwd)"
cache_directory="$(mktemp -d /tmp/tscodecbridge-asdf.XXXXXX)"
mallet_binary="${MALLET_BINARY:-/opt/mallet/bin/mallet}"

cleanup() {
    rm -rf -- "${cache_directory}"
}
trap cleanup EXIT

: "${SBLINT_SOURCE_DIR:?SBLINT_SOURCE_DIR must point to pinned SBLint source.}"
test -x "${mallet_binary}"

export PROJECT_ROOT="${project_root}"
export CL_SOURCE_REGISTRY="${project_root}//:${SBLINT_SOURCE_DIR}//:"
export ASDF_OUTPUT_TRANSLATIONS="/:${cache_directory}/"

printf '%s\n' 'Running SBLint...'
sbcl --noinform \
    --disable-debugger \
    --non-interactive \
    --load "${script_directory}/run-sblint.lisp"

printf '%s\n' 'Running Mallet strict lint...'
"${mallet_binary}" --strict --fail-on warning "${project_root}"
printf '%s\n' 'All lint gates passed.'
