#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

script_directory="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd -- "${script_directory}/.." && pwd)"
cache_directory="$(mktemp -d /tmp/tscodecbridge-asdf.XXXXXX)"

cleanup() {
    rm -rf -- "${cache_directory}"
}
trap cleanup EXIT

export PROJECT_ROOT="${project_root}"
export CL_SOURCE_REGISTRY="${project_root}//:"
export ASDF_OUTPUT_TRANSLATIONS="/:${cache_directory}/"

sbcl --noinform \
    --disable-debugger \
    --non-interactive \
    --load "${script_directory}/benchmark-large-pes.lisp"
