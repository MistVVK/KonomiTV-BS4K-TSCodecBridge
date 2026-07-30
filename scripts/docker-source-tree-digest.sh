#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

source_root="${1:-}"

if [[ -z "${source_root}" || ! -d "${source_root}" ]]; then
    printf 'Usage: %s SOURCE_ROOT\n' "${0##*/}" >&2
    exit 2
fi

# Gitが追跡できる通常fileとsymbolic linkだけを対象にし、path・Git実行bit・
# 内容をNUL区切りでhashする。umaskで変わるgroup/other書込bitや、
# mtime・uid・gidはcheckoutやDocker COPYで変わるため含めない。
unsupported_path="$(
    find "${source_root}" \
        -mindepth 1 \
        ! -type d \
        ! -type f \
        ! -type l \
        -print -quit
)"
if [[ -n "${unsupported_path}" ]]; then
    printf 'Unsupported source tree entry: %s\n' "${unsupported_path}" >&2
    exit 1
fi

(
    cd -- "${source_root}"
    find . -mindepth 1 \( -type f -o -type l \) -print0 |
        LC_ALL=C sort -z |
        while IFS= read -r -d '' relative_path; do
            if [[ -L "${relative_path}" ]]; then
                link_target="$(readlink -- "${relative_path}")"
                printf 'L\0%s\0%s\0' "${relative_path}" "${link_target}"
            else
                if [[ -x "${relative_path}" ]]; then
                    file_mode='100755'
                else
                    file_mode='100644'
                fi
                file_size="$(stat -c '%s' -- "${relative_path}")"
                file_sha256="$(sha256sum -- "${relative_path}" | awk '{ print $1 }')"
                printf 'F\0%s\0%s\0%s\0%s\0' \
                    "${relative_path}" \
                    "${file_mode}" \
                    "${file_size}" \
                    "${file_sha256}"
            fi
        done
) |
    sha256sum |
    awk '{ print $1 }'
