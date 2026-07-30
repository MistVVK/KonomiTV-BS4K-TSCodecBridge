#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

archive_path="${1:-}"
destination="${2:-}"

if [[ -z "${archive_path}" || -z "${destination}" ]]; then
    printf 'Usage: %s ARCHIVE DESTINATION\n' "${0##*/}" >&2
    exit 2
fi
test -s "${archive_path}"
test -d "${destination}"
test -z "$(find "${destination}" -mindepth 1 -print -quit)"

tar --list --gzip --file "${archive_path}" |
    awk '
        /^\// || /(^|\/)\.\.(\/|$)/ {
            invalid = 1
        }
        {
            split($0, path, "/")
            if (path[1] != "") {
                roots[path[1]] = 1
            }
        }
        END {
            for (root in roots) {
                root_count += 1
            }
            exit invalid || root_count != 1
        }
    '

if ! LC_ALL=C tar --list --verbose --gzip --file "${archive_path}" |
    awk '
        {
            entry_type = substr($1, 1, 1)
            if (entry_type != "-" && entry_type != "d") {
                invalid = 1
            }
        }
        END {
            exit invalid ? 1 : 0
        }
    '
then
    printf 'Archive contains an unsupported non-regular entry: %s\n' \
        "${archive_path}" >&2
    exit 1
fi

tar \
    --extract \
    --gzip \
    --file "${archive_path}" \
    --directory "${destination}" \
    --strip-components 1 \
    --no-same-owner \
    --no-same-permissions
