#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

initial_packages_path="${1:-}"
output_path="${2:-}"

if [[ -z "${initial_packages_path}" || -z "${output_path}" ]]; then
    printf 'Usage: %s INITIAL_PACKAGES_TSV OUTPUT_TSV\n' "${0##*/}" >&2
    exit 2
fi
test -s "${initial_packages_path}"

temporary_directory="$(mktemp -d /tmp/tscodecbridge-builder-packages.XXXXXX)"
installed_packages_path="${temporary_directory}/packages-after.tsv"
changed_packages_path="${temporary_directory}/packages-changed.tsv"

cleanup() {
    rm -rf -- "${temporary_directory}"
}
trap cleanup EXIT

dpkg-query \
    --show \
    --showformat='${binary:Package}\t${Version}\n' |
    LC_ALL=C sort > "${installed_packages_path}"
awk -F '\t' '
    NR == FNR {
        before[$1] = $2
        next
    }
    !($1 in before) || before[$1] != $2 {
        print $1 "\t" $2
    }
' "${initial_packages_path}" "${installed_packages_path}" > "${changed_packages_path}"
test -s "${changed_packages_path}"

: > "${output_path}"
while IFS=$'\t' read -r package version; do
    origin="$(
        apt-cache policy "${package}" |
            awk -v installed_version="${version}" '
                $1 == "***" && $2 == installed_version {
                    selected = 1
                    next
                }
                selected && $1 ~ /^[0-9]+$/ && $2 ~ /^https?:\/\// {
                    print $2 " " $3 " " $4 " " $5
                    exit
                }
            '
    )"
    if [[ ! "${origin}" =~ ^https?://(archive|security)\.ubuntu\.com/ubuntu[[:space:]] ]]; then
        printf 'Package has no official Ubuntu archive origin: %s %s (%s)\n' \
            "${package}" \
            "${version}" \
            "${origin}" >&2
        exit 1
    fi
    printf 'BUILDER_PACKAGE\t%s\t%s\t%s\n' \
        "${package}" \
        "${version}" \
        "${origin}" |
        tee -a "${output_path}"
done < "${changed_packages_path}"
