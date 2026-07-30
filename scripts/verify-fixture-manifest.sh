#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

script_directory="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd -- "${script_directory}/.." && pwd)"
fixture_directory="${project_root}/tests/fixtures"
manifest="${fixture_directory}/manifest.sexp"

manifest_files="$(
    awk '
        /:file "[^"]+"/ {
            line = $0
            sub(/^.*:file "/, "", line)
            sub(/".*$/, "", line)
            print line
        }
    ' "${manifest}" |
        sort
)"
actual_files="$(
    find "${fixture_directory}" \
        -maxdepth 1 \
        -type f \
        -name '*.ts' \
        -printf '%f\n' |
        sort
)"
test "${manifest_files}" = "${actual_files}"

awk '
    /:file "[^"]+"/ {
        file = $0
        sub(/^.*:file "/, "", file)
        sub(/".*$/, "", file)
    }
    /:sha256 "[0-9a-f]+"/ {
        digest = $0
        sub(/^.*:sha256 "/, "", digest)
        sub(/".*$/, "", digest)
        print digest "  tests/fixtures/" file
    }
' "${manifest}" |
    (
        cd "${project_root}"
        sha256sum --check --strict -
    )

printf '%s\n' 'Fixture manifest file set and SHA-256 values are current.'
