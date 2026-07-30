#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

script_directory="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd -- "${script_directory}/.." && pwd)"
runtime_image="${RUNTIME_IMAGE:-konomitv-bs4k-tscodecbridge:runtime}"
temporary_directory="$(mktemp -d /tmp/tscodecbridge-runtime-test.XXXXXX)"
container_id=''

cleanup() {
    if [[ -n "${container_id}" ]]; then
        docker container rm --force "${container_id}" >/dev/null 2>&1 || true
    fi
    rm -rf -- "${temporary_directory}"
}
trap cleanup EXIT

run_hardened_runtime() {
    docker run --rm \
        --network none \
        --read-only \
        --cap-drop ALL \
        --security-opt no-new-privileges \
        --tmpfs /tmp:rw,noexec,nosuid,size=16m \
        -i \
        "${runtime_image}" \
        "$@"
}

assert_semantic_transport_output() {
    local case_name="${1}"
    local input_path="${2}"
    local output_path="${3}"
    shift 3
    local input_size
    local output_size

    test -s "${output_path}"
    input_size="$(wc -c < "${input_path}")"
    output_size="$(wc -c < "${output_path}")"
    test "$((input_size % 188))" -eq 0
    test "$((output_size % 188))" -eq 0
    test "$((output_size / 188))" -eq "$((input_size / 188))"
    if cmp -s "${input_path}" "${output_path}"; then
        printf 'Runtime semantic fixture was not transformed: %s\n' \
            "${case_name}" >&2
        exit 1
    fi
    for expected_marker in "$@"; do
        if ! LC_ALL=C grep -aFq -- "${expected_marker}" "${output_path}"; then
            printf 'Runtime semantic output lacks marker %s: %s\n' \
                "${expected_marker}" "${case_name}" >&2
            exit 1
        fi
    done

    # 最終runtime自身のstrict TS fast pathで全packet境界を再検証する。
    run_hardened_runtime \
        < "${output_path}" \
        > "${temporary_directory}/${case_name}-revalidated.ts"
    cmp \
        "${output_path}" \
        "${temporary_directory}/${case_name}-revalidated.ts"
}

license_label="$(
    docker image inspect \
        --format '{{index .Config.Labels "org.opencontainers.image.licenses"}}' \
        "${runtime_image}"
)"
source_commit="$(
    docker image inspect \
        --format '{{index .Config.Labels "org.opencontainers.image.revision"}}' \
        "${runtime_image}"
)"
source_tree_sha256="$(
    docker image inspect \
        --format '{{index .Config.Labels "cc.konomi.konomitv-bs4k.source-tree-sha256"}}' \
        "${runtime_image}"
)"

test "${license_label}" = 'NOASSERTION'
[[ "${source_commit}" =~ ^[0-9a-f]{40}$ ]]
test "${source_commit}" != '0000000000000000000000000000000000000000'
[[ "${source_tree_sha256}" =~ ^[0-9a-f]{64}$ ]]
test "$(docker image inspect \
    --format '{{.Config.User}}' \
    "${runtime_image}")" = '65532:65532'
test "$(docker image inspect \
    --format '{{json .Config.Entrypoint}}' \
    "${runtime_image}")" = '["/usr/local/bin/ts-codec-bridge.elf"]'

container_id="$(docker container create --network none "${runtime_image}")"
docker container export "${container_id}" |
    tar --extract --file - --directory "${temporary_directory}"

manifest_path="${temporary_directory}/usr/share/doc/ts-codec-bridge/Runtime-Manifest.tsv"
test -s "${manifest_path}"
test "$(awk -F '\t' '$1 == "SOURCE_COMMIT" { print $2 }' "${manifest_path}")" = \
    "${source_commit}"
test "$(awk -F '\t' '$1 == "SOURCE_TREE_SHA256" { print $2 }' "${manifest_path}")" = \
    "${source_tree_sha256}"
test "$(awk -F '\t' '$1 == "CLI_VERSION" { print $2 }' "${manifest_path}")" = '0.1.0'
test "$(awk -F '\t' '$1 == "TS_MAPPING_VERSION" { print $2 }' "${manifest_path}")" = '1'
test "$(awk -F '\t' '$1 == "CA_CERTIFICATES_PACKAGE_VERSION" { print $2 }' "${manifest_path}")" = \
    '20260601~22.04.1'
test "$(awk -F '\t' '$1 == "CURL_PACKAGE_VERSION" { print $2 }' "${manifest_path}")" = \
    '7.81.0-1ubuntu1.25'
test "$(awk -F '\t' '$1 == "MAKE_PACKAGE_VERSION" { print $2 }' "${manifest_path}")" = \
    '4.3-4.1build1'
test "$(awk -F '\t' '$1 == "TAR_PACKAGE_VERSION" { print $2 }' "${manifest_path}")" = \
    '1.34+dfsg-1ubuntu0.1.22.04.6'
test "$(awk -F '\t' '$1 == "SBCL_PACKAGE_SHA256" { print $2 }' "${manifest_path}")" = \
    '683c3c5e6d25fdc37de1b3fd09f1f40e2af2ede21d6de2569549d2b16776a77d'
test "$(
    awk -F '\t' '$1 ~ /_PACKAGE_SHA256$/ && $2 ~ /^[0-9a-f]{64}$/ {
        count += 1
    } END { print count + 0 }' "${manifest_path}"
)" -eq 7
if grep -Eq 'working-tree|PENDING' "${manifest_path}"; then
    printf 'Runtime manifest contains non-reproducible provenance.\n' >&2
    exit 1
fi

awk -F '\t' '
    $1 == "BUILDER_PACKAGE" {
        package_count += 1
        if (NF != 4 ||
            $4 !~ /^https?:\/\/(archive|security)\.ubuntu\.com\/ubuntu /) {
            invalid = 1
        }
    }
    $1 == "RUNTIME_LIBRARY" {
        library_count += 1
        if (NF != 5 || $3 !~ /^[0-9a-f]{64}$/ ||
            $2 !~ /^\/(lib|lib64|usr\/lib)\//) {
            invalid = 1
        }
    }
    $1 == "EXECUTABLE_SHA256" ||
    $1 == "BUILD_GLIBC_LIBRARY_SHA256" ||
    $1 == "BUILD_ZLIB_LIBRARY_SHA256" {
        digest_count += 1
        if ($2 !~ /^[0-9a-f]{64}$/) {
            invalid = 1
        }
    }
    END {
        exit invalid || package_count == 0 || library_count == 0 ||
            digest_count != 3
    }
' "${manifest_path}"

expected_executable_sha256="$(
    awk -F '\t' '$1 == "EXECUTABLE_SHA256" { print $2 }' "${manifest_path}"
)"
test "$(
    sha256sum "${temporary_directory}/usr/local/bin/ts-codec-bridge.elf" |
        awk '{ print $1 }'
)" = "${expected_executable_sha256}"

while IFS=$'\t' read -r record_type library_path library_sha256 package_name package_version; do
    test "${record_type}" = 'RUNTIME_LIBRARY'
    test -n "${package_name}"
    test -n "${package_version}"
    test -f "${temporary_directory}${library_path}"
    test "$(
        sha256sum "${temporary_directory}${library_path}" |
            awk '{ print $1 }'
    )" = "${library_sha256}"
done < <(awk -F '\t' '$1 == "RUNTIME_LIBRARY"' "${manifest_path}")

if find "${temporary_directory}" -type f \
    \( -name '*.lisp' -o -name '*.asd' -o -name '*.fasl' \) |
    grep -q .
then
    printf 'Runtime image contains Common Lisp source or build products.\n' >&2
    exit 1
fi
for forbidden_path in \
    opt \
    work \
    build \
    usr/bin/sbcl \
    usr/bin/nala \
    usr/bin/curl \
    var/cache \
    var/lib/apt
do
    test ! -e "${temporary_directory}/${forbidden_path}"
done

find "${temporary_directory}/usr/share/doc/ts-codec-bridge" \
    -mindepth 1 \
    -maxdepth 1 \
    -type f \
    -printf '%f\n' |
    LC_ALL=C sort > "${temporary_directory}/runtime-doc-files.txt"
printf '%s\n' \
    Apache-2.0-LICENSE \
    GFDL-1.3-LICENSE \
    GLIBC-COPYRIGHT \
    GPL-2-LICENSE \
    LGPL-2.1-LICENSE \
    LICENSE \
    Runtime-Manifest.tsv \
    SBCL-COPYRIGHT \
    ZLIB-COPYRIGHT |
    LC_ALL=C sort > "${temporary_directory}/expected-runtime-doc-files.txt"
cmp \
    "${temporary_directory}/expected-runtime-doc-files.txt" \
    "${temporary_directory}/runtime-doc-files.txt"

common_license_count=0
while IFS=$'\t' read -r record_type license_name license_sha256 source_path; do
    test "${record_type}" = 'COMMON_LICENSE'
    test -n "${license_name}"
    test "${source_path}" = \
        "/usr/share/common-licenses/${license_name%-LICENSE}"
    printf '%s' "${license_sha256}" | grep -Eq '^[0-9a-f]{64}$'
    printf '%s  %s\n' \
        "${license_sha256}" \
        "${temporary_directory}/usr/share/doc/ts-codec-bridge/${license_name}" |
        sha256sum --check --strict -
    common_license_count=$((common_license_count + 1))
done < <(awk -F '\t' '$1 == "COMMON_LICENSE"' "${manifest_path}")
test "${common_license_count}" -eq 4

run_hardened_runtime --version |
    grep -Fqx '0.1.0'

run_hardened_runtime --mapping-version |
    grep -Fqx '1'

run_hardened_runtime \
    < /dev/null \
    > /dev/null

vp9_input="${project_root}/tests/fixtures/valid-vp9-opus.ts"
av1_input="${project_root}/tests/fixtures/valid-av1-aac.ts"
vp9_output="${temporary_directory}/vp9-opus-runtime.ts"
av1_output="${temporary_directory}/av1-aac-runtime.ts"

run_hardened_runtime \
    --video-codec vp9 \
    --audio-codec opus \
    < "${vp9_input}" \
    > "${vp9_output}"
assert_semantic_transport_output \
    vp9-opus \
    "${vp9_input}" \
    "${vp9_output}" \
    VP09 \
    KTVB \
    Opus

run_hardened_runtime \
    --video-codec av1 \
    --audio-codec aac \
    --transport-rate-kbps 2200 \
    < "${av1_input}" \
    > "${av1_output}"
assert_semantic_transport_output \
    av1-aac \
    "${av1_input}" \
    "${av1_output}" \
    AV01

printf 'Runtime image provenance, boundary, and hardening tests passed: %s\n' \
    "${runtime_image}"
