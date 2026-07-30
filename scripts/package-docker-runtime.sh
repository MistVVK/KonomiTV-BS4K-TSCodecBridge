#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

source_root="${1:-}"
builder_packages_path="${2:-}"
runtime_root="${3:-}"

if [[ -z "${source_root}" || -z "${builder_packages_path}" || -z "${runtime_root}" ]]; then
    printf 'Usage: %s SOURCE_ROOT BUILDER_PACKAGES_TSV RUNTIME_ROOT\n' "${0##*/}" >&2
    exit 2
fi

: "${SOURCE_COMMIT:?SOURCE_COMMIT is required}"
: "${SOURCE_TREE_SHA256:?SOURCE_TREE_SHA256 is required}"
: "${UBUNTU_IMAGE:?UBUNTU_IMAGE is required}"
: "${CA_CERTIFICATES_PACKAGE_VERSION:?CA_CERTIFICATES_PACKAGE_VERSION is required}"
: "${CA_CERTIFICATES_PACKAGE_SHA256:?CA_CERTIFICATES_PACKAGE_SHA256 is required}"
: "${CURL_PACKAGE_VERSION:?CURL_PACKAGE_VERSION is required}"
: "${CURL_PACKAGE_SHA256:?CURL_PACKAGE_SHA256 is required}"
: "${MAKE_PACKAGE_VERSION:?MAKE_PACKAGE_VERSION is required}"
: "${MAKE_PACKAGE_SHA256:?MAKE_PACKAGE_SHA256 is required}"
: "${TAR_PACKAGE_VERSION:?TAR_PACKAGE_VERSION is required}"
: "${TAR_PACKAGE_SHA256:?TAR_PACKAGE_SHA256 is required}"
: "${NALA_PACKAGE_VERSION:?NALA_PACKAGE_VERSION is required}"
: "${NALA_PACKAGE_SHA256:?NALA_PACKAGE_SHA256 is required}"
: "${SBCL_PACKAGE_VERSION:?SBCL_PACKAGE_VERSION is required}"
: "${SBCL_PACKAGE_SHA256:?SBCL_PACKAGE_SHA256 is required}"
: "${CL_SWANK_PACKAGE_VERSION:?CL_SWANK_PACKAGE_VERSION is required}"
: "${CL_SWANK_PACKAGE_SHA256:?CL_SWANK_PACKAGE_SHA256 is required}"
: "${SBLINT_COMMIT:?SBLINT_COMMIT is required}"
: "${SBLINT_SHA256:?SBLINT_SHA256 is required}"
: "${MALLET_VERSION:?MALLET_VERSION is required}"
: "${MALLET_SHA256:?MALLET_SHA256 is required}"
: "${CLI_VERSION:?CLI_VERSION is required}"
: "${TS_MAPPING_VERSION:?TS_MAPPING_VERSION is required}"

if [[ ! "${SOURCE_COMMIT}" =~ ^[0-9a-f]{40}$ || "${SOURCE_COMMIT}" == '0000000000000000000000000000000000000000' ]]; then
    printf 'SOURCE_COMMIT must be a nonzero 40-character lowercase commit SHA.\n' >&2
    exit 1
fi
if [[ ! "${SOURCE_TREE_SHA256}" =~ ^[0-9a-f]{64}$ ]]; then
    printf 'SOURCE_TREE_SHA256 must be a 64-character lowercase SHA-256.\n' >&2
    exit 1
fi
test -s "${builder_packages_path}"
awk -F '\t' '
    NF != 4 || $1 != "BUILDER_PACKAGE" {
        invalid = 1
    }
    $4 !~ /^https?:\/\/(archive|security)\.ubuntu\.com\/ubuntu / {
        invalid = 1
    }
    {
        package_count += 1
    }
    END {
        exit invalid || package_count == 0
    }
' "${builder_packages_path}"

bridge_binary="${source_root}/build/ts-codec-bridge.elf"
test -x "${bridge_binary}"
test -s "${source_root}/LICENSE"
test "$("${bridge_binary}" --version)" = "${CLI_VERSION}"
test "$("${bridge_binary}" --mapping-version)" = "${TS_MAPPING_VERSION}"

ldd_output="$(ldd "${bridge_binary}")"
printf '%s\n' "${ldd_output}"
if printf '%s\n' "${ldd_output}" | grep -Fq 'not found'; then
    printf 'Bridge executable has an unresolved dynamic dependency.\n' >&2
    exit 1
fi

mapfile -t resolved_libraries < <(
    printf '%s\n' "${ldd_output}" |
        awk '
            /=> \// {
                print $3
                next
            }
            /^[[:space:]]*\// {
                print $1
            }
        ' |
        LC_ALL=C sort -u
)
if [[ "${#resolved_libraries[@]}" -eq 0 ]]; then
    printf 'Bridge executable reported no resolved dynamic libraries.\n' >&2
    exit 1
fi

test ! -e "${runtime_root}"
install -d -m 0755 \
    "${runtime_root}/usr/local/bin" \
    "${runtime_root}/usr/share/doc/ts-codec-bridge"
install -m 0755 "${bridge_binary}" \
    "${runtime_root}/usr/local/bin/ts-codec-bridge.elf"
install -m 0644 "${source_root}/LICENSE" \
    "${runtime_root}/usr/share/doc/ts-codec-bridge/LICENSE"
install -m 0644 /usr/share/doc/sbcl/copyright \
    "${runtime_root}/usr/share/doc/ts-codec-bridge/SBCL-COPYRIGHT"
install -m 0644 /usr/share/doc/libc6/copyright \
    "${runtime_root}/usr/share/doc/ts-codec-bridge/GLIBC-COPYRIGHT"
install -m 0644 /usr/share/doc/zlib1g/copyright \
    "${runtime_root}/usr/share/doc/ts-codec-bridge/ZLIB-COPYRIGHT"
for common_license in Apache-2.0 GPL-2 LGPL-2.1 GFDL-1.3; do
    common_license_path="/usr/share/common-licenses/${common_license}"
    test -s "${common_license_path}"
    install -m 0644 "${common_license_path}" \
        "${runtime_root}/usr/share/doc/ts-codec-bridge/${common_license}-LICENSE"
done

runtime_libraries_path="${runtime_root}/runtime-libraries.tsv"
: > "${runtime_libraries_path}"
for resolved_library in "${resolved_libraries[@]}"; do
    library_name="${resolved_library##*/}"
    case "${library_name}" in
        ld-linux-x86-64.so.2 | libc.so.6 | libdl.so.2 | libm.so.6 | libpthread.so.0)
            package_name='libc6'
            ;;
        libz.so.1)
            package_name='zlib1g'
            ;;
        *)
            printf 'Unexpected Bridge dynamic dependency: %s\n' "${resolved_library}" >&2
            exit 1
            ;;
    esac
    case "${resolved_library}" in
        /lib/* | /lib64/* | /usr/lib/*)
            ;;
        *)
            printf 'Dynamic dependency escaped system library roots: %s\n' "${resolved_library}" >&2
            exit 1
            ;;
    esac

    destination="${runtime_root}${resolved_library}"
    install -d -m 0755 "$(dirname -- "${destination}")"
    cp --dereference --preserve=mode -- "${resolved_library}" "${destination}"
    library_sha256="$(sha256sum -- "${destination}" | awk '{ print $1 }')"
    package_version="$(dpkg-query --showformat='${Version}' --show "${package_name}")"
    printf 'RUNTIME_LIBRARY\t%s\t%s\t%s\t%s\n' \
        "${resolved_library}" \
        "${library_sha256}" \
        "${package_name}" \
        "${package_version}" |
        tee -a "${runtime_libraries_path}"
done

glibc_package_version="$(dpkg-query --showformat='${Version}' --show libc6)"
zlib_package_version="$(dpkg-query --showformat='${Version}' --show zlib1g)"
glibc_library_path="$(
    awk -F '\t' '$4 == "libc6" && $2 ~ /\/libc\.so\.6$/ { print $2; exit }' \
        "${runtime_libraries_path}"
)"
zlib_library_path="$(
    awk -F '\t' '$4 == "zlib1g" && $2 ~ /\/libz\.so\.1$/ { print $2; exit }' \
        "${runtime_libraries_path}"
)"
test -n "${glibc_library_path}"
test -n "${zlib_library_path}"
glibc_library_sha256="$(
    awk -F '\t' -v path="${glibc_library_path}" '$2 == path { print $3 }' \
        "${runtime_libraries_path}"
)"
zlib_library_sha256="$(
    awk -F '\t' -v path="${zlib_library_path}" '$2 == path { print $3 }' \
        "${runtime_libraries_path}"
)"
executable_sha256="$(
    sha256sum "${runtime_root}/usr/local/bin/ts-codec-bridge.elf" |
        awk '{ print $1 }'
)"

runtime_manifest="${runtime_root}/usr/share/doc/ts-codec-bridge/Runtime-Manifest.tsv"
{
    printf 'FORMAT_VERSION\t1\n'
    printf 'SOURCE_REPOSITORY\thttps://github.com/MistVVK/KonomiTV-BS4K-TSCodecBridge\n'
    printf 'SOURCE_COMMIT\t%s\n' "${SOURCE_COMMIT}"
    printf 'SOURCE_TREE_SHA256\t%s\n' "${SOURCE_TREE_SHA256}"
    printf 'UBUNTU_IMAGE\t%s\n' "${UBUNTU_IMAGE}"
    printf 'UBUNTU_CODENAME\tjammy\n'
    printf 'CA_CERTIFICATES_PACKAGE_VERSION\t%s\n' \
        "${CA_CERTIFICATES_PACKAGE_VERSION}"
    printf 'CA_CERTIFICATES_PACKAGE_SHA256\t%s\n' \
        "${CA_CERTIFICATES_PACKAGE_SHA256}"
    printf 'CURL_PACKAGE_VERSION\t%s\n' "${CURL_PACKAGE_VERSION}"
    printf 'CURL_PACKAGE_SHA256\t%s\n' "${CURL_PACKAGE_SHA256}"
    printf 'MAKE_PACKAGE_VERSION\t%s\n' "${MAKE_PACKAGE_VERSION}"
    printf 'MAKE_PACKAGE_SHA256\t%s\n' "${MAKE_PACKAGE_SHA256}"
    printf 'TAR_PACKAGE_VERSION\t%s\n' "${TAR_PACKAGE_VERSION}"
    printf 'TAR_PACKAGE_SHA256\t%s\n' "${TAR_PACKAGE_SHA256}"
    printf 'NALA_PACKAGE_VERSION\t%s\n' "${NALA_PACKAGE_VERSION}"
    printf 'NALA_PACKAGE_SHA256\t%s\n' "${NALA_PACKAGE_SHA256}"
    printf 'SBCL_PACKAGE_VERSION\t%s\n' "${SBCL_PACKAGE_VERSION}"
    printf 'SBCL_PACKAGE_SHA256\t%s\n' "${SBCL_PACKAGE_SHA256}"
    printf 'CL_SWANK_PACKAGE_VERSION\t%s\n' "${CL_SWANK_PACKAGE_VERSION}"
    printf 'CL_SWANK_PACKAGE_SHA256\t%s\n' "${CL_SWANK_PACKAGE_SHA256}"
    printf 'SBLINT_COMMIT\t%s\n' "${SBLINT_COMMIT}"
    printf 'SBLINT_SOURCE_SHA256\t%s\n' "${SBLINT_SHA256}"
    printf 'SBLINT_SOURCE_URL\thttps://codeload.github.com/cxxxr/sblint/tar.gz/%s\n' \
        "${SBLINT_COMMIT}"
    printf 'MALLET_VERSION\t%s\n' "${MALLET_VERSION}"
    printf 'MALLET_ARCHIVE_SHA256\t%s\n' "${MALLET_SHA256}"
    printf 'MALLET_ARCHIVE_URL\thttps://github.com/fukamachi/mallet/releases/download/%s/mallet-%s-linux-x86_64.tar.gz\n' \
        "${MALLET_VERSION}" \
        "${MALLET_VERSION}"
    printf 'CLI_VERSION\t%s\n' "${CLI_VERSION}"
    printf 'TS_MAPPING_VERSION\t%s\n' "${TS_MAPPING_VERSION}"
    printf 'BUILD_GLIBC_PACKAGE_VERSION\t%s\n' "${glibc_package_version}"
    printf 'BUILD_GLIBC_LIBRARY_SHA256\t%s\n' "${glibc_library_sha256}"
    printf 'BUILD_ZLIB_PACKAGE_VERSION\t%s\n' "${zlib_package_version}"
    printf 'BUILD_ZLIB_LIBRARY_SHA256\t%s\n' "${zlib_library_sha256}"
    printf 'EXECUTABLE_SHA256\t%s\n' "${executable_sha256}"
    for common_license in Apache-2.0 GPL-2 LGPL-2.1 GFDL-1.3; do
        common_license_path="/usr/share/common-licenses/${common_license}"
        printf 'COMMON_LICENSE\t%s-LICENSE\t%s\t%s\n' \
            "${common_license}" \
            "$(sha256sum "${common_license_path}" | awk '{ print $1 }')" \
            "${common_license_path}"
    done
    cat "${builder_packages_path}"
    cat "${runtime_libraries_path}"
} > "${runtime_manifest}"
chmod 0644 "${runtime_manifest}"
rm "${runtime_libraries_path}"

test "$(
    find "${runtime_root}" -type f \( -name '*.lisp' -o -name '*.asd' \) |
        wc -l
)" -eq 0
test ! -e "${runtime_root}/opt"
test ! -e "${runtime_root}/work"
test ! -e "${runtime_root}/build"
if grep -Fq 'working-tree' "${runtime_manifest}" || grep -Fq 'PENDING' "${runtime_manifest}"; then
    printf 'Runtime manifest contains non-reproducible provenance.\n' >&2
    exit 1
fi

printf 'Packaged Docker runtime executable SHA-256: %s\n' "${executable_sha256}"
