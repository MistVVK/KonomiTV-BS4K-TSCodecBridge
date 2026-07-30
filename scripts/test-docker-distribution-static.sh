#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

script_directory="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repository_root="$(cd -- "${script_directory}/.." && pwd)"
dockerfile="${repository_root}/Dockerfile"
dependencies="${repository_root}/DEPENDENCIES.md"
temporary_directory="$(mktemp -d /tmp/tscodecbridge-docker-static.XXXXXX)"

cleanup() {
    rm -rf -- "${temporary_directory}"
}
trap cleanup EXIT

for script in \
    build-runtime-image.sh \
    docker-source-tree-digest.sh \
    extract-docker-archive.sh \
    final-release-check.sh \
    package-docker-runtime.sh \
    record-docker-builder-packages.sh \
    test-docker-distribution-static.sh \
    test-runtime-image.sh
do
    bash -n "${script_directory}/${script}"
done

grep -Fxq 'ARG SOURCE_COMMIT' "${dockerfile}"
grep -Fxq 'ARG SOURCE_TREE_SHA256' "${dockerfile}"
grep -Fxq \
    '# syntax=docker/dockerfile:1.7@sha256:a57df69d0ea827fb7266491f2813635de6f17269be881f696fbfdf2d83dda33e' \
    "${dockerfile}"
for package_name in ca-certificates cl-swank curl make nala sbcl tar; do
    grep -Fq "verify_package ${package_name}" "${dockerfile}"
done
grep -Fq 'apt-get install -y --no-install-recommends /tmp/direct-packages/*.deb' \
    "${dockerfile}"
grep -Fq 'sha256sum --check --strict -' "${dockerfile}"
if grep -Eq '^ARG SOURCE_COMMIT=|^ARG SOURCE_TREE_SHA256=' "${dockerfile}"; then
    printf 'Docker provenance arguments must not have defaults.\n' >&2
    exit 1
fi
if grep -Fq 'working-tree' "${dockerfile}"; then
    printf 'Dockerfile contains the forbidden working-tree provenance sentinel.\n' >&2
    exit 1
fi

digest_gate_line="$(
    grep -nF 'docker-source-tree-digest.sh /work' "${dockerfile}" |
        cut -d: -f1
)"
build_gate_line="$(
    grep -nF 'make ci' "${dockerfile}" |
        cut -d: -f1
)"
test -n "${digest_gate_line}"
test -n "${build_gate_line}"
test "${digest_gate_line}" -lt "${build_gate_line}"

digest_left="${temporary_directory}/digest-left"
digest_right="${temporary_directory}/digest-right"
mkdir -p "${digest_left}" "${digest_right}"
printf '%s\n' 'regular' > "${digest_left}/regular.txt"
printf '%s\n' 'regular' > "${digest_right}/regular.txt"
printf '%s\n' 'executable' > "${digest_left}/executable.sh"
printf '%s\n' 'executable' > "${digest_right}/executable.sh"
chmod 0664 "${digest_left}/regular.txt"
chmod 0644 "${digest_right}/regular.txt"
chmod 0775 "${digest_left}/executable.sh"
chmod 0755 "${digest_right}/executable.sh"
test "$("${script_directory}/docker-source-tree-digest.sh" "${digest_left}")" = \
    "$("${script_directory}/docker-source-tree-digest.sh" "${digest_right}")"
chmod 0644 "${digest_right}/executable.sh"
test "$("${script_directory}/docker-source-tree-digest.sh" "${digest_left}")" != \
    "$("${script_directory}/docker-source-tree-digest.sh" "${digest_right}")"

grep -Fq 'record-docker-builder-packages.sh' "${dockerfile}"
grep -Fq 'package-docker-runtime.sh' "${dockerfile}"
grep -Fq '/usr/share/doc/ts-codec-bridge/Runtime-Manifest.tsv' "${dockerfile}"
grep -Fq "org.opencontainers.image.revision=\"\${SOURCE_COMMIT}\"" "${dockerfile}"
grep -Fq "cc.konomi.konomitv-bs4k.source-tree-sha256=\"\${SOURCE_TREE_SHA256}\"" "${dockerfile}"

build_wrapper="${script_directory}/build-runtime-image.sh"
grep -Fq 'status --porcelain=v1 --untracked-files=all' "${build_wrapper}"
grep -Fq "archive --format=tar \"\${source_commit}\"" "${build_wrapper}"
grep -Fq -- "--build-arg \"SOURCE_COMMIT=\${source_commit}\"" "${build_wrapper}"
grep -Fq -- "--build-arg \"SOURCE_TREE_SHA256=\${source_tree_sha256}\"" "${build_wrapper}"
grep -Fq 'Unsupported Docker build argument for canonical wrapper' "${build_wrapper}"

assert_wrapper_rejects() {
    local case_name="${1}"
    shift
    if "${build_wrapper}" "$@" \
        > "${temporary_directory}/${case_name}.stdout" \
        2> "${temporary_directory}/${case_name}.stderr"
    then
        printf 'Canonical wrapper unexpectedly accepted unsafe arguments: %s\n' \
            "${case_name}" >&2
        exit 1
    fi
    grep -Fq \
        'Unsupported Docker build argument for canonical wrapper:' \
        "${temporary_directory}/${case_name}.stderr"
}

assert_wrapper_rejects dockerfile --file Dockerfile
assert_wrapper_rejects target --target builder
assert_wrapper_rejects build-context --build-context dependency=.
assert_wrapper_rejects build-arg --build-arg SOURCE_COMMIT=0000000000000000000000000000000000000000
assert_wrapper_rejects label --label org.opencontainers.image.revision=forged

archive_source="${temporary_directory}/archive-source"
valid_destination="${temporary_directory}/valid-destination"
mkdir -p \
    "${archive_source}/fixture-root" \
    "${valid_destination}"
printf '%s\n' 'regular archive payload' \
    > "${archive_source}/fixture-root/payload.txt"
tar --create --gzip \
    --file "${temporary_directory}/valid.tar.gz" \
    --directory "${archive_source}" \
    fixture-root
"${script_directory}/extract-docker-archive.sh" \
    "${temporary_directory}/valid.tar.gz" \
    "${valid_destination}"
cmp \
    "${archive_source}/fixture-root/payload.txt" \
    "${valid_destination}/payload.txt"

assert_archive_rejects() {
    local case_name="${1}"
    local archive_path="${2}"
    local destination="${temporary_directory}/${case_name}-destination"
    mkdir "${destination}"
    if "${script_directory}/extract-docker-archive.sh" \
        "${archive_path}" \
        "${destination}" \
        > "${temporary_directory}/${case_name}.stdout" \
        2> "${temporary_directory}/${case_name}.stderr"
    then
        printf 'Archive extractor unexpectedly accepted entry type: %s\n' \
            "${case_name}" >&2
        exit 1
    fi
    grep -Fq 'Archive contains an unsupported non-regular entry:' \
        "${temporary_directory}/${case_name}.stderr"
}

fifo_source="${temporary_directory}/fifo-source"
mkdir -p "${fifo_source}/fixture-root"
mkfifo "${fifo_source}/fixture-root/unsupported.fifo"
tar --create --gzip \
    --file "${temporary_directory}/fifo.tar.gz" \
    --directory "${fifo_source}" \
    fixture-root
assert_archive_rejects fifo "${temporary_directory}/fifo.tar.gz"

symlink_source="${temporary_directory}/symlink-source"
mkdir -p "${symlink_source}/fixture-root"
printf '%s\n' 'symlink target' \
    > "${symlink_source}/fixture-root/target.txt"
ln -s target.txt "${symlink_source}/fixture-root/unsupported.symlink"
tar --create --gzip \
    --file "${temporary_directory}/symlink.tar.gz" \
    --directory "${symlink_source}" \
    fixture-root
assert_archive_rejects symlink "${temporary_directory}/symlink.tar.gz"

hardlink_source="${temporary_directory}/hardlink-source"
mkdir -p "${hardlink_source}/fixture-root"
printf '%s\n' 'hardlink target' \
    > "${hardlink_source}/fixture-root/target.txt"
ln \
    "${hardlink_source}/fixture-root/target.txt" \
    "${hardlink_source}/fixture-root/unsupported.hardlink"
tar --create --gzip \
    --file "${temporary_directory}/hardlink.tar.gz" \
    --directory "${hardlink_source}" \
    fixture-root
assert_archive_rejects hardlink "${temporary_directory}/hardlink.tar.gz"

scratch_section="$(
    awk '
        /^FROM scratch/ {
            in_scratch = 1
        }
        in_scratch {
            print
        }
    ' "${dockerfile}"
)"
printf '%s\n' "${scratch_section}" | grep -Fq 'COPY --from=builder /runtime/ /'
if printf '%s\n' "${scratch_section}" |
    grep -Eq 'COPY --from=builder /(work|build|opt)/'
then
    printf 'Final Docker stage copies builder source or toolchain content.\n' >&2
    exit 1
fi

grep -Fq 'scripts/build-runtime-image.sh' "${dependencies}"
grep -Fq 'SOURCE_TREE_SHA256' "${dependencies}"
grep -Fq 'Runtime-Manifest.tsv' "${dependencies}"
grep -Fq 'BUILDER_PACKAGE' "${dependencies}"
grep -Fq 'RUNTIME_LIBRARY' "${dependencies}"
grep -Fq 'Dockerfile 単体では' "${dependencies}"
grep -Fq '正式な配布 build では必ず上記 wrapper を使用' "${dependencies}"

printf '%s\n' 'Docker distribution static gates passed.'
