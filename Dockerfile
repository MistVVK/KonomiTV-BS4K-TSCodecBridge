# syntax=docker/dockerfile:1.7@sha256:a57df69d0ea827fb7266491f2813635de6f17269be881f696fbfdf2d83dda33e
# SPDX-License-Identifier: 0BSD

ARG UBUNTU_IMAGE=ubuntu:22.04@sha256:0e0a0fc6d18feda9db1590da249ac93e8d5abfea8f4c3c0c849ce512b5ef8982
ARG SOURCE_COMMIT
ARG SOURCE_TREE_SHA256

FROM ${UBUNTU_IMAGE} AS toolchain

ARG SOURCE_COMMIT
ARG SOURCE_TREE_SHA256
ARG CA_CERTIFICATES_PACKAGE_VERSION=20260601~22.04.1
ARG CA_CERTIFICATES_PACKAGE_SHA256=6e8cdcc8c86103acd4fc14649eac62ff2037108389074a7b167567af33c32245
ARG CURL_PACKAGE_VERSION=7.81.0-1ubuntu1.25
ARG CURL_PACKAGE_SHA256=d2d7eca9dc94e93a752589376b2da4c64b6bc300d175eb325cc3aed0389a72df
ARG MAKE_PACKAGE_VERSION=4.3-4.1build1
ARG MAKE_PACKAGE_SHA256=080b79a1a1623a2e6c6eead37d62b15fdf2c3dbfeafe8ecf5e31c54eb09eadcc
ARG TAR_PACKAGE_VERSION=1.34+dfsg-1ubuntu0.1.22.04.6
ARG TAR_PACKAGE_SHA256=9e7a93d96b2c3ed82aa5959933bec3e5082c264b0ae45e86ab450f83886faace
ARG NALA_PACKAGE_VERSION=0.11.1~bpo22.04.1
ARG NALA_PACKAGE_SHA256=d0124a63b3040de7dd8fb72806864aafe1015ea6b3215d54fa2de43a2d829c0a
ARG SBCL_PACKAGE_VERSION=2:2.1.11-1
ARG SBCL_PACKAGE_SHA256=683c3c5e6d25fdc37de1b3fd09f1f40e2af2ede21d6de2569549d2b16776a77d
ARG CL_SWANK_PACKAGE_VERSION=2:2.26.1+dfsg-2
ARG CL_SWANK_PACKAGE_SHA256=3e69cd7578ff4b8b72ea6141bbe804b8efc291bf4017088bde1e7d648570154b
ARG SBLINT_COMMIT=1037296f604c3210ce073a53539d4ae95b0c2f8c
ARG SBLINT_SHA256=4d7eec72c1322fd16dc444eb62a7981111965bfce72bf6161bf69e2a6786fd3e
ARG MALLET_VERSION=0.9.2
ARG MALLET_SHA256=95d28cc93bf22dd529ea084073478047edb59e690a1a029b85bfbe5579930808
ARG CLI_VERSION=0.1.0
ARG TS_MAPPING_VERSION=1
ENV DEBIAN_FRONTEND=noninteractive

# provenance引数は既定値を持たせず、toolchain取得より前に完全SHAだけを受け付ける。
COPY ./scripts/record-docker-builder-packages.sh \
     ./scripts/extract-docker-archive.sh \
     /usr/local/libexec/tscodecbridge/
RUN chmod 0755 /usr/local/libexec/tscodecbridge/*.sh && \
    test "${#SOURCE_COMMIT}" -eq 40 && \
    test "${SOURCE_COMMIT}" != '0000000000000000000000000000000000000000' && \
    ! printf '%s' "${SOURCE_COMMIT}" | grep -Eq '[^0-9a-f]' && \
    test "${#SOURCE_TREE_SHA256}" -eq 64 && \
    ! printf '%s' "${SOURCE_TREE_SHA256}" | grep -Eq '[^0-9a-f]'

# compilerとlint依存はJammy公式archiveからbuilderにだけ導入し、導入前後の全差分を記録する。
RUN test "$(uname -m)" = x86_64 && \
    . /etc/os-release && \
    test "${VERSION_CODENAME}" = jammy && \
    apt_sources="$(grep -RhsE '^[[:space:]]*deb[[:space:]]' \
        /etc/apt/sources.list /etc/apt/sources.list.d 2>/dev/null)" && \
    test -n "${apt_sources}" && \
    if printf '%s\n' "${apt_sources}" | \
        grep -Ev 'https?://(archive|security)\.ubuntu\.com/ubuntu/?([[:space:]]|$)'; then \
        echo 'Non-official apt source is forbidden in the Bridge builder.' >&2; \
        exit 1; \
    fi && \
    install -d -m 0755 /build/provenance && \
    dpkg-query --show --showformat='${binary:Package}\t${Version}\n' | \
        sort > /build/provenance/packages-before.tsv && \
    apt-get update && \
    install -d -m 0755 /tmp/direct-packages && \
    cd /tmp/direct-packages && \
    apt-get download \
        "ca-certificates=${CA_CERTIFICATES_PACKAGE_VERSION}" \
        "cl-swank=${CL_SWANK_PACKAGE_VERSION}" \
        "curl=${CURL_PACKAGE_VERSION}" \
        "make=${MAKE_PACKAGE_VERSION}" \
        "nala=${NALA_PACKAGE_VERSION}" \
        "sbcl=${SBCL_PACKAGE_VERSION}" \
        "tar=${TAR_PACKAGE_VERSION}" && \
    verify_package() { \
        package_name="${1}"; \
        expected_sha256="${2}"; \
        package_path="$(find /tmp/direct-packages -maxdepth 1 -type f \
            -name "${package_name}_*.deb" -print -quit)"; \
        test -n "${package_path}"; \
        printf '%s  %s\n' "${expected_sha256}" "${package_path}" | \
            sha256sum --check --strict -; \
    }; \
    verify_package ca-certificates "${CA_CERTIFICATES_PACKAGE_SHA256}" && \
    verify_package cl-swank "${CL_SWANK_PACKAGE_SHA256}" && \
    verify_package curl "${CURL_PACKAGE_SHA256}" && \
    verify_package make "${MAKE_PACKAGE_SHA256}" && \
    verify_package nala "${NALA_PACKAGE_SHA256}" && \
    verify_package sbcl "${SBCL_PACKAGE_SHA256}" && \
    verify_package tar "${TAR_PACKAGE_SHA256}" && \
    apt-get install -y --no-install-recommends /tmp/direct-packages/*.deb && \
    test "$(dpkg-query --showformat='${Version}' --show ca-certificates)" = "${CA_CERTIFICATES_PACKAGE_VERSION}" && \
    test "$(dpkg-query --showformat='${Version}' --show curl)" = "${CURL_PACKAGE_VERSION}" && \
    test "$(dpkg-query --showformat='${Version}' --show make)" = "${MAKE_PACKAGE_VERSION}" && \
    test "$(dpkg-query --showformat='${Version}' --show tar)" = "${TAR_PACKAGE_VERSION}" && \
    test "$(dpkg-query --showformat='${Version}' --show nala)" = "${NALA_PACKAGE_VERSION}" && \
    test "$(dpkg-query --showformat='${Version}' --show sbcl)" = "${SBCL_PACKAGE_VERSION}" && \
    test "$(dpkg-query --showformat='${Version}' --show cl-swank)" = "${CL_SWANK_PACKAGE_VERSION}" && \
    printf 'Ubuntu codename: %s\n' "${VERSION_CODENAME}" && \
    apt-cache policy ca-certificates curl make tar nala sbcl cl-swank && \
    apt-cache policy sbcl cl-swank | \
        grep -Eq 'https?://archive\.ubuntu\.com/ubuntu[[:space:]]+jammy/universe[[:space:]]+amd64[[:space:]]+Packages' && \
    dpkg-query --show \
        --showformat='${Package} ${Version} ${source:Package}\n' \
        sbcl cl-swank && \
    /usr/local/libexec/tscodecbridge/record-docker-builder-packages.sh \
        /build/provenance/packages-before.tsv \
        /build/provenance/builder-packages.tsv && \
    test -s /build/provenance/builder-packages.tsv && \
    apt-get clean && \
    rm -rf /tmp/direct-packages /var/lib/apt/lists/*

# lint toolは固定archiveを検証して一時toolchain領域へ展開する。
RUN mkdir -p /opt/sblint /opt/mallet && \
    curl --proto '=https' --tlsv1.2 --fail --location \
        --silent --show-error --retry 3 --retry-all-errors \
        "https://codeload.github.com/cxxxr/sblint/tar.gz/${SBLINT_COMMIT}" \
        --output /tmp/sblint.tar.gz && \
    printf '%s  %s\n' \
        "${SBLINT_SHA256}" \
        /tmp/sblint.tar.gz | sha256sum --check --strict - && \
    /usr/local/libexec/tscodecbridge/extract-docker-archive.sh \
        /tmp/sblint.tar.gz /opt/sblint && \
    curl --proto '=https' --tlsv1.2 --fail --location \
        --silent --show-error --retry 3 --retry-all-errors \
        "https://github.com/fukamachi/mallet/releases/download/${MALLET_VERSION}/mallet-${MALLET_VERSION}-linux-x86_64.tar.gz" \
        --output /tmp/mallet.tar.gz && \
    printf '%s  %s\n' \
        "${MALLET_SHA256}" \
        /tmp/mallet.tar.gz | sha256sum --check --strict - && \
    /usr/local/libexec/tscodecbridge/extract-docker-archive.sh \
        /tmp/mallet.tar.gz /opt/mallet && \
    test -f /opt/sblint/sblint.asd && \
    test "$(/opt/mallet/bin/mallet --version)" = \
        "Mallet version ${MALLET_VERSION}" && \
    rm /tmp/sblint.tar.gz /tmp/mallet.tar.gz

FROM toolchain AS builder

ARG UBUNTU_IMAGE
ARG SOURCE_COMMIT
ARG SOURCE_TREE_SHA256
ARG CA_CERTIFICATES_PACKAGE_VERSION=20260601~22.04.1
ARG CA_CERTIFICATES_PACKAGE_SHA256=6e8cdcc8c86103acd4fc14649eac62ff2037108389074a7b167567af33c32245
ARG CURL_PACKAGE_VERSION=7.81.0-1ubuntu1.25
ARG CURL_PACKAGE_SHA256=d2d7eca9dc94e93a752589376b2da4c64b6bc300d175eb325cc3aed0389a72df
ARG MAKE_PACKAGE_VERSION=4.3-4.1build1
ARG MAKE_PACKAGE_SHA256=080b79a1a1623a2e6c6eead37d62b15fdf2c3dbfeafe8ecf5e31c54eb09eadcc
ARG TAR_PACKAGE_VERSION=1.34+dfsg-1ubuntu0.1.22.04.6
ARG TAR_PACKAGE_SHA256=9e7a93d96b2c3ed82aa5959933bec3e5082c264b0ae45e86ab450f83886faace
ARG NALA_PACKAGE_VERSION=0.11.1~bpo22.04.1
ARG NALA_PACKAGE_SHA256=d0124a63b3040de7dd8fb72806864aafe1015ea6b3215d54fa2de43a2d829c0a
ARG SBCL_PACKAGE_VERSION=2:2.1.11-1
ARG SBCL_PACKAGE_SHA256=683c3c5e6d25fdc37de1b3fd09f1f40e2af2ede21d6de2569549d2b16776a77d
ARG CL_SWANK_PACKAGE_VERSION=2:2.26.1+dfsg-2
ARG CL_SWANK_PACKAGE_SHA256=3e69cd7578ff4b8b72ea6141bbe804b8efc291bf4017088bde1e7d648570154b
ARG SBLINT_COMMIT=1037296f604c3210ce073a53539d4ae95b0c2f8c
ARG SBLINT_SHA256=4d7eec72c1322fd16dc444eb62a7981111965bfce72bf6161bf69e2a6786fd3e
ARG MALLET_VERSION=0.9.2
ARG MALLET_SHA256=95d28cc93bf22dd529ea084073478047edb59e690a1a029b85bfbe5579930808
ARG CLI_VERSION=0.1.0
ARG TS_MAPPING_VERSION=1
WORKDIR /work
COPY . /work/

ENV SBLINT_SOURCE_DIR=/opt/sblint
ENV MALLET_BINARY=/opt/mallet/bin/mallet

# COPYしたcontextを宣言commitのgit archive由来digestと照合し、dirty/生成物混入を拒否する。
RUN observed_source_tree_sha256="$(scripts/docker-source-tree-digest.sh /work)" && \
    printf 'Source commit: %s\n' "${SOURCE_COMMIT}" && \
    printf 'Expected source tree SHA-256: %s\n' "${SOURCE_TREE_SHA256}" && \
    printf 'Observed source tree SHA-256: %s\n' "${observed_source_tree_sha256}" && \
    test "${observed_source_tree_sha256}" = "${SOURCE_TREE_SHA256}" && \
    scripts/test-docker-distribution-static.sh && \
    make ci && \
    test "$(build/ts-codec-bridge.elf --version)" = "${CLI_VERSION}" && \
    test "$(build/ts-codec-bridge.elf --mapping-version)" = "${TS_MAPPING_VERSION}" && \
    sha256sum build/ts-codec-bridge.elf && \
    ldd build/ts-codec-bridge.elf

# lddで解決された許可libraryだけを実体化し、全provenanceとhashをruntime manifestへ固定する。
RUN SOURCE_COMMIT="${SOURCE_COMMIT}" \
    SOURCE_TREE_SHA256="${SOURCE_TREE_SHA256}" \
    UBUNTU_IMAGE="${UBUNTU_IMAGE}" \
    CA_CERTIFICATES_PACKAGE_VERSION="${CA_CERTIFICATES_PACKAGE_VERSION}" \
    CA_CERTIFICATES_PACKAGE_SHA256="${CA_CERTIFICATES_PACKAGE_SHA256}" \
    CURL_PACKAGE_VERSION="${CURL_PACKAGE_VERSION}" \
    CURL_PACKAGE_SHA256="${CURL_PACKAGE_SHA256}" \
    MAKE_PACKAGE_VERSION="${MAKE_PACKAGE_VERSION}" \
    MAKE_PACKAGE_SHA256="${MAKE_PACKAGE_SHA256}" \
    TAR_PACKAGE_VERSION="${TAR_PACKAGE_VERSION}" \
    TAR_PACKAGE_SHA256="${TAR_PACKAGE_SHA256}" \
    NALA_PACKAGE_VERSION="${NALA_PACKAGE_VERSION}" \
    NALA_PACKAGE_SHA256="${NALA_PACKAGE_SHA256}" \
    SBCL_PACKAGE_VERSION="${SBCL_PACKAGE_VERSION}" \
    SBCL_PACKAGE_SHA256="${SBCL_PACKAGE_SHA256}" \
    CL_SWANK_PACKAGE_VERSION="${CL_SWANK_PACKAGE_VERSION}" \
    CL_SWANK_PACKAGE_SHA256="${CL_SWANK_PACKAGE_SHA256}" \
    SBLINT_COMMIT="${SBLINT_COMMIT}" \
    SBLINT_SHA256="${SBLINT_SHA256}" \
    MALLET_VERSION="${MALLET_VERSION}" \
    MALLET_SHA256="${MALLET_SHA256}" \
    CLI_VERSION="${CLI_VERSION}" \
    TS_MAPPING_VERSION="${TS_MAPPING_VERSION}" \
    scripts/package-docker-runtime.sh \
        /work \
        /build/provenance/builder-packages.tsv \
        /runtime && \
    test -s /runtime/usr/share/doc/ts-codec-bridge/Runtime-Manifest.tsv

FROM scratch

ARG SOURCE_COMMIT
ARG SOURCE_TREE_SHA256
LABEL org.opencontainers.image.title="KonomiTV-BS4K TS Codec Bridge" \
      org.opencontainers.image.licenses="NOASSERTION" \
      org.opencontainers.image.revision="${SOURCE_COMMIT}" \
      cc.konomi.konomitv-bs4k.source-tree-sha256="${SOURCE_TREE_SHA256}"

# 保存core、lddで解決したlibrary、表示、runtime manifestだけを実行stageへ移す。
COPY --from=builder /runtime/ /

RUN ["/usr/local/bin/ts-codec-bridge.elf", "--version"]
RUN ["/usr/local/bin/ts-codec-bridge.elf", "--mapping-version"]

USER 65532:65532

ENTRYPOINT ["/usr/local/bin/ts-codec-bridge.elf"]
