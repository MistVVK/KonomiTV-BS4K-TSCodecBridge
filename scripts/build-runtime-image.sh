#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

script_directory="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repository_root="$(cd -- "${script_directory}/.." && pwd)"
runtime_image="${RUNTIME_IMAGE:-konomitv-bs4k-tscodecbridge:runtime}"
temporary_directory="$(mktemp -d /tmp/tscodecbridge-docker-source.XXXXXX)"
docker_arguments=()

cleanup() {
    rm -rf -- "${temporary_directory}"
}
trap cleanup EXIT

# canonical wrapperが許可する追加引数を、provenance検査より先に確定する。
# Dockerfile、stage、context、build arg、labelを差し替えられる引数は受け付けない。
while [[ "$#" -gt 0 ]]; do
    case "${1}" in
        --no-cache | --pull)
            docker_arguments+=("${1}")
            shift
            ;;
        --progress=auto | --progress=plain | --progress=tty | --progress=rawjson)
            docker_arguments+=("${1}")
            shift
            ;;
        --progress)
            if [[ "$#" -lt 2 ]]; then
                printf 'Docker build argument requires a value: %s\n' "${1}" >&2
                exit 2
            fi
            case "${2}" in
                auto | plain | tty | rawjson)
                    docker_arguments+=("${1}" "${2}")
                    shift 2
                    ;;
                *)
                    printf 'Unsupported Docker build argument for canonical wrapper: %s %s\n' \
                        "${1}" "${2}" >&2
                    exit 2
                    ;;
            esac
            ;;
        --platform=linux/amd64)
            docker_arguments+=("${1}")
            shift
            ;;
        --platform)
            if [[ "$#" -lt 2 ]]; then
                printf 'Docker build argument requires a value: %s\n' "${1}" >&2
                exit 2
            fi
            if [[ "${2}" != 'linux/amd64' ]]; then
                printf 'Unsupported Docker build argument for canonical wrapper: %s %s\n' \
                    "${1}" "${2}" >&2
                exit 2
            fi
            docker_arguments+=("${1}" "${2}")
            shift 2
            ;;
        *)
            printf 'Unsupported Docker build argument for canonical wrapper: %s\n' \
                "${1}" >&2
            exit 2
            ;;
    esac
done

if [[ "$(git -C "${repository_root}" rev-parse --show-toplevel)" != "${repository_root}" ]]; then
    printf 'Repository root mismatch: %s\n' "${repository_root}" >&2
    exit 1
fi

# Docker provenanceはcommit済みのtreeだけを受け付ける。tracked差分、staged差分、
# untracked fileのいずれかがあれば、宣言commitと入力contextが一致しないため拒否する。
worktree_status="$(git -C "${repository_root}" status --porcelain=v1 --untracked-files=all)"
if [[ -n "${worktree_status}" ]]; then
    printf '%s\n' 'Docker runtime image requires a clean committed source tree.' >&2
    printf '%s\n' "${worktree_status}" >&2
    exit 78
fi

source_commit="$(git -C "${repository_root}" rev-parse --verify 'HEAD^{commit}')"
if [[ ! "${source_commit}" =~ ^[0-9a-f]{40}$ ]]; then
    printf 'Git returned an invalid full commit SHA: %s\n' "${source_commit}" >&2
    exit 1
fi

# expected digestはworking treeではなく宣言commitのgit archiveから計算する。
# Docker側はCOPY直後のcontextを同じalgorithmで再計算し、ignored生成物の混入も拒否する。
git -C "${repository_root}" archive --format=tar "${source_commit}" |
    tar --extract --file - --directory "${temporary_directory}"
source_tree_sha256="$(
    "${temporary_directory}/scripts/docker-source-tree-digest.sh" \
        "${temporary_directory}"
)"
if [[ ! "${source_tree_sha256}" =~ ^[0-9a-f]{64}$ ]]; then
    printf 'Source tree digest is invalid: %s\n' "${source_tree_sha256}" >&2
    exit 1
fi

printf 'Building source commit: %s\n' "${source_commit}"
printf 'Building source tree SHA-256: %s\n' "${source_tree_sha256}"
docker build \
    "${docker_arguments[@]}" \
    --build-arg "SOURCE_COMMIT=${source_commit}" \
    --build-arg "SOURCE_TREE_SHA256=${source_tree_sha256}" \
    --tag "${runtime_image}" \
    "${repository_root}"

printf 'Built provenance-verified runtime image: %s\n' "${runtime_image}"
