<!-- SPDX-License-Identifier: 0BSD -->

# 外部依存関係

このリポジトリには、以下の第三者ソースや実行形式を収録しない。
固定した外部 toolchain は Docker の build 時にだけ取得する。

| 名前 | 固定バージョン | SHA-256 | 取得元 |
| --- | --- | --- | --- |
| Dockerfile frontend | `1.7` | `a57df69d0ea827fb7266491f2813635de6f17269be881f696fbfdf2d83dda33e` | `docker.io/docker/dockerfile:1.7` |
| Ubuntu base image | 22.04 | `0e0a0fc6d18feda9db1590da249ac93e8d5abfea8f4c3c0c849ce512b5ef8982` | `docker.io/library/ubuntu:22.04` |
| ca-certificates package | `20260601~22.04.1` | `6e8cdcc8c86103acd4fc14649eac62ff2037108389074a7b167567af33c32245` | `https://archive.ubuntu.com/ubuntu/pool/main/c/ca-certificates/` |
| curl package | `7.81.0-1ubuntu1.25` | `d2d7eca9dc94e93a752589376b2da4c64b6bc300d175eb325cc3aed0389a72df` | `https://archive.ubuntu.com/ubuntu/pool/main/c/curl/` |
| make package | `4.3-4.1build1` | `080b79a1a1623a2e6c6eead37d62b15fdf2c3dbfeafe8ecf5e31c54eb09eadcc` | `https://archive.ubuntu.com/ubuntu/pool/main/m/make-dfsg/` |
| tar package | `1.34+dfsg-1ubuntu0.1.22.04.6` | `9e7a93d96b2c3ed82aa5959933bec3e5082c264b0ae45e86ab450f83886faace` | `https://archive.ubuntu.com/ubuntu/pool/main/t/tar/` |
| nala package | `0.11.1~bpo22.04.1` | `d0124a63b3040de7dd8fb72806864aafe1015ea6b3215d54fa2de43a2d829c0a` | `https://archive.ubuntu.com/ubuntu/pool/universe/n/nala/` |
| SBCL package | `2:2.1.11-1` | `683c3c5e6d25fdc37de1b3fd09f1f40e2af2ede21d6de2569549d2b16776a77d` | `http://archive.ubuntu.com/ubuntu/pool/universe/s/sbcl/` |
| cl-swank package | `2:2.26.1+dfsg-2` | `3e69cd7578ff4b8b72ea6141bbe804b8efc291bf4017088bde1e7d648570154b` | `http://archive.ubuntu.com/ubuntu/pool/universe/s/slime/` |
| SBLint source | `1037296f604c3210ce073a53539d4ae95b0c2f8c` | `4d7eec72c1322fd16dc444eb62a7981111965bfce72bf6161bf69e2a6786fd3e` | `https://codeload.github.com/cxxxr/sblint/tar.gz/1037296f604c3210ce073a53539d4ae95b0c2f8c` |
| Mallet | `0.9.2` | `95d28cc93bf22dd529ea084073478047edb59e690a1a029b85bfbe5579930808` | `https://github.com/fukamachi/mallet/releases/download/0.9.2/mallet-0.9.2-linux-x86_64.tar.gz` |
| FFmpeg source | `n8.1.2` / `38b88335f99e76ed89ff3c93f877fdefce736c13` | `2ae7e42343cfffb811d15cfe98b6d005f082595fcdf034d30a4ff90cfed9f9c6` | `https://github.com/FFmpeg/FFmpeg/archive/38b88335f99e76ed89ff3c93f877fdefce736c13.tar.gz` |
| 統合試験用 ffmpeg | `n8.1.2` | `5599c2bc61d987c7336ad4a7bf4b917fa8b260822fbb65c8b069188931339f76` | KonomiTV-BS4K の固定 FFmpeg 8.1.2 build |
| 統合試験用 ffprobe | `n8.1.2` | `877ca4ba5b63cabde0378cb39af3be12e449b6079c2d975b37aaab20f758c191` | KonomiTV-BS4K の固定 FFmpeg 8.1.2 build |

SBCL と cl-swank は Ubuntu 22.04 の公式 `universe` archive から導入する。
SBLint と Mallet は lint 専用 builder だけで使用する。
FFmpeg と ffprobe はリポジトリや runtime image へ収録せず、
`make test-ffmpeg-integration` の実行時だけ
`FFMPEG_BINARY`、`FFPROBE_BINARY` で上表の外部実行形式を指定する。
この gate は実行形式の SHA-256 と version を検証してから、VP9 / AV1 と
2本の Opus 音声を持つ TS を一時生成し、保存 Bridge 実行形式と ffprobe で
変換結果を検証する。一時 TS は終了時に削除し、fixture には収録しない。
Jammy の cl-swank は SBCL 2.1.11 で自身を初回compileすると上流由来の
warningを通知する。固定したcl-swankのsource/load pathnameに加えて、
実測したcondition型とmessage patternが一致するwarning、または確認済みの
condition型と完全一致summary messageの組だけを、cl-swank単体load中の
動的scopeでmuffleする。SBLint本体のloadと、直後に実行するBridgeの
ASD・全source・全testのfindingには適用しない。
保存実行形式には SBCL runtime が含まれるため、配布時は SBCL の著作権表示も
同梱する。

最終コンテナ image は本リポジトリの 0BSD ソースだけでなく、SBCL runtime、
glibc、zlib を含む複合配布物である。そのため OCI の
`org.opencontainers.image.licenses` は `NOASSERTION` とし、0BSD 単独の
image であるとは表示しない。個別のライセンス・著作権表示は
`/usr/share/doc/ts-codec-bridge/` に同梱する。

## Docker runtime image の provenance

配布用 image はリポジトリ直下で次の canonical wrapper から build する。

```bash
RUNTIME_IMAGE=konomitv-bs4k-tscodecbridge:runtime \
    scripts/build-runtime-image.sh
```

wrapper は tracked、staged、untracked の各差分がないことを検査し、
40文字の `SOURCE_COMMIT` を `HEAD` から取得する。さらに同じ commit の
`git archive` から source tree の決定的な `SOURCE_TREE_SHA256` を計算する。
Docker builder は `COPY` 直後に同じ digest を再計算し、宣言した commit と
実際の build context が一致しない場合は lint・compile より前に失敗する。
未 commit の作業ツリーを正規の配布物として扱う `working-tree` などの
代替値は受け付けない。

Dockerfile 単体では任意の commit と tree digest の組を受け取れてしまい、
両者の Git 上の対応関係を証明できない。したがって Docker の直接呼び出しは
正式な配布 build として非対応であり、`SOURCE_COMMIT` と
`SOURCE_TREE_SHA256` は canonical wrapper が管理する内部引数とする。
正式な配布 build では必ず上記 wrapper を使用すること。
wrapperへ追加できるDocker引数は、cache取得と表示方法だけを変える
`--no-cache`、`--pull`、`--progress`、および固定値
`--platform linux/amd64` に限定する。Dockerfile、target、build context、
build arg、labelなど、入力やprovenanceを変更できる引数は拒否する。
最終 stage は `scratch` とし、保存 ELF、`ldd` で解決して許可した共有
library、ライセンス表示、次の manifest だけを builder から複製する。
SBCL と glibc の Debian copyright が参照する Apache 2.0、GPL 2、
LGPL 2.1、GFDL 1.3 の共通ライセンス本文も固定 Jammy image から複製し、
各本文の SHA-256 を manifest の `COMMON_LICENSE` 行へ記録する。
source、SBCL compiler、lint tool、package manager、取得 cache は含めない。

この wrapper が作る runtime image は KonomiTV-BS4K のローカル統合検証用で、
本リポジトリの公開手順では OCI registry へ配布しない。glibc を含む image を
第三者へ配布する場合は、使用した `libc6` と同じ Ubuntu source package の
`.dsc`、upstream source、Debian patch source を image と同じ配布場所から
同等に取得できる状態にし、その直接 URL と SHA-256 を manifest へ追加する。
この対応を伴わない runtime image の外部配布はサポートしない。

`/usr/share/doc/ts-codec-bridge/Runtime-Manifest.tsv` には source commit と
tree digest、Ubuntu image、固定した package/tool、CLI・mapping version、
ELF・glibc・zlib の SHA-256 を記録する。また build 前後で追加・変更された
全 dpkg を `BUILDER_PACKAGE` 行として package・version・Ubuntu 公式 apt
origin と共に記録し、最終 stage に複製した全共有 library を
`RUNTIME_LIBRARY` 行として path・SHA-256・提供 package・version と共に
記録する。
表に記載した7個の直接 `.deb` は `apt-get download` 後、導入前に
各 SHA-256 を検証する。推移依存も導入後の全 package・version・公式 apt
origin を manifest へ記録し、同一の固定 Ubuntu image と公式 Jammy archive
以外からの混入を拒否する。
