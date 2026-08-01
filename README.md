# KonomiTV-BS4K-TSCodecBridge

MPEG-TS codec bridge for KonomiTV-BS4K, adding AV1, VP9 and Opus carriage without patching FFmpeg.

## ビルド

ビルドには Linux x86_64、GNU Make、SBCL が必要です。
生成される保存実行形式は、ビルドに使用した SBCL が依存する共有ライブラリを
実行時にも使用します。必要なライブラリは `ldd build/ts-codec-bridge.elf` で確認できます。

```bash
make build
```

生成物は `build/ts-codec-bridge.elf` です。

```bash
./build/ts-codec-bridge.elf --version
```

## 品質チェック

compile・単体テスト・fixture 検査は次のコマンドで実行します。

```bash
make compile test fixtures-check test-executable
```

lint には SBCL、SBLint、Mallet が必要です。事前に各 lint ツールを導入し、
コマンドを実行できるようにパスを通しておいてください。

```bash
make lint
```

FFmpeg 統合試験では、パス上の `ffmpeg` と `ffprobe` を使用します。

```bash
make test-ffmpeg-integration
```
