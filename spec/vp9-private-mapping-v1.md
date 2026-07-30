<!-- SPDX-License-Identifier: 0BSD -->

# KonomiTV-BS4K VP9 private mapping v1

## 適用範囲

この搬送は KonomiTV-BS4K の通常 API と、同じ版へ固定された
MistVVK/mpegts.js の間だけで用いる。`VP09` は登録済み MPEG
`format_identifier` ではなく、プロジェクト私有 marker である。
外部クライアントへ互換搬送として公開しない。

## PMT の ES entry

- `stream_type` は `0x06`。
- descriptor loop の先頭は
  `05 04 56 50 30 39` (`registration_descriptor("VP09")`)。
- 直後は次の 10 byte とする。

```text
80 08 4B 54 56 42 09 01 F0 00
```

各 field は次の意味を持つ。

| Offset | Byte | 意味 |
| ---: | ---: | --- |
| 0 | `0x80` | project-private descriptor tag |
| 1 | `0x08` | descriptor payload length |
| 2..5 | `KTVB` | KonomiTV-BS4K magic |
| 6 | `0x09` | codec id: VP9 |
| 7 | `0x01` | mapping version 1 |
| 8 | `0xF0` | v1 flags |
| 9 | `0x00` | reserved |

`v1 flags` は、bit 7 から順に 1 access unit / PES、raw VP9
superframe payload、`data_alignment_indicator=1`、key frame 先頭 packet
での random access indication を宣言する。下位 4 bit は 0 とする。

受信側は 2 個の descriptor、magic、codec id、version、flags、reserved
をすべて検証する。欠落、順序違い、未知 version、未知 flag、
非 0 reserved は VP9 と推測せず fail closed にする。

## PES と access unit

- `stream_id` は `0xE0`。
- `data_alignment_indicator` は 1。
- 1 PES は 1 個の FFmpeg VP9 packet に対応する。
- PES payload は VP9 frame または VP9 superframe の byte 列を変更せず
  収録する。独自 length prefix は付けない。
- superframe 内の各 frame は順番に全件検証する。空 frame、index の
  未被覆、後続 frame の破損を先頭 frame の成功だけで受理しない。
- spatial layer などにより各 subframe の coded size、render size、
  color metadata が異なることを許容する。
- PTS は必須とし、入力に DTS があれば保持する。
- key frame を収録する PES の先頭 TS packet は PUSI を 1 とし、
  adaptation field の random access indicator を 1 とする。
- continuity counter は payload を持つ packet だけで増分する。

## 構成情報

profile、bit depth、frame size、color configuration、key frame 判定は
VP9 uncompressed header から受信側が得る。superframe index は payload
の一部として保持し、各 subframe の境界検証に用いる。

同じ decoder state を継続する inter frame は profile を変更しない。
reference buffer の画像形式互換性は bit depth と chroma subsampling で
判定し、color space と range の metadata 差は reference 不互換としない。
frame size from refs は VP9 decoder の scaling 範囲を満たす reference が
少なくとも 1 個あることを要求する。

## 更新と失敗

同じ PID の mapping version または descriptor tuple が変化した場合、
受信側は既存 decoder state を継続利用しない。Bridge は破損を検出した
時点で非 0 終了し、検出前に stdout へ書いた byte 列の巻き戻しは保証
しない。

## 一次資料

- https://downloads.webmproject.org/docs/vp9/vp9-bitstream_superframe-and-uncompressed-header_v1.0.pdf
- https://www.webmproject.org/vp9/
- https://chromium.googlesource.com/webm/libvpx/
