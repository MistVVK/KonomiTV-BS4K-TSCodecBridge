<!-- SPDX-License-Identifier: 0BSD -->

# Opus MPEG-2 TS 検証契約

## 基本方針

FFmpeg 8 が生成する Opus private PES を正常系とし、Bridge は正しい
packet を byte-exact に通過させる。映像が AVC または HEVC でも、音声が
Opus なら descriptor 検証のため Bridge を通す。

## PMT

各 Opus ES entry について次を要求する。

- `stream_type` は `0x06`。
- `registration_descriptor` は tag `0x05`、length `0x04`、identifier
  `Opus`。
- DVB extension descriptor は tag `0x7F` で、extension tag `0x80` と
  有効な channel configuration を持つ。
- 同一 program 内の複数 Opus PID を独立 track として保持する。

descriptor が正しければ変更しない。欠落 descriptor の補完は、channel
configuration を入力から一意に検証できる場合だけ許可する。矛盾、
未知 extension、切断 descriptor は fail closed にする。

## PES payload

control header、lacing、trim、packet count を境界検査する。PES 内の
複数 Opus packet、PES を跨ぐ PTS の連続、mono、stereo、multichannel
を許容する。固定 20 ms と仮定しない。Bridge は Opus compressed packet
を無音補完のために複製しない。

## 非対象 PID

AAC、Timed ID3、字幕、データ PID、映像と、選択されていない program の
packet は byte-exact に維持する。
