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

`stream_id` は `0xBD` とし、音声 PES の `PES_packet_length` は必ず
非0とする。長さ0を許す映像 elementary stream の例外を Opus へ適用しない。
共通 optional header の flag、必要長、marker、reserved、stuffing も
厳密に検査する。`pack_header_field_flag` がある場合は、埋め込まれた
MPEG-1 / MPEG-2 pack header と任意の system header も構文検査する。
暗号解除機能を持たないため、非0の `PES_scrambling_control` は
fail closed にする。
`stream_id_extension` は外側の `stream_id` が `0xFD` の場合だけ許可し、
private data は隣接fieldとの境界を含めて `0x000001` を模倣しないことを
検査する。直前 PES の payload 状態が必要な `PES_CRC_flag` と、program
全体の順序状態が必要な `program_packet_sequence_counter_flag` は固定
FFmpeg 入力契約の対象外とし、flag が現れた時点で fail closed にする。

control header、lacing、trim、packet count を境界検査する。PES 内の
複数 Opus packet、PES を跨ぐ PTS の連続、mono、stereo、multichannel
を許容する。固定 20 ms と仮定しない。Bridge は Opus compressed packet
を無音補完のために複製しない。

## 非対象 PID

AAC、Timed ID3、字幕、データ PID、映像と、選択されていない program の
packet は byte-exact に維持する。
