<!-- SPDX-License-Identifier: 0BSD -->

# AV1 MPEG-2 TS 搬送契約

## 固定する上流文書

2026-03-25 版 AOM Working Group Draft
[Carriage of AV1 in MPEG-2 TS](https://aomediacodec.github.io/av1-mpeg2-ts/)
を実装対象とする。これは確定標準ではないため、Bridge の CLI version
とは別に搬送仕様版を管理する。上流 draft の変更を自動追従しない。
Buffer Pool と decoder timing は、この draft が規範参照する
[AV1 Bitstream & Decoding Process Specification Annex E](https://aomediacodec.github.io/av1-spec/av1-spec.pdf)
に固定する。

## 適合性の境界

この実装では「出力の MPEG-2 TS 搬送適合性」と「Bridge が検証して
受理する入力 AV1 subset」を区別する。

- 出力については、受理した各 OBU に start code と emulation prevention
  を施し、PMT、PES、timestamp、random access indication、T-STD をこの
  文書に定める 2026-03-25 draft の規則で検証する。これを
  **AOM draft に従う AV1 MPEG-2 TS 搬送**と呼ぶ。
- 入力については、Bridge は汎用 AV1 decoder ではない。
  統合試験に使用する FFmpeg と libaom-av1 の realtime 出力を
  対象に、entropy-coded tile payload の復号を行わず、OBU、frame header、
  tile group の構文、参照状態、timestamp に必要な意味だけを検証する。
  したがって入力 AV1 bitstream 全般への完全適合を表明しない。

固定 encoder 契約は、libaom-av1 の realtime usage、`row-mt=1`、
`lag-in-frames=0`、profile 0、8 bit または 10 bit の 4:2:0 である。
通常 mode と low-latency mode は独立した FFmpeg 統合試験を行う。
Bridge に入力する各映像 PES は、base layer の access unit を一つだけ
含まなければならない。

入力 subset で受理する AV1 構造は次のとおり。

- 一体型の Frame OBU（type 6）。
- Frame Header OBU（type 3）と、一つ以上の Tile Group OBU（type 4）
  からなる split frame。tile group は `TileNum` 順に連続し、全 tile を
  重複なく被覆しなければならない。
- Padding OBU（type 15）。搬送前の OBU 列での位置と payload を保つ。
- split frame の active frame header と extension header、payload が
  byte-exact に一致する Redundant Frame Header OBU（type 7）。
- 有効かつ showable な reference slot を指す
  `show_existing_frame`。`frame_id_numbers_present_flag` が有効な場合は
  `display_frame_id` も一致させ、delayed key frame は同一 decoded frame
  につき一度だけ表示できる。PTS は presentation、DTS は removal を
  表すものとして draft 3.5 の表を適用する。
- 一つの access unit 内の複数 sequence header、および access unit 間で
  再掲される sequence header。ただし payload はすべて byte-exact に
  一致するものに限る。

次の構造は、AV1 規格上の可否とは別に、固定 encoder が生成しないか、
完全 decoder 相当の状態を持たずに緩和すると誤受理になるため、入力
subset 外として識別可能なエラーで fail closed にする。

- nonzero `temporal_id` / `spatial_id`、および
  `operating_point_idc` が 0 または base-only `0x101` 以外の
  multilayer / multiple-operating-point 構成。
- 一つの coded video sequence 内で異なる sequence header。
  AV1 規格が `operating_parameters_info` に認める差分も、固定 encoder
  契約では生成されないため、Bridge は byte-exact 一致より広く緩和しない。
  sequence を変更する場合は、表示される新しい key frame で新しい coded
  video sequence を開始しなければならない。
- 一つの映像 PES に複数の coded frame / access unit を含む構成。
- Tile List OBU、Frame OBU に外付けした tile group / redundant header、
  不完全または順序違反の tile coverage、未知または不正な構文。

tile payload は、宣言された size、OBU 終端、tile coverage までを検査する。
算術復号後の AV1 画素内容までは検証しない。この制限は出力 tsOBU の
start code と emulation prevention の搬送適合性を弱めるものではないが、
入力 bitstream の decoder 適合性証明とは区別する。

## PMT

- `stream_type` は `0x06`。
- ES descriptor loop の先頭は
  `05 04 41 56 30 31` (`registration_descriptor("AV01")`)。
- 直後に tag `0x80`、length `0x04` の AV1 video descriptor を置く。
- descriptor の marker は 1、version は 1 とする。
- profile、level、tier、bit depth、monochrome、chroma sampling、
  sample position、HDR/WCG は sequence header OBU から得た値を使う。
- descriptor が変化したときは PMT version を増分し、新構成の access
  unit より前に新 PMT を出力する。

初回の sequence header を得るまでは、該当 program の初期 PAT、PMT、
映像 access unit を上限付きで保留する。構成を得られないまま上限または
EOF に達した場合は fail closed にする。

## Elementary stream と PES

- 入力の low-overhead OBU を OBU 境界で解析する。
- 各 OBU の前に `00 00 01` を置く。
- start-code emulation を生じる payload byte 列へ `0x03` を挿入する。
- 1 PES は 1 AV1 access unit のみを収録する。
- PES `stream_id` は `0xBD`、`data_alignment_indicator` は 1。
- PTS は必須とし、DTS が存在する場合は保持する。
- key frame または delayed key frame の先頭 packet では PUSI、
  random access indicator、elementary stream priority indicator を
  設定する。

OBU の不正 size、切断、禁止列、sequence header と descriptor の矛盾は
推測で補わず非 0 終了する。

## CBR と T-STD

AV1 変換では入力 transport stream の正の整数 CBR を
`--transport-rate-kbps` で必須指定する。単位は 1,000 bit/s とし、
指定値を丸めず arrival clock に使う。VP9 と passthrough でこの option
が指定された場合は拒否する。

なお、固定した draft §3.6.2.1 の記号表には `EBS_n` を
「multiplexing buffer `MB_n` の size」とする記載があるが、直後の定義
`EBS_n = BufferSize`、図、§3.6.2.3 の用法から
elementary stream buffer `EB_n` の size を指す編集上の誤記として扱う。

- 全計算を有理数で行い、映像 PID の TS packet は 188 byte 全体を
  transport buffer に入れる。transport buffer size は 512 byteとする。
  単一 packet の遅延ではなく、TB が空から非空になった時刻を busy
  period の起点として追跡し、連続非空期間を 1 秒以下に制限する。
  各 arrival、discontinuity、EOF で直前 busy period の最終空化時刻を
  検査し、TB が少なくとも毎秒 1 回空にならない場合は fail closed にする。
- PCR の基準 byte index は `packet_index * 188 + 10` とする。CBR から
  算出した PCR との差は 27 MHz clock で 13.5 tick 以下でなければ
  fail closed にする。
- 最初の PCR を基準とする DTS 時刻は
  `arrival(first_pcr_byte) + (DTS * 300 - first_PCR) / 27,000,000`
  とする。
- multiplex buffer には PES header byte と PES payload byte の双方を
  到着させる。PES payload の最初の byte を MB から EB へ転送するとき、
  その byte より前に MB へ到着済みの PES header byte を瞬時に除去する。
  decoder buffer に数えるのは `00 00 01` start code と
  `00 00 03 xx` の `03` を除去した元の AV1 byte とする。
- 2026-03-25 版 draft の level、tier、profile 表から `BitRate` を得る。
  level 31 または表にない level/tier は拒否する。`BufferSize` は
  1 秒分の `BitRate` とし、`Rx = Rbx = 1.1 * BitRate`、
  `EBS = BufferSize` とする。byte 単位への変換は明示的に 8 で割る。
- multiplex buffer size は bit 単位で
  `(1/750 + 0.004) * max(1100 * BitRate, 2,000,000)
  + 0.1 * BufferSize` とし、最後に 8 で割る。
- FFmpeg/libaom の realtime 出力では、実際には low-delay 動作でも
  `low_delay_mode_flag` が 0 のことがある。この入力を誤拒否しないため、
  Bridge の受理判定では flag の値にかかわらず decoder removal を
  `max(DTS, MB 最終 departure + 1 / 27,000,000 秒)` とする。
  bitstream の flag 自体は書き換えない。この挙動は draft の strict mode
  における EB underflow 禁止を意図的に緩和するものであり、厳格な T-STD
  適合性の判定ではなく、本プロジェクトの realtime 入力 subset の契約とする。
  access unit の最初の arrival から実効 removal までの上限は 10 秒とする。
  DTS または PTS が live mux の時刻ゆらぎにより実効 removal より前になる
  場合は 3 秒以内だけ arrival または removal へ clamp し、それを超えれば
  fail closed にする。同時刻では removal を arrival より先に適用する。
- Buffer Pool は AV1 Annex E の `BUFFER_POOL_MAX_SIZE = 10` とし、
  decoded access unit を Scheduled Removal Timing で瞬時に投入する。
  8 個の VBI、decoder reference count、player reference count、
  各 frame の最終 Presentation Time を管理する。presentation 済みの
  player reference は次の removal より前の Presentation Time で解放し、
  VBI 更新時は置換前後の decoder reference count を更新する。空き frame
  buffer がない場合、存在しない VBI を表示する場合、decode より前に
  presentation する場合は fail closed にする。delayed key frame を
  `show_existing_frame` で表示したときは、Annex E の
  `update_ref_buffers(displayIdx, 0xFF)` と同じく既存 buffer を全 VBI
  へ再登録する。

映像 PID または PCR PID の discontinuity は新しい epoch の境界とする。
旧 epoch の未完 access unit、分類状態、TB、MB、EB、予約済み removal は
完了を強制せず破棄する。将来 access unit の登録情報は保持する。
専用 PCR PID が access unit 途中で discontinuity になった場合、旧 PES
の continuation は次の映像 PUSI まで T-STD 対象外とし、新 PUSI は新しい
PCR epoch だけで検証する。

## 固定 packet 数での再配置

出力 TS packet 数は入力と完全一致させる。PMT の再生成など packet 単位の
追加分は、後続の既存 null packet slot または同じ変換で空いた対象 PID
slot だけへ FIFO で割り当てる。

AV1 の streaming 変換結果は packet 単位の追加分にせず、変換元 packet
ごとの origin slot と deadline slot を持つ byte FIFO として保持する。
映像 PID の各 target template では、FIFO 先頭からその template の payload
容量までを取り出し、最大 1 packet の replacement を元の slot に置く。
残った byte は次の target template へ送る。途中の null packet slot では
FIFO 先頭から最大 184 byte を映像 continuation として取り出せる。通常
packet slot では、FIFO 先頭の deadline を超えていないことを検証する。

- 変換対象外 packet は元の slot で byte-exact に保持する。
- 選択 PCR は同じ packet index と byte 位置に保持し、PCR 値を変更しない。
  payload を持つ PCR target template には PCR を保持した残り容量まで
  AV1 byte を置く。adaptation-only PCR packet の payload 容量は 0 とし、
  adaptation-only のまま保持する。
- AV1 byte segment の deadline は、その origin slot と明示 CBR から
  2 ms で求め、origin slot 末尾から消費 slot 先頭までを 2 ms 以内にする。
  byte を消費しない通常 packet は deadline slot まで、byte を消費する
  packet は deadline slot の次の slot まで許容し、それを超えた場合は
  拒否する。
- AV1 byte を target replacement packet へ具体化した後、その packet が
  PMT 前置などにより packet FIFO へ入る場合も、収録した最古 byte segment
  の origin slot と deadline slot を保持する。FIFO へ入れ直した slot を
  新しい origin として deadline を更新してはならない。
- 次の映像 PUSI に AV1 byte が残る場合は、その PUSI packet の slot と
  選択 PCR を維持したまま旧 PES の continuation target として使える。
  source payload は次 PES として通常どおり解析し、実際の PUSI と random
  access indication は同じ PES の後続 video/null target へ移す。旧 PES と
  次 PES の byte を同一 TS payload に混在させない。次 PUSI target を使っても
  旧 PES の残留を収容できない場合、discontinuity 境界の場合、または EOF に
  AV1 byte が残る場合は `REPACKETIZE_CAPACITY_EXHAUSTED` で fail closed にする。
- 縮小で空いた対象 PID slot は null packet で埋める。
- continuity counter は、元 template、null continuation の別によらず、
  実際に映像 payload を出力した packet ごとにだけ増分する。

## TS の維持条件

変換対象外 PID の packet は byte-exact に維持する。変換対象 PID は
PCR、discontinuity、adaptation-only packet を保持しつつ再 packetize
し、payload のある packetだけ continuity counter を増分する。
PMT section の CRC32/MPEG-2、section length、descriptor loop length を
再計算する。

## encoder 統合証明

`scripts/test-ffmpeg-integration.sh` は、パス上の FFmpeg / ffprobe を使い、
libaom-av1 の 8 bit / 10 bit と
normal / low-latency を別ケースで生成する。各 Bridge 出力から AV1
elementary stream を抽出し、
`scripts/verify-av1-ts-elementary-stream.sh` で次を byte 単位に検証する。

- 先頭および各 OBU 境界に `00 00 01` がある。
- 搬送 OBU の `obu_has_size_field` が 0 に正規化されている。
- Sequence Header OBU と Frame / Frame Header OBU が存在する。
- Tile List、reserved type、未削除の Temporal Delimiter がない。
- tsOBU payload に `00 00 00`、`00 00 01`、`00 00 02` がなく、
  `00 00 03` は後続 byte が 0 から 3 の emulation prevention 列に限る。

これとは独立に Lisp 単体試験で、Padding / Redundant Frame Header の
payload 保持、split tile group の完全被覆、`show_existing_frame` の
参照状態と PTS / DTS、同一および異なる sequence header を正負双方で
検証する。
