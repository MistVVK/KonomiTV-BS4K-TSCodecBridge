<!-- SPDX-License-Identifier: 0BSD -->

# Fixture 収録規則

追跡対象の fixture は、このリポジトリの 0BSD generator だけから決定的に
再生成できる byte 列に限定する。

収録してよいもの:

- 自作 generator が生成する PAT、PMT、PES、PCR、descriptor。
- codec payload として意味を持たない最小の境界検査用 byte 列。
- CRC、length、continuity counter、descriptor version、PSI の予約 PID
  範囲、program / elementary PID の重複を意図的に壊した corruption
  corpus。
- generator の version と出力 SHA-256。

収録しないもの:

- 実放送 TS、録画 TS、局名や実在番組由来の metadata。
- FFmpeg、AOM、mpegts.js、KonomiTV その他第三者が生成・配布する
  sample や source のコピー。
- SBCL、sblint、Mallet、FFmpeg の binary。
- ローカル絶対 path、host 名、IP address、token、秘密鍵。

FFmpeg との統合試験 sample は試験ごとに一時 directoryへ生成し、
Git worktree と release artifact へ入れない。試験終了後は SHA-256 と
検査結果だけを記録する。
