;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defconstant +maximum-pending-packet-count+ 131072)
(defconstant +maximum-pes-byte-count+ (* 64 1024 1024))
(defconstant +pts-modulus+ (ash 1 33))
(defconstant +pts-half-modulus+ (ash 1 32))
(defconstant +av1-signaling-change-minimum-ticks+ 90000)
(defconstant +pcr-modulus+ (* (ash 1 33) 300))
(defconstant +maximum-pcr-interval-ticks+ 2700000)
;; ES の DTS/PTS が PCR なしで進んでよい上限。
;; 規格上 PCR は 100ms 以内だが、FFmpeg live + 固定 muxrate では 29.97fps で
;; 数 frame 分 PCR 無しが連続することがあるため 500ms を許す。
(defconstant +maximum-pcr-gap-pts-ticks+ 45000)
;; 30000/1001 fps の 1 frame は 3003 ticks で、15 frame 端は
;; 45045 ticks に量子化される。500ms の意味を広げず、この端数差
;; 45 ticks だけを明示的に許容する。
(defconstant +pcr-gap-ntsc-frame-quantization-tolerance-ticks+ 45)
(defconstant +maximum-quantized-pcr-gap-pts-ticks+
  (+ +maximum-pcr-gap-pts-ticks+
     +pcr-gap-ntsc-frame-quantization-tolerance-ticks+))
(defconstant +maximum-dts-pcr-delay-ticks+ 900000)
;; FFmpeg の低遅延 mux では起動直後に DTS が PCR より数百 ms 先行することがある。
;; 2 秒以内の先行は許容し、それ以上だけを致命とする。
(defconstant +maximum-dts-before-pcr-ticks+ 180000)
(defconstant +maximum-av1-rap-gap-ticks+ 180000)
;; FFmpeg の live MPEG-TS mux は Opus frame 境界で 90kHz PTS が ±数 tick ずれることがある。
;; 1ms (90 ticks) 以内は実害のない丸めジッターとして rebase し、大きな飛びだけを致命とする。
(defconstant +opus-pts-jitter-tolerance-ticks+ 90)
(defconstant +repacketize-deadline-milliseconds+ 2)
;; FFmpeg の MPEG-TS muxer は 1 AU の映像 PES を連続 packet として出し、
;; CBR の null packet を AU 間へまとめる。low-overhead OBU から start-code
;; 形式への変換で増えた数 byte を次の null へ載せるには、2 ms では映像 PES
;; burst の途中で期限切れになる。KonomiTV が扱う最小 24 fps の 1 frame
;; (約 41.7 ms) を覆う 50 ms まで保持し、最終的な適合性は T-STD で検証する。
(defconstant +av1-stream-repacketize-deadline-milliseconds+ 50)

(defstruct pending-entry
  (packet (make-array +ts-packet-size+
                      :element-type 'octet)
          :type octet-vector)
  (slot-index 0 :type (integer 0 *))
  (resolved-p t :type boolean)
  (use-original-p t :type boolean)
  (replacements '() :type list)
  (replacement-provenances '() :type list)
  ;; 直前AV1 PESの末尾を収容するため、このsource packetの出力slotを
  ;; 使用済みなら真。source payload自体は次PESとして通常どおり解析する。
  (av1-output-target-consumed-p nil :type boolean)
  (event nil)
  (next nil))

(defstruct replacement-provenance
  (origin-slot 0 :type (integer 0 *))
  (deadline-slot 0 :type (integer 0 *)))

(defstruct output-packet-entry
  (packet (make-array +ts-packet-size+
                      :element-type 'octet)
          :type octet-vector)
  (origin-slot nil :type (or null (integer 0 *)))
  (deadline-slot 0 :type (integer 0 *))
  (next nil))

(defstruct pmt-event
  (entries '() :type list)
  (table (make-program-map-table)
         :type program-map-table)
  (source-section #() :type octet-vector)
  (pmt-pid 0 :type (unsigned-byte 13))
  (video-pid nil :type (or null (unsigned-byte 13)))
  (opus-pids '() :type list))

(defstruct video-event
  (entries '() :type list)
  (pes #() :type octet-vector)
  (codec :vp9 :type (member :vp9 :av1))
  (configuration nil)
  (random-access-p nil :type boolean)
  (frame-semantics nil :type (or null av1-frame-semantics))
  (refresh-frame-flags nil :type (or null octet))
  (source-pmt-table nil :type (or null program-map-table))
  (pmt-pid nil :type (or null (unsigned-byte 13)))
  (pmt-template nil :type (or null octet-vector))
  (video-pid 0 :type (unsigned-byte 13))
  (opus-pids '() :type list))

(defstruct av1-stream-byte-segment
  (octets #() :type octet-vector)
  (offset 0 :type fixnum)
  (origin-slot 0 :type (integer 0 *))
  (deadline-slot 0 :type (integer 0 *))
  (payload-unit-start-p nil :type boolean)
  (adaptation-flags 0 :type octet)
  (next nil))

(defstruct av1-stream-target-entry
  (entry (make-pending-entry) :type pending-entry)
  (kind :video :type (member :video :null))
  (next nil))

(defstruct (pes-assembler
            (:constructor %make-pes-assembler (pid kind)))
  (pid 0 :type (unsigned-byte 13) :read-only t)
  (kind :video :type (member :video :opus) :read-only t)
  (active-p nil :type boolean)
  (entries '() :type list)
  (first-entry-slot-index nil :type (or null (integer 0 *)))
  (buffer
    (make-array 0
                :element-type 'octet
                :adjustable t
                :fill-pointer 0)
    :type (vector octet))
  (expected-length nil :type (or null fixnum))
  (streaming-p nil :type boolean)
  (stream-configuration nil)
  (stream-random-access-kind nil)
  (stream-frame-semantics nil :type (or null av1-frame-semantics))
  (stream-ordering-time nil :type (or null integer))
  (stream-temporal-delimiter-seen-p nil :type boolean)
  (stream-sequence-header-in-access-unit-p nil :type boolean)
  (av1-stream-transformer nil
                          :type (or null av1-stream-transformer))
  (av1-stream-byte-head nil
                        :type (or null av1-stream-byte-segment))
  (av1-stream-byte-tail nil
                        :type (or null av1-stream-byte-segment))
  (av1-stream-byte-count 0 :type (integer 0 *))
  (av1-stream-target-head nil
                          :type (or null av1-stream-target-entry))
  (av1-stream-target-tail nil
                          :type (or null av1-stream-target-entry))
  (av1-stream-target-count 0 :type (integer 0 *))
  (av1-stream-output-start-p nil :type boolean)
  (av1-stream-last-template nil
                            :type (or null octet-vector))
  (last-continuity-counter nil
                           :type (or null (unsigned-byte 4)))
  (last-payload-packet nil :type (or null octet-vector))
  (skip-next-continuity-check-p nil :type boolean)
  (expected-pts nil :type (or null (unsigned-byte 33)))
  (skip-next-pts-check-p nil :type boolean)
  (skip-next-program-timestamp-check-p nil :type boolean)
  (pending-pts-discontinuity-p nil :type boolean)
  (program-timestamp-recorded-p nil :type boolean)
  (last-program-timestamp nil
                          :type (or null (unsigned-byte 33)))
  (last-program-unwrapped-timestamp nil :type (or null integer))
  (pcr-window-start-timestamp nil :type (or null integer))
  (source-pmt-table nil :type (or null program-map-table))
  (source-pmt-pid nil :type (or null (unsigned-byte 13)))
  (source-pmt-template nil :type (or null octet-vector))
  (source-opus-pids '() :type list)
  (source-av1-configuration nil)
  (source-av1-reduced-header-p nil :type boolean)
  (source-av1-tu-sequence-header-seen-p nil :type boolean))

(defstruct (bridge-processor
            (:constructor %make-bridge-processor
                (output video-codec audio-codec
                 requested-program-number tstd-model)))
  (output *standard-output* :type stream :read-only t)
  (video-codec :passthrough
               :type (member :passthrough :vp9 :av1)
               :read-only t)
  (audio-codec :aac
               :type (member :aac :opus)
               :read-only t)
  (transport-integrity-validator
    (make-payload-continuity-validator)
    :type payload-continuity-validator
    :read-only t)
  (requested-program-number nil
                            :type (or null (integer 1 65535))
                            :read-only t)
  (tstd-model nil :type (or null tstd-model) :read-only t)
  (pending-head nil)
  (pending-tail nil)
  (pending-count 0 :type fixnum)
  (output-packet-head nil)
  (output-packet-tail nil)
  (output-packet-count 0 :type fixnum)
  (output-slot-index 0 :type (integer 0 *))
  (pat-assembler (make-section-assembler 0)
                 :type section-assembler)
  (last-input-pat-signature nil :type (or null octet-vector))
  (last-input-pat-version nil :type (or null (unsigned-byte 5)))
  (pmt-assembler nil :type (or null section-assembler))
  (pmt-pid nil :type (or null (unsigned-byte 13)))
  (pat-program-number nil :type (or null (unsigned-byte 16)))
  (current-pmt-entries '() :type list)
  (latest-pmt-table nil :type (or null program-map-table))
  (latest-pmt-template nil :type (or null octet-vector))
  (current-video-pid nil :type (or null (unsigned-byte 13)))
  (current-opus-pids '() :type list)
  (pes-assemblers (make-hash-table :test #'eql)
                  :type hash-table)
  (output-continuity-counters (make-hash-table :test #'eql)
                              :type hash-table)
  (current-pcr-pid nil :type (or null (unsigned-byte 13)))
  (selected-pcr-ever-seen-p nil :type boolean)
  (selected-pcr-baselined-p nil :type boolean)
  (last-selected-pcr nil :type (or null integer))
  (last-selected-pcr-input-packet
    (make-array +ts-packet-size+
                :element-type 'octet)
    :type octet-vector)
  (last-selected-pcr-input-packet-valid-p nil :type boolean)
  (last-input-pmt-signature nil :type (or null octet-vector))
  (last-input-pmt-version nil :type (or null (unsigned-byte 5)))
  (last-source-pmt-signature nil :type (or null octet-vector))
  (last-source-pmt-version nil
                           :type (or null (unsigned-byte 5)))
  (last-output-pmt-signature nil :type (or null octet-vector))
  (last-output-pmt-version nil
                           :type (or null (unsigned-byte 5)))
  (planned-pmt-pid nil :type (or null (unsigned-byte 13)))
  (advertised-av1-configuration nil)
  (vp9-validation-state
    (make-vp9-validation-state)
    :type vp9-validation-state)
  (av1-sequence-validation-state
    nil
    :type (or null av1-sequence-validation-state))
  (av1-frame-validation-state
    (make-av1-frame-validation-state)
    :type av1-frame-validation-state)
  (last-av1-configuration nil)
  (last-av1-reduced-header-p nil :type boolean)
  (last-av1-timestamp nil :type (or null (unsigned-byte 33)))
  (last-av1-unwrapped-timestamp nil :type (or null integer))
  (last-av1-signaling-tuple nil :type (or null list))
  (last-av1-signaling-change-pts nil :type (or null integer))
  (av1-cll-present-p nil :type boolean)
  (av1-rap-without-cll-seen-p nil :type boolean)
  (av1-tu-sequence-header-seen-p nil :type boolean)
  (first-av1-ordering-time nil :type (or null integer))
  (last-av1-rap-ordering-time nil :type (or null integer))
  (seen-pmt-p nil :type boolean)
  (seen-video-pes-p nil :type boolean)
  (eof-p nil :type boolean))

(defun make-bridge-processor
    (output video-codec audio-codec
     &key program-number transport-rate-kbps)
  "OUTPUTとcodec選択からstream processorを作る。"
  (cond
    ((eq video-codec :av1)
     (unless transport-rate-kbps
       (bridge-error
        "TSTD_TRANSPORT_RATE_REQUIRED_FOR_AV1")))
    (transport-rate-kbps
     (bridge-error
      "TSTD_TRANSPORT_RATE_ONLY_VALID_FOR_AV1")))
  (%make-bridge-processor
   output video-codec audio-codec program-number
   (when transport-rate-kbps
     (make-tstd-model transport-rate-kbps))))

(defun reset-pes-program-timestamp-state (assembler)
  "単一ESのPTS/DTS展開基準を破棄する。"
  (setf
   (pes-assembler-last-program-timestamp assembler) nil
   (pes-assembler-last-program-unwrapped-timestamp assembler) nil
   (pes-assembler-pcr-window-start-timestamp assembler) nil)
  assembler)

(defun reset-program-timestamp-state (processor)
  "全対象ESのPTS/DTS展開基準を破棄する。"
  (maphash
   (lambda (pid assembler)
     (declare (ignore pid))
     (reset-pes-program-timestamp-state assembler))
   (bridge-processor-pes-assemblers processor))
  processor)

(defun reset-selected-pcr-state (processor)
  "選択programのPCR監視状態を初期化する。"
  (setf
   (bridge-processor-selected-pcr-ever-seen-p processor) nil
   (bridge-processor-selected-pcr-baselined-p processor) nil
   (bridge-processor-last-selected-pcr processor) nil
   (bridge-processor-last-selected-pcr-input-packet-valid-p
    processor)
   nil)
  (reset-program-timestamp-state processor)
  processor)

(defun reset-video-validation-state (processor)
  "Video PIDの参照状態とsequence検証epochを初期化する。"
  (setf
   (bridge-processor-vp9-validation-state processor)
   (make-vp9-validation-state)
   (bridge-processor-av1-sequence-validation-state processor)
   nil
   (bridge-processor-av1-frame-validation-state processor)
   (make-av1-frame-validation-state))
  processor)

(defun install-selected-pcr-pid (processor pcr-pid)
  "PMTが宣言するPCR PIDを監視対象としてinstallする。"
  (unless
      (eql pcr-pid
           (bridge-processor-current-pcr-pid processor))
    (setf (bridge-processor-current-pcr-pid processor) pcr-pid)
    (reset-selected-pcr-state processor))
  pcr-pid)

(defun unwrap-program-timestamp
    (assembler timestamp ordered-source-p)
  "単一ESの90kHz timestampを33-bit wrap込みで展開する。"
  (let ((last-timestamp
          (pes-assembler-last-program-timestamp assembler))
        (last-unwrapped
          (pes-assembler-last-program-unwrapped-timestamp
           assembler)))
    (let ((unwrapped
            (if last-timestamp
                (let* ((forward
                         (mod (- timestamp last-timestamp)
                              +pts-modulus+))
                       (delta
                         (if (>= forward +pts-half-modulus+)
                             (- forward +pts-modulus+)
                             forward)))
                  (when (and ordered-source-p (minusp delta))
                    (bridge-error
                     "PROGRAM_DTS_BACKWARD pid=0x~4,'0X previous=~D actual=~D"
                     (pes-assembler-pid assembler)
                     last-timestamp
                     timestamp))
                  (+ last-unwrapped delta))
                0)))
      (when (or (null last-unwrapped)
                (> unwrapped last-unwrapped))
        (setf
         (pes-assembler-last-program-timestamp assembler)
         timestamp
         (pes-assembler-last-program-unwrapped-timestamp
          assembler)
         unwrapped))
      unwrapped)))

(defun validate-program-timestamp-against-pcr
    (processor assembler timestamp ordered-source-p)
  "単一ESのtimestampからPCR gapとDTS/PCR上限を検証する。"
  (let ((unwrapped
          (unwrap-program-timestamp
           assembler timestamp ordered-source-p)))
    (when (and
           (bridge-processor-selected-pcr-baselined-p processor)
           (bridge-processor-last-selected-pcr processor))
      (let* ((pcr-base
               (floor
                (bridge-processor-last-selected-pcr processor)
                300))
             (delay
               (mod (- timestamp pcr-base) +pts-modulus+)))
        (if (>= delay +pts-half-modulus+)
            ;; timestamp が PCR より前。signed 最短距離で許容幅を超えたときだけ落とす。
            (let ((behind
                    (mod (- pcr-base timestamp) +pts-modulus+)))
              (when (> behind +maximum-dts-before-pcr-ticks+)
                (bridge-error
                 "DTS_BEFORE_PCR pcr_base=~D timestamp=~D"
                 pcr-base timestamp)))
            (when (> delay +maximum-dts-pcr-delay-ticks+)
              (bridge-error
               "DTS_PCR_DELAY_EXCEEDED delay_ticks=~D limit_ticks=~D"
               delay +maximum-dts-pcr-delay-ticks+)))))
    (let ((window-start
            (pes-assembler-pcr-window-start-timestamp
             assembler)))
      (cond
        ((null window-start)
         (setf
          (pes-assembler-pcr-window-start-timestamp assembler)
          unwrapped))
        (t
         (let ((elapsed (- unwrapped window-start)))
           ;; 500ms超を連続範囲として緩和しない。30000/1001fps の
           ;; 15 frame 端で実際に生じる 45045 ticks だけを追加で受理する。
           (when (and (> elapsed +maximum-pcr-gap-pts-ticks+)
                      (/= elapsed +maximum-quantized-pcr-gap-pts-ticks+))
             (bridge-error
              "SELECTED_PCR_GAP elapsed_90khz_ticks=~D limit_ticks=~D accepted_ntsc_quantized_boundary_ticks=~D"
              elapsed
              +maximum-pcr-gap-pts-ticks+
              +maximum-quantized-pcr-gap-pts-ticks+))))))
    unwrapped))

(defun restart-pcr-gap-windows (processor)
  "新しいPCRを基準に全ESのgap windowを再開する。"
  (maphash
   (lambda (pid assembler)
     (declare (ignore pid))
     (setf
      (pes-assembler-pcr-window-start-timestamp assembler)
      nil))
   (bridge-processor-pes-assemblers processor))
  processor)

(defun record-selected-pcr-packet (processor packet pid pcr)
  "選択PCR PIDのPACKETでPCR区間を更新する。"
  (when (eql pid
             (bridge-processor-current-pcr-pid processor))
    ;; 別PIDのpacketを挟んでも、選択PID上で直前とbyte-identicalなら
    ;; semantic stateへ同じdiscontinuity/PCRを二度適用しない。
    (when
        (and
         (bridge-processor-last-selected-pcr-input-packet-valid-p
          processor)
         (equalp
          packet
          (bridge-processor-last-selected-pcr-input-packet
           processor)))
      (return-from record-selected-pcr-packet :duplicate))
    (replace
     (bridge-processor-last-selected-pcr-input-packet processor)
     packet)
    (setf
     (bridge-processor-last-selected-pcr-input-packet-valid-p
      processor)
     t)
    (when (ts-discontinuity-indicator-p packet)
      (setf
       (bridge-processor-selected-pcr-baselined-p processor) nil
       (bridge-processor-last-selected-pcr processor) nil)
      (reset-program-timestamp-state processor)
      (reset-av1-ordering-epoch processor))
    (when pcr
      (let ((last
              (bridge-processor-last-selected-pcr processor)))
        (when last
          (let ((delta
                  (mod (- pcr last) +pcr-modulus+)))
            (when (or
                   (zerop delta)
                   (>= delta (ash +pcr-modulus+ -1))
                   (> delta +maximum-pcr-interval-ticks+))
              (bridge-error
               "SELECTED_PCR_INTERVAL_INVALID pid=0x~4,'0X ticks=~D"
               pid delta)))))
      (setf
       (bridge-processor-selected-pcr-ever-seen-p processor) t
       (bridge-processor-selected-pcr-baselined-p processor) t
       (bridge-processor-last-selected-pcr processor) pcr)
      (restart-pcr-gap-windows processor)))
  pcr)

(defun av1-signaling-tuple (configuration)
  "CONFIGURATIONのprofile/tier/level tupleを返す。"
  (list
   (av1-codec-configuration-profile configuration)
   (av1-codec-configuration-tier configuration)
   (av1-codec-configuration-level configuration)))

(defun reset-av1-ordering-epoch (processor)
  "discontinuity後のAV1時刻、RAP窓、TU状態を初期化する。"
  (setf
   (bridge-processor-last-av1-timestamp processor) nil
   (bridge-processor-last-av1-unwrapped-timestamp processor) nil
   (bridge-processor-last-av1-signaling-change-pts processor) nil
   (bridge-processor-av1-tu-sequence-header-seen-p processor) nil
   (bridge-processor-first-av1-ordering-time processor) nil
   (bridge-processor-last-av1-rap-ordering-time processor) nil)
  processor)

(defun commit-av1-temporal-unit-state
    (processor temporal-delimiter-seen-p
     sequence-header-in-access-unit-p)
  "検証済みAUのsequence header状態を現在のTUへ反映する。"
  (setf
   (bridge-processor-av1-tu-sequence-header-seen-p processor)
   (or
    sequence-header-in-access-unit-p
    (and
     (not temporal-delimiter-seen-p)
     (bridge-processor-av1-tu-sequence-header-seen-p
      processor))))
  processor)

(defun record-av1-ordering-time
    (processor assembler timestamp ordered-source-p)
  "AV1 DTS優先時刻を33-bit wrapとPTS reorderを考慮して展開する。"
  (when (pes-assembler-skip-next-pts-check-p assembler)
    (reset-av1-ordering-epoch processor)
    (setf (pes-assembler-skip-next-pts-check-p assembler) nil))
  (let ((last-timestamp
          (bridge-processor-last-av1-timestamp processor))
        (last-unwrapped
          (bridge-processor-last-av1-unwrapped-timestamp processor)))
    (let ((unwrapped
            (if last-timestamp
                (let* ((forward
                         (mod (- timestamp last-timestamp)
                              +pts-modulus+))
                       (delta
                         (if (>= forward +pts-half-modulus+)
                             (- forward +pts-modulus+)
                             forward)))
                  (when (and ordered-source-p (minusp delta))
                    (bridge-error
                     "AV1 DTS moves backward: previous=~D actual=~D"
                     last-timestamp timestamp))
                  (+ last-unwrapped delta))
                0)))
      (when (or (null last-unwrapped)
                (> unwrapped last-unwrapped))
        (setf (bridge-processor-last-av1-timestamp processor)
              timestamp
              (bridge-processor-last-av1-unwrapped-timestamp processor)
              unwrapped))
      (values unwrapped ordered-source-p))))

(defun record-av1-signaling-configuration
    (processor configuration ordering-time ordered-source-p)
  "profile/tier/level変更間隔を1秒以上に制限する。"
  (let ((tuple (av1-signaling-tuple configuration))
        (previous
          (bridge-processor-last-av1-signaling-tuple processor)))
    (unless (equal tuple previous)
      (when previous
        (let ((last-change
                (bridge-processor-last-av1-signaling-change-pts
                 processor)))
          (when last-change
            (let ((elapsed (- ordering-time last-change)))
              (cond
                ((minusp elapsed)
                 (when ordered-source-p
                   (bridge-error
                    "Successive AV1 signaling changes move backward in DTS")))
                ((< elapsed
                    +av1-signaling-change-minimum-ticks+)
                  (bridge-error
                   "Successive AV1 profile/tier/level changes are less than one second apart")))))))
      (setf
       (bridge-processor-last-av1-signaling-tuple processor)
       tuple
       (bridge-processor-last-av1-signaling-change-pts processor)
       ordering-time)))
  configuration)

(defun validate-av1-pes-timestamps (header semantics)
  "AOM §3.5のframe表示意味とPES PTS/DTS割当を検証する。"
  (let* ((pts (pes-header-pts header))
         (dts (or (pes-header-dts header) pts)))
    (unless pts
      (bridge-error "AV1_PTS_MISSING"))
    (when (and
           (not
            (av1-frame-semantics-show-existing-frame-p semantics))
           (not
            (av1-frame-semantics-show-frame-p semantics))
           (/= pts dts))
      (bridge-error
       "AV1_HIDDEN_FRAME_PTS_DTS_MISMATCH pts=~D dts=~D showable=~A"
       pts
       dts
       (av1-frame-semantics-showable-frame-p semantics)))
    (values pts dts)))

(defun record-av1-access-unit-conformance
    (processor random-access-kind hdr-cll-p ordering-time)
  "HDR_CLL反復と全AU上のRAP間隔をAOM §3.1に従い検証する。"
  (unless (bridge-processor-first-av1-ordering-time processor)
    (setf
     (bridge-processor-first-av1-ordering-time processor)
     ordering-time))
  (let* ((window-start
           (or
            (bridge-processor-last-av1-rap-ordering-time
             processor)
            (bridge-processor-first-av1-ordering-time processor)))
         (elapsed (- ordering-time window-start)))
    (when (> elapsed +maximum-av1-rap-gap-ticks+)
      (bridge-error
       "AV1_RAP_INTERVAL_EXCEEDED elapsed_ticks=~D limit_ticks=~D"
       elapsed
       +maximum-av1-rap-gap-ticks+)))
  (when (and
         hdr-cll-p
         (bridge-processor-av1-rap-without-cll-seen-p processor))
    (bridge-error
     "AV1_CLL_MISSING_AT_PRIOR_RAP"))
  (when hdr-cll-p
    (setf (bridge-processor-av1-cll-present-p processor) t))
  (when random-access-kind
    (when (and
           (bridge-processor-av1-cll-present-p processor)
           (not hdr-cll-p))
      (bridge-error
       "AV1_CLL_MISSING_AT_RAP"))
    (unless hdr-cll-p
      (setf
       (bridge-processor-av1-rap-without-cll-seen-p processor)
       t))
    (setf
     (bridge-processor-last-av1-rap-ordering-time processor)
     ordering-time))
  t)

(defun copy-descriptor-deep (descriptor)
  "DESCRIPTORとpayloadを複写する。"
  (make-descriptor
   :tag (descriptor-tag descriptor)
   :payload (copy-seq (descriptor-payload descriptor))))

(defun copy-pmt-stream-deep (stream)
  "PMT STREAMとdescriptor列を複写する。"
  (make-pmt-stream
   :stream-type (pmt-stream-stream-type stream)
   :elementary-pid (pmt-stream-elementary-pid stream)
   :descriptors
   (mapcar #'copy-descriptor-deep
           (pmt-stream-descriptors stream))))

(defun copy-program-map-table-deep (table)
  "PROGRAM-MAP-TABLEを再構築用に深く複写する。"
  (make-program-map-table
   :program-number (program-map-table-program-number table)
   :version (program-map-table-version table)
   :current-next-p (program-map-table-current-next-p table)
   :section-number (program-map-table-section-number table)
   :last-section-number (program-map-table-last-section-number table)
   :pcr-pid (program-map-table-pcr-pid table)
   :program-descriptors
   (mapcar #'copy-descriptor-deep
           (program-map-table-program-descriptors table))
   :streams
   (mapcar #'copy-pmt-stream-deep
           (program-map-table-streams table))))

(defun append-pending-entry (processor packet)
  "PROCESSORの出力待ちqueueへPACKETを追加する。"
  (let ((entry
          (make-pending-entry
           :packet packet
           :slot-index
           (+ (bridge-processor-output-slot-index processor)
              (bridge-processor-pending-count processor)))))
    (if (bridge-processor-pending-tail processor)
        (setf (pending-entry-next
               (bridge-processor-pending-tail processor))
              entry)
        (setf (bridge-processor-pending-head processor) entry))
    (setf (bridge-processor-pending-tail processor) entry)
    (incf (bridge-processor-pending-count processor))
    (when (> (bridge-processor-pending-count processor)
             +maximum-pending-packet-count+)
      (bridge-error
       "Semantic buffer exceeds ~D transport packets"
       +maximum-pending-packet-count+))
    entry))

(defun mark-entry-unresolved (entry)
  "ENTRYをsemantic検証待ちにする。"
  (setf (pending-entry-resolved-p entry) nil
        (pending-entry-use-original-p entry) nil
        (pending-entry-replacements entry) '()
        (pending-entry-replacement-provenances entry) '())
  entry)

(defun resolve-entry (entry replacements)
  "ENTRYをREPLACEMENTSで解決する。空listならpacketを抑止する。"
  (setf (pending-entry-resolved-p entry) t
        (pending-entry-use-original-p entry) nil
        (pending-entry-replacements entry) replacements
        (pending-entry-replacement-provenances entry) '())
  entry)

(defun resolve-entry-as-original (entry)
  "ENTRYを元packetのまま解決する。"
  (setf (pending-entry-resolved-p entry) t
        (pending-entry-use-original-p entry) t
        (pending-entry-replacements entry) '()
        (pending-entry-replacement-provenances entry) '())
  entry)

(defun descriptor-payload-prefix-p (descriptor prefix)
  "DESCRIPTORのpayloadがPREFIXで始まるかを返す。"
  (let ((payload (descriptor-payload descriptor)))
    (and (>= (length payload) (length prefix))
         (equalp (subseq payload 0 (length prefix))
                 prefix))))

(defun opus-registration-descriptor-p (descriptor)
  "DESCRIPTORがOpus registrationを示すかを返す。"
  (and (= (descriptor-tag descriptor) #x05)
       (descriptor-payload-prefix-p
        descriptor +opus-registration-identifier+)))

(defun opus-extension-descriptor-p (descriptor)
  "DESCRIPTORがOpus DVB extensionを示すかを返す。"
  (and (= (descriptor-tag descriptor) #x7f)
       (plusp (length (descriptor-payload descriptor)))
       (= (aref (descriptor-payload descriptor) 0) #x80)))

(defun av1-registration-descriptor-p (descriptor)
  "DESCRIPTORがAV01 registrationかを返す。"
  (descriptor-identifier-p descriptor
                           +av1-registration-identifier+))

(defun vp9-registration-descriptor-p (descriptor)
  "DESCRIPTORがVP09 registrationかを返す。"
  (descriptor-identifier-p descriptor
                           +vp9-registration-identifier+))

(defun stream-opus-hint-p (stream)
  "STREAMのdescriptorがOpusを明示するかを返す。"
  (some (lambda (descriptor)
          (or (opus-registration-descriptor-p descriptor)
              (opus-extension-descriptor-p descriptor)))
        (pmt-stream-descriptors stream)))

(defun descriptors-with-tag (descriptors tag)
  "DESCRIPTORSからTAGを持つ要素を順序どおり返す。"
  (remove-if-not
   (lambda (descriptor)
     (= (descriptor-tag descriptor) tag))
   descriptors))

(defun recognized-video-registration-kind (descriptor)
  "DESCRIPTORが示す既知映像codecを返す。"
  (cond
    ((av1-registration-descriptor-p descriptor) :av1)
    ((vp9-registration-descriptor-p descriptor) :vp9)
    (t nil)))

(defun reject-existing-video-mapping (kind descriptors)
  "KINDの既存完全mappingを検証後に二重変換防止で拒否する。"
  (ecase kind
    (:av1
     (validate-av1-mapping-descriptors descriptors))
    (:vp9
     (validate-vp9-mapping-descriptors descriptors)))
  (bridge-error
   "Input video PID already carries a complete ~A mapping"
   (string-upcase (symbol-name kind))))

(defun classify-video-input-stream (stream selected-codec)
  "STREAMをbare/raw hint/非対象へ分類し既存mappingを拒否する。"
  (unless (and (= (pmt-stream-stream-type stream) #x06)
               (not (stream-opus-hint-p stream)))
    (return-from classify-video-input-stream nil))
  (let* ((descriptors (pmt-stream-descriptors stream))
         (registrations (descriptors-with-tag descriptors #x05))
         (private-descriptors (descriptors-with-tag descriptors #x80))
         (recognized
           (remove nil
                   (mapcar #'recognized-video-registration-kind
                           registrations))))
    (when (> (length recognized) 1)
      (bridge-error
       "Input private video PID has conflicting codec registrations"))
    (when (and recognized
               (/= (length registrations) 1))
      (bridge-error
       "Input private video PID mixes known and unknown registrations"))
    (cond
      (recognized
       (let ((kind (first recognized)))
         (when private-descriptors
           (reject-existing-video-mapping kind descriptors))
         (unless (eq kind selected-codec)
           (bridge-error
            "Input video registration ~A conflicts with selected codec ~A"
            (string-upcase (symbol-name kind))
            (string-upcase (symbol-name selected-codec))))
         :registration-hint))
      (registrations
       (bridge-error
        "Input private video PID has an unknown registration"))
      (private-descriptors
       (bridge-error
        "Input private video PID has an unknown private mapping"))
      ;; ARIB字幕などのprivate PESはdata_component_descriptor等で用途を
      ;; 明示する。既知映像registrationを持たない有descriptor ESをbare
      ;; 映像候補へ含めると字幕を第2映像と誤認するため、非対象として保持する。
      (descriptors nil)
      (t :bare))))

(defun select-video-pid (table selected-codec)
  "TABLEから一意な変換対象private video PIDを選ぶ。"
  (let ((candidates
          (loop
            for stream in (program-map-table-streams table)
            when
              (classify-video-input-stream
               stream selected-codec)
              collect stream)))
    (when (null candidates)
      (bridge-error
       "PMT has no identifiable private video stream"))
    (when (> (length candidates) 1)
      (bridge-error
       "PMT has multiple private video candidates; selection is ambiguous"))
    (pmt-stream-elementary-pid (first candidates))))

(defun select-opus-pids (table)
  "TABLEの全Opus PIDを検証対象として返す。"
  (let ((opus-streams
          (remove-if-not #'stream-opus-hint-p
                         (program-map-table-streams table))))
    (when (null opus-streams)
      (bridge-error "PMT contains no identifiable Opus stream"))
    (mapcar #'pmt-stream-elementary-pid opus-streams)))

(defun validate-opus-descriptor-members (descriptors)
  "DESCRIPTORSのOpus pairを検証し、欠落registrationの有無を返す。"
  (let ((all-registrations
          (descriptors-with-tag descriptors #x05))
        (registrations
          (remove-if-not #'opus-registration-descriptor-p
                         descriptors))
        (extensions
          (remove-if-not #'opus-extension-descriptor-p
                         descriptors))
        (unknown-extensions
          (remove-if
           #'opus-extension-descriptor-p
           (descriptors-with-tag descriptors #x7f))))
    (when
        (some (lambda (descriptor)
                (not (opus-registration-descriptor-p descriptor)))
              all-registrations)
      (bridge-error
       "Opus stream contains an unknown or conflicting registration descriptor"))
    (when (> (length all-registrations) 1)
      (bridge-error "Opus registration descriptor is duplicated"))
    (when (> (length registrations) 1)
      (bridge-error "Opus registration descriptor is duplicated"))
    (when (> (length extensions) 1)
      (bridge-error "Opus DVB extension descriptor is duplicated"))
    (when unknown-extensions
      (bridge-error "Opus stream contains an unknown extension descriptor"))
    (when (and registrations
               (/= (length
                    (descriptor-payload (first registrations)))
                   4))
      (bridge-error "Opus registration descriptor length is invalid"))
    (unless extensions
      (if registrations
          (bridge-error
           "Opus channel configuration cannot be inferred from a missing extension descriptor")
          (bridge-error "Opus descriptors are missing")))
    (let* ((extension (first extensions))
           (payload (descriptor-payload extension)))
      (unless (= (length payload) 2)
        (bridge-error "Opus DVB extension descriptor length is invalid"))
      (unless (valid-opus-channel-configuration-p (aref payload 1))
        (bridge-error "Opus channel configuration is unsupported: 0x~2,'0X"
                      (aref payload 1))))
    (null registrations)))

(defun repair-opus-stream-descriptors (stream)
  "STREAMのOpus descriptorを検証し、一意なregistration欠落だけを補う。"
  (unless (= (pmt-stream-stream-type stream) #x06)
    (bridge-error "Opus stream type is not private PES: 0x~2,'0X"
                  (pmt-stream-stream-type stream)))
  (let* ((descriptors (pmt-stream-descriptors stream))
         (missing-registration-p
           (validate-opus-descriptor-members descriptors)))
    (if missing-registration-p
        (let ((extension
                (find-if #'opus-extension-descriptor-p
                         descriptors)))
          (setf (pmt-stream-descriptors stream)
                (list*
                 (make-descriptor
                  :tag #x05
                  :payload
                  (copy-seq +opus-registration-identifier+))
                 extension
                 (remove extension descriptors :test #'eq))))
        (unless (and (opus-registration-descriptor-p
                      (first descriptors))
                     (opus-extension-descriptor-p
                      (second descriptors)))
          (bridge-error
           "Opus descriptors are not the first ordered descriptor pair"))))
  stream)

(defun mapping-descriptor-remainder (descriptors)
  "映像mappingが所有するtag 0x05/0x80以外を保持する。"
  (remove-if
   (lambda (descriptor)
     (member (descriptor-tag descriptor)
             '(#x05 #x80)
             :test #'=))
   descriptors))

(defun apply-video-mapping (stream codec configuration)
  "STREAMへCODECの映像mappingを適用する。"
  (setf (pmt-stream-stream-type stream) #x06)
  (ecase codec
    (:vp9
     (setf (pmt-stream-descriptors stream)
           (append
            (make-vp9-mapping-descriptors)
            (mapping-descriptor-remainder
             (pmt-stream-descriptors stream)))))
    (:av1
     (unless configuration
       (bridge-error
        "AV1 PMT cannot be built before a sequence header"))
     (setf (pmt-stream-descriptors stream)
           (append
            (make-av1-mapping-descriptors configuration)
            (mapping-descriptor-remainder
             (pmt-stream-descriptors stream))))))
  stream)

(defun apply-pmt-mappings
    (table video-codec video-pid audio-codec opus-pids
     av1-configuration)
  "TABLEの対象ESへ選択codec mappingを適用したcopyを返す。"
  (let ((result (copy-program-map-table-deep table)))
    (unless (eq video-codec :passthrough)
      (let ((video-stream
              (find video-pid
                    (program-map-table-streams result)
                    :key #'pmt-stream-elementary-pid
                    :test #'=)))
        (unless video-stream
          (bridge-error "Selected video PID is absent from PMT: 0x~4,'0X"
                        video-pid))
        (apply-video-mapping
         video-stream video-codec av1-configuration)))
    (when (eq audio-codec :opus)
      (dolist (opus-pid opus-pids)
        (let ((opus-stream
                (find opus-pid
                      (program-map-table-streams result)
                      :key #'pmt-stream-elementary-pid
                      :test #'=)))
          (unless opus-stream
            (bridge-error "Selected Opus PID is absent from PMT: 0x~4,'0X"
                          opus-pid))
          (repair-opus-stream-descriptors opus-stream))))
    result))

(defun pmt-signature (table)
  "TABLEのversionだけを正規化したCRC付きsignatureを返す。"
  (let ((copy (copy-program-map-table-deep table)))
    (setf (program-map-table-version copy) 0)
    (build-pmt-section copy)))

(defun validate-input-pmt-transition (processor table)
  "入力PMTの内容とversionが同期して遷移することを検証する。"
  (let ((signature (pmt-signature table))
        (version (program-map-table-version table))
        (last-signature
          (bridge-processor-last-input-pmt-signature processor))
        (last-version
          (bridge-processor-last-input-pmt-version processor)))
    (when last-signature
      (let ((same-content-p (equalp signature last-signature)))
        (cond
          ((and same-content-p (/= version last-version))
           (bridge-error
            "PMT version changes without a content change"))
          ((and (not same-content-p)
                (/= version (logand (+ last-version 1) #x1f)))
           (bridge-error
            "PMT content changes without the next modulo-32 version")))))
    (setf (bridge-processor-last-input-pmt-signature processor)
          signature
          (bridge-processor-last-input-pmt-version processor)
          version))
  table)

(defun pat-signature (table)
  "TABLEのversionだけを正規化したCRC付きsignatureを返す。"
  (let ((copy (copy-program-association-table table)))
    (setf (program-association-table-version copy) 0)
    (build-pat-section copy)))

(defun validate-input-pat-transition (processor table)
  "入力PATの内容とversionが同期して遷移することを検証する。"
  (let ((signature (pat-signature table))
        (version (program-association-table-version table))
        (last-signature
          (bridge-processor-last-input-pat-signature processor))
        (last-version
          (bridge-processor-last-input-pat-version processor)))
    (when last-signature
      (let ((same-content-p (equalp signature last-signature)))
        (cond
          ((and same-content-p (/= version last-version))
           (bridge-error
            "PAT version changes without a content change"))
          ((and (not same-content-p)
                (/= version (logand (+ last-version 1) #x1f)))
           (bridge-error
            "PAT content changes without the next modulo-32 version")))))
    (setf (bridge-processor-last-input-pat-signature processor)
          signature
          (bridge-processor-last-input-pat-version processor)
          version))
  table)

(defun reset-pmt-output-state-for-pid (processor pmt-pid)
  "PMT PID変更時にversion/continuity計画を初期化する。"
  (unless (eql (bridge-processor-planned-pmt-pid processor)
               pmt-pid)
    (setf (bridge-processor-planned-pmt-pid processor) pmt-pid
          (bridge-processor-last-source-pmt-signature processor) nil
          (bridge-processor-last-source-pmt-version processor) nil
          (bridge-processor-last-output-pmt-signature processor) nil
          (bridge-processor-last-output-pmt-version processor) nil
          (bridge-processor-advertised-av1-configuration processor) nil)
    (remhash pmt-pid
             (bridge-processor-output-continuity-counters processor))))

(defun select-output-pmt-version
    (processor source-table mapped-table source-section)
  "入力と直前出力からMAPPED-TABLEのPMT versionを決める。"
  (let* ((source-signature (pmt-signature source-table))
         (mapped-signature (pmt-signature mapped-table))
         (source-version
           (program-map-table-version source-table))
         (mapped-at-source-version
           (copy-program-map-table-deep mapped-table))
         (first-output-p
           (null (bridge-processor-last-output-pmt-version
                  processor)))
         (same-plan-p
           (and (not first-output-p)
                (equalp source-signature
                        (bridge-processor-last-source-pmt-signature
                         processor))
                (equalp mapped-signature
                        (bridge-processor-last-output-pmt-signature
                         processor))))
         (output-version
           (cond
             (first-output-p
              (setf (program-map-table-version
                     mapped-at-source-version)
                    source-version)
              (if (equalp (build-pmt-section
                           mapped-at-source-version)
                          source-section)
                  source-version
                  (logand (+ source-version 1) #x1f)))
             (same-plan-p
              (bridge-processor-last-output-pmt-version processor))
             (t
              (logand
               (+ (bridge-processor-last-output-pmt-version
                   processor)
                  1)
               #x1f)))))
    (setf (bridge-processor-last-source-pmt-signature processor)
          source-signature
          (bridge-processor-last-source-pmt-version processor)
          source-version
          (bridge-processor-last-output-pmt-signature processor)
          mapped-signature
          (bridge-processor-last-output-pmt-version processor)
          output-version)
    output-version))

(defun output-continuity-counter
    (processor pid fallback)
  "PIDの次出力continuity counterを返す。未設定時はFALLBACKを使う。"
  (multiple-value-bind (counter present-p)
      (gethash pid
               (bridge-processor-output-continuity-counters processor))
    (if present-p counter fallback)))

(defun record-output-payload-count
    (processor pid start count)
  "PIDでCOUNT個payload packetを出した後のcounterを記録する。"
  (setf (gethash pid
                 (bridge-processor-output-continuity-counters processor))
        (logand (+ start count) #x0f)))

(defun packetize-pmt-section
    (processor pmt-pid section continuity-fallback transport-priority)
  "SECTIONをpointer field付きPMT packet列へ変換する。"
  (let ((payload
           (make-array (+ 1 (length section))
                       :element-type 'octet
                       :initial-element 0))
         (start
           (output-continuity-counter
            processor pmt-pid continuity-fallback)))
    (replace payload section :start1 1)
    (let ((packets
            (packetize-payload
             pmt-pid payload
             :continuity-counter start
             :payload-unit-start t
             :transport-priority transport-priority)))
      (record-output-payload-count
       processor pmt-pid start (length packets))
      packets)))

(defun plan-pmt-packets
    (processor source-table source-section pmt-pid video-pid
     opus-pids configuration continuity-fallback transport-priority)
  "source PMTからmapping/version/CRCを更新したpacket列を作る。"
  (reset-pmt-output-state-for-pid processor pmt-pid)
  (let* ((mapped
           (apply-pmt-mappings
            source-table
            (bridge-processor-video-codec processor)
            video-pid
            (bridge-processor-audio-codec processor)
            opus-pids
            configuration))
         (version
           (select-output-pmt-version
            processor source-table mapped source-section)))
    (setf (program-map-table-version mapped) version)
    (when (eq (bridge-processor-video-codec processor) :av1)
      (setf (bridge-processor-advertised-av1-configuration processor)
            configuration))
    (packetize-pmt-section
     processor pmt-pid (build-pmt-section mapped)
     continuity-fallback transport-priority)))

(defun next-video-event-configuration (entry)
  "ENTRY以降で最初に完成したAV1 video eventの構成を返す。"
  (loop for cursor = (pending-entry-next entry)
          then (pending-entry-next cursor)
        while cursor
        for event = (pending-entry-event cursor)
        when (typep event 'video-event)
          do (return (video-event-configuration event))))

(defun configuration-for-pmt-event (processor entry)
  "ENTRYのPMTが告知すべきAV1構成を返す。"
  (or (next-video-event-configuration entry)
      (bridge-processor-last-av1-configuration processor)))

(defun source-pmt-mapping-unchanged-p
    (processor event configuration)
  "EVENTのsource PMTが検証後もbyte単位で不変かを返す。"
  (let ((mapped
          (apply-pmt-mappings
           (pmt-event-table event)
           (bridge-processor-video-codec processor)
           (pmt-event-video-pid event)
           (bridge-processor-audio-codec processor)
           (pmt-event-opus-pids event)
           configuration)))
    (equalp (build-pmt-section mapped)
            (pmt-event-source-section event))))

(defun resolve-pmt-event (processor entry event)
  "EVENTのPMTを時系列上の現在状態で再構築する。"
  (let ((configuration
          (if (eq (bridge-processor-video-codec processor) :av1)
              (configuration-for-pmt-event processor entry)
              nil)))
    (when (and (eq (bridge-processor-video-codec processor) :av1)
               (null configuration))
      (return-from resolve-pmt-event nil))
    (let* ((entries (pmt-event-entries event))
           (first-entry (first entries))
           (template (pending-entry-packet first-entry)))
      (when (and
             (null
              (bridge-processor-last-output-pmt-version processor))
             (source-pmt-mapping-unchanged-p
              processor event configuration))
        (reset-pmt-output-state-for-pid
         processor (pmt-event-pmt-pid event))
        (let ((signature
                (pmt-signature (pmt-event-table event)))
              (version
                (program-map-table-version
                 (pmt-event-table event))))
          (setf
           (bridge-processor-last-source-pmt-signature processor)
           signature
           (bridge-processor-last-source-pmt-version processor)
           version
           (bridge-processor-last-output-pmt-signature processor)
           signature
           (bridge-processor-last-output-pmt-version processor)
           version))
        (let ((last-payload-entry
                (find-if
                 (lambda (source-entry)
                   (ts-has-payload-p
                    (pending-entry-packet source-entry)))
                 entries
                 :from-end t)))
          (unless last-payload-entry
            (bridge-error
             "Unchanged PMT group has no payload transport packet"))
          (setf
           (gethash
            (pmt-event-pmt-pid event)
            (bridge-processor-output-continuity-counters processor))
           (logand
            (+ (ts-continuity-counter
                (pending-entry-packet last-payload-entry))
               1)
            #x0f)))
        (dolist (source-entry entries)
          (resolve-entry-as-original source-entry))
        (return-from resolve-pmt-event t))
      (let ((packets
              (plan-pmt-packets
               processor
               (pmt-event-table event)
               (pmt-event-source-section event)
               (pmt-event-pmt-pid event)
               (pmt-event-video-pid event)
               (pmt-event-opus-pids event)
               configuration
               (ts-continuity-counter template)
               (ts-transport-priority-p template))))
        (resolve-entry first-entry packets)
        (dolist (other (rest entries))
          (resolve-entry other '()))
        t))))

(defun adaptation-output-counter (next-counter fallback)
  "payloadを増分しないadaptation-only packetのcounterを返す。"
  (if next-counter
      (logand (- next-counter 1) #x0f)
      fallback))

(defun append-entry-replacement (entry packet)
  "ENTRYのreplacement末尾へPACKETを追加する。"
  (setf (pending-entry-replacements entry)
        (append (pending-entry-replacements entry)
                (list packet)))
  (when (pending-entry-replacement-provenances entry)
    (setf
     (pending-entry-replacement-provenances entry)
     (append
      (pending-entry-replacement-provenances entry)
      (list nil))))
  entry)

(defun resolve-video-packet-entries
    (processor entries pes random-access-flags)
  "ENTRIESへPESを再配置し、PCR/adaptation情報とCCを維持する。"
  (let* ((payload-entries
           (remove-if-not
            (lambda (entry)
              (ts-has-payload-p
               (pending-entry-packet entry)))
            entries))
         (first-payload-entry (first payload-entries)))
    (unless first-payload-entry
      (bridge-error "Video PES has no payload transport packet"))
    (let* ((pid
             (ts-pid (pending-entry-packet
                      first-payload-entry)))
           (next-counter
             (output-continuity-counter
              processor pid
              (ts-continuity-counter
               (pending-entry-packet first-payload-entry))))
           (offset 0)
           (first-p t)
           (last-payload-entry nil)
           (last-template
             (pending-entry-packet first-payload-entry)))
      (dolist (entry entries)
        (let ((template (pending-entry-packet entry)))
          (cond
            ((ts-has-payload-p template)
             (when (ts-discontinuity-indicator-p template)
               (setf next-counter
                     (ts-continuity-counter template)))
             (let* ((flags (if first-p random-access-flags 0))
                    (capacity
                      (template-payload-capacity template flags))
                    (remaining (- (length pes) offset))
                    (count (min capacity remaining)))
               (if (plusp count)
                   (let ((packet
                           (make-payload-packet-from-template
                            template
                            (subseq pes offset (+ offset count))
                            next-counter
                            :payload-unit-start first-p
                            :adaptation-flags flags
                            :clear-adaptation-flags #x60)))
                     (resolve-entry entry (list packet))
                     (setf last-payload-entry entry
                           last-template template
                           offset (+ offset count)
                           next-counter
                           (logand (+ next-counter 1) #x0f)
                           first-p nil))
                   (let ((adaptation-only
                           (make-adaptation-only-from-template
                            template
                            (adaptation-output-counter
                             next-counter
                             (ts-continuity-counter template))
                            :clear-adaptation-flags #x60)))
                     (resolve-entry
                      entry
                      (if adaptation-only
                          (list adaptation-only)
                          '()))))))
            (t
             (let ((adaptation-only
                     (make-adaptation-only-from-template
                      template
                      (adaptation-output-counter
                       next-counter
                       (ts-continuity-counter template))
                      :clear-adaptation-flags #x60)))
               (resolve-entry
                entry
                (if adaptation-only
                    (list adaptation-only)
                    '())))))))
      (unless last-payload-entry
        (bridge-error "Video PES produced no payload packet"))
      (loop while (< offset (length pes))
            for remaining = (- (length pes) offset)
            for count = (min 184 remaining)
            for packet =
              (make-extra-payload-packet
               last-template
               (subseq pes offset (+ offset count))
               next-counter)
            do (append-entry-replacement
                last-payload-entry packet)
               (setf offset (+ offset count)
                     next-counter
                     (logand (+ next-counter 1) #x0f)))
      (setf (gethash pid
                     (bridge-processor-output-continuity-counters
                      processor))
            next-counter))))

(defun source-section-for-table (table)
  "TABLEを現在versionのCRC付きsectionへ変換する。"
  (build-pmt-section (copy-program-map-table-deep table)))

(defun repacketize-holdback-slot-count (processor)
  "明示CBRで2ms未満となる、未出力保持可能なpacket数を返す。"
  (let ((model
          (bridge-processor-tstd-model processor)))
    (if model
        (floor
         (*
          (tstd-arrival-clock-transport-rate-bps
           (tstd-model-clock model))
          +repacketize-deadline-milliseconds+)
         (* 1000 8 +ts-packet-size+))
        0)))

(defun synthetic-pmt-before-video (processor event)
  "EVENTのAV1構成をAU直前へ告知するPMT packet列を返す。"
  (let ((table (video-event-source-pmt-table event))
        (pmt-pid (video-event-pmt-pid event))
        (template (video-event-pmt-template event)))
    (unless (and table pmt-pid template)
      (bridge-error "AV1 configuration change has no source PMT"))
    (plan-pmt-packets
     processor
     table
     (source-section-for-table table)
     pmt-pid
     (video-event-video-pid event)
     (video-event-opus-pids event)
     (video-event-configuration event)
     (ts-continuity-counter template)
     (ts-transport-priority-p template))))

(defun backfill-synthetic-pmt-before-video
    (processor packets video-start-entry)
  "VIDEO-START-ENTRY前2ms内の未出力nullへPACKETSを時系列順に置く。

PCR付きPUSIを含む映像target packetは移動も分割もしない。先行nullを
確保できない場合はPMT-before-PUSIを満たせないためfail closedにする。"
  (unless packets
    (return-from backfill-synthetic-pmt-before-video '()))
  (let* ((origin-slot
           (pending-entry-slot-index video-start-entry))
         (holdback-count
           (repacketize-holdback-slot-count processor))
         (latest-entry
           (bridge-processor-pending-tail processor))
         (latest-slot
           (and latest-entry
                (pending-entry-slot-index latest-entry)))
         (earliest-slot
           (max
            0
            (-
             (or latest-slot origin-slot)
             holdback-count)))
         (candidates '()))
    (when (or
           (< holdback-count (length packets))
           (and latest-slot
                (> latest-slot
                   (+
                    origin-slot
                    (- holdback-count (length packets))))))
      (bridge-error
       "REPACKETIZE_CAPACITY_EXHAUSTED reason=pmt_backfill_information_deadline origin_slot=~D deadline_slot=~D actual_slot=~D"
       origin-slot
       (+
        origin-slot
        (- holdback-count (length packets)))
       (or latest-slot origin-slot)))
    (loop for entry = (bridge-processor-pending-head processor)
            then (pending-entry-next entry)
          while entry
          for slot = (pending-entry-slot-index entry)
          while (< slot origin-slot)
          when (and
                (>= slot earliest-slot)
                (pending-entry-resolved-p entry)
                (pending-entry-use-original-p entry)
                (= (ts-pid (pending-entry-packet entry))
                   +ts-null-pid+))
            do (push entry candidates))
    (setf candidates (nreverse candidates))
    (when (< (length candidates) (length packets))
      (bridge-error
       "REPACKETIZE_CAPACITY_EXHAUSTED reason=pmt_backfill_null_slots origin_slot=~D earliest_slot=~D required_packets=~D available_null_slots=~D"
       origin-slot
       earliest-slot
       (length packets)
       (length candidates)))
    (let ((selected
            (nthcdr
             (- (length candidates) (length packets))
             candidates)))
      (loop for entry in selected
            for packet in packets
            do
        ;; entryはまだallocatorへ渡していないoriginal nullだけに限定する。
        ;; slot indexは変えず、その固定slotの最終packetだけをPMTへ置換する。
        (setf
         (pending-entry-packet entry) packet
         (pending-entry-resolved-p entry) t
         (pending-entry-use-original-p entry) t
         (pending-entry-replacements entry) '()
         (pending-entry-replacement-provenances entry) '())))
    packets))

(defun register-av1-tstd-access-unit
    (processor video-pid header configuration semantics
     &optional refresh-frame-flags)
  "AV1 AUを最終packet出力前にprocessorのT-STDへ登録する。"
  (let ((model
          (or
           (bridge-processor-tstd-model processor)
           (bridge-error "TSTD_MODEL_MISSING_FOR_AV1")))
        (pcr-pid
          (bridge-processor-current-pcr-pid processor))
        (pts
          (or
           (pes-header-pts header)
           (bridge-error "TSTD_AV1_PTS_UNAVAILABLE")))
        (dts
          (or
           (pes-header-dts header)
           (pes-header-pts header)
           (bridge-error "TSTD_AV1_DTS_UNAVAILABLE"))))
    (register-tstd-access-unit
     model video-pid pcr-pid pts dts configuration
     semantics refresh-frame-flags)))

(defun resolve-video-event (processor event)
  "EVENTのPESをmapping形式へ再構築しentryを解決する。"
  (let* ((entries (video-event-entries event))
         (video-start-entry
           (or
            (find-if
             (lambda (entry)
               (ts-payload-unit-start-p
                (pending-entry-packet entry)))
             entries)
            (bridge-error
             "Video PES entries have no PUSI packet")))
         (configuration (video-event-configuration event))
         (synthetic-pmt
           (if (and (eq (video-event-codec event) :av1)
                    (not
                     (equalp
                      configuration
                      (bridge-processor-advertised-av1-configuration
                       processor))))
               (synthetic-pmt-before-video processor event)
               '()))
         (flags
           (if (video-event-random-access-p event)
               (if (eq (video-event-codec event) :av1)
                   #x60
                   #x40)
               0)))
    (when (eq (video-event-codec event) :av1)
      (register-av1-tstd-access-unit
       processor
       (video-event-video-pid event)
       (parse-pes-header (video-event-pes event))
       configuration
       (or
        (video-event-frame-semantics event)
        (bridge-error "TSTD_AV1_FRAME_SEMANTICS_MISSING"))
       (video-event-refresh-frame-flags event)))
    (when synthetic-pmt
      (backfill-synthetic-pmt-before-video
       processor synthetic-pmt video-start-entry))
    (resolve-video-packet-entries
     processor entries (video-event-pes event) flags)
    t))

(defun resolve-head-event-if-possible (processor entry)
  "ENTRY先頭eventをsemantic情報が揃っていれば解決する。"
  (let ((event (pending-entry-event entry)))
    (cond
      ((typep event 'pmt-event)
       (resolve-pmt-event processor entry event))
      ((typep event 'video-event)
       (resolve-video-event processor event))
      (t nil))))

(defun emit-output-packet (processor packet)
  "PROCESSORの最終T-STD sinkとOUTPUTへPACKETを出力する。"
  (let ((model
          (bridge-processor-tstd-model processor)))
    (when model
      (process-tstd-output-packet model packet)))
  (%write-octets
   (bridge-processor-output processor)
   packet
   +ts-packet-size+))

(defun repacketize-deadline-slot-from-origin
    (processor origin-slot
     &optional
       (deadline-milliseconds
         +repacketize-deadline-milliseconds+))
  "ORIGIN-SLOTから指定時間内に許される最終slot indexを返す。"
  (let ((model
          (bridge-processor-tstd-model processor)))
    (if model
        (+ origin-slot
           (floor
            (*
             (tstd-arrival-clock-transport-rate-bps
              (tstd-model-clock model))
             deadline-milliseconds)
            (* 1000 8 +ts-packet-size+)))
        origin-slot)))

(defun repacketize-deadline-slot (processor)
  "現在slotから2ms以内に許される最終slot indexを返す。"
  (repacketize-deadline-slot-from-origin
   processor
   (bridge-processor-output-slot-index processor)))

(defun append-av1-stream-byte-segment
    (processor assembler octets origin-slot)
  "変換済みOCTETSを入力ORIGIN-SLOT付きFIFO segmentへ追加する。"
  (when (zerop (length octets))
    (return-from append-av1-stream-byte-segment nil))
  (let* ((payload-unit-start-p
           (pes-assembler-av1-stream-output-start-p assembler))
         (segment
          (make-av1-stream-byte-segment
           :octets octets
           :origin-slot origin-slot
           :deadline-slot
           (repacketize-deadline-slot-from-origin
            processor origin-slot
            +av1-stream-repacketize-deadline-milliseconds+)
           :payload-unit-start-p payload-unit-start-p
           :adaptation-flags
           (if (and
                payload-unit-start-p
                (pes-assembler-stream-random-access-kind assembler))
               #x60
               0))))
    (if (pes-assembler-av1-stream-byte-tail assembler)
        (setf
         (av1-stream-byte-segment-next
          (pes-assembler-av1-stream-byte-tail assembler))
         segment)
        (setf
         (pes-assembler-av1-stream-byte-head assembler)
         segment))
    (setf
     (pes-assembler-av1-stream-byte-tail assembler)
     segment)
    (incf
     (pes-assembler-av1-stream-byte-count assembler)
     (length octets))
    (when payload-unit-start-p
      (setf
       (pes-assembler-av1-stream-output-start-p assembler)
       nil))
    segment))

(defun validate-av1-stream-byte-deadline
    (processor assembler
     &key consume-current-slot-p actual-slot)
  "AV1 byte FIFO先頭segmentのAU間null待機期限を検証する。"
  (let ((segment
          (pes-assembler-av1-stream-byte-head assembler))
        (resolved-slot
          (or
           actual-slot
           (bridge-processor-output-slot-index processor))))
    (when (and
           segment
           (> resolved-slot
              (+
               (av1-stream-byte-segment-deadline-slot segment)
               (if consume-current-slot-p 1 0))))
      (bridge-error
       "REPACKETIZE_CAPACITY_EXHAUSTED origin_slot=~D deadline_slot=~D actual_slot=~D residual_bytes=~D"
       (av1-stream-byte-segment-origin-slot segment)
       (av1-stream-byte-segment-deadline-slot segment)
       resolved-slot
       (pes-assembler-av1-stream-byte-count assembler))))
  assembler)

(defun drain-av1-stream-bytes (assembler maximum-count)
  "AV1 byte FIFO先頭からPUSI境界を跨がず最大 byte 数を取り出す。"
  (let ((result
          (make-array maximum-count
                      :element-type 'octet))
        (output-offset 0))
    (loop while (and
                 (< output-offset maximum-count)
                 (pes-assembler-av1-stream-byte-head assembler))
          for segment =
            (pes-assembler-av1-stream-byte-head assembler)
          do
      ;; PES末尾残留と次PES先頭を同じTS payloadへ混在させない。
      (when (and
             (plusp output-offset)
             (av1-stream-byte-segment-payload-unit-start-p segment))
        (return))
      (let* ((segment-octets
               (av1-stream-byte-segment-octets segment))
             (segment-offset
               (av1-stream-byte-segment-offset segment))
             (count
               (min
                (- maximum-count output-offset)
                (- (length segment-octets) segment-offset))))
        (replace result segment-octets
                 :start1 output-offset
                 :start2 segment-offset
                 :end2 (+ segment-offset count))
        (incf output-offset count)
        (incf (av1-stream-byte-segment-offset segment) count)
        (decf (pes-assembler-av1-stream-byte-count assembler)
              count)
        ;; PUSIはsegmentの最初の出力packetだけに付ける。
        (when (and
               (plusp count)
               (av1-stream-byte-segment-payload-unit-start-p
                segment))
          (setf
           (av1-stream-byte-segment-payload-unit-start-p segment)
           nil
           (av1-stream-byte-segment-adaptation-flags segment)
           0))
        (when (= (av1-stream-byte-segment-offset segment)
                 (length segment-octets))
          (setf
           (pes-assembler-av1-stream-byte-head assembler)
           (av1-stream-byte-segment-next segment))
          (unless (pes-assembler-av1-stream-byte-head assembler)
            (setf
             (pes-assembler-av1-stream-byte-tail assembler)
             nil)))))
    (if (= output-offset maximum-count)
        result
        (subseq result 0 output-offset))))

(defun active-av1-stream-byte-assembler (processor)
  "現在のAV1 streaming assemblerがbyte residualを持てば返す。"
  (let* ((pid
           (bridge-processor-current-video-pid processor))
         (assembler
           (and
            pid
            (gethash
             pid
             (bridge-processor-pes-assemblers processor)))))
    (when (and
           assembler
           (pes-assembler-active-p assembler)
           (pes-assembler-streaming-p assembler)
           (eq (bridge-processor-video-codec processor) :av1)
           (plusp
            (pes-assembler-av1-stream-byte-count assembler)))
      assembler)))

(defun enqueue-av1-stream-target-entry
    (assembler entry &key (kind :video))
  "未出力AV1 target templateをsource slot順queueへ追加する。"
  (let ((target
          (make-av1-stream-target-entry
           :entry entry :kind kind)))
    (if (pes-assembler-av1-stream-target-tail assembler)
        (setf
         (av1-stream-target-entry-next
          (pes-assembler-av1-stream-target-tail assembler))
         target)
        (setf
         (pes-assembler-av1-stream-target-head assembler)
         target))
    (setf
     (pes-assembler-av1-stream-target-tail assembler)
     target)
    (incf
     (pes-assembler-av1-stream-target-count assembler))
    target))

(defun dequeue-av1-stream-target-entry (assembler)
  "AV1 target template queueの先頭ENTRYを取り出す。"
  (let ((target
          (pes-assembler-av1-stream-target-head assembler)))
    (unless target
      (return-from dequeue-av1-stream-target-entry nil))
    (setf
     (pes-assembler-av1-stream-target-head assembler)
     (av1-stream-target-entry-next target))
    (decf
     (pes-assembler-av1-stream-target-count assembler))
    (unless (pes-assembler-av1-stream-target-head assembler)
      (setf
       (pes-assembler-av1-stream-target-tail assembler)
       nil))
    (av1-stream-target-entry-entry target)))

(defun make-av1-stream-null-continuation
    (processor assembler)
  "現在null slotへAV1 FIFO先頭から最大184 byteを配置する。"
  (let* ((template
          (or
           (pes-assembler-av1-stream-last-template assembler)
           (bridge-error
            "AV1 streaming residual has no video packet template")))
         (segment
           (or
            (pes-assembler-av1-stream-byte-head assembler)
            (bridge-error
             "AV1 streaming continuation has no byte segment")))
         (payload-unit-start-p
           (av1-stream-byte-segment-payload-unit-start-p segment))
         (flags
           (av1-stream-byte-segment-adaptation-flags segment))
         (capacity
           (if (plusp flags) 182 184)))
    (validate-av1-stream-byte-deadline
     processor assembler :consume-current-slot-p t)
    (let* ((pid (pes-assembler-pid assembler))
           (counter
             (output-continuity-counter
              processor pid
              (ts-continuity-counter template)))
           (payload
             (drain-av1-stream-bytes assembler capacity))
           (packet
             (make-ts-packet
              pid counter payload
              :payload-unit-start payload-unit-start-p
              :random-access (logbitp 6 flags)
              :elementary-stream-priority (logbitp 5 flags)
              :transport-priority
              (ts-transport-priority-p template))))
      (record-output-payload-count processor pid counter 1)
      packet)))

(defun finish-av1-stream-byte-fifo (assembler)
  "PUSI/EOF境界でAV1 byte residualが残らないことを検証する。"
  (when (plusp
         (pes-assembler-av1-stream-byte-count assembler))
    (let ((segment
            (pes-assembler-av1-stream-byte-head assembler)))
      (bridge-error
       "REPACKETIZE_CAPACITY_EXHAUSTED residual_bytes=~D origin_slot=~D deadline_slot=~D"
       (pes-assembler-av1-stream-byte-count assembler)
       (av1-stream-byte-segment-origin-slot segment)
       (av1-stream-byte-segment-deadline-slot segment))))
  assembler)

(defun enqueue-output-packet
    (processor packet deadline-slot &key origin-slot)
  "既存null/reclaimed slot待ちextra PACKETをFIFO末尾へ追加する。"
  (when (ts-pcr packet)
    (bridge-error
     "REPACKETIZE_PCR_CANNOT_USE_RECLAIMED_SLOT"))
  (when (ts-discontinuity-indicator-p packet)
    (bridge-error
     "REPACKETIZE_DISCONTINUITY_CANNOT_USE_RECLAIMED_SLOT"))
  (let ((entry
          (make-output-packet-entry
           :packet packet
           :origin-slot origin-slot
           :deadline-slot deadline-slot)))
    (if (bridge-processor-output-packet-tail processor)
        (setf
         (output-packet-entry-next
          (bridge-processor-output-packet-tail processor))
         entry)
        (setf
         (bridge-processor-output-packet-head processor)
         entry))
    (setf
     (bridge-processor-output-packet-tail processor)
     entry)
    (incf (bridge-processor-output-packet-count processor))
    (when (> (bridge-processor-output-packet-count processor)
             +maximum-pending-packet-count+)
      (bridge-error
       "REPACKETIZE_CAPACITY_EXHAUSTED pending_packets=~D"
       (bridge-processor-output-packet-count processor)))
    entry))

(defun dequeue-output-packet (processor)
  "固定packet allocatorのFIFO先頭packetを取り出す。"
  (let ((entry
          (bridge-processor-output-packet-head processor)))
    (unless entry
      (return-from dequeue-output-packet nil))
    (setf
     (bridge-processor-output-packet-head processor)
     (output-packet-entry-next entry))
    (decf (bridge-processor-output-packet-count processor))
    (unless (bridge-processor-output-packet-head processor)
      (setf
       (bridge-processor-output-packet-tail processor)
       nil))
    (output-packet-entry-packet entry)))

(defun validate-repacketize-deadline
    (processor &key consume-current-slot-p)
  "FIFO先頭extraが2ms deadlineを越えていないか検証する。

CONSUME-CURRENT-SLOT-Pなら現在slotの先頭でextraを消費できるため、
生成元slotの末尾から現在slotの先頭までを待ち時間として判定する。"
  (let ((entry
          (bridge-processor-output-packet-head processor)))
    (when (and
           ;; 2 ms 制約は明示 CBR を必須とする AV1 搬送だけの契約。
           ;; VP9/Opus の固定 packet allocator は EOF 容量だけを検証する。
           (bridge-processor-tstd-model processor)
           entry
           (> (bridge-processor-output-slot-index processor)
              (+
               (output-packet-entry-deadline-slot entry)
               (if consume-current-slot-p 1 0))))
      (if (output-packet-entry-origin-slot entry)
          (let* ((model
                   (bridge-processor-tstd-model processor))
                 (packet-index
                   (and model
                        (tstd-model-packet-index model))))
            (bridge-error
             "REPACKETIZE_CAPACITY_EXHAUSTED origin_slot=~D deadline_slot=~D actual_slot=~D pid=0x~4,'0X pusi=~A pcr=~A discontinuity=~A pending_packets=~D tstd_overflow=~A tb_fullness=~A tb_rate=~A tb_last_arrival=~A tb_service_end=~A tstd_packet_index=~A"
             (output-packet-entry-origin-slot entry)
             (output-packet-entry-deadline-slot entry)
             (bridge-processor-output-slot-index processor)
             (ts-pid (output-packet-entry-packet entry))
             (ts-payload-unit-start-p
              (output-packet-entry-packet entry))
             (not
              (null
               (ts-pcr (output-packet-entry-packet entry))))
             (ts-discontinuity-indicator-p
              (output-packet-entry-packet entry))
             (bridge-processor-output-packet-count processor)
             (output-packet-would-overflow-tstd-p
              processor
              (output-packet-entry-packet entry))
             (and model
                  (tstd-model-transport-buffer-fullness model))
             (and model
                  (tstd-model-rx-bytes-per-second model))
             (and model
                  (tstd-model-transport-buffer-last-arrival model))
             (and model
                  (tstd-model-transport-buffer-service-end model))
             packet-index))
          (bridge-error
           "REPACKETIZE_CAPACITY_EXHAUSTED deadline_slot=~D actual_slot=~D pid=0x~4,'0X pusi=~A pcr=~A discontinuity=~A pending_packets=~D tstd_overflow=~A"
           (output-packet-entry-deadline-slot entry)
           (bridge-processor-output-slot-index processor)
           (ts-pid (output-packet-entry-packet entry))
           (ts-payload-unit-start-p
            (output-packet-entry-packet entry))
           (not
            (null
             (ts-pcr (output-packet-entry-packet entry))))
           (ts-discontinuity-indicator-p
            (output-packet-entry-packet entry))
           (bridge-processor-output-packet-count processor)
           (output-packet-would-overflow-tstd-p
            processor
            (output-packet-entry-packet entry))))))
  processor)

(defun finish-output-packet-allocation (processor)
  "EOFで未割当extra packetが残っていないことを検証する。"
  (when (bridge-processor-output-packet-head processor)
    (bridge-error
     "REPACKETIZE_CAPACITY_EXHAUSTED pending_packets=~D"
     (bridge-processor-output-packet-count processor)))
  processor)

(defun make-output-null-packet ()
  "変換で空いた固定slotを埋める188-byte null packetを作る。"
  (make-ts-packet
   +ts-null-pid+ 0
   (make-array 184
               :element-type 'octet
               :initial-element #xff)))

(defun output-packet-would-overflow-tstd-p
    (processor packet)
  "現在slotへPACKETを置くとAV1映像TBがoverflowするか返す。"
  (let ((model
          (bridge-processor-tstd-model processor)))
    (and
     model
     (tstd-output-video-packet-would-overflow-p
      model packet))))

(defun av1-stream-continuation-would-overflow-tstd-p
    (processor assembler)
  "現在slotへAV1 byte FIFO continuationを置くとTB overflowするか返す。"
  (let ((model
          (bridge-processor-tstd-model processor))
        (segment
          (pes-assembler-av1-stream-byte-head assembler)))
    (and
     model
     segment
     (tstd-video-packet-would-overflow-p
      model
      :payload-unit-start-p
      (av1-stream-byte-segment-payload-unit-start-p
       segment)))))

(defun av1-null-inside-current-pes-p (assembler entry)
  "ENTRYのnullが現在AV1 PESのPUSI以降のslotにあるか返す。"
  (let ((first-slot
          (pes-assembler-first-entry-slot-index assembler)))
    (and
     first-slot
     (<= first-slot
         (pending-entry-slot-index entry)))))

(defun allocate-output-entry (processor entry)
  "非対象/PCR slotを固定しextraはnull/reclaimed slotだけへ割り当てる。"
  (let* ((original (pending-entry-packet entry))
         (original-null-p
           (= (ts-pid original) +ts-null-pid+))
         (original-pcr
           (when
               (eql
                (ts-pid original)
                (bridge-processor-current-pcr-pid processor))
             (ts-pcr original)))
         (replacements
           (pending-entry-replacements entry))
         (replacement-provenances
           (pending-entry-replacement-provenances entry))
         (deadline (repacketize-deadline-slot processor))
         (av1-assembler
           (active-av1-stream-byte-assembler processor))
         (av1-target-entry-p
           (and
            av1-assembler
            (= (ts-pid original)
               (pes-assembler-pid av1-assembler))
            (not (pending-entry-use-original-p entry))))
         (av1-null-consumption-p
           (and
            av1-assembler
            original-null-p
            (pes-assembler-av1-stream-last-template
             av1-assembler)
            (av1-null-inside-current-pes-p
             av1-assembler entry)
            (pending-entry-use-original-p entry)
            (null
             (bridge-processor-output-packet-head processor))))
         (output nil)
         (extras '())
         (extra-provenances '()))
    ;; NILは全replacementが現在slot起点である従来経路を表す。
    (unless (or
             (null replacement-provenances)
             (= (length replacements)
                (length replacement-provenances)))
      (bridge-error
       "INTERNAL_REPLACEMENT_PROVENANCE_COUNT_MISMATCH packets=~D provenances=~D"
       (length replacements)
       (length replacement-provenances)))
    (when (and
           av1-assembler
           (not av1-target-entry-p)
           (not av1-null-consumption-p))
      (validate-av1-stream-byte-deadline
       processor av1-assembler))
    (cond
      ((pending-entry-use-original-p entry)
       (cond
         (original-null-p
          (let ((queued
                  (bridge-processor-output-packet-head
                   processor)))
            (cond
              (queued
               (cond
                 ((output-packet-would-overflow-tstd-p
                   processor
                   (output-packet-entry-packet queued))
                  ;; 現slotはnullのまま残し、映像packetは次の
                  ;; 安全なreclaimed slotまでFIFO先頭で待たせる。
                  (validate-repacketize-deadline processor)
                  (setf output original))
                 (t
                  (validate-repacketize-deadline
                   processor :consume-current-slot-p t)
                  (setf
                   output
                   (dequeue-output-packet processor)))))
              ((and
                av1-assembler
                (av1-stream-continuation-would-overflow-tstd-p
                 processor av1-assembler))
               (validate-av1-stream-byte-deadline
                processor av1-assembler)
               (setf output original))
              (av1-null-consumption-p
               (setf
                output
                (make-av1-stream-null-continuation
                 processor av1-assembler)))
              (t
               (setf output original)))))
         (t
          (validate-repacketize-deadline processor)
          (setf output original))))
      (original-pcr
       (validate-repacketize-deadline processor)
       (when (bridge-processor-output-packet-head processor)
         (bridge-error
          "REPACKETIZE_PREFIX_CANNOT_PRECEDE_FIXED_PCR"))
       (let ((pcr-position
               (position
                original-pcr replacements
                :key #'ts-pcr
                :test #'eql)))
         (unless pcr-position
           (bridge-error
            "REPACKETIZE_PCR_POSITION_CANNOT_BE_PRESERVED"))
         (when (plusp pcr-position)
           (bridge-error
            "REPACKETIZE_PREFIX_CANNOT_PRECEDE_FIXED_PCR"))
         (setf
          output (first replacements)
          extras (rest replacements)
          extra-provenances
          (rest replacement-provenances))))
      (replacements
       (cond
         ((bridge-processor-output-packet-head processor)
          (let* ((queued
                   (bridge-processor-output-packet-head
                    processor))
                 (candidate
                   (output-packet-entry-packet queued)))
            (cond
              ((output-packet-would-overflow-tstd-p
                processor candidate)
               (validate-repacketize-deadline processor)
               (setf output (make-output-null-packet)))
              (t
               (validate-repacketize-deadline
                processor :consume-current-slot-p t)
               ;; 先行extraを現在の変換対象slotへ置き、現在slotの
               ;; replacementを次の対象/null slotへ送る。
               (setf
                output
                (dequeue-output-packet processor))))
            (setf
             extras replacements
             extra-provenances replacement-provenances)))
         (t
          (let ((candidate (first replacements)))
            (if
                (and
                 (not (ts-pcr candidate))
                 (output-packet-would-overflow-tstd-p
                  processor candidate))
                ;; 映像burstの超過slotをnullへreclaimし、候補自身も
                ;; 2ms期限付きFIFOへ送る。T-STD検証値は変更しない。
                (setf
                 output (make-output-null-packet)
                 extras replacements
                 extra-provenances replacement-provenances)
                (setf
                 output candidate
                 extras (rest replacements)
                 extra-provenances
                 (rest replacement-provenances)))))))
      (t
       ;; 同一変換groupで空いたtarget slotは既存容量なので、
       ;; mux-rate未確定でも直前extraをここへ戻せる。
       (let ((queued
               (bridge-processor-output-packet-head
                processor)))
         (cond
           ((and
             queued
             (output-packet-would-overflow-tstd-p
              processor
              (output-packet-entry-packet queued)))
            (validate-repacketize-deadline processor)
            (setf output (make-output-null-packet)))
           (queued
            (validate-repacketize-deadline
             processor :consume-current-slot-p t)
           (setf
             output
             (dequeue-output-packet processor)))
           (t
            (setf output (make-output-null-packet)))))))
    (loop for packet in extras
          for index from 0
          for provenance =
            (and
             extra-provenances
             (nth index extra-provenances))
          do
      (enqueue-output-packet
       processor
       packet
       (if provenance
           (replacement-provenance-deadline-slot provenance)
           deadline)
       :origin-slot
       (and
        provenance
        (replacement-provenance-origin-slot provenance))))
    (when original-pcr
      (unless (eql (ts-pcr output) original-pcr)
        (bridge-error
         "REPACKETIZE_PCR_POSITION_CANNOT_BE_PRESERVED")))
    (emit-output-packet processor output)
    (incf (bridge-processor-output-slot-index processor))
    output))

(defun pending-head-outside-holdback-p (processor)
  "queue先頭がPMT backfill用2ms窓より古ければ真を返す。"
  (let ((head
          (bridge-processor-pending-head processor))
        (tail
          (bridge-processor-pending-tail processor))
        (holdback-count
          (repacketize-holdback-slot-count processor)))
    (when (or (null head) (zerop holdback-count))
      (return-from pending-head-outside-holdback-p t))
    (>=
     (-
      (pending-entry-slot-index tail)
      (pending-entry-slot-index head))
     holdback-count)))

(defun resolve-oldest-ready-event-lookahead (processor)
  "先行resolved entryを越え、最古unresolved eventだけを先読み解決する。"
  (loop for entry = (bridge-processor-pending-head processor)
          then (pending-entry-next entry)
        while entry
        unless (pending-entry-resolved-p entry)
          do
             ;; eventを持たないunresolved targetは意味確定待ちなので、
             ;; それを飛び越えて後続eventを時系列逆順に解決しない。
             (return
               (and
                (pending-entry-event entry)
                (resolve-head-event-if-possible
                 processor entry)))
        finally (return nil)))

(defun flush-resolved-entries (processor &key force-p)
  "PROCESSORのqueue先頭から解決済みentryを順番に出力する。

通常は初回AV1構成のPMTを先行nullへ置けるよう、明示CBRの2ms分を
未出力で保持する。FORCE-PならEOFで保持分もすべて出力する。"
  (loop while
        (resolve-oldest-ready-event-lookahead processor))
  (loop
    for entry = (bridge-processor-pending-head processor)
    while entry
    do (unless (pending-entry-resolved-p entry)
         (unless (resolve-head-event-if-possible processor entry)
           (return)))
       (when (and
              (not force-p)
              (not
               (pending-head-outside-holdback-p processor)))
         (return))
       (allocate-output-entry processor entry)
       (setf (bridge-processor-pending-head processor)
             (pending-entry-next entry))
       (decf (bridge-processor-pending-count processor))
       (when (null (bridge-processor-pending-head processor))
         (setf (bridge-processor-pending-tail processor) nil)))
  nil)

(defun invalidate-selected-program-state (processor)
  "PAT選択変更時に旧program由来のsemantic状態を失効する。"
  (let ((target-pids
          (remove-duplicates
           (append
            (when (bridge-processor-current-video-pid processor)
              (list
               (bridge-processor-current-video-pid processor)))
            (bridge-processor-current-opus-pids processor))
           :test #'=)))
    (dolist (pid target-pids)
      (let ((assembler
              (gethash
               pid
               (bridge-processor-pes-assemblers processor))))
        (when (and assembler
                   (pes-assembler-active-p assembler))
          (bridge-error
           "PAT changes program selection during active PES on PID 0x~4,'0X"
           pid))))
    (dolist (pid target-pids)
      (remhash pid
               (bridge-processor-pes-assemblers processor))
      (remhash pid
               (bridge-processor-output-continuity-counters
                processor))))
  (setf
   (bridge-processor-current-pcr-pid processor) nil
   (bridge-processor-current-video-pid processor) nil
   (bridge-processor-current-opus-pids processor) '()
   (bridge-processor-latest-pmt-table processor) nil
   (bridge-processor-latest-pmt-template processor) nil
   (bridge-processor-seen-pmt-p processor) nil
   (bridge-processor-seen-video-pes-p processor) nil
   (bridge-processor-last-av1-configuration processor) nil
   (bridge-processor-last-av1-reduced-header-p processor) nil
   (bridge-processor-last-av1-timestamp processor) nil
   (bridge-processor-last-av1-unwrapped-timestamp processor) nil
   (bridge-processor-last-av1-signaling-tuple processor) nil
   (bridge-processor-last-av1-signaling-change-pts processor) nil
   (bridge-processor-av1-cll-present-p processor) nil
   (bridge-processor-av1-rap-without-cll-seen-p processor) nil
   (bridge-processor-av1-tu-sequence-header-seen-p processor) nil
   (bridge-processor-first-av1-ordering-time processor) nil
   (bridge-processor-last-av1-rap-ordering-time processor) nil)
  (reset-video-validation-state processor)
  (reset-selected-pcr-state processor)
  processor)

(defun parse-current-pat (processor section)
  "PAT SECTIONから単一programのPMT PIDを更新する。"
  (let* ((table (parse-pat-section section))
         (programs
           (remove 0
                   (program-association-table-programs table)
                   :key #'pat-program-program-number
                   :test #'=)))
    (unless (and (program-association-table-current-next-p table)
                 (zerop
                  (program-association-table-section-number table))
                 (zerop
                  (program-association-table-last-section-number table)))
      (bridge-error "PAT must be a current single section"))
    (validate-input-pat-transition processor table)
    (let* ((requested
             (bridge-processor-requested-program-number processor))
           (program
             (cond
               (requested
                (or (find requested programs
                          :key #'pat-program-program-number
                          :test #'=)
                    (bridge-error
                     "Requested program is absent from PAT: ~D"
                     requested)))
               (t
                (unless (= (length programs) 1)
                  (bridge-error
                   "PAT has ~D programs; --program-number is required"
                   (length programs)))
                (first programs))))
           (new-pmt-pid (pat-program-pid program))
           (new-program-number
             (pat-program-program-number program))
           (selection-change-p
             (or
              (not
               (eql new-pmt-pid
                    (bridge-processor-pmt-pid processor)))
              (not
               (eql new-program-number
                    (bridge-processor-pat-program-number processor))))))
      (when selection-change-p
        (when (bridge-processor-current-pmt-entries processor)
          (bridge-error
           "PAT changes program selection during an incomplete PMT"))
        (invalidate-selected-program-state processor)
        (setf
              (bridge-processor-last-input-pmt-signature processor)
              nil
              (bridge-processor-last-input-pmt-version processor)
              nil
              (bridge-processor-last-source-pmt-signature processor)
              nil
              (bridge-processor-last-source-pmt-version processor)
              nil
              (bridge-processor-last-output-pmt-signature processor)
              nil
              (bridge-processor-last-output-pmt-version processor)
              nil
              (bridge-processor-advertised-av1-configuration processor)
              nil))
      (unless (eql new-pmt-pid
                   (bridge-processor-pmt-pid processor))
        (setf (bridge-processor-pmt-pid processor) new-pmt-pid
              (bridge-processor-pmt-assembler processor)
              (make-section-assembler new-pmt-pid)))
      (setf (bridge-processor-pat-program-number processor)
            new-program-number))))

(defun process-pat-packet (processor packet)
  "PACKETをPAT assemblerへ与える。"
  (let ((assembler
          (bridge-processor-pat-assembler processor)))
    ;; 完全duplicateはdiscontinuity_indicator付きでも状態を変えない。
    (when (exact-section-packet-duplicate-p assembler packet)
      (return-from process-pat-packet nil))
    (when (ts-discontinuity-indicator-p packet)
      (when (incomplete-section-buffer-p assembler)
        (bridge-error
         "PAT discontinuity interrupts an incomplete section"))
      (setf (bridge-processor-last-input-pat-signature processor) nil
            (bridge-processor-last-input-pat-version processor) nil))
    (dolist (section
             (feed-section-packet assembler packet))
      (parse-current-pat processor section))))

(defun validate-current-pmt-table (processor table)
  "TABLEが選択PATの単一current PMTであることを検証する。"
  (unless (and (program-map-table-current-next-p table)
               (zerop (program-map-table-section-number table))
               (zerop (program-map-table-last-section-number table)))
    (bridge-error "PMT must be a current single section"))
  (unless (= (program-map-table-program-number table)
             (bridge-processor-pat-program-number processor))
    (bridge-error
     "PMT program number does not match PAT: expected=~D actual=~D"
     (bridge-processor-pat-program-number processor)
     (program-map-table-program-number table))))

(defun install-pmt-targets (processor table)
  "TABLEから現在のvideo/Opus PID集合を確定する。"
  (let ((old-video-pid
           (bridge-processor-current-video-pid processor))
         (old-opus-pids
           (bridge-processor-current-opus-pids processor))
        (video-pid
          (unless
              (eq (bridge-processor-video-codec processor)
                  :passthrough)
            (select-video-pid
             table
             (bridge-processor-video-codec processor))))
        (opus-pids
          (if (eq (bridge-processor-audio-codec processor) :opus)
              (select-opus-pids table)
              '())))
    (dolist (retired-pid
             (remove-duplicates
              (append
               (when (and old-video-pid
                          (not (eql old-video-pid video-pid)))
                 (list old-video-pid))
               (set-difference old-opus-pids opus-pids :test #'=))
              :test #'=))
      (let ((assembler
              (gethash retired-pid
                       (bridge-processor-pes-assemblers processor))))
        (when (and assembler
                   (pes-assembler-active-p assembler))
          (bridge-error
           "PMT retires target PID 0x~4,'0X during an active PES"
           retired-pid))
        (remhash retired-pid
                 (bridge-processor-pes-assemblers processor))))
    (when (not (eql old-video-pid video-pid))
      (reset-video-validation-state processor)
      (setf (bridge-processor-last-av1-configuration processor) nil
            (bridge-processor-last-av1-reduced-header-p processor) nil
            (bridge-processor-last-av1-timestamp processor) nil
            (bridge-processor-last-av1-unwrapped-timestamp processor) nil
            (bridge-processor-last-av1-signaling-tuple processor) nil
            (bridge-processor-last-av1-signaling-change-pts processor)
            nil
            (bridge-processor-av1-cll-present-p processor) nil
            (bridge-processor-av1-rap-without-cll-seen-p processor)
            nil
            (bridge-processor-av1-tu-sequence-header-seen-p
             processor)
            nil
            (bridge-processor-first-av1-ordering-time processor)
            nil
            (bridge-processor-last-av1-rap-ordering-time processor)
            nil))
    (setf (bridge-processor-current-video-pid processor) video-pid
          (bridge-processor-current-opus-pids processor) opus-pids)
    (values video-pid opus-pids)))

(defun finish-pmt-group (processor section)
  "完成PMT SECTIONをevent化し対象PIDを更新する。"
  (let ((table (parse-pmt-section section)))
    (validate-current-pmt-table processor table)
    (validate-input-pmt-transition processor table)
    (install-selected-pcr-pid
     processor (program-map-table-pcr-pid table))
    (multiple-value-bind (video-pid opus-pids)
        (install-pmt-targets processor table)
      (let* ((entries
               (nreverse
                (bridge-processor-current-pmt-entries processor)))
             (first-entry (first entries))
             (event
               (make-pmt-event
                :entries entries
                :table (copy-program-map-table-deep table)
                :source-section section
                :pmt-pid (bridge-processor-pmt-pid processor)
                :video-pid video-pid
                :opus-pids opus-pids)))
        (unless first-entry
          (bridge-error "Completed PMT has no transport packet"))
        (setf (pending-entry-event first-entry) event
              (bridge-processor-current-pmt-entries processor) '()
              (bridge-processor-latest-pmt-table processor)
              (copy-program-map-table-deep table)
              (bridge-processor-latest-pmt-template processor)
              (copy-seq (pending-entry-packet first-entry))
              (bridge-processor-seen-pmt-p processor) t)))))

(defun process-pmt-packet (processor entry packet)
  "PACKETをPMT assemblerへ与え、完成時にevent化する。"
  (let ((assembler
          (bridge-processor-pmt-assembler processor)))
    (when (exact-section-packet-duplicate-p assembler packet)
      (resolve-entry entry '())
      (return-from process-pmt-packet nil)))
  (when (and (ts-discontinuity-indicator-p packet)
             (bridge-processor-current-pmt-entries processor))
    (bridge-error
     "PMT discontinuity interrupts an incomplete section"))
  (when (ts-discontinuity-indicator-p packet)
    (setf (bridge-processor-last-input-pmt-signature processor) nil
          (bridge-processor-last-input-pmt-version processor) nil))
  (unless (ts-has-payload-p packet)
    (when
        (feed-section-packet
         (bridge-processor-pmt-assembler processor)
         packet)
      (bridge-error
       "Adaptation-only PMT packet unexpectedly completes a section"))
    (return-from process-pmt-packet nil))
  (when (ts-payload-unit-start-p packet)
    (let ((payload-offset (ts-payload-offset packet)))
      (unless (and payload-offset
                   (zerop (aref packet payload-offset)))
        (bridge-error "PMT pointer field must be zero"))))
  (let* ((assembler
           (bridge-processor-pmt-assembler processor))
         (sections
          (feed-section-packet
           assembler
           packet)))
    (mark-entry-unresolved entry)
    (push entry (bridge-processor-current-pmt-entries processor))
    (when (> (length sections) 1)
      (bridge-error
       "Multiple PMT sections in one transport packet group are unsupported"))
    (when sections
      (finish-pmt-group processor (first sections)))
    (when (and (null sections)
               (ts-payload-unit-start-p packet)
               (not
                (incomplete-section-buffer-p
                 (bridge-processor-pmt-assembler processor))))
      (dolist (duplicate-entry
               (bridge-processor-current-pmt-entries processor))
        (resolve-entry duplicate-entry '()))
      (setf (bridge-processor-current-pmt-entries processor) '()))))

(defun ensure-pes-assembler (processor pid kind)
  "PID/KINDに対応するPES assemblerを返す。"
  (let ((existing
          (gethash pid
                   (bridge-processor-pes-assemblers processor))))
    (when (and existing
               (not (eq (pes-assembler-kind existing) kind)))
      (bridge-error "PID 0x~4,'0X changes semantic stream kind" pid))
    (or existing
        (setf (gethash pid
                       (bridge-processor-pes-assemblers processor))
              (%make-pes-assembler pid kind)))))

(defun current-pes-assembler-for-packet (processor packet)
  "PACKETが現在または継続中の対象PESならassemblerを返す。"
  (let* ((pid (ts-pid packet))
         (existing
           (gethash pid
                    (bridge-processor-pes-assemblers processor))))
    (cond
      ((and existing (pes-assembler-active-p existing))
       existing)
      ((eql pid (bridge-processor-current-video-pid processor))
       (ensure-pes-assembler processor pid :video))
      ((member pid
               (bridge-processor-current-opus-pids processor)
               :test #'=)
       (ensure-pes-assembler processor pid :opus))
      (t nil))))

(defun clear-pes-event-state (assembler)
  "ASSEMBLERの完了済みPES eventとAV1残留を初期化する。"
  (setf (pes-assembler-active-p assembler) nil
        (pes-assembler-entries assembler) '()
        (pes-assembler-first-entry-slot-index assembler) nil
        (fill-pointer (pes-assembler-buffer assembler)) 0
        (pes-assembler-expected-length assembler) nil
        (pes-assembler-streaming-p assembler) nil
        (pes-assembler-stream-configuration assembler) nil
        (pes-assembler-stream-random-access-kind assembler) nil
        (pes-assembler-stream-frame-semantics assembler) nil
        (pes-assembler-stream-ordering-time assembler) nil
        (pes-assembler-stream-temporal-delimiter-seen-p assembler) nil
        (pes-assembler-stream-sequence-header-in-access-unit-p
         assembler)
        nil
        (pes-assembler-av1-stream-transformer assembler) nil
        (pes-assembler-source-pmt-table assembler) nil
        (pes-assembler-source-pmt-pid assembler) nil
        (pes-assembler-source-pmt-template assembler) nil
        (pes-assembler-source-opus-pids assembler) '()
        (pes-assembler-source-av1-configuration assembler) nil
        (pes-assembler-source-av1-reduced-header-p assembler) nil
        (pes-assembler-source-av1-tu-sequence-header-seen-p
         assembler)
        nil
        (pes-assembler-program-timestamp-recorded-p assembler) nil)
  (setf
   (pes-assembler-av1-stream-byte-head assembler) nil
   (pes-assembler-av1-stream-byte-tail assembler) nil
   (pes-assembler-av1-stream-byte-count assembler) 0
   (pes-assembler-av1-stream-target-head assembler) nil
   (pes-assembler-av1-stream-target-tail assembler) nil
   (pes-assembler-av1-stream-target-count assembler) 0
   (pes-assembler-av1-stream-output-start-p assembler) nil
   (pes-assembler-av1-stream-last-template assembler) nil)
  assembler)

(defun start-pes-event (processor assembler)
  "ASSEMBLERへ新しいPESの時点情報を保存する。"
  (when (pes-assembler-pending-pts-discontinuity-p assembler)
    (setf (pes-assembler-skip-next-pts-check-p assembler) t
          (pes-assembler-skip-next-program-timestamp-check-p
           assembler)
          t
          (pes-assembler-pending-pts-discontinuity-p assembler) nil))
  (setf (pes-assembler-active-p assembler) t
        (pes-assembler-first-entry-slot-index assembler) nil
        (pes-assembler-source-pmt-table assembler)
        (and (bridge-processor-latest-pmt-table processor)
             (copy-program-map-table-deep
              (bridge-processor-latest-pmt-table processor)))
        (pes-assembler-source-pmt-pid assembler)
        (bridge-processor-pmt-pid processor)
        (pes-assembler-source-pmt-template assembler)
        (and (bridge-processor-latest-pmt-template processor)
             (copy-seq
              (bridge-processor-latest-pmt-template processor)))
        (pes-assembler-source-opus-pids assembler)
        (copy-list
         (bridge-processor-current-opus-pids processor))
        (pes-assembler-source-av1-configuration assembler)
        (bridge-processor-last-av1-configuration processor)
        (pes-assembler-source-av1-reduced-header-p assembler)
        (bridge-processor-last-av1-reduced-header-p processor)
        (pes-assembler-source-av1-tu-sequence-header-seen-p
         assembler)
        (bridge-processor-av1-tu-sequence-header-seen-p
         processor))
  assembler)

(defun append-pes-octets (assembler packet)
  "PACKETのpayloadをASSEMBLERへ上限付きで追加する。"
  (let ((payload-offset (ts-payload-offset packet)))
    (when payload-offset
      (loop for position from payload-offset below +ts-packet-size+
            do (vector-push-extend
                (aref packet position)
                (pes-assembler-buffer assembler))
               (when (> (length (pes-assembler-buffer assembler))
                        +maximum-pes-byte-count+)
                 (bridge-error
                  "PES exceeds semantic buffer limit: ~D bytes"
                  +maximum-pes-byte-count+)))))
  (when (and (null (pes-assembler-expected-length assembler))
             (>= (length (pes-assembler-buffer assembler)) 6))
    (let ((declared
            (read-u16-be
             (pes-assembler-buffer assembler) 4)))
      (when (plusp declared)
        (setf (pes-assembler-expected-length assembler)
              (+ declared 6)))))
  assembler)

(defun parse-available-pes-header (assembler)
  "ASSEMBLERに完全headerがあれば宣言payload長を無視してparseする。"
  (let ((buffer (pes-assembler-buffer assembler)))
    (unless (>= (length buffer) 9)
      (return-from parse-available-pes-header nil))
    (let ((header-end (+ 9 (aref buffer 8))))
      (unless (>= (length buffer) header-end)
        (return-from parse-available-pes-header nil))
      (let ((header-prefix
              (subseq buffer 0 header-end)))
        ;; partial PESでもheader自体を共通parserで厳格検査する。
        (setf (aref header-prefix 4) 0
              (aref header-prefix 5) 0)
        (parse-pes-header header-prefix)))))

(defun program-ordering-assembler-p (processor assembler)
  "ASSEMBLERをprogramの時刻基準に使うか返す。"
  (declare (ignore processor))
  (member
   (pes-assembler-kind assembler)
   '(:video :opus)
   :test #'eq))

(defun maybe-record-program-timestamp (processor assembler)
  "完全PES headerを一度だけPCR監視へ記録する。"
  (when (or
         (pes-assembler-program-timestamp-recorded-p assembler)
         (not (program-ordering-assembler-p processor assembler)))
    (return-from maybe-record-program-timestamp nil))
  (let ((header (parse-available-pes-header assembler)))
    (unless header
      (return-from maybe-record-program-timestamp nil))
    (let ((timestamp
            (or (pes-header-dts header)
                (pes-header-pts header))))
      (unless timestamp
        (bridge-error
         "PROGRAM_TIMESTAMP_MISSING pid=0x~4,'0X"
         (pes-assembler-pid assembler)))
      (when
          (pes-assembler-skip-next-program-timestamp-check-p
           assembler)
        (reset-pes-program-timestamp-state assembler)
        (setf
         (pes-assembler-skip-next-program-timestamp-check-p
          assembler)
         nil))
      (validate-program-timestamp-against-pcr
       processor assembler timestamp
       (not (null (pes-header-dts header))))
      (setf
       (pes-assembler-program-timestamp-recorded-p assembler) t)
      timestamp)))

(defun trim-complete-pes-buffer (assembler)
  "宣言PES長を超えたTS stuffingを検証してbufferを切り詰める。"
  (let ((expected (pes-assembler-expected-length assembler))
        (buffer (pes-assembler-buffer assembler)))
    (when (and expected
               (> (length buffer) expected))
      (loop for position from expected below (length buffer)
            unless (= (aref buffer position) #xff)
              do (bridge-error
                  "Non-stuffing data follows declared PES length at offset ~D"
                  position))
      (setf (fill-pointer buffer) expected)))
  assembler)

(defun duplicate-pes-packet-p
    (assembler packet &key allow-counter-rebase-p)
  "PACKETが直前payload packetの完全duplicateかを検証して返す。"
  (let ((last-counter
          (pes-assembler-last-continuity-counter assembler))
        (last-packet
          (pes-assembler-last-payload-packet assembler))
        (counter (ts-continuity-counter packet)))
    (when (and last-counter (= counter last-counter))
      (cond
        ((and last-packet (equalp last-packet packet))
         (return-from duplicate-pes-packet-p t))
        ((not allow-counter-rebase-p)
         (bridge-error
          "Conflicting duplicate PES packet on PID 0x~4,'0X"
          (pes-assembler-pid assembler)))))
    nil))

(defun validate-pes-transport-continuity (assembler packet)
  "対象PES PIDのpayload continuityを検証しduplicateならNILを返す。"
  (unless (ts-has-payload-p packet)
    (when (ts-discontinuity-indicator-p packet)
      (setf (pes-assembler-skip-next-continuity-check-p assembler) t
            (pes-assembler-pending-pts-discontinuity-p assembler) t))
    (return-from validate-pes-transport-continuity t))
  (let* ((discontinuity-p
           (ts-discontinuity-indicator-p packet))
         (skip-check-p
           (or discontinuity-p
               (pes-assembler-skip-next-continuity-check-p
                assembler))))
    (when (duplicate-pes-packet-p
           assembler packet
           :allow-counter-rebase-p skip-check-p)
      (return-from validate-pes-transport-continuity nil))
    (let ((last-counter
          (pes-assembler-last-continuity-counter assembler))
          (counter (ts-continuity-counter packet)))
      (when (and last-counter
                 (not skip-check-p)
                 (/= counter
                     (logand (+ last-counter 1) #x0f)))
        (bridge-error
         "PES continuity error on PID 0x~4,'0X: expected=~D actual=~D"
         (pes-assembler-pid assembler)
         (logand (+ last-counter 1) #x0f)
         counter))
      (when discontinuity-p
        (setf
         (pes-assembler-pending-pts-discontinuity-p assembler)
         t))
      (setf (pes-assembler-last-continuity-counter assembler) counter
            (pes-assembler-last-payload-packet assembler)
            (copy-seq packet)
            (pes-assembler-skip-next-continuity-check-p assembler)
            nil)
      t)))

(defun validate-target-transport-packet (assembler packet)
  "対象PES PACKETのerror/scrambling/PIDを検証する。"
  (when (ts-transport-error-p packet)
    (bridge-error "Transport error indicator is set on PES PID 0x~4,'0X"
                  (pes-assembler-pid assembler)))
  (unless (zerop (ts-scrambling-control packet))
    (bridge-error "Scrambled PES is unsupported on PID 0x~4,'0X"
                  (pes-assembler-pid assembler))))

(defun vp9-streaming-prefix-ready-p (assembler)
  "ASSEMBLERが安全に逐次VP9出力を始められるheader量を持つか返す。"
  (let ((buffer (pes-assembler-buffer assembler)))
    (and (>= (length buffer) 9)
         (zerop (read-u16-be buffer 4))
         (>= (length buffer)
             (+ 9 (aref buffer 8) 32)))))

(defun streaming-output-order-ready-p (processor assembler)
  "ASSEMBLERより前のpending entryがすべて解決済みか返す。

先行PES eventが未解決の間に後続PESを逐次開始すると、後続側が
出力continuity counterを先に予約し、最終出力順とcounter順が入れ替わる。"
  (let ((first-entry
          (car (last (pes-assembler-entries assembler)))))
    (unless first-entry
      (bridge-error "Streaming PES has no transport packets"))
    (loop for entry = (bridge-processor-pending-head processor)
            then (pending-entry-next entry)
          while entry
          do
             (when (eq entry first-entry)
               (return-from streaming-output-order-ready-p t))
             (unless (pending-entry-resolved-p entry)
               (return-from streaming-output-order-ready-p nil)))
    (bridge-error
     "Streaming PES first entry is absent from pending output queue")))

(defun try-start-vp9-streaming (processor assembler)
  "長さ0 VP9 PESを次PUSI前に検証・識別子変換して逐次出力する。"
  (unless (and (eq (pes-assembler-kind assembler) :video)
               (eq (bridge-processor-video-codec processor) :vp9)
               (not (pes-assembler-streaming-p assembler))
               (vp9-streaming-prefix-ready-p assembler))
    (return-from try-start-vp9-streaming nil))
  ;; 出力CCの予約順をtransport slot順と一致させる。
  (loop while
        (resolve-oldest-ready-event-lookahead processor))
  (unless (streaming-output-order-ready-p
           processor assembler)
    (return-from try-start-vp9-streaming nil))
  (let* ((source-pes
           (coerce (pes-assembler-buffer assembler)
                   '(simple-array (unsigned-byte 8) (*))))
         (header (parse-pes-header source-pes))
         (payload
           (subseq source-pes
                   (pes-header-payload-offset header)))
         (configuration
           (parse-vp9-frame-prefix payload))
         (output-pes (copy-seq source-pes))
         (flags
           (if (vp9-frame-configuration-key-frame-p configuration)
               #x40
               0)))
    (unless (pes-header-pts header)
      (bridge-error "Video PES does not carry a PTS"))
    (setf (aref output-pes 3) #xe0
          (aref output-pes 6)
          (logior (aref output-pes 6) #x04))
    (resolve-video-packet-entries
     processor
     (reverse (pes-assembler-entries assembler))
     output-pes
     flags)
    (setf (pes-assembler-streaming-p assembler) t
          (bridge-processor-seen-video-pes-p processor) t)
    t))

(defun make-streaming-av1-video-event
    (assembler entries configuration random-access-kind)
  "AV1逐次開始時のPMT生成に必要なVIDEO-EVENTを作る。"
  (make-video-event
   :entries entries
   :pes (make-array 0 :element-type 'octet)
   :codec :av1
   :configuration configuration
   :random-access-p (not (null random-access-kind))
   :source-pmt-table
   (pes-assembler-source-pmt-table assembler)
   :pmt-pid
   (pes-assembler-source-pmt-pid assembler)
   :pmt-template
   (pes-assembler-source-pmt-template assembler)
   :video-pid (pes-assembler-pid assembler)
   :opus-pids
   (pes-assembler-source-opus-pids assembler)))

(defun resolve-av1-stream-byte-target-entry
    (processor assembler entry packet)
  "AV1 byte FIFOを現在のvideo target templateへ最大1 packetで詰める。"
  (when (ts-has-payload-p packet)
    (setf
     (pes-assembler-av1-stream-last-template assembler)
     packet))
  (let* ((head-segment
           (pes-assembler-av1-stream-byte-head assembler))
         (start-p
           (and
            head-segment
            (av1-stream-byte-segment-payload-unit-start-p
             head-segment)))
         (flags
           (if head-segment
               (av1-stream-byte-segment-adaptation-flags
                head-segment)
               0))
         (capacity
           (if (ts-has-payload-p packet)
               (template-payload-capacity packet flags)
               0))
         (fallback
           (ts-continuity-counter packet))
         (counter
           (output-continuity-counter
            processor
            (pes-assembler-pid assembler)
            fallback)))
    (when (ts-discontinuity-indicator-p packet)
      (setf counter fallback))
    (when (plusp
           (pes-assembler-av1-stream-byte-count assembler))
      (validate-av1-stream-byte-deadline
       processor assembler
       :consume-current-slot-p (plusp capacity)
       :actual-slot (pending-entry-slot-index entry)))
    (let* ((segment
             (pes-assembler-av1-stream-byte-head assembler))
           (provenance
             (and
              segment
              (make-replacement-provenance
               :origin-slot
               (av1-stream-byte-segment-origin-slot segment)
               :deadline-slot
               (av1-stream-byte-segment-deadline-slot segment))))
           (payload
             (drain-av1-stream-bytes assembler capacity)))
      (cond
        ((plusp (length payload))
         (resolve-entry
          entry
          (list
           (make-payload-packet-from-template
            packet payload counter
            :payload-unit-start start-p
            :adaptation-flags flags
            :clear-adaptation-flags #x60)))
         (setf
          (pending-entry-replacement-provenances entry)
          (list provenance))
         (record-output-payload-count
          processor
          (pes-assembler-pid assembler)
          counter 1))
        (t
         (let ((adaptation-only
                 (make-adaptation-only-from-template
                  packet
                  (adaptation-output-counter counter fallback)
                   :clear-adaptation-flags #x60)))
            (resolve-entry
             entry
             (if adaptation-only
                 (list adaptation-only)
                 '())))))))
  entry)

(defun resolve-av1-stream-byte-null-target-entry
    (processor assembler entry)
  "保持中null targetへAV1 byteを最大1 packet分だけ時系列配置する。"
  (let* ((segment
           (pes-assembler-av1-stream-byte-head assembler))
         (start-p
           (and
            segment
            (av1-stream-byte-segment-payload-unit-start-p
             segment)))
         (flags
           (if segment
               (av1-stream-byte-segment-adaptation-flags segment)
               0))
         (capacity
           (if (plusp flags) 182 184)))
    (if (and
         segment
         (plusp
          (pes-assembler-av1-stream-byte-count assembler)))
        (let* ((template
                 (or
                  (pes-assembler-av1-stream-last-template assembler)
                  (bridge-error
                   "AV1 streaming null target has no video template")))
               (pid
                 (pes-assembler-pid assembler))
               (fallback
                 (ts-continuity-counter template))
               (counter
                 (output-continuity-counter
                  processor pid fallback))
               (provenance
                 (make-replacement-provenance
                  :origin-slot
                  (av1-stream-byte-segment-origin-slot segment)
                  :deadline-slot
                  (av1-stream-byte-segment-deadline-slot segment))))
          (validate-av1-stream-byte-deadline
           processor assembler
           :consume-current-slot-p t
           :actual-slot
           (pending-entry-slot-index entry))
          (let ((payload
                  (drain-av1-stream-bytes assembler capacity)))
            (resolve-entry
             entry
             (list
              (make-ts-packet
               pid counter payload
               :payload-unit-start start-p
               :random-access (logbitp 6 flags)
               :elementary-stream-priority
               (logbitp 5 flags)
               :transport-priority
               (ts-transport-priority-p template))))
            (setf
             (pending-entry-replacement-provenances entry)
             (list provenance))
            (record-output-payload-count
             processor pid counter 1)))
        (resolve-entry-as-original entry)))
  entry)

(defun av1-stream-target-payload-capacity (assembler target)
  "現在FIFO先頭flagを保持したTARGETのpayload容量を返す。"
  (let ((packet
          (pending-entry-packet
           (av1-stream-target-entry-entry target)))
        (segment
          (pes-assembler-av1-stream-byte-head assembler)))
    (ecase (av1-stream-target-entry-kind target)
      (:video
       (if (ts-has-payload-p packet)
           (template-payload-capacity
            packet
            (if segment
                (av1-stream-byte-segment-adaptation-flags segment)
                0))
           0))
      (:null
       (if (and
            segment
            (plusp
             (av1-stream-byte-segment-adaptation-flags
              segment)))
           182
           184)))))

(defun resolve-ready-av1-stream-target-entries
    (processor assembler &key force-p current-slot)
  "2ms未出力窓内でAV1 byteをtarget templateへ時系列順に再充填する。

容量を満たすまでtargetを未解決で保持し、後続source chunkのbyteで
先行targetのstuffingを減らす。2ms窓を越える時とPUSI/EOFでは部分量でも
確定し、既にallocatorへ渡したslotは変更しない。"
  (let ((resolved-count 0)
        (resolved-slot
          (or
           current-slot
           (let ((tail
                   (bridge-processor-pending-tail processor)))
             (and tail
                  (pending-entry-slot-index tail)))
           (bridge-processor-output-slot-index processor)))
        (holdback-count
          (repacketize-holdback-slot-count processor)))
    (loop
      for target =
        (pes-assembler-av1-stream-target-head assembler)
      while target
      for entry = (av1-stream-target-entry-entry target)
      for capacity =
        (av1-stream-target-payload-capacity assembler target)
      for available =
        (pes-assembler-av1-stream-byte-count assembler)
      for expired-p =
        (>=
         (- resolved-slot
            (pending-entry-slot-index entry))
         holdback-count)
      do
        (unless (or
                 force-p
                 (zerop capacity)
                 (>= available capacity)
                 expired-p)
          (return))
        (ecase (av1-stream-target-entry-kind target)
          (:video
           (resolve-av1-stream-byte-target-entry
            processor assembler entry
            (pending-entry-packet entry)))
          (:null
           (resolve-av1-stream-byte-null-target-entry
            processor assembler entry)))
        (dequeue-av1-stream-target-entry assembler)
        (incf resolved-count))
    resolved-count))

(defun rewritten-av1-pes-header-slice
    (packet start end pes-offset)
  "PACKETのPES header断片をAV1 mapping識別子へ書き換えて返す。"
  (let ((slice (subseq packet start end)))
    (loop for local-offset from 0 below (length slice)
          for pes-position from pes-offset
          do
      (case pes-position
        (3
         (setf (aref slice local-offset) #xbd))
        (6
         (setf
          (aref slice local-offset)
          (logior (aref slice local-offset) #x04)))
        (otherwise nil)))
    slice))

(defun transform-initial-av1-entry
    (processor assembler entry transformer
     pes-offset payload-offset)
  "逐次開始前に蓄積したENTRYをsource slot境界のまま変換する。"
  (let* ((packet (pending-entry-packet entry))
         (packet-payload-offset (ts-payload-offset packet))
         (source-count
           (if packet-payload-offset
               (- +ts-packet-size+ packet-payload-offset)
               0))
         (next-pes-offset (+ pes-offset source-count))
         (header-count
           (if packet-payload-offset
               (max
                0
                (min source-count
                     (- payload-offset pes-offset)))
               0))
         (header
           (if (plusp header-count)
               (rewritten-av1-pes-header-slice
                packet
                packet-payload-offset
                (+ packet-payload-offset header-count)
                pes-offset)
               #()))
         (es-start
           (and
            packet-payload-offset
            (+ packet-payload-offset header-count)))
         (converted
           (if (and es-start
                    (< es-start +ts-packet-size+))
               (transform-av1-stream-chunk
                transformer packet
                :start es-start
                :end +ts-packet-size+)
               #()))
         (output
           (concatenate-octet-vectors header converted)))
    (append-av1-stream-byte-segment
     processor assembler output
     (pending-entry-slot-index entry))
    next-pes-offset))

(defun enqueue-initial-av1-stream-target-entries
    (assembler entries)
  "初回AV1範囲のvideo templateと介在nullをslot順queueへ追加する。"
  (let ((first-entry (first entries))
        (last-entry (car (last entries))))
    (loop for cursor = first-entry then (pending-entry-next cursor)
          while cursor
          do
      (cond
        ((and
          (member cursor entries :test #'eq)
          (pending-entry-av1-output-target-consumed-p cursor))
         ;; 出力slotは旧PESが使用済みでも、後続nullを新PESのtargetへ
         ;; 変換するためのPID・priority・CC templateには利用できる。
         (setf
          (pes-assembler-av1-stream-last-template assembler)
          (pending-entry-packet cursor)))
        ((and
          (member cursor entries :test #'eq)
          (not
           (pending-entry-av1-output-target-consumed-p cursor)))
         (enqueue-av1-stream-target-entry
          assembler cursor :kind :video))
        ((and
          (pending-entry-resolved-p cursor)
          (pending-entry-use-original-p cursor)
          (= (ts-pid (pending-entry-packet cursor))
             +ts-null-pid+))
         (mark-entry-unresolved cursor)
         (enqueue-av1-stream-target-entry
          assembler cursor :kind :null)))
      (when (eq cursor last-entry)
        (return))))
  assembler)

(defun queued-av1-stream-target-capacity (assembler)
  "現在queue済みのAV1 targetが収容できる合計byte数を返す。"
  (loop for target =
          (pes-assembler-av1-stream-target-head assembler)
            then (av1-stream-target-entry-next target)
        while target
        sum (av1-stream-target-payload-capacity assembler target)))

(defun maybe-borrow-next-av1-pusi-target
    (processor assembler entry packet)
  "旧AV1 PESの末尾不足時だけ次PUSIのslotを継続packetとして借りる。

PUSIのsource payloadは次PESとして解析し、実際のPUSI出力は後続の
video/null targetへ移す。選択PCRを含むadaptation fieldは元slotに残す。"
  (unless (and
           (eq (bridge-processor-video-codec processor) :av1)
           (pes-assembler-streaming-p assembler)
           (ts-payload-unit-start-p packet)
           (not (ts-discontinuity-indicator-p packet))
           (> (pes-assembler-av1-stream-byte-count assembler)
              (queued-av1-stream-target-capacity assembler)))
    (return-from maybe-borrow-next-av1-pusi-target nil))
  (mark-entry-unresolved entry)
  (setf (pending-entry-av1-output-target-consumed-p entry) t)
  (enqueue-av1-stream-target-entry assembler entry :kind :video)
  t)

(defun resolve-initial-av1-stream-entries
    (processor assembler entries transformer payload-offset
     synthetic-pmt)
  "初回prefixを含む蓄積ENTRIESをsource slot順で解決し出力する。"
  (let ((pes-offset 0)
        (video-start-entry
          (or
           (find-if
            (lambda (entry)
              (ts-payload-unit-start-p
               (pending-entry-packet entry)))
            entries)
           (bridge-error
            "Streaming AV1 entries have no PUSI packet"))))
    (when synthetic-pmt
      (backfill-synthetic-pmt-before-video
       processor synthetic-pmt video-start-entry))
    (dolist (entry entries)
      (setf pes-offset
            (transform-initial-av1-entry
             processor assembler entry transformer
             pes-offset payload-offset)))
    (enqueue-initial-av1-stream-target-entries
     assembler entries)
    ;; prefix判明までに保持した全source chunkを先に変換し、未出力target
    ;; templateへslot順で詰める。これによりentry境界ごとのstuffingを
    ;; 後続byteで再利用できる。
    (resolve-ready-av1-stream-target-entries
     processor assembler)
    (flush-resolved-entries processor)
    entries))

(defun try-start-av1-streaming (processor assembler)
  "長さ0 AV1 PESをOBU状態機械で次PUSI前に逐次出力する。"
  (unless (and (eq (pes-assembler-kind assembler) :video)
               (eq (bridge-processor-video-codec processor) :av1)
               (not (pes-assembler-streaming-p assembler)))
    (return-from try-start-av1-streaming nil))
  (let ((buffer (pes-assembler-buffer assembler)))
    (unless (and (>= (length buffer) 9)
                 (zerop (read-u16-be buffer 4))
                 (>= (length buffer)
                     (+ 9 (aref buffer 8))))
      (return-from try-start-av1-streaming nil))
    (let* ((source-pes
             (coerce buffer
                     '(simple-array (unsigned-byte 8) (*))))
           (header (parse-pes-header source-pes))
           (payload-offset (pes-header-payload-offset header))
           (payload (subseq source-pes payload-offset))
           (ordering-time nil)
           (ordered-source-p nil))
      (unless (pes-header-pts header)
        (bridge-error "Video PES does not carry a PTS"))
        (multiple-value-bind
            (configuration reduced-header-p
             random-access-kind frame-header-found-p
             frame-semantics hdr-cll-p
             temporal-delimiter-seen-p
             sequence-header-in-access-unit-p)
          (parse-av1-stream-prefix-metadata
           payload
           (pes-assembler-source-av1-configuration assembler)
           (pes-assembler-source-av1-reduced-header-p assembler)
           (pes-assembler-source-av1-tu-sequence-header-seen-p
            assembler))
        (declare (ignore hdr-cll-p))
        (unless (and configuration frame-header-found-p)
          (return-from try-start-av1-streaming nil))
        (validate-av1-pes-timestamps header frame-semantics)
        (multiple-value-setq
            (ordering-time ordered-source-p)
          (record-av1-ordering-time
           processor assembler
           (or (pes-header-dts header)
               (pes-header-pts header))
           (not (null (pes-header-dts header)))))
        (unless (equalp
                 configuration
                 (pes-assembler-source-av1-configuration assembler))
          (record-av1-signaling-configuration
           processor configuration ordering-time
           ordered-source-p))
        ;; HDR_CLLはframe OBU後にも置けるため、RAP/CLL policyは
        ;; 完全PES境界でのみ確定する。
        (let* ((transformer (make-av1-stream-transformer))
               (entries
                 (reverse
                  (pes-assembler-entries assembler)))
               (first-entry (first entries)))
          (unless first-entry
            (bridge-error "Streaming AV1 PES has no transport packets"))
          (setf
           (bridge-processor-last-av1-configuration processor)
           configuration
           (bridge-processor-last-av1-reduced-header-p processor)
           reduced-header-p
           (pes-assembler-streaming-p assembler) t
           (pes-assembler-stream-configuration assembler)
           configuration
           (pes-assembler-stream-random-access-kind assembler)
           random-access-kind
           (pes-assembler-stream-frame-semantics assembler)
           frame-semantics
           (pes-assembler-stream-ordering-time assembler)
           ordering-time
           (pes-assembler-stream-temporal-delimiter-seen-p
            assembler)
           temporal-delimiter-seen-p
           (pes-assembler-stream-sequence-header-in-access-unit-p
            assembler)
           sequence-header-in-access-unit-p
           (pes-assembler-av1-stream-transformer assembler)
           transformer
           (pes-assembler-av1-stream-output-start-p assembler) t)
          (register-av1-tstd-access-unit
           processor
           (pes-assembler-pid assembler)
           header
           configuration
           frame-semantics)
          ;; PMT eventの意味だけを先に確定する。ここで固定slotをallocate
          ;; すると、tailからちょうどH slot前のnullがbackfill直前に
          ;; 出力されてしまうため、synthetic PMT配置後までflushしない。
          (loop while
                (resolve-oldest-ready-event-lookahead processor))
          (let* ((event
                   (make-streaming-av1-video-event
                    assembler entries configuration
                    random-access-kind))
                 (synthetic-pmt
                   (if (equalp
                        configuration
                        (bridge-processor-advertised-av1-configuration
                         processor))
                       '()
                       (synthetic-pmt-before-video
                        processor event))))
            (resolve-initial-av1-stream-entries
             processor assembler entries
             transformer payload-offset
             synthetic-pmt))
          (setf (bridge-processor-seen-video-pes-p processor) t)
          t)))))

(defun resolve-streaming-video-entry
    (processor assembler entry packet payload)
  "逐次変換済みPAYLOADをPACKET templateへ配置してENTRYを解決する。"
  (let* ((pid (pes-assembler-pid assembler))
         (fallback (ts-continuity-counter packet))
         (next-counter
           (output-continuity-counter processor pid fallback))
         (capacity (template-payload-capacity packet 0)))
    (when (ts-discontinuity-indicator-p packet)
      (setf next-counter fallback))
    (cond
      ((plusp (length payload))
       (let* ((first-count (min capacity (length payload)))
              (first-packet
                (make-payload-packet-from-template
                 packet
                 (subseq payload 0 first-count)
                 next-counter
                 :clear-adaptation-flags #x60))
              (packets (list first-packet))
              (offset first-count))
         (incf next-counter)
         (setf next-counter (logand next-counter #x0f))
         (loop while (< offset (length payload))
               for count =
                 (min 184 (- (length payload) offset))
               do (setf packets
                        (append
                         packets
                         (list
                          (make-extra-payload-packet
                           packet
                           (subseq payload offset
                                   (+ offset count))
                           next-counter)))
                        offset (+ offset count)
                        next-counter
                        (logand (+ next-counter 1) #x0f)))
         (resolve-entry entry packets)
         (setf (gethash
                pid
                (bridge-processor-output-continuity-counters
                 processor))
               next-counter)))
      (t
       (let ((adaptation-only
               (make-adaptation-only-from-template
                packet
                (adaptation-output-counter
                 next-counter fallback)
                :clear-adaptation-flags #x60)))
         (resolve-entry
          entry
          (if adaptation-only
              (list adaptation-only)
              '()))))))
  entry)

(defun resolve-streaming-vp9-entry (processor assembler entry packet)
  "逐次出力中のVP9継続PACKETを元templateに従って解決する。"
  (let ((payload-offset (ts-payload-offset packet)))
    (resolve-streaming-video-entry
     processor assembler entry packet
     (if payload-offset
         (subseq packet payload-offset +ts-packet-size+)
         #()))))

(defun resolve-streaming-av1-entry (processor assembler entry packet)
  "逐次出力中のAV1継続PACKETを変換して解決する。"
  (let ((payload-offset (ts-payload-offset packet)))
    (when payload-offset
      (append-av1-stream-byte-segment
       processor assembler
       (transform-av1-stream-chunk
        (pes-assembler-av1-stream-transformer assembler)
       packet
       :start payload-offset
       :end +ts-packet-size+)
       (pending-entry-slot-index entry)))
    (enqueue-av1-stream-target-entry assembler entry)
    (resolve-ready-av1-stream-target-entries
     processor assembler
     :current-slot
     (pending-entry-slot-index entry))))

(defun resolve-active-streaming-video-entry
    (processor assembler entry packet)
  "現在選択中のcodecで逐次PACKETを解決する。"
  (ecase (bridge-processor-video-codec processor)
    (:vp9
     (resolve-streaming-vp9-entry
      processor assembler entry packet))
    (:av1
     (resolve-streaming-av1-entry
      processor assembler entry packet))))

(defun resolve-expired-active-av1-targets (processor)
  "非映像slot進行でも2ms窓を越えるAV1 targetを確定する。"
  (let* ((pid
           (bridge-processor-current-video-pid processor))
         (assembler
           (and
            pid
            (gethash
             pid
             (bridge-processor-pes-assemblers processor))))
         (tail
           (bridge-processor-pending-tail processor)))
    (when (and
           assembler
           tail
           (pes-assembler-streaming-p assembler)
           (pes-assembler-av1-stream-target-head assembler))
      (resolve-ready-av1-stream-target-entries
       processor assembler
       :current-slot
       (pending-entry-slot-index tail)))))

(defun maybe-enqueue-active-av1-null-target
    (processor entry packet)
  "AV1 PES中のnullを未出力byte再充填用targetとして保持する。"
  (unless (= (ts-pid packet) +ts-null-pid+)
    (return-from maybe-enqueue-active-av1-null-target nil))
  (let* ((pid
           (bridge-processor-current-video-pid processor))
         (assembler
           (and
            pid
            (gethash
             pid
             (bridge-processor-pes-assemblers processor)))))
    (when (and
           assembler
           (pes-assembler-active-p assembler)
           (pes-assembler-streaming-p assembler)
           (eq (bridge-processor-video-codec processor) :av1))
      (mark-entry-unresolved entry)
      (enqueue-av1-stream-target-entry
       assembler entry :kind :null)
      t)))

(defun validate-streamed-vp9-pes (processor pes entries)
  "既に出力したVP9 PES全体を境界で検証する。失敗時は巻き戻さない。"
  (let* ((header (parse-pes-header pes))
         (payload-offset (pes-header-payload-offset header))
         (payload
           (make-array (- (length pes) payload-offset)
                       :element-type 'octet
                       :displaced-to pes
                       :displaced-index-offset payload-offset)))
    (unless (pes-header-pts header)
      (bridge-error "Video PES does not carry a PTS"))
    (multiple-value-bind
          (configuration ranges next-state structures)
        (parse-vp9-access-unit
         payload
         :state
         (bridge-processor-vp9-validation-state processor))
      (declare (ignore configuration ranges structures))
      (unless (every #'pending-entry-resolved-p entries)
        (bridge-error
         "Streaming VP9 PES leaves unresolved transport packets"))
      (setf
       (bridge-processor-vp9-validation-state processor)
       next-state
       (bridge-processor-seen-video-pes-p processor)
       t)
      t)))

(defun validate-streamed-av1-pes
    (processor assembler pes entries)
  "既に出力したAV1 PES全体と逐次変換状態を境界で検証する。"
  (let* ((header (parse-pes-header pes))
         (payload-offset (pes-header-payload-offset header))
         (payload
           (make-array (- (length pes) payload-offset)
                       :element-type 'octet
                       :displaced-to pes
                       :displaced-index-offset payload-offset))
         (sequence-header (av1-sequence-header-obu payload))
         (configuration
           (pes-assembler-stream-configuration assembler))
         (reduced-header-p
           (if sequence-header
               (av1-sequence-header-reduced-header-p payload)
               (pes-assembler-source-av1-reduced-header-p
                assembler)))
         (random-access-kind nil)
         (frame-semantics nil)
         (hdr-cll-p nil)
         (temporal-delimiter-seen-p nil)
         (sequence-header-in-access-unit-p nil)
         (next-frame-state nil)
         (resolved-sequence-state nil))
    (unless (pes-header-pts header)
      (bridge-error "Video PES does not carry a PTS"))
    (finish-av1-stream-transformer
     (pes-assembler-av1-stream-transformer assembler))
    (resolve-ready-av1-stream-target-entries
     processor assembler :force-p t)
    (when (pes-assembler-av1-stream-target-head assembler)
      (bridge-error
       "INTERNAL_AV1_STREAM_TARGET_QUEUE_NOT_DRAINED count=~D"
       (pes-assembler-av1-stream-target-count assembler)))
    (finish-av1-stream-byte-fifo assembler)
    (when sequence-header
      (multiple-value-bind (parsed width height)
          (parse-av1-sequence-header payload)
        (declare (ignore width height))
        (unless (equalp parsed configuration)
          (bridge-error
           "Streaming AV1 configuration changes after output begins"))))
    (multiple-value-setq
        (random-access-kind frame-semantics
         temporal-delimiter-seen-p
         sequence-header-in-access-unit-p)
      (av1-access-unit-random-access-kind
       payload
       reduced-header-p
       (pes-assembler-source-av1-tu-sequence-header-seen-p
        assembler)))
    (validate-av1-pes-timestamps header frame-semantics)
    (setf hdr-cll-p
          (av1-access-unit-hdr-cll-p payload))
    (multiple-value-bind
          (structure-result validated-frame-state
           validated-sequence-state)
        (validate-av1-frame-access-unit-structure
         payload
         (bridge-processor-av1-sequence-validation-state processor)
         (bridge-processor-av1-frame-validation-state processor))
      (update-tstd-access-unit-frame-metadata
       (or
        (bridge-processor-tstd-model processor)
        (bridge-error "TSTD_MODEL_MISSING_FOR_AV1"))
       (or
        (pes-header-dts header)
        (pes-header-pts header)
        (bridge-error "TSTD_AV1_DTS_UNAVAILABLE"))
       (av1-frame-structure-result-semantics
        structure-result)
       (av1-frame-structure-result-refresh-frame-flags
        structure-result))
      (setf next-frame-state validated-frame-state
            resolved-sequence-state validated-sequence-state))
    (unless (eql
             random-access-kind
             (pes-assembler-stream-random-access-kind assembler))
      (bridge-error
       "Streaming AV1 random access classification changes at PES boundary"))
    (unless (equalp
             frame-semantics
             (pes-assembler-stream-frame-semantics assembler))
      (bridge-error
       "Streaming AV1 frame semantics change at PES boundary"))
    (unless (eql
             temporal-delimiter-seen-p
             (pes-assembler-stream-temporal-delimiter-seen-p
              assembler))
      (bridge-error
       "Streaming AV1 temporal delimiter state changes at PES boundary"))
    (unless (eql
             sequence-header-in-access-unit-p
             (pes-assembler-stream-sequence-header-in-access-unit-p
              assembler))
      (bridge-error
       "Streaming AV1 sequence header state changes at PES boundary"))
    (unless (every #'pending-entry-resolved-p entries)
      (bridge-error
       "Streaming AV1 PES leaves unresolved transport packets"))
    (record-av1-access-unit-conformance
     processor
     random-access-kind
     hdr-cll-p
     (or
      (pes-assembler-stream-ordering-time assembler)
      (bridge-error
       "Streaming AV1 ordering time is unavailable")))
    (commit-av1-temporal-unit-state
     processor
     temporal-delimiter-seen-p
     sequence-header-in-access-unit-p)
    (setf
     (bridge-processor-av1-frame-validation-state processor)
     next-frame-state
     (bridge-processor-av1-sequence-validation-state processor)
     resolved-sequence-state
     (bridge-processor-seen-video-pes-p processor)
     t)
    t))

(defun validate-streamed-video-pes
    (processor assembler pes entries)
  "CODEC別に逐次出力済みPESを全体検証する。"
  (ecase (bridge-processor-video-codec processor)
    (:vp9
     (validate-streamed-vp9-pes processor pes entries))
    (:av1
     (validate-streamed-av1-pes
      processor assembler pes entries))))

(defun make-video-event-from-pes (processor assembler pes entries)
  "完成PESをcodec規則で変換準備したVIDEO-EVENTにする。"
  (let* ((header (parse-pes-header pes))
         (payload
           (subseq pes (pes-header-payload-offset header)))
         (codec (bridge-processor-video-codec processor))
         (configuration nil)
         (reduced-header-p nil)
         (random-access-p nil)
         (frame-semantics nil)
         (refresh-frame-flags nil)
         (output-pes nil)
         (next-vp9-state nil)
         (next-av1-frame-state nil)
         (next-av1-sequence-state nil))
    (unless (pes-header-pts header)
      (bridge-error "Video PES does not carry a PTS"))
    (ecase codec
      (:vp9
       (multiple-value-bind
             (vp9-configuration ranges validated-state structures)
           (parse-vp9-access-unit
            payload
            :state
            (bridge-processor-vp9-validation-state processor))
         (declare (ignore ranges structures))
         (setf random-access-p
               (vp9-frame-configuration-key-frame-p
                vp9-configuration)
               next-vp9-state
               validated-state
               output-pes
               (make-pes #xe0 payload
                         (pes-header-pts header)
                         :dts (pes-header-dts header)
                         :data-alignment t))))
      (:av1
       (let ((sequence-header
               (av1-sequence-header-obu payload))
             (ordering-time nil)
             (ordered-source-p nil))
         (multiple-value-setq
             (ordering-time ordered-source-p)
           (record-av1-ordering-time
            processor assembler
            (or (pes-header-dts header)
                (pes-header-pts header))
            (not (null (pes-header-dts header)))))
         (cond
           (sequence-header
            (multiple-value-bind
                  (parsed-configuration width height)
                (parse-av1-sequence-header payload)
              (declare (ignore width height))
              (record-av1-signaling-configuration
               processor parsed-configuration ordering-time
               ordered-source-p)
              (setf configuration parsed-configuration))
            (setf
             (bridge-processor-last-av1-configuration processor)
             configuration
             reduced-header-p
             (av1-sequence-header-reduced-header-p payload)
             (bridge-processor-last-av1-reduced-header-p processor)
             reduced-header-p))
           (t
            (setf
             configuration
             (pes-assembler-source-av1-configuration assembler)
             reduced-header-p
             (pes-assembler-source-av1-reduced-header-p
              assembler))))
         (unless configuration
           (bridge-error
            "Initial AV1 access unit has no sequence header"))
         (multiple-value-bind
               (structure-result validated-frame-state
                validated-sequence-state)
             (validate-av1-frame-access-unit-structure
              payload
              (bridge-processor-av1-sequence-validation-state
               processor)
              (bridge-processor-av1-frame-validation-state processor))
           (setf frame-semantics
                 (av1-frame-structure-result-semantics
                  structure-result)
                 refresh-frame-flags
                 (av1-frame-structure-result-refresh-frame-flags
                  structure-result)
                 next-av1-frame-state validated-frame-state
                 next-av1-sequence-state validated-sequence-state))
         (multiple-value-bind
               (random-access-kind parsed-frame-semantics
                temporal-delimiter-seen-p
                sequence-header-in-access-unit-p)
             (av1-access-unit-random-access-kind
              payload
              reduced-header-p
              (pes-assembler-source-av1-tu-sequence-header-seen-p
               assembler))
           (validate-av1-pes-timestamps header parsed-frame-semantics)
           (record-av1-access-unit-conformance
            processor
            random-access-kind
            (av1-access-unit-hdr-cll-p payload)
            ordering-time)
           (commit-av1-temporal-unit-state
            processor
            temporal-delimiter-seen-p
            sequence-header-in-access-unit-p)
           (let ((converted
                 (convert-av1-access-unit-to-ts-format payload)))
             (setf random-access-p
                   (not (null random-access-kind))
                   output-pes
                   (make-pes #xbd converted
                             (pes-header-pts header)
                             :dts (pes-header-dts header)
                             :data-alignment t)))))))
    (let ((event
            (make-video-event
             :entries entries
             :pes output-pes
             :codec codec
             :configuration configuration
             :random-access-p random-access-p
             :frame-semantics frame-semantics
             :refresh-frame-flags refresh-frame-flags
             :source-pmt-table
             (pes-assembler-source-pmt-table assembler)
             :pmt-pid
             (pes-assembler-source-pmt-pid assembler)
             :pmt-template
             (pes-assembler-source-pmt-template assembler)
             :video-pid (pes-assembler-pid assembler)
             :opus-pids
             (pes-assembler-source-opus-pids assembler))))
      (ecase codec
        (:vp9
         (setf
          (bridge-processor-vp9-validation-state processor)
          next-vp9-state))
        (:av1
         (setf
          (bridge-processor-av1-frame-validation-state processor)
          next-av1-frame-state
          (bridge-processor-av1-sequence-validation-state processor)
          next-av1-sequence-state)))
      (setf (bridge-processor-seen-video-pes-p processor) t)
      event)))

(defun opus-pts-delta-ticks (expected actual)
  "33-bit PTS 空間での signed 最短距離を返す。"
  (let ((forward (mod (- actual expected) +pts-modulus+)))
    (if (>= forward +pts-half-modulus+)
        (- forward +pts-modulus+)
        forward)))

(defun validate-opus-pts-continuity (assembler pes samples)
  "ASSEMBLERのOpus PTSを可変packet durationに対して検証する。"
  (let* ((header (parse-pes-header pes))
         (pts (pes-header-pts header))
         (expected (pes-assembler-expected-pts assembler))
         (skip-p
           (pes-assembler-skip-next-pts-check-p assembler))
         (ticks (/ (* samples 90000) +opus-clock-rate+)))
    (unless (integerp ticks)
      (bridge-error "Opus duration does not map to an integer 90kHz PTS"))
    (when (and expected (not skip-p))
      (let ((delta (opus-pts-delta-ticks expected pts)))
        ;; 完全一致、または FFmpeg live mux の微小ジッターだけを通す。
        ;; 許容時は actual PTS を起点に再計算し、誤差を蓄積させない。
        (unless (or (zerop delta)
                    (<= (abs delta) +opus-pts-jitter-tolerance-ticks+))
          (bridge-error
           "Opus PTS discontinuity on PID 0x~4,'0X: expected=~D actual=~D"
           (pes-assembler-pid assembler) expected pts))))
    (setf (pes-assembler-expected-pts assembler)
          (mod (+ pts ticks) +pts-modulus+)
          (pes-assembler-skip-next-pts-check-p assembler) nil)))

(defun finish-pes-event (processor assembler)
  "ASSEMBLERの完成PESを検証しpending entryを解決可能にする。"
  (unless (pes-assembler-active-p assembler)
    (return-from finish-pes-event nil))
  (let ((expected (pes-assembler-expected-length assembler))
        (actual (length (pes-assembler-buffer assembler))))
    (when (and expected (/= expected actual))
      (bridge-error
       "PES is truncated: expected=~D actual=~D PID=0x~4,'0X"
       expected actual (pes-assembler-pid assembler))))
  (let* ((pes (pes-assembler-buffer assembler))
         (entries (nreverse (pes-assembler-entries assembler)))
         (first-entry (first entries)))
    (unless first-entry
      (bridge-error "PES has no transport packet entries"))
    (ecase (pes-assembler-kind assembler)
      (:video
       (if (pes-assembler-streaming-p assembler)
           (validate-streamed-video-pes
            processor assembler pes entries)
           (let ((event
                   (make-video-event-from-pes
                    processor assembler pes entries)))
             (setf (pending-entry-event first-entry) event))))
      (:opus
       (let ((samples (validate-ffmpeg-opus-pes pes)))
         (validate-opus-pts-continuity assembler pes samples)
         (dolist (entry entries)
           (resolve-entry-as-original entry)))))
    (clear-pes-event-state assembler)
    t))

(defun declared-pes-complete-p (assembler)
  "ASSEMBLERが非0宣言長のPES全体を保持するかを返す。"
  (let ((expected (pes-assembler-expected-length assembler)))
    (and expected
         (>= (length (pes-assembler-buffer assembler))
             expected))))

(defun process-target-pes-packet
    (processor assembler entry packet)
  "対象PIDのPACKETをPES単位で検証・保留する。"
  (let ((output-target-consumed-p nil))
    (validate-target-transport-packet assembler packet)
    (unless (validate-pes-transport-continuity assembler packet)
      (resolve-entry entry '())
      (return-from process-target-pes-packet nil))
    (when (and (ts-payload-unit-start-p packet)
               (pes-assembler-active-p assembler))
      (setf output-target-consumed-p
            (maybe-borrow-next-av1-pusi-target
             processor assembler entry packet))
      (finish-pes-event processor assembler))
    (cond
      ((ts-payload-unit-start-p packet)
       (unless (ts-has-payload-p packet)
         (bridge-error "PES start packet has no payload"))
       (start-pes-event processor assembler))
      ((not (pes-assembler-active-p assembler))
       (if (ts-has-payload-p packet)
           (bridge-error
            "PES payload arrives without PUSI on PID 0x~4,'0X"
            (pes-assembler-pid assembler))
           (return-from process-target-pes-packet nil))))
    (unless output-target-consumed-p
      (mark-entry-unresolved entry))
    (unless (pes-assembler-first-entry-slot-index assembler)
      (setf (pes-assembler-first-entry-slot-index assembler)
            (pending-entry-slot-index entry)))
    (push entry (pes-assembler-entries assembler))
    (when (ts-has-payload-p packet)
      (append-pes-octets assembler packet)
      (maybe-record-program-timestamp processor assembler)
      (cond
        ((pes-assembler-streaming-p assembler)
         (resolve-active-streaming-video-entry
          processor assembler entry packet))
        ((or (try-start-vp9-streaming processor assembler)
             (try-start-av1-streaming processor assembler))
         nil)
        ((declared-pes-complete-p assembler)
         (trim-complete-pes-buffer assembler)
         (finish-pes-event processor assembler))))
    (when (and (not (ts-has-payload-p packet))
               (pes-assembler-streaming-p assembler))
      (resolve-active-streaming-video-entry
       processor assembler entry packet))
    t))

(defun process-bridge-packet (processor packet)
  "厳格検証済みTS PACKETをsemantic processorへ1個与える。"
  (validate-ts-packet packet)
  (let* ((pid (ts-pid packet))
         (selected-pcr-pid-p
           (eql
            pid
            (bridge-processor-current-pcr-pid processor))))
    ;; 選択PCRの診断codeはglobal検証より先に保つが、状態は更新しない。
    (when (and selected-pcr-pid-p
               (ts-transport-error-p packet))
      (bridge-error
       "SELECTED_PCR_TRANSPORT_ERROR pid=0x~4,'0X"
       pid))
    ;; queue追加とsemantic state更新の前に全PIDをfail closed検証する。
    (validate-ts-packet-integrity
     (bridge-processor-transport-integrity-validator processor)
     packet)
    (record-selected-pcr-packet
     processor packet pid
     (when selected-pcr-pid-p
       (ts-pcr packet)))
    (let ((entry (append-pending-entry processor packet)))
      (cond
        ((zerop pid)
         (process-pat-packet processor packet))
        ((and (bridge-processor-pmt-pid processor)
              (= pid (bridge-processor-pmt-pid processor)))
         (process-pmt-packet processor entry packet))
        (t
         (let ((assembler
                 (current-pes-assembler-for-packet
                  processor packet)))
           (when assembler
             (process-target-pes-packet
              processor assembler entry packet)))))
      (maybe-enqueue-active-av1-null-target
       processor entry packet)
      (resolve-expired-active-av1-targets processor)
      (flush-resolved-entries processor)))
  nil)

(defun incomplete-section-buffer-p (assembler)
  "ASSEMBLERが未完section byteを保持するかを返す。"
  (and assembler
       (plusp
        (length (section-assembler-buffer assembler)))))

(defun finish-bridge-processor (processor)
  "EOFで未完状態をfail closed検査し、残りqueueを出力する。"
  (setf (bridge-processor-eof-p processor) t)
  (when (incomplete-section-buffer-p
         (bridge-processor-pat-assembler processor))
    (bridge-error "EOF truncates a PAT section"))
  (when (or (bridge-processor-current-pmt-entries processor)
            (incomplete-section-buffer-p
             (bridge-processor-pmt-assembler processor)))
    (bridge-error "EOF truncates a PMT section"))
  (maphash
   (lambda (pid assembler)
     (declare (ignore pid))
     (when (pes-assembler-active-p assembler)
       (finish-pes-event processor assembler)))
   (bridge-processor-pes-assemblers processor))
  (unless (bridge-processor-seen-pmt-p processor)
    (bridge-error "EOF occurs before a complete PMT"))
  (when (and
         (bridge-processor-current-pcr-pid processor)
         (not
          (bridge-processor-selected-pcr-ever-seen-p processor)))
    (bridge-error
     "SELECTED_PCR_MISSING pid=0x~4,'0X"
     (bridge-processor-current-pcr-pid processor)))
  (when (and
         (bridge-processor-current-pcr-pid processor)
         (not
          (bridge-processor-selected-pcr-baselined-p processor)))
    (bridge-error
     "SELECTED_PCR_REBASE_REQUIRED pid=0x~4,'0X"
     (bridge-processor-current-pcr-pid processor)))
  (when (and (eq (bridge-processor-video-codec processor) :av1)
             (not (bridge-processor-seen-video-pes-p processor)))
    (bridge-error "EOF occurs before the initial AV1 access unit"))
  (flush-resolved-entries processor :force-p t)
  (when (bridge-processor-pending-head processor)
    (bridge-error "EOF leaves unresolved semantic transport packets"))
  (finish-output-packet-allocation processor)
  (let ((model
          (bridge-processor-tstd-model processor)))
    (when model
      (finish-tstd-model model)))
  (handler-case
      (finish-output (bridge-processor-output processor))
    (stream-error (cause)
      (error 'bridge-io-error
             :message (format nil "Output flush failed: ~A" cause)
             :operation :flush
             :cause cause)))
  nil)
