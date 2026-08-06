;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defconstant +tstd-transport-buffer-size+ 512)
(defconstant +tstd-maximum-transport-buffer-busy-period+ 1)
(defconstant +tstd-maximum-access-unit-delay+ 10)
;; FFmpeg live AV1 は DTS が transport arrival より数秒先行することがある。
;; この秒数以内なら arrival 直後へ clamp し、それを超える破綻だけ落とす。
(defconstant +tstd-maximum-removal-before-arrival-seconds+ 3)
(defconstant +av1-buffer-pool-size+ 10)
(defconstant +av1-virtual-buffer-index-count+ 8)

(defstruct tstd-buffer-pool
  (decoder-reference-counts
    (make-array +av1-buffer-pool-size+
                :element-type 'fixnum
                :initial-element 0)
    :type (simple-array fixnum (10)))
  (player-reference-counts
    (make-array +av1-buffer-pool-size+
                :element-type 'fixnum
                :initial-element 0)
    :type (simple-array fixnum (10)))
  (presentation-times
    (make-array +av1-buffer-pool-size+
                :initial-element nil)
    :type simple-vector)
  (virtual-buffer-indices
    (make-array +av1-virtual-buffer-index-count+
                :element-type 'fixnum
                :initial-element -1)
    :type (simple-array fixnum (8)))
  (decoded-access-unit-count 0 :type (integer 0 *))
  (presentation-removal-count 0 :type (integer 0 *)))

(defstruct (tstd-model
            (:constructor %make-tstd-model
                (transport-rate-kbps clock)))
  (transport-rate-kbps 0 :type (integer 1 *))
  (clock (make-tstd-arrival-clock 1)
         :type tstd-arrival-clock)
  (packet-index 0 :type (integer 0 *))
  (video-pid nil :type (or null (unsigned-byte 13)))
  (pcr-pid nil :type (or null (unsigned-byte 13)))
  (pending-access-units '() :type list)
  (current-access-unit nil :type (or null tstd-access-unit))
  (discarding-access-unit-p nil :type boolean)
  (classifier (make-tstd-pes-classifier)
              :type tstd-pes-classifier)
  (rx-bytes-per-second 1 :type (rational 0 *))
  (multiplex-buffer-size 0 :type (rational 0 *))
  (transport-buffer-fullness 0 :type (rational 0 *))
  (transport-buffer-last-arrival nil :type (or null rational))
  (transport-buffer-service-end nil :type (or null rational))
  (transport-buffer-busy-start nil :type (or null rational))
  (transport-buffer-last-empty nil :type (or null rational))
  (multiplex-buffer-fullness 0 :type (rational 0 *))
  (multiplex-buffer-last-arrival nil :type (or null rational))
  (multiplex-buffer-service-end nil :type (or null rational))
  (multiplex-buffer-pending-header-bytes 0
                                         :type (integer 0 *))
  (multiplex-buffer-header-removal-time nil
                                        :type (or null rational))
  (elementary-buffer-fullness 0 :type (integer 0 *))
  (elementary-buffer-removals '() :type list)
  (buffer-pool (make-tstd-buffer-pool)
               :type tstd-buffer-pool))

(defun make-tstd-model (transport-rate-kbps)
  "明示CBR transport rateからAV1 T-STD modelを作る。"
  (%make-tstd-model
   transport-rate-kbps
   (make-tstd-arrival-clock transport-rate-kbps)))

(defun av1-main-tier-bitrate (level)
  "AV1 level indexのMain tier MaxBitrateをbit/sで返す。"
  (case level
    (0 1500000)
    (1 3000000)
    (4 6000000)
    (5 10000000)
    (8 12000000)
    (9 20000000)
    (12 30000000)
    (13 40000000)
    ((14 15 16) 60000000)
    (17 100000000)
    ((18 19) 160000000)
    (otherwise nil)))

(defun av1-high-tier-bitrate (level)
  "AV1 level indexのHigh tier MaxBitrateをbit/sで返す。"
  (case level
    (8 30000000)
    (9 50000000)
    (12 100000000)
    (13 160000000)
    ((14 15 16) 240000000)
    (17 480000000)
    ((18 19) 800000000)
    (otherwise nil)))

(defun av1-profile-bitrate-factor (profile)
  "AV1 profileに対応するMaxBitrate倍率を返す。"
  (case profile
    (0 1)
    (1 2)
    (2 3)
    (otherwise
     (bridge-error
      "TSTD_AV1_PROFILE_UNSUPPORTED profile=~D"
      profile))))

(defun av1-tstd-bitrate-bps (configuration)
  "CONFIGURATIONのlevel/tier/profileからBitRateを厳格に得る。"
  (let ((level
          (av1-codec-configuration-level configuration))
        (tier
          (av1-codec-configuration-tier configuration))
        (profile
          (av1-codec-configuration-profile configuration)))
    (when (= level 31)
      (bridge-error "TSTD_AV1_LEVEL_31_UNSUPPORTED"))
    (let ((base
            (if (zerop tier)
                (av1-main-tier-bitrate level)
                (av1-high-tier-bitrate level))))
      (unless base
        (bridge-error
         "TSTD_AV1_LEVEL_TIER_BITRATE_UNDEFINED level=~D tier=~D"
         level tier))
      (* base (av1-profile-bitrate-factor profile)))))

(defun av1-tstd-elementary-buffer-size (configuration)
  "AOM draftのBufferSizeをbyte単位で返す。"
  (/ (av1-tstd-bitrate-bps configuration) 8))

(defun av1-tstd-multiplex-buffer-size (configuration)
  "AOM draftのMBS式をbitからbyteへ明示変換して返す。"
  (let* ((bit-rate (av1-tstd-bitrate-bps configuration))
         (buffer-size bit-rate)
         (peak-rate (max (* 1100 bit-rate) 2000000))
         (size-bits
           (+ (* (+ 1/750 1/250) peak-rate)
              (* 1/10 buffer-size))))
    (/ size-bits 8)))

(defun av1-tstd-rx-bytes-per-second (configuration)
  "AOM draftのRx=Rbx=1.1*BitRateをbyte/sで返す。"
  (/ (* 11 (av1-tstd-bitrate-bps configuration))
     80))

(defun register-tstd-access-unit
    (model video-pid pcr-pid pts dts configuration
     semantics &optional refresh-frame-flags)
  "出力前にAV1 AUのPID、DTS、buffer parameterをFIFOへ登録する。"
  (unless (and (typep video-pid '(unsigned-byte 13))
               (typep pcr-pid '(unsigned-byte 13)))
    (bridge-error
     "TSTD_PID_UNAVAILABLE video_pid=~S pcr_pid=~S"
     video-pid pcr-pid))
  (when (and (tstd-model-video-pid model)
             (or (/= video-pid (tstd-model-video-pid model))
                 (/= pcr-pid (tstd-model-pcr-pid model)))
             (or (tstd-model-current-access-unit model)
                 (tstd-model-pending-access-units model)))
    (bridge-error "TSTD_PID_CHANGE_WITH_BUFFERED_ACCESS_UNIT"))
  (setf
   (tstd-model-video-pid model) video-pid
   (tstd-model-pcr-pid model) pcr-pid)
  (let ((access-unit
          (make-tstd-access-unit
           :pts pts
           :dts dts
           :low-delay-mode-p
           (av1-codec-configuration-low-delay-mode-p
            configuration)
           :show-existing-frame-p
           (av1-frame-semantics-show-existing-frame-p semantics)
           :frame-to-show-map-index
           (av1-frame-semantics-frame-to-show-map-index semantics)
           :frame-type
           (av1-frame-semantics-frame-type semantics)
           :show-frame-p
           (av1-frame-semantics-show-frame-p semantics)
           :refresh-frame-flags refresh-frame-flags
           :rx-bytes-per-second
           (av1-tstd-rx-bytes-per-second configuration)
           :multiplex-buffer-size
           (av1-tstd-multiplex-buffer-size configuration)
           :elementary-buffer-size
           (av1-tstd-elementary-buffer-size configuration))))
    (when (and
           (null (tstd-model-current-access-unit model))
           (null (tstd-model-pending-access-units model)))
      ;; 初回PUSIより前の同一PID上PCR/adaptation-only packetも
      ;; 188 byte全体をTBで処理できるよう、先頭AUのparameterを先行適用する。
      (setf
       (tstd-model-rx-bytes-per-second model)
       (tstd-access-unit-rx-bytes-per-second access-unit)
       (tstd-model-multiplex-buffer-size model)
       (tstd-access-unit-multiplex-buffer-size access-unit)))
    (setf (tstd-model-pending-access-units model)
          (nconc
           (tstd-model-pending-access-units model)
           (list access-unit)))
    access-unit))

(defun validate-tstd-transport-busy-period
    (model empty-time boundary)
  "TBの連続非空期間をEMPTY-TIMEで閉じ、1秒条件を検証する。"
  (let ((busy-start
          (tstd-model-transport-buffer-busy-start model)))
    (unless busy-start
      (bridge-error
       "TSTD_TB_BUSY_PERIOD_STATE_MISSING boundary=~A"
       boundary))
    (let ((duration (- empty-time busy-start)))
      (when (> duration
               +tstd-maximum-transport-buffer-busy-period+)
        (bridge-error
         "TSTD_TB_NOT_EMPTY_WITHIN_ONE_SECOND duration=~A boundary=~A"
         duration boundary)))
    (setf
     (tstd-model-transport-buffer-fullness model) 0
     (tstd-model-transport-buffer-last-empty model) empty-time
     (tstd-model-transport-buffer-busy-start model) nil
     (tstd-model-transport-buffer-service-end model) nil))
  model)

(defun finish-tstd-transport-busy-period (model boundary)
  "EOFまたはdiscontinuityでTBを最終空化しbusy periodを検証する。"
  (let ((service-end
          (tstd-model-transport-buffer-service-end model))
        (busy-start
          (tstd-model-transport-buffer-busy-start model))
        (fullness
          (tstd-model-transport-buffer-fullness model)))
    (cond
      ((and service-end busy-start)
       (validate-tstd-transport-busy-period
        model service-end boundary))
      ((or service-end busy-start (plusp fullness))
       (bridge-error
        "TSTD_TB_FINAL_EMPTY_STATE_INVALID boundary=~A fullness=~A"
        boundary fullness))))
  model)

(defun process-tstd-transport-byte (model arrival)
  "映像PIDの1 byteをTBへ投入し、そのbyteの離脱完了時刻を返す。"
  (let ((rate (tstd-model-rx-bytes-per-second model))
        (last-arrival
          (tstd-model-transport-buffer-last-arrival model))
        (service-end
          (tstd-model-transport-buffer-service-end model)))
    (when (and service-end (<= service-end arrival))
      (validate-tstd-transport-busy-period
       model service-end :arrival)
      (setf service-end nil))
    (unless (tstd-model-transport-buffer-busy-start model)
      (setf
       (tstd-model-transport-buffer-busy-start model)
       arrival))
    (let ((fullness
            (if last-arrival
                (max
                 0
                 (- (tstd-model-transport-buffer-fullness model)
                    (* rate (- arrival last-arrival))))
                0)))
      (incf fullness)
      (when (> fullness +tstd-transport-buffer-size+)
        (bridge-error
         "TSTD_TB_OVERFLOW fullness=~A capacity=~D"
         fullness +tstd-transport-buffer-size+))
      (let* ((service-start
               (if service-end
                   (max arrival service-end)
                   arrival))
             (next-service-end
               (+ service-start
                  (/ rate))))
        (when (> (- next-service-end
                    (tstd-model-transport-buffer-busy-start model))
                 +tstd-maximum-transport-buffer-busy-period+)
          (bridge-error
           "TSTD_TB_NOT_EMPTY_WITHIN_ONE_SECOND duration=~A boundary=ARRIVAL"
           (- next-service-end
              (tstd-model-transport-buffer-busy-start model))))
        (setf
         (tstd-model-transport-buffer-fullness model) fullness
         (tstd-model-transport-buffer-last-arrival model) arrival
         (tstd-model-transport-buffer-service-end model)
         next-service-end)
        next-service-end))))

(defun process-tstd-transport-packet (model arrival)
  "映像PIDの188 byteを各t(i)でTBへ投入し、離脱時刻rangeを返す。"
  (let* ((clock (tstd-model-clock model))
         (arrival-interval
           (/ 8
               (tstd-arrival-clock-transport-rate-bps
                clock)))
         (service-interval
           (/ (tstd-model-rx-bytes-per-second model)))
         (first-departure
           (process-tstd-transport-byte model arrival))
         (second-arrival (+ arrival arrival-interval)))
    ;; 最初のbyteが次のarrivalまでに空なら、以後も各byteは独立した
    ;; busy periodになる。全188回の有理数recurrenceを閉形式で更新する。
    (when (<= first-departure second-arrival)
      (validate-tstd-transport-busy-period
       model first-departure :arrival)
      (let* ((last-offset (1- +ts-packet-size+))
             (last-arrival
               (+ arrival (* last-offset arrival-interval)))
             (last-departure (+ last-arrival service-interval))
             (penultimate-departure
               (+ arrival
                  (* (- last-offset 1) arrival-interval)
                  service-interval))
             (second-departure
               (+ second-arrival service-interval)))
        (setf
         (tstd-model-transport-buffer-fullness model) 1
         (tstd-model-transport-buffer-last-arrival model)
         last-arrival
         (tstd-model-transport-buffer-service-end model)
         last-departure
         (tstd-model-transport-buffer-busy-start model)
         last-arrival
         (tstd-model-transport-buffer-last-empty model)
         penultimate-departure)
        (return-from process-tstd-transport-packet
          (if (= (- second-departure first-departure)
                 arrival-interval)
              (vector
               (make-tstd-departure-range
                0 +ts-packet-size+
                first-departure arrival-interval))
              (vector
               (make-tstd-departure-range
                0 1 first-departure 0)
               (make-tstd-departure-range
                1 +ts-packet-size+
                second-departure arrival-interval))))))
    ;; TBがbyte間で空にならない条件は従来のrecurrenceを保ち、結果だけを
    ;; 等間隔rangeへ圧縮して後段の一括処理へ渡す。
    (let ((ranges '())
          (range-start 0)
          (range-first first-departure)
          (range-interval nil)
          (previous first-departure))
      (loop for offset from 1 below +ts-packet-size+
            for departure =
              (process-tstd-transport-byte
               model
               (+ arrival (* offset arrival-interval)))
            for interval = (- departure previous)
            do
               (cond
                 ((null range-interval)
                  (setf range-interval interval))
                 ((/= interval range-interval)
                  (push
                   (make-tstd-departure-range
                    range-start offset range-first
                    range-interval)
                   ranges)
                  (setf range-start offset
                        range-first departure
                        range-interval nil)))
               (setf previous departure))
      (push
       (make-tstd-departure-range
        range-start +ts-packet-size+ range-first
        (or range-interval 0))
       ranges)
      (coerce (nreverse ranges) 'simple-vector))))

(defun tstd-transport-packet-overflow-p
    (model arrival rate)
  "現在のTB状態へ1 packetを投入した際の最大fullnessを解析的に予測する。"
  (let* ((clock (tstd-model-clock model))
         (arrival-interval
           (/ 8
              (tstd-arrival-clock-transport-rate-bps
               clock)))
         (fullness
           (tstd-model-transport-buffer-fullness model))
         (last-arrival
           (tstd-model-transport-buffer-last-arrival model))
         (service-end
           (tstd-model-transport-buffer-service-end model))
         (first-fullness
           (1+
            (if (and service-end
                     (<= service-end arrival))
                0
                (if last-arrival
                    (max
                     0
                     (- fullness
                        (* rate
                           (- arrival last-arrival))))
                    0))))
         ;; 2 byte目以降は各arrival間にRATE * intervalだけ減り、
         ;; 1 byte増える。正なら188 byte目、非正なら1 byte目が最大。
         (fullness-increment
           (- 1 (* rate arrival-interval)))
         (maximum-fullness
           (if (plusp fullness-increment)
               (+ first-fullness
                  (* (1- +ts-packet-size+)
                     fullness-increment))
               first-fullness)))
    (> maximum-fullness +tstd-transport-buffer-size+)))

(defun tstd-video-packet-would-overflow-p
    (model &key payload-unit-start-p)
  "現在slotへ映像packetを置いた場合のTB overflowを非破壊で予測する。"
  (let* ((packet-index (tstd-model-packet-index model))
         (arrival
           (tstd-packet-arrival-time
            (tstd-model-clock model)
            packet-index))
         (next-access-unit
           (and
            payload-unit-start-p
            (first
             (tstd-model-pending-access-units model))))
         (rate
           (if next-access-unit
               (tstd-access-unit-rx-bytes-per-second
                next-access-unit)
               (tstd-model-rx-bytes-per-second model))))
    (tstd-transport-packet-overflow-p
     model arrival rate)))

(defun tstd-output-video-packet-would-overflow-p
    (model packet)
  "最終出力候補PACKETが現在slotで映像TBをoverflowさせるか返す。"
  (let ((video-pid (tstd-model-video-pid model)))
    (and
     video-pid
     (= (ts-pid packet) video-pid)
     ;; discontinuityは実処理で旧TB epochを破棄してから投入する。
     (not (ts-discontinuity-indicator-p packet))
     (tstd-video-packet-would-overflow-p
      model
      :payload-unit-start-p
      (ts-payload-unit-start-p packet)))))

(defun apply-tstd-multiplex-header-removal-before
    (model time)
  "TIME以前に予約されたPES headerの瞬時除去を適用する。"
  (let ((removal-time
          (tstd-model-multiplex-buffer-header-removal-time
           model)))
    (when (and removal-time (<= removal-time time))
      (let ((header-count
              (tstd-model-multiplex-buffer-pending-header-bytes
               model)))
        (when (> header-count
                 (tstd-model-multiplex-buffer-fullness model))
          (bridge-error
           "TSTD_MB_HEADER_REMOVAL_UNDERFLOW fullness=~A header=~D"
           (tstd-model-multiplex-buffer-fullness model)
           header-count))
        (decf
         (tstd-model-multiplex-buffer-fullness model)
         header-count)
        (setf
         (tstd-model-multiplex-buffer-pending-header-bytes
          model)
         0
         (tstd-model-multiplex-buffer-header-removal-time
          model)
         nil))))
  model)

(defun tstd-multiplex-fullness-at
    (model time)
  "予約済みpayload transferとheader除去をTIMEまで反映する。"
  (apply-tstd-multiplex-header-removal-before model time)
  (let ((last-arrival
          (tstd-model-multiplex-buffer-last-arrival model))
        (service-end
          (tstd-model-multiplex-buffer-service-end model))
        (header-count
          (tstd-model-multiplex-buffer-pending-header-bytes
           model)))
    (if (and last-arrival service-end
             (> service-end last-arrival)
             (> time last-arrival))
        (max
         header-count
         (-
          (tstd-model-multiplex-buffer-fullness model)
          (*
           (tstd-model-rx-bytes-per-second model)
           (- (min time service-end) last-arrival))))
        (tstd-model-multiplex-buffer-fullness model))))

(defun process-tstd-multiplex-header-range
    (model start end first-departure)
  "PES header byteをMBへ到着させ、payload transferまで保持する。"
  (let ((count (- end start)))
    (unless (plusp count)
      (return-from process-tstd-multiplex-header-range nil))
    (let* ((rate (tstd-model-rx-bytes-per-second model))
           (interval (/ rate))
           (starting-fullness
             (tstd-multiplex-fullness-at
              model first-departure))
           (last-arrival
             (+ first-departure (* (- count 1) interval)))
           (service-end
             (tstd-model-multiplex-buffer-service-end model))
           (service-between-arrivals
             (if (and service-end
                      (> service-end first-departure))
                 (*
                  rate
                  (-
                   (min last-arrival service-end)
                   first-departure))
                 0))
           (fullness
             (- (+ starting-fullness count)
                service-between-arrivals))
           (capacity
             (tstd-model-multiplex-buffer-size model)))
      (when
          (tstd-model-multiplex-buffer-header-removal-time
           model)
        (bridge-error
         "TSTD_MB_NEW_PES_BEFORE_HEADER_REMOVAL"))
      (when (> fullness capacity)
        (bridge-error
         "TSTD_MB_OVERFLOW fullness=~A capacity=~A"
         fullness capacity))
      (setf
       (tstd-model-multiplex-buffer-fullness model) fullness
       (tstd-model-multiplex-buffer-last-arrival model)
       last-arrival)
      (incf
       (tstd-model-multiplex-buffer-pending-header-bytes
        model)
       count)
      last-arrival)))

(defun append-tstd-departure-range
    (access-unit start end first-departure interval)
  "MB離脱時刻の等間隔区間をACCESS-UNITへ連結して保持する。"
  (let* ((ranges
           (tstd-access-unit-mb-departure-ranges access-unit))
         (range-count (length ranges))
         (previous
           (when (plusp range-count)
             (aref ranges (- range-count 1)))))
    (when (and previous
               (/= (tstd-departure-range-end previous)
                   start))
      (bridge-error
       "TSTD_MB_DEPARTURE_RANGE_NONCONTIGUOUS previous=~D actual=~D"
       (tstd-departure-range-end previous)
       start))
    (if (and previous
             (= (- end start) 1))
        (let* ((previous-start
                 (tstd-departure-range-start previous))
               (previous-end
                 (tstd-departure-range-end previous))
               (previous-count
                 (- previous-end previous-start))
               (next-interval
                 (- first-departure
                    (+
                     (tstd-departure-range-first-departure
                      previous)
                     (*
                      (- previous-count 1)
                      (tstd-departure-range-interval
                       previous))))))
          (cond
            ((= previous-count 1)
             (setf
              (tstd-departure-range-interval previous)
              next-interval
              (tstd-departure-range-end previous)
              end))
            ((= next-interval
                (tstd-departure-range-interval previous))
             (setf
              (tstd-departure-range-end previous)
              end))
            (t
             (vector-push-extend
              (make-tstd-departure-range
               start end first-departure 0)
              ranges))))
        (vector-push-extend
         (make-tstd-departure-range
          start end first-departure interval)
         ranges)))
  access-unit)

(defun process-tstd-multiplex-range
    (model access-unit packet start end first-departure)
  "TBから等間隔で出たPES payload範囲をMBへ一括投入する。"
  (let* ((count (- end start))
         (rate (tstd-model-rx-bytes-per-second model))
         (interval (/ rate))
         (fullness
           (tstd-multiplex-fullness-at
            model first-departure))
         (capacity
           (tstd-model-multiplex-buffer-size model)))
    (unless (plusp count)
      (return-from process-tstd-multiplex-range nil))
    ;; arrival間隔とMB service間隔は共に1/rateなので、
    ;; 先頭byte投入後のfullnessはこの範囲内で一定になる。
    (incf fullness)
    (when (> fullness capacity)
      (bridge-error
       "TSTD_MB_OVERFLOW fullness=~A capacity=~A"
       fullness capacity))
    (let* ((service-end
             (tstd-model-multiplex-buffer-service-end model))
           (service-start
             (if service-end
                 (max first-departure service-end)
                 first-departure))
           (first-service-end
             (+ service-start interval))
           (remaining-count (- count 1))
           (last-source-departure
             (+ first-departure
                (* remaining-count interval)))
           (last-service-end
             (+ first-service-end
                (* remaining-count interval)))
           (header-count
             (tstd-model-multiplex-buffer-pending-header-bytes
              model))
           (es (tstd-access-unit-es access-unit))
           (es-start (length es))
           (es-end (+ es-start count)))
      (when (and (plusp header-count)
                 (null
                  (tstd-model-multiplex-buffer-header-removal-time
                   model)))
        (setf
         (tstd-model-multiplex-buffer-header-removal-time
          model)
         first-service-end))
      (setf
       (tstd-model-multiplex-buffer-fullness model) fullness
       (tstd-model-multiplex-buffer-last-arrival model)
       last-source-departure
       (tstd-model-multiplex-buffer-service-end model)
       last-service-end)
      (apply-tstd-multiplex-header-removal-before
       model last-source-departure)
      (unless (tstd-access-unit-mb-first-departure access-unit)
        (setf
         (tstd-access-unit-mb-first-departure access-unit)
         first-service-end))
      (setf
       (tstd-access-unit-mb-last-departure access-unit)
       last-service-end)
      (loop for offset from start below end
            do
        (vector-push-extend
         (aref packet offset) es))
      (append-tstd-departure-range
       access-unit es-start es-end
       first-service-end interval)
      last-service-end)))

(defun process-tstd-multiplex-header-source-range
    (model start end first-departure source-interval)
  "TB離脱間隔を保ってPES header範囲をMBへ投入する。"
  (let ((service-interval
          (/ (tstd-model-rx-bytes-per-second model))))
    (if (= source-interval service-interval)
        (process-tstd-multiplex-header-range
         model start end first-departure)
        (loop for offset from start below end
              for departure = first-departure
                then (+ departure source-interval)
              do
                 (process-tstd-multiplex-header-range
                  model offset (+ offset 1) departure)))))

(defun process-tstd-independent-multiplex-range
    (model access-unit packet start end
     first-departure source-interval)
  "byte間で空になるMBへpayload範囲を閉形式で投入する。"
  (let* ((count (- end start))
         (rate (tstd-model-rx-bytes-per-second model))
         (service-interval (/ rate))
         (fullness
           (tstd-multiplex-fullness-at
            model first-departure))
         (capacity
           (tstd-model-multiplex-buffer-size model)))
    (unless (zerop fullness)
      (bridge-error
       "TSTD_MB_INDEPENDENT_RANGE_NOT_EMPTY fullness=~A"
       fullness))
    (when (> 1 capacity)
      (bridge-error
       "TSTD_MB_OVERFLOW fullness=1 capacity=~A"
       capacity))
    (let* ((last-source-departure
             (+ first-departure
                (* (- count 1) source-interval)))
           (first-service-end
             (+ first-departure service-interval))
           (last-service-end
             (+ last-source-departure service-interval))
           (es (tstd-access-unit-es access-unit))
           (es-start (length es))
           (es-end (+ es-start count)))
      (setf
       (tstd-model-multiplex-buffer-fullness model) 1
       (tstd-model-multiplex-buffer-last-arrival model)
       last-source-departure
       (tstd-model-multiplex-buffer-service-end model)
       last-service-end)
      (unless (tstd-access-unit-mb-first-departure access-unit)
        (setf
         (tstd-access-unit-mb-first-departure access-unit)
         first-service-end))
      (setf
       (tstd-access-unit-mb-last-departure access-unit)
       last-service-end)
      (loop for offset from start below end
            do
               (vector-push-extend
                (aref packet offset) es))
      (append-tstd-departure-range
       access-unit es-start es-end
       first-service-end source-interval)
      last-service-end)))

(defun process-tstd-multiplex-source-range
    (model access-unit packet start end
     first-departure source-interval)
  "TB離脱間隔を保ってPES payload範囲をMBへ投入する。"
  (let ((service-interval
          (/ (tstd-model-rx-bytes-per-second model))))
    (when (= source-interval service-interval)
      (return-from process-tstd-multiplex-source-range
        (process-tstd-multiplex-range
         model access-unit packet start end first-departure)))
    (loop with offset = start
          with departure = first-departure
          while (< offset end)
          for service-end =
            (tstd-model-multiplex-buffer-service-end model)
          for header-removal =
            (tstd-model-multiplex-buffer-header-removal-time model)
          for pending-header =
            (tstd-model-multiplex-buffer-pending-header-bytes model)
          do
             (when
                 (and
                  service-end
                  (<= service-end departure)
                  (>= source-interval service-interval)
                  (or
                   (zerop pending-header)
                   (and header-removal
                        (<= header-removal departure))))
               (return
                 (process-tstd-independent-multiplex-range
                  model access-unit packet offset end
                  departure source-interval)))
             (process-tstd-multiplex-range
              model access-unit packet offset (+ offset 1)
              departure)
             (incf offset)
             (incf departure source-interval))))

(defun insert-tstd-elementary-removal (model time count)
  "EB removalを時刻順queueへ追加する。"
  (setf
   (tstd-model-elementary-buffer-removals model)
   (merge
    'list
    (list (cons time count))
    (tstd-model-elementary-buffer-removals model)
    #'<
    :key #'car))
  model)

(defun apply-tstd-elementary-removal (model removal)
  "単一EB removalを適用しunderflowを検証する。"
  (let ((count (cdr removal))
        (fullness
          (tstd-model-elementary-buffer-fullness model)))
    (when (> count fullness)
      (bridge-error
       "TSTD_EB_UNDERFLOW fullness=~D removal=~D"
       fullness count))
    (decf
     (tstd-model-elementary-buffer-fullness model)
     count))
  model)

(defun apply-tstd-elementary-removals-before
    (model time)
  "TIMEより前のEB removalを順番に適用する。"
  (loop
    for removal =
      (first
       (tstd-model-elementary-buffer-removals model))
    while (and removal (<= (car removal) time))
    do
      (pop (tstd-model-elementary-buffer-removals model))
      (apply-tstd-elementary-removal model removal))
  model)

(defun increase-tstd-elementary-fullness
    (model count capacity)
  "EBへCOUNT byteを一括投入しoverflowを検証する。"
  (incf (tstd-model-elementary-buffer-fullness model)
        count)
  (when (> (tstd-model-elementary-buffer-fullness model)
           capacity)
    (bridge-error
     "TSTD_EB_OVERFLOW fullness=~D capacity=~A"
     (tstd-model-elementary-buffer-fullness model)
     capacity))
  model)

(defun process-tstd-elementary-byte
    (model arrival capacity)
  "decoder byteをEBへ投入しoverflowを検証する。"
  (let ((next
          (first
           (tstd-model-elementary-buffer-removals model))))
    (when (and next (<= (car next) arrival))
      (apply-tstd-elementary-removals-before
       model arrival)))
  (increase-tstd-elementary-fullness model 1 capacity))

(defun first-tstd-departure-not-before
    (departures start end time)
  "DEPARTURESのSTART以上END未満でTIME以上の最初のindexを返す。"
  (let ((lower start)
        (upper end))
    (loop while (< lower upper)
          for middle = (floor (+ lower upper) 2)
          do
      (if (< (aref departures middle) time)
          (setf lower (+ middle 1))
          (setf upper middle)))
    lower))

(defun process-tstd-elementary-range
    (model departures start end capacity)
  "連続decoder byte範囲をremoval境界ごとにEBへ一括投入する。"
  (let ((position start))
    (loop while (< position end)
          for next =
            (first
             (tstd-model-elementary-buffer-removals model))
          do
      (unless next
        (increase-tstd-elementary-fullness
         model (- end position) capacity)
        (return))
      (let ((split
              (first-tstd-departure-not-before
               departures position end (car next))))
        (when (> split position)
          (increase-tstd-elementary-fullness
           model (- split position) capacity)
          (setf position split))
        (when (= position end)
          (return))
        (apply-tstd-elementary-removals-before
         model (aref departures position)))))
  model)

(defun tstd-departure-range-time
    (range position)
  "RANGE内POSITIONのMB departure時刻を返す。"
  (+ (tstd-departure-range-first-departure range)
     (* (- position (tstd-departure-range-start range))
        (tstd-departure-range-interval range))))

(defun first-tstd-range-departure-not-before
    (range start end time)
  "RANGEのSTART以上END未満でTIME以上の最初のindexを返す。"
  (let ((lower start)
        (upper end))
    (loop while (< lower upper)
          for middle = (floor (+ lower upper) 2)
          do
      (if (< (tstd-departure-range-time range middle)
             time)
          (setf lower (+ middle 1))
          (setf upper middle)))
    lower))

(defun process-tstd-elementary-departure-range
    (model range start end capacity)
  "単一MB departure rangeのdecoder byteをEBへ一括投入する。"
  (let ((position start))
    (loop while (< position end)
          for next =
            (first
             (tstd-model-elementary-buffer-removals model))
          do
      (unless next
        (increase-tstd-elementary-fullness
         model (- end position) capacity)
        (return))
      (let ((last-arrival
              (tstd-departure-range-time
               range (- end 1))))
        (when (> (car next) last-arrival)
          (increase-tstd-elementary-fullness
           model (- end position) capacity)
          (return)))
      (let ((split
              (first-tstd-range-departure-not-before
               range position end (car next))))
        (when (> split position)
          (increase-tstd-elementary-fullness
           model (- split position) capacity)
          (setf position split))
        (when (= position end)
          (return))
        (apply-tstd-elementary-removals-before
         model
         (tstd-departure-range-time range position)))))
  model)

(defun process-tstd-elementary-departure-ranges
    (model decoder-ranges departure-ranges capacity)
  "DECODER-RANGESとMB departure rangeの交差をEBへ投入する。"
  (let ((departure-index 0)
        (departure-count (length departure-ranges)))
    (dolist (decoder-range decoder-ranges)
      (let ((position (car decoder-range))
            (end (cdr decoder-range)))
        (loop while (< position end)
              do
          (loop while
                  (and
                   (< departure-index departure-count)
                   (<=
                    (tstd-departure-range-end
                     (aref departure-ranges departure-index))
                    position))
                do (incf departure-index))
          (when (= departure-index departure-count)
            (bridge-error
             "TSTD_MB_DEPARTURE_RANGE_MISSING offset=~D"
             position))
          (let* ((departure-range
                   (aref departure-ranges departure-index))
                 (departure-start
                   (tstd-departure-range-start departure-range))
                 (intersection-end
                   (min
                    end
                    (tstd-departure-range-end
                     departure-range))))
            (when (> departure-start position)
              (bridge-error
               "TSTD_MB_DEPARTURE_RANGE_GAP offset=~D next=~D"
               position departure-start))
            (process-tstd-elementary-departure-range
             model departure-range position
             intersection-end capacity)
            (setf position intersection-end)))))
  model))

(defun reset-tstd-buffer-pool (pool)
  "Annex EのBuffer Pool、VBI、参照countを空へ戻す。"
  (fill (tstd-buffer-pool-decoder-reference-counts pool) 0)
  (fill (tstd-buffer-pool-player-reference-counts pool) 0)
  (fill (tstd-buffer-pool-presentation-times pool) nil)
  (fill (tstd-buffer-pool-virtual-buffer-indices pool) -1)
  pool)

(defun release-tstd-buffer-pool-presentations
    (pool time &key all)
  "TIMEより前、またはALL指定時のpresentation参照を解放する。"
  (let ((player-counts
          (tstd-buffer-pool-player-reference-counts pool))
        (decoder-counts
          (tstd-buffer-pool-decoder-reference-counts pool))
        (presentation-times
          (tstd-buffer-pool-presentation-times pool)))
    (dotimes (index +av1-buffer-pool-size+)
      (let ((presentation-time
              (aref presentation-times index)))
        (when (and
               (plusp (aref player-counts index))
               presentation-time
               (or all (< presentation-time time)))
          (setf (aref player-counts index) 0)
          (incf
           (tstd-buffer-pool-presentation-removal-count pool))
          (when (zerop (aref decoder-counts index))
            (setf (aref presentation-times index) nil))))))
  pool)

(defun tstd-buffer-pool-free-index (pool)
  "未割当のBuffer Pool indexを返し、なければNILを返す。"
  (let ((decoder-counts
          (tstd-buffer-pool-decoder-reference-counts pool))
        (player-counts
          (tstd-buffer-pool-player-reference-counts pool)))
    (dotimes (index +av1-buffer-pool-size+ nil)
      (when (and
             (zerop (aref decoder-counts index))
             (zerop (aref player-counts index)))
        (return index)))))

(defun update-tstd-buffer-pool-reference-slots
    (pool buffer-index refresh-frame-flags)
  "Annex E update_ref_buffers相当のVBIとDecoderRefCount更新を行う。"
  (let ((decoder-counts
          (tstd-buffer-pool-decoder-reference-counts pool))
        (virtual-indices
          (tstd-buffer-pool-virtual-buffer-indices pool)))
    (dotimes (slot +av1-virtual-buffer-index-count+)
      (when (logbitp slot refresh-frame-flags)
        (let ((old-index (aref virtual-indices slot)))
          (when (not (minusp old-index))
            (unless
                (plusp (aref decoder-counts old-index))
              (bridge-error
               "TSTD_BUFFER_POOL_REFERENCE_COUNT_INVALID slot=~D index=~D"
               slot old-index))
            (decf (aref decoder-counts old-index))))
        (setf (aref virtual-indices slot) buffer-index)
        (incf (aref decoder-counts buffer-index)))))
  pool)

(defun process-tstd-buffer-pool-access-unit
    (model access-unit removal-time)
  "decoded AUのPool投入、VBI更新、presentation予約を検証する。"
  (let ((pool (tstd-model-buffer-pool model))
        (show-existing-p
          (tstd-access-unit-show-existing-frame-p access-unit))
        (show-frame-p
          (tstd-access-unit-show-frame-p access-unit))
        (presentation-time
          (tstd-timestamp-time
           (tstd-model-clock model)
           (tstd-access-unit-pts access-unit)))
        (display-index nil))
    (release-tstd-buffer-pool-presentations
     pool removal-time)
    (if show-existing-p
        (let* ((map-index
                 (or
                  (tstd-access-unit-frame-to-show-map-index
                   access-unit)
                  (bridge-error
                   "TSTD_BUFFER_POOL_SHOW_EXISTING_INDEX_MISSING")))
               (frame-type
                 (or
                  (tstd-access-unit-frame-type access-unit)
                  (bridge-error
                   "TSTD_BUFFER_POOL_FRAME_TYPE_MISSING")))
               (refresh-frame-flags
                 (or
                  (tstd-access-unit-refresh-frame-flags
                   access-unit)
                  (bridge-error
                   "TSTD_BUFFER_POOL_METADATA_MISSING")))
               (expected-refresh-frame-flags
                 (if (zerop frame-type) #xff 0)))
          (unless (= refresh-frame-flags
                     expected-refresh-frame-flags)
            (bridge-error
             "TSTD_BUFFER_POOL_SHOW_EXISTING_REFRESH_INVALID frame_type=~D flags=~D expected=~D"
             frame-type refresh-frame-flags
             expected-refresh-frame-flags))
          (setf display-index
                (aref
                 (tstd-buffer-pool-virtual-buffer-indices pool)
                 map-index))
          (when (minusp display-index)
            (bridge-error
             "TSTD_BUFFER_POOL_SHOW_EXISTING_EMPTY slot=~D"
             map-index))
          ;; Annex E decode_process: delayed keyをshow_existingで表示した
          ;; 場合は、その既存bufferを8個すべてのVBIへ再登録する。
          (when (plusp refresh-frame-flags)
            (update-tstd-buffer-pool-reference-slots
             pool display-index refresh-frame-flags)))
        (let ((refresh-frame-flags
                (or
                 (tstd-access-unit-refresh-frame-flags
                  access-unit)
                 (bridge-error
                  "TSTD_BUFFER_POOL_METADATA_MISSING")))
              (free-index
                (or
                 (tstd-buffer-pool-free-index pool)
                 (bridge-error
                  "TSTD_BUFFER_POOL_OVERFLOW capacity=~D"
                  +av1-buffer-pool-size+))))
          (setf display-index free-index)
          (incf
           (tstd-buffer-pool-decoded-access-unit-count pool))
          (update-tstd-buffer-pool-reference-slots
           pool free-index refresh-frame-flags)))
    (when (or show-existing-p show-frame-p)
      (when (< presentation-time removal-time)
        ;; FFmpeg live AV1 では PTS が clamp 後の removal より前になることがある。
        ;; 許容幅内は presentation を removal に揃え、それ以上だけ落とす。
        (let ((behind (- removal-time presentation-time)))
          (when (> behind +tstd-maximum-removal-before-arrival-seconds+)
            (bridge-error
             "TSTD_BUFFER_POOL_PRESENTATION_BEFORE_DECODE presentation=~A removal=~A"
             presentation-time removal-time))
          (setf presentation-time removal-time)))
      (setf
       (aref (tstd-buffer-pool-presentation-times pool)
             display-index)
       presentation-time)
      (incf
       (aref (tstd-buffer-pool-player-reference-counts pool)
             display-index)))
    pool))

(defun update-tstd-access-unit-frame-metadata
    (model dts semantics refresh-frame-flags)
  "逐次出力後に完全検証できたframe metadataを登録済みAUへ確定する。"
  (let ((access-unit
          (or
           (let ((current
                   (tstd-model-current-access-unit model)))
             (when (and current
                        (= (tstd-access-unit-dts current) dts)
                        (null
                         (tstd-access-unit-refresh-frame-flags
                          current)))
               current))
           (find-if
            (lambda (candidate)
              (and
               (= (tstd-access-unit-dts candidate) dts)
               (null
                (tstd-access-unit-refresh-frame-flags
                 candidate))))
            (tstd-model-pending-access-units model)))))
    (unless access-unit
      (bridge-error
       "TSTD_BUFFER_POOL_ACCESS_UNIT_METADATA_TARGET_MISSING dts=~D"
       dts))
    (unless
        (and
         (eql
          (tstd-access-unit-show-existing-frame-p access-unit)
          (av1-frame-semantics-show-existing-frame-p semantics))
         (eql
          (tstd-access-unit-show-frame-p access-unit)
          (av1-frame-semantics-show-frame-p semantics))
         (eql
          (tstd-access-unit-frame-to-show-map-index access-unit)
          (av1-frame-semantics-frame-to-show-map-index semantics)))
      (bridge-error
       "TSTD_BUFFER_POOL_ACCESS_UNIT_METADATA_CHANGED dts=~D"
       dts))
    (setf
     (tstd-access-unit-frame-type access-unit)
     (av1-frame-semantics-frame-type semantics)
     (tstd-access-unit-refresh-frame-flags access-unit)
     refresh-frame-flags)
    access-unit))

(defun finish-tstd-access-unit (model)
  "現在AUのMB/EB deadlineとbuffer状態を確定する。"
  (let ((access-unit
          (tstd-model-current-access-unit model)))
    (unless access-unit
      (return-from finish-tstd-access-unit nil))
    (finish-tstd-pes-classifier
     (tstd-model-classifier model))
    (let* ((es (tstd-access-unit-es access-unit))
           (departure-ranges
             (tstd-access-unit-mb-departure-ranges access-unit))
           (decoder-ranges (av1-decoder-byte-ranges es))
           (decoded-count
             (loop for (start . end) in decoder-ranges
                   sum (- end start)))
           (first-arrival
             (or
              (tstd-access-unit-transport-first-arrival access-unit)
              (bridge-error "TSTD_ACCESS_UNIT_ARRIVAL_MISSING")))
           (first-departure
             (or
              (tstd-access-unit-mb-first-departure access-unit)
              (bridge-error "TSTD_ACCESS_UNIT_MB_EMPTY")))
           (last-departure
             (or
              (tstd-access-unit-mb-last-departure access-unit)
              (bridge-error "TSTD_ACCESS_UNIT_MB_EMPTY")))
           (removal-time
             (tstd-timestamp-time
              (tstd-model-clock model)
              (tstd-access-unit-dts access-unit)))
           ;; FFmpeg live / HW AV1 は bitstream の low_delay_mode_flag が
           ;; 立たないことが多く、厳密 DTS 除去だと起動直後に破綻する。
           ;; MB 最終 departure 以降に除去する low-delay 相当を常に使う。
           (effective-removal-time
             (max
              removal-time
              (+ last-departure
                 (/ 1 +tstd-system-clock-rate+))))
           (delay (- effective-removal-time first-arrival)))
      (declare (ignore first-departure))
      (when (minusp delay)
        ;; それでも arrival より前なら、許容幅内は arrival 直後へ clamp する。
        (let ((behind (- first-arrival effective-removal-time)))
          (when (> behind +tstd-maximum-removal-before-arrival-seconds+)
            (bridge-error
             "TSTD_ACCESS_UNIT_REMOVAL_BEFORE_ARRIVAL delay=~A"
             delay))
          (setf effective-removal-time
                (+ first-arrival
                   (/ 1 +tstd-system-clock-rate+))
                delay
                (- effective-removal-time first-arrival))))
      (when (> delay +tstd-maximum-access-unit-delay+)
        (bridge-error
         "TSTD_ACCESS_UNIT_DELAY_EXCEEDED delay=~A limit=~A"
         delay +tstd-maximum-access-unit-delay+))
      (let ((capacity
              (tstd-access-unit-elementary-buffer-size
               access-unit)))
        (process-tstd-buffer-pool-access-unit
         model access-unit effective-removal-time)
        (insert-tstd-elementary-removal
         model effective-removal-time decoded-count)
        (let ((next-removal
                (first
                 (tstd-model-elementary-buffer-removals model))))
          (if (and next-removal
                   (<= (car next-removal)
                       last-departure))
              (process-tstd-elementary-departure-ranges
               model decoder-ranges departure-ranges capacity)
              (increase-tstd-elementary-fullness
               model decoded-count capacity)))))
    (setf (tstd-model-current-access-unit model) nil)
    access-unit))

(defun start-next-tstd-access-unit (model arrival)
  "登録済みFIFO先頭AUを現在のPUSIへ対応付ける。"
  (let ((access-unit
          (pop (tstd-model-pending-access-units model))))
    (unless access-unit
      (bridge-error "TSTD_ACCESS_UNIT_METADATA_MISSING"))
    (setf
     (tstd-model-current-access-unit model) access-unit
     (tstd-model-discarding-access-unit-p model) nil
     (tstd-model-rx-bytes-per-second model)
     (tstd-access-unit-rx-bytes-per-second access-unit)
     (tstd-model-multiplex-buffer-size model)
     (tstd-access-unit-multiplex-buffer-size access-unit)
     (tstd-access-unit-transport-first-arrival access-unit)
     arrival)
    (start-tstd-pes-classifier
     (tstd-model-classifier model))
    access-unit))

(defun reset-tstd-buffer-epoch (model)
  "discontinuityで旧epochのAUとTB/MB/EB状態を破棄する。"
  (finish-tstd-transport-busy-period model :discontinuity)
  (setf
   ;; current AUのmetadataは既にpending FIFOからpop済みである。
   ;; それだけを旧epochと共に破棄し、将来AUのmetadataは保持する。
   (tstd-model-current-access-unit model) nil
   (tstd-model-discarding-access-unit-p model) t
   (tstd-model-transport-buffer-fullness model) 0
   (tstd-model-transport-buffer-last-arrival model) nil
   (tstd-model-transport-buffer-service-end model) nil
   (tstd-model-transport-buffer-busy-start model) nil
   (tstd-model-transport-buffer-last-empty model) nil
   (tstd-model-multiplex-buffer-fullness model) 0
   (tstd-model-multiplex-buffer-last-arrival model) nil
   (tstd-model-multiplex-buffer-service-end model) nil
   (tstd-model-multiplex-buffer-pending-header-bytes model) 0
   (tstd-model-multiplex-buffer-header-removal-time model) nil
   (tstd-model-elementary-buffer-fullness model) 0
   (tstd-model-elementary-buffer-removals model) '())
  (discard-tstd-pes-classifier
   (tstd-model-classifier model))
  (let ((next
          (first (tstd-model-pending-access-units model))))
    (when next
      (setf
       (tstd-model-rx-bytes-per-second model)
       (tstd-access-unit-rx-bytes-per-second next)
       (tstd-model-multiplex-buffer-size model)
       (tstd-access-unit-multiplex-buffer-size next))))
  (reset-tstd-arrival-clock-epoch
   (tstd-model-clock model))
  (reset-tstd-buffer-pool
   (tstd-model-buffer-pool model))
  model)

(defun process-tstd-output-packet (model packet)
  "最終出力順のPACKETをAV1 T-STDへ1個投入する。"
  (let* ((packet-index (tstd-model-packet-index model))
         (arrival
           (tstd-packet-arrival-time
            (tstd-model-clock model)
            packet-index))
         (pid (ts-pid packet))
         (video-pid (tstd-model-video-pid model))
         (pcr-pid (tstd-model-pcr-pid model))
         (video-p (and video-pid (= pid video-pid)))
         (pcr-p (and pcr-pid (= pid pcr-pid)))
         (start-p
           (and video-p
                (ts-payload-unit-start-p packet)))
         (discontinuity-p
           (and (or video-p pcr-p)
                (ts-discontinuity-indicator-p packet))))
    (when discontinuity-p
      (reset-tstd-buffer-epoch model))
    (when pcr-p
      (let ((pcr (ts-pcr packet)))
        (when pcr
          (observe-tstd-pcr
           (tstd-model-clock model)
           packet-index pcr))))
    (when video-p
      (when start-p
        (when (tstd-model-current-access-unit model)
          (finish-tstd-access-unit model))
        (start-next-tstd-access-unit model arrival))
      (let ((access-unit
              (tstd-model-current-access-unit model))
            (tb-departure-ranges
              (process-tstd-transport-packet
               model arrival)))
        (cond
          (access-unit
           (let ((ranges
                    (classify-tstd-video-packet-byte-ranges
                     (tstd-model-classifier model)
                     packet)))
              (dolist (range ranges)
                (loop for departure-range across
                        tb-departure-ranges
                      for start =
                        (max
                         (tstd-pes-byte-range-start range)
                         (tstd-departure-range-start
                          departure-range))
                      for end =
                        (min
                         (tstd-pes-byte-range-end range)
                         (tstd-departure-range-end
                          departure-range))
                      when (< start end)
                        do
                  (let ((first-departure
                          (tstd-departure-range-time
                           departure-range start))
                        (source-interval
                          (tstd-departure-range-interval
                           departure-range)))
                    (ecase
                        (tstd-pes-byte-range-kind range)
                      (:header
                       (process-tstd-multiplex-header-source-range
                        model start end first-departure
                        source-interval))
                      (:payload
                       (process-tstd-multiplex-source-range
                        model access-unit packet start end
                        first-departure source-interval))))))))
          ((and
            (ts-payload-offset packet)
            (not (tstd-model-discarding-access-unit-p model)))
           (bridge-error
            "TSTD_AV1_PES_CONTINUATION_WITHOUT_START")))))
    (incf (tstd-model-packet-index model))
    packet))

(defun finish-tstd-multiplex-buffer (model)
  "EOFで予約済みMB transferとPES header除去を最終空化する。"
  (let ((service-end
          (tstd-model-multiplex-buffer-service-end model)))
    (when service-end
      (setf
       (tstd-model-multiplex-buffer-fullness model)
       (tstd-multiplex-fullness-at model service-end)))
    (when
        (or
         (plusp
          (tstd-model-multiplex-buffer-pending-header-bytes
           model))
         (tstd-model-multiplex-buffer-header-removal-time
          model))
      (bridge-error
       "TSTD_MB_PES_HEADER_NOT_REMOVED_AT_EOF count=~D"
       (tstd-model-multiplex-buffer-pending-header-bytes
        model)))
    (unless
        (zerop (tstd-model-multiplex-buffer-fullness model))
      (bridge-error
       "TSTD_MB_NOT_EMPTY_AT_EOF fullness=~A"
       (tstd-model-multiplex-buffer-fullness model)))
    (setf
     (tstd-model-multiplex-buffer-last-arrival model)
     service-end
     (tstd-model-multiplex-buffer-service-end model)
     nil))
  model)

(defun finish-tstd-model (model)
  "EOFで最後のAUと全T-STD bufferを検証する。"
  (when (tstd-model-current-access-unit model)
    (finish-tstd-access-unit model))
  (when (tstd-model-pending-access-units model)
    (bridge-error
     "TSTD_ACCESS_UNIT_NOT_EMITTED count=~D"
     (length (tstd-model-pending-access-units model))))
  (finish-tstd-transport-busy-period model :eof)
  (finish-tstd-multiplex-buffer model)
  (loop
    while (tstd-model-elementary-buffer-removals model)
    for removal =
      (pop (tstd-model-elementary-buffer-removals model))
    do (apply-tstd-elementary-removal model removal))
  (unless (zerop
           (tstd-model-elementary-buffer-fullness model))
    (bridge-error
     "TSTD_EB_NOT_EMPTY_AT_EOF fullness=~D"
     (tstd-model-elementary-buffer-fullness model)))
  (release-tstd-buffer-pool-presentations
   (tstd-model-buffer-pool model) 0 :all t)
  model)
