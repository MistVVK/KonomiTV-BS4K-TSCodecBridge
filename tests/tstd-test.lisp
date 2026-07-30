;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defun make-tstd-test-configuration
    (&key (profile 0) (level 0) (tier 0) low-delay-mode-p)
  "T-STD単体試験用の最小AV1 configurationを作る。"
  (make-av1-codec-configuration
   :profile profile
   :level level
   :tier tier
   :low-delay-mode-p low-delay-mode-p))

(define-bridge-test tstd-pcr-clock-uses-first-byte-anchor
  (let* ((clock (make-tstd-arrival-clock 2200))
         (packet-index 10)
         (pcr (* 90000 300))
         (anchor
           (tstd-byte-arrival-time
            clock
            (tstd-pcr-anchor-byte-index packet-index))))
    (observe-tstd-pcr clock packet-index pcr)
    (check-bridge-test
     (= (tstd-timestamp-time clock 93600)
        (+ anchor 1/25)))))

(define-bridge-test tstd-pcr-cbr-residual-boundary
  (let ((within (make-tstd-arrival-clock 1504))
        (outside (make-tstd-arrival-clock 1504))
        (pcr 27000000))
    (observe-tstd-pcr within 0 pcr)
    (observe-tstd-pcr within 1 (+ pcr 27000 13))
    (observe-tstd-pcr outside 0 pcr)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (observe-tstd-pcr
         outside 1 (+ pcr 27000 14)))))))

(define-bridge-test tstd-tb-must-empty-once-per-second
  (let ((boundary (make-tstd-model 1504))
        (continuous (make-tstd-model 1504)))
    (setf
     (tstd-model-rx-bytes-per-second boundary)
     +ts-packet-size+
     (tstd-model-rx-bytes-per-second continuous)
     (/ (* +ts-packet-size+ 5) 3))
    (process-tstd-transport-packet boundary 0)
    (finish-tstd-transport-busy-period boundary :eof)
    (check-bridge-test
     (= (tstd-model-transport-buffer-last-empty boundary)
        1))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        ;; 各packet自身のdelayは0.6秒と0.7秒だが、TBは1.2秒
        ;; 連続して非空になるため二つ目のarrivalでfail closedにする。
        (process-tstd-transport-packet continuous 0)
        (process-tstd-transport-packet continuous 1/2))))))

(define-bridge-test tstd-tb-final-boundaries-fail-closed
  (dolist (boundary '(:eof :discontinuity))
    (let ((model (make-tstd-model 1504)))
      (setf
       (tstd-model-transport-buffer-fullness model) 1
       (tstd-model-transport-buffer-busy-start model) 0
       (tstd-model-transport-buffer-service-end model) 1001/1000)
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (finish-tstd-transport-busy-period
           model boundary)))))))

(define-bridge-test tstd-pes-header-enters-and-leaves-multiplex-buffer
  (let ((model (make-tstd-model 1504))
        (access-unit
          (make-tstd-access-unit
           :multiplex-buffer-size 100
           :elementary-buffer-size 100))
        (packet (octets 10 11)))
    (setf
     (tstd-model-rx-bytes-per-second model) 2
     (tstd-model-multiplex-buffer-size model) 100)
    (process-tstd-multiplex-header-range
     model 0 3 0)
    (check-bridge-test
     (= (tstd-model-multiplex-buffer-fullness model) 3))
    (check-bridge-test
     (= (tstd-model-multiplex-buffer-pending-header-bytes model)
        3))
    (process-tstd-multiplex-range
     model access-unit packet 0 2 3/2)
    (check-bridge-test
     (= (tstd-model-multiplex-buffer-fullness model) 1))
    (check-bridge-test
     (zerop
      (tstd-model-multiplex-buffer-pending-header-bytes model)))
    (check-bridge-test
     (null
      (tstd-model-multiplex-buffer-header-removal-time model)))
    (finish-tstd-multiplex-buffer model)
    (check-bridge-test
     (zerop (tstd-model-multiplex-buffer-fullness model)))))

(define-bridge-test tstd-pes-header-multiplex-capacity-boundary
  (let ((within (make-tstd-model 1504))
        (outside (make-tstd-model 1504)))
    (setf
     (tstd-model-rx-bytes-per-second within) 100
     (tstd-model-rx-bytes-per-second outside) 100
     (tstd-model-multiplex-buffer-size within) 9
     (tstd-model-multiplex-buffer-size outside) 8)
    (process-tstd-multiplex-header-range within 0 9 0)
    (check-bridge-test
     (= (tstd-model-multiplex-buffer-fullness within) 9))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (process-tstd-multiplex-header-range
         outside 0 9 0))))))

(define-bridge-test tstd-pes-header-arrival-keeps-prior-payload-service
  (let ((within (make-tstd-model 1504))
        (outside (make-tstd-model 1504)))
    (dolist (model (list within outside))
      (setf
       (tstd-model-rx-bytes-per-second model) 2
       (tstd-model-multiplex-buffer-fullness model) 5
       (tstd-model-multiplex-buffer-last-arrival model) 1
       (tstd-model-multiplex-buffer-service-end model) 3))
    (setf
     (tstd-model-multiplex-buffer-size within) 5
     (tstd-model-multiplex-buffer-size outside) 4)
    (process-tstd-multiplex-header-range
     within 0 4 3/2)
    (check-bridge-test
     (= (tstd-model-multiplex-buffer-fullness within) 5))
    (check-bridge-test
     (= (tstd-model-multiplex-buffer-pending-header-bytes within)
        4))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (process-tstd-multiplex-header-range
         outside 0 4 3/2))))))

(define-bridge-test tstd-pes-classifier-preserves-header-range
  (let* ((pes
           (make-pes
            #xbd (octets 0 0 1 #x08)
            90000 :data-alignment t))
         (packet
           (make-ts-packet
            +test-video-pid+ 0 pes
            :payload-unit-start t))
         (classifier (make-tstd-pes-classifier))
         (payload-offset (ts-payload-offset packet))
         (pes-payload-offset
           (pes-header-payload-offset
            (parse-pes-header pes))))
    (start-tstd-pes-classifier classifier)
    (let ((ranges
            (classify-tstd-video-packet-byte-ranges
             classifier packet)))
      (check-bridge-test (= (length ranges) 2))
      (let ((header (first ranges))
            (payload (second ranges)))
        (check-bridge-test
         (eq (tstd-pes-byte-range-kind header) :header))
        (check-bridge-test
         (= (tstd-pes-byte-range-start header)
            payload-offset))
        (check-bridge-test
         (= (tstd-pes-byte-range-end header)
            (+ payload-offset pes-payload-offset)))
        (check-bridge-test
         (eq (tstd-pes-byte-range-kind payload) :payload))
        (check-bridge-test
         (= (-
             (tstd-pes-byte-range-end payload)
             (tstd-pes-byte-range-start payload))
            4))))))

(define-bridge-test tstd-multiplex-range-matches-byte-recurrence
  (let ((model (make-tstd-model 1504))
        (access-unit
          (make-tstd-access-unit
           :multiplex-buffer-size 100
           :elementary-buffer-size 100))
        (packet (octets 10 11 12 13 14 15)))
    (setf
     (tstd-model-rx-bytes-per-second model) 2
     (tstd-model-multiplex-buffer-size model) 100
     (tstd-model-multiplex-buffer-fullness model) 5
     (tstd-model-multiplex-buffer-last-arrival model) 1
     (tstd-model-multiplex-buffer-service-end model) 2)
    (process-tstd-multiplex-range
     model access-unit packet 1 6 3/2)
    (check-bridge-test
     (= (tstd-model-multiplex-buffer-fullness model) 5))
    (check-bridge-test
     (= (tstd-model-multiplex-buffer-last-arrival model) 7/2))
    (check-bridge-test
     (= (tstd-model-multiplex-buffer-service-end model) 9/2))
    (check-bridge-test
     (= (tstd-access-unit-mb-first-departure access-unit) 5/2))
    (check-bridge-test
     (= (tstd-access-unit-mb-last-departure access-unit) 9/2))
    (check-bridge-test
     (equalp
      (tstd-access-unit-es access-unit)
      (octets 11 12 13 14 15)))
    (let ((range
            (aref
             (tstd-access-unit-mb-departure-ranges access-unit)
             0)))
      (check-bridge-test
       (and
        (= (tstd-departure-range-start range) 0)
        (= (tstd-departure-range-end range) 5)
        (= (tstd-departure-range-time range 0) 5/2)
        (= (tstd-departure-range-time range 4) 9/2))))))

(define-bridge-test tstd-same-time-removal-precedes-arrival
  (let ((ordered (make-tstd-model 1504))
        (underflow (make-tstd-model 1504)))
    (setf
     (tstd-model-elementary-buffer-fullness ordered) 1
     (tstd-model-elementary-buffer-removals ordered)
     (list (cons 1 1))
     (tstd-model-elementary-buffer-removals underflow)
     (list (cons 1 1)))
    (process-tstd-elementary-byte ordered 1 10)
    (check-bridge-test
     (= (tstd-model-elementary-buffer-fullness ordered)
        1))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (process-tstd-elementary-byte
         underflow 1 10))))))

(define-bridge-test tstd-elementary-range-keeps-removal-order
  (let ((ordered (make-tstd-model 1504))
        (underflow (make-tstd-model 1504))
        (departures (vector 1 2 3)))
    (setf
     (tstd-model-elementary-buffer-fullness ordered) 1
     (tstd-model-elementary-buffer-removals ordered)
     (list (cons 2 2))
     (tstd-model-elementary-buffer-removals underflow)
     (list (cons 2 2)))
    (process-tstd-elementary-range
     ordered departures 0 3 10)
    (check-bridge-test
     (= (tstd-model-elementary-buffer-fullness ordered)
        2))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (process-tstd-elementary-range
         underflow departures 0 3 10))))))

(define-bridge-test tstd-departure-range-keeps-removal-order
  (let ((ordered (make-tstd-model 1504))
        (underflow (make-tstd-model 1504))
        (range
          (make-tstd-departure-range 0 3 1 1)))
    (setf
     (tstd-model-elementary-buffer-fullness ordered) 1
     (tstd-model-elementary-buffer-removals ordered)
     (list (cons 2 2))
     (tstd-model-elementary-buffer-removals underflow)
     (list (cons 2 2)))
    (process-tstd-elementary-departure-range
     ordered range 0 3 10)
    (check-bridge-test
     (= (tstd-model-elementary-buffer-fullness ordered)
        2))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (process-tstd-elementary-departure-range
         underflow range 0 3 10))))))

(define-bridge-test tstd-discontinuity-discards-prior-epoch-buffers
  (let ((model (make-tstd-model 1504)))
    (setf
     (tstd-model-transport-buffer-fullness model) 188
     (tstd-model-transport-buffer-busy-start model) 0
     (tstd-model-transport-buffer-service-end model) 1/2
     (tstd-model-multiplex-buffer-fullness model) 12
     (tstd-model-elementary-buffer-fullness model) 9
     (tstd-model-elementary-buffer-removals model)
     (list (cons 100 9)))
    (reset-tstd-buffer-epoch model)
    (check-bridge-test
     (zerop
      (tstd-model-transport-buffer-fullness model)))
    (check-bridge-test
     (zerop
      (tstd-model-multiplex-buffer-fullness model)))
    (check-bridge-test
     (zerop
      (tstd-model-elementary-buffer-fullness model)))
    (check-bridge-test
     (null
      (tstd-model-elementary-buffer-removals model)))))

(define-bridge-test tstd-pcr-discontinuity-discards-partial-au
  (let ((model (make-tstd-model 1504))
         (old
           (make-tstd-access-unit
            :rx-bytes-per-second 1504
            :multiplex-buffer-size 1000
            :elementary-buffer-size 1000))
         (next
           (make-tstd-access-unit
            :dts 90000
            :rx-bytes-per-second 1504
            :multiplex-buffer-size 1000
            :elementary-buffer-size 1000))
         (pcr
           (make-test-adaptation-only-packet
            +test-data-pid+ 0
            :discontinuity t)))
    (setf
     (tstd-model-video-pid model) +test-video-pid+
     (tstd-model-pcr-pid model) +test-data-pid+
     (tstd-model-current-access-unit model) old
     (tstd-model-pending-access-units model) (list next)
     (tstd-model-transport-buffer-fullness model) 188
     (tstd-model-transport-buffer-busy-start model) 0
     (tstd-model-transport-buffer-service-end model) 1/8
     (tstd-model-multiplex-buffer-fullness model) 12
     (tstd-model-elementary-buffer-fullness model) 9)
    (start-tstd-pes-classifier
     (tstd-model-classifier model))
    (set-test-pcr pcr 0)
    (process-tstd-output-packet model pcr)
    (check-bridge-test
     (null (tstd-model-current-access-unit model)))
    (check-bridge-test
     (not
      (tstd-pes-classifier-active-p
       (tstd-model-classifier model))))
    (check-bridge-test
     (equal
      (tstd-model-pending-access-units model)
      (list next)))
    (check-bridge-test
     (tstd-model-discarding-access-unit-p model))
    ;; 専用PCR PIDの境界後、旧PESのcontinuationは次PUSIまで無視する。
    (process-tstd-output-packet
     model
     (make-ts-packet
      +test-video-pid+ 1 (octets 1 2 3)))
    (check-bridge-test
     (= (tstd-model-transport-buffer-fullness model)
        +ts-packet-size+))
    (process-tstd-output-packet
     model
     (make-ts-packet
      +test-video-pid+ 2
      (make-pes #xbd (octets 0 0 1 #x08) 90000)
      :payload-unit-start t))
    (check-bridge-test
     (eq (tstd-model-current-access-unit model) next))
    (check-bridge-test
     (not
      (tstd-model-discarding-access-unit-p model)))))

(define-bridge-test tstd-av1-decoder-byte-classification
  (check-bridge-test
   (= (count-av1-decoder-bytes
       (octets 0 0 1 #x08 0 0 3 1 2))
      5))
  (check-bridge-test
   (signals-bridge-error-p
    (lambda ()
      (count-av1-decoder-bytes
       (octets 0 0 1 #x08 0 0 2))))))

(define-bridge-test tstd-av1-decoder-byte-adjustable-vector
  (let ((access-unit
          (make-array
           32
           :element-type 'octet
           :adjustable t
           :fill-pointer 0))
        (indices '()))
    (loop for byte across
            (octets
             0 0 1 #x08 0 0 3 1 2
             0 0 1 #x10)
          do (vector-push-extend byte access-unit))
    (map-av1-decoder-byte-indices
     (lambda (position)
       (push position indices))
     access-unit)
    (check-bridge-test
     (equal
      (nreverse indices)
      '(3 4 5 7 8 12)))))

(define-bridge-test tstd-av1-level-buffer-formulas
  (let ((configuration
          (make-tstd-test-configuration)))
    (check-bridge-test
     (= (av1-tstd-bitrate-bps configuration)
        1500000))
    (check-bridge-test
     (= (av1-tstd-rx-bytes-per-second configuration)
        206250))
    (check-bridge-test
     (= (av1-tstd-elementary-buffer-size configuration)
        187500))
    (check-bridge-test
     (= (av1-tstd-multiplex-buffer-size configuration)
        1118750)))
  (check-bridge-test
   (signals-bridge-error-p
    (lambda ()
      (av1-tstd-bitrate-bps
       (make-tstd-test-configuration :level 31)))))
  (check-bridge-test
   (signals-bridge-error-p
    (lambda ()
      (av1-tstd-bitrate-bps
      (make-tstd-test-configuration
        :level 0 :tier 1))))))

(define-bridge-test tstd-buffer-pool-capacity-is-ten-decoded-frames
  (let* ((model (make-tstd-model 1504))
         (clock (tstd-model-clock model)))
    (observe-tstd-pcr clock 0 0)
    (let ((removal-time
            (tstd-timestamp-time clock 0)))
      (dotimes (index +av1-buffer-pool-size+)
        (process-tstd-buffer-pool-access-unit
         model
         (make-tstd-access-unit
          :pts 90000
          :dts 0
          :show-frame-p t
          :refresh-frame-flags 0)
         removal-time))
      (check-bridge-test
       (= (tstd-buffer-pool-decoded-access-unit-count
           (tstd-model-buffer-pool model))
          +av1-buffer-pool-size+))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (process-tstd-buffer-pool-access-unit
           model
           (make-tstd-access-unit
            :pts 90000
            :dts 0
            :show-frame-p t
            :refresh-frame-flags 0)
           removal-time)))))))

(define-bridge-test tstd-buffer-pool-presentation-releases-slot
  (let* ((model (make-tstd-model 1504))
         (clock (tstd-model-clock model))
         (first
           (make-tstd-access-unit
            :pts 90000
            :dts 0
            :show-frame-p t
            :refresh-frame-flags 1))
         (second
           (make-tstd-access-unit
            :pts 270000
            :dts 180000
            :show-frame-p t
            :refresh-frame-flags 1)))
    (observe-tstd-pcr clock 0 0)
    (process-tstd-buffer-pool-access-unit
     model first (tstd-timestamp-time clock 0))
    (let* ((pool (tstd-model-buffer-pool model))
           (first-index
             (aref
              (tstd-buffer-pool-virtual-buffer-indices pool)
              0)))
      (check-bridge-test
       (plusp
        (aref
         (tstd-buffer-pool-player-reference-counts pool)
         first-index)))
      (process-tstd-buffer-pool-access-unit
       model second (tstd-timestamp-time clock 180000))
      (check-bridge-test
       (= (tstd-buffer-pool-presentation-removal-count pool)
          1))
      (check-bridge-test
       (zerop
        (aref
         (tstd-buffer-pool-decoder-reference-counts pool)
         first-index)))
      (check-bridge-test
       (= (tstd-buffer-pool-decoded-access-unit-count pool)
          2)))))

(define-bridge-test tstd-buffer-pool-show-existing-key-refreshes-all-slots
  (let* ((model (make-tstd-model 1504))
         (clock (tstd-model-clock model))
         (pool (tstd-model-buffer-pool model)))
    (observe-tstd-pcr clock 0 0)
    (process-tstd-buffer-pool-access-unit
     model
     (make-tstd-access-unit
      :pts 0
      :dts 0
      :frame-type 0
      :show-frame-p nil
      :refresh-frame-flags 1)
     (tstd-timestamp-time clock 0))
    (let ((key-index
            (aref
             (tstd-buffer-pool-virtual-buffer-indices pool)
             0)))
      (process-tstd-buffer-pool-access-unit
       model
       (make-tstd-access-unit
        :pts 90000
        :dts 90000
        :show-existing-frame-p t
        :frame-to-show-map-index 0
        :frame-type 0
        :show-frame-p nil
        :refresh-frame-flags #xff)
       (tstd-timestamp-time clock 90000))
      (dotimes (slot +av1-virtual-buffer-index-count+)
        (check-bridge-test
         (= (aref
             (tstd-buffer-pool-virtual-buffer-indices pool)
             slot)
            key-index)))
      (check-bridge-test
       (= (aref
           (tstd-buffer-pool-decoder-reference-counts pool)
           key-index)
          +av1-virtual-buffer-index-count+))
      (check-bridge-test
       (= (aref
           (tstd-buffer-pool-player-reference-counts pool)
           key-index)
          1))
      (check-bridge-test
       (= (tstd-buffer-pool-decoded-access-unit-count pool)
          1)))))

(define-bridge-test tstd-buffer-pool-metadata-and-presentation-fail-closed
  (let* ((model (make-tstd-model 1504))
         (clock (tstd-model-clock model)))
    (observe-tstd-pcr clock 0 0)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (process-tstd-buffer-pool-access-unit
         model
         (make-tstd-access-unit
          :pts 0
          :dts 90000
          :show-frame-p t)
         (tstd-timestamp-time clock 90000)))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (process-tstd-buffer-pool-access-unit
         model
         (make-tstd-access-unit
          :pts 0
          :dts 90000
          :show-frame-p t
          :refresh-frame-flags 0)
         (tstd-timestamp-time clock 90000)))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (process-tstd-buffer-pool-access-unit
         model
         (make-tstd-access-unit
          :pts 90000
          :dts 90000
          :show-existing-frame-p t
          :frame-to-show-map-index 0
          :frame-type 1
          :refresh-frame-flags #xff)
         (tstd-timestamp-time clock 90000)))))))

(define-bridge-test tstd-low-delay-buffer-pool-uses-scheduled-removal
  (let* ((model (make-tstd-model 1504))
         (clock (tstd-model-clock model))
         (access-unit
           (make-tstd-access-unit
            :pts 45000
            :dts 0
            :low-delay-mode-p t
            :show-frame-p t
            :refresh-frame-flags 0
            :rx-bytes-per-second 206250
            :multiplex-buffer-size 1118750
            :elementary-buffer-size 187500
            :transport-first-arrival 0
            :mb-first-departure 1
            :mb-last-departure 1)))
    (observe-tstd-pcr clock 0 0)
    (map nil
         (lambda (byte)
           (vector-push-extend
            byte
            (tstd-access-unit-es access-unit)))
         (octets 0 0 1 #x08))
    (vector-push-extend
     (make-tstd-departure-range 0 4 1 0)
     (tstd-access-unit-mb-departure-ranges access-unit))
    (setf (tstd-model-current-access-unit model)
          access-unit)
    ;; low_delay時のscheduled removalはMB最終departureより後になる。
    ;; PTSがraw DTSより後でもscheduled removalより前ならfail closedにする。
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (finish-tstd-access-unit model))))))

(define-bridge-test tstd-low-delay-effective-removal-obeys-delay-limit
  (let* ((model (make-tstd-model 1504))
         (clock (tstd-model-clock model)))
    (observe-tstd-pcr clock 0 0)
    (let* ((anchor-time
             (tstd-timestamp-time clock 0))
           (access-unit
             (make-tstd-access-unit
              :pts 1080000
              :dts 90000
              :low-delay-mode-p t
              :show-frame-p t
              :refresh-frame-flags 0
              :rx-bytes-per-second 206250
              :multiplex-buffer-size 1118750
              :elementary-buffer-size 187500
              :transport-first-arrival anchor-time
              :mb-first-departure (+ anchor-time 11)
              :mb-last-departure (+ anchor-time 11))))
      (map nil
           (lambda (byte)
             (vector-push-extend
              byte
              (tstd-access-unit-es access-unit)))
           (octets 0 0 1 #x08))
      (vector-push-extend
       (make-tstd-departure-range
        0 4 (+ anchor-time 11) 0)
       (tstd-access-unit-mb-departure-ranges access-unit))
      (setf (tstd-model-current-access-unit model)
            access-unit)
      ;; raw DTSのdelayは1秒だが、low_delayの実除去時刻は10秒制約を超える。
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (finish-tstd-access-unit model)))))))

(define-bridge-test tstd-cli-rate-contract-is-av1-only
  (check-bridge-test
   (signals-bridge-error-p
    (lambda ()
      (parse-command-line
       '("--video-codec" "av1")))))
  (check-bridge-test
   (signals-bridge-error-p
    (lambda ()
      (parse-command-line
       '("--video-codec" "vp9"
         "--transport-rate-kbps" "2200")))))
  (check-bridge-test
   (signals-bridge-error-p
    (lambda ()
      (parse-command-line
       '("--video-codec" "av1"
         "--transport-rate-kbps" "0")))))
  (let ((options
          (parse-command-line
           '("--video-codec" "av1"
             "--transport-rate-kbps" "2200"))))
    (check-bridge-test
     (= (bridge-options-transport-rate-kbps options)
        2200))))

(define-bridge-test fixed-packet-allocator-consumes-null-slots
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor
            output :av1 :aac
            :transport-rate-kbps
            +test-transport-rate-kbps+))
         (source
           (make-ts-packet
            #x101 0 (octets 1)))
         (first
           (make-ts-packet
            #x101 0 (octets 2)))
         (second
           (make-ts-packet
            #x101 1 (octets 3)))
         (null-packet
           (make-output-null-packet)))
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet source
      :resolved-p t
      :use-original-p nil
      :replacements (list first second)))
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet null-packet
      :resolved-p t
      :use-original-p t))
    (let ((packets
            (octets-to-packet-list
             (collected-octets output))))
      (check-bridge-test (= (length packets) 2))
      (check-bridge-test (equalp (first packets) first))
      (check-bridge-test (equalp (second packets) second))
      (check-bridge-test
       (zerop
        (bridge-processor-output-packet-count
         processor))))))

(define-bridge-test fixed-packet-allocator-keeps-pcr-slot
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor
            output :av1 :aac
            :transport-rate-kbps
            +test-transport-rate-kbps+))
         (source
           (make-ts-packet
            #x101 0 (octets 1)))
         (primary
           (make-ts-packet
            #x101 0 (octets 2)))
         (extra
           (make-ts-packet
            #x101 1 (octets 3)))
         (pcr
           (make-test-program-pcr-packet
            90000 :counter 2))
         (null-packet (make-output-null-packet)))
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet source
      :resolved-p t
      :use-original-p nil
      :replacements (list primary extra)))
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet pcr
      :resolved-p t
      :use-original-p t))
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet null-packet
      :resolved-p t
      :use-original-p t))
    (let ((packets
            (octets-to-packet-list
             (collected-octets output))))
      (check-bridge-test (= (length packets) 3))
      (check-bridge-test (equalp (first packets) primary))
      (check-bridge-test (equalp (second packets) pcr))
      (check-bridge-test (equalp (third packets) extra)))))

(define-bridge-test fixed-packet-allocator-enforces-two-ms-deadline
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor
            output :av1 :aac
            :transport-rate-kbps
            +test-transport-rate-kbps+))
         (source
           (make-ts-packet
            #x101 0 (octets 1)))
         (ordinary
           (make-ts-packet
            #x120 0 (octets 9))))
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet source
      :resolved-p t
      :use-original-p nil
      :replacements
      (list
       (make-ts-packet #x101 0 (octets 2))
       (make-ts-packet #x101 1 (octets 3)))))
    (loop repeat 2
          do
      (allocate-output-entry
       processor
       (make-pending-entry
        :packet ordinary
        :resolved-p t
        :use-original-p t)))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (allocate-output-entry
         processor
         (make-pending-entry
          :packet ordinary
          :resolved-p t
          :use-original-p t)))))))

(define-bridge-test fixed-packet-allocator-uses-boundary-target-slot
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor
            output :av1 :aac
            :transport-rate-kbps 1504))
         (source
           (make-ts-packet #x101 0 (octets 1)))
         (primary
           (make-ts-packet #x101 0 (octets 2)))
         (extra
           (make-ts-packet #x101 1 (octets 3)))
         (ordinary
           (make-ts-packet #x120 0 (octets 9)))
         (null-packet
           (make-output-null-packet)))
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet source
      :resolved-p t
      :use-original-p nil
      :replacements (list primary extra)))
    ;; 1504 kbit/sでは1 packetが1ms。extraが最初に置けるslot 1から
    ;; slot 3先頭まではちょうど2msなので境界値を許可する。
    (loop repeat 2
          do
      (allocate-output-entry
       processor
       (make-pending-entry
        :packet ordinary
        :resolved-p t
        :use-original-p t)))
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet null-packet
      :resolved-p t
      :use-original-p t))
    (let ((packets
            (octets-to-packet-list
             (collected-octets output))))
      (check-bridge-test (= (length packets) 4))
      (check-bridge-test (equalp (first packets) primary))
      (check-bridge-test (equalp (second packets) ordinary))
      (check-bridge-test (equalp (third packets) ordinary))
      (check-bridge-test (equalp (fourth packets) extra)))))

(define-bridge-test fixed-packet-allocator-carries-through-target-slots
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor
            output :av1 :aac
            :transport-rate-kbps
            +test-transport-rate-kbps+))
         (first-source
           (make-ts-packet #x101 0 (octets 1)))
         (second-source
           (make-ts-packet #x101 1 (octets 2)))
         (first-primary
           (make-ts-packet #x101 0 (octets 3)))
         (first-extra
           (make-ts-packet #x101 1 (octets 4)))
         (second-primary
           (make-ts-packet #x101 2 (octets 5))))
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet first-source
      :resolved-p t
      :use-original-p nil
      :replacements (list first-primary first-extra)))
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet second-source
      :resolved-p t
      :use-original-p nil
      :replacements (list second-primary)))
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet (make-output-null-packet)
      :resolved-p t
      :use-original-p t))
    (let ((packets
            (octets-to-packet-list
             (collected-octets output))))
      (check-bridge-test
       (equalp packets
               (list first-primary first-extra second-primary)))
      (check-bridge-test
       (zerop
        (bridge-processor-output-packet-count processor))))))

(defun make-av1-byte-fifo-shape-template
    (index packet-count pcr-indices tail-payload-count)
  "通常AV1 AUのvideo target容量配置を模したtemplateを作る。"
  (let ((packet
          (cond
            ((zerop index)
             (make-ts-packet
              +test-video-pid+ 0
              (make-pattern-octets 176 index)
              :payload-unit-start t
              :random-access t
              :elementary-stream-priority t))
            ((member index pcr-indices :test #'=)
             (make-ts-packet
              +test-video-pid+ (logand index #x0f)
              (make-pattern-octets 176 index)))
            ((= index (- packet-count 1))
             (make-ts-packet
              +test-video-pid+ (logand index #x0f)
              (make-pattern-octets
               tail-payload-count index)))
            (t
             (make-ts-packet
              +test-video-pid+ (logand index #x0f)
              (make-pattern-octets 184 index))))))
    (when (or (zerop index)
              (member index pcr-indices :test #'=))
      (set-test-pcr
       packet
       (* (+ 90000 index) 300)))
    packet))

(defun run-av1-byte-fifo-shape-test
    (packet-count pcr-indices tail-payload-count)
  "PACKET-COUNT規模のAV1 +2 byte carryを固定target内で検証する。"
  (let* ((processor
           (make-bridge-processor
            (make-instance 'octet-collector-stream)
            :av1 :aac
            :transport-rate-kbps
            +test-transport-rate-kbps+))
         (assembler
           (%make-pes-assembler +test-video-pid+ :video))
         (templates
           (loop for index from 0 below packet-count
                 collect
                 (make-av1-byte-fifo-shape-template
                  index packet-count pcr-indices
                  tail-payload-count)))
         (chunks
           (loop for template in templates
                 for index from 0
                 for capacity =
                   (template-payload-capacity
                    template
                    (if (zerop index) #x60 0))
                 collect
                 (make-pattern-octets
                  (cond
                    ((zerop index) (+ capacity 2))
                    ((= index (- packet-count 1))
                     tail-payload-count)
                    (t capacity))
                  (+ 71 index))))
         (entries
           (loop for template in templates
                 for index from 0
                 collect
                 (make-pending-entry
                  :packet template
                  :slot-index index
                  :resolved-p nil
                  :use-original-p nil))))
    (setf
     (pes-assembler-active-p assembler) t
     (pes-assembler-streaming-p assembler) t
     (pes-assembler-stream-random-access-kind assembler) :key
     (pes-assembler-av1-stream-output-start-p assembler) t)
    (loop for entry in entries
          for chunk in chunks
          for index from 0
          do
      (append-av1-stream-byte-segment
       processor assembler chunk index)
      (resolve-av1-stream-byte-target-entry
       processor assembler entry
       (pending-entry-packet entry)))
    (let ((outputs
            (loop for entry in entries
                  append
                  (pending-entry-replacements entry))))
      (check-bridge-test (= (length outputs) packet-count))
      (check-bridge-test
       (every
        (lambda (entry)
          (= (length
              (pending-entry-replacements entry))
             1))
        entries))
      (check-bridge-test
       (equalp
        (apply #'concatenate-octets
               (mapcar
                (lambda (packet)
                  (subseq packet
                          (ts-payload-offset packet)))
                outputs))
        (apply #'concatenate-octets chunks)))
      (check-bridge-test
       (payload-continuity-valid-p
        outputs +test-video-pid+))
      (loop for template in templates
            for output in outputs
            for index from 0
            when (or
                  (zerop index)
                  (member index pcr-indices :test #'=))
              do
        (check-bridge-test
         (= (ts-pcr output) (ts-pcr template)))
        (check-bridge-test
         (equalp
          (subseq output 6 12)
          (subseq template 6 12))))
      (check-bridge-test
       (zerop
        (pes-assembler-av1-stream-byte-count assembler)))
      (check-bridge-test
       (zerop
        (bridge-processor-output-packet-count processor)))
      t)))

(define-bridge-test av1-byte-fifo-absorbs-320x192-key-shape
  (run-av1-byte-fifo-shape-test
   46 '(20) 91))

(define-bridge-test av1-byte-fifo-absorbs-long-1080p-key-shape
  (run-av1-byte-fifo-shape-test
   320
   (loop for index from 30 below 319 by 30
         collect index)
   103))

(define-bridge-test av1-byte-fifo-keeps-adaptation-only-pcr-capacity-zero
  (let* ((processor
           (make-bridge-processor
            (make-instance 'octet-collector-stream)
            :av1 :aac
            :transport-rate-kbps 1504))
         (assembler
           (%make-pes-assembler +test-video-pid+ :video))
         (pcr
           (make-test-program-pcr-packet
            90000 :counter 7))
         (pcr-entry
           (make-pending-entry
            :packet pcr
            :slot-index 1
            :resolved-p nil
            :use-original-p nil))
         (payload-template
           (make-ts-packet
            +test-video-pid+ 8
            (make-pattern-octets 184 3)))
         (payload-entry
           (make-pending-entry
            :packet payload-template
            :slot-index 2
            :resolved-p nil
            :use-original-p nil)))
    (setf
     (pes-assembler-active-p assembler) t
     (pes-assembler-streaming-p assembler) t
     (pes-assembler-stream-random-access-kind assembler) :key
     (pes-assembler-av1-stream-output-start-p assembler) t)
    (append-av1-stream-byte-segment
     processor assembler (octets #xaa #xbb) 0)
    (resolve-av1-stream-byte-target-entry
     processor assembler pcr-entry pcr)
    (let ((output-pcr
            (first
             (pending-entry-replacements pcr-entry))))
      (check-bridge-test output-pcr)
      (check-bridge-test
       (not (ts-has-payload-p output-pcr)))
      (check-bridge-test
       (= (ts-pcr output-pcr) (ts-pcr pcr)))
      (check-bridge-test
       (= (pes-assembler-av1-stream-byte-count assembler)
          2)))
    (resolve-av1-stream-byte-target-entry
     processor assembler payload-entry payload-template)
    (let ((output
            (first
             (pending-entry-replacements payload-entry))))
      (check-bridge-test (ts-payload-unit-start-p output))
      (check-bridge-test
       (equalp
        (subseq output (ts-payload-offset output))
        (octets #xaa #xbb)))
      (check-bridge-test
       (zerop
        (pes-assembler-av1-stream-byte-count assembler))))))

(define-bridge-test av1-byte-fifo-deadline-and-boundary-fail-closed
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :av1 :aac
           :transport-rate-kbps 1504))
        (assembler
          (%make-pes-assembler +test-video-pid+ :video)))
    (append-av1-stream-byte-segment
     processor assembler (octets #x11) 0)
    (validate-av1-stream-byte-deadline
     processor assembler :actual-slot 2)
    (validate-av1-stream-byte-deadline
     processor assembler
     :consume-current-slot-p t
     :actual-slot 3)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-av1-stream-byte-deadline
         processor assembler :actual-slot 3))))
    ;; 次PUSI/EOFで旧PESを跨がせず、同じ空検査でfail closedにする。
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (finish-av1-stream-byte-fifo assembler))))))

(define-bridge-test fixed-packet-allocator-rejects-eof-backlog
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :av1 :aac
           :transport-rate-kbps
           +test-transport-rate-kbps+)))
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet (make-ts-packet #x101 0 (octets 1))
      :resolved-p t
      :use-original-p nil
      :replacements
      (list
       (make-ts-packet #x101 0 (octets 2))
       (make-ts-packet #x101 1 (octets 3)))))
    (check-bridge-test
     (handler-case
         (progn
           (finish-output-packet-allocation processor)
           nil)
       (bridge-error (condition)
         (search
          "REPACKETIZE_CAPACITY_EXHAUSTED pending_packets=1"
          (bridge-error-message condition)))))))

(define-bridge-test fixed-packet-allocator-fills-shrink-with-null
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor output :vp9 :aac))
         (source
           (make-ts-packet
            #x101 0 (octets 1))))
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet source
      :resolved-p t
      :use-original-p nil
      :replacements '()))
    (let ((packets
            (octets-to-packet-list
             (collected-octets output))))
      (check-bridge-test (= (length packets) 1))
      (check-bridge-test
       (= (ts-pid (first packets))
          +ts-null-pid+)))))
