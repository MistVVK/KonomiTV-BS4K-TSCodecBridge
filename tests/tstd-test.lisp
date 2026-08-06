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

(defun process-tstd-transport-packet-reference
    (model arrival)
  "単体試験用に188 byteのTB recurrenceを逐次実行する。"
  (let* ((clock (tstd-model-clock model))
         (arrival-interval
           (/ 8
              (tstd-arrival-clock-transport-rate-bps
               clock)))
         (departures
           (make-array +ts-packet-size+)))
    (dotimes (offset +ts-packet-size+ departures)
      (setf
       (aref departures offset)
       (process-tstd-transport-byte
        model
        (+ arrival (* offset arrival-interval)))))))

(defun tstd-packet-departure-at (ranges offset)
  "圧縮済みRANGESからpacket内OFFSETのTB離脱時刻を返す。"
  (loop for range across ranges
        when (< offset (tstd-departure-range-end range))
          do (return
               (tstd-departure-range-time range offset))
        finally
           (bridge-error
            "Test departure range is missing offset ~D"
            offset)))

(defun tstd-transport-packet-overflow-reference-p
    (model arrival rate)
  "実際のbyte逐次TB処理で1 packetがoverflowするか非破壊で返す。"
  (let ((reference (copy-tstd-model model)))
    (setf
     (tstd-model-rx-bytes-per-second reference)
     rate)
    (handler-case
        (progn
          (process-tstd-transport-packet-reference
           reference arrival)
          nil)
      (bridge-error (condition)
        (if (search
             "TSTD_TB_OVERFLOW"
             (bridge-error-message condition)
             :test #'char=)
            t
            (error condition))))))

(defun tstd-transport-predictor-state (model)
  "TB overflow予測の非破壊性を検査するscalar stateを返す。"
  (list
   (tstd-model-packet-index model)
   (tstd-model-rx-bytes-per-second model)
   (tstd-model-transport-buffer-fullness model)
   (tstd-model-transport-buffer-last-arrival model)
   (tstd-model-transport-buffer-service-end model)
   (tstd-model-transport-buffer-busy-start model)
   (tstd-model-transport-buffer-last-empty model)
   (tstd-model-current-access-unit model)
   (tstd-model-pending-access-units model)))

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

(define-bridge-test tstd-tb-overflow-predictor-matches-byte-recurrence
  (let ((seed #x13579bdf)
        (checked-count 0)
        (rate-change-count 0)
        (transport-rates '(1504 2200 10470 20000))
        (rx-rates '(50000 100000 188000 376000
                    825000 1500000 3000000)))
    (labels
        ((next-random (limit)
           ;; 実行ごとに同じ実在可能state集合を作る決定的LCG。
           (setf seed
                 (logand
                  #xffffffff
                  (+ (* seed 1664525) 1013904223)))
           (mod seed limit)))
      (loop repeat 300
            do
        (let* ((transport-rate
                 (nth (next-random (length transport-rates))
                      transport-rates))
               (setup-rate
                 (nth (next-random (length rx-rates))
                      rx-rates))
               (candidate-rate
                 (nth (next-random (length rx-rates))
                      rx-rates))
               (setup-count (next-random 7))
               (model (make-tstd-model transport-rate))
               (clock (tstd-model-clock model))
               (slot-index 0))
          (setf
           (tstd-model-rx-bytes-per-second model)
           setup-rate)
          (handler-case
            (progn
                ;; 公開の逐次処理だけで予測直前のstateを作る。
                (loop repeat setup-count
                      do
                  (incf slot-index (1+ (next-random 5)))
                  (process-tstd-transport-packet
                   model
                   (tstd-packet-arrival-time
                    clock slot-index)))
                (incf slot-index (1+ (next-random 8)))
                (let* ((arrival
                         (tstd-packet-arrival-time
                          clock slot-index))
                       (before
                         (tstd-transport-predictor-state model))
                       (predicted
                         (tstd-transport-packet-overflow-p
                          model arrival candidate-rate))
                       (reference
                         (tstd-transport-packet-overflow-reference-p
                          model arrival candidate-rate)))
                  (check-bridge-test (eq predicted reference))
                  (check-bridge-test
                   (equal
                    before
                    (tstd-transport-predictor-state model)))
                  (incf checked-count)
                  (unless (= setup-rate candidate-rate)
                    (incf rate-change-count))))
            (bridge-error (condition)
              ;; 遅いRxでsetup中に512 byteを越えた組だけを除外する。
              (unless (search
                       "TSTD_TB_OVERFLOW"
                       (bridge-error-message condition)
                       :test #'char=)
                (error condition)))))))
    (check-bridge-test (> checked-count 150))
    (check-bridge-test (> rate-change-count 100))))

(define-bridge-test tstd-tb-overflow-predictor-uses-next-pusi-rate
  (let* ((model (make-tstd-model 10470))
         (clock (tstd-model-clock model))
         (next-access-unit
           (make-tstd-access-unit
            :rx-bytes-per-second 825000)))
    (setf
     (tstd-model-rx-bytes-per-second model) 825000)
    (dotimes (packet-index 7)
      (process-tstd-transport-packet
       model
       (tstd-packet-arrival-time clock packet-index)))
    (setf
     (tstd-model-packet-index model) 7
     (tstd-model-rx-bytes-per-second model) 2000000
     (tstd-model-pending-access-units model)
     (list next-access-unit))
    (let ((before (tstd-transport-predictor-state model)))
      (check-bridge-test
       (not
        (tstd-video-packet-would-overflow-p model)))
      (check-bridge-test
       (tstd-video-packet-would-overflow-p
        model :payload-unit-start-p t))
      (check-bridge-test
       (equal
        before
        (tstd-transport-predictor-state model))))))

(define-bridge-test tstd-tb-arrival-is-byte-paced
  (let* ((model (make-tstd-model 10470))
         (clock (tstd-model-clock model)))
    (setf
     (tstd-model-rx-bytes-per-second model)
     825000)
    ;; AV1 Level 3.0のRxでは7連続packetまで512 byte TBに収まる。
    (dotimes (packet-index 7)
      (process-tstd-transport-packet
       model
       (tstd-packet-arrival-time clock packet-index)))
    (check-bridge-test
     (= (tstd-model-transport-buffer-fullness model)
        169984/349))
    ;; 8 packet目はoffset 67で初めて上限を越える。
    (let ((message
            (handler-case
                (progn
                  (process-tstd-transport-packet
                   model
                   (tstd-packet-arrival-time clock 7))
                  nil)
              (bridge-error (condition)
                (bridge-error-message condition)))))
      (check-bridge-test
       (and
          message
          (search
           "TSTD_TB_OVERFLOW fullness=178756/349 capacity=512"
           message
           :test #'char=))))))

(define-bridge-test tstd-tb-packet-ranges-match-byte-recurrence
  (dolist (rate '(100000 188000 376000))
    (let* ((optimized (make-tstd-model 1504))
           (reference (make-tstd-model 1504))
           (clock (tstd-model-clock optimized)))
      (setf
       (tstd-model-rx-bytes-per-second optimized) rate
       (tstd-model-rx-bytes-per-second reference) rate)
      (dolist (packet-index '(0 1 3))
        (let* ((arrival
                 (tstd-packet-arrival-time
                  clock packet-index))
               (ranges
                 (process-tstd-transport-packet
                  optimized arrival))
               (departures
                 (process-tstd-transport-packet-reference
                  reference arrival)))
          (dotimes (offset +ts-packet-size+)
            (check-bridge-test
             (= (tstd-packet-departure-at ranges offset)
                (aref departures offset))))
          (check-bridge-test
           (equal
            (tstd-transport-predictor-state optimized)
            (tstd-transport-predictor-state reference))))))))

(define-bridge-test tstd-tb-fast-drain-waits-for-each-byte
  (let ((model (make-tstd-model 1504)))
    ;; Rxがtransport byte rateより速い場合、各byteは次の到着前に空化する。
    (setf
     (tstd-model-rx-bytes-per-second model)
     376000)
    (let ((departures
            (process-tstd-transport-packet model 0)))
      (check-bridge-test
       (= (tstd-packet-departure-at departures 0)
          1/376000))
      (check-bridge-test
       (= (tstd-packet-departure-at departures 187)
          375/376000))
      (check-bridge-test
       (= (tstd-model-transport-buffer-fullness model) 1)))
    (finish-tstd-transport-busy-period model :eof)
    (check-bridge-test
     (= (tstd-model-transport-buffer-last-empty model)
        375/376000))))

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
        23313/125))
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
    ;; presentation が removal より 4 秒以上前なら fail closed（3 秒許容を超える）。
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (process-tstd-buffer-pool-access-unit
         model
         (make-tstd-access-unit
          :pts 0
          :dts 360000
          :show-frame-p t
          :refresh-frame-flags 0)
         (tstd-timestamp-time clock 360000)))))
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
            :pts 0
            :dts 0
            :low-delay-mode-p t
            :show-frame-p t
            :refresh-frame-flags 0
            :rx-bytes-per-second 206250
            :multiplex-buffer-size 1118750
            :elementary-buffer-size 187500
            :transport-first-arrival 0
            :mb-first-departure 4
            :mb-last-departure 4)))
    (observe-tstd-pcr clock 0 0)
    (map nil
         (lambda (byte)
           (vector-push-extend
            byte
            (tstd-access-unit-es access-unit)))
         (octets 0 0 1 #x08))
    (vector-push-extend
     (make-tstd-departure-range 0 4 4 0)
     (tstd-access-unit-mb-departure-ranges access-unit))
    (setf (tstd-model-current-access-unit model)
          access-unit)
    ;; scheduled removal が PTS より 4 秒以上後なら fail closed。
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

(define-bridge-test repacketize-holdback-emits-head-at-exact-boundary
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor
            output :av1 :aac
            :transport-rate-kbps 1504))
         (packets
           (loop repeat 3
                 collect (make-output-null-packet))))
    (check-bridge-test
     (= (repacketize-holdback-slot-count processor) 2))
    (append-pending-entry processor (first packets))
    (flush-resolved-entries processor)
    (check-bridge-test
     (zerop (length (collected-octets output))))
    (append-pending-entry processor (second packets))
    (flush-resolved-entries processor)
    (check-bridge-test
     (zerop (length (collected-octets output))))
    ;; tail-slot - head-slot = H となった同じ呼出しでheadを出力する。
    (append-pending-entry processor (third packets))
    (flush-resolved-entries processor)
    (check-bridge-test
     (= (length (collected-octets output))
        +ts-packet-size+))
    (check-bridge-test
     (= (pending-entry-slot-index
         (bridge-processor-pending-head processor))
        1))
    (check-bridge-test
     (= (bridge-processor-pending-count processor) 2))))

(defun make-test-backfill-processor ()
  "PMT backfill境界試験用の1504kbps processorを返す。"
  (make-bridge-processor
   (make-instance 'octet-collector-stream)
   :av1 :aac
   :transport-rate-kbps 1504))

(defun make-test-fixed-video-pusi ()
  "backfill試験用のPCR付きPUSI packetを返す。"
  (first
   (packetize-test-video-with-pcr
    (make-pes
     #xe0
     (make-test-av1-access-unit 8)
     90000)
    :continuity-counter 7
    :pcr-lead-ticks
    +test-tstd-removal-delay-ticks+)))

(define-bridge-test pmt-backfill-allows-latest-at-information-deadline
  (let* ((processor (make-test-backfill-processor))
         (ordinary
           (make-ts-packet +test-data-pid+ 0 (octets 1)))
         (null-packet (make-output-null-packet))
         (pusi-packet (make-test-fixed-video-pusi))
         (pusi-copy (copy-seq pusi-packet))
         (pmt-packet
           (make-ts-packet
            +test-pmt-pid+ 5 (octets 0 1 2)
            :payload-unit-start t))
         (old-entry
           (append-pending-entry processor ordinary))
         (null-entry
           (append-pending-entry processor null-packet))
         (pusi-entry
           (append-pending-entry processor pusi-packet))
         (latest-entry
           (append-pending-entry processor ordinary)))
    (declare (ignore old-entry latest-entry))
    ;; origin=2, latest=origin+1（M=1の情報期限）でorigin-1のnullを使える。
    (backfill-synthetic-pmt-before-video
     processor (list pmt-packet) pusi-entry)
    (check-bridge-test
     (= (pending-entry-slot-index null-entry) 1))
    (check-bridge-test
     (equalp (pending-entry-packet null-entry) pmt-packet))
    (check-bridge-test
     (= (pending-entry-slot-index pusi-entry) 2))
    (check-bridge-test
     (equalp (pending-entry-packet pusi-entry) pusi-copy))))

(define-bridge-test pmt-backfill-rejects-information-after-deadline
  (let* ((processor (make-test-backfill-processor))
         (ordinary
           (make-ts-packet +test-data-pid+ 0 (octets 1)))
         (pmt-packet
           (make-ts-packet
            +test-pmt-pid+ 5 (octets 0 1 2)
            :payload-unit-start t))
         (null-entry
           (append-pending-entry
            processor (make-output-null-packet)))
         (ordinary-entry
           (append-pending-entry processor ordinary))
         (pusi-entry
           (append-pending-entry
            processor (make-test-fixed-video-pusi))))
    (declare (ignore null-entry ordinary-entry))
    (append-pending-entry processor ordinary)
    (append-pending-entry processor ordinary)
    ;; origin=2, latest=origin+2 はM=1の情報期限 origin+1 を越える。
    (let ((message
            (handler-case
                (progn
                  (backfill-synthetic-pmt-before-video
                   processor (list pmt-packet) pusi-entry)
                  nil)
              (bridge-error (condition)
                (bridge-error-message condition)))))
      (check-bridge-test
       (and message
            (search
             "reason=pmt_backfill_information_deadline"
             message :test #'char=))))))

(define-bridge-test pmt-backfill-rejects-future-null
  (let ((processor (make-test-backfill-processor))
        (ordinary
          (make-ts-packet +test-data-pid+ 0 (octets 1)))
        (pmt-packet
          (make-ts-packet
           +test-pmt-pid+ 5 (octets 0 1 2)
           :payload-unit-start t)))
    (append-pending-entry processor ordinary)
    (let ((pusi-entry
            (append-pending-entry
             processor (make-test-fixed-video-pusi))))
      ;; future nullは保持窓内でもPUSI先行PMTには使えない。
      (append-pending-entry processor (make-output-null-packet))
      (let ((message
              (handler-case
                  (progn
                    (backfill-synthetic-pmt-before-video
                     processor (list pmt-packet) pusi-entry)
                    nil)
                (bridge-error (condition)
                  (bridge-error-message condition)))))
        (check-bridge-test
         (and message
              (search
               "reason=pmt_backfill_null_slots"
               message :test #'char=)))))))

(define-bridge-test pmt-backfill-rejects-null-older-than-holdback
  (let ((processor (make-test-backfill-processor))
        (ordinary
          (make-ts-packet +test-data-pid+ 0 (octets 1)))
        (pmt-packet
          (make-ts-packet
           +test-pmt-pid+ 5 (octets 0 1 2)
           :payload-unit-start t)))
    (append-pending-entry processor (make-output-null-packet))
    (append-pending-entry processor ordinary)
    (append-pending-entry processor ordinary)
    (let* ((pusi-entry
             (append-pending-entry
              processor (make-test-fixed-video-pusi)))
           (message
             (handler-case
                 (progn
                   (backfill-synthetic-pmt-before-video
                    processor (list pmt-packet) pusi-entry)
                   nil)
               (bridge-error (condition)
                 (bridge-error-message condition)))))
      ;; origin-3のnullは未出力でも候補に戻さない。
      (check-bridge-test
       (and message
            (search
             "reason=pmt_backfill_null_slots"
             message :test #'char=))))))

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

(define-bridge-test fixed-packet-allocator-keeps-header-flags-byte-exact
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor output :vp9 :aac))
         (source
           (make-ts-packet #x101 12 (octets 1)))
         (fixed-discontinuity
           (make-ts-packet
            #x101 13 (octets 2 3)
            :payload-unit-start t
            :random-access t
            :elementary-stream-priority t
            :transport-priority t))
         (queued
           (make-ts-packet #x101 14 (octets 4 5)))
         (null-packet (make-output-null-packet)))
    (setf
     (aref fixed-discontinuity 5)
     (logior (aref fixed-discontinuity 5) #x80))
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet source
      :resolved-p t
      :use-original-p nil
      :replacements
      (list fixed-discontinuity queued)))
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet null-packet
      :resolved-p t
      :use-original-p t))
    (let ((packets
            (octets-to-packet-list
             (collected-octets output))))
      (check-bridge-test
       (equalp packets
               (list fixed-discontinuity queued)))
      (check-bridge-test
       (ts-payload-unit-start-p (first packets)))
      (check-bridge-test
       (= (ts-continuity-counter (first packets)) 13))
      (check-bridge-test
       (ts-random-access-indicator-p (first packets)))
      (check-bridge-test
       (ts-elementary-stream-priority-indicator-p
        (first packets)))
      (check-bridge-test
       (ts-discontinuity-indicator-p (first packets))))))

(define-bridge-test fixed-packet-allocator-rejects-delayed-discontinuity
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor output :vp9 :aac))
         (source (make-ts-packet #x101 0 (octets 1)))
         (primary (make-ts-packet #x101 0 (octets 2)))
         (queued (make-ts-packet #x101 1 (octets 3)))
         (next-source (make-ts-packet #x101 2 (octets 4)))
         (discontinuity
           (make-ts-packet #x101 2 (octets 5))))
    (setf
     (aref discontinuity 5)
     (logior (aref discontinuity 5) #x80))
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet source
      :resolved-p t
      :use-original-p nil
      :replacements (list primary queued)))
    (let ((message
            (handler-case
                (progn
                  (allocate-output-entry
                   processor
                   (make-pending-entry
                    :packet next-source
                    :resolved-p t
                    :use-original-p nil
                    :replacements (list discontinuity)))
                  nil)
              (bridge-error (condition)
                (bridge-error-message condition)))))
      (check-bridge-test
       (and
        message
        (search
         "REPACKETIZE_DISCONTINUITY_CANNOT_USE_RECLAIMED_SLOT"
         message
         :test #'char=)))
      ;; fail-closedはcurrent slotを部分出力しない。
      (check-bridge-test
       (= (length (collected-octets output))
          +ts-packet-size+)))))

(define-bridge-test fixed-packet-allocator-rejects-fixed-pcr-collision
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor output :vp9 :aac))
         (source (make-ts-packet #x101 0 (octets 1)))
         (primary (make-ts-packet #x101 0 (octets 2)))
         (queued (make-ts-packet #x101 1 (octets 3)))
         (pcr
           (make-test-program-pcr-packet
            90000 :counter 2)))
    (setf
     (bridge-processor-current-pcr-pid processor)
     +test-video-pid+)
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet source
      :resolved-p t
      :use-original-p nil
      :replacements (list primary queued)))
    (let ((message
            (handler-case
                (progn
                  (allocate-output-entry
                   processor
                   (make-pending-entry
                    :packet pcr
                    :resolved-p t
                    :use-original-p nil
                    :replacements (list (copy-seq pcr))))
                  nil)
              (bridge-error (condition)
                (bridge-error-message condition)))))
      (check-bridge-test
       (and
        message
        (search
         "REPACKETIZE_PREFIX_CANNOT_PRECEDE_FIXED_PCR"
         message
         :test #'char=)))
      (check-bridge-test
       (= (length (collected-octets output))
          +ts-packet-size+)))))

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

(defun configure-forced-tstd-smoothing-state (processor)
  "次の映像packetだけがTB overflowする実数fullness stateを設定する。"
  (let ((model (bridge-processor-tstd-model processor)))
    (setf
     (tstd-model-video-pid model) +test-video-pid+
     (tstd-model-rx-bytes-per-second model) 100000
     ;; このallocator試験ではPES/MBではなくTB平準化だけを対象にする。
     (tstd-model-discarding-access-unit-p model) t
     (tstd-model-transport-buffer-fullness model) 500
     (tstd-model-transport-buffer-last-arrival model) 0
     (tstd-model-transport-buffer-service-end model) 1/200
     (tstd-model-transport-buffer-busy-start model) 0)
    model))

(define-bridge-test fixed-packet-allocator-smoothing-uses-two-ms-boundary
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor
            output :av1 :aac
            :transport-rate-kbps 1504))
         (candidate
           (make-ts-packet
            +test-video-pid+ 7 (octets 7)))
         (ordinary
           (make-ts-packet #x120 0 (octets 9)))
         (null-packet (make-output-null-packet)))
    (configure-forced-tstd-smoothing-state processor)
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet
      (make-ts-packet +test-video-pid+ 7 (octets #xff))
      :resolved-p t
      :use-original-p nil
      :replacements (list candidate)
      :replacement-provenances
      (list
       (make-replacement-provenance
        :origin-slot 0
        :deadline-slot 2))))
    ;; slot 0で平準化queueへ送り、slot 3先頭（待ち時間ちょうど2ms）
    ;; のnull容量でbyte-exactに回収する。
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
      (check-bridge-test
       (= (ts-pid (first packets)) +ts-null-pid+))
      (check-bridge-test (equalp (fourth packets) candidate))
      (check-bridge-test
       (zerop
        (bridge-processor-output-packet-count processor))))))

(define-bridge-test fixed-packet-allocator-smoothing-fails-after-two-ms
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor
            output :av1 :aac
            :transport-rate-kbps 1504))
         (candidate
           (make-ts-packet
            +test-video-pid+ 7 (octets 7)))
         (ordinary
           (make-ts-packet #x120 0 (octets 9))))
    (configure-forced-tstd-smoothing-state processor)
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet
      (make-ts-packet +test-video-pid+ 7 (octets #xff))
      :resolved-p t
      :use-original-p nil
      :replacements (list candidate)
      :replacement-provenances
      (list
       (make-replacement-provenance
        :origin-slot 0
        :deadline-slot 2))))
    (loop repeat 2
          do
      (allocate-output-entry
       processor
       (make-pending-entry
        :packet ordinary
        :resolved-p t
        :use-original-p t)))
    ;; slot 3も固定packetなら次に使えるslot 4は期限外なので即時fail。
    (let ((message
            (handler-case
                (progn
                  (allocate-output-entry
                   processor
                   (make-pending-entry
                    :packet ordinary
                    :resolved-p t
                    :use-original-p t))
                  nil)
              (bridge-error (condition)
                (bridge-error-message condition)))))
      (check-bridge-test
       (and
        message
        (search
         "REPACKETIZE_CAPACITY_EXHAUSTED origin_slot=0 deadline_slot=2 actual_slot=3"
         message
         :test #'char=)))
      (check-bridge-test
       (= (length (collected-octets output))
          (* 3 +ts-packet-size+))))))

(define-bridge-test fixed-packet-allocator-paces-av1-tb-with-null
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor
            output :av1 :aac
            :transport-rate-kbps 10470))
         (model (bridge-processor-tstd-model processor))
         (replacements
           (loop for index below 8
                 collect
             (make-ts-packet
              +test-video-pid+
              (logand index #x0f)
              (octets index)))))
    (setf
     (tstd-model-video-pid model) +test-video-pid+
     (tstd-model-rx-bytes-per-second model) 825000
     ;; この単体試験ではPES/MBではなくTB平準化だけを対象にする。
     (tstd-model-discarding-access-unit-p model) t)
    (loop for replacement in replacements
          do
      (allocate-output-entry
       processor
       (make-pending-entry
        :packet
        (make-ts-packet
         +test-video-pid+
         (ts-continuity-counter replacement)
         (octets #xff))
        :resolved-p t
        :use-original-p nil
        :replacements (list replacement))))
    ;; 8個目を置くと真にoverflowするslotはnullへreclaimされ、
    ;; 直後の既存null slotで同じpacketを2ms以内に回収する。
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet (make-output-null-packet)
      :resolved-p t
      :use-original-p t))
    (let* ((packets
             (octets-to-packet-list
              (collected-octets output)))
           (video-packets
             (remove-if-not
              (lambda (packet)
                (= (ts-pid packet) +test-video-pid+))
              packets)))
      (check-bridge-test (= (length packets) 9))
      (check-bridge-test
       (every #'equalp video-packets replacements))
      (check-bridge-test
       (= (ts-pid (nth 7 packets)) +ts-null-pid+))
      (check-bridge-test
       (equalp (nth 8 packets) (nth 7 replacements)))
      (check-bridge-test
       (<= (tstd-model-transport-buffer-fullness model)
           +tstd-transport-buffer-size+))
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
    ;; 一般の再配置期限 2 ms を越えても、24 fps の 1 frame を覆う
    ;; AV1 専用 50 ms 窓内なら AU 間の null packet を待てる。
    (validate-av1-stream-byte-deadline
     processor assembler :actual-slot 50)
    (validate-av1-stream-byte-deadline
     processor assembler
     :consume-current-slot-p t
     :actual-slot 51)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-av1-stream-byte-deadline
         processor assembler :actual-slot 51))))
    ;; 次PUSI/EOFで旧PESを跨がせず、同じ空検査でfail closedにする。
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (finish-av1-stream-byte-fifo assembler))))))

(define-bridge-test av1-byte-fifo-does-not-use-null-without-template
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor
            output :av1 :aac
            :transport-rate-kbps 1504))
         (assembler
           (%make-pes-assembler +test-video-pid+ :video))
         (null-packet (make-output-null-packet)))
    (setf
     (bridge-processor-current-video-pid processor)
     +test-video-pid+
     (gethash
      +test-video-pid+
      (bridge-processor-pes-assemblers processor))
     assembler
     (pes-assembler-active-p assembler) t
     (pes-assembler-streaming-p assembler) t)
    ;; slot 4で生成されたbyteを、video template未確定のslot 0
    ;; nullへ置くとPUSIとtransport headerを正しく構成できない。
    (append-av1-stream-byte-segment
     processor assembler (octets #x11) 4)
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet null-packet
      :slot-index 0
      :resolved-p t
      :use-original-p t))
    (let ((packets
            (octets-to-packet-list
             (collected-octets output))))
      (check-bridge-test (= (length packets) 1))
      (check-bridge-test (equalp (first packets) null-packet))
      (check-bridge-test
       (= (pes-assembler-av1-stream-byte-count assembler) 1)))))

(define-bridge-test av1-byte-fifo-does-not-use-null-before-current-pes
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor
            output :av1 :aac
            :transport-rate-kbps 1504))
         (assembler
           (%make-pes-assembler +test-video-pid+ :video))
         (template
           (make-ts-packet
            +test-video-pid+ 0 (octets #x00)))
         (source-entry
           (make-pending-entry
            :packet template
            :slot-index 4
            :resolved-p nil
            :use-original-p nil))
         (null-packet (make-output-null-packet)))
    (setf
     (bridge-processor-current-video-pid processor)
     +test-video-pid+
     (gethash
      +test-video-pid+
      (bridge-processor-pes-assemblers processor))
     assembler
     (pes-assembler-active-p assembler) t
     (pes-assembler-streaming-p assembler) t
     (pes-assembler-entries assembler) (list source-entry)
     (pes-assembler-first-entry-slot-index assembler) 4
     (pes-assembler-av1-stream-last-template assembler)
     template)
    ;; 前PESのtemplateが残っていても、現PESのPUSIより前のnullに
    ;; 現PES先頭byteを逆行配置してはならない。
    (append-av1-stream-byte-segment
     processor assembler (octets #x11) 4)
    (allocate-output-entry
     processor
     (make-pending-entry
      :packet null-packet
      :slot-index 0
      :resolved-p t
      :use-original-p t))
    (let ((packets
            (octets-to-packet-list
             (collected-octets output))))
      (check-bridge-test (= (length packets) 1))
      (check-bridge-test (equalp (first packets) null-packet))
      (check-bridge-test
       (= (pes-assembler-av1-stream-byte-count assembler) 1)))))

(define-bridge-test av1-null-current-pes-boundary-uses-recorded-slot
  (let ((processor
           (make-bridge-processor
            (make-instance 'octet-collector-stream)
            :passthrough :opus))
         (assembler
           (%make-pes-assembler +test-video-pid+ :video))
         (first-entry
           (make-pending-entry
            :packet
            (make-ts-packet
             +test-video-pid+ 0
             (octets 0 0 1 #xe0 0 100)
             :payload-unit-start t)
            :slot-index 40)))
    (process-target-pes-packet
     processor assembler first-entry
     (pending-entry-packet first-entry))
    (check-bridge-test
     (= (pes-assembler-first-entry-slot-index assembler) 40))
    ;; 境界判定が成長するentry listを末尾走査しないことを直接検証する。
    (setf (pes-assembler-entries assembler) '())
    (check-bridge-test
     (not
      (av1-null-inside-current-pes-p
       assembler
       (make-pending-entry :slot-index 39))))
    (check-bridge-test
     (av1-null-inside-current-pes-p
      assembler
      (make-pending-entry :slot-index 40)))
    (check-bridge-test
     (av1-null-inside-current-pes-p
      assembler
      (make-pending-entry :slot-index 40000)))
    (clear-pes-event-state assembler)
    (check-bridge-test
     (null (pes-assembler-first-entry-slot-index assembler)))))

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
