;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defconstant +soak-validation-consing-limit-per-read+ 64
  "Gray stream基準に対してTS検証が追加してよいread当たりbyte数。")

(defconstant +soak-latency-bucket-count+ 60001)
(defconstant +soak-latency-bucket-nanoseconds+ 1000)
(defconstant +soak-cycle-access-unit-count+ 16)
(defconstant +soak-minimum-rss-slope-duration-seconds+ 600)

(defstruct (soak-latency-histogram
            (:constructor make-soak-latency-histogram ()))
  (counts
    (make-array +soak-latency-bucket-count+
                :element-type '(unsigned-byte 64)
                :initial-element 0)
    :type (simple-array (unsigned-byte 64) (*)))
  (sample-count 0 :type (unsigned-byte 64))
  (maximum-nanoseconds 0 :type (unsigned-byte 64)))

(defstruct (soak-rss-sampler
            (:constructor make-soak-rss-sampler
                (interval-ticks growth-limit-kib
                 slope-limit-kib-per-hour
                 &optional normalized-sampling-p)))
  (interval-ticks 1 :type (integer 1 *))
  (growth-limit-kib 0 :type (integer 0 *))
  (slope-limit-kib-per-hour 0 :type (integer 0 *))
  (normalized-sampling-p nil :type boolean)
  (next-sample-tick 0 :type (integer 0 *))
  (sample-count 0 :type (integer 0 *))
  (first-kib nil :type (or null (integer 0 *)))
  (last-kib nil :type (or null (integer 0 *)))
  (minimum-kib nil :type (or null (integer 0 *)))
  (maximum-kib nil :type (or null (integer 0 *)))
  (increase-streak 0 :type (integer 0 *))
  (maximum-increase-streak 0 :type (integer 0 *))
  (samples '() :type list)
  (normalized-samples '() :type list))

(defun record-soak-latency (histogram nanoseconds)
  "HISTOGRAMへ非負のmonotonic wall nanosecondを記録する。"
  (let ((bucket
          (min
           (ceiling
            nanoseconds
            +soak-latency-bucket-nanoseconds+)
           (- +soak-latency-bucket-count+ 1))))
    (incf (aref (soak-latency-histogram-counts histogram)
                bucket))
    (incf (soak-latency-histogram-sample-count histogram))
    (setf
     (soak-latency-histogram-maximum-nanoseconds histogram)
     (max
      nanoseconds
      (soak-latency-histogram-maximum-nanoseconds histogram))))
  histogram)

(defun soak-percentile-nanoseconds (histogram percentile)
  "HISTOGRAMのnearest-rank PERCENTILEをnanosecondで返す。"
  (let* ((sample-count
           (soak-latency-histogram-sample-count histogram))
         (rank
           (max 1 (ceiling (* percentile sample-count))))
         (seen 0))
    (when (zerop sample-count)
      (bridge-error "Soak latency histogram has no samples"))
    (loop for count across
            (soak-latency-histogram-counts histogram)
          for bucket from 0
          do (incf seen count)
          when (>= seen rank)
            do
               (return
                 (*
                  bucket
                  +soak-latency-bucket-nanoseconds+))
          finally
             (bridge-error "Soak latency histogram is inconsistent"))))

(defun soak-ticks-to-milliseconds (ticks)
  "内部時計TICKSをmillisecondへ変換する。"
  (* 1000.0d0
     (/ ticks internal-time-units-per-second)))

(defun soak-nanoseconds-to-milliseconds (nanoseconds)
  "NANOSECONDSをmillisecondへ変換する。"
  (/ nanoseconds 1000000.0d0))

(defun soak-latency-result (histogram)
  "HISTOGRAMの分位値と2ms gateをplistで返す。"
  (let ((p50
          (soak-nanoseconds-to-milliseconds
           (soak-percentile-nanoseconds
            histogram 0.50d0)))
        (p95
          (soak-nanoseconds-to-milliseconds
           (soak-percentile-nanoseconds
            histogram 0.95d0)))
        (p99
          (soak-nanoseconds-to-milliseconds
           (soak-percentile-nanoseconds
            histogram 0.99d0)))
        (maximum
          (soak-nanoseconds-to-milliseconds
           (soak-latency-histogram-maximum-nanoseconds
            histogram))))
    (list
     :latency-clock :clock-monotonic-raw
     :latency-samples
     (soak-latency-histogram-sample-count histogram)
     :latency-p50-ms p50
     :latency-p95-ms p95
     :latency-p99-ms p99
     :latency-max-ms maximum
     :latency-p99-under-2ms-p (< p99 2.0d0))))

(defun read-linux-rss-kib ()
  "Linux /procから現在processのresident set sizeをKiBで返す。"
  (with-open-file
      (stream #P"/proc/self/status"
              :direction :input
              :external-format :utf-8)
    (loop for line = (read-line stream nil nil)
          while line
          when (and (>= (length line) 6)
                    (string= line "VmRSS:"
                             :end1 6 :end2 6))
            do (return
                 (parse-integer line
                                :start 6
                                :junk-allowed t))
          finally
             (bridge-error
              "Linux process status does not contain VmRSS"))))

(defun sample-soak-rss (sampler &optional force-p)
  "期限到達時またはFORCE-P時にraw RSSと必要ならfull GC後RSSを記録する。"
  (let ((now (get-internal-real-time)))
    (unless (or force-p
                (>= now
                    (soak-rss-sampler-next-sample-tick sampler)))
      (return-from sample-soak-rss nil))
    (let ((rss (read-linux-rss-kib))
          (previous (soak-rss-sampler-last-kib sampler)))
      (unless (soak-rss-sampler-first-kib sampler)
        (setf (soak-rss-sampler-first-kib sampler) rss
              (soak-rss-sampler-minimum-kib sampler) rss
              (soak-rss-sampler-maximum-kib sampler) rss))
      (if (and previous (> rss previous))
          (incf (soak-rss-sampler-increase-streak sampler))
          (setf (soak-rss-sampler-increase-streak sampler) 0))
      (setf
       (soak-rss-sampler-maximum-increase-streak sampler)
       (max
        (soak-rss-sampler-maximum-increase-streak sampler)
        (soak-rss-sampler-increase-streak sampler))
       (soak-rss-sampler-last-kib sampler) rss
       (soak-rss-sampler-minimum-kib sampler)
       (min rss (soak-rss-sampler-minimum-kib sampler))
       (soak-rss-sampler-maximum-kib sampler)
       (max rss (soak-rss-sampler-maximum-kib sampler))
       (soak-rss-sampler-next-sample-tick sampler)
       (+ now (soak-rss-sampler-interval-ticks sampler)))
      (incf (soak-rss-sampler-sample-count sampler))
      (push (cons now rss)
            (soak-rss-sampler-samples sampler))
      (when (soak-rss-sampler-normalized-sampling-p sampler)
        (sb-ext:gc :full t)
        (push
         (cons (get-internal-real-time)
               (read-linux-rss-kib))
         (soak-rss-sampler-normalized-samples sampler)))
      rss)))

(defun soak-rss-slope-kib-per-hour (samples)
  "時刻とRSSのSAMPLES両端からKiB/hourの傾きを返す。"
  (if (< (length samples) 2)
      0.0d0
      (let* ((first (first samples))
             (last (car (last samples)))
             (elapsed (- (car last) (car first))))
        (if (zerop elapsed)
            0.0d0
            (* (- (cdr last) (cdr first))
               (/ (* 3600.0d0
                     internal-time-units-per-second)
                  elapsed))))))

(defun soak-rss-regression-slope-kib-per-hour (samples)
  "時刻とRSSのSAMPLES全点に最小二乗回帰を適用しKiB/hourを返す。"
  (when (< (length samples) 2)
    (return-from soak-rss-regression-slope-kib-per-hour
      0.0d0))
  (let ((origin (caar samples))
        (count (coerce (length samples) 'double-float))
        (sum-x 0.0d0)
        (sum-y 0.0d0)
        (sum-xx 0.0d0)
        (sum-xy 0.0d0))
    (dolist (sample samples)
      (let ((x
              (coerce (- (car sample) origin)
                      'double-float))
            (y (coerce (cdr sample) 'double-float)))
        (incf sum-x x)
        (incf sum-y y)
        (incf sum-xx (* x x))
        (incf sum-xy (* x y))))
    (let ((denominator
            (- sum-xx (/ (* sum-x sum-x) count))))
      (if (zerop denominator)
          0.0d0
          (*
           (/ (- sum-xy
                 (/ (* sum-x sum-y) count))
              denominator)
           3600.0d0
           internal-time-units-per-second)))))

(defun soak-rss-tail-samples (samples)
  "SAMPLESの後半を返す。"
  (nthcdr (floor (length samples) 2) samples))

(defun soak-rss-sample-first (samples &optional (fallback 0))
  "SAMPLESの先頭RSSを返し、空ならFALLBACKを返す。"
  (if samples
      (cdar samples)
      fallback))

(defun soak-rss-sample-last (samples &optional (fallback 0))
  "SAMPLESの末尾RSSを返し、空ならFALLBACKを返す。"
  (if samples
      (cdr (car (last samples)))
      fallback))

(defun soak-rss-sample-minimum (samples &optional (fallback 0))
  "SAMPLESの最小RSSを返し、空ならFALLBACKを返す。"
  (if samples
      (reduce #'min samples :key #'cdr)
      fallback))

(defun soak-rss-sample-maximum (samples &optional (fallback 0))
  "SAMPLESの最大RSSを返し、空ならFALLBACKを返す。"
  (if samples
      (reduce #'max samples :key #'cdr)
      fallback))

(defun soak-rss-sample-growth (samples)
  "SAMPLESの先頭から末尾までの非負RSS増加量を返す。"
  (max
   0
   (-
    (soak-rss-sample-last samples)
    (soak-rss-sample-first samples))))

(defun soak-rss-maximum-increase-streak (samples)
  "SAMPLESでRSSが連続して増えた最大区間数を返す。"
  (let ((previous nil)
        (streak 0)
        (maximum 0))
    (dolist (sample samples)
      (if (and previous (> (cdr sample) previous))
          (incf streak)
          (setf streak 0))
      (setf previous (cdr sample)
            maximum (max maximum streak)))
    maximum))

(defun soak-rss-result
    (sampler &optional (slope-gate-applicable-p t))
  "SAMPLERのraw・同一GC位相RSS推移を観測窓別gate付きplistで返す。"
  (let* ((raw-samples
           (nreverse
            (copy-list
             (soak-rss-sampler-samples sampler))))
         (normalized-samples
           (nreverse
            (copy-list
             (soak-rss-sampler-normalized-samples sampler))))
         (gate-samples
           (if slope-gate-applicable-p
               normalized-samples
               raw-samples))
         (raw-tail-samples
           (soak-rss-tail-samples raw-samples))
         (gate-tail-samples
           (soak-rss-tail-samples gate-samples))
         (raw-slope
           (soak-rss-slope-kib-per-hour
            raw-samples))
         (raw-tail-slope
           (soak-rss-slope-kib-per-hour
            raw-tail-samples))
         (slope
           (soak-rss-regression-slope-kib-per-hour
            gate-samples))
         (tail-slope
           (soak-rss-regression-slope-kib-per-hour
            gate-tail-samples))
         (raw-first
           (soak-rss-sample-first raw-samples))
         (raw-last
           (soak-rss-sample-last raw-samples raw-first))
         (raw-growth (soak-rss-sample-growth raw-samples))
         (first
           (soak-rss-sample-first gate-samples))
         (last
           (soak-rss-sample-last gate-samples first))
         (growth (max 0 (- last first)))
         (samples (length gate-samples))
         (maximum-increase-streak
           (soak-rss-maximum-increase-streak gate-samples))
         (continuous-increase-p
           (and (> samples 2)
                (>=
                 maximum-increase-streak
                 (- samples 1))))
         (sufficient-normalized-samples-p
           (or
            (not slope-gate-applicable-p)
            (>= (length normalized-samples) 3)))
         (passed
           (and
            sufficient-normalized-samples-p
            (<= growth
                (soak-rss-sampler-growth-limit-kib sampler))
            (or
             (not slope-gate-applicable-p)
             (and
              (or
               (<= tail-slope
                   (soak-rss-sampler-slope-limit-kib-per-hour
                    sampler))
               (<= raw-tail-slope
                   (soak-rss-sampler-slope-limit-kib-per-hour
                    sampler)))
              (not continuous-increase-p))))))
    (list
     :rss-samples samples
     :rss-first-kib first
     :rss-last-kib last
     :rss-minimum-kib
     (soak-rss-sample-minimum gate-samples first)
     :rss-maximum-kib
     (soak-rss-sample-maximum gate-samples last)
     :rss-growth-kib growth
     :rss-growth-limit-kib
     (soak-rss-sampler-growth-limit-kib sampler)
     :rss-raw-samples (length raw-samples)
     :rss-raw-first-kib raw-first
     :rss-raw-last-kib raw-last
     :rss-raw-minimum-kib
     (soak-rss-sample-minimum raw-samples raw-first)
     :rss-raw-maximum-kib
     (soak-rss-sample-maximum raw-samples raw-last)
     :rss-raw-growth-kib raw-growth
     :rss-raw-slope-kib-per-hour raw-slope
     :rss-raw-tail-slope-kib-per-hour raw-tail-slope
     :rss-normalized-samples (length normalized-samples)
     :rss-normalized-sample-count-sufficient-p
     sufficient-normalized-samples-p
     :rss-slope-kib-per-hour slope
     :rss-tail-slope-kib-per-hour tail-slope
     :rss-slope-limit-kib-per-hour
     (soak-rss-sampler-slope-limit-kib-per-hour sampler)
     :rss-slope-gate-applicable-p
     slope-gate-applicable-p
     :rss-maximum-increase-streak
     maximum-increase-streak
     :rss-not-continuously-growing-p
     (not continuous-increase-p)
     :rss-gate-passed-p passed)))

(defun make-synthetic-soak-rss-samples (values)
  "1分間隔のVALUESからRSS gate単体試験用sample列を作る。"
  (loop for value in values
        for index from 0
        collect
        (cons
         (* index 60 internal-time-units-per-second)
         value)))

(defun make-synthetic-soak-rss-sampler
    (raw-values normalized-values)
  "RAW-VALUESとNORMALIZED-VALUESを持つRSS samplerを作る。"
  (let ((sampler
          (make-soak-rss-sampler
           internal-time-units-per-second
           16384 1024 t)))
    (setf
     (soak-rss-sampler-samples sampler)
     (reverse
      (make-synthetic-soak-rss-samples raw-values))
     (soak-rss-sampler-normalized-samples sampler)
     (reverse
      (make-synthetic-soak-rss-samples normalized-values)))
    sampler))

(define-bridge-test soak-rss-gc-phase-normalization-ignores-sawtooth
  (let* ((sampler
           (make-synthetic-soak-rss-sampler
            '(100000 120000 90000 121000 91000 119000
              90000 122000 92000 120000 91000 123000)
            '(46000 46000 46000 46000 46000 46000
              46000 46000 46000 46000 46000 46000)))
         (result (soak-rss-result sampler t)))
    (check-bridge-test
     (>
      (getf result :rss-raw-tail-slope-kib-per-hour)
      (getf result :rss-slope-limit-kib-per-hour)))
    (check-bridge-test
     (zerop (getf result :rss-tail-slope-kib-per-hour)))
    (check-bridge-test
     (getf result :rss-gate-passed-p)))
  ;; full GC後のpage residencyが断続的に増えても、raw RSSが減少して
  ;; いる場合はretentionと断定しない。
  (let* ((sampler
           (make-synthetic-soak-rss-sampler
            '(120000 116000 112000 108000 104000 100000
              98000 96000 94000 92000 90000)
            '(46000 46256 46256 46512 46768 46768
              47024 47280 47280 47536 47792)))
         (result (soak-rss-result sampler t)))
    (check-bridge-test
     (>
      (getf result :rss-tail-slope-kib-per-hour)
      (getf result :rss-slope-limit-kib-per-hour)))
    (check-bridge-test
     (<
      (getf result :rss-raw-tail-slope-kib-per-hour)
      (getf result :rss-slope-limit-kib-per-hour)))
    (check-bridge-test
     (getf result :rss-gate-passed-p))))

(define-bridge-test soak-rss-gc-phase-normalization-rejects-retention
  (let* ((sampler
           (make-synthetic-soak-rss-sampler
            '(100000 90000 120000 91000 119000 90000
              121000 92000 118000 91000 120000)
            '(46000 46256 46256 46512 46768 46768
              47024 47280 47280 47536 47792)))
         (result (soak-rss-result sampler t)))
    (check-bridge-test
     (< (getf result :rss-growth-kib)
        (getf result :rss-growth-limit-kib)))
    (check-bridge-test
     (getf result :rss-not-continuously-growing-p))
    (check-bridge-test
     (>
      (getf result :rss-tail-slope-kib-per-hour)
      (getf result :rss-slope-limit-kib-per-hour)))
    (check-bridge-test
     (not (getf result :rss-gate-passed-p)))))

(defun append-plists (&rest plists)
  "PLISTSを順番どおり連結した新しいplistを返す。"
  (apply #'append plists))

(defun make-soak-pass-through-pattern ()
  "CCが周回しても連続する16 packetの決定的patternを作る。"
  (apply
   #'concatenate-octets
   (loop for counter from 0 below 16
         collect
         (make-ts-packet
          #x120 counter
          (make-pattern-octets 184 (+ 31 counter))))))

(defclass soak-cyclic-input-stream
    (sb-gray:fundamental-binary-input-stream)
  ((pattern
    :initarg :pattern
    :reader soak-input-pattern)
   (position
    :initform 0
    :accessor soak-input-position)
   (deadline
    :initarg :deadline
    :reader soak-input-deadline)
   (remaining-reads
    :initarg :remaining-reads
    :initform nil
    :accessor soak-input-remaining-reads)
   (maximum-read-bytes
    :initarg :maximum-read-bytes
    :initform nil
    :reader soak-input-maximum-read-bytes)
   (last-read-nanoseconds
    :initform 0
    :accessor soak-input-last-read-nanoseconds)
   (byte-count
    :initform 0
    :accessor soak-input-byte-count)))

(defmethod stream-element-type
    ((stream soak-cyclic-input-stream))
  (declare (ignore stream))
  'octet)

(defun soak-input-finished-p (stream)
  "STREAMがdeadlineまたはread上限へ到達したか返す。"
  (let ((remaining (soak-input-remaining-reads stream)))
    (or (and remaining (zerop remaining))
        (and (soak-input-deadline stream)
             (>= (get-internal-real-time)
                 (soak-input-deadline stream))))))

(defmethod sb-gray:stream-read-byte
    ((stream soak-cyclic-input-stream))
  (let ((octet (make-array 1 :element-type 'octet)))
    (if (= (sb-gray:stream-read-sequence stream octet 0 1) 1)
        (aref octet 0)
        :eof)))

(defmethod sb-gray:stream-read-sequence
    ((stream soak-cyclic-input-stream) sequence
     &optional (start 0) end)
  (when (soak-input-finished-p stream)
    (return-from sb-gray:stream-read-sequence start))
  (let* ((actual-end (or end (length sequence)))
         (pattern (soak-input-pattern stream))
         (count
           (min
            (- actual-end start)
            (or (soak-input-maximum-read-bytes stream)
                (- actual-end start))))
         (written 0))
    (loop while (< written count)
          for source-position = (soak-input-position stream)
          for amount =
            (min (- count written)
                 (- (length pattern) source-position))
          do (replace sequence pattern
                      :start1 (+ start written)
                      :start2 source-position
                      :end2 (+ source-position amount))
             (setf written (+ written amount)
                   (soak-input-position stream)
                   (mod (+ source-position amount)
                        (length pattern))))
    (when (soak-input-remaining-reads stream)
      (decf (soak-input-remaining-reads stream)))
    (incf (soak-input-byte-count stream) count)
    (setf
     (soak-input-last-read-nanoseconds stream)
     (performance-monotonic-nanoseconds))
    (+ start count)))

(defclass soak-pass-through-output-stream
    (sb-gray:fundamental-binary-output-stream)
  ((input
    :initarg :input
    :reader soak-pass-output-input)
   (pattern
    :initarg :pattern
    :reader soak-pass-output-pattern)
   (pattern-position
    :initform 0
    :accessor soak-pass-output-pattern-position)
   (byte-count
    :initform 0
    :accessor soak-pass-output-byte-count)
   (packet-count
    :initform 0
    :accessor soak-pass-output-packet-count)
   (byte-mismatches
    :initform 0
    :accessor soak-pass-output-byte-mismatches)
   (alignment-errors
    :initform 0
    :accessor soak-pass-output-alignment-errors)
   (continuity-errors
    :initform 0
    :accessor soak-pass-output-continuity-errors)
   (last-continuity-counter
    :initform nil
    :accessor soak-pass-output-last-continuity-counter)
   (latency
    :initform (make-soak-latency-histogram)
    :accessor soak-pass-output-latency)
   (rss-sampler
    :initarg :rss-sampler
    :initform nil
    :reader soak-pass-output-rss-sampler)
   (warm-up-writes
    :initarg :warm-up-writes
    :initform nil
    :reader soak-pass-output-warm-up-writes)
   (measurement-writes
    :initarg :measurement-writes
    :initform nil
    :reader soak-pass-output-measurement-writes)
   (write-count
    :initform 0
    :accessor soak-pass-output-write-count)
   (consing-start
    :initform nil
    :accessor soak-pass-output-consing-start)
   (consing-end
    :initform nil
    :accessor soak-pass-output-consing-end)))

(defmethod stream-element-type
    ((stream soak-pass-through-output-stream))
  (declare (ignore stream))
  'octet)

(defmethod sb-gray:stream-write-byte
    ((stream soak-pass-through-output-stream) byte)
  (declare (ignore stream byte))
  (bridge-error "Pass-through soak produced a non-packet byte write"))

(defun inspect-soak-pass-through-packet
    (stream sequence offset)
  "SEQUENCEのOFFSETにあるpass-through packetのCCを検証する。"
  (unless (= (aref sequence offset) +ts-sync-byte+)
    (incf (soak-pass-output-alignment-errors stream)))
  (let ((counter (logand (aref sequence (+ offset 3)) #x0f))
        (last
          (soak-pass-output-last-continuity-counter stream)))
    (when (and last
               (/= counter (logand (+ last 1) #x0f)))
      (incf (soak-pass-output-continuity-errors stream)))
    (setf (soak-pass-output-last-continuity-counter stream)
          counter))
  (incf (soak-pass-output-packet-count stream)))

(defmethod sb-gray:stream-write-sequence
    ((stream soak-pass-through-output-stream) sequence
     &optional (start 0) end)
  (let* ((actual-end (or end (length sequence)))
         (count (- actual-end start))
         (pattern (soak-pass-output-pattern stream)))
    (record-soak-latency
     (soak-pass-output-latency stream)
     (- (performance-monotonic-nanoseconds)
        (soak-input-last-read-nanoseconds
         (soak-pass-output-input stream))))
    (unless (zerop (mod count +ts-packet-size+))
      (incf (soak-pass-output-alignment-errors stream)))
    (loop for offset from start below actual-end
          for pattern-position =
            (soak-pass-output-pattern-position stream)
          do (unless (= (aref sequence offset)
                        (aref pattern pattern-position))
               (incf
                (soak-pass-output-byte-mismatches stream)))
             (setf
              (soak-pass-output-pattern-position stream)
              (mod (+ pattern-position 1)
                   (length pattern))))
    (loop for offset from start below actual-end
            by +ts-packet-size+
          while (<= (+ offset +ts-packet-size+) actual-end)
          do (inspect-soak-pass-through-packet
              stream sequence offset))
    (incf (soak-pass-output-byte-count stream) count)
    (incf (soak-pass-output-write-count stream))
    (let ((warm-up
            (soak-pass-output-warm-up-writes stream)))
      (when (and warm-up
                 (= (soak-pass-output-write-count stream)
                    warm-up))
        (let ((latency
                (make-soak-latency-histogram)))
          (setf (soak-pass-output-latency stream)
                latency
                (soak-pass-output-consing-start stream)
                (sb-ext:get-bytes-consed)))))
    (let ((warm-up
            (soak-pass-output-warm-up-writes stream))
          (measurement
            (soak-pass-output-measurement-writes stream)))
      (when (and warm-up measurement
                 (= (soak-pass-output-write-count stream)
                    (+ warm-up measurement)))
        (setf (soak-pass-output-consing-end stream)
              (sb-ext:get-bytes-consed))))
    (when (soak-pass-output-rss-sampler stream)
      (sample-soak-rss
       (soak-pass-output-rss-sampler stream)))
    sequence))

(defclass soak-ts-monitor-stream
    (sb-gray:fundamental-binary-output-stream)
  ((video-pid
    :initarg :video-pid
    :reader soak-monitor-video-pid)
   (byte-count
    :initform 0
    :accessor soak-monitor-byte-count)
   (packet-count
    :initform 0
    :accessor soak-monitor-packet-count)
   (video-access-unit-count
    :initform 0
    :accessor soak-monitor-video-access-unit-count)
   (alignment-errors
    :initform 0
    :accessor soak-monitor-alignment-errors)
   (continuity-errors
    :initform 0
    :accessor soak-monitor-continuity-errors)
   (last-continuity-counters
    :initform (make-hash-table :test #'eql)
    :reader soak-monitor-last-continuity-counters)
   (latency
    :initform (make-soak-latency-histogram)
    :accessor soak-monitor-latency)
   (pending-latency-start
    :initform nil
    :accessor soak-monitor-pending-latency-start)))

(defmethod stream-element-type ((stream soak-ts-monitor-stream))
  (declare (ignore stream))
  'octet)

(defmethod sb-gray:stream-write-byte
    ((stream soak-ts-monitor-stream) byte)
  (declare (ignore stream byte))
  (bridge-error "Semantic soak produced a non-packet byte write"))

(defun soak-sequence-pid (sequence offset)
  "SEQUENCEのOFFSETにあるTS packetからPIDを読む。"
  (logior (ash (logand (aref sequence (+ offset 1))
                       #x1f)
               8)
          (aref sequence (+ offset 2))))

(defun soak-sequence-has-payload-p (sequence offset)
  "SEQUENCEのOFFSETにあるTS packetがpayloadを持つか返す。"
  (logbitp 4 (aref sequence (+ offset 3))))

(defun soak-sequence-discontinuity-p (sequence offset)
  "SEQUENCEのOFFSETにあるTS packetのdiscontinuityを返す。"
  (and (logbitp 5 (aref sequence (+ offset 3)))
       (plusp (aref sequence (+ offset 4)))
       (logbitp 7 (aref sequence (+ offset 5)))))

(defun inspect-soak-semantic-packet
    (stream sequence offset)
  "SEQUENCEのOFFSETにあるpacketのsync、CC、PUSIを集計する。"
  (unless (= (aref sequence offset) +ts-sync-byte+)
    (incf (soak-monitor-alignment-errors stream))
    (return-from inspect-soak-semantic-packet nil))
  (let ((pid (soak-sequence-pid sequence offset)))
    (when (and (= pid (soak-monitor-video-pid stream))
               (logbitp 6 (aref sequence (+ offset 1))))
      (incf (soak-monitor-video-access-unit-count stream)))
    (when (soak-sequence-has-payload-p sequence offset)
      (let* ((counters
               (soak-monitor-last-continuity-counters stream))
             (counter
               (logand (aref sequence (+ offset 3)) #x0f))
             (last (gethash pid counters)))
        (when (and last
                   (not
                    (soak-sequence-discontinuity-p
                     sequence offset))
                   (/= counter (logand (+ last 1) #x0f)))
          (incf (soak-monitor-continuity-errors stream)))
        (setf (gethash pid counters) counter))))
  (incf (soak-monitor-packet-count stream))
  t)

(defmethod sb-gray:stream-write-sequence
    ((stream soak-ts-monitor-stream) sequence
     &optional (start 0) end)
  (let ((actual-end (or end (length sequence))))
    (when (soak-monitor-pending-latency-start stream)
      (record-soak-latency
       (soak-monitor-latency stream)
       (- (performance-monotonic-nanoseconds)
          (soak-monitor-pending-latency-start stream)))
      (setf (soak-monitor-pending-latency-start stream) nil))
    (unless (zerop (mod (- actual-end start)
                        +ts-packet-size+))
      (incf (soak-monitor-alignment-errors stream)))
    (loop for offset from start below actual-end
            by +ts-packet-size+
          while (<= (+ offset +ts-packet-size+) actual-end)
          do (inspect-soak-semantic-packet
              stream sequence offset))
    (incf (soak-monitor-byte-count stream)
          (- actual-end start))
    sequence))

(defun reset-soak-semantic-monitor (monitor)
  "MONITORのwarm-up後metricsをCC状態だけ残して初期化する。"
  (setf
   (soak-monitor-byte-count monitor) 0
   (soak-monitor-packet-count monitor) 0
   (soak-monitor-video-access-unit-count monitor) 0
   (soak-monitor-alignment-errors monitor) 0
   (soak-monitor-continuity-errors monitor) 0
   (soak-monitor-latency monitor)
   (make-soak-latency-histogram)
   (soak-monitor-pending-latency-start monitor) nil)
  monitor)

(defun make-soak-semantic-cycle (packet-function)
  "PACKET-FUNCTIONからCC連続な16 access unit cycleを作る。"
  (let ((continuity-counter 0)
        (pts 90000)
        (packet-index 0)
        (cycle '()))
    (loop repeat +soak-cycle-access-unit-count+
          do
      (let ((packets
              (funcall packet-function
                       pts continuity-counter
                       packet-index)))
        (push packets cycle)
        (setf continuity-counter
              (logand
               (+ continuity-counter (length packets))
               #x0f)
              packet-index
              (+ packet-index (length packets))
              pts (mod (+ pts 3600) +pts-modulus+)))
          finally (return (nreverse cycle)))))

(defun process-soak-access-unit (processor monitor packets)
  "PACKETSを1 access unitとして処理し先頭出力遅延を測る。"
  (setf (soak-monitor-pending-latency-start monitor)
        (performance-monotonic-nanoseconds))
  (process-bridge-packet processor (first packets))
  (when (soak-monitor-pending-latency-start monitor)
    (bridge-error
     "Soak access unit did not stream before the next PUSI"))
  (dolist (packet (rest packets))
    (process-bridge-packet processor packet))
  t)

(defun warm-up-soak-semantic-processor
    (processor monitor cycle)
  "PROCESSORを十分warm-upしMONITORの測定値を初期化する。"
  (loop repeat 8
        do (dolist (packets cycle)
             (process-soak-access-unit
              processor monitor packets)))
  (reset-soak-semantic-monitor monitor)
  t)

(defun run-semantic-soak
    (codec packet-function duration-seconds
     rss-sample-seconds rss-growth-limit-kib
     rss-slope-limit-kib-per-hour)
  "CODECの逐次変換をDURATION-SECONDS連続検証する。"
  (let* ((monitor
           (make-instance
            'soak-ts-monitor-stream
            :video-pid +test-video-pid+))
         (processor
           (make-bridge-processor
            monitor codec :aac
            :transport-rate-kbps
            (when (eq codec :av1)
              +test-transport-rate-kbps+)))
         (cycle
           (make-soak-semantic-cycle packet-function))
         (interval-ticks
           (max 1
                (round
                 (* rss-sample-seconds
                    internal-time-units-per-second))))
         (rss
           (make-soak-rss-sampler
            interval-ticks rss-growth-limit-kib
            rss-slope-limit-kib-per-hour
            (>=
             duration-seconds
             +soak-minimum-rss-slope-duration-seconds+)))
         (duration-ticks
           (max 1
                (round
                 (* duration-seconds
                    internal-time-units-per-second))))
         (input-access-units 0)
         (cycle-index 0))
    (dolist (packet
             (append
              (make-test-pat-packets)
              (make-test-pmt-packets :two-audio-p nil)))
      (process-bridge-packet processor packet))
    (warm-up-soak-semantic-processor
     processor monitor cycle)
    (sample-soak-rss rss t)
    (let ((consing-start (sb-ext:get-bytes-consed))
          (gc-start sb-ext:*gc-run-time*)
          (deadline (+ (get-internal-real-time)
                       duration-ticks)))
      (loop while (< (get-internal-real-time) deadline)
            do (process-soak-access-unit
                processor monitor
                (nth cycle-index cycle))
               (incf input-access-units)
               (setf cycle-index
                     (mod (+ cycle-index 1)
                          (length cycle)))
               (sample-soak-rss rss))
      (finish-bridge-processor processor)
      (sample-soak-rss rss t)
      (let* ((gc-runtime
               (- sb-ext:*gc-run-time* gc-start))
             (consed
               (- (sb-ext:get-bytes-consed)
                  consing-start))
             (packet-loss
               (- input-access-units
                  (soak-monitor-video-access-unit-count monitor)))
             (latency
               (soak-latency-result
                (soak-monitor-latency monitor)))
             (rss-result
               (soak-rss-result
                rss
                (>=
                 duration-seconds
                 +soak-minimum-rss-slope-duration-seconds+)))
             (rss-before-full-gc
               (soak-rss-sampler-last-kib rss))
             (rss-after-full-gc
               (progn
                 (sb-ext:gc :full t)
                 (read-linux-rss-kib)))
             (passed
               (and
                (zerop packet-loss)
                (zerop
                 (soak-monitor-alignment-errors monitor))
                (zerop
                 (soak-monitor-continuity-errors monitor))
                (getf latency :latency-p99-under-2ms-p)
                (getf rss-result
                      :rss-gate-passed-p))))
        (append-plists
         (list
          :mode codec
          :duration-seconds duration-seconds
          :input-access-units input-access-units
          :output-access-units
          (soak-monitor-video-access-unit-count monitor)
          :packet-loss packet-loss
          :output-packets
          (soak-monitor-packet-count monitor)
          :output-bytes
          (soak-monitor-byte-count monitor)
          :alignment-errors
          (soak-monitor-alignment-errors monitor)
          :continuity-errors
          (soak-monitor-continuity-errors monitor)
          :bytes-consed consed
          :bytes-consed-per-access-unit
          (if (plusp input-access-units)
              (/ consed input-access-units)
              consed)
          :gc-runtime-ms
          (soak-ticks-to-milliseconds gc-runtime)
          :rss-before-full-gc-kib rss-before-full-gc
          :rss-after-full-gc-kib rss-after-full-gc)
         latency rss-result
         (list :passed passed))))))

(defun run-pass-through-copy
    (&key duration-seconds read-count warm-up-writes
          measurement-writes
          rss-sample-seconds rss-growth-limit-kib
          rss-slope-limit-kib-per-hour
          maximum-read-packets (validated-p t))
  "productionの検証付きfast pathを時間またはread回数上限まで検証する。"
  (let* ((pattern (make-soak-pass-through-pattern))
         (duration-ticks
           (when duration-seconds
             (max 1
                  (round
                   (* duration-seconds
                      internal-time-units-per-second)))))
         (deadline
           (when duration-ticks
             (+ (get-internal-real-time)
                duration-ticks)))
         (rss
           (when rss-sample-seconds
             (make-soak-rss-sampler
              (max 1
                   (round
                    (* rss-sample-seconds
                       internal-time-units-per-second)))
             rss-growth-limit-kib
              rss-slope-limit-kib-per-hour
              (>=
               duration-seconds
               +soak-minimum-rss-slope-duration-seconds+))))
         (input
           (make-instance
            'soak-cyclic-input-stream
            :pattern pattern
            :deadline deadline
            :remaining-reads read-count
            :maximum-read-bytes
            (when maximum-read-packets
              (* maximum-read-packets
                 +ts-packet-size+))))
         (output
           (make-instance
            'soak-pass-through-output-stream
            :input input
            :pattern pattern
            :rss-sampler rss
            :warm-up-writes warm-up-writes
            :measurement-writes measurement-writes)))
    (when rss
      (sample-soak-rss rss t))
    (if validated-p
        (validate-and-copy-ts-stream input output)
        (copy-binary-stream input output))
    (when rss
      (sample-soak-rss rss t))
    (values input output rss)))

(defun measure-soak-fast-path-consing
    (read-count &key maximum-read-packets (validated-p t))
  "warm-up後READ-COUNT回のfast path consingを返す。"
  (let ((warm-up-writes 256))
    (multiple-value-bind (input output rss)
        (run-pass-through-copy
         :read-count (+ warm-up-writes read-count)
         :warm-up-writes warm-up-writes
         :measurement-writes read-count
         :maximum-read-packets maximum-read-packets
         :validated-p validated-p)
      (declare (ignore input rss))
      (unless (soak-pass-output-consing-start output)
        (bridge-error "Fast-path consing warm-up did not complete"))
      (unless (soak-pass-output-consing-end output)
        (bridge-error "Fast-path consing measurement did not complete"))
      (- (soak-pass-output-consing-end output)
         (soak-pass-output-consing-start output)))))

(defun run-pass-through-latency-probe
    (profile packets-per-read repetition read-count)
  "PROFILEのchunk幅で検証付きfast path latencyを反復測定する。"
  (let ((baseline-consed
          (measure-soak-fast-path-consing
           read-count
           :maximum-read-packets packets-per-read
           :validated-p nil))
        (warm-up-writes 256)
        (gc-start sb-ext:*gc-run-time*))
    (multiple-value-bind (input output rss)
        (run-pass-through-copy
         :read-count (+ warm-up-writes read-count)
         :warm-up-writes warm-up-writes
         :measurement-writes read-count
         :maximum-read-packets packets-per-read)
      (declare (ignore rss))
      (let* ((consing-start
               (soak-pass-output-consing-start output))
             (consed
               (if (and consing-start
                        (soak-pass-output-consing-end output))
                   (- (soak-pass-output-consing-end output)
                      consing-start)
                   -1))
             (latency
               (soak-latency-result
                (soak-pass-output-latency output)))
             (packet-loss
               (- (floor (soak-input-byte-count input)
                         +ts-packet-size+)
                  (soak-pass-output-packet-count output)))
             (consing-overhead
               (max 0 (- consed baseline-consed)))
             (consing-overhead-limit
               (* read-count
                  +soak-validation-consing-limit-per-read+))
             (passed
               (and
                (zerop packet-loss)
                (zerop
                 (soak-pass-output-byte-mismatches output))
                (zerop
                 (soak-pass-output-alignment-errors output))
                (zerop
                 (soak-pass-output-continuity-errors output))
                (<= consing-overhead
                    consing-overhead-limit)
                (getf latency
                      :latency-p99-under-2ms-p))))
        (append-plists
         (list
          :mode :latency-probe
          :profile profile
          :repetition repetition
          :packets-per-read packets-per-read
          :input-packets
          (floor (soak-input-byte-count input)
                 +ts-packet-size+)
          :packet-loss packet-loss
          :continuity-errors
          (soak-pass-output-continuity-errors output)
          :baseline-bytes-consed baseline-consed
          :bytes-consed consed
          :validation-overhead-bytes consing-overhead
          :validation-overhead-limit-bytes
          consing-overhead-limit
          :validation-overhead-within-limit-p
          (<= consing-overhead consing-overhead-limit)
          :gc-runtime-ms
          (soak-ticks-to-milliseconds
           (- sb-ext:*gc-run-time* gc-start)))
         latency
         (list :passed passed))))))

(defun run-pass-through-latency-matrix
    (repetitions read-count)
  "通常・低遅延・超低遅延相当のchunk幅を複数反復する。"
  (let ((results '()))
    ;; SBCLが初回のGray stream dispatchなどを遅延初期化する場合がある。
    ;; 測定反復へその一度限りのallocationを混ぜないよう、同じ経路を
    ;; 1回完走させてから各profileを測る。
    (run-pass-through-latency-probe
     :warm-up 64 0 read-count)
    (dolist (profile
             '((:normal 64)
               (:low-latency 8)
               (:ultra-low-latency 1)))
      (dotimes (repetition repetitions)
        (push
         (run-pass-through-latency-probe
          (first profile)
          (second profile)
          (+ repetition 1)
          read-count)
         results)))
    (nreverse results)))

(defun run-pass-through-soak
    (duration-seconds rss-sample-seconds
     rss-growth-limit-kib rss-slope-limit-kib-per-hour
     consing-read-count)
  "検証付きbyte-exact fast pathを長時間検証しconsingも測る。"
  (let ((baseline-consed
          (measure-soak-fast-path-consing
           consing-read-count
           :validated-p nil))
        (consed
          (measure-soak-fast-path-consing
           consing-read-count)))
    (let ((gc-start sb-ext:*gc-run-time*))
      (multiple-value-bind (input output rss)
          (run-pass-through-copy
           :duration-seconds duration-seconds
           :rss-sample-seconds rss-sample-seconds
           :rss-growth-limit-kib rss-growth-limit-kib
           :rss-slope-limit-kib-per-hour
           rss-slope-limit-kib-per-hour)
        (let* ((gc-runtime
                 (- sb-ext:*gc-run-time* gc-start))
               (latency
               (soak-latency-result
                (soak-pass-output-latency output)))
             (rss-result
               (soak-rss-result
                rss
                (>=
                 duration-seconds
                 +soak-minimum-rss-slope-duration-seconds+)))
             (rss-before-full-gc
               (soak-rss-sampler-last-kib rss))
             (rss-after-full-gc
               (progn
                 (sb-ext:gc :full t)
                 (read-linux-rss-kib)))
             (packet-loss
               (- (floor (soak-input-byte-count input)
                         +ts-packet-size+)
                  (soak-pass-output-packet-count output)))
             (consing-overhead
               (max 0 (- consed baseline-consed)))
             (consing-overhead-limit
               (* consing-read-count
                  +soak-validation-consing-limit-per-read+))
             (passed
               (and
                (zerop packet-loss)
                (= (soak-input-byte-count input)
                   (soak-pass-output-byte-count output))
                (zerop
                 (soak-pass-output-byte-mismatches output))
                (zerop
                 (soak-pass-output-alignment-errors output))
                (zerop
                 (soak-pass-output-continuity-errors output))
                (<= consing-overhead
                    consing-overhead-limit)
                (getf latency :latency-p99-under-2ms-p)
                (getf rss-result
                      :rss-gate-passed-p))))
        (append-plists
         (list
          :mode :validated-fast-path
          :duration-seconds duration-seconds
          :input-bytes
          (soak-input-byte-count input)
          :output-bytes
          (soak-pass-output-byte-count output)
          :output-packets
          (soak-pass-output-packet-count output)
          :packet-loss packet-loss
          :byte-mismatches
          (soak-pass-output-byte-mismatches output)
          :alignment-errors
          (soak-pass-output-alignment-errors output)
          :continuity-errors
          (soak-pass-output-continuity-errors output)
          :fast-path-measured-reads consing-read-count
          :fast-path-baseline-bytes-consed baseline-consed
          :fast-path-bytes-consed consed
          :fast-path-validation-overhead-bytes
          consing-overhead
          :fast-path-validation-overhead-limit-bytes
          consing-overhead-limit
          :fast-path-validation-overhead-within-limit-p
          (<= consing-overhead consing-overhead-limit)
          :gc-runtime-ms
          (soak-ticks-to-milliseconds gc-runtime)
          :rss-before-full-gc-kib rss-before-full-gc
          :rss-after-full-gc-kib rss-after-full-gc)
         latency rss-result
         (list :passed passed)))))))

(defun run-soak-suite
    (mode duration-seconds
     &key (rss-sample-seconds 10)
          (rss-growth-limit-kib 16384)
          (rss-slope-limit-kib-per-hour 1024)
          (consing-read-count 10000)
          (latency-repetitions 3)
          (latency-read-count 10000))
  "MODEの再現可能な長時間gateを実行し結果plist列を返す。"
  (unless (and (realp duration-seconds)
               (plusp duration-seconds))
    (bridge-error "Soak duration must be positive"))
  (unless (and (realp rss-sample-seconds)
               (plusp rss-sample-seconds))
    (bridge-error "Soak RSS sample interval must be positive"))
  (unless (and (integerp rss-growth-limit-kib)
               (>= rss-growth-limit-kib 0))
    (bridge-error "Soak RSS growth limit must be non-negative"))
  (unless (and (integerp rss-slope-limit-kib-per-hour)
               (>= rss-slope-limit-kib-per-hour 0))
    (bridge-error "Soak RSS slope limit must be non-negative"))
  (unless (and (integerp consing-read-count)
               (plusp consing-read-count))
    (bridge-error "Soak consing read count must be positive"))
  (unless (and (integerp latency-repetitions)
               (plusp latency-repetitions))
    (bridge-error "Soak latency repetitions must be positive"))
  (unless (and (integerp latency-read-count)
               (plusp latency-read-count))
    (bridge-error "Soak latency read count must be positive"))
  (unless (member mode
                  '(:pass-through :vp9 :av1 :all)
                  :test #'eq)
    (bridge-error "Unsupported soak mode: ~S" mode))
  (let ((results '()))
    (when (member mode '(:pass-through :all) :test #'eq)
      (setf results
            (nconc
             results
             (run-pass-through-latency-matrix
              latency-repetitions latency-read-count)))
      (setf results
            (nconc
             results
             (list
              (run-pass-through-soak
               duration-seconds rss-sample-seconds
               rss-growth-limit-kib
               rss-slope-limit-kib-per-hour
               consing-read-count)))))
    (when (member mode '(:vp9 :all) :test #'eq)
      (push
       (run-semantic-soak
        :vp9 #'make-large-zero-length-vp9-packets
        duration-seconds rss-sample-seconds
        rss-growth-limit-kib
        rss-slope-limit-kib-per-hour)
       results))
    (when (member mode '(:av1 :all) :test #'eq)
      (push
       (run-semantic-soak
        :av1 #'make-large-zero-length-av1-packets
        duration-seconds rss-sample-seconds
        rss-growth-limit-kib
        rss-slope-limit-kib-per-hour)
       results))
    results))
