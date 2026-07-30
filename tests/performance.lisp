;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defconstant +performance-nanoseconds-per-second+ 1000000000)
(defconstant +performance-monotonic-raw-clock-id+ 4)
(defconstant +benchmark-cbr-pcr-interval-packets+ 100)
(defconstant +benchmark-warm-up-sample-count+ 10)

(declaim
 (inline performance-clock-nanoseconds
         performance-monotonic-nanoseconds
         performance-thread-cpu-nanoseconds))

(defun performance-clock-nanoseconds (clock-id)
  "Linux CLOCK-IDの時刻をnanosecondで返す。"
  (multiple-value-bind (seconds nanoseconds)
      (sb-unix::clock-gettime clock-id)
    (+ (* seconds +performance-nanoseconds-per-second+)
       nanoseconds)))

(defun performance-monotonic-nanoseconds ()
  "CLOCK_MONOTONIC_RAWのwall clockをnanosecondで返す。"
  (performance-clock-nanoseconds
   +performance-monotonic-raw-clock-id+))

(defun performance-thread-cpu-nanoseconds ()
  "現在threadが消費したCPU時刻をnanosecondで返す。"
  (performance-clock-nanoseconds
   sb-unix::clock-thread-cputime-id))

(defclass benchmark-counting-output-stream
    (sb-gray:fundamental-binary-output-stream)
  ((byte-count
    :initform 0
    :accessor benchmark-output-byte-count)
   (sample-start-wall-nanoseconds
    :initform nil
    :accessor benchmark-output-sample-start-wall-nanoseconds)
   (sample-start-cpu-nanoseconds
    :initform nil
    :accessor benchmark-output-sample-start-cpu-nanoseconds)
   (first-output-wall-nanoseconds
    :initform nil
    :accessor benchmark-output-first-output-wall-nanoseconds)
   (first-output-cpu-nanoseconds
    :initform nil
    :accessor benchmark-output-first-output-cpu-nanoseconds)))

(defmethod stream-element-type
    ((stream benchmark-counting-output-stream))
  (declare (ignore stream))
  'octet)

(defun record-benchmark-first-output (stream)
  "有効なsampleの最初のwrite時刻だけをSTREAMへ記録する。"
  (when
      (and
       (benchmark-output-sample-start-wall-nanoseconds stream)
       (null
        (benchmark-output-first-output-wall-nanoseconds stream)))
    (setf
     (benchmark-output-first-output-wall-nanoseconds stream)
     (performance-monotonic-nanoseconds)
     (benchmark-output-first-output-cpu-nanoseconds stream)
     (performance-thread-cpu-nanoseconds)))
  stream)

(defmethod sb-gray:stream-write-byte
    ((stream benchmark-counting-output-stream) byte)
  (record-benchmark-first-output stream)
  (incf (benchmark-output-byte-count stream))
  byte)

(defmethod sb-gray:stream-write-sequence
    ((stream benchmark-counting-output-stream) sequence
     &optional (start 0) end)
  (record-benchmark-first-output stream)
  (incf
   (benchmark-output-byte-count stream)
   (- (or end (length sequence)) start))
  sequence)

(defun nanoseconds-between-milliseconds (start end)
  "nanosecond表現のSTARTからENDをmillisecondへ変換する。"
  (/ (- end start) 1000000.0d0))

(defun begin-benchmark-first-output-sample (stream)
  "STREAMの次のwriteまでを測るsampleを開始する。"
  (setf
   (benchmark-output-first-output-wall-nanoseconds stream) nil
   (benchmark-output-first-output-cpu-nanoseconds stream) nil
   (benchmark-output-sample-start-wall-nanoseconds stream)
   (performance-monotonic-nanoseconds)
   (benchmark-output-sample-start-cpu-nanoseconds stream)
   (performance-thread-cpu-nanoseconds))
  stream)

(defun finish-benchmark-first-output-sample (stream)
  "STREAMで記録したwall/CPU first-output時間を返す。"
  (let ((wall-start
          (benchmark-output-sample-start-wall-nanoseconds stream))
        (wall-end
          (benchmark-output-first-output-wall-nanoseconds stream))
        (cpu-start
          (benchmark-output-sample-start-cpu-nanoseconds stream))
        (cpu-end
          (benchmark-output-first-output-cpu-nanoseconds stream)))
    (setf
     (benchmark-output-sample-start-wall-nanoseconds stream) nil
     (benchmark-output-sample-start-cpu-nanoseconds stream) nil)
    (when wall-end
      (values
       (nanoseconds-between-milliseconds
        wall-start wall-end)
       (nanoseconds-between-milliseconds
        cpu-start cpu-end)))))

(defun percentile-value (values percentile)
  "VALUESのnearest-rank PERCENTILE値を返す。"
  (let* ((sorted (sort (copy-list values) #'<))
         (rank
           (max 1
                (ceiling (* percentile
                            (length sorted)))))
         (index (- rank 1)))
    (nth index sorted)))

(defun make-large-zero-length-vp9-pes (pts)
  "benchmark用70KB超VP9 PES_packet_length=0を作る。"
  (let* ((payload
           (concatenate-octets
            (make-test-vp9-key-frame 0 3840 2160)
            (make-pattern-octets 70000 83)))
         (last-index (- (length payload) 1)))
    (setf (aref payload last-index) #x55)
    (make-pes #xbd payload pts)))

(defun make-large-zero-length-vp9-packets
    (pts continuity-counter &optional first-packet-index)
  "benchmark用PESをTS packet化する。"
  (declare (ignore first-packet-index))
  (packetize-test-video-with-pcr
   (make-large-zero-length-vp9-pes pts)
   :continuity-counter continuity-counter
   :discontinuity (zerop continuity-counter)))

(defun benchmark-cbr-pcr (packet-index)
  "2.2Mbit/s CBR上のPACKET-INDEXに対応するPCRを返す。"
  (mod
   (round
    (*
     packet-index
     +ts-packet-size+
     8
     +tstd-system-clock-rate+)
    (* +test-transport-rate-kbps+ 1000))
   +pcr-modulus+))

(defun benchmark-cbr-pts (packet-index)
  "2.2Mbit/s CBR上のPACKET-INDEXから1秒後のPTSを返す。"
  (mod
   (+ (floor (benchmark-cbr-pcr packet-index) 300)
      +test-tstd-removal-delay-ticks+)
   +pts-modulus+))

(defun benchmark-cbr-pcr-packet-p (packet-index first-p)
  "PACKET-INDEXへPCRを置くか返す。PUSIには常に置く。"
  (or first-p
      (zerop
       (mod packet-index
            +benchmark-cbr-pcr-interval-packets+))))

(defun packetize-benchmark-av1-pes
    (pes continuity-counter first-packet-index)
  "PESを2.2Mbit/s CBR PCR付きTS packet列へする。"
  (let ((packets '())
        (offset 0)
        (counter continuity-counter)
        (packet-index first-packet-index)
        (first-p t))
    (loop while (< offset (length pes))
          for pcr-p =
            (benchmark-cbr-pcr-packet-p packet-index first-p)
          for capacity =
            (cond
              (first-p 170)
              (pcr-p 176)
              (t 184))
          for count =
            (min capacity (- (length pes) offset))
          for packet =
            (make-ts-packet
             +test-video-pid+ counter
             (subseq pes offset (+ offset count))
             :payload-unit-start first-p
             :transport-priority first-p)
          do
             (when (and first-p
                        (zerop first-packet-index))
               (setf (aref packet 5)
                     (logior (aref packet 5) #x80)))
             (when pcr-p
               (set-test-pcr
                packet
                (benchmark-cbr-pcr packet-index)))
             (push packet packets)
             (incf offset count)
             (setf
              counter (logand (+ counter 1) #x0f)
              packet-index (+ packet-index 1)
              first-p nil))
    (nreverse packets)))

(defun make-large-zero-length-av1-access-unit ()
  "benchmark用64KiB超AV1 access unitを作る。"
  (let* ((sequence
           (make-av1-structure-test-sequence :level 8))
         (minimum-frame
           (make-av1-structure-test-key-frame))
         (frame-obu
           (first (parse-av1-obus minimum-frame)))
         (frame-payload
           (concatenate-octets
            (subseq
             minimum-frame
             (av1-obu-payload-start frame-obu)
             (av1-obu-end frame-obu))
            (make-pattern-octets 69000 109))))
    (concatenate-octets
     sequence
     (make-av1-structure-test-obu 6 frame-payload))))

(defun make-large-zero-length-av1-pes (pts)
  "benchmark用70KB超AV1 PES_packet_length=0を作る。"
  (make-pes
   #xe0
   (make-large-zero-length-av1-access-unit)
   pts))

(defun make-large-zero-length-av1-packets
    (pts continuity-counter &optional (first-packet-index 0))
  "benchmark用AV1 PESを明示2.2Mbit/s CBRでTS packet化する。"
  (declare (ignore pts))
  (packetize-benchmark-av1-pes
   (make-large-zero-length-av1-pes
    (benchmark-cbr-pts first-packet-index))
   continuity-counter
   first-packet-index))

(define-bridge-test av1-zero-length-large-pes-streams-before-next-pusi
  (let* ((access-unit
           (make-large-zero-length-av1-access-unit))
         (source-pes
           (make-pes #xe0 access-unit 90000))
         (video-packets
           (packetize-test-video-with-pcr
            source-pes
            :continuity-counter 0
            :pcr-lead-ticks
            +test-tstd-removal-delay-ticks+))
         (output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor
            output :av1 :aac
            :transport-rate-kbps
            +test-transport-rate-kbps+)))
    (dolist (packet
             (append
              (make-test-pat-packets)
              (make-test-pmt-packets :two-audio-p nil)))
      (process-bridge-packet processor packet))
    (let ((length-before
            (length (octet-collector-data output))))
      (process-bridge-packet processor (first video-packets))
      (check-bridge-test
       (> (length (octet-collector-data output))
          length-before)))
    (dolist (packet (rest video-packets))
      (process-bridge-packet processor packet))
    (finish-bridge-processor processor)
    (let* ((output-packets
             (octets-to-packet-list (collected-octets output)))
           (rebuilt
             (first-unbounded-pes-on-pid
              output-packets +test-video-pid+))
           (header (parse-pes-header rebuilt)))
      (check-bridge-test (zerop (read-u16-be source-pes 4)))
      (check-bridge-test
       (= (pes-header-stream-id header) #xbd))
      (check-bridge-test
       (equalp
        (subseq rebuilt (pes-header-payload-offset header))
        (convert-av1-access-unit-to-ts-format access-unit)))
      (check-bridge-test
       (payload-continuity-valid-p
        output-packets +test-video-pid+)))))

(defun run-codec-large-pes-benchmark
    (codec packet-function sample-count)
  "CODECの長さ0 PESから先頭出力までの時間を測定する。"
  (unless (and (integerp sample-count)
               (>= sample-count 100))
    (bridge-error
     "Benchmark sample count must be at least 100"))
  (let* ((output
           (make-instance 'benchmark-counting-output-stream))
         (processor
           (make-bridge-processor
            output codec :aac
            :transport-rate-kbps
            (when (eq codec :av1)
              +test-transport-rate-kbps+)))
         (continuity-counter 0)
         (pts 90000)
         (packet-index 0)
         (current
           (funcall
            packet-function pts continuity-counter packet-index))
         (samples '())
         (cpu-samples '())
         (streamed-before-next-pusi-p t))
    (dolist (packet
             (append
              (make-test-pat-packets)
              (make-test-pmt-packets :two-audio-p nil)))
      (process-bridge-packet processor packet))
    (loop for sample-index from
            (- +benchmark-warm-up-sample-count+)
              below sample-count
          do
      (let ((output-length
              (benchmark-output-byte-count output))
            (measurement-p
              (not (minusp sample-index))))
        (when measurement-p
          (begin-benchmark-first-output-sample output))
        (process-bridge-packet processor (first current))
        (when measurement-p
          (multiple-value-bind (wall-latency cpu-latency)
              (finish-benchmark-first-output-sample output)
            (cond
              (wall-latency
               (push wall-latency samples)
               (push cpu-latency cpu-samples))
              (t
               (push 2.0d0 samples)
               (push 2.0d0 cpu-samples)
               (setf streamed-before-next-pusi-p nil)))))
        (unless
            (> (benchmark-output-byte-count output)
               output-length)
          (setf streamed-before-next-pusi-p nil)))
      (dolist (packet (rest current))
        (process-bridge-packet processor packet))
      (setf continuity-counter
            (logand (+ continuity-counter (length current)) #x0f)
            packet-index
            (+ packet-index (length current))
            pts (mod (+ pts 3600) +pts-modulus+))
      (setf current
            (funcall packet-function
                     pts continuity-counter packet-index)))
    (finish-bridge-processor processor)
    (let ((p50 (percentile-value samples 0.50d0))
          (p95 (percentile-value samples 0.95d0))
          (p99 (percentile-value samples 0.99d0))
          (maximum (apply #'max samples))
          (cpu-p99
            (percentile-value cpu-samples 0.99d0))
          (cpu-maximum (apply #'max cpu-samples)))
      (list
       :sample-count sample-count
       :warm-up-sample-count
       +benchmark-warm-up-sample-count+
       :au-payload-bytes 70000
       :clock :clock-monotonic-raw
       :auxiliary-clock :thread-cpu-clock-gettime
       :input-packet-count
       (length (funcall packet-function 0 0 0))
       :first-output-p50-ms p50
       :first-output-p95-ms p95
       :first-output-p99-ms p99
       :first-output-max-ms maximum
       :first-output-thread-cpu-p99-ms cpu-p99
       :first-output-thread-cpu-max-ms cpu-maximum
       :streams-before-next-pusi streamed-before-next-pusi-p
       :p99-under-2ms-p
       (and streamed-before-next-pusi-p
            (< p99 2.0d0))))))

(defun run-large-pes-benchmark (&optional (sample-count 100))
  "VP9のPES_packet_length=0先頭出力時間を測定する。"
  (run-codec-large-pes-benchmark
   :vp9 #'make-large-zero-length-vp9-packets sample-count))

(defun run-large-av1-pes-benchmark (&optional (sample-count 100))
  "AV1のPES_packet_length=0先頭出力時間を測定する。"
  (run-codec-large-pes-benchmark
   :av1 #'make-large-zero-length-av1-packets sample-count))
