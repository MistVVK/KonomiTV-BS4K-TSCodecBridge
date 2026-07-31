;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defconstant +test-pmt-pid+ #x100)
(defconstant +test-video-pid+ #x101)
(defconstant +test-audio-one-pid+ #x102)
(defconstant +test-audio-two-pid+ #x103)
(defconstant +test-timed-id3-pid+ #x104)
(defconstant +test-data-pid+ #x105)
(defconstant +test-subtitle-pid+ #x106)
(defconstant +test-transport-rate-kbps+ 2200)
(defconstant +test-tstd-removal-delay-ticks+ 90000)

(defun make-section-ts-packets (pid section continuity-counter)
  "SECTIONをpointer field 0付きTS packet列へする。"
  (let ((payload
          (make-array (+ (length section) 1)
                      :element-type 'octet
                      :initial-element 0)))
    (replace payload section :start1 1)
    (packetize-payload
     pid payload
     :continuity-counter continuity-counter
     :payload-unit-start t)))

(defun make-test-pat-packets ()
  "processor fixture用PAT packet列を作る。"
  (make-section-ts-packets
   0
   (build-pat-section
    (make-program-association-table
     :transport-stream-id 1
     :version 2
     :programs
     (list
      (make-pat-program
       :program-number 1
       :pid +test-pmt-pid+))))
   0))

(defun make-test-opus-descriptors (channel-configuration
                                   &key registration)
  "processor fixture用Opus descriptor列を作る。"
  (append
   (when registration
     (list
      (make-descriptor
       :tag #x05
       :payload (octets #x4f #x70 #x75 #x73))))
   (list
    (make-descriptor
     :tag #x7f
     :payload (octets #x80 channel-configuration)))))

(defun make-test-pmt-table (&key (version 7)
                                 (two-audio-p t)
                                 (opus-registration-p t))
  "raw private videoとOpusを持つprocessor fixture PMTを作る。"
  (make-program-map-table
   :program-number 1
   :version version
   :pcr-pid +test-video-pid+
   :streams
   (append
    (list
     (make-pmt-stream
      :stream-type #x06
      :elementary-pid +test-video-pid+)
     (make-pmt-stream
      :stream-type #x06
      :elementary-pid +test-audio-one-pid+
      :descriptors
      (make-test-opus-descriptors
       2 :registration opus-registration-p)))
    (when two-audio-p
      (list
       (make-pmt-stream
        :stream-type #x06
        :elementary-pid +test-audio-two-pid+
        :descriptors
        (make-test-opus-descriptors
         1 :registration opus-registration-p))))
    (list
     (make-pmt-stream
      :stream-type #x06
      :elementary-pid +test-subtitle-pid+
      :descriptors
      (list
       (make-descriptor
        :tag #xfd
        :payload (octets #x00 #x08))))
     (make-pmt-stream
      :stream-type #x15
      :elementary-pid +test-timed-id3-pid+)
     (make-pmt-stream
      :stream-type #x0d
      :elementary-pid +test-data-pid+)))))

(defun make-test-pmt-packets (&rest arguments)
  "ARGUMENTSのfixture PMT packet列を作る。"
  (make-section-ts-packets
   +test-pmt-pid+
   (build-pmt-section
    (apply #'make-test-pmt-table arguments))
   4))

(defun make-test-opus-pes (pts raw-packet)
  "1 control packetを持つFFmpeg形式Opus PESを作る。"
  (let ((payload
          (concatenate-octets
           (octets #x7f #xe0 (length raw-packet))
           raw-packet)))
    (make-pes #xbd payload pts :data-alignment t)))

(defun make-pattern-octets (length seed)
  "LENGTH byteの決定的patternを作る。"
  (let ((result
          (make-array length :element-type 'octet)))
    (loop for position from 0 below length
          do (setf (aref result position)
                   (logand (+ seed (* position 29)) #xff)))
    result))

(defun packet-list-to-octets (packets)
  "PACKETSを1本のTS byte vectorへ連結する。"
  (apply #'concatenate-octets packets))

(defun set-test-pcr (packet pcr)
  "PACKETの既存adaptation fieldへPCRを設定する。"
  (when (< (ts-adaptation-field-length packet) 7)
    (bridge-error "Test packet has no room for a PCR"))
  (let ((base (floor pcr 300))
        (extension (mod pcr 300)))
    (setf (aref packet 5)
          (logior (aref packet 5) #x10)
          (aref packet 6) (ldb (byte 8 25) base)
          (aref packet 7) (ldb (byte 8 17) base)
          (aref packet 8) (ldb (byte 8 9) base)
          (aref packet 9) (ldb (byte 8 1) base)
          (aref packet 10)
          (logior (ash (ldb (byte 1 0) base) 7)
                  #x7e
                  (ldb (byte 1 8) extension))
          (aref packet 11) (ldb (byte 8 0) extension)))
  packet)

(defun make-test-program-pcr-packet
    (timestamp &key (counter 0) discontinuity)
  "PROGRAM timestampと同じ90kHz時刻のPCR-only packetを作る。"
  (let ((packet
          (make-test-adaptation-only-packet
           +test-video-pid+ counter
           :discontinuity discontinuity)))
    (set-test-pcr packet (* timestamp 300))))

(defun packetize-test-video-with-pcr
    (pes &key (continuity-counter 9)
              (discontinuity t)
              (transport-priority t)
              (pcr-lead-ticks 0))
  "PESを先頭PCR/discontinuity付きfixture packet列へする。"
  (let* ((header (parse-pes-header pes))
         (ordering-time
           (or (pes-header-dts header)
               (pes-header-pts header)))
         (first-count (min 170 (length pes)))
         (first
           (make-ts-packet
            +test-video-pid+ continuity-counter
            (subseq pes 0 first-count)
            :payload-unit-start t
            :transport-priority transport-priority)))
    (when discontinuity
      (setf (aref first 5)
            (logior (aref first 5) #x80)))
    (set-test-pcr
     first
     (*
       (mod
       (- ordering-time
          pcr-lead-ticks)
       +pts-modulus+)
      300))
    (if (= first-count (length pes))
        (list first)
        (cons
         first
         (packetize-payload
          +test-video-pid+
          (subseq pes first-count)
         :continuity-counter
          (logand (+ continuity-counter 1) #x0f))))))

(defun retime-test-pcrs-for-cbr
    (packets transport-rate-kbps)
  "PACKETSのPCRを明示CBR上のpacket indexへ決定的に整合させる。"
  (let ((result (mapcar #'copy-seq packets))
        (epoch-index nil)
        (epoch-pcr nil)
        (rate-bps (* transport-rate-kbps 1000)))
    (loop for packet in result
          for packet-index from 0
          when (ts-pcr-flag-p packet)
            do
              (when (or (null epoch-index)
                        (ts-discontinuity-indicator-p packet))
                (setf
                 epoch-index packet-index
                 epoch-pcr
                 (mod
                  (-
                   (ts-pcr packet)
                   (*
                    +test-tstd-removal-delay-ticks+
                    300))
                  +pcr-modulus+)))
              (set-test-pcr
               packet
               (mod
                (+ epoch-pcr
                   (round
                    (*
                     (- packet-index epoch-index)
                     +ts-packet-size+
                     8
                     +tstd-system-clock-rate+)
                    rate-bps))
                +pcr-modulus+)))
    result))

(defun octets-to-packet-list (octets)
  "OCTETSを188-byte TS packet listへ分割する。"
  (unless (zerop (mod (length octets) +ts-packet-size+))
    (bridge-error "Test output is not transport packet aligned"))
  (loop for offset from 0 below (length octets)
          by +ts-packet-size+
        collect
        (subseq octets offset (+ offset +ts-packet-size+))))

(defun run-packet-processor
    (packets video-codec audio-codec
     &key (transport-rate-kbps +test-transport-rate-kbps+)
          (retime-p t))
  "PACKETSをprocessorへ与え、出力packet列を返す。"
  (let* ((source-packets
           (if (and (eq video-codec :av1) retime-p)
               (retime-test-pcrs-for-cbr
                packets transport-rate-kbps)
               packets))
         (output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor
            output video-codec audio-codec
            :transport-rate-kbps
            (when (eq video-codec :av1)
              transport-rate-kbps))))
    (dolist (packet source-packets)
      (process-bridge-packet processor (copy-seq packet)))
    (finish-bridge-processor processor)
    (octets-to-packet-list (collected-octets output))))

(defun sections-on-pid (packets pid)
  "PACKETSからPID上の完成PSI sectionを集める。"
  (let ((assembler (make-section-assembler pid))
        (sections '()))
    (dolist (packet packets)
      (when (= (ts-pid packet) pid)
        (setf sections
              (nconc sections
                     (feed-section-packet assembler packet)))))
    sections))

(defun first-pes-on-pid (packets pid)
  "PACKETSからPIDの最初の宣言長PESを復元する。"
  (let ((buffer
          (make-array 256
                      :element-type 'octet
                      :adjustable t
                      :fill-pointer 0))
        (started-p nil)
        (expected nil))
    (dolist (packet packets)
      (when (= (ts-pid packet) pid)
        (when (ts-payload-unit-start-p packet)
          (when started-p
            (return))
          (setf started-p t))
        (when (and started-p (ts-has-payload-p packet))
          (loop for position from (ts-payload-offset packet)
                  below +ts-packet-size+
                do (vector-push-extend
                    (aref packet position) buffer))
          (when (and (null expected)
                     (>= (length buffer) 6))
            (let ((declared (read-u16-be buffer 4)))
              (when (plusp declared)
                (setf expected (+ declared 6)))))
          (when (and expected (>= (length buffer) expected))
            (return)))))
    (unless (and expected (>= (length buffer) expected))
      (bridge-error "Test PES reconstruction is incomplete"))
    (subseq buffer 0 expected)))

(defun first-unbounded-pes-on-pid (packets pid)
  "PACKETSからPIDの最初のPESを次PUSI境界まで復元する。"
  (let ((buffer
          (make-array 1024
                      :element-type 'octet
                      :adjustable t
                      :fill-pointer 0))
        (started-p nil))
    (dolist (packet packets)
      (when (= (ts-pid packet) pid)
        (when (ts-payload-unit-start-p packet)
          (when started-p
            (return))
          (setf started-p t))
        (when (and started-p (ts-has-payload-p packet))
          (loop for position from (ts-payload-offset packet)
                  below +ts-packet-size+
                do (vector-push-extend
                    (aref packet position) buffer)))))
    (unless started-p
      (bridge-error "Test unbounded PES is absent"))
    (coerce buffer '(simple-array (unsigned-byte 8) (*)))))

(defun payload-continuity-valid-p (packets pid)
  "PIDのpayload-bearing packet CCが連続しているかを返す。"
  (let ((last nil))
    (dolist (packet packets t)
      (when (and (= (ts-pid packet) pid)
                 (ts-has-payload-p packet))
        (let ((counter (ts-continuity-counter packet)))
          (when (and last
                     (/= counter (logand (+ last 1) #x0f)))
            (return-from payload-continuity-valid-p nil))
          (setf last counter))))))

(defun make-test-av1-access-unit (level)
  "完全構文を持つAV1 key access unitを作る。"
  (concatenate-octets
   (make-av1-structure-test-sequence :level level)
   (make-av1-structure-test-key-frame)))

(defun feed-test-processor-packets (processor packets)
  "PROCESSORへPACKETSを順番に複写して与える。"
  (dolist (packet packets processor)
    (process-bridge-packet processor (copy-seq packet))))

(defun make-test-state-processor (video-codec)
  "VIDEO-CODECの状態遷移検証用processorを作る。"
  (make-bridge-processor
   (make-instance 'octet-collector-stream)
   video-codec
   :aac
   :transport-rate-kbps
   (when (eq video-codec :av1)
     +test-transport-rate-kbps+)))

(defun vp9-test-state-empty-p (state)
  "STATEにcurrent configurationと有効referenceがないかを返す。"
  (and
   (null
    (vp9-validation-state-current-configuration state))
   (every
    (lambda (slot)
      (not (vp9-reference-slot-valid-p slot)))
    (vp9-validation-state-slots state))))

(defun av1-test-frame-state-empty-p (state)
  "STATEに有効referenceがないかを返す。"
  (every
   (lambda (slot)
     (not
      (av1-reference-slot-validation-state-valid-p slot)))
   (av1-frame-validation-state-reference-slots state)))

(defun make-test-pat-packets-for-pmt
    (pmt-pid version continuity-counter)
  "PMT-PIDへ遷移するprocessor test用PAT packet列を作る。"
  (make-section-ts-packets
   0
   (build-pat-section
    (make-program-association-table
     :transport-stream-id 1
     :version version
     :programs
     (list
      (make-pat-program
       :program-number 1
       :pid pmt-pid))))
   continuity-counter))

(defun make-test-pmt-packets-for-video
    (video-pid version continuity-counter)
  "VIDEO-PIDへ遷移するprocessor test用PMT packet列を作る。"
  (let ((table
          (make-test-pmt-table
           :version version
           :two-audio-p nil)))
    (setf
     (program-map-table-pcr-pid table)
     video-pid
     (pmt-stream-elementary-pid
      (first (program-map-table-streams table)))
     video-pid)
    (make-section-ts-packets
     +test-pmt-pid+
     (build-pmt-section table)
     continuity-counter)))

(define-bridge-test vp9-processor-retains-frame-state-between-access-units
  (let* ((processor (make-test-state-processor :vp9))
         (key-access-unit
           (make-vp9-structure-test-key-frame))
         (inter-access-unit
           (make-vp9-structure-test-inter-frame))
         (key-packets
           (packetize-test-video-with-pcr
            (make-pes #xbd key-access-unit 90000)
            :continuity-counter 9))
         (inter-packets
           (packetize-payload
            +test-video-pid+
            (make-pes #xbd inter-access-unit 93600)
            :continuity-counter
            (logand (+ 9 (length key-packets)) #x0f)
            :payload-unit-start t)))
    (feed-test-processor-packets
     processor
     (append
      (make-test-pat-packets)
      (make-test-pmt-packets :two-audio-p nil)
      key-packets))
    (let ((key-state
            (bridge-processor-vp9-validation-state processor)))
      (check-bridge-test
       (vp9-frame-configuration-key-frame-p
        (vp9-validation-state-current-configuration key-state)))
      (feed-test-processor-packets processor inter-packets)
      (let ((inter-state
              (bridge-processor-vp9-validation-state processor)))
        (check-bridge-test (not (eq key-state inter-state)))
        (check-bridge-test
         (not
          (vp9-frame-configuration-key-frame-p
           (vp9-validation-state-current-configuration
            inter-state)))))))
  (let ((processor (make-test-state-processor :vp9))
        (inter-packets
           (packetize-test-video-with-pcr
            (make-pes
             #xbd
             (make-vp9-structure-test-inter-frame)
             90000))))
    (feed-test-processor-packets
     processor
     (append
      (make-test-pat-packets)
      (make-test-pmt-packets :two-audio-p nil)))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (feed-test-processor-packets
         processor inter-packets))))))

(define-bridge-test av1-processor-retains-and-rolls-back-frame-state
  (let* ((processor (make-test-state-processor :av1))
         (key-access-unit (make-test-av1-access-unit 0))
         (inter-access-unit
           (make-av1-structure-test-inter-frame))
         (key-packets
           (packetize-test-video-with-pcr
            (make-pes #xe0 key-access-unit 90000)
            :continuity-counter 9))
         (inter-packets
           (packetize-payload
            +test-video-pid+
            (make-pes #xe0 inter-access-unit 93600)
            :continuity-counter
            (logand (+ 9 (length key-packets)) #x0f)
            :payload-unit-start t))
         (prefix-count
           (+ (length (make-test-pat-packets))
              (length
               (make-test-pmt-packets :two-audio-p nil))
              (length key-packets)))
         (packets
           (retime-test-pcrs-for-cbr
            (append
             (make-test-pat-packets)
             (make-test-pmt-packets :two-audio-p nil)
             key-packets
             inter-packets)
            +test-transport-rate-kbps+)))
    (feed-test-processor-packets
     processor
     (subseq packets 0 prefix-count))
    (let ((key-state
            (bridge-processor-av1-frame-validation-state
             processor))
          (sequence-state
            (bridge-processor-av1-sequence-validation-state
             processor)))
      (check-bridge-test
       (every
        #'av1-reference-slot-validation-state-valid-p
        (av1-frame-validation-state-reference-slots
         key-state)))
      (feed-test-processor-packets
       processor
       (subseq packets prefix-count))
      (check-bridge-test
       (not
        (eq key-state
            (bridge-processor-av1-frame-validation-state
             processor))))
      (check-bridge-test
       (eq sequence-state
           (bridge-processor-av1-sequence-validation-state
            processor)))))
  (let* ((processor (make-test-state-processor :av1))
         (key-access-unit (make-test-av1-access-unit 0))
         (inter-access-unit
           (make-av1-structure-test-inter-frame))
         (corrupt-access-unit
           (subseq inter-access-unit
                   0
                   (- (length inter-access-unit) 1)))
         (key-packets
           (packetize-test-video-with-pcr
            (make-pes #xe0 key-access-unit 90000)
            :continuity-counter 9))
         (corrupt-packets
           (packetize-payload
            +test-video-pid+
            (make-pes #xe0 corrupt-access-unit 93600)
            :continuity-counter
            (logand (+ 9 (length key-packets)) #x0f)
            :payload-unit-start t))
         (prefix-count
           (+ (length (make-test-pat-packets))
              (length
               (make-test-pmt-packets :two-audio-p nil))
              (length key-packets)))
         (packets
           (retime-test-pcrs-for-cbr
            (append
             (make-test-pat-packets)
             (make-test-pmt-packets :two-audio-p nil)
             key-packets
             corrupt-packets)
            +test-transport-rate-kbps+)))
    (feed-test-processor-packets
     processor
     (subseq packets 0 prefix-count))
    (let ((frame-state
            (bridge-processor-av1-frame-validation-state
             processor))
          (sequence-state
            (bridge-processor-av1-sequence-validation-state
             processor)))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-av1-frame-access-unit-structure
           corrupt-access-unit
           sequence-state
           frame-state))))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (feed-test-processor-packets
           processor
           (subseq packets prefix-count)))))
      (check-bridge-test
       (eq frame-state
           (bridge-processor-av1-frame-validation-state
            processor)))
      (check-bridge-test
       (eq sequence-state
           (bridge-processor-av1-sequence-validation-state
            processor))))))

(define-bridge-test processor-resets-codec-state-on-pat-and-pmt-target-change
  (let ((processor (make-test-state-processor :vp9))
        (key-packets
           (packetize-test-video-with-pcr
            (make-pes
             #xbd
             (make-vp9-structure-test-key-frame)
             90000))))
    (feed-test-processor-packets
     processor
     (append
      (make-test-pat-packets)
      (make-test-pmt-packets :two-audio-p nil)
      key-packets))
    (check-bridge-test
     (not
      (vp9-test-state-empty-p
       (bridge-processor-vp9-validation-state
        processor))))
    (feed-test-processor-packets
     processor
     (make-test-pmt-packets-for-video #x107 8 5))
    (check-bridge-test
     (vp9-test-state-empty-p
      (bridge-processor-vp9-validation-state
       processor))))
  (let* ((processor (make-test-state-processor :av1))
         (key-packets
           (packetize-test-video-with-pcr
            (make-pes
             #xe0
             (make-test-av1-access-unit 0)
             90000)))
         (prefix-count
           (+ (length (make-test-pat-packets))
              (length
               (make-test-pmt-packets :two-audio-p nil))
              (length key-packets)))
         (packets
           (retime-test-pcrs-for-cbr
            (append
             (make-test-pat-packets)
             (make-test-pmt-packets :two-audio-p nil)
             key-packets
             (make-test-pat-packets-for-pmt #x110 3 1))
            +test-transport-rate-kbps+)))
    (feed-test-processor-packets
     processor
     (subseq packets 0 prefix-count))
    (check-bridge-test
     (bridge-processor-av1-sequence-validation-state
      processor))
    (check-bridge-test
     (not
      (av1-test-frame-state-empty-p
       (bridge-processor-av1-frame-validation-state
        processor))))
    (feed-test-processor-packets
     processor
     (subseq packets prefix-count))
    (check-bridge-test
     (null
      (bridge-processor-av1-sequence-validation-state
       processor)))
    (check-bridge-test
     (av1-test-frame-state-empty-p
      (bridge-processor-av1-frame-validation-state
       processor)))))

(define-bridge-test opus-control-payload-validation
  (let* ((packet (octets #xf8 #xff #xfe))
         (payload
           (concatenate-octets
            (octets #x7f #xe0 (length packet))
            packet)))
    (check-bridge-test
     (= (parse-ffmpeg-opus-control-payload payload) 960))
    (setf (aref payload 2) 20)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (parse-ffmpeg-opus-control-payload payload))))))

(define-bridge-test opus-multiple-control-packets-and-variable-pts
  (let* ((raw-one (octets #xf8 #x10 #x20))
         (raw-two (octets #xf8 #x30 #x40))
         (control
           (concatenate-octets
            (octets #x7f #xe0 (length raw-one))
            raw-one
            (octets #x7f #xe0 (length raw-two))
            raw-two))
         (first-pes (make-pes #xbd control 90000))
         (second-pes
           (make-test-opus-pes
            93600 (octets #xf8 #x50 #x60)))
         (first-packets
           (packetize-payload
            +test-audio-one-pid+ first-pes
            :continuity-counter 0
            :payload-unit-start t))
         (second-packets
           (packetize-payload
            +test-audio-one-pid+ second-pes
            :continuity-counter
            (logand (length first-packets) #x0f)
            :payload-unit-start t))
         (input
           (append
            (make-test-pat-packets)
            (make-test-pmt-packets
             :two-audio-p nil)
            (list
             (make-test-program-pcr-packet 90000))
            first-packets second-packets))
         (output
           (run-packet-processor
            input :passthrough :opus)))
    (check-bridge-test
     (= (validate-ffmpeg-opus-pes first-pes) 1920))
    (let ((zero-length-pes (copy-seq first-pes)))
      (setf (aref zero-length-pes 4) 0
            (aref zero-length-pes 5) 0)
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-ffmpeg-opus-pes zero-length-pes)))))
    (dolist (packet (append first-packets second-packets))
      (check-bridge-test
       (find packet output :test #'equalp)))))

(define-bridge-test opus-pts-small-jitter-is-accepted
  "FFmpeg live mux で観測される ±2 tick 級の Opus PTS ジッターを通す。"
  (let* ((raw-one (octets #xf8 #x10 #x20))
         (raw-two (octets #xf8 #x30 #x40))
         (first-pes (make-test-opus-pes 90000 raw-one))
         ;; 20ms (1800 ticks) ではなく +2 tick の 1802 で送る。
         (second-pes (make-test-opus-pes 91802 raw-two))
         (first-packets
           (packetize-payload
            +test-audio-one-pid+ first-pes
            :continuity-counter 0
            :payload-unit-start t))
         (second-packets
           (packetize-payload
            +test-audio-one-pid+ second-pes
            :continuity-counter
            (logand (length first-packets) #x0f)
            :payload-unit-start t))
         (input
           (append
            (make-test-pat-packets)
            (make-test-pmt-packets :two-audio-p nil)
            (list (make-test-program-pcr-packet 90000))
            first-packets second-packets))
         (output
           (run-packet-processor
            input :passthrough :opus)))
    (dolist (packet (append first-packets second-packets))
      (check-bridge-test
       (find packet output :test #'equalp)))))

(define-bridge-test opus-pts-large-jump-is-rejected
  "1ms を超える Opus PTS 飛びは従来どおり致命エラーにする。"
  (let* ((raw-one (octets #xf8 #x10 #x20))
         (raw-two (octets #xf8 #x30 #x40))
         (first-pes (make-test-opus-pes 90000 raw-one))
         ;; 1800 + 91 = 1891 ticks 先。許容 90 を超える。
         (second-pes (make-test-opus-pes 91891 raw-two))
         (first-packets
           (packetize-payload
            +test-audio-one-pid+ first-pes
            :continuity-counter 0
            :payload-unit-start t))
         (second-packets
           (packetize-payload
            +test-audio-one-pid+ second-pes
            :continuity-counter
            (logand (length first-packets) #x0f)
            :payload-unit-start t))
         (input
           (append
            (make-test-pat-packets)
            (make-test-pmt-packets :two-audio-p nil)
            (list (make-test-program-pcr-packet 90000))
            first-packets second-packets)))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (run-packet-processor input :passthrough :opus))))))

(define-bridge-test vp9-opus-processor-end-to-end
  (let* ((pat (make-test-pat-packets))
         (pmt (make-test-pmt-packets))
         (repeated-pmt
           (make-section-ts-packets
            +test-pmt-pid+
            (build-pmt-section (make-test-pmt-table))
            5))
         (vp9-frame
           (concatenate-octets
            (make-test-vp9-key-frame 0 1920 1080)
            (make-pattern-octets 420 7)))
         (video-pes
           (make-pes #xbd vp9-frame 90000
                     :dts 87000))
         (video-packets
           (packetize-test-video-with-pcr video-pes))
         (opus-one
           (packetize-payload
            +test-audio-one-pid+
            (make-test-opus-pes
             90000 (octets #xf8 #xff #xfe))
            :continuity-counter 2
            :payload-unit-start t))
         (opus-two
           (packetize-payload
            +test-audio-two-pid+
            (make-test-opus-pes
             90000 (octets #xf8 #xaa #xbb))
            :continuity-counter 5
            :payload-unit-start t))
         (timed-id3
           (make-ts-packet
            +test-timed-id3-pid+ 3
            (make-pattern-octets 37 21)
            :payload-unit-start t))
         (data
           (make-ts-packet
            +test-data-pid+ 11
            (make-pattern-octets 91 38)
            :payload-unit-start t))
         (input
           (append
            pat pmt
            opus-one
            (list timed-id3)
            (list (first video-packets))
            (list data)
            (rest video-packets)
            opus-two
            repeated-pmt))
         (output
           (run-packet-processor input :vp9 :opus))
         (sections
           (sections-on-pid output +test-pmt-pid+))
         (mapped-pmt
           (parse-pmt-section (first sections)))
         (video-stream
           (find +test-video-pid+
                 (program-map-table-streams mapped-pmt)
                 :key #'pmt-stream-elementary-pid
                 :test #'=))
         (output-video-pes
           (first-pes-on-pid output +test-video-pid+))
         (video-header
           (parse-pes-header output-video-pes))
         (first-video-packet
           (find-if
            (lambda (packet)
              (and (= (ts-pid packet) +test-video-pid+)
                   (ts-payload-unit-start-p packet)))
            output))
         (input-octets (packet-list-to-octets input))
         (output-octets (packet-list-to-octets output))
         (inspection (inspect-ts-octets output-octets))
         (video-inspection
           (gethash +test-video-pid+
                    (ts-inspection-pids inspection))))
    (check-bridge-test (= (length sections) 2))
    (check-bridge-test
     (= (program-map-table-version mapped-pmt) 8))
    (validate-vp9-mapping-descriptors
     (pmt-stream-descriptors video-stream))
    (check-bridge-test (= (pes-header-stream-id video-header) #xe0))
    (check-bridge-test (pes-header-data-alignment-p video-header))
    (check-bridge-test (= (pes-header-pts video-header) 90000))
    (check-bridge-test (= (pes-header-dts video-header) 87000))
    (check-bridge-test
     (equalp
      (subseq output-video-pes
              (pes-header-payload-offset video-header))
      vp9-frame))
    (check-bridge-test
     (ts-random-access-indicator-p first-video-packet))
    (check-bridge-test
     (ts-transport-priority-p first-video-packet))
    (check-bridge-test
     (ts-discontinuity-indicator-p first-video-packet))
    (check-bridge-test
     (= (ts-pcr first-video-packet) 26100000))
    (check-bridge-test
     (payload-continuity-valid-p output +test-video-pid+))
    (check-bridge-test
     (find timed-id3 output :test #'equalp))
    (check-bridge-test
     (find data output :test #'equalp))
    (check-bridge-test
     (= (ts-inspection-packet-count inspection)
        (length output)))
    (check-bridge-test
     (equal (pid-inspection-pcr-values video-inspection)
            '(26100000)))
    (check-bridge-test
     (non-target-packets-byte-exact-p
      input-octets output-octets
      (list +test-pmt-pid+
            +test-video-pid+
            +test-audio-one-pid+
            +test-audio-two-pid+)))
    (dolist (packet (append opus-one opus-two))
      (check-bridge-test
       (find packet output :test #'equalp)))))

(define-bridge-test opus-registration-only-missing-is-filled
  (let* ((input
           (append
            (make-test-pat-packets)
            (make-test-pmt-packets
             :two-audio-p nil
             :opus-registration-p nil)
            (list
             (make-test-program-pcr-packet 45000))
            (packetize-payload
             +test-audio-one-pid+
             (make-test-opus-pes
              45000 (octets #xf8 #x01 #x02))
             :payload-unit-start t)))
         (output
           (run-packet-processor input :passthrough :opus))
         (section
           (first
            (sections-on-pid output +test-pmt-pid+)))
         (table (parse-pmt-section section))
         (stream
           (find +test-audio-one-pid+
                 (program-map-table-streams table)
                 :key #'pmt-stream-elementary-pid
                 :test #'=)))
    (check-bridge-test
     (= (validate-opus-descriptors
         (pmt-stream-descriptors stream))
        2))))

(define-bridge-test av1-processor-signals-descriptor-and-rap
  (let* ((access-unit (make-test-av1-access-unit 8))
         (video-packets
           (packetize-test-video-with-pcr
            (make-pes #xe0 access-unit 180000
                      :dts 177000)
            :continuity-counter 6
            :transport-priority t))
         (input
           (append
            (make-test-pat-packets)
            (make-test-pmt-packets
             :two-audio-p nil)
            video-packets))
         (output
           (run-packet-processor input :av1 :aac))
         (section
           (first
            (sections-on-pid output +test-pmt-pid+)))
         (table (parse-pmt-section section))
         (stream
           (find +test-video-pid+
                 (program-map-table-streams table)
                 :key #'pmt-stream-elementary-pid
                 :test #'=))
         (pes (first-pes-on-pid output +test-video-pid+))
         (header (parse-pes-header pes))
         (first-packet
           (find-if
            (lambda (packet)
              (and (= (ts-pid packet) +test-video-pid+)
                   (ts-payload-unit-start-p packet)))
            output)))
    (validate-av1-mapping-descriptors
     (pmt-stream-descriptors stream))
    (check-bridge-test (= (pes-header-stream-id header) #xbd))
    (check-bridge-test
     (equalp
      (subseq pes (pes-header-payload-offset header))
      (convert-av1-access-unit-to-ts-format access-unit)))
    (check-bridge-test
     (ts-random-access-indicator-p first-packet))
    (check-bridge-test
     (ts-elementary-stream-priority-indicator-p first-packet))
    (check-bridge-test
     (ts-transport-priority-p first-packet))))

(defun packet-indices-matching (packets predicate)
  "PACKETSでPREDICATEを満たすindexを返す。"
  (loop for packet in packets
        for index from 0
        when (funcall predicate packet)
          collect index))

(defun make-av1-configuration-change-fixture
    (&key (prior-null-p t) (transport-rate-kbps 1504))
  "AV1 level変更直前のnull有無を選べる固定PUSI/PCR fixtureを作る。"
  (let* ((first-access-unit (make-test-av1-access-unit 8))
         (second-access-unit (make-test-av1-access-unit 9))
         (first-video
           (packetize-test-video-with-pcr
            (make-pes #xe0 first-access-unit 90000)
            :continuity-counter 1))
         (second-video
           (packetize-test-video-with-pcr
            (make-pes #xe0 second-access-unit 180000)
            :continuity-counter
            (logand (+ 1 (length first-video)) #x0f)
            :discontinuity nil
            :pcr-lead-ticks
            +test-tstd-removal-delay-ticks+))
         (intermediate-pcr
           (loop
             for timestamp from 99000 to 171000 by 9000
             collect
             (make-test-program-pcr-packet
              timestamp
              :counter
              (logand (length first-video) #x0f))))
         (prefix
           (append
            (make-test-pat-packets)
            (make-test-pmt-packets :two-audio-p nil)
            first-video
            intermediate-pcr))
         (between
           (append
            (when prior-null-p
              (list (make-output-null-packet)))
            (list
             (make-ts-packet
              +test-data-pid+ 1 (octets 1 2 3)))))
         (origin-slot
           (+ (length prefix) (length between)))
         (input
           (retime-test-pcrs-for-cbr
            (append
             prefix
             between
             second-video
             (list (make-output-null-packet)))
            transport-rate-kbps)))
    (values input origin-slot)))

(define-bridge-test av1-configuration-change-injects-pmt-before-au
  (multiple-value-bind (input origin-slot)
      (make-av1-configuration-change-fixture)
    (let* ((source-pusi (nth origin-slot input))
         (output
           (run-packet-processor
            input :av1 :aac
            :transport-rate-kbps 1504
            :retime-p nil))
         (sections
           (sections-on-pid output +test-pmt-pid+))
         (versions
           (mapcar
            (lambda (section)
              (program-map-table-version
               (parse-pmt-section section)))
            sections))
         (pmt-indices
           (packet-indices-matching
            output
            (lambda (packet)
              (and (= (ts-pid packet) +test-pmt-pid+)
                   (ts-payload-unit-start-p packet)))))
         (video-indices
           (packet-indices-matching
            output
            (lambda (packet)
              (and (= (ts-pid packet) +test-video-pid+)
                   (ts-payload-unit-start-p packet)))))
         (output-pusi (nth origin-slot output)))
      (check-bridge-test (equal versions '(8 9)))
      (check-bridge-test (= (length output) (length input)))
      (check-bridge-test (= (length video-indices) 2))
      (check-bridge-test (= (length pmt-indices) 2))
      (check-bridge-test
       (< (second pmt-indices)
          (second video-indices)))
      ;; 合成PMTをexact-2ms境界の先行nullへ置く。
      (check-bridge-test
       (= (second pmt-indices) (- origin-slot 2)))
      ;; 介在する固定data packetもslot/byteを変えない。
      (check-bridge-test
       (equalp
        (nth (- origin-slot 1) output)
        (nth (- origin-slot 1) input)))
      ;; 合成PMTを先行nullへbackfillしても、PCR付きPUSIは同じslotに残す。
      (check-bridge-test (= (second video-indices) origin-slot))
      (check-bridge-test
       (= (ts-pid output-pusi) (ts-pid source-pusi)))
      (check-bridge-test
       (= (ts-continuity-counter output-pusi)
          (ts-continuity-counter source-pusi)))
      (check-bridge-test (ts-payload-unit-start-p output-pusi))
      (check-bridge-test (ts-has-payload-p output-pusi))
      (check-bridge-test (ts-random-access-indicator-p output-pusi))
      (check-bridge-test
       (ts-elementary-stream-priority-indicator-p output-pusi))
      (check-bridge-test
       (= (ts-pcr output-pusi) (ts-pcr source-pusi)))
      (check-bridge-test
       (equalp
        (subseq output-pusi 6 12)
        (subseq source-pusi 6 12)))
      (check-bridge-test
       (payload-continuity-valid-p output +test-pmt-pid+))
      (check-bridge-test
       (payload-continuity-valid-p output +test-video-pid+)))))

(define-bridge-test av1-configuration-change-without-prior-null-fails-in-place
  (multiple-value-bind (input origin-slot)
      (make-av1-configuration-change-fixture :prior-null-p nil)
    (let* ((source-pusi (nth origin-slot input))
           (output (make-instance 'octet-collector-stream))
           (processor
             (make-bridge-processor
              output :av1 :aac
              :transport-rate-kbps
              1504))
           (message
             (handler-case
                 (progn
                   (dolist (packet input)
                     (process-bridge-packet
                      processor (copy-seq packet)))
                   nil)
               (bridge-error (condition)
                 (bridge-error-message condition))))
           (pending-pusi
             (loop for entry =
                     (bridge-processor-pending-head processor)
                       then (pending-entry-next entry)
                   while entry
                   when (= (pending-entry-slot-index entry)
                           origin-slot)
                     return entry)))
      (check-bridge-test
       (and message
            (search
             "reason=pmt_backfill_null_slots"
             message :test #'char=)))
      (check-bridge-test
       (search
        (format nil "origin_slot=~D" origin-slot)
        message :test #'char=))
      ;; fail closed時もPUSI/PCR packet自体は移動・分割・書換えしない。
      (check-bridge-test pending-pusi)
      (check-bridge-test
       (= (pending-entry-slot-index pending-pusi)
          origin-slot))
      (check-bridge-test
       (equalp (pending-entry-packet pending-pusi)
               source-pusi))
      (check-bridge-test
       (ts-payload-unit-start-p
        (pending-entry-packet pending-pusi)))
      (check-bridge-test
       (= (ts-pcr (pending-entry-packet pending-pusi))
          (ts-pcr source-pusi))))))

(defun make-av1-initial-interleaved-null-fixture
    (&key trailing-null-p)
  "初回AV1 prefixの途中にnullを置くsource順検証fixtureを作る。"
  (let* ((sequence
           (make-av1-structure-test-sequence :level 8))
         (padding
           (make-av1-structure-test-obu
            15
            (make-array 260
                        :element-type 'octet
                        :initial-element 0)))
         (access-unit
           (concatenate-octets
            sequence
            padding
            (make-av1-structure-test-key-frame)))
         (source-pes
           (make-pes #xe0 access-unit 90000))
         (header
           (parse-pes-header source-pes))
         (payload-offset
           (pes-header-payload-offset header))
         (video-packets nil)
         (pat-packets
           (make-test-pat-packets))
         (pmt-packets
           (make-test-pmt-packets :two-audio-p nil))
         (null-packet
           (make-output-null-packet)))
    (setf (aref source-pes 4) 0
          (aref source-pes 5) 0)
    (setf video-packets
          (packetize-test-video-with-pcr
           source-pes
           :continuity-counter 0
           :pcr-lead-ticks
           +test-tstd-removal-delay-ticks+))
    (let ((expected-header
            (copy-seq
             (subseq source-pes 0 payload-offset))))
      (setf (aref expected-header 3) #xbd
            (aref expected-header 6)
            (logior
             (aref expected-header 6)
             #x04))
      (values
       (append
        pat-packets
        pmt-packets
        (list
         (first video-packets)
         null-packet)
        (rest video-packets)
        (when trailing-null-p
          (list (copy-seq null-packet))))
       (concatenate-octets
        expected-header
        (convert-av1-access-unit-to-ts-format
         access-unit))
       (+ (length pat-packets)
          (length pmt-packets)
          1)))))

(define-bridge-test av1-initial-byte-fifo-preserves-interleaved-null-order
  (multiple-value-bind (input expected intermediate-null-index)
      (make-av1-initial-interleaved-null-fixture
       :trailing-null-p t)
    (let* ((trailing-null (car (last input)))
           (output
             (run-packet-processor input :av1 :aac))
           (rebuilt
             (first-unbounded-pes-on-pid
              output +test-video-pid+)))
      (check-bridge-test
       (= (length output) (length input)))
      (check-bridge-test
       (equalp rebuilt expected))
      (check-bridge-test
       (= (ts-pid (nth intermediate-null-index output))
          +test-video-pid+))
      ;; PES完了後の将来nullは借用せずbyte-exactに残す。
      (check-bridge-test
       (equalp (car (last output)) trailing-null))
      (check-bridge-test
       (payload-continuity-valid-p
        output +test-video-pid+)))))

(define-bridge-test av1-initial-byte-fifo-uses-interleaved-null-capacity
  (multiple-value-bind (input expected intermediate-null-index)
      (make-av1-initial-interleaved-null-fixture)
    (let* ((output
             (run-packet-processor input :av1 :aac))
           (rebuilt
             (first-unbounded-pes-on-pid
              output +test-video-pid+)))
      (check-bridge-test
       (= (length output) (length input)))
      (check-bridge-test (equalp rebuilt expected))
      (check-bridge-test
       (= (ts-pid (nth intermediate-null-index output))
          +test-video-pid+))
      (check-bridge-test
       (= (ts-pid (car (last output)))
          +test-video-pid+))
      (check-bridge-test
       (payload-continuity-valid-p
        output +test-video-pid+)))))

(define-bridge-test av1-pusi-boundary-rejects-residual-without-carry
  (multiple-value-bind (input expected intermediate-null-index)
      (make-av1-initial-interleaved-null-fixture)
    (declare (ignore expected))
    (let* ((without-null
             (loop for packet in input
                   for index from 0
                   unless (= index intermediate-null-index)
                     collect packet))
           (last-video
             (find +test-video-pid+ without-null
                   :key #'ts-pid :test #'= :from-end t))
           (next-pusi
             (first
              (packetize-test-video-with-pcr
               (make-pes
                #xe0
                (make-test-av1-access-unit 8)
                180000)
               :continuity-counter
               (logand
                (+ (ts-continuity-counter last-video) 1)
                #x0f)
               :discontinuity nil
               :pcr-lead-ticks
               +test-tstd-removal-delay-ticks+)))
           (pusi-origin (length without-null))
           (source
             (retime-test-pcrs-for-cbr
              (append
               without-null
               (list next-pusi
                     (make-output-null-packet)))
              +test-transport-rate-kbps+))
           (output (make-instance 'octet-collector-stream))
           (processor
             (make-bridge-processor
              output :av1 :aac
              :transport-rate-kbps
              +test-transport-rate-kbps+))
           (message
             (handler-case
                 (progn
                   (dolist (packet source)
                     (process-bridge-packet
                      processor (copy-seq packet)))
                   nil)
               (bridge-error (condition)
                 (bridge-error-message condition))))
           (pending-pusi
             (loop for entry =
                     (bridge-processor-pending-head processor)
                       then (pending-entry-next entry)
                   while entry
                   when (= (pending-entry-slot-index entry)
                           pusi-origin)
                     return entry)))
      (check-bridge-test
       (and message
            (search
             "REPACKETIZE_CAPACITY_EXHAUSTED residual_bytes="
             message :test #'char=)))
      ;; 旧PES残留は次PUSIやその後のnullへcarryせず、その場で拒否する。
      (check-bridge-test pending-pusi)
      (check-bridge-test
       (= (pending-entry-slot-index pending-pusi)
          pusi-origin))
      (check-bridge-test
       (equalp
        (pending-entry-packet pending-pusi)
        (nth pusi-origin source)))
      (check-bridge-test
       (ts-payload-unit-start-p
        (pending-entry-packet pending-pusi))))))

(define-bridge-test av1-eof-boundary-rejects-residual-without-carry
  (multiple-value-bind (input expected intermediate-null-index)
      (make-av1-initial-interleaved-null-fixture)
    (declare (ignore expected))
    (let* ((without-null
             (loop for packet in input
                   for index from 0
                   unless (= index intermediate-null-index)
                     collect packet))
           (source
             (retime-test-pcrs-for-cbr
              without-null +test-transport-rate-kbps+))
           (pusi-origin
             (position-if
              (lambda (packet)
                (and
                 (= (ts-pid packet) +test-video-pid+)
                 (ts-payload-unit-start-p packet)))
              source))
           (source-pusi (nth pusi-origin source))
           (output (make-instance 'octet-collector-stream))
           (processor
             (make-bridge-processor
              output :av1 :aac
              :transport-rate-kbps
              +test-transport-rate-kbps+))
           (process-message
             (handler-case
                 (progn
                   (dolist (packet source)
                     (process-bridge-packet
                      processor (copy-seq packet)))
                   nil)
               (bridge-error (condition)
                 (bridge-error-message condition))))
           (finish-message
             (and
              (null process-message)
              (handler-case
                  (progn
                    (finish-bridge-processor processor)
                    nil)
                (bridge-error (condition)
                  (bridge-error-message condition)))))
           (pending-pusi
             (loop for entry =
                     (bridge-processor-pending-head processor)
                       then (pending-entry-next entry)
                   while entry
                   when (= (pending-entry-slot-index entry)
                           pusi-origin)
                     return entry)))
      (check-bridge-test (null process-message))
      (check-bridge-test
       (and finish-message
            (search
             "REPACKETIZE_CAPACITY_EXHAUSTED residual_bytes="
             finish-message :test #'char=)))
      ;; EOFでも残留を将来slotへ持ち越さず、固定PUSI/PCR sourceを保つ。
      (check-bridge-test pending-pusi)
      (check-bridge-test
       (= (pending-entry-slot-index pending-pusi)
          pusi-origin))
      (check-bridge-test
       (equalp
        (pending-entry-packet pending-pusi)
        source-pusi))
      (check-bridge-test
       (= (ts-pcr (pending-entry-packet pending-pusi))
          (ts-pcr source-pusi))))))

(define-bridge-test av1-target-holdback-resolves-at-exact-two-ms
  (let* ((transport-rate-kbps 1504)
         (processor
             (make-bridge-processor
              (make-instance 'octet-collector-stream)
              :av1 :aac
              :transport-rate-kbps transport-rate-kbps))
         (assembler
           (%make-pes-assembler +test-video-pid+ :video))
         (entry
           (make-pending-entry
            :packet
            (make-ts-packet
             +test-video-pid+ 0
             (make-array 184
                         :element-type 'octet
                         :initial-element #xff))
            :slot-index 0
            :resolved-p nil
            :use-original-p nil)))
    (append-av1-stream-byte-segment
     processor assembler (octets #x11) 0)
    (enqueue-av1-stream-target-entry assembler entry)
    ;; H=2。slot 1では未確定、slot 2到達と同じ呼出しで部分targetを確定する。
    (check-bridge-test
     (zerop
      (resolve-ready-av1-stream-target-entries
       processor assembler :current-slot 1)))
    (check-bridge-test (not (pending-entry-resolved-p entry)))
    (check-bridge-test
     (= (resolve-ready-av1-stream-target-entries
         processor assembler :current-slot 2)
        1))
    (check-bridge-test (pending-entry-resolved-p entry))
    (let ((provenance
            (first
             (pending-entry-replacement-provenances entry))))
      (check-bridge-test
       (= (replacement-provenance-origin-slot provenance) 0))
      (check-bridge-test
       (= (replacement-provenance-deadline-slot provenance) 2)))))

(define-bridge-test strict-stream-reader-handles-partial-reads
  (let* ((vp9-frame
           (make-test-vp9-key-frame 0 320 180))
         (packets
           (append
            (make-test-pat-packets)
            (make-test-pmt-packets :two-audio-p nil)
            (packetize-test-video-with-pcr
             (make-pes #xbd vp9-frame 90000)
             :continuity-counter 0)))
         (input-octets (packet-list-to-octets packets))
         (input
           (make-instance
            'octet-chunk-input-stream
            :data input-octets
            :chunk-size 7))
         (output (make-instance 'octet-collector-stream)))
    (process-ts-stream input output :vp9 :aac)
    (inspect-ts-octets (collected-octets output))
    (let ((truncated-input
            (make-instance
             'octet-chunk-input-stream
             :data (subseq input-octets
                           0 (- (length input-octets) 1))
             :chunk-size 5))
          (truncated-output
            (make-instance 'octet-collector-stream)))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (process-ts-stream
           truncated-input truncated-output :vp9 :aac)))))))

(define-bridge-test split-pmt-is-rebuilt-with-valid-crc-and-continuity
  (let ((large-program-descriptors
           (list
            (make-descriptor
             :tag #x90
             :payload (make-pattern-octets 200 1))
            (make-descriptor
             :tag #x91
             :payload (make-pattern-octets 200 2))))
         (table (make-test-pmt-table :two-audio-p nil))
         (video-frame
           (make-test-vp9-key-frame 0 640 360)))
    (setf (program-map-table-program-descriptors table)
          large-program-descriptors)
    (let* ((section (build-pmt-section table))
           (pmt-packets
             (make-section-ts-packets
              +test-pmt-pid+ section 3))
           (input
             (append
              (make-test-pat-packets)
              pmt-packets
              (packetize-test-video-with-pcr
               (make-pes #xbd video-frame 90000)
               :continuity-counter 0)))
           (output
             (run-packet-processor input :vp9 :aac))
           (output-sections
             (sections-on-pid output +test-pmt-pid+)))
      (check-bridge-test (> (length pmt-packets) 1))
      (check-bridge-test (= (length output-sections) 1))
      (check-bridge-test
       (valid-crc32-mpeg2-p (first output-sections)))
      (check-bridge-test
       (payload-continuity-valid-p output +test-pmt-pid+)))))

(define-bridge-test zero-length-large-pes-completes-at-next-pusi
  (let* ((payload
           (concatenate-octets
            (make-test-vp9-key-frame 0 3840 2160)
            (make-pattern-octets 70000 17)))
         (last-index (- (length payload) 1)))
    (setf (aref payload last-index) #x55)
    (let* ((first-pes (make-pes #xbd payload 90000))
           (first-packets
             (packetize-test-video-with-pcr
              first-pes :continuity-counter 0))
           (second-pes
             (make-pes
              #xbd
              (make-test-vp9-key-frame 0 3840 2160)
              93600))
           (second-packets
             (packetize-test-video-with-pcr
              second-pes
              :continuity-counter
              (logand (length first-packets) #x0f)
              :discontinuity nil))
           (input
             (append
              (make-test-pat-packets)
              (make-test-pmt-packets :two-audio-p nil)
              first-packets second-packets))
           (output
             (run-packet-processor input :vp9 :aac))
           (rebuilt
             (first-unbounded-pes-on-pid
              output +test-video-pid+))
           (header (parse-pes-header rebuilt)))
      (check-bridge-test
       (zerop (read-u16-be first-pes 4)))
      (check-bridge-test
       (= (pes-header-stream-id header) #xe0))
      (check-bridge-test
       (equalp
        (subseq rebuilt (pes-header-payload-offset header))
        payload))
      (check-bridge-test
       (payload-continuity-valid-p
       output +test-video-pid+)))))

(define-bridge-test vp9-streaming-preserves-order-behind-unresolved-pes
  (let* ((processor
           (make-bridge-processor
            (make-instance 'octet-collector-stream)
            :vp9 :aac))
         (assembler
           (%make-pes-assembler +test-video-pid+ :video))
         (earlier-entry
           (append-pending-entry
            processor (make-output-null-packet)))
         (streaming-entry
           (append-pending-entry
            processor (make-output-null-packet))))
    (mark-entry-unresolved earlier-entry)
    (mark-entry-unresolved streaming-entry)
    (setf (pes-assembler-entries assembler)
          (list streaming-entry))
    ;; 先行eventが未解決の間は後続PESにCCを予約させない。
    (check-bridge-test
     (not
      (streaming-output-order-ready-p
       processor assembler)))
    (resolve-entry-as-original earlier-entry)
    (check-bridge-test
     (streaming-output-order-ready-p
      processor assembler))))

(define-bridge-test cli-codec-options-and-defaults
  (let ((defaults (parse-command-line '()))
        (advanced
          (parse-command-line
           '("--audio-codec" "opus"
             "--video-codec" "av1"
             "--transport-rate-kbps" "2200")))
        (mapping-version
          (parse-command-line '("--mapping-version"))))
    (check-bridge-test
     (eq (bridge-options-video-codec defaults) :passthrough))
    (check-bridge-test
     (eq (bridge-options-audio-codec defaults) :aac))
    (check-bridge-test
     (eq (bridge-options-video-codec advanced) :av1))
    (check-bridge-test
     (eq (bridge-options-audio-codec advanced) :opus))
    (check-bridge-test
     (= (bridge-options-transport-rate-kbps advanced)
        +test-transport-rate-kbps+))
    (check-bridge-test
     (eq (bridge-options-action mapping-version)
         :mapping-version))
    (check-bridge-test
     (= +vp9-mapping-version+ 1))
    (check-bridge-test
     (search "--mapping-version"
             (with-output-to-string (stream)
               (%print-help stream))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (parse-command-line
         '("--video-codec" "vp9"
           "--video-codec" "av1")))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (parse-command-line
         '("--mapping-version"
           "--mapping-version")))))))

(define-bridge-test processor-fails-on-truncated-pes
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor output :vp9 :aac))
         (packets
           (append
            (make-test-pat-packets)
            (make-test-pmt-packets :two-audio-p nil)
            (list
             (first
              (packetize-payload
               +test-video-pid+
               (make-pes
                #xbd
                (concatenate-octets
                 (make-test-vp9-key-frame 0 640 360)
                 (make-pattern-octets 500 4))
                90000)
               :payload-unit-start t))))))
    (dolist (packet packets)
      (process-bridge-packet processor packet))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (finish-bridge-processor processor))))))
