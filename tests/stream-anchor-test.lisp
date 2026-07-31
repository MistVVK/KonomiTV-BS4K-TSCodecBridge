;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defun make-test-stream-anchor-pmt-table ()
  "標準AVCとtimed-ID3を持つAnchor fixture PMTを作る。"
  (make-program-map-table
   :program-number 1
   :version 3
   :pcr-pid +test-video-pid+
   :streams
   (list
    (make-pmt-stream
     :stream-type #x1b
     :elementary-pid +test-video-pid+)
    (make-pmt-stream
     :stream-type #x0f
     :elementary-pid +test-audio-one-pid+)
    (make-pmt-stream
     :stream-type #x15
     :elementary-pid +test-timed-id3-pid+)
    (make-pmt-stream
     :stream-type #x0d
     :elementary-pid +test-data-pid+))))

(defun make-test-stream-anchor-pmt-packets ()
  "Anchor fixture PMTをTS packet列へする。"
  (make-section-ts-packets
   +test-pmt-pid+
   (build-pmt-section
    (make-test-stream-anchor-pmt-table))
   4))

(defun make-test-id3-frame (identifier payload)
  "ID3v2.4の単純なFRAMEを作る。"
  (concatenate-octets
   identifier
   (encode-id3-synchsafe-u32 (length payload))
   (octets 0 0)
   payload))

(defun make-test-stream-anchor-extra-frame (byte-count)
  "Stream Anchorと同居させるTXXX fixture frameを作る。"
  (make-test-id3-frame
   (map '(simple-array (unsigned-byte 8) (*))
        #'char-code "TXXX")
   (make-pattern-octets byte-count 71)))

(defun make-test-stream-anchor-id3
    (owner generation sequence source-time
     &key (version 1) (flags 0) (reserved 0)
       (extra-frame-byte-count 0))
  "OWNERのPRIVを持つStream Anchor用ID3v2.4 tagを作る。"
  (let ((payload
          (make-array +stream-anchor-payload-byte-count+
                      :element-type 'octet
                      :initial-element 0)))
    (setf (aref payload 0) version
          (aref payload 1) flags
          (aref payload 2) (ldb (byte 8 8) reserved)
          (aref payload 3) (ldb (byte 8 0) reserved))
    (write-stream-anchor-u64 generation payload 4)
    (write-u32-be sequence payload 12)
    (write-stream-anchor-u64 source-time payload 16)
    (let* ((extra-frame
             (if (plusp extra-frame-byte-count)
                 (make-test-stream-anchor-extra-frame
                  extra-frame-byte-count)
                 (make-array 0 :element-type 'octet)))
           (priv-frame
             (make-test-id3-frame
              +id3-priv-frame-identifier+
              (concatenate-octets
               owner
               (octets 0)
               payload)))
           (frames
             (concatenate-octets extra-frame priv-frame)))
      (concatenate-octets
       +id3-signature+
       (octets 4 0 0)
       (encode-id3-synchsafe-u32 (length frames))
       frames))))

(defun make-test-stream-anchor-pes
    (pts generation sequence source-time
     &key (owner +stream-anchor-source-owner+)
       (version 1) (flags 0) (reserved 0)
       (extra-frame-byte-count 0))
  "source marker fixture PESを作る。"
  (make-pes
   #xbd
   (make-test-stream-anchor-id3
    owner generation sequence source-time
    :version version
    :flags flags
    :reserved reserved
    :extra-frame-byte-count extra-frame-byte-count)
   pts))

(defun packetize-test-stream-anchor
    (pts generation sequence source-time
     &key (continuity-counter 0)
       (owner +stream-anchor-source-owner+)
       (version 1) (flags 0) (reserved 0)
       (extra-frame-byte-count 0))
  "source marker fixtureをtimed-ID3 PIDへpacketizeする。"
  (packetize-payload
   +test-timed-id3-pid+
   (make-test-stream-anchor-pes
    pts generation sequence source-time
    :owner owner
    :version version
    :flags flags
    :reserved reserved
    :extra-frame-byte-count extra-frame-byte-count)
   :continuity-counter continuity-counter
   :payload-unit-start t))

(defun packetize-test-anchor-video
    (pts continuity-counter &key dts)
  "小さい標準video PESをpacketizeする。"
  (packetize-payload
   +test-video-pid+
   (make-pes
    #xe0
    (make-pattern-octets 29 (logand pts #xff))
    pts
    :dts dts)
   :continuity-counter continuity-counter
   :payload-unit-start t))

(defun run-stream-anchor-finalizer-test
    (packets &key
       (maximum-distance-ticks
         +stream-anchor-default-maximum-distance-ticks+))
  "PACKETSをAnchor finalizerへ通して出力packet列を返す。"
  (let* ((output (make-instance 'octet-collector-stream))
         (finalizer
           (make-stream-anchor-finalizer
            output
            :maximum-distance-ticks maximum-distance-ticks)))
    (dolist (packet packets)
      (process-stream-anchor-packet
       finalizer (copy-seq packet)))
    (finish-stream-anchor-finalizer finalizer)
    (octets-to-packet-list (collected-octets output))))

(defun all-declared-pes-on-pid (packets pid)
  "PACKETSからPID上の宣言長PESをすべて復元する。"
  (let ((buffer
          (make-array 256
                      :element-type 'octet
                      :adjustable t
                      :fill-pointer 0))
        (expected nil)
        (active-p nil)
        (result '()))
    (labels ((finish-current ()
               (when (and active-p expected
                          (>= (length buffer) expected))
                 (push
                  (subseq buffer 0 expected)
                  result))
               (setf active-p nil
                     expected nil
                     (fill-pointer buffer) 0)))
      (dolist (packet packets)
        (when (= (ts-pid packet) pid)
          (when (ts-payload-unit-start-p packet)
            (finish-current)
            (setf active-p t))
          (when (and active-p (ts-has-payload-p packet))
            (loop for position from (ts-payload-offset packet)
                    below +ts-packet-size+
                  do (vector-push-extend
                      (aref packet position)
                      buffer))
            (when (and (null expected)
                       (>= (length buffer) 6))
              (let ((declared (read-u16-be buffer 4)))
                (when (plusp declared)
                  (setf expected (+ declared 6)))))
            (when (and expected
                       (>= (length buffer) expected))
              (finish-current)))))
      (finish-current))
    (nreverse result)))

(defun test-stream-anchor-fields (pes)
  "final Anchor PESからPTS、generation、sequence、source timeを返す。"
  (let* ((header (parse-pes-header pes))
         (id3 (subseq pes (pes-header-payload-offset header)))
         (owner-position
           (search +stream-anchor-final-owner+ id3)))
    (unless owner-position
      (bridge-error "Test final Stream Anchor owner is absent"))
    (let ((payload-start
            (+ owner-position
               (length +stream-anchor-final-owner+)
               1)))
      (values
       (pes-header-pts header)
       (read-stream-anchor-u64 id3 (+ payload-start 4))
       (read-u32-be id3 (+ payload-start 12))
       (read-stream-anchor-u64 id3 (+ payload-start 16))
       id3))))

(defun make-test-stream-anchor-prefix ()
  "Anchor fixtureのPAT/PMT packet列を返す。"
  (append
   (make-test-pat-packets)
   (make-test-stream-anchor-pmt-packets)))

(defun make-test-mapped-stream-anchor-prefix (descriptors)
  "Bridge private映像mappingを持つAnchor fixtureのPAT/PMTを作る。"
  (append
   (make-test-pat-packets)
   (make-section-ts-packets
    +test-pmt-pid+
    (build-pmt-section
     (make-program-map-table
      :program-number 1
      :version 3
      :pcr-pid +test-video-pid+
      :streams
      (list
       (make-pmt-stream
        :stream-type #x06
        :elementary-pid +test-video-pid+
        :descriptors descriptors)
       (make-pmt-stream
        :stream-type #x15
        :elementary-pid +test-timed-id3-pid+))))
    4)))

(define-bridge-test stream-anchor-exact-match-finalizes-owner-and-pts
  (let* ((input-anchor
           (packetize-test-stream-anchor
            90000 7 1 123456))
         (output
           (run-stream-anchor-finalizer-test
            (append
             (make-test-stream-anchor-prefix)
             input-anchor
             (packetize-test-anchor-video 90000 0))))
         (pes
           (first
            (all-declared-pes-on-pid
             output +test-timed-id3-pid+))))
    (multiple-value-bind
          (pts generation sequence source-time id3)
        (test-stream-anchor-fields pes)
      (check-bridge-test (= pts 90000))
      (check-bridge-test (= generation 7))
      (check-bridge-test (= sequence 1))
      (check-bridge-test (= source-time 123456))
      (check-bridge-test
       (null (search +stream-anchor-source-owner+ id3))))))

(define-bridge-test stream-anchor-nearest-tie-prefers-past-au
  (let* ((output
           (run-stream-anchor-finalizer-test
            (append
             (make-test-stream-anchor-prefix)
             (packetize-test-anchor-video 99000 0)
             (packetize-test-stream-anchor
              100000 8 10 500000)
             (packetize-test-anchor-video 101000 1))))
         (pes
           (first
            (all-declared-pes-on-pid
             output +test-timed-id3-pid+))))
    (multiple-value-bind
          (pts generation sequence source-time)
        (test-stream-anchor-fields pes)
      (check-bridge-test (= pts 99000))
      (check-bridge-test (= generation 8))
      (check-bridge-test (= sequence 10))
      (check-bridge-test (= source-time 499000)))))

(define-bridge-test stream-anchor-distance-excess-fails-closed
  (check-bridge-test
   (signals-bridge-error-p
    (lambda ()
      (run-stream-anchor-finalizer-test
       (append
        (make-test-stream-anchor-prefix)
        (packetize-test-anchor-video 90000 0)
        (packetize-test-stream-anchor
         100000 9 1 100000)
        (packetize-test-anchor-video 110000 1)))))))

(define-bridge-test stream-anchor-invalid-payload-fails-closed
  (dolist
      (arguments
       (list
        (list :version 2)
        (list :flags 1)
        (list :reserved 1)))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (run-stream-anchor-finalizer-test
         (append
          (make-test-stream-anchor-prefix)
          (apply
           #'packetize-test-stream-anchor
           90000 10 1 90000 arguments)
          (packetize-test-anchor-video 90000 0))))))))

(define-bridge-test stream-anchor-sequence-rollback-fails-closed
  (check-bridge-test
   (signals-bridge-error-p
    (lambda ()
      (run-stream-anchor-finalizer-test
       (append
        (make-test-stream-anchor-prefix)
        (packetize-test-anchor-video 90000 0)
        (packetize-test-stream-anchor
         90000 11 2 90000
         :continuity-counter 0)
        (packetize-test-anchor-video 112500 1)
        (packetize-test-stream-anchor
         112500 11 1 112500
         :continuity-counter 1)))))))

(define-bridge-test stream-anchor-generation-change-allows-sequence-reset
  (let* ((output
           (run-stream-anchor-finalizer-test
            (append
             (make-test-stream-anchor-prefix)
             (packetize-test-stream-anchor
              90000 20 99 90000
              :continuity-counter 0)
             (packetize-test-anchor-video 90000 0)
             (packetize-test-stream-anchor
              112500 21 0 112500
              :continuity-counter 1)
             (packetize-test-anchor-video 112500 1))))
         (fields
           (mapcar
            (lambda (pes)
              (multiple-value-bind
                    (pts generation sequence source-time id3)
                  (test-stream-anchor-fields pes)
                (declare (ignore pts source-time id3))
                (list generation sequence)))
            (all-declared-pes-on-pid
             output +test-timed-id3-pid+))))
    (check-bridge-test
     (equal fields '((20 99) (21 0))))))

(define-bridge-test stream-anchor-preserves-other-timed-id3-byte-exact
  (let* ((other-owner
           (map '(simple-array (unsigned-byte 8) (*))
                #'char-code
                "com.example.other"))
         (other
           (packetize-test-stream-anchor
            90000 99 77 90000
            :owner other-owner))
         (output
           (run-stream-anchor-finalizer-test
            (append
             (make-test-stream-anchor-prefix)
             other
             (packetize-test-anchor-video 90000 0)))))
    (dolist (packet other)
      (check-bridge-test
       (find packet output :test #'equalp)))))

(define-bridge-test stream-anchor-multiple-packet-id3-preserves-packet-count
  (let* ((extra-frame
           (make-test-stream-anchor-extra-frame 300))
         (source
           (packetize-test-stream-anchor
            90000 12 1 200000
            :extra-frame-byte-count 300))
         (interleaved-source
           (loop for remaining on source
                 append
                 (if (rest remaining)
                     (list
                      (first remaining)
                      (make-output-null-packet))
                     (list (first remaining)))))
         (input
           (append
            (make-test-stream-anchor-prefix)
            interleaved-source
            (packetize-test-anchor-video 90000 0)))
         (output
           (run-stream-anchor-finalizer-test
            input))
         (output-anchor-packets
           (remove
            +test-timed-id3-pid+
            output
            :key #'ts-pid
            :test-not #'=))
         (pes
           (first
            (all-declared-pes-on-pid
             output +test-timed-id3-pid+))))
    (check-bridge-test (> (length source) 1))
    (check-bridge-test
     (= (length source) (length output-anchor-packets)))
    (check-bridge-test
     (equal
      (mapcar #'ts-pid input)
      (mapcar #'ts-pid output)))
    (multiple-value-bind
          (pts generation sequence source-time id3)
        (test-stream-anchor-fields pes)
      (check-bridge-test (= pts 90000))
      (check-bridge-test (= generation 12))
      (check-bridge-test (= sequence 1))
      (check-bridge-test (= source-time 200000))
      (check-bridge-test
       (equalp
        extra-frame
        (subseq id3 10 (+ 10 (length extra-frame)))))
      (check-bridge-test
       (null (search +stream-anchor-source-owner+ id3))))))

(define-bridge-test stream-anchor-preserves-adaptation-only-slot-and-payload-cc
  (let* ((source
           (packetize-test-stream-anchor
            90000 12 1 200000
            :extra-frame-byte-count 300))
         (adaptation-only
           (make-test-adaptation-only-packet
            +test-timed-id3-pid+
            (ts-continuity-counter (first source))))
         (source-with-adaptation
           (append
            (list (first source) adaptation-only)
            (rest source)))
         (input
           (append
            (make-test-stream-anchor-prefix)
            source-with-adaptation
            (packetize-test-anchor-video 90000 0)))
         (output
           (run-stream-anchor-finalizer-test input))
         (output-id3-packets
           (remove
            +test-timed-id3-pid+
            output
            :key #'ts-pid
            :test-not #'=))
         (output-payload-packets
           (remove-if-not #'ts-has-payload-p output-id3-packets))
         (pes
           (first
            (all-declared-pes-on-pid
             output +test-timed-id3-pid+))))
    (check-bridge-test (> (length source) 1))
    (check-bridge-test
     (= (length source-with-adaptation)
        (length output-id3-packets)))
    (check-bridge-test
     (equal
      (mapcar #'ts-pid input)
      (mapcar #'ts-pid output)))
    (check-bridge-test
     (= 1 (count adaptation-only output :test #'equalp)))
    (check-bridge-test
     (equal
      (mapcar #'ts-continuity-counter source)
      (mapcar #'ts-continuity-counter output-payload-packets)))
    (check-bridge-test
     (= 2 (ts-adaptation-field-control
           (second output-id3-packets))))
    (multiple-value-bind
          (pts generation sequence source-time id3)
        (test-stream-anchor-fields pes)
      (declare (ignore id3))
      (check-bridge-test (= pts 90000))
      (check-bridge-test (= generation 12))
      (check-bridge-test (= sequence 1))
      (check-bridge-test (= source-time 200000)))))

(define-bridge-test stream-anchor-duplicate-source-packet-mirrors-final-marker
  (let* ((source
           (first
            (packetize-test-stream-anchor
             90000 3 1 200000)))
         (output
           (run-stream-anchor-finalizer-test
            (append
             (make-test-stream-anchor-prefix)
             (list source (copy-seq source))
             (packetize-test-anchor-video 90000 0))))
         (output-id3-packets
           (remove
            +test-timed-id3-pid+
            output
            :key #'ts-pid
            :test-not #'=))
         (output-pes
           (all-declared-pes-on-pid
            output +test-timed-id3-pid+)))
    (check-bridge-test (= 2 (length output-id3-packets)))
    (check-bridge-test
     (equalp
      (first output-id3-packets)
      (second output-id3-packets)))
    (check-bridge-test (= 2 (length output-pes)))
    (dolist (pes output-pes)
      (multiple-value-bind
            (pts generation sequence source-time id3)
          (test-stream-anchor-fields pes)
        (check-bridge-test (= pts 90000))
        (check-bridge-test (= generation 3))
        (check-bridge-test (= sequence 1))
        (check-bridge-test (= source-time 200000))
        (check-bridge-test
         (null (search +stream-anchor-source-owner+ id3)))))))

(define-bridge-test stream-anchor-last-payload-entries-do-not-retain-flushed-queue
  (let* ((output (make-instance 'octet-collector-stream))
         (finalizer (make-stream-anchor-finalizer output))
         (packets
           (append
            (make-test-stream-anchor-prefix)
            (packetize-test-stream-anchor
             90000 3 1 200000)
            (packetize-test-anchor-video 90000 0)
            (loop for index from 0 below 32
                  collect
                  (make-ts-packet
                   +test-data-pid+
                   (logand index #x0f)
                   (octets index))))))
    (dolist (packet packets)
      (process-stream-anchor-packet
       finalizer (copy-seq packet)))
    (finish-stream-anchor-finalizer finalizer)
    (check-bridge-test
     (loop
       for entry across
       (stream-anchor-finalizer-last-payload-entries finalizer)
       always
       (or
        (null entry)
        (null (stream-anchor-pending-entry-next entry)))))))

(define-bridge-test stream-anchor-three-pending-markers-keep-sequence-order
  (let* ((output
           (run-stream-anchor-finalizer-test
            (append
             (make-test-stream-anchor-prefix)
             (packetize-test-stream-anchor
              90000 13 1 90000
              :continuity-counter 0)
             (packetize-test-stream-anchor
              112500 13 2 112500
              :continuity-counter 1)
             (packetize-test-stream-anchor
              135000 13 3 135000
              :continuity-counter 2)
             (packetize-test-anchor-video 90000 0)
             (packetize-test-anchor-video 112500 1)
             (packetize-test-anchor-video 135000 2))))
         (pes-list
           (all-declared-pes-on-pid
            output +test-timed-id3-pid+))
         (sequences
           (mapcar
            (lambda (pes)
              (nth-value 2
                         (test-stream-anchor-fields pes)))
            pes-list)))
    (check-bridge-test (equal sequences '(1 2 3)))))

(define-bridge-test stream-anchor-wraparound-exact-match
  (let* ((pts (- +pts-modulus+ 100))
         (output
           (run-stream-anchor-finalizer-test
            (append
             (make-test-stream-anchor-prefix)
             (packetize-test-stream-anchor
              pts 14 1 800000)
             (packetize-test-anchor-video pts 0)
             (packetize-test-anchor-video 200 1))))
         (pes
           (first
            (all-declared-pes-on-pid
             output +test-timed-id3-pid+))))
    (check-bridge-test
     (= (nth-value 0 (test-stream-anchor-fields pes))
        pts))))

(define-bridge-test stream-anchor-cli-forces-semantic-path
  (let ((options
          (parse-command-line
           '("--stream-anchor-v1"
             "--stream-anchor-max-distance-ticks"
             "3754"))))
    (check-bridge-test
     (bridge-options-stream-anchor-v1-p options))
    (check-bridge-test
     (= (bridge-options-stream-anchor-maximum-distance-ticks
         options)
        3754))
    (check-bridge-test
     (advanced-codec-selection-p options))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (parse-command-line
         '("--stream-anchor-max-distance-ticks" "10")))))))

(define-bridge-test stream-anchor-passthrough-aac-stream-integration
  (let* ((video-pes
           (make-pes
            #xe0 (make-pattern-octets 29 41) 90000))
         (packets
           (append
            (make-test-stream-anchor-prefix)
            (packetize-test-stream-anchor
             90000 15 1 345678)
            (packetize-test-video-with-pcr video-pes)))
         (input
           (make-instance
            'octet-chunk-input-stream
            :data (packet-list-to-octets packets)
            :chunk-size 113))
         (output (make-instance 'octet-collector-stream)))
    (process-ts-stream
     input output :passthrough :aac
     :stream-anchor-v1-p t)
    (let* ((output-packets
             (octets-to-packet-list
              (collected-octets output)))
           (pes
             (first
              (all-declared-pes-on-pid
               output-packets +test-timed-id3-pid+))))
      (multiple-value-bind
            (pts generation sequence source-time)
          (test-stream-anchor-fields pes)
        (check-bridge-test (= pts 90000))
        (check-bridge-test (= generation 15))
        (check-bridge-test (= sequence 1))
        (check-bridge-test (= source-time 345678))))))

(define-bridge-test stream-anchor-vp9-semantic-stream-integration
  (let* ((vp9-frame
           (concatenate-octets
            (make-test-vp9-key-frame 0 640 360)
            (make-pattern-octets 120 39)))
         (video-pes
           (make-pes #xbd vp9-frame 90000 :dts 87000))
         (packets
           (append
            (make-test-pat-packets)
            (make-test-pmt-packets :two-audio-p nil)
            (packetize-test-stream-anchor
             90000 16 1 567890)
            (packetize-test-video-with-pcr video-pes)))
         (input
           (make-instance
            'octet-chunk-input-stream
            :data (packet-list-to-octets packets)
            :chunk-size 97))
         (output (make-instance 'octet-collector-stream)))
    (process-ts-stream
     input output :vp9 :aac
     :stream-anchor-v1-p t)
    (let* ((output-packets
             (octets-to-packet-list
              (collected-octets output)))
           (pes
             (first
              (all-declared-pes-on-pid
               output-packets +test-timed-id3-pid+))))
      (multiple-value-bind
            (pts generation sequence source-time)
          (test-stream-anchor-fields pes)
        (check-bridge-test (= pts 90000))
        (check-bridge-test (= generation 16))
        (check-bridge-test (= sequence 1))
        (check-bridge-test (= source-time 567890))))))

(define-bridge-test stream-anchor-recognizes-bridge-video-mappings
  (dolist
      (descriptors
       (list
        (make-vp9-mapping-descriptors)
        (make-av1-mapping-descriptors
         (make-av1-codec-configuration
          :level 8))))
    (check-bridge-test
     (stream-anchor-video-stream-p
      (make-pmt-stream
       :stream-type #x06
       :elementary-pid +test-video-pid+
       :descriptors descriptors)))))

(define-bridge-test stream-anchor-finalizes-av1-private-stream-id
  (let* ((configuration
           (make-av1-codec-configuration :level 8))
         (output
           (run-stream-anchor-finalizer-test
            (append
             (make-test-mapped-stream-anchor-prefix
              (make-av1-mapping-descriptors configuration))
             (packetize-test-stream-anchor
              90000 16 1 456789)
             (packetize-payload
              +test-video-pid+
              (make-pes
               #xbd (make-pattern-octets 29 82) 90000)
              :payload-unit-start t))))
         (pes
           (first
            (all-declared-pes-on-pid
             output +test-timed-id3-pid+))))
    (multiple-value-bind
          (pts generation sequence source-time)
        (test-stream-anchor-fields pes)
      (check-bridge-test (= pts 90000))
      (check-bridge-test (= generation 16))
      (check-bridge-test (= sequence 1))
      (check-bridge-test (= source-time 456789)))))
