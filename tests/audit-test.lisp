;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defun run-validating-fast-path (octets &key (chunk-size 17))
  "OCTETSを通常の検証付きfast pathへ通した結果を返す。"
  (let ((input
          (make-instance
           'octet-chunk-input-stream
           :data octets
           :chunk-size chunk-size))
        (output (make-instance 'octet-collector-stream)))
    (validate-and-copy-ts-stream input output)
    (collected-octets output)))

(defun run-strict-pass-through
    (octets &key (chunk-size 17))
  "OCTETSを全PID厳格検証付きpass-through相当経路へ通した結果を返す。"
  (let ((input
          (make-instance
           'octet-chunk-input-stream
           :data octets
           :chunk-size chunk-size))
        (output (make-instance 'octet-collector-stream)))
    (validate-and-copy-ts-stream input output)
    (collected-octets output)))

(defun rewrite-psi-crc (section)
  "変更したSECTIONの末尾CRCを再計算する。"
  (write-u32-be
   (crc32-mpeg2 section :end (- (length section) 4))
   section (- (length section) 4))
  section)

(defun make-audit-video-stream (&key descriptors (pid +test-video-pid+))
  "映像mapping分類test用private streamを作る。"
  (make-pmt-stream
   :stream-type #x06
   :elementary-pid pid
   :descriptors descriptors))

(defun make-audit-registration (identifier)
  "4-byte IDENTIFIERのregistration descriptorを作る。"
  (make-descriptor :tag #x05 :payload (copy-seq identifier)))

(defun make-audit-av1-configuration (level)
  "時系列test用AV1 configurationを作る。"
  (make-av1-codec-configuration
   :profile 0 :level level :tier 0
   :high-bitdepth 0 :hdr-wcg-idc 0))

(defun make-audit-av1-hdr-cll-obu ()
  "HDR_CLL metadataと正しいtrailing_bitsを持つOBUを作る。"
  (octets #x2a 6 1 0 100 0 50 #x80))

(defun make-audit-pcr-packet
    (pcr &key (counter 0) discontinuity)
  "PCR検証test用packetを作る。"
  (let ((packet
          (make-ts-packet
           +test-video-pid+ counter
           (octets 1 2 3)
           :payload-unit-start t)))
    (when discontinuity
      (setf (aref packet 5)
            (logior (aref packet 5) #x80)))
    (set-test-pcr packet pcr)))

(defun bridge-error-message-contains-p (function marker)
  "FUNCTIONのBRIDGE-ERROR messageがMARKERを含むか返す。"
  (handler-case
      (progn
        (funcall function)
        nil)
    (bridge-error (condition)
      (not
       (null
        (search marker
                (bridge-error-message condition)
                :test #'char=))))))

(define-bridge-test validating-fast-path-is-strict-and-byte-exact
  (let* ((packets
           (loop for counter from 0 below 4
                 collect
                 (make-ts-packet
                  #x120 counter
                  (make-pattern-octets
                   (if (evenp counter) 184 91)
                   counter))))
         (valid (packet-list-to-octets packets)))
    (dolist (chunk-size '(1 7 187 188 189 12032))
      (check-bridge-test
       (equalp valid
               (run-validating-fast-path
                valid :chunk-size chunk-size))))
    (dolist (length '(1 187 189))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (run-validating-fast-path
           (subseq valid 0 length)
           :chunk-size 5)))))
    (let ((bad-sync (copy-seq valid))
          (bad-control (copy-seq valid))
          (bad-adaptation (copy-seq valid)))
      (setf (aref bad-sync 0) 0
            (aref bad-control 3)
            (logand (aref bad-control 3) #xcf)
            (aref bad-adaptation (+ +ts-packet-size+ 4))
            183)
      (dolist (corrupt
               (list bad-sync bad-control bad-adaptation))
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (run-validating-fast-path corrupt))))))))

(define-bridge-test validating-fast-path-checks-payload-integrity
  (let* ((first
           (make-ts-packet
            #x120 3 (make-pattern-octets 184 1)))
         (second
           (make-ts-packet
            #x120 4 (make-pattern-octets 90 2)))
         (valid
           (packet-list-to-octets
            (list first second))))
    (dolist (corruption
             (list
              (lambda (octets)
                (setf
                 (aref octets 1)
                 (logior (aref octets 1) #x80)))
              (lambda (octets)
                (setf
                 (aref octets 3)
                 (logior (aref octets 3) #x80)))
              (lambda (octets)
                (setf
                 (aref octets (+ +ts-packet-size+ 3))
                 (logior
                  (logand
                   (aref octets (+ +ts-packet-size+ 3))
                   #xf0)
                  7)))))
      (let ((corrupt (copy-seq valid)))
        (funcall corruption corrupt)
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (run-validating-fast-path corrupt))))))))

(define-bridge-test validating-fast-path-handles-duplicates-and-discontinuity
  (let* ((first
           (make-ts-packet
            #x121 7 (make-pattern-octets 80 3)))
         (duplicate (copy-seq first))
         (next
           (make-ts-packet
            #x121 8 (make-pattern-octets 184 4)))
         (valid-duplicate
           (packet-list-to-octets
            (list first duplicate next))))
    (check-bridge-test
     (equalp
      valid-duplicate
      (run-validating-fast-path valid-duplicate)))
    (let ((conflict (copy-seq duplicate)))
      (setf (aref conflict (ts-payload-offset conflict))
            (logxor
             (aref conflict (ts-payload-offset conflict))
             1))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (run-validating-fast-path
           (packet-list-to-octets
            (list first conflict))))))))
  (let* ((first
           (make-ts-packet
            #x122 2 (make-pattern-octets 184 5)))
         (adaptation-discontinuity
           (make-test-adaptation-only-packet
            #x122 14 :discontinuity t))
         (rebased
           (make-ts-packet
            #x122 11 (make-pattern-octets 77 6)))
         (following
           (make-ts-packet
            #x122 12 (make-pattern-octets 184 7)))
         (valid
           (packet-list-to-octets
            (list first
                  adaptation-discontinuity
                  rebased
                  following))))
    (check-bridge-test
     (equalp valid (run-validating-fast-path valid))))
  (let ((first
          (make-ts-packet
           #x122 2 (make-pattern-octets 184 5)))
        (adaptation-without-discontinuity
          (make-test-adaptation-only-packet #x122 14))
        (bad-rebase
          (make-ts-packet
           #x122 11 (make-pattern-octets 77 6))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (run-validating-fast-path
         (packet-list-to-octets
          (list first
                adaptation-without-discontinuity
                bad-rebase)))))))
  (let ((first
          (make-ts-packet
           #x123 4 (make-pattern-octets 184 8)))
        (rebased
          (make-ts-packet
           #x123 13 (make-pattern-octets 70 9)))
        (following
          (make-ts-packet
           #x123 14 (make-pattern-octets 184 10))))
    (setf (aref rebased 5)
          (logior (aref rebased 5) #x80))
    (let ((valid
            (packet-list-to-octets
             (list first rebased following))))
      (check-bridge-test
       (equalp valid (run-validating-fast-path valid))))))

(define-bridge-test pass-through-validates-ts-framing-and-header
  (let* ((first
           (make-ts-packet
            #x130 0 (make-pattern-octets 184 11)))
         (second
           (make-ts-packet
            #x130 1 (make-pattern-octets 51 12)))
         (valid
           (packet-list-to-octets
            (list first second))))
    (dolist (chunk-size '(1 187 188 189 12032))
      (check-bridge-test
       (equalp
        valid
        (run-strict-pass-through
         valid :chunk-size chunk-size))))
    (dolist (length '(1 187 189 375))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (run-strict-pass-through
           (subseq valid 0 length)
           :chunk-size 7)))))
    (let ((bad-sync (copy-seq valid))
          (bad-control (copy-seq valid))
          (bad-tei (copy-seq valid))
          (bad-scrambling (copy-seq valid))
          (bad-continuity (copy-seq valid)))
      (setf (aref bad-sync 0) 0
            (aref bad-control 3)
            (logand (aref bad-control 3) #xcf)
            (aref bad-tei 1)
            (logior (aref bad-tei 1) #x80)
            (aref bad-scrambling 3)
            (logior (aref bad-scrambling 3) #x80)
            (aref bad-continuity (+ +ts-packet-size+ 3))
            (logior
             (logand
              (aref bad-continuity (+ +ts-packet-size+ 3))
              #xf0)
             7))
      (dolist (corrupt
               (list bad-sync bad-control bad-tei
                     bad-scrambling bad-continuity))
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (run-strict-pass-through corrupt))))))))

(define-bridge-test transport-adaptation-and-pcr-are-strict
  (let ((adaptation-only
          (make-test-adaptation-only-packet
           #x140 3 :discontinuity t))
        (payload
          (make-ts-packet #x140 4 (octets 1 2 3))))
    (validate-ts-packet adaptation-only)
    (setf (aref adaptation-only 4) 182)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda () (validate-ts-packet adaptation-only))))
    (setf (aref payload 5) #x10
          (aref payload 4) 1)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda () (validate-ts-packet payload)))))
  (let ((pcr (make-audit-pcr-packet 27000000)))
    (check-bridge-test (= (ts-pcr pcr) 27000000))
    (setf (aref pcr 10)
          (logand (aref pcr 10) #x81))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda () (ts-pcr pcr)))))
  (let ((pcr (make-audit-pcr-packet 27000000)))
    (setf (aref pcr 10)
          (logior (logand (aref pcr 10) #xfe) 1)
          (aref pcr 11) #xff)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda () (ts-pcr pcr))))))

(define-bridge-test repacketized-zero-length-adaptation-is-valid
  (let* ((template
           (make-ts-packet
            #x141 2
            (make-pattern-octets 183 7)))
         (packet
           (make-adaptation-only-from-template
            template 2)))
    (check-bridge-test
     (zerop (ts-adaptation-field-length template)))
    (validate-ts-packet packet)
    (check-bridge-test
     (zerop (ts-adaptation-flags packet)))))

(define-bridge-test pusi-psi-stuffing-must-be-all-ff
  (let* ((section
           (build-pat-section
            (make-program-association-table
             :transport-stream-id 1
             :version 2
             :programs
             (list
              (make-pat-program
               :program-number 1
               :pid +test-pmt-pid+)))))
         (payload
           (make-array 184
                       :element-type 'octet
                       :initial-element #xff))
         (packet nil)
         (section-end
           (+ 4 1 (length section))))
    (setf (aref payload 0) 0)
    (replace payload section :start1 1)
    (setf packet
          (make-test-payload-only-packet
           0 0 payload :payload-unit-start t))
    (check-bridge-test
     (= (aref packet section-end) #xff))
    (setf (aref packet (+ section-end 1)) 0)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (feed-section-packet
         (make-section-assembler 0)
         packet))))))

(define-bridge-test psi-reserved-bits-fail-with-valid-crc
  (let ((pat
          (build-pat-section
           (make-program-association-table
            :transport-stream-id 1
            :programs
            (list
             (make-pat-program
              :program-number 1 :pid +test-pmt-pid+))))))
    (setf (aref pat 1) (logand (aref pat 1) #x3f))
    (rewrite-psi-crc pat)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda () (parse-pat-section pat)))))
  (let ((pat
          (build-pat-section
           (make-program-association-table
            :transport-stream-id 1
            :programs
            (list
             (make-pat-program
              :program-number 1 :pid +test-pmt-pid+))))))
    (setf (aref pat 10) 0)
    (rewrite-psi-crc pat)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda () (parse-pat-section pat)))))
  (dolist (offset '(8 10 13 15))
    (let ((pmt
            (build-pmt-section
             (make-test-pmt-table :two-audio-p nil))))
      (setf (aref pmt offset) 0)
      (rewrite-psi-crc pmt)
      (check-bridge-test
       (signals-bridge-error-p
        (lambda () (parse-pmt-section pmt)))
       (format nil "PMT reserved field offset ~D" offset)))))

(define-bridge-test video-input-mapping-classification-is-unambiguous
  (let ((bare (make-audit-video-stream))
        (av1-hint
          (make-audit-video-stream
           :descriptors
           (list
            (make-audit-registration
             +av1-registration-identifier+))))
        (vp9-hint
          (make-audit-video-stream
           :descriptors
           (list
            (make-audit-registration
             +vp9-registration-identifier+))))
        (unknown
          (make-audit-video-stream
           :descriptors
           (list
            (make-audit-registration
             (octets #x58 #x58 #x58 #x58)))))
        (subtitle
          (make-audit-video-stream
           :pid +test-subtitle-pid+
           :descriptors
           (list
            (make-descriptor
             :tag #xfd
             :payload (octets #x00 #x08))))))
    (check-bridge-test
     (eq (classify-video-input-stream bare :av1) :bare))
    (check-bridge-test
     (eq (classify-video-input-stream av1-hint :av1)
         :registration-hint))
    (check-bridge-test
     (eq (classify-video-input-stream vp9-hint :vp9)
         :registration-hint))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (classify-video-input-stream av1-hint :vp9))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (classify-video-input-stream unknown :vp9))))
    (check-bridge-test
     (null (classify-video-input-stream subtitle :av1)))
    (check-bridge-test
     (= (select-video-pid
         (make-program-map-table
          :program-number 1
          :pcr-pid +test-video-pid+
          :streams (list bare subtitle))
         :av1)
        +test-video-pid+)))
  (let* ((complete
           (make-av1-mapping-descriptors
            (make-audit-av1-configuration 8)))
         (stream
           (make-audit-video-stream :descriptors complete)))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (classify-video-input-stream stream :av1))))))

(define-bridge-test opus-packet-size-and-trim-limits
  (check-bridge-test
   (= (validate-opus-packet
       (concatenate-octets
        (octets #xf8)
        (make-pattern-octets 1275 1)))
      960))
  (check-bridge-test
   (signals-bridge-error-p
    (lambda ()
      (validate-opus-packet
       (concatenate-octets
        (octets #xf8)
        (make-pattern-octets 1276 1))))))
  (check-bridge-test
   (= (validate-opus-packet (octets #xf8)) 960))
  (check-bridge-test
   (= (validate-opus-packet (octets #xf9)) 1920))
  (check-bridge-test
   (= (validate-opus-packet (octets #xfa 0)) 1920))
  (check-bridge-test
   (= (validate-opus-packet (octets #xfb 2)) 1920))
  (check-bridge-test
   (= (validate-opus-packet (octets #xfb #x82 0)) 1920))
  (check-bridge-test
   (= (validate-opus-packet (octets #xfb #x82 1 #xaa))
      1920))
  (let* ((raw (octets #xf8 1))
         (payload
           (concatenate-octets
            (octets #x7f #xf8 2 #x02 #x58 #x01 #xf4)
            raw)))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (parse-ffmpeg-opus-control-payload payload))))))

(define-bridge-test opus-descriptor-rejects-unknown-extension
  (let ((descriptors
          (append
           (make-test-opus-descriptors 2 :registration t)
           (list
            (make-descriptor
             :tag #x7f :payload (octets #x81 0))))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-opus-descriptor-members descriptors)))))
  (let ((descriptors
          (append
           (make-test-opus-descriptors 2)
           (list
            (make-audit-registration
             (octets #x41 #x41 #x43 #x20))))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-opus-descriptor-members descriptors))))))

(define-bridge-test pusi-discontinuity-rebases-new-pes
  (let ((first-pes
          (make-test-opus-pes 90000 (octets #xf8)))
        (second-pes
          (make-test-opus-pes 180000 (octets #xf8)))
        (third-pes
          (make-test-opus-pes 181800 (octets #xf8))))
    ;; 長さ0の旧PESは次のPUSIまでactiveであり続ける。
    (write-u16-be 0 first-pes 4)
    (let ((first
            (first
             (packetize-payload
              +test-audio-one-pid+ first-pes
              :continuity-counter 5
              :payload-unit-start t)))
          ;; discontinuity packetは旧packetと同一CCでも新baselineになる。
          (second
            (first
             (packetize-payload
              +test-audio-one-pid+ second-pes
              :continuity-counter 5
              :payload-unit-start t)))
          (third
            (first
             (packetize-payload
              +test-audio-one-pid+ third-pes
              :continuity-counter 6
              :payload-unit-start t))))
      (setf (aref second 5)
            (logior (aref second 5) #x80))
      (let ((output
              (run-packet-processor
               (append
                (make-test-pat-packets)
                (make-test-pmt-packets :two-audio-p nil)
                (list
                 (make-test-program-pcr-packet 90000)
                 first
                 (make-test-program-pcr-packet
                  180000 :counter 0 :discontinuity t)
                 second third))
               :passthrough :opus)))
        (dolist (packet (list first second third))
          (check-bridge-test
           (find packet output :test #'equalp)))))))

(define-bridge-test continuation-discontinuity-rebases-next-pes
  (let* ((initial
           (packetize-payload
            +test-audio-one-pid+
            (make-test-opus-pes 90000 (octets #xf8))
            :continuity-counter 0
            :payload-unit-start t))
         (continued
           (packetize-payload
            +test-audio-one-pid+
            (make-test-opus-pes
             91800
             (concatenate-octets
              (octets #xf8)
              (make-pattern-octets 250 11)))
            :continuity-counter 1
            :payload-unit-start t))
         (jumped
           (packetize-payload
            +test-audio-one-pid+
            (make-test-opus-pes 180000 (octets #xf8))
            :continuity-counter 3
            :payload-unit-start t))
         (following
           (packetize-payload
            +test-audio-one-pid+
            (make-test-opus-pes 181800 (octets #xf8))
            :continuity-counter 4
            :payload-unit-start t))
         (audio-packets
           (append initial continued jumped following)))
    (check-bridge-test (= (length continued) 2))
    (setf (aref (second continued) 5)
          (logior (aref (second continued) 5) #x80))
    (let ((output
            (run-packet-processor
             (append
              (make-test-pat-packets)
              (make-test-pmt-packets :two-audio-p nil)
              (list
               (make-test-program-pcr-packet 90000))
              initial continued
              (list
               (make-test-program-pcr-packet
                180000 :counter 0 :discontinuity t))
              jumped following)
             :passthrough :opus)))
      (dolist (packet audio-packets)
        (check-bridge-test
         (find packet output :test #'equalp))))))

(define-bridge-test discontinuity-duplicate-is-still-dropped
  (let ((assembler
          (%make-pes-assembler +test-audio-one-pid+ :opus))
        (packet
          (make-ts-packet
           +test-audio-one-pid+ 7 (octets 1 2 3))))
    (setf (aref packet 5)
          (logior (aref packet 5) #x80))
    (check-bridge-test
     (validate-pes-transport-continuity assembler packet))
    (check-bridge-test
     (not
      (validate-pes-transport-continuity
       assembler (copy-seq packet))))))

(define-bridge-test av1-obu-boundaries-and-tile-list
  (let ((tile-list (octets #x42 1 0)))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (parse-av1-obus tile-list))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (transform-av1-stream-chunk
         (make-av1-stream-transformer)
         tile-list))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (parse-av1-stream-prefix-metadata
         tile-list nil nil)))))
  (dolist (header '(#x02 #x4a #x72))
    (let ((reserved-obu (octets header 0)))
      (check-bridge-test
       (bridge-error-message-contains-p
        (lambda ()
          (parse-av1-obus reserved-obu))
        "AV1_INPUT_SUBSET_OBU_TYPE_UNSUPPORTED"))
      (check-bridge-test
       (bridge-error-message-contains-p
        (lambda ()
          (transform-av1-stream-chunk
           (make-av1-stream-transformer)
           reserved-obu))
        "AV1_INPUT_SUBSET_OBU_TYPE_UNSUPPORTED"))))
  (let* ((without-size (octets #x30 0 0 1 2))
         (converted
           (convert-av1-access-unit-to-ts-format without-size))
         (transformer (make-av1-stream-transformer))
         (streamed
           (transform-av1-stream-chunk
            transformer without-size)))
    (finish-av1-stream-transformer transformer)
    (check-bridge-test (equalp converted streamed))
    (check-bridge-test
     (equalp converted
             (octets 0 0 1 #x30 0 0 #x03 1 2)))))

(define-bridge-test av1-random-access-uses-temporal-unit-sequence-header
  (let* ((full (make-test-av1-non-reduced-access-unit))
         (obus (parse-av1-obus full))
         (sequence (first obus))
         (frame (second obus))
         (sequence-bytes
           (subseq full
                   (av1-obu-start sequence)
                   (av1-obu-end sequence)))
         (visible-frame
           (subseq full
                   (av1-obu-start frame)
                   (av1-obu-end frame)))
         (hidden-frame (copy-seq visible-frame))
         (temporal-delimiter (octets #x12 0))
         (show-existing-frame (copy-seq visible-frame)))
    (setf (aref hidden-frame
                (- (av1-obu-payload-start frame)
                   (av1-obu-start frame)))
          #x00
          (aref show-existing-frame
                (- (av1-obu-payload-start frame)
                   (av1-obu-start frame)))
          #x80)
    (check-bridge-test
     (eq (av1-access-unit-random-access-kind
          full nil)
         :key))
    (check-bridge-test
     (null
      (av1-access-unit-random-access-kind
       visible-frame nil)))
    (check-bridge-test
     (eq (av1-access-unit-random-access-kind
          visible-frame nil t)
         :key))
    (check-bridge-test
     (eq (av1-access-unit-random-access-kind
          hidden-frame nil t)
         :delayed-key))
    (check-bridge-test
     (eq (av1-access-unit-random-access-kind
          (concatenate-octets sequence-bytes hidden-frame)
          nil)
         :delayed-key))
    (check-bridge-test
     (null
      (av1-access-unit-random-access-kind
       (concatenate-octets temporal-delimiter visible-frame)
       nil t)))
    (check-bridge-test
     (bridge-error-message-contains-p
      (lambda ()
        (av1-access-unit-random-access-kind
         (concatenate-octets sequence-bytes show-existing-frame)
         nil))
      "AV1_FRAME_OBU_SHOW_EXISTING_FORBIDDEN"))
    (multiple-value-bind
          (kind semantics delimiter-p sequence-p)
        (av1-access-unit-random-access-kind
         (octets #x1a 1 #x88)
         nil)
      (declare (ignore delimiter-p sequence-p))
      (check-bridge-test (null kind))
      (check-bridge-test
       (av1-frame-semantics-show-existing-frame-p semantics))
      (check-bridge-test
       (= (av1-frame-semantics-frame-to-show-map-index semantics)
          0)))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (av1-access-unit-random-access-kind
         (concatenate-octets visible-frame sequence-bytes)
         nil))))
    (check-bridge-test
     (bridge-error-message-contains-p
      (lambda ()
        (av1-access-unit-random-access-kind
         (concatenate-octets visible-frame visible-frame)
         nil))
      "AV1_MULTIPLE_ACCESS_UNITS_IN_PES"))
    (check-bridge-test
     (bridge-error-message-contains-p
      (lambda ()
        (parse-av1-stream-prefix-metadata
         (concatenate-octets visible-frame visible-frame)
         nil nil))
      "AV1_MULTIPLE_ACCESS_UNITS_IN_PES"))
    (multiple-value-bind
          (configuration reduced-header-p kind found-p semantics
           hdr-cll-p temporal-delimiter-seen-p
           sequence-header-in-access-unit-p)
        (parse-av1-stream-prefix-metadata
         (concatenate-octets
          temporal-delimiter sequence-bytes visible-frame)
         nil nil)
      (declare
       (ignore configuration reduced-header-p semantics hdr-cll-p))
      (check-bridge-test (eq kind :key))
      (check-bridge-test found-p)
      (check-bridge-test temporal-delimiter-seen-p)
      (check-bridge-test sequence-header-in-access-unit-p))
    (let ((processor
            (make-bridge-processor
             (make-instance 'octet-collector-stream)
             :av1 :aac
             :transport-rate-kbps
             +test-transport-rate-kbps+)))
      (commit-av1-temporal-unit-state processor nil t)
      (check-bridge-test
       (bridge-processor-av1-tu-sequence-header-seen-p processor))
      (commit-av1-temporal-unit-state processor nil nil)
      (check-bridge-test
       (bridge-processor-av1-tu-sequence-header-seen-p processor))
      (commit-av1-temporal-unit-state processor t nil)
      (check-bridge-test
       (not
        (bridge-processor-av1-tu-sequence-header-seen-p
         processor))))))

(define-bridge-test av1-frame-semantics-govern-pes-timestamps
  (let ((hidden-semantics
          (read-av1-frame-semantics
           (make-bit-reader (octets #x08))
           nil 6))
        (shown-semantics
          (read-av1-frame-semantics
           (make-bit-reader (octets #x10))
           nil 6))
        (show-existing-semantics
          (read-av1-frame-semantics
           (make-bit-reader (octets #xd0))
           nil 3)))
    (check-bridge-test
     (eq
      (av1-frame-random-access-kind hidden-semantics t)
      :delayed-key))
    (check-bridge-test
     (null
      (av1-frame-random-access-kind hidden-semantics nil)))
    (check-bridge-test
     (not
      (av1-frame-semantics-show-frame-p hidden-semantics)))
    (check-bridge-test
     (av1-frame-semantics-showable-frame-p hidden-semantics))
    (check-bridge-test
     (bridge-error-message-contains-p
      (lambda ()
        (validate-av1-pes-timestamps
         (parse-pes-header
          (make-pes #xe0 (octets 0) 90000 :dts 87000))
         hidden-semantics))
      "AV1_HIDDEN_FRAME_PTS_DTS_MISMATCH"))
    ;; shown/show_existingはPresentationTimeとRemovalTimeなので、
    ;; 数値としてPTS < DTSでも§3.5違反とは限らない。
    (validate-av1-pes-timestamps
     (parse-pes-header
      (make-pes #xe0 (octets 0) 87000 :dts 90000))
     shown-semantics)
    (check-bridge-test
     (av1-frame-semantics-show-existing-frame-p
      show-existing-semantics))
    (check-bridge-test
     (= (av1-frame-semantics-frame-to-show-map-index
         show-existing-semantics)
        5))
    (multiple-value-bind (pts dts)
        (validate-av1-pes-timestamps
         (parse-pes-header
          (make-pes #xe0 (octets 0) 87000 :dts 90000))
         show-existing-semantics)
      (check-bridge-test (= pts 87000))
      (check-bridge-test (= dts 90000)))
    (multiple-value-bind (pts dts)
        (validate-av1-pes-timestamps
         (parse-pes-header
          (make-pes #xe0 (octets 0) 91000))
         show-existing-semantics)
      (check-bridge-test (= pts 91000))
      (check-bridge-test (= dts 91000)))
    (check-bridge-test
     (bridge-error-message-contains-p
      (lambda ()
        (read-av1-frame-semantics
         (make-bit-reader (octets #x80))
         nil 6))
      "AV1_FRAME_OBU_SHOW_EXISTING_FORBIDDEN"))))

(define-bridge-test av1-hdr-cll-and-rap-interval-policy
  (let* ((full (make-test-av1-non-reduced-access-unit))
         (obus (parse-av1-obus full))
         (sequence (first obus))
         (frame (second obus))
         (with-cll
           (concatenate-octets
            (subseq full
                    (av1-obu-start sequence)
                    (av1-obu-end sequence))
            (make-audit-av1-hdr-cll-obu)
            (subseq full
                    (av1-obu-start frame)
                    (av1-obu-end frame))))
         (with-post-frame-cll
           (concatenate-octets
            full
            (make-audit-av1-hdr-cll-obu))))
    (check-bridge-test
     (av1-access-unit-hdr-cll-p with-cll))
    (check-bridge-test
     (av1-access-unit-hdr-cll-p with-post-frame-cll))
    (let ((malformed
            (copy-seq (make-audit-av1-hdr-cll-obu))))
      (setf (aref malformed (- (length malformed) 1)) 0)
      (check-bridge-test
       (bridge-error-message-contains-p
        (lambda ()
          (av1-access-unit-hdr-cll-p malformed))
        "AV1_HDR_CLL_TRAILING_BITS_INVALID"))))
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :av1 :aac
           :transport-rate-kbps
           +test-transport-rate-kbps+)))
    (record-av1-access-unit-conformance
     processor :key nil 0)
    (check-bridge-test
     (bridge-error-message-contains-p
      (lambda ()
        (record-av1-access-unit-conformance
         processor nil t 3600))
      "AV1_CLL_MISSING_AT_PRIOR_RAP")))
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :av1 :aac
           :transport-rate-kbps
           +test-transport-rate-kbps+)))
    (record-av1-access-unit-conformance
     processor nil t 0)
    (check-bridge-test
     (bridge-error-message-contains-p
      (lambda ()
        (record-av1-access-unit-conformance
         processor :key nil 90000))
      "AV1_CLL_MISSING_AT_RAP")))
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :av1 :aac
           :transport-rate-kbps
           +test-transport-rate-kbps+)))
    (record-av1-access-unit-conformance
     processor :key t 0)
    (record-av1-access-unit-conformance
     processor :key t +maximum-av1-rap-gap-ticks+)
    (check-bridge-test
     (bridge-error-message-contains-p
      (lambda ()
        (record-av1-access-unit-conformance
         processor :key t
         (+ (* 2 +maximum-av1-rap-gap-ticks+) 1)))
      "AV1_RAP_INTERVAL_EXCEEDED")))
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :av1 :aac
           :transport-rate-kbps
           +test-transport-rate-kbps+)))
    (record-av1-access-unit-conformance
     processor nil nil 0)
    (record-av1-access-unit-conformance
     processor nil nil +maximum-av1-rap-gap-ticks+)
    (check-bridge-test
     (bridge-error-message-contains-p
      (lambda ()
        (record-av1-access-unit-conformance
         processor nil nil
         (+ +maximum-av1-rap-gap-ticks+ 1)))
      "AV1_RAP_INTERVAL_EXCEEDED"))))

(define-bridge-test av1-level-and-operating-point-validation
  (let ((reserved (make-test-av1-access-unit 24)))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (parse-av1-sequence-header reserved)))))
  (let ((bits
          (make-array 64
                      :element-type 'bit
                      :adjustable t
                      :fill-pointer 0)))
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 1 5)
    (append-integer-bits bits 0 12)
    (append-integer-bits bits 8 5)
    (append-integer-bits bits 1 1)
    (append-integer-bits bits 0 12)
    (append-integer-bits bits 9 5)
    (append-integer-bits bits 1 1)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (parse-av1-operating-points
         (make-bit-reader (bit-vector-to-octets bits))
         nil))))))

(define-bridge-test av1-signaling-time-uses-wrap-and-ordered-source
  (let* ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :av1 :aac
           :transport-rate-kbps
           +test-transport-rate-kbps+))
         (assembler (%make-pes-assembler +test-video-pid+ :video))
         (initial (- +pts-modulus+ 1800))
         (too-soon-first
           (mod (+ initial 89999) +pts-modulus+))
         (first-change
           (mod (+ initial 90000) +pts-modulus+))
         (too-soon-second
           (mod (+ first-change 89999) +pts-modulus+))
         (boundary
           (mod (+ first-change 90000) +pts-modulus+)))
    (multiple-value-bind (time ordered)
        (record-av1-ordering-time
         processor assembler initial t)
      (record-av1-signaling-configuration
       processor (make-audit-av1-configuration 8)
       time ordered))
    ;; 初期Aも変更時刻として記録し、最初の実変更Bにも1秒制約を課す。
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (multiple-value-bind (time ordered)
            (record-av1-ordering-time
             processor assembler too-soon-first t)
          (record-av1-signaling-configuration
           processor (make-audit-av1-configuration 9)
           time ordered)))))
    (multiple-value-bind (time ordered)
        (record-av1-ordering-time
         processor assembler first-change t)
      (record-av1-signaling-configuration
       processor (make-audit-av1-configuration 9)
       time ordered))
    ;; 次の変更Cは1秒直前なら拒否し、ちょうど1秒なら許可する。
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (multiple-value-bind (time ordered)
            (record-av1-ordering-time
             processor assembler too-soon-second t)
          (record-av1-signaling-configuration
           processor (make-audit-av1-configuration 10)
           time ordered)))))
    (multiple-value-bind (time ordered)
        (record-av1-ordering-time
         processor assembler boundary t)
      (record-av1-signaling-configuration
       processor (make-audit-av1-configuration 10)
       time ordered))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (record-av1-ordering-time
         processor assembler
         (mod (- boundary 1) +pts-modulus+)
         t)))))
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :av1 :aac
           :transport-rate-kbps
           +test-transport-rate-kbps+))
        (assembler (%make-pes-assembler +test-video-pid+ :video)))
    (record-av1-ordering-time
     processor assembler 90000 nil)
    (record-av1-ordering-time
     processor assembler 87000 nil)))

(define-bridge-test inspector-accepts-split-pes-optional-header
  (let* ((pes
           (make-pes
            #xe0 (make-pattern-octets 40 9)
            90000 :dts 87000))
         (first
           (make-ts-packet
            #x150 0 (subseq pes 0 7)
            :payload-unit-start t))
         (second
           (make-ts-packet
            #x150 1 (subseq pes 7)))
         (inspection
           (inspect-ts-octets
            (packet-list-to-octets
             (list first second))))
         (state
           (gethash #x150 (ts-inspection-pids inspection))))
    (check-bridge-test
     (equal (pid-inspection-pts-values state)
            '(90000)))
    (check-bridge-test
     (equal (pid-inspection-dts-values state)
            '(87000)))))

(define-bridge-test pcr-interval-and-discontinuity-policy
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :passthrough :aac)))
    (install-selected-pcr-pid processor +test-video-pid+)
    (process-bridge-packet
     processor (make-audit-pcr-packet 27000000))
    (process-bridge-packet
     processor
     (make-audit-pcr-packet
      (+ 27000000 +maximum-pcr-interval-ticks+)
      :counter 1))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (process-bridge-packet
         processor
         (make-audit-pcr-packet
          (+ 27000000
             (* 2 +maximum-pcr-interval-ticks+)
             1)
          :counter 2)))))
    (process-bridge-packet
     processor
     (make-audit-pcr-packet
      (+ 27000000
         (* 2 +maximum-pcr-interval-ticks+)
         1)
      :counter 2
      :discontinuity t)))
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :passthrough :aac)))
    (install-selected-pcr-pid processor +test-video-pid+)
    (process-bridge-packet
     processor (make-audit-pcr-packet 27000000))
    (process-bridge-packet
     processor
     (make-test-adaptation-only-packet
      +test-video-pid+ 0 :discontinuity t))
    (process-bridge-packet
     processor
     (make-audit-pcr-packet
      (+ 27000000
         (* 10 +maximum-pcr-interval-ticks+))
     :counter 1))))

(define-bridge-test selected-pcr-duplicate-is-semantic-no-op
  (let ((processor
           (make-bridge-processor
            (make-instance 'octet-collector-stream)
            :passthrough :aac))
         (assembler
           (%make-pes-assembler +test-video-pid+ :video))
         (first
           (make-test-program-pcr-packet
            90000 :discontinuity t)))
    (setf
     (gethash +test-video-pid+
              (bridge-processor-pes-assemblers processor))
     assembler)
    (install-selected-pcr-pid processor +test-video-pid+)
    (record-selected-pcr-packet
     processor first +test-video-pid+ (ts-pcr first))
    (validate-program-timestamp-against-pcr
     processor assembler 90000 t)
    (record-selected-pcr-packet
     processor
     (make-test-adaptation-only-packet
      +test-data-pid+ 0)
     +test-data-pid+ nil)
    (check-bridge-test
     (eq
      (record-selected-pcr-packet
       processor (copy-seq first)
       +test-video-pid+ (ts-pcr first))
      :duplicate))
    (check-bridge-test
     (bridge-error-message-contains-p
      (lambda ()
        (validate-program-timestamp-against-pcr
         processor assembler 99001 t))
      "SELECTED_PCR_GAP"))))

(define-bridge-test selected-pcr-rejects-nonduplicate-stopped-clock
  (let ((processor
           (make-bridge-processor
            (make-instance 'octet-collector-stream)
            :passthrough :aac))
         (first
           (make-test-program-pcr-packet 90000 :counter 0))
         (second
           (make-test-program-pcr-packet 90000 :counter 1)))
    (install-selected-pcr-pid processor +test-video-pid+)
    (record-selected-pcr-packet
     processor first +test-video-pid+ (ts-pcr first))
    (check-bridge-test
     (bridge-error-message-contains-p
      (lambda ()
        (record-selected-pcr-packet
         processor second +test-video-pid+ (ts-pcr second)))
      "SELECTED_PCR_INTERVAL_INVALID"))))

(define-bridge-test malformed-pcr-on-unselected-pid-is-byte-exact
  (let* ((output
           (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor
            output :passthrough :aac))
         (wrong-pid-packet
           (make-test-adaptation-only-packet
            +test-data-pid+ 0)))
    (set-test-pcr wrong-pid-packet 27000000)
    ;; PCR reserved bitsを壊す。adaptation field自体の境界は有効なまま。
    (setf (aref wrong-pid-packet 10)
          (logand (aref wrong-pid-packet 10) #x81))
    (dolist (packet
             (append
              (make-test-pat-packets)
              (make-test-pmt-packets :two-audio-p nil)
              (list
               (make-test-program-pcr-packet 90000)
               wrong-pid-packet)))
      (process-bridge-packet processor packet))
    (finish-bridge-processor processor)
    (check-bridge-test
     (find wrong-pid-packet
           (octets-to-packet-list
            (collected-octets output))
           :test #'equalp))))

(define-bridge-test dedicated-pcr-pid-is-independent-from-video-pid
  (let ((processor
           (make-bridge-processor
            (make-instance 'octet-collector-stream)
            :vp9 :aac))
         (table
           (make-test-pmt-table :two-audio-p nil)))
    (setf
     (program-map-table-pcr-pid table)
     +test-data-pid+)
    (dolist (packet (make-test-pat-packets))
      (process-bridge-packet processor packet))
    (dolist
        (packet
         (make-section-ts-packets
          +test-pmt-pid+
          (build-pmt-section table)
          4))
      (process-bridge-packet processor packet))
    (check-bridge-test
     (= (bridge-processor-current-video-pid processor)
        +test-video-pid+))
    (check-bridge-test
     (= (bridge-processor-current-pcr-pid processor)
        +test-data-pid+))))

(define-bridge-test dedicated-pcr-pid-rejects-transport-error
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :vp9 :aac))
        (table
          (make-test-pmt-table :two-audio-p nil))
        (packet
          (make-test-adaptation-only-packet
           +test-data-pid+ 0)))
    (setf
     (program-map-table-pcr-pid table)
     +test-data-pid+)
    (set-test-pcr packet 27000000)
    (setf (aref packet 1)
          (logior (aref packet 1) #x80))
    (dolist (source (make-test-pat-packets))
      (process-bridge-packet processor source))
    (dolist
        (source
         (make-section-ts-packets
          +test-pmt-pid+
          (build-pmt-section table)
          4))
      (process-bridge-packet processor source))
    (check-bridge-test
     (bridge-error-message-contains-p
      (lambda ()
        (process-bridge-packet processor packet))
      "SELECTED_PCR_TRANSPORT_ERROR"))))

(define-bridge-test selected-pcr-is-required-and-wrong-pid-is-ignored
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :passthrough :aac)))
    (dolist (packet
             (append
              (make-test-pat-packets)
              (make-test-pmt-packets :two-audio-p nil)))
      (process-bridge-packet processor packet))
    (let ((wrong-first
            (make-test-adaptation-only-packet
             +test-data-pid+ 0))
          (wrong-second
            (make-test-adaptation-only-packet
             +test-data-pid+ 0)))
      (set-test-pcr wrong-first 0)
      (set-test-pcr
       wrong-second
       (+ +maximum-pcr-interval-ticks+ 1))
      (process-bridge-packet processor wrong-first)
      (process-bridge-packet processor wrong-second))
    (check-bridge-test
     (not
      (bridge-processor-selected-pcr-ever-seen-p processor)))
    (check-bridge-test
     (bridge-error-message-contains-p
      (lambda () (finish-bridge-processor processor))
      "SELECTED_PCR_MISSING"))))

(define-bridge-test selected-pcr-gap-and-dts-delay-fail-closed
  (let* ((processor
           (make-bridge-processor
            (make-instance 'octet-collector-stream)
            :vp9 :aac))
         (first-pes
           (make-pes
            #xbd (make-test-vp9-key-frame 0 320 180)
            90000 :dts 90000))
         (first
           (packetize-test-video-with-pcr
            first-pes :continuity-counter 0))
         (second
           (packetize-payload
            +test-video-pid+
            (make-pes
             #xbd (make-test-vp9-key-frame 0 320 180)
             99001 :dts 99001)
            :continuity-counter
            (logand (length first) #x0f)
            :payload-unit-start t)))
    (dolist (packet
             (append
              (make-test-pat-packets)
              (make-test-pmt-packets :two-audio-p nil)
              first))
      (process-bridge-packet processor packet))
    (check-bridge-test
     (bridge-error-message-contains-p
      (lambda ()
        (dolist (packet second)
          (process-bridge-packet processor packet)))
      "SELECTED_PCR_GAP")))
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :passthrough :aac)))
    (install-selected-pcr-pid processor +test-video-pid+)
    (process-bridge-packet
     processor (make-test-program-pcr-packet 0))
    (check-bridge-test
     (bridge-error-message-contains-p
      (lambda ()
        (validate-program-timestamp-against-pcr
         processor
         (%make-pes-assembler +test-video-pid+ :video)
         (+ +maximum-dts-pcr-delay-ticks+ 1)
         t))
      "DTS_PCR_DELAY_EXCEEDED"))))

(define-bridge-test opus-clock-detects-pcr-gap-while-video-stalls
  (let* ((processor
           (make-bridge-processor
            (make-instance 'octet-collector-stream)
            :vp9 :opus))
         (video
           (packetize-test-video-with-pcr
            (make-pes
             #xbd (make-test-vp9-key-frame 0 320 180)
             90000 :dts 90000)
            :continuity-counter 0))
         (first-opus
           (packetize-payload
            +test-audio-one-pid+
            (make-test-opus-pes
             90000 (octets #xf8))
            :continuity-counter 0
            :payload-unit-start t))
         (late-opus
           (loop
             for pts from 91800 to 100800 by 1800
             for counter from (length first-opus)
             append
             (packetize-payload
              +test-audio-one-pid+
              (make-test-opus-pes
               pts (octets #xf8))
              :continuity-counter (logand counter #x0f)
              :payload-unit-start t))))
    (dolist (packet
             (append
              (make-test-pat-packets)
              (make-test-pmt-packets :two-audio-p nil)
              video
              first-opus))
      (process-bridge-packet processor packet))
    (check-bridge-test
     (bridge-error-message-contains-p
      (lambda ()
        (dolist (packet late-opus)
          (process-bridge-packet processor packet)))
      "SELECTED_PCR_GAP"))))

(define-bridge-test program-ordering-is-isolated-per-elementary-stream
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :passthrough :aac))
        (video
          (%make-pes-assembler +test-video-pid+ :video))
        (opus
          (%make-pes-assembler +test-audio-one-pid+ :opus)))
    (validate-program-timestamp-against-pcr
     processor video 90000 t)
    (validate-program-timestamp-against-pcr
     processor opus 99000 nil)
    (validate-program-timestamp-against-pcr
     processor video 93600 t)))

(define-bridge-test selected-pcr-discontinuity-and-pid-change-rebase
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :passthrough :aac)))
    (dolist (packet
             (append
              (make-test-pat-packets)
              (make-test-pmt-packets :two-audio-p nil)))
      (process-bridge-packet processor packet))
    (process-bridge-packet
     processor (make-test-program-pcr-packet 0))
    (process-bridge-packet
     processor
     (make-test-adaptation-only-packet
      +test-video-pid+ 0 :discontinuity t))
    (check-bridge-test
     (bridge-error-message-contains-p
      (lambda () (finish-bridge-processor processor))
      "SELECTED_PCR_REBASE_REQUIRED")))
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :passthrough :aac)))
    (install-selected-pcr-pid processor +test-video-pid+)
    (record-selected-pcr-packet
     processor
     (make-test-program-pcr-packet 0)
     +test-video-pid+ 0)
    (install-selected-pcr-pid processor +test-data-pid+)
    (check-bridge-test
     (not
      (bridge-processor-selected-pcr-ever-seen-p processor)))
    (record-selected-pcr-packet
     processor
     (make-test-program-pcr-packet 90000)
     +test-video-pid+ 27000000)
    (check-bridge-test
     (not
      (bridge-processor-selected-pcr-ever-seen-p processor)))))

(define-bridge-test selected-pcr-window-restarts-after-each-pcr
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :passthrough :aac))
        (assembler
          (%make-pes-assembler +test-video-pid+ :video)))
    (setf
     (gethash +test-video-pid+
              (bridge-processor-pes-assemblers processor))
     assembler)
    (install-selected-pcr-pid processor +test-video-pid+)
    (validate-program-timestamp-against-pcr
     processor assembler 900000 t)
    (process-bridge-packet
     processor
     (make-test-program-pcr-packet 904500))
    (validate-program-timestamp-against-pcr
     processor assembler 905400 t)
    (validate-program-timestamp-against-pcr
     processor assembler 910800 t)))

(define-bridge-test repacketization-preserves-selected-pcr-bytes
  (let* ((pes
           (make-pes
            #xbd (make-test-vp9-key-frame 0 640 360)
            90000 :dts 87000))
         (video
           (packetize-test-video-with-pcr pes))
         (input-first (first video))
         (output
           (run-packet-processor
            (append
             (make-test-pat-packets)
             (make-test-pmt-packets :two-audio-p nil)
             video)
            :vp9 :aac))
         (output-first
           (find-if
            (lambda (packet)
              (and
               (= (ts-pid packet) +test-video-pid+)
               (ts-payload-unit-start-p packet)))
            output)))
    (check-bridge-test output-first)
    (check-bridge-test
     (= (ts-pcr input-first) (ts-pcr output-first)))
    (check-bridge-test
     (equalp
      (subseq input-first 6 12)
      (subseq output-first 6 12)))
    (check-bridge-test
     (eql
      (ts-discontinuity-indicator-p input-first)
      (ts-discontinuity-indicator-p output-first)))
    (check-bridge-test
     (eql
      (ts-transport-priority-p input-first)
      (ts-transport-priority-p output-first)))))

(define-bridge-test input-pmt-version-transition-is-strict
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :passthrough :aac))
        (first (make-test-pmt-table :version 31))
        (same (make-test-pmt-table :version 31))
        (changed (make-test-pmt-table :version 0
                                      :two-audio-p nil)))
    (validate-input-pmt-transition processor first)
    (validate-input-pmt-transition processor same)
    (validate-input-pmt-transition processor changed))
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :passthrough :aac))
        (first (make-test-pmt-table :version 7))
        (wrong (make-test-pmt-table :version 9
                                    :two-audio-p nil)))
    (validate-input-pmt-transition processor first)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-input-pmt-transition processor wrong)))))
  (let ((processor
          (make-bridge-processor
           (make-instance 'octet-collector-stream)
           :passthrough :aac))
        (first (make-test-pmt-table :version 7))
        (version-only (make-test-pmt-table :version 8)))
    (validate-input-pmt-transition processor first)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-input-pmt-transition processor version-only))))))

(define-bridge-test input-pat-version-transition-is-strict
  (flet ((table (version program-number pmt-pid)
           (make-program-association-table
            :transport-stream-id 1
            :version version
            :programs
            (list
             (make-pat-program
              :program-number program-number
              :pid pmt-pid)))))
    (let ((processor
            (make-bridge-processor
             (make-instance 'octet-collector-stream)
             :passthrough :aac)))
      (validate-input-pat-transition
       processor (table 31 1 +test-pmt-pid+))
      (validate-input-pat-transition
       processor (table 31 1 +test-pmt-pid+))
      (validate-input-pat-transition
       processor (table 0 2 (+ +test-pmt-pid+ 1))))
    (let ((processor
            (make-bridge-processor
             (make-instance 'octet-collector-stream)
             :passthrough :aac)))
      (validate-input-pat-transition
       processor (table 7 1 +test-pmt-pid+))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-input-pat-transition
           processor (table 9 2 +test-pmt-pid+))))))
    (let ((processor
            (make-bridge-processor
             (make-instance 'octet-collector-stream)
             :passthrough :aac)))
      (validate-input-pat-transition
       processor (table 7 1 +test-pmt-pid+))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-input-pat-transition
           processor (table 8 1 +test-pmt-pid+))))))))

(define-bridge-test pat-discontinuity-rebases-version-transition
  (flet ((packets (version counter &key discontinuity)
           (let ((result
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
                        :pid +test-pmt-pid+))))
                    counter)))
             (when discontinuity
               (setf (aref (first result) 5)
                     (logior (aref (first result) 5) #x80)))
             result)))
    (let ((processor
            (make-bridge-processor
             (make-instance 'octet-collector-stream)
             :passthrough :aac)))
      (dolist (packet (packets 7 0))
        (process-bridge-packet processor packet))
      (process-bridge-packet
       processor
       (make-test-adaptation-only-packet
        0 0 :discontinuity t))
      (dolist (packet (packets 19 11))
        (process-bridge-packet processor packet))
      (check-bridge-test
       (= (bridge-processor-last-input-pat-version processor) 19)))
    (let ((processor
             (make-bridge-processor
              (make-instance 'octet-collector-stream)
              :passthrough :aac))
           (first (first (packets 7 4 :discontinuity t))))
      (process-bridge-packet processor first)
      (process-bridge-packet processor (copy-seq first))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (dolist (packet (packets 8 5))
            (process-bridge-packet processor packet))))))))

(define-bridge-test pmt-discontinuity-rebases-input-version-only
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor output :passthrough :aac)))
    (dolist (packet (make-test-pat-packets))
      (process-bridge-packet processor packet))
    (dolist (packet (make-test-pmt-packets :version 7))
      (process-bridge-packet processor packet))
    (process-bridge-packet
     processor
     (make-test-adaptation-only-packet
      +test-pmt-pid+ 4 :discontinuity t))
    (dolist (packet
             (make-section-ts-packets
              +test-pmt-pid+
              (build-pmt-section
               (make-test-pmt-table :version 19))
              12))
      (process-bridge-packet processor packet))
    (process-bridge-packet
     processor (make-test-program-pcr-packet 0))
    (finish-bridge-processor processor)
    (check-bridge-test
     (= (bridge-processor-last-input-pmt-version processor) 19))
    (let* ((packets
             (octets-to-packet-list
              (collected-octets output)))
           (sections
             (sections-on-pid packets +test-pmt-pid+)))
      (check-bridge-test (= (length sections) 2))
      (check-bridge-test
       (equal
        (mapcar
         (lambda (section)
           (program-map-table-version
            (parse-pmt-section section)))
         sections)
        '(7 7))))))

(define-bridge-test pmt-payload-discontinuity-keeps-output-continuity
  (let* ((output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor output :passthrough :aac))
         (rebased
           (make-section-ts-packets
            +test-pmt-pid+
            (build-pmt-section
             (make-test-pmt-table :version 19))
            12)))
    (dolist (packet (make-test-pat-packets))
      (process-bridge-packet processor packet))
    (dolist (packet (make-test-pmt-packets :version 7))
      (process-bridge-packet processor packet))
    (setf (aref (first rebased) 5)
          (logior (aref (first rebased) 5) #x80))
    (dolist (packet rebased)
      (process-bridge-packet processor packet))
    (process-bridge-packet
     processor (make-test-program-pcr-packet 0))
    (finish-bridge-processor processor)
    (let* ((packets
             (octets-to-packet-list
              (collected-octets output)))
           (pmt-packets
             (remove-if-not
              (lambda (packet)
                (= (ts-pid packet) +test-pmt-pid+))
              packets))
           (sections
             (sections-on-pid packets +test-pmt-pid+)))
      (check-bridge-test (= (length pmt-packets) 2))
      (check-bridge-test
       (equal
        (mapcar #'ts-continuity-counter pmt-packets)
        '(4 5)))
      (check-bridge-test
       (equal
        (mapcar
         (lambda (section)
           (program-map-table-version
            (parse-pmt-section section)))
         sections)
        '(7 7)))
      (inspect-ts-octets (collected-octets output)))))

(define-bridge-test pat-program-change-on-same-pmt-pid-is-applied
  (flet ((section (version program-number)
           (build-pat-section
            (make-program-association-table
             :transport-stream-id 1
             :version version
             :programs
             (list
              (make-pat-program
               :program-number program-number
               :pid +test-pmt-pid+))))))
    (let ((processor
            (make-bridge-processor
             (make-instance 'octet-collector-stream)
             :passthrough :aac)))
      (parse-current-pat processor (section 3 1))
      (setf
       (bridge-processor-current-video-pid processor)
       +test-video-pid+
       (bridge-processor-current-opus-pids processor)
       (list +test-audio-one-pid+)
       (bridge-processor-latest-pmt-table processor)
       (make-test-pmt-table :two-audio-p nil)
       (bridge-processor-latest-pmt-template processor)
       (make-ts-packet +test-pmt-pid+ 4 (octets 1))
       (bridge-processor-seen-pmt-p processor)
       t)
      (setf
       (gethash
        +test-video-pid+
        (bridge-processor-pes-assemblers processor))
       (%make-pes-assembler +test-video-pid+ :video)
       (gethash
        +test-audio-one-pid+
        (bridge-processor-pes-assemblers processor))
       (%make-pes-assembler +test-audio-one-pid+ :opus))
      (parse-current-pat processor (section 4 2))
      (check-bridge-test
       (= (bridge-processor-pat-program-number processor) 2))
      (check-bridge-test
       (null (bridge-processor-current-video-pid processor)))
      (check-bridge-test
       (null (bridge-processor-current-opus-pids processor)))
      (check-bridge-test
       (null (bridge-processor-latest-pmt-table processor)))
      (check-bridge-test
       (null (bridge-processor-latest-pmt-template processor)))
      (check-bridge-test
       (not (bridge-processor-seen-pmt-p processor)))
      (check-bridge-test
       (null
        (gethash
         +test-video-pid+
         (bridge-processor-pes-assemblers processor))))
      (check-bridge-test
       (null
        (current-pes-assembler-for-packet
         processor
         (make-ts-packet
          +test-video-pid+ 0 (octets 1)
          :payload-unit-start t))))
      (let ((pmt (make-test-pmt-table :version 19)))
        (setf (program-map-table-program-number pmt) 2)
        (validate-current-pmt-table processor pmt)
        (validate-input-pmt-transition processor pmt)))))

(define-bridge-test pat-program-change-rejects-active-old-target
  (flet ((section (version program-number)
           (build-pat-section
            (make-program-association-table
             :transport-stream-id 1
             :version version
             :programs
             (list
              (make-pat-program
               :program-number program-number
               :pid +test-pmt-pid+))))))
    (let ((processor
             (make-bridge-processor
              (make-instance 'octet-collector-stream)
              :vp9 :aac))
           (assembler
             (%make-pes-assembler +test-video-pid+ :video)))
      (parse-current-pat processor (section 3 1))
      (setf
       (bridge-processor-current-video-pid processor)
       +test-video-pid+
       (pes-assembler-active-p assembler)
       t
       (gethash
        +test-video-pid+
        (bridge-processor-pes-assemblers processor))
       assembler)
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (parse-current-pat processor (section 4 2))))))))

(define-bridge-test pmt-discontinuity-cannot-reorder-incomplete-pmt
  (let* ((table (make-test-pmt-table :two-audio-p nil))
         (output (make-instance 'octet-collector-stream))
         (processor
           (make-bridge-processor output :vp9 :aac)))
    (setf (program-map-table-program-descriptors table)
          (list
           (make-descriptor
            :tag #x90
            :payload (make-pattern-octets 220 5))))
    (let ((pmt-packets
            (make-section-ts-packets
             +test-pmt-pid+
             (build-pmt-section table)
             0)))
      (check-bridge-test (> (length pmt-packets) 1))
      (dolist (packet (make-test-pat-packets))
        (process-bridge-packet processor packet))
      (process-bridge-packet processor (first pmt-packets))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (process-bridge-packet
           processor
           (make-test-adaptation-only-packet
            +test-pmt-pid+ 0 :discontinuity t))))))))

(define-bridge-test pmt-discontinuity-duplicate-precedes-incomplete-check
  (let ((table (make-test-pmt-table :two-audio-p nil)))
    (setf (program-map-table-program-descriptors table)
          (list
           (make-descriptor
            :tag #x90
            :payload (make-pattern-octets 220 13))))
    (let* ((section (build-pmt-section table))
           (payload
             (concatenate-octets (octets 0) section))
           (packets
             (packetize-payload
              +test-pmt-pid+ payload
              :continuity-counter 4
              :payload-unit-start t
              :random-access t)))
      (check-bridge-test (> (length packets) 1))
      (setf (aref (first packets) 5)
            (logior (aref (first packets) 5) #x80))
      (run-packet-processor
       (append
        (make-test-pat-packets)
        (list (first packets)
              (copy-seq (first packets)))
        (rest packets)
        (list
         (make-test-program-pcr-packet 0)))
       :passthrough :aac))))
