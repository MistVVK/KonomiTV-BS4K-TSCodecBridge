;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defun make-test-pat (&optional (program-count 1))
  "PROGRAM-COUNT件を持つtest用PATを作る。"
  (make-program-association-table
   :transport-stream-id 1
   :version 3
   :programs
   (loop for index from 1 to program-count
         collect
         (make-pat-program
          :program-number index
          :pid (+ #x100 index)))))

(defun make-test-adaptation-only-packet
    (pid continuity-counter &key discontinuity)
  "test用のadaptation-only TS packetを作る。"
  (let ((packet
          (make-array +ts-packet-size+
                      :element-type 'octet
                      :initial-element #xff)))
    (setf (aref packet 0) +ts-sync-byte+
          (aref packet 1) (ldb (byte 5 8) pid)
          (aref packet 2) (ldb (byte 8 0) pid)
          (aref packet 3) (logior #x20 continuity-counter)
          (aref packet 4) 183
          (aref packet 5) (if discontinuity #x80 0))
    packet))

(defun make-test-payload-only-packet
    (pid continuity-counter payload &key payload-unit-start)
  "184 byte PAYLOADを持つtest用payload-only TS packetを作る。"
  (unless (= (length payload) 184)
    (bridge-error "Test payload-only packet requires exactly 184 bytes"))
  (let ((packet
          (make-array +ts-packet-size+
                      :element-type 'octet)))
    (setf (aref packet 0) +ts-sync-byte+
          (aref packet 1)
          (logior (if payload-unit-start #x40 0)
                  (ldb (byte 5 8) pid))
          (aref packet 2) (ldb (byte 8 0) pid)
          (aref packet 3) (logior #x10 continuity-counter))
    (replace packet payload :start1 4)
    packet))

(define-bridge-test transport-packet-roundtrip
  (let* ((payload (octets 1 2 3 4))
         (packet
           (make-ts-packet #x101 7 payload
                           :payload-unit-start t
                           :random-access t
                           :transport-priority t)))
    (validate-ts-packet packet)
    (check-bridge-test (= (ts-pid packet) #x101))
    (check-bridge-test (= (ts-continuity-counter packet) 7))
    (check-bridge-test (ts-payload-unit-start-p packet))
    (check-bridge-test (ts-transport-priority-p packet))
    (check-bridge-test (ts-random-access-indicator-p packet))
    (check-bridge-test
     (equalp (subseq packet (ts-payload-offset packet)) payload))))

(define-bridge-test pat-build-parse-roundtrip
  (let* ((table (make-test-pat 3))
         (section (build-pat-section table))
         (parsed (parse-pat-section section)))
    (check-bridge-test (valid-crc32-mpeg2-p section))
    (check-bridge-test
     (= (program-association-table-version parsed) 3))
    (check-bridge-test
     (= (length (program-association-table-programs parsed)) 3))
    (check-bridge-test
     (= (pat-program-pid
         (first (program-association-table-programs parsed)))
        #x101))))

(define-bridge-test split-pat-section-assembly
  (let* ((section (build-pat-section (make-test-pat 60)))
         (payload (make-array (+ (length section) 1)
                              :element-type 'octet
                              :initial-element 0))
         (assembler (make-section-assembler 0))
         (completed '()))
    (replace payload section :start1 1)
    (dolist (packet
             (packetize-payload 0 payload
                                :payload-unit-start t))
      (setf completed
            (nconc completed
                   (feed-section-packet assembler packet))))
    (check-bridge-test (= (length completed) 1))
    (check-bridge-test (equalp (first completed) section))))

(define-bridge-test split-psi-payload-stuffing-assembly
  (let* ((table
           (make-program-map-table
            :program-number 1
            :version 1
            :pcr-pid #x101
            :program-descriptors
            (list
             (make-descriptor
              :tag #x90
              :payload
              (make-array 200
                          :element-type 'octet
                          :initial-element #x55)))
            :streams
            (list
             (make-pmt-stream
              :stream-type #x06
              :elementary-pid #x101))))
         (section (build-pmt-section table))
         (payload-length
           (* 184 (ceiling (+ 1 (length section)) 184)))
         (payload
           (make-array payload-length
                       :element-type 'octet
                       :initial-element #xff))
         (packets '())
         (assembler (make-section-assembler #x100))
         (completed '()))
    (setf (aref payload 0) 0)
    (replace payload section :start1 1)
    (loop for offset from 0 below payload-length by 184
          for counter from 3
          do (push
              (make-test-payload-only-packet
               #x100 (logand counter #x0f)
               (subseq payload offset (+ offset 184))
               :payload-unit-start (zerop offset))
              packets))
    (setf packets (nreverse packets))
    (check-bridge-test (> (length packets) 1))
    (dolist (packet packets)
      (setf completed
            (nconc completed
                   (feed-section-packet assembler packet))))
    (check-bridge-test (= (length completed) 1))
    (check-bridge-test (equalp (first completed) section))
    (let ((corrupt (mapcar #'copy-seq packets))
          (corrupt-assembler (make-section-assembler #x100)))
      (setf (aref (car (last corrupt)) 187) 0)
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (dolist (packet corrupt)
            (feed-section-packet corrupt-assembler packet))))))))

(define-bridge-test psi-adaptation-discontinuity-rebases-continuity
  (let* ((section (build-pat-section (make-test-pat)))
         (payload
           (concatenate-octets (octets 0) section))
         (first
           (first
            (packetize-payload
             0 payload
             :continuity-counter 0
             :payload-unit-start t)))
         (rebased
           (first
            (packetize-payload
             0 payload
             :continuity-counter 9
             :payload-unit-start t)))
         (following
           (first
            (packetize-payload
             0 payload
             :continuity-counter 10
             :payload-unit-start t)))
         (bad-following
           (first
            (packetize-payload
             0 payload
             :continuity-counter 12
             :payload-unit-start t)))
         (discontinuity
           (make-test-adaptation-only-packet
            0 0 :discontinuity t))
         (assembler (make-section-assembler 0)))
    (feed-section-packet assembler first)
    (feed-section-packet assembler discontinuity)
    (check-bridge-test
     (= (length
         (feed-section-packet assembler rebased))
        1))
    (check-bridge-test
     (= (length
         (feed-section-packet assembler following))
        1))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (feed-section-packet assembler bad-following))))
    (let ((strict (make-section-assembler 0)))
      (feed-section-packet strict first)
      (feed-section-packet
       strict
       (make-test-adaptation-only-packet 0 0))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (feed-section-packet strict rebased)))))))

(define-bridge-test psi-discontinuity-duplicate-is-dropped-before-reset
  (let* ((section (build-pat-section (make-test-pat)))
         (packet
           (first
            (packetize-payload
             0
             (concatenate-octets (octets 0) section)
             :continuity-counter 7
             :payload-unit-start t
             :random-access t)))
         (assembler (make-section-assembler 0)))
    (setf (aref packet 5)
          (logior (aref packet 5) #x80))
    (check-bridge-test
     (= (length (feed-section-packet assembler packet)) 1))
    (check-bridge-test
     (null
      (feed-section-packet assembler (copy-seq packet))))))

(define-bridge-test inspector-discontinuity-duplicate-counts-pts-once
  (let* ((pes
           (make-pes #xe0 (octets 1 2 3) 90000))
         (packet
           (make-ts-packet
            #x151 4 pes :payload-unit-start t))
         (state nil))
    (setf (aref packet 5)
          (logior (aref packet 5) #x80))
    (setf state
          (gethash
           #x151
           (ts-inspection-pids
            (inspect-ts-octets
             (concatenate-octets packet packet)))))
    (check-bridge-test
     (= (pid-inspection-packet-count state) 2))
    (check-bridge-test
     (equal (pid-inspection-pts-values state)
            '(90000)))))

(define-bridge-test pes-adaptation-discontinuity-rebases-continuity
  (let ((first (make-ts-packet #x101 0 (octets 1)))
        (rebased (make-ts-packet #x101 9 (octets 2)))
        (following (make-ts-packet #x101 10 (octets 3)))
        (bad-following (make-ts-packet #x101 12 (octets 4)))
        (discontinuity
          (make-test-adaptation-only-packet
           #x101 0 :discontinuity t))
        (assembler (%make-pes-assembler #x101 :video)))
    (validate-pes-transport-continuity assembler first)
    (validate-pes-transport-continuity assembler discontinuity)
    (check-bridge-test
     (validate-pes-transport-continuity assembler rebased))
    (check-bridge-test
     (validate-pes-transport-continuity assembler following))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-pes-transport-continuity
         assembler bad-following))))
    (let ((strict (%make-pes-assembler #x101 :video)))
      (validate-pes-transport-continuity strict first)
      (validate-pes-transport-continuity
       strict
       (make-test-adaptation-only-packet #x101 0))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-pes-transport-continuity
           strict rebased)))))))

(define-bridge-test pmt-build-parse-roundtrip
  (let* ((video
           (make-pmt-stream
            :stream-type #x06
            :elementary-pid #x101
            :descriptors (make-vp9-mapping-descriptors)))
         (audio
           (make-pmt-stream
            :stream-type #x0f
            :elementary-pid #x102))
         (table
           (make-program-map-table
            :program-number 1
            :version 7
            :pcr-pid #x101
            :streams (list video audio)))
         (section (build-pmt-section table))
         (parsed (parse-pmt-section section)))
    (check-bridge-test (= (program-map-table-version parsed) 7))
    (check-bridge-test (= (program-map-table-pcr-pid parsed) #x101))
    (check-bridge-test (= (length (program-map-table-streams parsed)) 2))
    (validate-vp9-mapping-descriptors
     (pmt-stream-descriptors
      (first (program-map-table-streams parsed))))))

(define-bridge-test pes-build-parse-roundtrip
  (let* ((payload (octets #x82 #x49 #x83 #x42))
         (pes (make-pes #xe0 payload 90000
                        :dts 87000
                        :data-alignment t))
         (header (parse-pes-header pes)))
    (check-bridge-test (= (pes-header-stream-id header) #xe0))
    (check-bridge-test (= (pes-header-pts header) 90000))
    (check-bridge-test (= (pes-header-dts header) 87000))
    (check-bridge-test (pes-header-data-alignment-p header))
    (check-bridge-test
     (equalp (subseq pes (pes-header-payload-offset header))
             payload))
    (let ((scrambled (copy-seq pes)))
      (setf (aref scrambled 6)
            (logior (aref scrambled 6) #x10))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (parse-pes-header scrambled)))))
    (let ((truncated-escr
            (make-pes #xbd payload 90000)))
      (setf (aref truncated-escr 7) #xa0)
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (parse-pes-header truncated-escr)))))
    (let* ((escr (octets #xc4 0 #x04 0 #x04 #x01))
           (payload-offset (pes-header-payload-offset header))
           (extended
             (concatenate-octets
              (subseq pes 0 payload-offset)
              escr
              payload)))
      (setf (aref extended 7)
            (logior (aref extended 7) #x20)
            (aref extended 8)
            (+ (aref extended 8) (length escr)))
      (write-u16-be (+ (pes-header-packet-length header)
                       (length escr))
                    extended 4)
      (let ((extended-header (parse-pes-header extended)))
        (check-bridge-test
         (equalp
          (subseq extended
                  (pes-header-payload-offset extended-header))
          payload)))
      (setf (aref extended (+ payload-offset 2))
            (logand (aref extended (+ payload-offset 2))
                    #xfb))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (parse-pes-header extended)))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-pes-escr-field
         (octets #x04 0 #x04 0 #x04 #x01)
         0 6))))
    (check-bridge-test
     (= (validate-pes-es-rate-field
         (octets #x80 0 #x03) 0 3)
        3))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-pes-es-rate-field
         (octets 0 0 #x03) 0 3))))
    (check-bridge-test
     (= (validate-pes-trick-mode-field
         (octets #x47) 0 1)
        1))
    (check-bridge-test
     (= (validate-pes-trick-mode-field
         (octets 0) 0 1)
        1))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-pes-trick-mode-field
         (octets #x40) 0 1))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-pes-trick-mode-field
         (octets #x20) 0 1))))
    (check-bridge-test
     (= (validate-pes-extension-field
         (octets #x0f #x81 #xff) 0 3 #xbd)
        3))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-pes-extension-field
         (octets #x0f #x01 #xff) 0 3 #xbd))))
    (let ((tref-extension
            (concatenate-octets
             (octets #x0f #x86 #xfe)
             (encode-pes-timestamp 90000 #x0f))))
      (check-bridge-test
       (= (validate-pes-extension-field
           tref-extension 0 (length tref-extension) #xbd)
          (length tref-extension)))
      (setf (aref tref-extension 3)
            (logand (aref tref-extension 3) #x7f))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-pes-extension-field
           tref-extension 0 (length tref-extension) #xbd)))))
    (check-bridge-test
     (= (validate-pes-extension-field
         (octets #x0f #x81 #x01) 0 3 #xfd)
        3))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-pes-extension-field
         (octets #x0f #x81 #x01) 0 3 #xbd))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (parse-pes-header
         (make-pes #xfd payload 90000)))))
    (let* ((source (make-pes #xfd payload 90000))
           (source-payload-offset (+ 9 (aref source 8)))
           (extension (octets #x0f #x81 #x01))
           (extended
             (concatenate-octets
              (subseq source 0 source-payload-offset)
              extension
              payload)))
      (setf (aref extended 7)
            (logior (aref extended 7) #x01)
            (aref extended 8)
            (+ (aref extended 8) (length extension)))
      (write-u16-be
       (+ (read-u16-be source 4) (length extension))
       extended 4)
      (let ((extended-header (parse-pes-header extended)))
        (check-bridge-test
         (= (pes-header-stream-id extended-header) #xfd))
        (check-bridge-test
         (equalp
          (subseq extended
                  (pes-header-payload-offset extended-header))
          payload))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-pes-extension-field
         (octets #x2e) 0 1 #xbd))))
    (let ((crc-pes
            (make-pes #xbd payload 90000)))
      (setf (aref crc-pes 7)
            (logior (aref crc-pes 7) #x02))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (parse-pes-header crc-pes)))))
    (let* ((source (make-pes #xbd payload 90000))
           (source-payload-offset
             (pes-header-payload-offset
              (parse-pes-header source)))
           (crc-field (octets #x12 #x34))
           (extended
             (concatenate-octets
              (subseq source 0 source-payload-offset)
              crc-field
              payload)))
      (setf (aref extended 7)
            (logior (aref extended 7) #x02)
            (aref extended 8)
            (+ (aref extended 8) (length crc-field)))
      (write-u16-be
       (+ (read-u16-be source 4) (length crc-field))
       extended 4)
      ;; CRC field自体の長さが妥当でも、直前PES payloadを検証できない
      ;; 単一packet parserではfail closedにする。
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (parse-pes-header extended)))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        ;; 両markerとoriginal_stuff_lengthが妥当でも、program全体の
        ;; sequence状態を検証できないためfail closedにする。
        (validate-pes-extension-field
         (octets #x2e #x85 #xc3) 0 3 #xbd))))
    (check-bridge-test
     (= (validate-pes-extension-field
         (octets #x1e #x40 0) 0 3 #xbd)
        3))
    (check-bridge-test
     (= (validate-pes-extension-field
         (octets #x1e #x60 0) 0 3 #xe0)
        3))
    (dolist (bad-p-std
             (list
              (list (octets #x1e #x60 0) #xc0)
              (list (octets #x1e #x40 0) #xe0)))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-pes-extension-field
           (first bad-p-std)
           0
           (length (first bad-p-std))
           (second bad-p-std))))))
    (let ((private-data
            (concatenate-octets
             (octets #x8e)
             (make-array
              16
              :element-type 'octet
              :initial-element #x55))))
      (check-bridge-test
       (= (validate-pes-extension-field
           private-data 0 (length private-data) #xbd)
          (length private-data)))
      (setf (aref private-data 1) 0
            (aref private-data 2) 0
            (aref private-data 3) 1)
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-pes-extension-field
           private-data 0 (length private-data) #xbd)))))
    (let ((boundary
            (make-array
             20
             :element-type 'octet
             :initial-element #x55)))
      (setf (aref boundary 0) 0
            (aref boundary 1) 0
            (aref boundary 2) 1)
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-pes-private-data-field boundary 2 18))))
      (fill boundary #x55)
      (setf (aref boundary 17) 0
            (aref boundary 18) 0
            (aref boundary 19) 1)
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-pes-private-data-field boundary 2 20)))))
    (let* ((source
             (make-pes #xbd (octets 0 1 #x55) 90000))
           (payload-offset
             (pes-header-payload-offset
              (parse-pes-header source)))
           (private-bytes
             (make-array
              16
              :element-type 'octet
              :initial-element #x55))
           (extension
             (concatenate-octets
              (octets #x8e)
              private-bytes))
           (extended
             (concatenate-octets
              (subseq source 0 payload-offset)
              extension
              (subseq source payload-offset))))
      (setf (aref private-bytes 15) 0
            (aref extended
                  (+ payload-offset (length extension) -1))
            0
            (aref extended 7)
            (logior (aref extended 7) #x01)
            (aref extended 8)
            (+ (aref extended 8) (length extension)))
      (write-u16-be
       (+ (read-u16-be source 4) (length extension))
       extended 4)
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (parse-pes-header extended)))))
    (let* ((mpeg-two-pack
             (octets
              0 0 1 #xba
              #x44 0 #x04 0 #x04 #x01
              0 0 #x07 #xf8))
           (mpeg-one-pack
             (octets
              0 0 1 #xba
              #x21 0 #x01 0 #x01 #x80 0 #x03))
           (system-header
             (octets
              0 0 1 #xbb 0 6
              #x80 0 #x03 0 #x20 #x7f))
           (mpeg-one-system-header
             (octets
              0 0 1 #xbb 0 6
              #x80 0 #x03 0 #x20 #xff))
           (pack-with-system
             (concatenate-octets
              mpeg-two-pack system-header))
           (mpeg-one-pack-with-system
             (concatenate-octets
              mpeg-one-pack mpeg-one-system-header))
           (pack-extension
             (concatenate-octets
              (octets #x4e (length pack-with-system))
              pack-with-system)))
      (check-bridge-test
       (= (validate-program-stream-pack-header
           mpeg-two-pack 0 (length mpeg-two-pack))
          (length mpeg-two-pack)))
      (check-bridge-test
       (= (validate-program-stream-pack-header
           mpeg-one-pack 0 (length mpeg-one-pack))
          (length mpeg-one-pack)))
      (check-bridge-test
       (= (validate-program-stream-pack-header
           mpeg-one-pack-with-system
           0
           (length mpeg-one-pack-with-system))
          (length mpeg-one-pack-with-system)))
      (check-bridge-test
       (= (validate-pes-extension-field
           pack-extension 0 (length pack-extension) #xbd)
          (length pack-extension)))
      (let ((bad-marker (copy-seq mpeg-two-pack)))
        (setf (aref bad-marker 12)
              (logand (aref bad-marker 12) #xfe))
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (validate-program-stream-pack-header
             bad-marker 0 (length bad-marker))))))
      (let* ((bad-system (copy-seq system-header))
             (bad-pack
               (concatenate-octets
                mpeg-two-pack bad-system)))
        (setf (aref bad-pack
                    (+ (length mpeg-two-pack) 9))
              #x84)
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (validate-program-stream-pack-header
             bad-pack 0 (length bad-pack))))))
      (let ((bad-rate
              (copy-seq pack-with-system)))
        (setf (aref bad-rate 12) #x0b)
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (validate-program-stream-pack-header
             bad-rate 0 (length bad-rate))))))
      (let ((bad-rate
              (copy-seq mpeg-one-pack-with-system)))
        (setf (aref bad-rate 11) #x05)
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (validate-program-stream-pack-header
             bad-rate 0 (length bad-rate))))))
      (let ((bad-reserved
              (copy-seq mpeg-one-pack-with-system)))
        (setf (aref bad-reserved
                    (+ (length mpeg-one-pack) 11))
              #x7f)
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (validate-program-stream-pack-header
             bad-reserved 0 (length bad-reserved))))))
      (let* ((auxiliary-system
               (octets
                0 0 1 #xbb 0 12
                #x80 0 #x03 0 #x20 #x7f
                #xb7 #xc0 #x10 #xb6 #xe0 #x01))
             (auxiliary-pack
               (concatenate-octets
                mpeg-two-pack auxiliary-system)))
        (check-bridge-test
         (= (validate-program-stream-pack-header
             auxiliary-pack 0 (length auxiliary-pack))
            (length auxiliary-pack)))
        (let ((bad-scale (copy-seq auxiliary-pack)))
          (setf (aref bad-scale
                      (+ (length mpeg-two-pack) 16))
                #xc0)
          (check-bridge-test
           (signals-bridge-error-p
            (lambda ()
              (validate-program-stream-pack-header
               bad-scale 0 (length bad-scale))))))
        (let ((mpeg-one-auxiliary
                (concatenate-octets
                 mpeg-one-pack auxiliary-system)))
          (setf (aref mpeg-one-auxiliary
                      (+ (length mpeg-one-pack) 11))
                #xff)
          (check-bridge-test
           (signals-bridge-error-p
            (lambda ()
              (validate-program-stream-pack-header
               mpeg-one-auxiliary
               0
               (length mpeg-one-auxiliary)))))))
      (let ((stuffed
              (concatenate-octets
               mpeg-two-pack (octets #xff))))
        (setf (aref stuffed 13) #xf9)
        (check-bridge-test
         (= (validate-program-stream-pack-header
             stuffed 0 (length stuffed))
            (length stuffed)))
        (let ((bad-stuffing (copy-seq stuffed)))
          (setf (aref bad-stuffing 14) 0)
          (check-bridge-test
           (signals-bridge-error-p
            (lambda ()
              (validate-program-stream-pack-header
               bad-stuffing 0 (length bad-stuffing))))))
        (let ((truncated (subseq stuffed 0 14)))
          (check-bridge-test
           (signals-bridge-error-p
            (lambda ()
              (validate-program-stream-pack-header
               truncated 0 (length truncated)))))))
      (let ((bad-scr-extension
              (copy-seq mpeg-two-pack)))
        (setf (aref bad-scr-extension 8) #x06
              (aref bad-scr-extension 9) #x59)
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (validate-program-stream-pack-header
             bad-scr-extension
             0
             (length bad-scr-extension))))))
      (let ((zero-rate
              (copy-seq mpeg-two-pack)))
        (setf (aref zero-rate 10) 0
              (aref zero-rate 11) 0
              (aref zero-rate 12) #x03)
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (validate-program-stream-pack-header
             zero-rate 0 (length zero-rate))))))
      (let* ((duplicate-system
               (octets
                0 0 1 #xbb 0 12
                #x80 0 #x03 0 #x20 #x7f
                #xc0 #xc0 #x01
                #xc0 #xc0 #x01))
             (duplicate-pack
               (concatenate-octets
                mpeg-two-pack duplicate-system)))
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (validate-program-stream-pack-header
             duplicate-pack 0 (length duplicate-pack))))))
      (let* ((duplicate-extended-system
               (octets
                0 0 1 #xbb 0 18
                #x80 0 #x03 0 #x20 #x7f
                #xb7 #xc0 #x10 #xb6 #xe0 #x01
                #xb7 #xc0 #x10 #xb6 #xe0 #x01))
             (duplicate-extended-pack
               (concatenate-octets
                mpeg-two-pack duplicate-extended-system)))
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (validate-program-stream-pack-header
             duplicate-extended-pack
             0
             (length duplicate-extended-pack))))))
      (dolist
          (overlapping-system
           (list
            (octets
             0 0 1 #xbb 0 12
             #x80 0 #x03 0 #x20 #x7f
             #xb8 #xc0 #x01
             #xc0 #xc0 #x01)
            (octets
             0 0 1 #xbb 0 12
             #x80 0 #x03 0 #x20 #x7f
             #xb9 #xe0 #x01
             #xe0 #xe0 #x01)
            (octets
             0 0 1 #xbb 0 15
             #x80 0 #x03 0 #x20 #x7f
             #xfd #xc0 #x01
             #xb7 #xc0 #x10 #xb6 #xe0 #x01)
            (octets
             0 0 1 #xbb 0 15
             #x80 0 #x03 0 #x20 #x7f
             #xb9 #xe0 #x01
             #xb7 #xc0 #x10 #xb6 #xe0 #x01)))
        (let ((overlapping-pack
                (concatenate-octets
                 mpeg-two-pack overlapping-system)))
          (check-bridge-test
           (signals-bridge-error-p
            (lambda ()
              (validate-program-stream-pack-header
               overlapping-pack
               0
               (length overlapping-pack)))))))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-pes-extension-field
           (octets #x4e 0) 0 2 #xbd)))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-pes-header-stuffing
         (octets 0) 0 1))))))
