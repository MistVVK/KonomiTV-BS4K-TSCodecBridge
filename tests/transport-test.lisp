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
             payload))))
