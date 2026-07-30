;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defun fixture-pathname (name)
  "NAMEの公開fixture pathnameを返す。"
  (asdf:system-relative-pathname
   "konomitv-bs4k-tscodecbridge/tests"
   (format nil "tests/fixtures/~A" name)))

(defun read-fixture-octets (name)
  "NAMEの公開fixtureをoctet vectorで読む。"
  (with-open-file
      (stream (fixture-pathname name)
              :direction :input
              :element-type 'octet)
    (let ((result
            (make-array (file-length stream)
                        :element-type 'octet)))
      (unless (= (read-sequence result stream)
                 (length result))
        (bridge-error "Fixture read is truncated: ~A" name))
      result)))

(defun run-octet-processor (octets video-codec audio-codec)
  "OCTETSを小分けreadでprocessorへ通し出力octet列を返す。"
  (let ((input
          (make-instance
           'octet-chunk-input-stream
           :data octets
           :chunk-size 13))
        (output (make-instance 'octet-collector-stream)))
    (process-ts-stream
     input output video-codec audio-codec
     :transport-rate-kbps
     (when (eq video-codec :av1)
       +test-transport-rate-kbps+))
    (collected-octets output)))

(define-bridge-test committed-fixtures-match-deterministic-generator
  (let ((vp9
          (packet-list-to-octets
           (make-public-vp9-opus-fixture-packets)))
        (vp9-superframe
          (packet-list-to-octets
           (make-public-vp9-opus-fixture-packets
            :access-unit
            (make-public-vp9-superframe-access-unit))))
        (av1
          (packet-list-to-octets
           (make-public-av1-aac-fixture-packets))))
    (dolist (fixture
               `(("valid-vp9-opus.ts" ,vp9)
               ("valid-vp9-superframe.ts" ,vp9-superframe)
               ("valid-av1-aac.ts" ,av1)
               ("corrupt-sync.ts"
                ,(corrupt-sync-octets vp9))
               ("corrupt-truncated-packet.ts"
                ,(subseq vp9 0 (- (length vp9) 17)))
               ("corrupt-pmt-crc.ts"
                ,(corrupt-pmt-crc-octets vp9))
               ("corrupt-pat-null-program-pid.ts"
                ,(corrupt-pat-null-program-pid-octets vp9))
               ("corrupt-pat-reserved-program-pid.ts"
                ,(corrupt-pat-reserved-program-pid-octets vp9))
               ("corrupt-pat-duplicate-program-number.ts"
                ,(corrupt-pat-duplicate-program-number-octets vp9))
               ("corrupt-pmt-null-elementary-pid.ts"
                ,(corrupt-pmt-null-elementary-pid-octets vp9))
               ("corrupt-pmt-zero-program-number.ts"
                ,(corrupt-pmt-zero-program-number-octets vp9))
               ("corrupt-pmt-reserved-elementary-pid.ts"
                ,(corrupt-pmt-reserved-elementary-pid-octets vp9))
               ("corrupt-pmt-reserved-pcr-pid.ts"
                ,(corrupt-pmt-reserved-pcr-pid-octets vp9))
               ("corrupt-pmt-duplicate-elementary-pid.ts"
                ,(corrupt-pmt-duplicate-elementary-pid-octets vp9))
               ("corrupt-video-cc.ts"
                ,(corrupt-video-continuity-octets vp9))
               ("corrupt-opus-lacing.ts"
                ,(corrupt-opus-lacing-octets vp9))
               ("corrupt-opus-zero-pes-length.ts"
                ,(corrupt-opus-zero-pes-length-octets vp9))
               ("corrupt-pes-truncated-escr.ts"
                ,(corrupt-pes-truncated-escr-octets vp9))
               ("corrupt-vp9-reference-scale.ts"
                ,(packet-list-to-octets
                  (make-public-vp9-opus-fixture-packets
                   :access-unit
                   (make-public-vp9-invalid-scale-access-unit))))
               ("corrupt-vp9-second-subframe.ts"
                ,(packet-list-to-octets
                  (make-public-vp9-opus-fixture-packets
                   :access-unit
                   (make-public-vp9-corrupt-second-frame-access-unit))))
               ("corrupt-vp9-compressed-header-size.ts"
                ,(packet-list-to-octets
                  (make-public-vp9-opus-fixture-packets
                   :access-unit
                   (make-public-vp9-compressed-header-overrun-access-unit))))
               ("corrupt-vp9-tile-size.ts"
                ,(packet-list-to-octets
                  (make-public-vp9-opus-fixture-packets
                   :access-unit
                   (make-public-vp9-tile-overrun-access-unit))))
               ("corrupt-vp9-reserved-color-space.ts"
                ,(packet-list-to-octets
                  (make-public-vp9-opus-fixture-packets
                   :access-unit
                   (make-public-vp9-reserved-color-access-unit))))))
      (check-bridge-test
       (equalp (read-fixture-octets (first fixture))
               (second fixture))
       (first fixture)))))

(define-bridge-test committed-valid-fixtures-pass-processor-and-inspector
  (let* ((vp9-input
           (read-fixture-octets "valid-vp9-opus.ts"))
         (vp9-output
           (run-octet-processor vp9-input :vp9 :opus))
         (vp9-superframe-input
           (read-fixture-octets "valid-vp9-superframe.ts"))
         (vp9-superframe-output
           (run-octet-processor
            vp9-superframe-input :vp9 :opus))
         (av1-input
           (read-fixture-octets "valid-av1-aac.ts"))
         (av1-output
           (run-octet-processor av1-input :av1 :aac))
         (vp9-inspection (inspect-ts-octets vp9-output))
         (vp9-superframe-inspection
           (inspect-ts-octets vp9-superframe-output))
         (av1-inspection (inspect-ts-octets av1-output)))
    (check-bridge-test
     (plusp (ts-inspection-packet-count vp9-inspection)))
    (check-bridge-test
     (plusp
      (ts-inspection-packet-count
       vp9-superframe-inspection)))
    (check-bridge-test
     (plusp (ts-inspection-packet-count av1-inspection)))
    (check-bridge-test
     (non-target-packets-byte-exact-p
      vp9-input vp9-output
      (list +test-pmt-pid+
            +test-video-pid+
            +test-audio-one-pid+
            +test-audio-two-pid+)))
    (check-bridge-test
     (non-target-packets-byte-exact-p
      vp9-superframe-input vp9-superframe-output
      (list +test-pmt-pid+
            +test-video-pid+
            +test-audio-one-pid+
            +test-audio-two-pid+)))
    (check-bridge-test
     (non-target-packets-byte-exact-p
      av1-input av1-output
      (list +test-pmt-pid+ +test-video-pid+)))))

(define-bridge-test committed-corrupt-fixtures-fail-closed
  (dolist (name
           '("corrupt-sync.ts"
             "corrupt-truncated-packet.ts"
             "corrupt-pmt-crc.ts"
             "corrupt-pat-null-program-pid.ts"
             "corrupt-pat-reserved-program-pid.ts"
             "corrupt-pat-duplicate-program-number.ts"
             "corrupt-pmt-null-elementary-pid.ts"
             "corrupt-pmt-zero-program-number.ts"
             "corrupt-pmt-reserved-elementary-pid.ts"
             "corrupt-pmt-reserved-pcr-pid.ts"
             "corrupt-pmt-duplicate-elementary-pid.ts"
             "corrupt-video-cc.ts"))
    (check-bridge-test
     (signals-bridge-error-p
     (lambda ()
        (inspect-ts-octets
         (read-fixture-octets name))))
     name))
  (check-bridge-test
   (signals-bridge-error-p
   (lambda ()
      (run-validating-fast-path
       (read-fixture-octets "corrupt-video-cc.ts"))))
   "corrupt-video-cc.ts default fast path")
  (dolist
      (name
       '("corrupt-pat-null-program-pid.ts"
         "corrupt-pat-reserved-program-pid.ts"
         "corrupt-pat-duplicate-program-number.ts"
         "corrupt-pmt-null-elementary-pid.ts"
         "corrupt-pmt-zero-program-number.ts"
         "corrupt-pmt-reserved-elementary-pid.ts"
         "corrupt-pmt-reserved-pcr-pid.ts"
         "corrupt-pmt-duplicate-elementary-pid.ts"))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (run-octet-processor
         (read-fixture-octets name)
         :vp9 :opus)))
     name))
  (dolist
      (name
       '("corrupt-opus-lacing.ts"
         "corrupt-opus-zero-pes-length.ts"
         "corrupt-pes-truncated-escr.ts"))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (run-octet-processor
         (read-fixture-octets name)
         :vp9 :opus)))
     name))
  (dolist
      (name
       '("corrupt-vp9-reference-scale.ts"
         "corrupt-vp9-second-subframe.ts"
         "corrupt-vp9-compressed-header-size.ts"
         "corrupt-vp9-tile-size.ts"
         "corrupt-vp9-reserved-color-space.ts"))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (run-octet-processor
         (read-fixture-octets name)
         :vp9 :opus)))
     name)))

(define-bridge-test semantic-path-rejects-non-target-transport-corruption
  (let* ((valid
           (read-fixture-octets "valid-vp9-opus.ts"))
         (packets (ts-octets-to-packets valid))
         (timed-id3-index
           (position
            +test-timed-id3-pid+ packets
            :key #'ts-pid :test #'=))
         (data-index
           (position
            +test-data-pid+ packets
            :key #'ts-pid :test #'=)))
    (unless (and timed-id3-index data-index)
      (bridge-error
       "Public fixture is missing non-target integrity test PIDs"))
    (flet ((replace-packet (index replacement)
             (loop for packet in packets
                   for position from 0
                   collect
                   (if (= position index)
                       replacement
                       (copy-seq packet))))
           (insert-packet-after (index inserted)
             (loop for packet in packets
                   for position from 0
                   append
                   (if (= position index)
                       (list (copy-seq packet) inserted)
                       (list (copy-seq packet))))))
      (let ((tei
              (copy-seq (nth timed-id3-index packets)))
            (scrambled
              (copy-seq (nth data-index packets)))
            (conflicting-duplicate
              (copy-seq (nth data-index packets))))
        (setf
         (aref tei 1)
         (logior (aref tei 1) #x80)
         (aref scrambled 3)
         (logior (aref scrambled 3) #x80)
         (aref conflicting-duplicate
               (1- +ts-packet-size+))
         (logxor
          (aref conflicting-duplicate
                (1- +ts-packet-size+))
          1))
        (dolist
            (corrupt-packets
             (list
              (replace-packet timed-id3-index tei)
              (replace-packet data-index scrambled)
              (insert-packet-after
               data-index conflicting-duplicate)))
          (check-bridge-test
           (signals-bridge-error-p
            (lambda ()
              (run-octet-processor
               (packet-list-to-octets corrupt-packets)
               :vp9 :opus))))))
      (let* ((duplicate-input
               (packet-list-to-octets
                (insert-packet-after
                 data-index
                 (copy-seq (nth data-index packets)))))
             (duplicate-output
               (run-octet-processor
                duplicate-input :vp9 :opus)))
        (check-bridge-test
         (non-target-packets-byte-exact-p
          duplicate-input duplicate-output
          (list +test-pmt-pid+
                +test-video-pid+
                +test-audio-one-pid+
                +test-audio-two-pid+)))))))
