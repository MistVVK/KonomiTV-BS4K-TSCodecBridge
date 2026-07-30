;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defun make-public-vp9-opus-fixture-packets (&key access-unit)
  "公開fixture用VP9+2 Opus TS packet列を決定的に作る。"
  (let* ((video-pes
           (make-pes
            #xbd
            (or
             access-unit
             (concatenate-octets
              (make-test-vp9-key-frame 0 1920 1080)
              (make-pattern-octets 640 91)))
            90000
            :dts 87000))
         (video-packets
           (packetize-test-video-with-pcr video-pes))
         (opus-one-pes
           (packetize-payload
            +test-audio-one-pid+
            (make-test-opus-pes
             90000 (octets #xf8 #x11 #x22))
            :continuity-counter 1
            :payload-unit-start t))
         (opus-two-pes
           (packetize-payload
            +test-audio-two-pid+
            (make-test-opus-pes
             90000 (octets #xf8 #x33 #x44))
            :continuity-counter 6
            :payload-unit-start t))
         (first-pmt (make-test-pmt-packets))
         (second-pmt
           (make-section-ts-packets
            +test-pmt-pid+
            (build-pmt-section (make-test-pmt-table))
            (+ 4 (length first-pmt))))
         (timed-id3
           (make-ts-packet
            +test-timed-id3-pid+ 0
            (make-pattern-octets 59 12)
            :payload-unit-start t))
         (data
           (make-ts-packet
            +test-data-pid+ 0
            (make-pattern-octets 73 33)
            :payload-unit-start t)))
    (append
     (make-test-pat-packets)
     first-pmt
     opus-one-pes
     (list timed-id3)
     (list (first video-packets))
     (list data)
     (rest video-packets)
     opus-two-pes
     second-pmt)))

(defun make-public-vp9-superframe-access-unit ()
  "異なるcoded sizeを持つ正常VP9 superframeを作る。"
  (make-vp9-structure-test-superframe
   (list
    (make-vp9-structure-test-key-frame
     :width 64 :height 64)
    (make-vp9-structure-test-key-frame
     :width 128 :height 64))
   1))

(defun make-public-vp9-invalid-scale-access-unit ()
  "reference倍率を超えるinter frameを持つVP9 superframeを作る。"
  (make-vp9-structure-test-superframe
   (list
    (make-vp9-structure-test-key-frame
     :width 64 :height 64)
    (make-vp9-structure-test-inter-frame
     :width 1025
     :height 64
     :size-reference-position nil))
   1))

(defun make-public-vp9-corrupt-second-frame-access-unit ()
  "2個目のframe markerを壊したVP9 superframeを作る。"
  (let ((second
          (make-vp9-structure-test-inter-frame)))
    (setf (aref second 0) 0)
    (make-vp9-structure-test-superframe
     (list
      (make-vp9-structure-test-key-frame)
      second)
     1)))

(defun make-public-vp9-compressed-header-overrun-access-unit ()
  "compressed header sizeがframe境界を超えるVP9 frameを作る。"
  (make-vp9-structure-test-key-frame
   :compressed-header-size 32
   :actual-compressed-header-size 1))

(defun make-public-vp9-tile-overrun-access-unit ()
  "非最終tile sizeがframe境界を超えるVP9 frameを作る。"
  (make-vp9-structure-test-key-frame
   :width 512
   :column-log2 1
   :declared-tile-sizes '(4096)
   :actual-tile-sizes '(1 1)))

(defun make-public-vp9-reserved-color-access-unit ()
  "reserved color spaceを持つVP9 frameを作る。"
  (make-vp9-structure-test-key-frame
   :color-space 6))

(defun make-public-av1-aac-pmt ()
  "公開fixture用raw AV1+Aac PMTを作る。"
  (make-program-map-table
   :program-number 1
   :version 11
   :pcr-pid +test-video-pid+
   :streams
   (list
    (make-pmt-stream
     :stream-type #x06
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

(defun make-public-av1-aac-fixture-packets ()
  "公開fixture用AV1+AAC TS packet列を決定的に作る。"
  (let ((pmt
           (make-section-ts-packets
            +test-pmt-pid+
            (build-pmt-section
             (make-public-av1-aac-pmt))
            3))
         (video
           (packetize-test-video-with-pcr
            (make-pes
             #xe0
             (make-test-av1-access-unit 8)
             180000
             :dts 177000)
            :continuity-counter 4
            :transport-priority t
            :pcr-lead-ticks
            +test-tstd-removal-delay-ticks+))
         (audio
           (packetize-payload
            +test-audio-one-pid+
            (make-pes
             #xc0
             (make-pattern-octets 196 51)
             180000)
            :continuity-counter 8
            :payload-unit-start t))
         (timed-id3
           (make-ts-packet
            +test-timed-id3-pid+ 2
            (make-pattern-octets 45 71)
            :payload-unit-start t))
         (data
           (make-ts-packet
            +test-data-pid+ 12
            (make-pattern-octets 100 19)
            :payload-unit-start t)))
    (append
     (make-test-pat-packets)
     pmt
     audio
     (list timed-id3)
     video
     (list data))))

(defun corrupt-sync-octets (octets)
  "OCTETSの2 packet目のsync byteを壊す。"
  (let ((result (copy-seq octets)))
    (setf (aref result +ts-packet-size+) 0)
    result))

(defun corrupt-pmt-crc-octets (octets)
  "OCTETSの最初のPMT section CRCを1 bit反転する。"
  (let ((result (copy-seq octets)))
    (loop for offset from 0 below (length result)
            by +ts-packet-size+
          for packet = (subseq result offset
                               (+ offset +ts-packet-size+))
          when (= (ts-pid packet) +test-pmt-pid+)
            do (let* ((payload-offset
                        (ts-payload-offset packet))
                      (section-start (+ payload-offset 1))
                      (section-length
                        (section-total-length
                         (subseq packet section-start
                                 (+ section-start 3))))
                      (crc-last
                        (+ offset section-start
                           section-length -1)))
                 (setf (aref result crc-last)
                       (logxor (aref result crc-last) 1))
                 (return)))
    result))

(defun corrupt-video-continuity-octets (octets)
  "OCTETSの2個目のvideo payload CCを飛ばす。"
  (let ((result (copy-seq octets))
        (seen 0))
    (loop for offset from 0 below (length result)
            by +ts-packet-size+
          for packet = (subseq result offset
                               (+ offset +ts-packet-size+))
          when (and (= (ts-pid packet) +test-video-pid+)
                    (ts-has-payload-p packet))
            do (incf seen)
               (when (= seen 2)
                 (setf (aref result (+ offset 3))
                       (logior
                        (logand (aref result (+ offset 3)) #xf0)
                        (logand
                         (+ (ts-continuity-counter packet) 3)
                         #x0f)))
                 (return)))
    result))

(defun corrupt-opus-lacing-octets (octets)
  "OCTETSの最初のOpus control lacingを過大値へする。"
  (let ((result (copy-seq octets)))
    (loop for offset from 0 below (length result)
            by +ts-packet-size+
          for packet = (subseq result offset
                               (+ offset +ts-packet-size+))
          when (and (= (ts-pid packet) +test-audio-one-pid+)
                    (ts-payload-unit-start-p packet))
            do (let* ((ts-payload-offset
                        (ts-payload-offset packet))
                      (pes-bytes
                        (subseq packet ts-payload-offset))
                      (header (parse-pes-header
                               (subseq
                                pes-bytes
                                0
                                (+ 6 (read-u16-be
                                      pes-bytes 4)))))
                      (lacing-offset
                        (+ offset
                           ts-payload-offset
                           (pes-header-payload-offset header)
                           2)))
                 (setf (aref result lacing-offset) #xfe)
                 (return)))
    result))

(defun write-fixture-octets (pathname octets)
  "OCTETSをPATHNAMEへ上書きする。"
  (ensure-directories-exist pathname)
  (with-open-file
      (stream pathname
              :direction :output
              :if-exists :supersede
              :if-does-not-exist :create
              :element-type 'octet)
    (write-sequence octets stream))
  pathname)

(defun generate-public-fixtures (project-root)
  "PROJECT-ROOT/tests/fixturesへ正常・破損fixtureを再生成する。"
  (let ((fixture-directory
           (merge-pathnames "tests/fixtures/" project-root))
         (vp9
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
    (write-fixture-octets
     (merge-pathnames "valid-vp9-opus.ts" fixture-directory)
     vp9)
    (write-fixture-octets
     (merge-pathnames "valid-av1-aac.ts" fixture-directory)
     av1)
    (write-fixture-octets
     (merge-pathnames "valid-vp9-superframe.ts"
                      fixture-directory)
     vp9-superframe)
    (write-fixture-octets
     (merge-pathnames "corrupt-sync.ts" fixture-directory)
     (corrupt-sync-octets vp9))
    (write-fixture-octets
     (merge-pathnames "corrupt-truncated-packet.ts"
                      fixture-directory)
     (subseq vp9 0 (- (length vp9) 17)))
    (write-fixture-octets
     (merge-pathnames "corrupt-pmt-crc.ts"
                      fixture-directory)
     (corrupt-pmt-crc-octets vp9))
    (write-fixture-octets
     (merge-pathnames "corrupt-video-cc.ts"
                      fixture-directory)
     (corrupt-video-continuity-octets vp9))
    (write-fixture-octets
     (merge-pathnames "corrupt-opus-lacing.ts"
                      fixture-directory)
     (corrupt-opus-lacing-octets vp9))
    (dolist
        (entry
         `(("corrupt-vp9-reference-scale.ts"
            ,(make-public-vp9-invalid-scale-access-unit))
           ("corrupt-vp9-second-subframe.ts"
            ,(make-public-vp9-corrupt-second-frame-access-unit))
           ("corrupt-vp9-compressed-header-size.ts"
            ,(make-public-vp9-compressed-header-overrun-access-unit))
           ("corrupt-vp9-tile-size.ts"
            ,(make-public-vp9-tile-overrun-access-unit))
           ("corrupt-vp9-reserved-color-space.ts"
            ,(make-public-vp9-reserved-color-access-unit))))
      (write-fixture-octets
       (merge-pathnames (first entry) fixture-directory)
       (packet-list-to-octets
        (make-public-vp9-opus-fixture-packets
         :access-unit (second entry)))))
    t))
