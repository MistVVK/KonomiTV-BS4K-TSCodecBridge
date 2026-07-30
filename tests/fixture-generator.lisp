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
            :payload-unit-start t))
         (subtitle
           (make-ts-packet
            +test-subtitle-pid+ 0
            (make-pattern-octets 83 54)
            :payload-unit-start t)))
    (append
     (make-test-pat-packets)
     first-pmt
     opus-one-pes
     (list timed-id3)
     (list (first video-packets))
     (list data)
     (list subtitle)
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
     :elementary-pid +test-data-pid+)
    (make-pmt-stream
     :stream-type #x06
     :elementary-pid +test-subtitle-pid+
     :descriptors
     (list
      (make-descriptor
       :tag #xfd
       :payload (octets #x00 #x08)))))))

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
            :payload-unit-start t))
         (subtitle
           (make-ts-packet
            +test-subtitle-pid+ 9
            (make-pattern-octets 91 84)
            :payload-unit-start t)))
    (append
     (make-test-pat-packets)
     pmt
     audio
     (list timed-id3)
     video
     (list data subtitle))))

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

(defun corrupt-pat-null-program-pid-octets (octets)
  "OCTETSの最初のPAT program PIDをnull PIDへする。"
  (let ((result (copy-seq octets))
        (found nil))
    (loop for offset from 0 below (length result)
            by +ts-packet-size+
          for packet = (subseq result offset
                               (+ offset +ts-packet-size+))
          when (= (ts-pid packet) 0)
            do (let* ((payload-offset (ts-payload-offset packet))
                      (section-start (+ payload-offset 1))
                      (section-length
                        (section-total-length
                         (subseq packet section-start
                                 (+ section-start 3))))
                      (section
                        (subseq packet section-start
                                (+ section-start section-length))))
                 (setf (aref section 10) #xff
                       (aref section 11) #xff)
                 (rewrite-psi-crc section)
                 (replace result section
                          :start1 (+ offset section-start))
                 (setf found t)
                 (return)))
    (unless found
      (bridge-error "Public fixture does not contain a PAT"))
    result))

(defun corrupt-pat-reserved-program-pid-octets (octets)
  "OCTETSの最初のPAT program PIDを予約範囲へする。"
  (let ((result (copy-seq octets))
        (found nil))
    (loop for offset from 0 below (length result)
            by +ts-packet-size+
          for packet = (subseq result offset
                               (+ offset +ts-packet-size+))
          when (= (ts-pid packet) 0)
            do (let* ((payload-offset (ts-payload-offset packet))
                      (section-start (+ payload-offset 1))
                      (section-length
                        (section-total-length
                         (subseq packet section-start
                                 (+ section-start 3))))
                      (section
                        (subseq packet section-start
                                (+ section-start section-length))))
                 (setf (aref section 10) #xe0
                       (aref section 11) #x0f)
                 (rewrite-psi-crc section)
                 (replace result section
                          :start1 (+ offset section-start))
                 (setf found t)
                 (return)))
    (unless found
      (bridge-error "Public fixture does not contain a PAT"))
    result))

(defun corrupt-pat-duplicate-program-number-octets (octets)
  "OCTETSの最初のPATへ重複program_numberを追加する。"
  (let* ((packets (ts-octets-to-packets octets))
         (index (position 0 packets :key #'ts-pid :test #'=)))
    (unless index
      (bridge-error "Public fixture does not contain a PAT"))
    (let* ((packet (nth index packets))
           (payload-offset (ts-payload-offset packet))
           (section-start (+ payload-offset 1))
           (section-length
             (section-total-length
              (subseq packet section-start (+ section-start 3))))
           (section
             (subseq packet section-start
                     (+ section-start section-length)))
           (table (parse-pat-section section))
           (first-program
             (first (program-association-table-programs table)))
           (second-program-number
             (let ((candidate
                     (logand
                      (1+ (pat-program-program-number first-program))
                      #xffff)))
               (if (zerop candidate) 1 candidate))))
      (setf
       (program-association-table-programs table)
       (append
        (program-association-table-programs table)
        (list
         (make-pat-program
          :program-number second-program-number
          :pid (pat-program-pid first-program)))))
      (let ((expanded (build-pat-section table)))
        (write-u16-be
         (pat-program-program-number first-program)
         expanded 12)
        (rewrite-psi-crc expanded)
        (let ((replacement
                (make-section-ts-packets
                 0 expanded (ts-continuity-counter packet))))
          (unless (= (length replacement) 1)
            (bridge-error
             "Expanded public PAT requires multiple packets"))
          (setf (nth index packets) (first replacement)))))
    (packet-list-to-octets packets)))

(defun corrupt-pmt-null-elementary-pid-octets (octets)
  "OCTETSの最初のPMT映像elementary_PIDをnull PIDへする。"
  (let ((result (copy-seq octets))
        (found nil))
    (loop for offset from 0 below (length result)
            by +ts-packet-size+
          for packet = (subseq result offset
                               (+ offset +ts-packet-size+))
          when (= (ts-pid packet) +test-pmt-pid+)
            do (let* ((payload-offset (ts-payload-offset packet))
                      (section-start (+ payload-offset 1))
                      (section-length
                        (section-total-length
                         (subseq packet section-start
                                 (+ section-start 3))))
                      (section
                        (subseq packet section-start
                                (+ section-start section-length))))
                 ;; 公開fixtureのprogram_info_lengthは0で、先頭ESはoffset 12。
                 (unless (and (= (logand (aref section 10) #x0f) 0)
                              (= (aref section 11) 0))
                   (bridge-error
                    "Public PMT fixture program info is not empty"))
                 (setf (aref section 13) #xff
                       (aref section 14) #xff)
                 (rewrite-psi-crc section)
                 (replace result section
                          :start1 (+ offset section-start))
                 (setf found t)
                 (return)))
    (unless found
      (bridge-error "Public fixture does not contain a PMT"))
    result))

(defun corrupt-pmt-zero-program-number-octets (octets)
  "OCTETSの最初のPMT program_numberを予約値0へする。"
  (let ((result (copy-seq octets))
        (found nil))
    (loop for offset from 0 below (length result)
            by +ts-packet-size+
          for packet = (subseq result offset
                               (+ offset +ts-packet-size+))
          when (= (ts-pid packet) +test-pmt-pid+)
            do (let* ((payload-offset (ts-payload-offset packet))
                      (section-start (+ payload-offset 1))
                      (section-length
                        (section-total-length
                         (subseq packet section-start
                                 (+ section-start 3))))
                      (section
                        (subseq packet section-start
                                (+ section-start section-length))))
                 (write-u16-be 0 section 3)
                 (rewrite-psi-crc section)
                 (replace result section
                          :start1 (+ offset section-start))
                 (setf found t)
                 (return)))
    (unless found
      (bridge-error "Public fixture does not contain a PMT"))
    result))

(defun corrupt-pmt-reserved-elementary-pid-octets (octets)
  "OCTETSの最初のPMT映像elementary_PIDを予約範囲へする。"
  (let ((result (copy-seq octets))
        (found nil))
    (loop for offset from 0 below (length result)
            by +ts-packet-size+
          for packet = (subseq result offset
                               (+ offset +ts-packet-size+))
          when (= (ts-pid packet) +test-pmt-pid+)
            do (let* ((payload-offset (ts-payload-offset packet))
                      (section-start (+ payload-offset 1))
                      (section-length
                        (section-total-length
                         (subseq packet section-start
                                 (+ section-start 3))))
                      (section
                        (subseq packet section-start
                                (+ section-start section-length))))
                 (setf (aref section 13) #xe0
                       (aref section 14) #x0f)
                 (rewrite-psi-crc section)
                 (replace result section
                          :start1 (+ offset section-start))
                 (setf found t)
                 (return)))
    (unless found
      (bridge-error "Public fixture does not contain a PMT"))
    result))

(defun corrupt-pmt-reserved-pcr-pid-octets (octets)
  "OCTETSの最初のPMT PCR_PIDを予約範囲へする。"
  (let ((result (copy-seq octets))
        (found nil))
    (loop for offset from 0 below (length result)
            by +ts-packet-size+
          for packet = (subseq result offset
                               (+ offset +ts-packet-size+))
          when (= (ts-pid packet) +test-pmt-pid+)
            do (let* ((payload-offset (ts-payload-offset packet))
                      (section-start (+ payload-offset 1))
                      (section-length
                        (section-total-length
                         (subseq packet section-start
                                 (+ section-start 3))))
                      (section
                        (subseq packet section-start
                                (+ section-start section-length))))
                 (setf (aref section 8) #xe0
                       (aref section 9) #x0f)
                 (rewrite-psi-crc section)
                 (replace result section
                          :start1 (+ offset section-start))
                 (setf found t)
                 (return)))
    (unless found
      (bridge-error "Public fixture does not contain a PMT"))
    result))

(defun corrupt-pmt-duplicate-elementary-pid-octets (octets)
  "OCTETSの最初のPMTで第2 ES PIDを第1 ES PIDと重複させる。"
  (let ((result (copy-seq octets))
        (found nil))
    (loop for offset from 0 below (length result)
            by +ts-packet-size+
          for packet = (subseq result offset
                               (+ offset +ts-packet-size+))
          when (= (ts-pid packet) +test-pmt-pid+)
            do (let* ((payload-offset (ts-payload-offset packet))
                      (section-start (+ payload-offset 1))
                      (section-length
                        (section-total-length
                         (subseq packet section-start
                                 (+ section-start 3))))
                      (section
                        (subseq packet section-start
                                (+ section-start section-length)))
                      (program-info-length
                        (logior
                         (ash (logand (aref section 10) #x0f) 8)
                         (aref section 11)))
                      (first-offset (+ 12 program-info-length))
                      (first-info-length
                        (logior
                         (ash
                          (logand (aref section (+ first-offset 3))
                                  #x0f)
                          8)
                         (aref section (+ first-offset 4))))
                      (second-offset
                        (+ first-offset 5 first-info-length)))
                 (when (> (+ second-offset 5)
                          (- (length section) 4))
                   (bridge-error
                    "Public PMT fixture does not contain two ES entries"))
                 (setf (aref section (+ second-offset 1))
                       (aref section (+ first-offset 1))
                       (aref section (+ second-offset 2))
                       (aref section (+ first-offset 2)))
                 (rewrite-psi-crc section)
                 (replace result section
                          :start1 (+ offset section-start))
                 (setf found t)
                 (return)))
    (unless found
      (bridge-error "Public fixture does not contain a PMT"))
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

(defun corrupt-opus-zero-pes-length-octets (octets)
  "OCTETSの最初のOpus PES_packet_lengthを禁止値0へする。"
  (let ((result (copy-seq octets))
        (found nil))
    (loop for offset from 0 below (length result)
            by +ts-packet-size+
          for packet = (subseq result offset
                               (+ offset +ts-packet-size+))
          when (and (= (ts-pid packet) +test-audio-one-pid+)
                    (ts-payload-unit-start-p packet))
            do (let ((pes-offset
                       (+ offset (ts-payload-offset packet))))
                 (unless (and (= (aref result pes-offset) 0)
                              (= (aref result (+ pes-offset 1)) 0)
                              (= (aref result (+ pes-offset 2)) 1)
                              (= (aref result (+ pes-offset 3)) #xbd))
                   (bridge-error
                    "Public Opus fixture PES prefix is invalid"))
                 (setf (aref result (+ pes-offset 4)) 0
                       (aref result (+ pes-offset 5)) 0
                       found t)
                 (return)))
    (unless found
      (bridge-error "Public fixture does not contain an Opus PES"))
    result))

(defun corrupt-pes-truncated-escr-octets (octets)
  "OCTETSの最初のOpus PESへ領域のないESCR flagを設定する。"
  (let ((result (copy-seq octets))
        (found nil))
    (loop for offset from 0 below (length result)
            by +ts-packet-size+
          for packet = (subseq result offset
                               (+ offset +ts-packet-size+))
          when (and (= (ts-pid packet) +test-audio-one-pid+)
                    (ts-payload-unit-start-p packet))
            do (let ((pes-offset
                       (+ offset (ts-payload-offset packet))))
                 (unless (and (= (aref result (+ pes-offset 7)) #x80)
                              (= (aref result (+ pes-offset 8)) 5))
                   (bridge-error
                    "Public Opus fixture optional header is invalid"))
                 (setf (aref result (+ pes-offset 7)) #xa0
                       found t)
                 (return)))
    (unless found
      (bridge-error "Public fixture does not contain an Opus PES"))
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
     (merge-pathnames "corrupt-pat-null-program-pid.ts"
                      fixture-directory)
     (corrupt-pat-null-program-pid-octets vp9))
    (write-fixture-octets
     (merge-pathnames "corrupt-pat-reserved-program-pid.ts"
                      fixture-directory)
     (corrupt-pat-reserved-program-pid-octets vp9))
    (write-fixture-octets
     (merge-pathnames "corrupt-pat-duplicate-program-number.ts"
                      fixture-directory)
     (corrupt-pat-duplicate-program-number-octets vp9))
    (write-fixture-octets
     (merge-pathnames "corrupt-pmt-null-elementary-pid.ts"
                      fixture-directory)
     (corrupt-pmt-null-elementary-pid-octets vp9))
    (write-fixture-octets
     (merge-pathnames "corrupt-pmt-zero-program-number.ts"
                      fixture-directory)
     (corrupt-pmt-zero-program-number-octets vp9))
    (write-fixture-octets
     (merge-pathnames "corrupt-pmt-reserved-elementary-pid.ts"
                      fixture-directory)
     (corrupt-pmt-reserved-elementary-pid-octets vp9))
    (write-fixture-octets
     (merge-pathnames "corrupt-pmt-reserved-pcr-pid.ts"
                      fixture-directory)
     (corrupt-pmt-reserved-pcr-pid-octets vp9))
    (write-fixture-octets
     (merge-pathnames "corrupt-pmt-duplicate-elementary-pid.ts"
                      fixture-directory)
     (corrupt-pmt-duplicate-elementary-pid-octets vp9))
    (write-fixture-octets
     (merge-pathnames "corrupt-video-cc.ts"
                      fixture-directory)
     (corrupt-video-continuity-octets vp9))
    (write-fixture-octets
     (merge-pathnames "corrupt-opus-lacing.ts"
                      fixture-directory)
     (corrupt-opus-lacing-octets vp9))
    (write-fixture-octets
     (merge-pathnames "corrupt-opus-zero-pes-length.ts"
                      fixture-directory)
     (corrupt-opus-zero-pes-length-octets vp9))
    (write-fixture-octets
     (merge-pathnames "corrupt-pes-truncated-escr.ts"
                      fixture-directory)
     (corrupt-pes-truncated-escr-octets vp9))
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
