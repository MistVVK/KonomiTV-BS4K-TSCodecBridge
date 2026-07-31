;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defconstant +stream-anchor-version+ 1)
(defconstant +stream-anchor-payload-byte-count+ 24)
(defconstant +stream-anchor-default-maximum-distance-ticks+ 4500)
(defconstant +stream-anchor-maximum-video-history-ticks+ 450000)
(defconstant +stream-anchor-maximum-video-point-count+ 2048)
(defconstant +stream-anchor-maximum-pending-packet-count+ 131072)

(defparameter +stream-anchor-source-owner+
  (map '(simple-array (unsigned-byte 8) (*))
       #'char-code
       "com.konomitv-bs4k.stream-anchor-source.v1"))

(defparameter +stream-anchor-final-owner+
  (map '(simple-array (unsigned-byte 8) (*))
       #'char-code
       "com.konomitv-bs4k.stream-anchor.v1"))

(defparameter +id3-signature+
  (map '(simple-array (unsigned-byte 8) (*))
       #'char-code
       "ID3"))

(defparameter +id3-priv-frame-identifier+
  (map '(simple-array (unsigned-byte 8) (*))
       #'char-code
       "PRIV"))

(defstruct stream-anchor-source
  (id3 (make-array 0 :element-type 'octet)
       :type octet-vector)
  (frame-start 0 :type fixnum)
  (frame-end 0 :type fixnum)
  (payload (make-array 0 :element-type 'octet)
           :type octet-vector)
  (generation-id 0 :type (unsigned-byte 64))
  (sequence 0 :type (unsigned-byte 32))
  (source-time 0 :type (unsigned-byte 64)))

(defstruct stream-anchor-pending-entry
  (packet (make-array +ts-packet-size+
                      :element-type 'octet)
          :type octet-vector)
  (resolved-p t :type boolean)
  (use-original-p t :type boolean)
  (replacements '() :type list)
  (duplicate-entries '() :type list)
  (next nil))

(defstruct stream-anchor-video-point
  (pts 0 :type (unsigned-byte 33))
  (unwrapped-pts 0 :type integer))

(defstruct stream-anchor-marker
  (source (make-stream-anchor-source)
          :type stream-anchor-source)
  (pes-header (make-pes-header) :type pes-header)
  (pts 0 :type (unsigned-byte 33))
  (unwrapped-pts nil :type (or null integer))
  (entries '() :type list))

(defstruct (stream-anchor-id3-assembler
            (:constructor %make-stream-anchor-id3-assembler (pid)))
  (pid 0 :type (unsigned-byte 13) :read-only t)
  (active-p nil :type boolean)
  (entries '() :type list)
  (buffer
    (make-array 0
                :element-type 'octet
                :adjustable t
                :fill-pointer 0)
    :type (vector octet))
  (expected-length nil :type (or null fixnum)))

(defstruct stream-anchor-video-header-state
  (active-p nil :type boolean)
  (buffer
    (make-array 0
                :element-type 'octet
                :adjustable t
                :fill-pointer 0)
    :type (vector octet)))

(defstruct (stream-anchor-finalizer
            (:constructor %make-stream-anchor-finalizer
                (output requested-program-number maximum-distance-ticks)))
  (output *standard-output* :type stream :read-only t)
  (requested-program-number nil
                            :type (or null (integer 1 65535))
                            :read-only t)
  (maximum-distance-ticks
    +stream-anchor-default-maximum-distance-ticks+
    :type (integer 0 *)
    :read-only t)
  (transport-integrity-validator
    (make-payload-continuity-validator)
    :type payload-continuity-validator
    :read-only t)
  (last-payload-entries
    (make-array +ts-pid-count+ :initial-element nil)
    :type simple-vector
    :read-only t)
  (pending-head nil)
  (pending-tail nil)
  (pending-count 0 :type fixnum)
  (pat-assembler (make-section-assembler 0)
                 :type section-assembler)
  (pmt-assembler nil :type (or null section-assembler))
  (pmt-pid nil :type (or null (unsigned-byte 13)))
  (program-number nil :type (or null (unsigned-byte 16)))
  (video-pid nil :type (or null (unsigned-byte 13)))
  (timed-id3-pids '() :type list)
  (id3-assemblers (make-hash-table :test #'eql)
                  :type hash-table)
  (video-header-state (make-stream-anchor-video-header-state)
                      :type stream-anchor-video-header-state)
  (video-points '() :type list)
  (pending-markers '() :type list)
  (last-video-ordering-timestamp nil
                                 :type (or null (unsigned-byte 33)))
  (last-video-unwrapped-ordering-timestamp nil
                                           :type (or null integer))
  (last-generation-id nil :type (or null (unsigned-byte 64)))
  (last-sequence nil :type (or null (unsigned-byte 32)))
  (seen-pmt-p nil :type boolean)
  (seen-video-p nil :type boolean)
  (eof-p nil :type boolean))

(defclass stream-anchor-output-stream
    (sb-gray:fundamental-binary-output-stream)
  ((finalizer
    :initarg :finalizer
    :reader stream-anchor-output-finalizer)
   (packet-buffer
    :initform
    (make-array +ts-packet-size+
                :element-type 'octet
                :adjustable t
                :fill-pointer 0)
    :reader stream-anchor-output-packet-buffer)))

(defmethod stream-element-type
    ((stream stream-anchor-output-stream))
  (declare (ignore stream))
  'octet)

(defun read-stream-anchor-u64 (octets offset)
  "OCTETSのOFFSETからnetwork byte orderの64 bit整数を読む。"
  (ensure-octet-range octets offset 8 :read-stream-anchor-u64)
  (let ((value 0))
    (loop for position from offset below (+ offset 8)
          do (setf value
                   (logior
                    (ash value 8)
                    (aref octets position))))
    value))

(defun write-stream-anchor-u64 (value octets offset)
  "VALUEをnetwork byte orderの64 bit整数としてOCTETSへ書く。"
  (unless (typep value '(unsigned-byte 64))
    (bridge-error "Stream Anchor value does not fit in 64 bits: ~D"
                  value))
  (ensure-octet-range octets offset 8 :write-stream-anchor-u64)
  (loop for position from 0 below 8
        do (setf (aref octets (+ offset position))
                 (ldb (byte 8 (* (- 7 position) 8)) value)))
  octets)

(defun decode-id3-synchsafe-u32 (octets offset)
  "ID3の4 byte synchsafe整数を厳格に読む。"
  (ensure-octet-range octets offset 4 :decode-id3-synchsafe-u32)
  (let ((value 0))
    (loop for position from offset below (+ offset 4)
          for byte = (aref octets position)
          do
             (when (logbitp 7 byte)
               (bridge-error
                "STREAM_ANCHOR_ID3_SYNCHSAFE_INVALID offset=~D"
                position))
             (setf value (logior (ash value 7) byte)))
    value))

(defun encode-id3-synchsafe-u32 (value)
  "VALUEを4 byte ID3 synchsafe整数へ変換する。"
  (unless (typep value '(unsigned-byte 28))
    (bridge-error
     "STREAM_ANCHOR_ID3_SIZE_OUT_OF_RANGE value=~D"
     value))
  (let ((result (make-array 4 :element-type 'octet)))
    (setf (aref result 0) (ldb (byte 7 21) value)
          (aref result 1) (ldb (byte 7 14) value)
          (aref result 2) (ldb (byte 7 7) value)
          (aref result 3) (ldb (byte 7 0) value))
    result))

(defun stream-anchor-owner-at-p
    (octets start end owner)
  "OCTETSのSTARTからENDがOWNERとNULだけなら真を返す。"
  (and (= (- end start) (+ (length owner) 1))
       (equalp (subseq octets start (+ start (length owner)))
               owner)
       (zerop (aref octets (+ start (length owner))))))

(defun parse-stream-anchor-source-id3 (id3)
  "source ownerを持つID3v2.4を厳格に解析する。
source ownerを含まないID3はNILを返し、他ownerの内容には介入しない。"
  (unless (search +stream-anchor-source-owner+ id3)
    (return-from parse-stream-anchor-source-id3 nil))
  (unless (and (>= (length id3) 10)
               (equalp (subseq id3 0 3)
                       +id3-signature+))
    (bridge-error "STREAM_ANCHOR_ID3_HEADER_INVALID"))
  (unless (and (= (aref id3 3) 4)
               (= (aref id3 4) 0)
               (= (aref id3 5) 0))
    (bridge-error
     "STREAM_ANCHOR_ID3_VERSION_OR_FLAGS_INVALID version=~D revision=~D flags=0x~2,'0X"
     (aref id3 3) (aref id3 4) (aref id3 5)))
  (let* ((tag-size (decode-id3-synchsafe-u32 id3 6))
         (tag-end (+ 10 tag-size))
         (position 10)
         (source nil))
    (unless (= tag-end (length id3))
      (bridge-error
       "STREAM_ANCHOR_ID3_SIZE_MISMATCH declared=~D actual=~D"
       tag-size (- (length id3) 10)))
    (loop while (< position tag-end)
          do
             (when (zerop (aref id3 position))
               (unless
                   (loop for index from position below tag-end
                         always (zerop (aref id3 index)))
                 (bridge-error
                  "STREAM_ANCHOR_ID3_PADDING_INVALID offset=~D"
                  position))
               (setf position tag-end)
               (loop-finish))
             (when (> (+ position 10) tag-end)
               (bridge-error
                "STREAM_ANCHOR_ID3_FRAME_HEADER_TRUNCATED offset=~D"
                position))
             (let* ((frame-start position)
                    (frame-size
                      (decode-id3-synchsafe-u32 id3 (+ position 4)))
                    (payload-start (+ position 10))
                    (frame-end (+ payload-start frame-size)))
               (when (> frame-end tag-end)
                 (bridge-error
                  "STREAM_ANCHOR_ID3_FRAME_TRUNCATED offset=~D size=~D"
                  position frame-size))
               (when
                   (equalp
                    (subseq id3 position (+ position 4))
                    +id3-priv-frame-identifier+)
                 (let ((owner-end
                         (position 0 id3
                                   :start payload-start
                                   :end frame-end)))
                   (when
                       (and owner-end
                            (stream-anchor-owner-at-p
                             id3 payload-start (+ owner-end 1)
                             +stream-anchor-source-owner+))
                     (when source
                       (bridge-error
                        "STREAM_ANCHOR_ID3_SOURCE_FRAME_DUPLICATED"))
                     (unless (and
                              (zerop (aref id3 (+ position 8)))
                              (zerop (aref id3 (+ position 9))))
                       (bridge-error
                        "STREAM_ANCHOR_ID3_PRIV_FLAGS_INVALID"))
                     (let* ((anchor-start (+ owner-end 1))
                            (anchor-end (+ anchor-start
                                           +stream-anchor-payload-byte-count+)))
                       (unless (= anchor-end frame-end)
                         (bridge-error
                          "STREAM_ANCHOR_PAYLOAD_SIZE_INVALID actual=~D"
                          (- frame-end anchor-start)))
                       (let ((payload
                               (subseq id3 anchor-start anchor-end)))
                         (unless (= (aref payload 0)
                                    +stream-anchor-version+)
                           (bridge-error
                            "STREAM_ANCHOR_VERSION_INVALID actual=~D"
                            (aref payload 0)))
                         (unless (zerop (aref payload 1))
                           (bridge-error
                            "STREAM_ANCHOR_FLAGS_INVALID actual=0x~2,'0X"
                            (aref payload 1)))
                         (unless (and (zerop (aref payload 2))
                                      (zerop (aref payload 3)))
                           (bridge-error
                            "STREAM_ANCHOR_RESERVED_INVALID"))
                         (setf
                          source
                          (make-stream-anchor-source
                           :id3 (copy-seq id3)
                           :frame-start frame-start
                           :frame-end frame-end
                           :payload payload
                           :generation-id
                           (read-stream-anchor-u64 payload 4)
                           :sequence (read-u32-be payload 12)
                           :source-time
                           (read-stream-anchor-u64 payload 16))))))))
               (setf position frame-end)))
    (unless source
      (bridge-error
       "STREAM_ANCHOR_SOURCE_OWNER_IS_NOT_A_VALID_PRIV_FRAME"))
    source))

(defun build-final-stream-anchor-id3 (source source-time)
  "SOURCEをfinal ownerへ変換しSOURCE-TIMEをtarget AU時刻へ補正する。"
  (let* ((payload
           (copy-seq (stream-anchor-source-payload source)))
         (priv-payload
           (concatenate-octet-vectors
            +stream-anchor-final-owner+
            (make-array 1
                        :element-type 'octet
                        :initial-element 0)
            payload))
         (frame
           (concatenate-octet-vectors
            +id3-priv-frame-identifier+
            (encode-id3-synchsafe-u32 (length priv-payload))
            (make-array 2
                        :element-type 'octet
                        :initial-element 0)
            priv-payload))
         (id3
           (concatenate-octet-vectors
            (subseq (stream-anchor-source-id3 source)
                    0
                    (stream-anchor-source-frame-start source))
            frame
            (subseq (stream-anchor-source-id3 source)
                    (stream-anchor-source-frame-end source))))
         (tag-size (- (length id3) 10)))
    (write-stream-anchor-u64 source-time payload 16)
    ;; PAYLOAD更新後にPRIV payloadへ反映し直す。
    (replace id3 payload
             :start1
             (+ (stream-anchor-source-frame-start source)
                10
                (length +stream-anchor-final-owner+)
                1))
    (replace id3 (encode-id3-synchsafe-u32 tag-size)
             :start1 6)
    id3))

(defun stream-anchor-signed-timestamp-delta (from to)
  "33 bit FROMからTOへの符号付き最短距離を90kHz tickで返す。"
  (let ((forward (mod (- to from) +pts-modulus+)))
    (if (>= forward +pts-half-modulus+)
        (- forward +pts-modulus+)
        forward)))

(defun append-stream-anchor-pending-entry (finalizer packet)
  "FINALIZERの出力待ちqueueへPACKETを追加する。"
  (let ((entry
          (make-stream-anchor-pending-entry :packet packet)))
    (if (stream-anchor-finalizer-pending-tail finalizer)
        (setf
         (stream-anchor-pending-entry-next
          (stream-anchor-finalizer-pending-tail finalizer))
         entry)
        (setf (stream-anchor-finalizer-pending-head finalizer)
              entry))
    (setf (stream-anchor-finalizer-pending-tail finalizer) entry)
    (incf (stream-anchor-finalizer-pending-count finalizer))
    (when (> (stream-anchor-finalizer-pending-count finalizer)
             +stream-anchor-maximum-pending-packet-count+)
      (bridge-error
       "STREAM_ANCHOR_PENDING_PACKET_LIMIT_EXCEEDED limit=~D"
       +stream-anchor-maximum-pending-packet-count+))
    entry))

(defun mark-stream-anchor-entry-unresolved (entry)
  "ENTRYをAnchor解決待ちにする。"
  (setf (stream-anchor-pending-entry-resolved-p entry) nil
        (stream-anchor-pending-entry-use-original-p entry) nil
        (stream-anchor-pending-entry-replacements entry) '())
  entry)

(defun resolve-stream-anchor-entry-original (entry)
  "ENTRYをbyte-exactの元packetで解決する。"
  (setf (stream-anchor-pending-entry-resolved-p entry) t
        (stream-anchor-pending-entry-use-original-p entry) t
        (stream-anchor-pending-entry-replacements entry) '())
  (dolist
      (duplicate
       (stream-anchor-pending-entry-duplicate-entries entry))
    (resolve-stream-anchor-entry-original duplicate))
  (setf (stream-anchor-pending-entry-duplicate-entries entry) '())
  entry)

(defun resolve-stream-anchor-entry-replacements
    (entry replacements)
  "ENTRYをREPLACEMENTSで解決する。"
  (setf (stream-anchor-pending-entry-resolved-p entry) t
        (stream-anchor-pending-entry-use-original-p entry) nil
        (stream-anchor-pending-entry-replacements entry)
        replacements)
  (dolist
      (duplicate
       (stream-anchor-pending-entry-duplicate-entries entry))
    (resolve-stream-anchor-entry-replacements
     duplicate
     (mapcar #'copy-seq replacements)))
  (setf (stream-anchor-pending-entry-duplicate-entries entry) '())
  entry)

(defun link-stream-anchor-duplicate-entry
    (original duplicate)
  "DUPLICATEをORIGINALと同じ最終packetへ解決する。"
  (cond
    ((not (stream-anchor-pending-entry-resolved-p original))
     (mark-stream-anchor-entry-unresolved duplicate)
     (push
      duplicate
      (stream-anchor-pending-entry-duplicate-entries original)))
    ((stream-anchor-pending-entry-use-original-p original)
     (resolve-stream-anchor-entry-original duplicate))
    (t
     (resolve-stream-anchor-entry-replacements
      duplicate
      (mapcar
       #'copy-seq
       (stream-anchor-pending-entry-replacements original)))))
  duplicate)

(defun flush-stream-anchor-pending-entries (finalizer)
  "解決済みqueueを先頭から実出力する。"
  (loop
    for entry = (stream-anchor-finalizer-pending-head finalizer)
    while (and entry
               (stream-anchor-pending-entry-resolved-p entry))
    do
       (let ((next
               (stream-anchor-pending-entry-next entry)))
         (if (stream-anchor-pending-entry-use-original-p entry)
             (%write-octets
              (stream-anchor-finalizer-output finalizer)
              (stream-anchor-pending-entry-packet entry)
              +ts-packet-size+)
             (dolist
                 (packet
                  (stream-anchor-pending-entry-replacements entry))
               (%write-octets
                (stream-anchor-finalizer-output finalizer)
                packet
                +ts-packet-size+)))
         (setf
          (stream-anchor-finalizer-pending-head finalizer) next
          (stream-anchor-pending-entry-next entry) nil)
         (decf (stream-anchor-finalizer-pending-count finalizer))
         (when (null (stream-anchor-finalizer-pending-head finalizer))
           (setf (stream-anchor-finalizer-pending-tail finalizer) nil))))
  nil)

(defun stream-anchor-video-registration-p (stream)
  "STREAMがBridgeのVP9/AV1 mappingを示すか返す。"
  (some
   (lambda (descriptor)
     (or (vp9-registration-descriptor-p descriptor)
         (av1-registration-descriptor-p descriptor)))
   (pmt-stream-descriptors stream)))

(defun stream-anchor-video-stream-p (stream)
  "STREAMがAnchor対象の標準/Bridge映像ESなら真を返す。"
  (or (member (pmt-stream-stream-type stream)
              '(#x01 #x02 #x1b #x24)
              :test #'=)
      (and (= (pmt-stream-stream-type stream) #x06)
           (stream-anchor-video-registration-p stream))))

(defun select-stream-anchor-program (finalizer table)
  "PATからFINALIZERが追跡するprogramを一意に選ぶ。"
  (let ((programs
           (remove 0
                   (program-association-table-programs table)
                   :key #'pat-program-program-number
                   :test #'=))
         (requested
           (stream-anchor-finalizer-requested-program-number
            finalizer)))
    (cond
      (requested
       (or (find requested programs
                 :key #'pat-program-program-number
                 :test #'=)
           (bridge-error
            "STREAM_ANCHOR_REQUESTED_PROGRAM_ABSENT program=~D"
            requested)))
      ((= (length programs) 1)
       (first programs))
      (t
       (bridge-error
        "STREAM_ANCHOR_PROGRAM_SELECTION_AMBIGUOUS count=~D"
        (length programs))))))

(defun process-stream-anchor-pat-packet (finalizer packet)
  "PATを追跡して対象PMT PIDを更新する。"
  (dolist
      (section
       (feed-section-packet
        (stream-anchor-finalizer-pat-assembler finalizer)
        packet))
    (let* ((table (parse-pat-section section))
           (program
             (select-stream-anchor-program finalizer table))
           (pmt-pid (pat-program-pid program))
           (program-number (pat-program-program-number program)))
      (unless (and
               (program-association-table-current-next-p table)
               (zerop
                (program-association-table-section-number table))
               (zerop
                (program-association-table-last-section-number table)))
        (bridge-error
         "STREAM_ANCHOR_PAT_NOT_CURRENT_SINGLE_SECTION"))
      (unless (and
               (eql pmt-pid
                    (stream-anchor-finalizer-pmt-pid finalizer))
               (eql program-number
                    (stream-anchor-finalizer-program-number finalizer)))
        (when
            (some #'stream-anchor-id3-assembler-active-p
                  (loop
                    for assembler being the hash-values
                      of (stream-anchor-finalizer-id3-assemblers
                          finalizer)
                    collect assembler))
          (bridge-error
           "STREAM_ANCHOR_PAT_CHANGE_DURING_TIMED_ID3_PES"))
        (when (stream-anchor-finalizer-pending-markers finalizer)
          (bridge-error
           "STREAM_ANCHOR_PAT_CHANGE_WITH_PENDING_MARKER"))
        (setf
         (stream-anchor-finalizer-pmt-pid finalizer) pmt-pid
         (stream-anchor-finalizer-program-number finalizer)
         program-number
         (stream-anchor-finalizer-pmt-assembler finalizer)
         (make-section-assembler pmt-pid)
         (stream-anchor-finalizer-video-pid finalizer) nil
         (stream-anchor-finalizer-timed-id3-pids finalizer) '()
         (stream-anchor-finalizer-id3-assemblers finalizer)
         (make-hash-table :test #'eql)
         (stream-anchor-finalizer-seen-pmt-p finalizer) nil)))))

(defun install-stream-anchor-pmt (finalizer table)
  "PMTから映像PIDとtimed-ID3 PID集合を確定する。"
  (unless (and
           (program-map-table-current-next-p table)
           (zerop (program-map-table-section-number table))
           (zerop (program-map-table-last-section-number table))
           (= (program-map-table-program-number table)
              (stream-anchor-finalizer-program-number finalizer)))
    (bridge-error
     "STREAM_ANCHOR_PMT_NOT_CURRENT_SELECTED_SINGLE_SECTION"))
  (let ((video-streams
          (remove-if-not
           #'stream-anchor-video-stream-p
           (program-map-table-streams table)))
        (timed-id3-pids
          (loop
            for stream in (program-map-table-streams table)
            when (= (pmt-stream-stream-type stream) #x15)
              collect (pmt-stream-elementary-pid stream))))
    (unless (= (length video-streams) 1)
      (bridge-error
       "STREAM_ANCHOR_VIDEO_SELECTION_INVALID count=~D"
       (length video-streams)))
    (when (null timed-id3-pids)
      (bridge-error "STREAM_ANCHOR_TIMED_ID3_STREAM_MISSING"))
    (let ((video-pid
            (pmt-stream-elementary-pid (first video-streams))))
      (when
          (and (stream-anchor-finalizer-video-pid finalizer)
               (/= video-pid
                   (stream-anchor-finalizer-video-pid finalizer))
               (stream-anchor-finalizer-pending-markers finalizer))
        (bridge-error
         "STREAM_ANCHOR_VIDEO_PID_CHANGE_WITH_PENDING_MARKER"))
      (setf
       (stream-anchor-finalizer-video-pid finalizer) video-pid
       (stream-anchor-finalizer-timed-id3-pids finalizer)
       timed-id3-pids
       (stream-anchor-finalizer-seen-pmt-p finalizer) t))))

(defun process-stream-anchor-pmt-packet (finalizer packet)
  "完成PMTを解析してAnchor対象PIDを更新する。"
  (let ((assembler
          (stream-anchor-finalizer-pmt-assembler finalizer)))
    (unless assembler
      (return-from process-stream-anchor-pmt-packet nil))
    (dolist (section (feed-section-packet assembler packet))
      (install-stream-anchor-pmt
       finalizer (parse-pmt-section section)))))

(defun append-packet-payload-to-vector (packet vector)
  "PACKETのpayloadをVECTORへ追加する。"
  (let ((offset (ts-payload-offset packet)))
    (when offset
      (loop for position from offset below +ts-packet-size+
            do (vector-push-extend
                (aref packet position) vector))))
  vector)

(defun trim-stream-anchor-id3-buffer (assembler)
  "宣言PES長を超えたTS stuffingを検証して切り詰める。"
  (let ((expected
          (stream-anchor-id3-assembler-expected-length assembler))
        (buffer
          (stream-anchor-id3-assembler-buffer assembler)))
    (when (and expected (> (length buffer) expected))
      (loop for position from expected below (length buffer)
            unless (= (aref buffer position) #xff)
              do (bridge-error
                  "STREAM_ANCHOR_PES_TRAILING_DATA offset=~D"
                  position))
      (setf (fill-pointer buffer) expected)))
  assembler)

(defun clear-stream-anchor-id3-assembler (assembler)
  "ASSEMBLERの現在PESだけを初期化する。"
  (setf
   (stream-anchor-id3-assembler-active-p assembler) nil
   (stream-anchor-id3-assembler-entries assembler) '()
   (fill-pointer
    (stream-anchor-id3-assembler-buffer assembler))
   0
   (stream-anchor-id3-assembler-expected-length assembler) nil)
  assembler)

(defun resolve-stream-anchor-id3-as-original (assembler)
  "ASSEMBLERのPESを非Anchorとしてbyte-exact解決する。"
  (dolist
      (entry
       (reverse
        (stream-anchor-id3-assembler-entries assembler)))
    (resolve-stream-anchor-entry-original entry))
  (clear-stream-anchor-id3-assembler assembler))

(defun register-stream-anchor-sequence (finalizer source)
  "generation内sequenceを厳格な増加列として検証する。"
  (let ((generation
          (stream-anchor-source-generation-id source))
        (sequence
          (stream-anchor-source-sequence source))
        (last-generation
          (stream-anchor-finalizer-last-generation-id finalizer))
        (last-sequence
          (stream-anchor-finalizer-last-sequence finalizer)))
    (when (and last-generation
               (= generation last-generation)
               last-sequence
               (<= sequence last-sequence))
      (bridge-error
       "STREAM_ANCHOR_SEQUENCE_ROLLBACK generation=~D previous=~D actual=~D"
       generation last-sequence sequence))
    (setf
     (stream-anchor-finalizer-last-generation-id finalizer)
     generation
     (stream-anchor-finalizer-last-sequence finalizer)
     sequence))
  source)

(defun make-stream-anchor-replacement-packets (pes entries)
  "ENTRIESと同じpayload packet数/CCでPESを再packetizeする。"
  (let* ((templates
           (mapcar #'stream-anchor-pending-entry-packet entries))
         (packet-count (length templates))
         (remaining (length pes))
         (offset 0)
         (packets '()))
    (when (< remaining packet-count)
      (bridge-error
       "STREAM_ANCHOR_REPACKETIZE_PACKET_COUNT_UNPRESERVABLE packets=~D bytes=~D"
       packet-count remaining))
    (loop
      for template in templates
      for index from 0
      for packets-left = (- packet-count index 1)
      for capacity = (template-payload-capacity
                      template
                      (ts-adaptation-flags template))
      for count = (min capacity (- remaining packets-left))
      do
         (when (not (plusp count))
           (bridge-error
            "STREAM_ANCHOR_REPACKETIZE_CAPACITY_EXHAUSTED index=~D"
            index))
         (push
          (make-payload-packet-from-template
           template
           (subseq pes offset (+ offset count))
           (ts-continuity-counter template)
           :payload-unit-start (zerop index))
          packets)
         (incf offset count)
         (decf remaining count))
    (unless (zerop remaining)
      (bridge-error
       "STREAM_ANCHOR_REPACKETIZE_CAPACITY_INSUFFICIENT remaining=~D"
       remaining))
    (nreverse packets)))

(defun finalize-stream-anchor-marker (marker point)
  "MARKERをPOINTのencoded AU PTSへ確定してpending entryを解決する。"
  (let* ((source
           (stream-anchor-marker-source marker))
         (delta
           (- (stream-anchor-video-point-unwrapped-pts point)
              (stream-anchor-marker-unwrapped-pts marker)))
         (source-time
           (+ (stream-anchor-source-source-time source) delta)))
    (unless (typep source-time '(unsigned-byte 64))
      (bridge-error
       "STREAM_ANCHOR_SOURCE_TIME_OUT_OF_RANGE value=~D"
       source-time))
    (let* ((id3
             (build-final-stream-anchor-id3 source source-time))
           (pes
             (make-pes
              #xbd id3
              (stream-anchor-video-point-pts point)
              :data-alignment
              (pes-header-data-alignment-p
               (stream-anchor-marker-pes-header marker))))
           (entries (stream-anchor-marker-entries marker))
           (replacement
             (make-stream-anchor-replacement-packets pes entries)))
      ;; 各replacementを元のTS slotへ戻し、別PIDとのinterleaveを維持する。
      (loop for entry in entries
            for packet in replacement
            do
               (resolve-stream-anchor-entry-replacements
                entry (list packet)))))
  marker)

(defun stream-anchor-point-better-p
    (candidate current marker-unwrapped)
  "CANDIDATEがCURRENTより近いか返す。同距離なら過去側を優先する。"
  (let ((candidate-delta
          (- (stream-anchor-video-point-unwrapped-pts candidate)
             marker-unwrapped))
        (current-delta
          (- (stream-anchor-video-point-unwrapped-pts current)
             marker-unwrapped)))
    (or (< (abs candidate-delta) (abs current-delta))
        (and (= (abs candidate-delta) (abs current-delta))
             (< candidate-delta current-delta)))))

(defun choose-stream-anchor-video-point (finalizer marker)
  "MARKERに最も近い上限内のvideo pointを返す。"
  (let ((best nil)
        (marker-unwrapped
          (stream-anchor-marker-unwrapped-pts marker))
        (limit
          (stream-anchor-finalizer-maximum-distance-ticks
           finalizer)))
    (dolist (point (stream-anchor-finalizer-video-points finalizer))
      (let ((distance
              (abs
               (- (stream-anchor-video-point-unwrapped-pts point)
                  marker-unwrapped))))
        (when (and
               (<= distance limit)
               (or (null best)
                   (stream-anchor-point-better-p
                    point best marker-unwrapped)))
          (setf best point))))
    best))

(defun assign-stream-anchor-marker-epoch (finalizer marker)
  "MARKERの33 bit PTSを現在のvideo ordering epochへ展開する。"
  (when (and
         (null (stream-anchor-marker-unwrapped-pts marker))
         (stream-anchor-finalizer-last-video-ordering-timestamp
          finalizer))
    (setf
     (stream-anchor-marker-unwrapped-pts marker)
     (+
      (stream-anchor-finalizer-last-video-unwrapped-ordering-timestamp
       finalizer)
      (stream-anchor-signed-timestamp-delta
       (stream-anchor-finalizer-last-video-ordering-timestamp finalizer)
       (stream-anchor-marker-pts marker)))))
  marker)

(defun resolve-ready-stream-anchor-markers
    (finalizer &key force)
  "exactを即時優先し、ordering watermark通過後だけ有界nearestを確定する。"
  (let ((remaining '())
        (watermark
          (stream-anchor-finalizer-last-video-unwrapped-ordering-timestamp
           finalizer))
        (limit
          (stream-anchor-finalizer-maximum-distance-ticks
           finalizer)))
    (dolist (marker
             (stream-anchor-finalizer-pending-markers finalizer))
      (assign-stream-anchor-marker-epoch finalizer marker)
      (let* ((marker-unwrapped
               (stream-anchor-marker-unwrapped-pts marker))
             (point
               (and marker-unwrapped
                    (choose-stream-anchor-video-point
                     finalizer marker)))
             (exact-p
               (and point
                    (zerop
                     (- (stream-anchor-video-point-unwrapped-pts point)
                        marker-unwrapped))))
             (ready-p
               (or force
                   exact-p
                   (and watermark
                        marker-unwrapped
                        (> watermark
                           (+ marker-unwrapped limit))))))
        (cond
          ((and ready-p point)
           (finalize-stream-anchor-marker marker point))
          (ready-p
           (bridge-error
            "STREAM_ANCHOR_VIDEO_MATCH_EXCEEDED generation=~D sequence=~D limit_ticks=~D"
            (stream-anchor-source-generation-id
             (stream-anchor-marker-source marker))
            (stream-anchor-source-sequence
             (stream-anchor-marker-source marker))
            limit))
          (t
           (push marker remaining)))))
    (setf (stream-anchor-finalizer-pending-markers finalizer)
          (nreverse remaining)))
  (flush-stream-anchor-pending-entries finalizer))

(defun prune-stream-anchor-video-points (finalizer)
  "時刻と個数の明示上限を超えた古いvideo pointを破棄する。"
  (let ((watermark
          (stream-anchor-finalizer-last-video-unwrapped-ordering-timestamp
           finalizer)))
    (when watermark
      (setf
       (stream-anchor-finalizer-video-points finalizer)
       (loop
         for point in
           (stream-anchor-finalizer-video-points finalizer)
         for index from 0
         when (and
               (< index +stream-anchor-maximum-video-point-count+)
               (>=
                (stream-anchor-video-point-unwrapped-pts point)
                (- watermark
                   +stream-anchor-maximum-video-history-ticks+)))
           collect point))))
  finalizer)

(defun record-stream-anchor-video-header (finalizer header)
  "encoded video PESのPTSを候補点、DTS優先時刻をwatermarkとして記録する。"
  (let ((pts (pes-header-pts header))
        (ordering
          (or (pes-header-dts header)
              (pes-header-pts header))))
    (unless (and pts ordering)
      (bridge-error "STREAM_ANCHOR_VIDEO_PTS_MISSING"))
    (let* ((last-raw
             (stream-anchor-finalizer-last-video-ordering-timestamp
              finalizer))
           (last-unwrapped
             (stream-anchor-finalizer-last-video-unwrapped-ordering-timestamp
              finalizer))
           (ordering-unwrapped
             (if last-raw
                 (let ((delta
                         (stream-anchor-signed-timestamp-delta
                          last-raw ordering)))
                   (when (minusp delta)
                     (bridge-error
                      "STREAM_ANCHOR_VIDEO_ORDERING_TIME_ROLLBACK previous=~D actual=~D"
                      last-raw ordering))
                   (+ last-unwrapped delta))
                 ordering))
           (pts-unwrapped
             (+ ordering-unwrapped
                (stream-anchor-signed-timestamp-delta
                 ordering pts))))
      (setf
       (stream-anchor-finalizer-last-video-ordering-timestamp finalizer)
       ordering
       (stream-anchor-finalizer-last-video-unwrapped-ordering-timestamp
        finalizer)
       ordering-unwrapped
       (stream-anchor-finalizer-video-points finalizer)
       (cons
        (make-stream-anchor-video-point
         :pts pts
         :unwrapped-pts pts-unwrapped)
        (stream-anchor-finalizer-video-points finalizer))
       (stream-anchor-finalizer-seen-video-p finalizer) t)
      (prune-stream-anchor-video-points finalizer)
      (resolve-ready-stream-anchor-markers finalizer)))
  header)

(defun parse-stream-anchor-video-header-if-ready
    (finalizer state)
  "STATEに完全なPES headerがあればPTS/DTSを記録する。"
  (let ((buffer (stream-anchor-video-header-state-buffer state)))
    (unless (>= (length buffer) 9)
      (return-from parse-stream-anchor-video-header-if-ready nil))
    (let ((header-end (+ 9 (aref buffer 8))))
      (unless (>= (length buffer) header-end)
        (return-from parse-stream-anchor-video-header-if-ready nil))
      (let ((prefix (subseq buffer 0 header-end)))
        (setf (aref prefix 4) 0
              (aref prefix 5) 0)
        (let ((header (parse-pes-header prefix)))
          (unless (or
                   (<= #xe0 (pes-header-stream-id header) #xef)
                   (= (pes-header-stream-id header) #xbd))
            (bridge-error
             "STREAM_ANCHOR_VIDEO_STREAM_ID_INVALID actual=0x~2,'0X"
             (pes-header-stream-id header)))
          (record-stream-anchor-video-header finalizer header)
          (setf (stream-anchor-video-header-state-active-p state) nil
                (fill-pointer buffer) 0)
          t)))))

(defun process-stream-anchor-video-packet
    (finalizer packet)
  "映像PESのheaderだけを逐次復元しencoded AU PTSを記録する。"
  (let ((state
          (stream-anchor-finalizer-video-header-state finalizer)))
    (when (ts-payload-unit-start-p packet)
      (when (stream-anchor-video-header-state-active-p state)
        (bridge-error
         "STREAM_ANCHOR_VIDEO_PES_HEADER_TRUNCATED"))
      (unless (ts-has-payload-p packet)
        (bridge-error
         "STREAM_ANCHOR_VIDEO_PES_START_WITHOUT_PAYLOAD"))
      (setf (stream-anchor-video-header-state-active-p state) t
            (fill-pointer
             (stream-anchor-video-header-state-buffer state))
            0))
    (when (and
           (stream-anchor-video-header-state-active-p state)
           (ts-has-payload-p packet))
      (append-packet-payload-to-vector
       packet (stream-anchor-video-header-state-buffer state))
      (parse-stream-anchor-video-header-if-ready finalizer state))))

(defun finish-stream-anchor-id3-pes
    (finalizer assembler)
  "完成timed-ID3 PESをsource markerか非対象かに分類する。"
  (unless
      (search
       +stream-anchor-source-owner+
       (stream-anchor-id3-assembler-buffer assembler))
    (resolve-stream-anchor-id3-as-original assembler)
    (return-from finish-stream-anchor-id3-pes nil))
  (trim-stream-anchor-id3-buffer assembler)
  (let* ((pes
           (coerce
            (stream-anchor-id3-assembler-buffer assembler)
            '(simple-array (unsigned-byte 8) (*))))
         (entries
           (reverse
            (stream-anchor-id3-assembler-entries assembler)))
         (header (parse-pes-header pes)))
    (unless (= (pes-header-stream-id header) #xbd)
      (resolve-stream-anchor-id3-as-original assembler)
      (return-from finish-stream-anchor-id3-pes nil))
    (let* ((id3
             (subseq pes (pes-header-payload-offset header)))
           (source (parse-stream-anchor-source-id3 id3)))
      (cond
        ((null source)
         (dolist (entry entries)
           (resolve-stream-anchor-entry-original entry))
         (clear-stream-anchor-id3-assembler assembler))
        (t
         (unless (and
                  (pes-header-pts header)
                  (null (pes-header-dts header)))
           (bridge-error
            "STREAM_ANCHOR_SOURCE_PES_TIMESTAMP_INVALID"))
         (register-stream-anchor-sequence finalizer source)
         (setf
          (stream-anchor-finalizer-pending-markers finalizer)
          (nconc
           (stream-anchor-finalizer-pending-markers finalizer)
           (list
            (make-stream-anchor-marker
             :source source
             :pes-header header
             :pts (pes-header-pts header)
             :entries entries))))
         (clear-stream-anchor-id3-assembler assembler)
         (resolve-ready-stream-anchor-markers finalizer)))))
  t)

(defun process-stream-anchor-id3-packet
    (finalizer entry packet)
  "timed-ID3 PESを保留しsource ownerだけをAnchor変換対象にする。"
  (let* ((pid (ts-pid packet))
         (assembler
           (or
            (gethash
             pid
             (stream-anchor-finalizer-id3-assemblers finalizer))
            (setf
             (gethash
              pid
              (stream-anchor-finalizer-id3-assemblers finalizer))
             (%make-stream-anchor-id3-assembler pid)))))
    (when (ts-payload-unit-start-p packet)
      (when (stream-anchor-id3-assembler-active-p assembler)
        ;; 非宣言長の他owner PESは変更せず、次PUSIを境界として解決する。
        (if (search
             +stream-anchor-source-owner+
             (stream-anchor-id3-assembler-buffer assembler))
            (bridge-error
             "STREAM_ANCHOR_TIMED_ID3_PES_TRUNCATED pid=0x~4,'0X"
             pid)
            (resolve-stream-anchor-id3-as-original assembler)))
      (unless (ts-has-payload-p packet)
        (bridge-error
         "STREAM_ANCHOR_TIMED_ID3_START_WITHOUT_PAYLOAD pid=0x~4,'0X"
         pid))
      (setf (stream-anchor-id3-assembler-active-p assembler) t))
    (unless (stream-anchor-id3-assembler-active-p assembler)
      (return-from process-stream-anchor-id3-packet nil))
    ;; adaptation-only packet は PES byte 数にも payload CC 列にも参加しない。
    ;; 元 slot のまま即時解決し、payload template 一覧へ混ぜない。
    (unless (ts-has-payload-p packet)
      (resolve-stream-anchor-entry-original entry)
      (return-from process-stream-anchor-id3-packet t))
    (mark-stream-anchor-entry-unresolved entry)
    (push entry (stream-anchor-id3-assembler-entries assembler))
    (append-packet-payload-to-vector
     packet (stream-anchor-id3-assembler-buffer assembler))
    (when (and
           (null
            (stream-anchor-id3-assembler-expected-length
             assembler))
           (>=
            (length
             (stream-anchor-id3-assembler-buffer assembler))
            6))
      (let ((declared
              (read-u16-be
               (stream-anchor-id3-assembler-buffer assembler)
               4)))
        (when (plusp declared)
          (setf
           (stream-anchor-id3-assembler-expected-length assembler)
           (+ declared 6)))))
    (let ((expected
            (stream-anchor-id3-assembler-expected-length
             assembler)))
      (when (and
             expected
             (>=
              (length
               (stream-anchor-id3-assembler-buffer assembler))
              expected))
        (finish-stream-anchor-id3-pes
         finalizer assembler))))
  t)

(defun process-stream-anchor-packet (finalizer packet)
  "FINALIZERへBridge出力TS packetを1個与える。"
  (validate-ts-packet packet)
  (let ((integrity-result
          (validate-ts-packet-integrity
           (stream-anchor-finalizer-transport-integrity-validator
            finalizer)
           packet))
        (pid (ts-pid packet))
         (entry
           (append-stream-anchor-pending-entry finalizer packet)))
    (cond
      ((eq integrity-result :duplicate)
       (let ((original
               (aref
                (stream-anchor-finalizer-last-payload-entries
                 finalizer)
                pid)))
         (unless original
           (bridge-error
            "STREAM_ANCHOR_DUPLICATE_WITHOUT_ORIGINAL pid=0x~4,'0X"
            pid))
         (link-stream-anchor-duplicate-entry original entry)))
      (t
       (when (ts-has-payload-p packet)
         (setf
          (aref
           (stream-anchor-finalizer-last-payload-entries
            finalizer)
           pid)
          entry))
       (cond
         ((zerop pid)
          (process-stream-anchor-pat-packet finalizer packet))
         ((and
           (stream-anchor-finalizer-pmt-pid finalizer)
           (= pid (stream-anchor-finalizer-pmt-pid finalizer)))
          (process-stream-anchor-pmt-packet finalizer packet))
         ((and
           (stream-anchor-finalizer-video-pid finalizer)
           (= pid (stream-anchor-finalizer-video-pid finalizer)))
          (process-stream-anchor-video-packet finalizer packet))
         ((member
           pid
           (stream-anchor-finalizer-timed-id3-pids finalizer)
           :test #'=)
          (process-stream-anchor-id3-packet
           finalizer entry packet)))))
    (flush-stream-anchor-pending-entries finalizer))
  nil)

(defun make-stream-anchor-finalizer
    (output &key program-number
                 (maximum-distance-ticks
                   +stream-anchor-default-maximum-distance-ticks+))
  "Anchor v1 finalizerを作る。"
  (unless (typep maximum-distance-ticks '(integer 0 *))
    (bridge-error
     "STREAM_ANCHOR_MAXIMUM_DISTANCE_INVALID value=~S"
     maximum-distance-ticks))
  (%make-stream-anchor-finalizer
   output program-number maximum-distance-ticks))

(defun finish-stream-anchor-finalizer (finalizer)
  "EOFで未完状態を検査し、全markerをfail-closedで確定する。"
  (setf (stream-anchor-finalizer-eof-p finalizer) t)
  (when
      (plusp
       (length
        (section-assembler-buffer
         (stream-anchor-finalizer-pat-assembler finalizer))))
    (bridge-error "STREAM_ANCHOR_EOF_TRUNCATES_PAT"))
  (when (and
         (stream-anchor-finalizer-pmt-assembler finalizer)
         (plusp
          (length
           (section-assembler-buffer
            (stream-anchor-finalizer-pmt-assembler finalizer)))))
    (bridge-error "STREAM_ANCHOR_EOF_TRUNCATES_PMT"))
  (maphash
   (lambda (pid assembler)
     (declare (ignore pid))
     (when (stream-anchor-id3-assembler-active-p assembler)
       (let ((buffer
               (stream-anchor-id3-assembler-buffer assembler)))
         (if (search +stream-anchor-source-owner+ buffer)
             (bridge-error
              "STREAM_ANCHOR_EOF_TRUNCATES_SOURCE_PES")
             (resolve-stream-anchor-id3-as-original assembler)))))
   (stream-anchor-finalizer-id3-assemblers finalizer))
  (when
      (stream-anchor-video-header-state-active-p
       (stream-anchor-finalizer-video-header-state finalizer))
    (bridge-error "STREAM_ANCHOR_EOF_TRUNCATES_VIDEO_PES_HEADER"))
  (unless (stream-anchor-finalizer-seen-pmt-p finalizer)
    (bridge-error "STREAM_ANCHOR_EOF_BEFORE_PMT"))
  (unless (stream-anchor-finalizer-seen-video-p finalizer)
    (bridge-error "STREAM_ANCHOR_EOF_BEFORE_VIDEO_PTS"))
  (resolve-ready-stream-anchor-markers finalizer :force t)
  (flush-stream-anchor-pending-entries finalizer)
  (when (stream-anchor-finalizer-pending-head finalizer)
    (bridge-error
     "STREAM_ANCHOR_EOF_LEAVES_UNRESOLVED_PACKETS"))
  (handler-case
      (finish-output
       (stream-anchor-finalizer-output finalizer))
    (stream-error (cause)
      (error 'bridge-io-error
             :message
             (format nil "Stream Anchor output flush failed: ~A"
                     cause)
             :operation :flush
             :cause cause)))
  nil)

(defun feed-stream-anchor-output-octet (stream byte)
  "STREAMのpacket bufferへBYTEを追加し188 byte単位でfinalizerへ渡す。"
  (let ((buffer (stream-anchor-output-packet-buffer stream)))
    (vector-push-extend byte buffer)
    (when (= (length buffer) +ts-packet-size+)
      (process-stream-anchor-packet
       (stream-anchor-output-finalizer stream)
       (coerce buffer
               '(simple-array (unsigned-byte 8) (*))))
      (setf (fill-pointer buffer) 0)))
  byte)

(defmethod sb-gray:stream-write-byte
    ((stream stream-anchor-output-stream) byte)
  (feed-stream-anchor-output-octet stream byte))

(defmethod sb-gray:stream-write-sequence
    ((stream stream-anchor-output-stream) sequence
     &optional (start 0) end)
  (let ((actual-end (or end (length sequence))))
    (loop for position from start below actual-end
          do (feed-stream-anchor-output-octet
              stream (aref sequence position))))
  sequence)

(defun finish-stream-anchor-output-stream (stream)
  "Bridge出力wrapperのpacket境界とfinalizerを完了する。"
  (let ((buffer (stream-anchor-output-packet-buffer stream)))
    (when (plusp (length buffer))
      (bridge-error
       "STREAM_ANCHOR_EOF_TRUNCATES_TRANSPORT_PACKET bytes=~D"
       (length buffer))))
  (finish-stream-anchor-finalizer
   (stream-anchor-output-finalizer stream)))
