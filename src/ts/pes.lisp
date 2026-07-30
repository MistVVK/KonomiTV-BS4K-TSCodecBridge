;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defstruct pes-header
  (stream-id 0 :type octet)
  (packet-length 0 :type (unsigned-byte 16))
  (data-alignment-p nil :type boolean)
  (pts nil :type (or null (unsigned-byte 33)))
  (dts nil :type (or null (unsigned-byte 33)))
  (payload-offset 0 :type fixnum))

(defun validate-pes-timestamp-field (octets offset expected-prefix)
  "PES timestamp fieldのprefixとmarker bitを検証する。"
  (ensure-octet-range octets offset 5 :pes-timestamp)
  (unless (= (ldb (byte 4 4) (aref octets offset))
             expected-prefix)
    (bridge-error "PES timestamp prefix is invalid at offset ~D" offset))
  (unless (and (logbitp 0 (aref octets offset))
               (logbitp 0 (aref octets (+ offset 2)))
               (logbitp 0 (aref octets (+ offset 4))))
    (bridge-error "PES timestamp marker bit is missing at offset ~D"
                  offset)))

(defun decode-pes-timestamp (octets offset expected-prefix)
  "OCTETSのPES timestamp fieldから33 bit値を読む。"
  (validate-pes-timestamp-field octets offset expected-prefix)
  (logior (ash (ldb (byte 3 1) (aref octets offset)) 30)
          (ash (aref octets (+ offset 1)) 22)
          (ash (ldb (byte 7 1) (aref octets (+ offset 2))) 15)
          (ash (aref octets (+ offset 3)) 7)
          (ldb (byte 7 1) (aref octets (+ offset 4)))))

(defun encode-pes-timestamp (value prefix)
  "33 bit VALUEをPREFIX付きPES timestamp fieldへ変換する。"
  (unless (typep value '(unsigned-byte 33))
    (bridge-error "PES timestamp does not fit in 33 bits: ~D" value))
  (unless (typep prefix '(unsigned-byte 4))
    (bridge-error "PES timestamp prefix does not fit in 4 bits: ~D"
                  prefix))
  (let ((result (make-array 5 :element-type 'octet)))
    (setf (aref result 0)
          (logior (ash prefix 4)
                  (ash (ldb (byte 3 30) value) 1)
                  1)
          (aref result 1) (ldb (byte 8 22) value)
          (aref result 2)
          (logior (ash (ldb (byte 7 15) value) 1) 1)
          (aref result 3) (ldb (byte 8 7) value)
          (aref result 4)
          (logior (ash (ldb (byte 7 0) value) 1) 1))
    result))

(defconstant +maximum-pes-header-stuffing-byte-count+ 32)

(defun ensure-pes-optional-header-range
    (offset length header-end field)
  "PES optional header内にFIELDのLENGTH byteが収まる次位置を返す。"
  (let ((next (+ offset length)))
    (when (> next header-end)
      (bridge-error "PES ~A field is truncated" field))
    next))

(defun validate-pes-escr-field (pes offset header-end)
  "OFFSETのESCR fieldを検証して次位置を返す。"
  (let ((next
          (ensure-pes-optional-header-range
           offset 6 header-end "ESCR")))
    (unless (= (logand (aref pes offset) #xc0) #xc0)
      (bridge-error "PES ESCR reserved bits are invalid"))
    (unless (and
             (logbitp 2 (aref pes offset))
             (logbitp 2 (aref pes (+ offset 2)))
             (logbitp 2 (aref pes (+ offset 4)))
             (logbitp 0 (aref pes (+ offset 5))))
      (bridge-error "PES ESCR marker bit is missing"))
    (let ((extension
            (logior
             (ash (logand (aref pes (+ offset 4)) #x03) 7)
             (ash (aref pes (+ offset 5)) -1))))
      (when (> extension 299)
        (bridge-error "PES ESCR extension is invalid: ~D" extension)))
    next))

(defun validate-pes-es-rate-field (pes offset header-end)
  "OFFSETのES_rate fieldを検証して次位置を返す。"
  (let ((next
          (ensure-pes-optional-header-range
           offset 3 header-end "ES_rate")))
    (unless (and
             (logbitp 7 (aref pes offset))
             (logbitp 0 (aref pes (+ offset 2))))
      (bridge-error "PES ES_rate marker bit is missing"))
    (let ((rate
            (logior
             (ash (logand (aref pes offset) #x7f) 15)
             (ash (aref pes (+ offset 1)) 7)
             (ash (aref pes (+ offset 2)) -1))))
      (when (zerop rate)
        (bridge-error "PES ES_rate value zero is forbidden")))
    next))

(defun validate-pes-trick-mode-field (pes offset header-end)
  "OFFSETのDSM trick mode fieldを検証して次位置を返す。"
  (let* ((next
           (ensure-pes-optional-header-range
            offset 1 header-end "DSM trick mode"))
         (field (aref pes offset))
         (control (ldb (byte 3 5) field))
         (field-id (ldb (byte 2 3) field)))
    (case control
      ((0 3)
       (when (= field-id 3)
         (bridge-error "PES trick mode field_id value is reserved")))
      ((1 4)
       (when (zerop (logand field #x1f))
         (bridge-error "PES trick mode rep_cntrl value zero is forbidden")))
      (2
       (when (= field-id 3)
         (bridge-error "PES trick mode field_id value is reserved"))
       (unless (= (logand field #x07) #x07)
         (bridge-error "PES freeze-frame reserved bits are invalid")))
      (otherwise
       (bridge-error "PES trick_mode_control value is reserved: ~D"
                     control)))
    next))

(defun validate-pes-reserved-octets (pes start end field)
  "PESのSTARTからENDが0xFFの予約byteであることを検証する。"
  (loop for offset from start below end
        unless (= (aref pes offset) #xff)
          do (bridge-error "PES ~A reserved byte is invalid at offset ~D"
                           field offset)))

(defun validate-pes-extension-two-field
    (pes offset header-end stream-id)
  "OFFSETのPES extension field 2を検証し、次位置とextended構文使用有無を返す。"
  (let* ((payload-start
           (ensure-pes-optional-header-range
            offset 1 header-end "extension field 2 length"))
         (length-field (aref pes offset))
         (field-length (logand length-field #x7f))
         (payload-end
           (ensure-pes-optional-header-range
            payload-start field-length header-end "extension field 2")))
    (unless (logbitp 7 length-field)
      (bridge-error "PES extension field 2 marker bit is missing"))
    (when (zerop field-length)
      (bridge-error "PES extension field 2 is empty"))
    (let* ((extension (aref pes payload-start))
           (stream-id-extension-p (not (logbitp 7 extension)))
           (tref-p
             (and
              (not stream-id-extension-p)
              (not (logbitp 0 extension))))
           (required-end
             (cond
               (stream-id-extension-p
                (unless (= stream-id #xfd)
                  (bridge-error
                   "PES stream_id_extension requires stream_id 0xFD"))
                (+ payload-start 1))
               (tref-p
                (unless (= (logand extension #x7e) #x7e)
                  (bridge-error
                   "PES extension field 2 reserved bits are invalid"))
                (let ((end
                        (ensure-pes-optional-header-range
                         (+ payload-start 1)
                         5 payload-end "TREF")))
                  (validate-pes-timestamp-field
                   pes (+ payload-start 1) #x0f)
                  end))
               (t
                (unless (= (logand extension #x7e) #x7e)
                  (bridge-error
                   "PES extension field 2 reserved bits are invalid"))
                (+ payload-start 1)))))
      (validate-pes-reserved-octets
       pes required-end payload-end "extension field 2")
      (values payload-end stream-id-extension-p))))

(defun validate-pes-private-data-field (pes start header-end)
  "STARTの16-byte private dataと境界のstart code非模倣を検証する。"
  (let ((end
          (ensure-pes-optional-header-range
           start 16 header-end "private data")))
    (loop for offset from (max 0 (- start 2))
          below (min end (- (length pes) 2))
          when (= (read-u24-be pes offset) 1)
            do (bridge-error
                "PES private data emulates a packet start code at offset ~D"
                offset))
    end))

(defun validate-program-stream-system-header
    (pes start end mpeg-two-p program-mux-rate)
  "STARTからENDのprogram stream system headerを検証する。"
  (when (< (- end start) 12)
    (bridge-error "PES pack system header is truncated"))
  (unless (= (read-u32-be pes start) #x000001bb)
    (bridge-error "PES pack system header start code is invalid"))
  (let* ((header-length (read-u16-be pes (+ start 4)))
         (declared-end (+ start 6 header-length)))
    (unless (= declared-end end)
      (bridge-error
       "PES pack system header length mismatch: declared=~D actual=~D"
       header-length (- end start 6)))
    (when (< header-length 6)
      (bridge-error "PES pack system header fixed fields are truncated"))
    (unless (and
             (logbitp 7 (aref pes (+ start 6)))
             (logbitp 0 (aref pes (+ start 8)))
             (logbitp 5 (aref pes (+ start 10))))
      (bridge-error "PES pack system header marker bit is missing"))
    (let ((rate-bound
            (logior
             (ash (logand (aref pes (+ start 6)) #x7f) 15)
             (ash (aref pes (+ start 7)) 7)
             (ash (aref pes (+ start 8)) -1))))
      (when (< rate-bound program-mux-rate)
        (bridge-error
         "PES pack system rate_bound is below program_mux_rate")))
    (when (> (ldb (byte 6 2) (aref pes (+ start 9))) 32)
      (bridge-error "PES pack system header audio_bound exceeds 32"))
    (when (> (logand (aref pes (+ start 10)) #x1f) 16)
      (bridge-error "PES pack system header video_bound exceeds 16"))
    (if mpeg-two-p
        (unless (= (logand (aref pes (+ start 11)) #x7f) #x7f)
          (bridge-error
           "PES MPEG-2 pack system reserved bits are invalid"))
        (unless (= (aref pes (+ start 11)) #xff)
          (bridge-error
           "PES MPEG-1 pack system reserved byte is invalid")))
    (let ((position (+ start 12))
          (seen-streams (make-hash-table :test #'equal))
          (aggregate-audio-p nil)
          (aggregate-video-p nil)
          (aggregate-extended-p nil)
          (specific-audio-p nil)
          (specific-video-p nil)
          (specific-extended-p nil))
      (loop while (< position end)
            do (let ((stream-id (aref pes position)))
                 (cond
                   ((= stream-id #xb7)
                    (unless mpeg-two-p
                      (bridge-error
                       "PES MPEG-1 pack system has an extended stream bound"))
                    (ensure-pes-optional-header-range
                     position 6 end "pack extended stream bound")
                    (unless (and
                             (= (aref pes (+ position 1)) #xc0)
                             (zerop
                              (logand
                               (aref pes (+ position 2))
                               #x80))
                             (= (aref pes (+ position 3)) #xb6)
                             (= (logand
                                 (aref pes (+ position 4))
                                 #xc0)
                                #xc0))
                      (bridge-error
                       "PES pack extended stream bound syntax is invalid"))
                    (let* ((stream-id-extension
                             (logand
                              (aref pes (+ position 2))
                              #x7f))
                           (scale
                             (if
                                 (logbitp
                                  5
                                  (aref pes (+ position 4)))
                                 1
                                 0))
                           (key
                            (list
                             stream-id
                             stream-id-extension)))
                      (when (and
                             (<= #x10 stream-id-extension #x1f)
                             (zerop scale))
                        (bridge-error
                         "PES pack auxiliary video stream bound scale is invalid"))
                      (when aggregate-extended-p
                        (bridge-error
                         "PES pack extended stream bounds overlap"))
                      (setf specific-extended-p t)
                      (when (<= #x10 stream-id-extension #x1f)
                        (when aggregate-video-p
                          (bridge-error
                           "PES pack video stream bounds overlap"))
                        (setf specific-video-p t))
                      (when (gethash key seen-streams)
                        (bridge-error
                         "PES pack system header repeats a stream bound"))
                      (setf (gethash key seen-streams) t))
                    (incf position 6))
                   (t
                    (ensure-pes-optional-header-range
                     position 3 end "pack stream bound")
                    (unless (or (= stream-id #xb8)
                                (= stream-id #xb9)
                                (>= stream-id #xbc))
                      (bridge-error
                       "PES pack system header stream_id is invalid: 0x~2,'0X"
                       stream-id))
                    (unless (= (logand
                                (aref pes (+ position 1))
                                #xc0)
                               #xc0)
                      (bridge-error
                       "PES pack stream bound prefix is invalid"))
                    (let ((scale
                            (if
                                (logbitp
                                 5
                                 (aref pes (+ position 1)))
                                1
                                0)))
                      (when (and
                             (or (= stream-id #xb8)
                                 (<= #xc0 stream-id #xdf))
                             (plusp scale))
                        (bridge-error
                         "PES pack audio stream bound scale is invalid"))
                      (when (and
                             (or (= stream-id #xb9)
                                 (<= #xe0 stream-id #xef))
                             (zerop scale))
                        (bridge-error
                         "PES pack video stream bound scale is invalid")))
                    (cond
                      ((= stream-id #xb8)
                       (when specific-audio-p
                         (bridge-error
                          "PES pack audio stream bounds overlap"))
                       (setf aggregate-audio-p t))
                      ((= stream-id #xb9)
                       (when specific-video-p
                         (bridge-error
                          "PES pack video stream bounds overlap"))
                       (setf aggregate-video-p t))
                      ((= stream-id #xfd)
                       (when specific-extended-p
                         (bridge-error
                          "PES pack extended stream bounds overlap"))
                       (setf aggregate-extended-p t))
                      ((<= #xc0 stream-id #xdf)
                       (when aggregate-audio-p
                         (bridge-error
                          "PES pack audio stream bounds overlap"))
                       (setf specific-audio-p t))
                      ((<= #xe0 stream-id #xef)
                       (when aggregate-video-p
                         (bridge-error
                          "PES pack video stream bounds overlap"))
                       (setf specific-video-p t)))
                    (when (gethash stream-id seen-streams)
                      (bridge-error
                       "PES pack system header repeats a stream bound"))
                    (setf (gethash stream-id seen-streams) t)
                    (incf position 3))))))
    end))

(defun validate-program-stream-pack-header (pes start end)
  "STARTからENDのMPEG-1またはMPEG-2 pack headerを検証する。"
  (when (< (- end start) 12)
    (bridge-error "PES pack header is truncated"))
  (unless (= (read-u32-be pes start) #x000001ba)
    (bridge-error "PES pack header start code is invalid"))
  (multiple-value-bind
      (position mpeg-two-p program-mux-rate)
      (cond
            ((= (logand (aref pes (+ start 4)) #xc0) #x40)
             (ensure-pes-optional-header-range
              start 14 end "MPEG-2 pack header")
             (unless (and
                      (logbitp 2 (aref pes (+ start 4)))
                      (logbitp 2 (aref pes (+ start 6)))
                      (logbitp 2 (aref pes (+ start 8)))
                      (logbitp 0 (aref pes (+ start 9)))
                      (= (logand
                          (aref pes (+ start 12))
                          #x03)
                         #x03))
               (bridge-error "PES MPEG-2 pack marker bit is missing"))
             (let* ((scr-extension
                      (logior
                       (ash
                        (logand (aref pes (+ start 8)) #x03)
                        7)
                       (ash (aref pes (+ start 9)) -1)))
                    (program-mux-rate
                      (logior
                       (ash (aref pes (+ start 10)) 14)
                       (ash (aref pes (+ start 11)) 6)
                       (ash (aref pes (+ start 12)) -2)))
                    (stuffing-count
                      (logand (aref pes (+ start 13)) #x07))
                    (pack-end (+ start 14 stuffing-count)))
               (when (> scr-extension 299)
                 (bridge-error
                  "PES MPEG-2 pack SCR extension is invalid: ~D"
                  scr-extension))
               (when (zerop program-mux-rate)
                 (bridge-error
                  "PES MPEG-2 pack program_mux_rate zero is forbidden"))
               (unless (= (logand (aref pes (+ start 13)) #xf8) #xf8)
                 (bridge-error
                  "PES MPEG-2 pack reserved bits are invalid"))
               (when (> pack-end end)
                 (bridge-error "PES MPEG-2 pack stuffing is truncated"))
               (validate-pes-reserved-octets
                pes (+ start 14) pack-end "MPEG-2 pack stuffing")
               (values pack-end t program-mux-rate)))
            ((= (logand (aref pes (+ start 4)) #xf0) #x20)
             (unless (and
                      (logbitp 0 (aref pes (+ start 4)))
                      (logbitp 0 (aref pes (+ start 6)))
                      (logbitp 0 (aref pes (+ start 8)))
                      (logbitp 7 (aref pes (+ start 9)))
                      (logbitp 0 (aref pes (+ start 11))))
               (bridge-error "PES MPEG-1 pack marker bit is missing"))
             (let ((program-mux-rate
                     (logior
                      (ash
                       (logand (aref pes (+ start 9)) #x7f)
                       15)
                      (ash (aref pes (+ start 10)) 7)
                      (ash (aref pes (+ start 11)) -1))))
               (when (zerop program-mux-rate)
                 (bridge-error
                  "PES MPEG-1 pack program_mux_rate zero is forbidden"))
               (values (+ start 12) nil program-mux-rate)))
            (t
             (bridge-error "PES pack header version prefix is invalid")))
    (cond
      ((= position end) end)
      ((< position end)
       (validate-program-stream-system-header
        pes position end mpeg-two-p program-mux-rate))
      (t
       (bridge-error "PES pack header exceeds pack_field_length")))))

(defun validate-pes-extension-field
    (pes offset header-end stream-id)
  "OFFSETのPES extensionを検証し、次位置とextended構文使用有無を返す。"
  (let* ((position
           (ensure-pes-optional-header-range
            offset 1 header-end "extension flags"))
         (flags (aref pes offset))
         (private-data-p (logbitp 7 flags))
         (pack-header-p (logbitp 6 flags))
         (sequence-counter-p (logbitp 5 flags))
         (p-std-buffer-p (logbitp 4 flags))
         (extension-two-p (logbitp 0 flags))
         (stream-id-extension-p nil))
    (unless (= (logand flags #x0e) #x0e)
      (bridge-error "PES extension reserved bits are invalid"))
    (when private-data-p
      (setf position
            (validate-pes-private-data-field
             pes position header-end)))
    (when pack-header-p
      (let ((length-offset position))
        (setf position
              (ensure-pes-optional-header-range
               position 1 header-end "pack field length"))
        (let ((pack-end
                (ensure-pes-optional-header-range
                 position (aref pes length-offset)
                 header-end "pack header")))
          (validate-program-stream-pack-header
           pes position pack-end)
          (setf position pack-end))))
    (when sequence-counter-p
      (bridge-error
       "PES program packet sequence counter is unsupported"))
    (when p-std-buffer-p
      (let ((next
              (ensure-pes-optional-header-range
               position 2 header-end "P-STD buffer")))
        (unless (= (logand (aref pes position) #xc0) #x40)
          (bridge-error "PES P-STD buffer prefix is invalid"))
        (let ((scale
                (if (logbitp 5 (aref pes position)) 1 0)))
          (when (and (<= #xc0 stream-id #xdf)
                     (plusp scale))
            (bridge-error
             "PES audio P-STD buffer scale is invalid"))
          (when (and (<= #xe0 stream-id #xef)
                     (zerop scale))
            (bridge-error
             "PES video P-STD buffer scale is invalid")))
        (setf position next)))
    (when extension-two-p
      (multiple-value-setq
          (position stream-id-extension-p)
        (validate-pes-extension-two-field
         pes position header-end stream-id)))
    (values position stream-id-extension-p)))

(defun validate-pes-header-stuffing (pes start end)
  "PES optional header末尾のstuffing byteを検証する。"
  (let ((count (- end start)))
    (when (> count +maximum-pes-header-stuffing-byte-count+)
      (bridge-error "PES header has too many stuffing bytes: ~D" count))
    (loop for offset from start below end
          unless (= (aref pes offset) #xff)
            do (bridge-error
                "PES stuffing byte is invalid at offset ~D" offset))))

(defun parse-pes-header (pes)
  "PES byte列を検証し、PES-HEADERを返す。"
  (ensure-octet-range pes 0 9 :parse-pes-header)
  (unless (= (read-u24-be pes 0) 1)
    (bridge-error "PES start code prefix is invalid"))
  (unless (= (ldb (byte 2 6) (aref pes 6)) 2)
    (bridge-error "PES optional header marker is invalid"))
  (unless (zerop (ldb (byte 2 4) (aref pes 6)))
    (bridge-error "Scrambled PES packets are unsupported"))
  (let* ((packet-length (read-u16-be pes 4))
         (header-data-length (aref pes 8))
         (payload-offset (+ 9 header-data-length))
         (pts-dts-flags (ldb (byte 2 6) (aref pes 7)))
         (optional-flags (aref pes 7))
         (position 9)
         (stream-id-extension-p nil)
         (pts nil)
         (dts nil))
    (ensure-octet-range pes 9 header-data-length
                        :parse-pes-header-data)
    (when (and (plusp packet-length)
               (/= (+ packet-length 6) (length pes)))
      (bridge-error "PES packet length mismatch: declared=~D actual=~D"
                    packet-length (- (length pes) 6)))
    (case pts-dts-flags
      (0 nil)
      (2
       (setf position
             (ensure-pes-optional-header-range
              position 5 payload-offset "PTS")
             pts (decode-pes-timestamp pes 9 2)))
      (3
       (setf position
             (ensure-pes-optional-header-range
              position 10 payload-offset "PTS/DTS")
             pts (decode-pes-timestamp pes 9 3)
             dts (decode-pes-timestamp pes 14 1)))
      (otherwise
       (bridge-error "PES PTS_DTS_flags value is forbidden: ~D"
                     pts-dts-flags)))
    (when (logbitp 5 optional-flags)
      (setf position
            (validate-pes-escr-field pes position payload-offset)))
    (when (logbitp 4 optional-flags)
      (setf position
            (validate-pes-es-rate-field pes position payload-offset)))
    (when (logbitp 3 optional-flags)
      (setf position
            (validate-pes-trick-mode-field
             pes position payload-offset)))
    (when (logbitp 2 optional-flags)
      (let ((next
              (ensure-pes-optional-header-range
               position 1 payload-offset "additional copy information")))
        (unless (logbitp 7 (aref pes position))
          (bridge-error
           "PES additional copy information marker bit is missing"))
        (setf position next)))
    (when (logbitp 1 optional-flags)
      (bridge-error "PES previous packet CRC is unsupported"))
    (when (logbitp 0 optional-flags)
      (multiple-value-setq
          (position stream-id-extension-p)
        (validate-pes-extension-field
         pes position payload-offset (aref pes 3))))
    (when (and (= (aref pes 3) #xfd)
               (not stream-id-extension-p))
      (bridge-error
       "PES stream_id 0xFD requires stream_id_extension syntax"))
    (validate-pes-header-stuffing pes position payload-offset)
    (make-pes-header
     :stream-id (aref pes 3)
     :packet-length packet-length
     :data-alignment-p (logbitp 2 (aref pes 6))
     :pts pts
     :dts dts
     :payload-offset payload-offset)))

(defun make-pes (stream-id payload pts
                 &key dts data-alignment)
  "STREAM-ID、PAYLOAD、PTSからPES packetを作る。"
  (unless (typep stream-id 'octet)
    (bridge-error "PES stream id does not fit in 8 bits: ~D" stream-id))
  (unless (typep pts '(unsigned-byte 33))
    (bridge-error "PES PTS is required and must fit in 33 bits: ~S" pts))
  (let* ((timestamp-length (if dts 10 5))
         (header-length (+ 9 timestamp-length))
         (result (make-array (+ header-length (length payload))
                             :element-type 'octet))
         (declared-length (+ 3 timestamp-length (length payload)))
         (packet-length (if (> declared-length #xffff)
                            0
                            declared-length)))
    (write-u24-be 1 result 0)
    (setf (aref result 3) stream-id)
    (write-u16-be packet-length result 4)
    (setf (aref result 6) (logior #x80
                                  (if data-alignment #x04 0))
          (aref result 7) (if dts #xc0 #x80)
          (aref result 8) timestamp-length)
    (replace result
             (encode-pes-timestamp pts (if dts 3 2))
             :start1 9)
    (when dts
      (replace result
               (encode-pes-timestamp dts 1)
               :start1 14))
    (replace result payload :start1 header-length)
    result))

(defun rewrite-pes-identification (pes stream-id)
  "PESのstream_idを更新しdata_alignment_indicatorを1にする。"
  (parse-pes-header pes)
  (let ((result (copy-seq pes)))
    (setf (aref result 3) stream-id
          (aref result 6) (logior (aref result 6) #x04))
    result))
