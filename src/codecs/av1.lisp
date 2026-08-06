;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defconstant +av1-obu-tile-list-type+ 8)
(defconstant +av1-obu-temporal-delimiter-type+ 2)
(defconstant +av1-obu-metadata-type+ 5)
(defconstant +av1-metadata-hdr-cll-type+ 1)

(defstruct av1-obu
  (type 0 :type (unsigned-byte 4))
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (header-length 0 :type (integer 1 2))
  (payload-start 0 :type fixnum))

(defstruct av1-frame-semantics
  (show-existing-frame-p nil :type boolean)
  (frame-to-show-map-index nil
                           :type (or null (unsigned-byte 3)))
  (frame-type nil :type (or null (unsigned-byte 2)))
  (show-frame-p nil :type (or null boolean))
  (showable-frame-p nil :type (or null boolean)))

(defstruct av1-stream-transformer
  (state :header
         :type (member :header :extension :size :payload
                       :unbounded-payload))
  (header 0 :type octet)
  (extension-octet nil :type (or null octet))
  (type 0 :type (unsigned-byte 4))
  (drop-p nil :type boolean)
  (extension-p nil :type boolean)
  (has-size-p t :type boolean)
  (size-value 0 :type (integer 0 *))
  (size-shift 0 :type fixnum)
  (size-byte-count 0 :type (integer 0 8))
  (payload-remaining 0 :type (integer 0 *))
  (unbounded-payload-byte-count 0 :type (integer 0 *))
  (zero-count 0 :type fixnum))

(defun ensure-av1-obu-type-allowed (type &optional offset)
  "TYPEがAOM TS draftで搬送可能なOBU typeであることを検証する。"
  (when (= type +av1-obu-tile-list-type+)
    (if offset
        (bridge-error "AV1 Tile List OBU is forbidden at offset ~D"
                      offset)
        (bridge-error "AV1 Tile List OBU is forbidden")))
  (unless (member type '(1 2 3 4 5 6 7 15) :test #'=)
    (if offset
        (bridge-error
         "AV1_INPUT_SUBSET_OBU_TYPE_UNSUPPORTED type=~D offset=~D"
         type offset)
        (bridge-error
         "AV1_INPUT_SUBSET_OBU_TYPE_UNSUPPORTED type=~D"
         type)))
  type)

(defstruct av1-decoder-model
  (buffer-delay-length 0 :type (integer 1 32))
  (buffer-removal-time-length 0 :type (integer 1 32))
  (frame-presentation-time-length 0 :type (integer 1 32)))

(defun parse-av1-obus (access-unit)
  "low-overhead AV1 ACCESS-UNITをOBU境界へ分解する。"
  (let ((obus '())
        (offset 0))
    (loop while (< offset (length access-unit))
          do
      (let* ((header (aref access-unit offset))
             (forbidden (ldb (byte 1 7) header))
             (type (ldb (byte 4 3) header))
             (extension-p (logbitp 2 header))
             (has-size-p (logbitp 1 header))
             (reserved (ldb (byte 1 0) header))
             (header-length (if extension-p 2 1))
             (payload-size-offset (+ offset header-length)))
        (ensure-av1-obu-type-allowed type offset)
        (unless (zerop forbidden)
          (bridge-error "AV1 OBU forbidden bit is set at offset ~D"
                        offset))
        (unless (zerop reserved)
          (bridge-error "AV1 OBU reserved bit is set at offset ~D"
                        offset))
        (ensure-octet-range access-unit offset header-length
                            :parse-av1-obu-header)
        (when extension-p
          (let ((extension (aref access-unit (+ offset 1))))
            (unless (zerop (logand extension #x07))
              (bridge-error
               "AV1 OBU extension reserved bits are not zero at offset ~D"
               offset))))
        (multiple-value-bind (payload-start end)
            (if has-size-p
                (multiple-value-bind (payload-size decoded-start)
                    (decode-uleb128
                     access-unit payload-size-offset
                     :maximum-bytes 8)
                  (values decoded-start
                          (+ decoded-start payload-size)))
                (values payload-size-offset
                        (length access-unit)))
          (ensure-octet-range
           access-unit payload-start (- end payload-start)
           :parse-av1-obu-payload)
          (push (make-av1-obu
                 :type type
                 :start offset
                 :end end
                 :header-length header-length
                 :payload-start payload-start)
                obus)
          (setf offset end))))
    (nreverse obus)))

(defun write-av1-escaped-obu (access-unit obu output)
  "OBUをsize無しstart-code形式とemulation preventionで追加する。"
  (when (= (av1-obu-type obu)
           +av1-obu-temporal-delimiter-type+)
    (unless (= (av1-obu-payload-start obu)
               (av1-obu-end obu))
      (bridge-error
       "AV1_TEMPORAL_DELIMITER_PAYLOAD_NONEMPTY size=~D"
       (- (av1-obu-end obu)
          (av1-obu-payload-start obu))))
    (return-from write-av1-escaped-obu output))
  (vector-push-extend 0 output)
  (vector-push-extend 0 output)
  (vector-push-extend 1 output)
  (let ((zero-count 0))
    (flet ((write-escaped (value)
             (when (and (>= zero-count 2)
                        (<= value 3))
               (vector-push-extend 3 output)
               (setf zero-count 0))
             (vector-push-extend value output)
             (if (zerop value)
                 (incf zero-count)
                 (setf zero-count 0))))
      ;; start-code形式ではobu_has_size_fieldを0にしULEB sizeを除く。
      (write-escaped
       (logand
        (aref access-unit (av1-obu-start obu))
        #xfd))
      (when (= (av1-obu-header-length obu) 2)
        (write-escaped
         (aref access-unit
               (+ (av1-obu-start obu) 1))))
      (loop for offset from (av1-obu-payload-start obu)
              below (av1-obu-end obu)
            for value = (aref access-unit offset)
          do (when (and (>= zero-count 2)
                        (<= value 3))
               (vector-push-extend 3 output)
               (setf zero-count 0))
             (vector-push-extend value output)
             (if (zerop value)
                 (incf zero-count)
                 (setf zero-count 0))))))

(defun write-av1-stream-escaped-octet
    (transformer value output)
  "VALUEをemulation prevention付きでOUTPUTへ追加する。"
  (when (and (>= (av1-stream-transformer-zero-count
                   transformer)
                  2)
             (<= value 3))
    (vector-push-extend 3 output)
    (setf (av1-stream-transformer-zero-count transformer) 0))
  (vector-push-extend value output)
  (if (zerop value)
      (incf (av1-stream-transformer-zero-count transformer))
      (setf (av1-stream-transformer-zero-count transformer) 0))
  output)

(defun start-av1-stream-obu (transformer header output)
  "新しいAV1 OBUのHEADERを検証しsize確定まで保持する。"
  (let ((type (ldb (byte 4 3) header)))
    (ensure-av1-obu-type-allowed type)
    (setf
     (av1-stream-transformer-type transformer) type
     (av1-stream-transformer-drop-p transformer)
     (= type +av1-obu-temporal-delimiter-type+)))
  (unless (zerop (ldb (byte 1 7) header))
    (bridge-error "AV1 OBU forbidden bit is set in streaming input"))
  (unless (zerop (ldb (byte 1 0) header))
    (bridge-error "AV1 OBU reserved bit is set in streaming input"))
  (setf (av1-stream-transformer-header transformer) header
        (av1-stream-transformer-extension-octet transformer) nil
        (av1-stream-transformer-extension-p transformer)
        (logbitp 2 header)
        (av1-stream-transformer-has-size-p transformer)
        (logbitp 1 header)
        (av1-stream-transformer-size-value transformer) 0
        (av1-stream-transformer-size-shift transformer) 0
        (av1-stream-transformer-size-byte-count transformer) 0
        (av1-stream-transformer-unbounded-payload-byte-count
         transformer)
        0
        (av1-stream-transformer-zero-count transformer) 0)
  (setf (av1-stream-transformer-state transformer)
        (if (av1-stream-transformer-extension-p transformer)
            :extension
            (if (av1-stream-transformer-has-size-p transformer)
                :size
                :unbounded-payload)))
  (when (and
         (not (av1-stream-transformer-extension-p transformer))
         (not (av1-stream-transformer-has-size-p transformer)))
    (emit-av1-stream-obu-prefix transformer output))
  transformer)

(defun emit-av1-stream-obu-prefix (transformer output)
  "保持中OBUのsize無しprefixをstart code付きで出力する。"
  (unless (av1-stream-transformer-drop-p transformer)
    (vector-push-extend 0 output)
    (vector-push-extend 0 output)
    (vector-push-extend 1 output)
    (setf (av1-stream-transformer-zero-count transformer) 0)
    (write-av1-stream-escaped-octet
     transformer
     (logand
      (av1-stream-transformer-header transformer)
      #xfd)
     output)
    (when (av1-stream-transformer-extension-octet transformer)
      (write-av1-stream-escaped-octet
       transformer
       (av1-stream-transformer-extension-octet transformer)
       output)))
  output)

(defun write-av1-stream-size-octet
    (transformer value output)
  "AV1 OBU sizeのVALUEを状態へ反映し搬送出力からは除く。"
  (when (= (av1-stream-transformer-size-byte-count transformer) 8)
    (bridge-error "AV1 OBU size exceeds eight bytes"))
  (setf (av1-stream-transformer-size-value transformer)
        (logior
         (av1-stream-transformer-size-value transformer)
         (ash (logand value #x7f)
              (av1-stream-transformer-size-shift transformer))))
  (incf (av1-stream-transformer-size-byte-count transformer))
  (if (logbitp 7 value)
      (incf (av1-stream-transformer-size-shift transformer) 7)
      (let ((payload-size
              (av1-stream-transformer-size-value transformer)))
        (when (and
               (av1-stream-transformer-drop-p transformer)
               (plusp payload-size))
          (bridge-error
           "AV1_TEMPORAL_DELIMITER_PAYLOAD_NONEMPTY size=~D"
           payload-size))
        (emit-av1-stream-obu-prefix transformer output)
        (setf (av1-stream-transformer-payload-remaining transformer)
              payload-size
              (av1-stream-transformer-state transformer)
              (if (zerop payload-size) :header :payload))))
  transformer)

(defun transform-av1-stream-chunk
    (transformer octets &key (start 0) end)
  "低overhead AV1のOCTETS断片をMPEG-TS形式へ逐次変換する。"
  (let* ((resolved-end (or end (length octets)))
         (output
           (make-array (+ (- resolved-end start) 16)
                       :element-type 'octet
                       :adjustable t
                       :fill-pointer 0)))
    (ensure-octet-range octets start (- resolved-end start)
                        :transform-av1-stream-chunk)
    (loop for position from start below resolved-end
          for value = (aref octets position)
          do
      (ecase (av1-stream-transformer-state transformer)
        (:header
         (start-av1-stream-obu transformer value output))
        (:extension
         (unless (zerop (logand value #x07))
           (bridge-error
            "AV1 OBU extension reserved bits are not zero in streaming input"))
         (setf
          (av1-stream-transformer-extension-octet transformer)
          value)
         (setf (av1-stream-transformer-state transformer)
               (if (av1-stream-transformer-has-size-p transformer)
                   :size
                   :unbounded-payload))
         (unless (av1-stream-transformer-has-size-p transformer)
           (emit-av1-stream-obu-prefix transformer output)))
        (:size
         (write-av1-stream-size-octet
          transformer value output))
        (:payload
         (unless (av1-stream-transformer-drop-p transformer)
           (write-av1-stream-escaped-octet
            transformer value output))
         (decf (av1-stream-transformer-payload-remaining
                transformer))
         (when (zerop
                (av1-stream-transformer-payload-remaining
                 transformer))
           (setf (av1-stream-transformer-state transformer)
                 :header)))
        (:unbounded-payload
         (incf
          (av1-stream-transformer-unbounded-payload-byte-count
           transformer))
         (unless (av1-stream-transformer-drop-p transformer)
           (write-av1-stream-escaped-octet
            transformer value output)))))
    (coerce output '(simple-array (unsigned-byte 8) (*)))))

(defun finish-av1-stream-transformer (transformer)
  "TRANSFORMERがOBU境界で完了していることを検証する。"
  (unless (member
           (av1-stream-transformer-state transformer)
           '(:header :unbounded-payload)
           :test #'eq)
    (bridge-error "EOF or PES boundary truncates an AV1 OBU"))
  (when (and
         (eq (av1-stream-transformer-state transformer)
             :unbounded-payload)
         (av1-stream-transformer-drop-p transformer)
         (plusp
          (av1-stream-transformer-unbounded-payload-byte-count
           transformer)))
    (bridge-error
     "AV1_TEMPORAL_DELIMITER_PAYLOAD_NONEMPTY size=~D"
     (av1-stream-transformer-unbounded-payload-byte-count
      transformer)))
  t)

(defun convert-av1-access-unit-to-ts-format (access-unit)
  "low-overhead AV1 ACCESS-UNITをAOM TS draft形式へ変換する。"
  (let ((obus (parse-av1-obus access-unit))
        (output
          (make-array (+ (length access-unit) 64)
                      :element-type 'octet
                      :adjustable t
                      :fill-pointer 0)))
    (when (null obus)
      (bridge-error "AV1 access unit contains no OBU"))
    (dolist (obu obus)
      (write-av1-escaped-obu access-unit obu output))
    (coerce output '(simple-array (unsigned-byte 8) (*)))))

(defun av1-sequence-header-obu (access-unit)
  "ACCESS-UNIT内のsequence header OBUを返す。なければNILを返す。"
  (let ((obu
          (find 1 (parse-av1-obus access-unit)
                :key #'av1-obu-type
                :test #'=)))
    (when obu
      (subseq access-unit
              (av1-obu-start obu)
              (av1-obu-end obu)))))

(defun av1-metadata-obu-hdr-cll-p (access-unit obu)
  "OBUが構文上有効なHDR_CLL metadataなら真を返す。"
  (unless (= (av1-obu-type obu) +av1-obu-metadata-type+)
    (return-from av1-metadata-obu-hdr-cll-p nil))
  (multiple-value-bind (metadata-type metadata-start)
      (decode-uleb128
       access-unit
       (av1-obu-payload-start obu)
       :maximum-bytes 8)
    (unless (= metadata-type +av1-metadata-hdr-cll-type+)
      (return-from av1-metadata-obu-hdr-cll-p nil))
    ;; max_cll, max_fall各16 bitとtrailing_bitsの先頭bitを要求する。
    (when (< (- (av1-obu-end obu) metadata-start) 5)
      (bridge-error
       "AV1_HDR_CLL_METADATA_TRUNCATED offset=~D"
       (av1-obu-start obu)))
    (unless (and
             (= (- (av1-obu-end obu) metadata-start) 5)
             (= (aref access-unit (+ metadata-start 4)) #x80))
      (bridge-error
       "AV1_HDR_CLL_TRAILING_BITS_INVALID offset=~D"
       (av1-obu-start obu)))
    t))

(defun av1-access-unit-hdr-cll-p (access-unit)
  "ACCESS-UNITにHDR_CLL metadata OBUがあれば真を返す。"
  (some
   (lambda (obu)
     (av1-metadata-obu-hdr-cll-p access-unit obu))
   (parse-av1-obus access-unit)))

(defun read-av1-uvlc (reader)
  "AV1のunsigned variable length codeを読む。"
  (let ((leading-zeroes 0))
    (loop until (= (read-one-bit reader) 1)
          do (incf leading-zeroes))
    (if (>= leading-zeroes 32)
        #xffffffff
        (+ (- (ash 1 leading-zeroes) 1)
           (read-bits reader leading-zeroes)))))

(defun parse-av1-timing-info (reader)
  "AV1 timing_infoを読み飛ばす。"
  (skip-bits reader 32)
  (skip-bits reader 32)
  (when (= (read-one-bit reader) 1)
    (read-av1-uvlc reader)))

(defun parse-av1-decoder-model-info (reader)
  "AV1 decoder_model_infoを読み、後続field長を返す。"
  (let ((buffer-delay-length (+ (read-bits reader 5) 1)))
    (skip-bits reader 32)
    (let ((buffer-removal-time-length
            (+ (read-bits reader 5) 1))
          (frame-presentation-time-length
            (+ (read-bits reader 5) 1)))
      (make-av1-decoder-model
       :buffer-delay-length buffer-delay-length
       :buffer-removal-time-length buffer-removal-time-length
       :frame-presentation-time-length
       frame-presentation-time-length))))

(defun skip-av1-operating-parameters (reader decoder-model)
  "AV1 operating_parameters_infoを読みlow_delay_mode_flagを返す。"
  (let ((length
          (av1-decoder-model-buffer-delay-length decoder-model)))
    (skip-bits reader length)
    (skip-bits reader length)
    (= (read-one-bit reader) 1)))

(defun av1-hdr-wcg-idc (color-primaries transfer-characteristics)
  "AV1 color descriptionからdraft descriptorのHDR/WCG分類を返す。"
  (let ((wide-color-p (= color-primaries 9))
        (high-dynamic-range-p
          (member transfer-characteristics '(16 18) :test #'=)))
    (cond
      ((and wide-color-p high-dynamic-range-p) 2)
      (wide-color-p 1)
      ((and (= color-primaries 1)
            (member transfer-characteristics '(1 6 13 14 15)
                    :test #'=))
       0)
      (t 3))))

(defun parse-av1-color-configuration (reader profile)
  "AV1 color_configを読み、descriptor向けfieldを返す。"
  (let* ((high-bitdepth (read-one-bit reader))
         (twelve-bit
           (if (and (= profile 2)
                    (= high-bitdepth 1))
               (read-one-bit reader)
               0))
         (monochrome
           (if (= profile 1)
               0
               (read-one-bit reader)))
         (color-description-present-p (= (read-one-bit reader) 1))
         (color-primaries 2)
         (transfer-characteristics 2)
         (matrix-coefficients 2))
    (when color-description-present-p
      (setf color-primaries (read-bits reader 8)
            transfer-characteristics (read-bits reader 8)
            matrix-coefficients (read-bits reader 8)))
    (cond
      ((= monochrome 1)
       (read-one-bit reader)
       (values high-bitdepth twelve-bit monochrome 1 1 0
               (av1-hdr-wcg-idc color-primaries
                                transfer-characteristics)))
      ((and (= color-primaries 1)
            (= transfer-characteristics 13)
            (= matrix-coefficients 0))
       ;; color_range/subsamplingは推定値だがseparate_uv_delta_qは存在する。
       (read-one-bit reader)
       (values high-bitdepth twelve-bit monochrome 0 0 0
               (av1-hdr-wcg-idc color-primaries
                                transfer-characteristics)))
      (t
       (read-one-bit reader)
       (multiple-value-bind
             (subsampling-x subsampling-y)
           (cond
             ((= profile 0) (values 1 1))
             ((= profile 1) (values 0 0))
             ((= twelve-bit 1)
              (let ((x (read-one-bit reader)))
                (values x
                        (if (= x 1)
                            (read-one-bit reader)
                            0))))
             (t (values 1 0)))
         (let ((chroma-sample-position
                 (if (and (= subsampling-x 1)
                          (= subsampling-y 1))
                     (read-bits reader 2)
                     0)))
           (when (= chroma-sample-position 3)
             (bridge-error
              "AV1 chroma sample position uses a reserved value"))
           (read-one-bit reader)
           (values high-bitdepth twelve-bit monochrome
                   subsampling-x subsampling-y
                   chroma-sample-position
                   (av1-hdr-wcg-idc
                    color-primaries
                    transfer-characteristics))))))))

(defun parse-av1-operating-points (reader reduced-header-p)
  "AV1 operating point群を読み、OP0 level/tier/初期delayを返す。"
  (when reduced-header-p
    (let ((level (read-bits reader 5)))
      (when (and (> level 23) (/= level 31))
        (bridge-error "AV1 sequence level is reserved: ~D" level))
      (return-from parse-av1-operating-points
        (values level 0 nil nil))))
  (let ((decoder-model nil))
    (when (= (read-one-bit reader) 1)
      (parse-av1-timing-info reader)
      (when (= (read-one-bit reader) 1)
        (setf decoder-model
              (parse-av1-decoder-model-info reader))))
    (let ((initial-display-delay-present-p
            (= (read-one-bit reader) 1))
          (operating-point-count (+ (read-bits reader 5) 1))
          (first-level nil)
          (first-tier nil)
          (first-delay nil)
          (first-low-delay-mode-p nil))
      (loop repeat operating-point-count
            for operating-point-index from 0
            do (skip-bits reader 12)
               (let* ((level (read-bits reader 5))
                      (tier
                        (if (> level 7)
                            (read-one-bit reader)
                            0)))
                 (when (and (> level 23) (/= level 31))
                   (bridge-error
                    "AV1 sequence level is reserved: ~D"
                    level))
                 (if (zerop operating-point-index)
                     (setf first-level level
                           first-tier tier)
                     (when (or (> level first-level)
                               (and (= level first-level)
                                    (> tier first-tier)))
                       (bridge-error
                        "AV1 operating point exceeds the level/tier signaled by operating point zero"))))
               (when decoder-model
                 (when (= (read-one-bit reader) 1)
                   (let ((low-delay-mode-p
                           (skip-av1-operating-parameters
                            reader decoder-model)))
                     (when (zerop operating-point-index)
                       (setf first-low-delay-mode-p
                             low-delay-mode-p)))))
               (when initial-display-delay-present-p
                 (when (= (read-one-bit reader) 1)
                   (let ((delay (read-bits reader 4)))
                     (when (zerop operating-point-index)
                       (setf first-delay delay)))))
            finally
               (return
                 (values first-level
                         first-tier
                         first-delay
                         first-low-delay-mode-p))))))

(defun skip-av1-sequence-tools (reader reduced-header-p)
  "sequence headerのtool enable field群を読み飛ばす。"
  (skip-bits reader 3)
  (unless reduced-header-p
    (skip-bits reader 4)
    (let ((enable-order-hint-p (= (read-one-bit reader) 1)))
      (when enable-order-hint-p
        (skip-bits reader 2))
      (let ((choose-screen-content-tools-p
              (= (read-one-bit reader) 1))
            (force-screen-content-tools 2))
        (unless choose-screen-content-tools-p
          (setf force-screen-content-tools
                (read-one-bit reader)))
        (when (> force-screen-content-tools 0)
          (unless (= (read-one-bit reader) 1)
            (read-one-bit reader))))
      (when enable-order-hint-p
        (skip-bits reader 3))))
  (skip-bits reader 3))

(defun parse-av1-sequence-header (access-unit)
  "ACCESS-UNITのsequence headerからAV1 codec configurationを得る。"
  (let* ((obus (parse-av1-obus access-unit))
         (obu (find 1 obus :key #'av1-obu-type :test #'=)))
    (unless obu
      (bridge-error "AV1 access unit does not contain a sequence header"))
    (let ((reader
            (make-bit-reader access-unit
                             :start (av1-obu-payload-start obu)
                             :end (av1-obu-end obu))))
      (let ((profile (read-bits reader 3)))
        (when (> profile 2)
          (bridge-error "AV1 sequence profile is reserved: ~D"
                        profile))
        (unless (zerop profile)
          (bridge-error
           "AV1_INPUT_SUBSET_PROFILE_UNSUPPORTED profile=~D"
           profile))
        (skip-bits reader 1)
        (let ((reduced-header-p (= (read-one-bit reader) 1)))
          (multiple-value-bind
                (level tier initial-delay low-delay-mode-p)
              (parse-av1-operating-points reader reduced-header-p)
            (let* ((width-bits (+ (read-bits reader 4) 1))
                   (height-bits (+ (read-bits reader 4) 1))
                   (maximum-width (+ (read-bits reader width-bits) 1))
                   (maximum-height (+ (read-bits reader height-bits) 1)))
              (unless reduced-header-p
                (when (= (read-one-bit reader) 1)
                  (skip-bits reader 7)))
              (skip-av1-sequence-tools reader reduced-header-p)
              (multiple-value-bind
                    (high-bitdepth twelve-bit monochrome
                     subsampling-x subsampling-y sample-position
                     hdr-wcg-idc)
                  (parse-av1-color-configuration reader profile)
                (unless
                    (and
                     (zerop twelve-bit)
                     (zerop monochrome)
                     (= subsampling-x 1)
                     (= subsampling-y 1))
                  (bridge-error
                   "AV1_INPUT_SUBSET_COLOR_UNSUPPORTED profile=~D high_bitdepth=~D twelve_bit=~D monochrome=~D subsampling_x=~D subsampling_y=~D"
                   profile high-bitdepth twelve-bit monochrome
                   subsampling-x subsampling-y))
                (skip-bits reader 1)
                (values
                 (make-av1-codec-configuration
                  :profile profile
                  :level level
                  :tier tier
                  :high-bitdepth high-bitdepth
                  :twelve-bit twelve-bit
                  :monochrome monochrome
                  :chroma-subsampling-x subsampling-x
                  :chroma-subsampling-y subsampling-y
                  :chroma-sample-position sample-position
                  :hdr-wcg-idc hdr-wcg-idc
                  :low-delay-mode-p low-delay-mode-p
                  :initial-presentation-delay initial-delay)
                 maximum-width
                 maximum-height)))))))))

(defun av1-sequence-header-reduced-header-p (access-unit)
  "ACCESS-UNITのsequence headerからreduced_still_picture_headerを返す。"
  (let* ((obus (parse-av1-obus access-unit))
         (obu (find 1 obus :key #'av1-obu-type :test #'=)))
    (unless obu
      (bridge-error "AV1 access unit does not contain a sequence header"))
    (let ((reader
            (make-bit-reader access-unit
                             :start (av1-obu-payload-start obu)
                             :end (av1-obu-end obu))))
      (skip-bits reader 3)
      (skip-bits reader 1)
      (= (read-one-bit reader) 1))))

(defun read-av1-frame-semantics
    (reader reduced-header-p obu-type)
  "frame header先頭からAOM §3.5に必要な表示意味を読む。"
  (when reduced-header-p
    (return-from read-av1-frame-semantics
      (make-av1-frame-semantics
       :frame-type 0
       :show-frame-p t
       :showable-frame-p nil)))
  (let ((show-existing-frame-p
          (= (read-one-bit reader) 1)))
    (when show-existing-frame-p
      (when (= obu-type 6)
        (bridge-error
         "AV1_FRAME_OBU_SHOW_EXISTING_FORBIDDEN"))
      (return-from read-av1-frame-semantics
        (make-av1-frame-semantics
         :show-existing-frame-p t
         :frame-to-show-map-index
         (read-bits reader 3))))
    (let* ((frame-type (read-bits reader 2))
           (show-frame-p (= (read-one-bit reader) 1))
           (showable-frame-p
             (if show-frame-p
                 (not (zerop frame-type))
                 (= (read-one-bit reader) 1))))
      (make-av1-frame-semantics
       :frame-type frame-type
       :show-frame-p show-frame-p
       :showable-frame-p showable-frame-p))))

(defun av1-frame-random-access-kind
    (semantics sequence-header-in-temporal-unit-p)
  "SEMANTICSからkey/delayed keyのRAP種別を返す。"
  (when (and
         sequence-header-in-temporal-unit-p
         (not
          (av1-frame-semantics-show-existing-frame-p semantics))
         (eql (av1-frame-semantics-frame-type semantics) 0))
    (if (av1-frame-semantics-show-frame-p semantics)
        :key
        :delayed-key)))

(defun av1-access-unit-random-access-kind
    (access-unit reduced-header-p
     &optional sequence-header-in-temporal-unit-p)
  "ACCESS-UNITのRAP種別、表示意味、TU境界情報を返す。"
  (let* ((obus (parse-av1-obus access-unit))
         (sequence-header-p
           (find 1 obus :key #'av1-obu-type :test #'=))
         (temporal-delimiters
           (remove-if-not
            (lambda (obu)
              (= (av1-obu-type obu)
                 +av1-obu-temporal-delimiter-type+))
            obus))
         (temporal-delimiter-p
           (not (null temporal-delimiters)))
         (frame-obus
           (remove-if-not
            (lambda (obu)
              (member (av1-obu-type obu) '(3 6) :test #'=))
            obus)))
    (when (> (length temporal-delimiters) 1)
      (bridge-error
       "AV1_MULTIPLE_TEMPORAL_DELIMITERS_IN_ACCESS_UNIT count=~D"
       (length temporal-delimiters)))
    (when temporal-delimiter-p
      (let ((delimiter (first temporal-delimiters)))
        (unless (eq delimiter (first obus))
          (bridge-error
           "AV1_TEMPORAL_DELIMITER_NOT_FIRST"))
        (unless (= (av1-obu-payload-start delimiter)
                   (av1-obu-end delimiter))
          (bridge-error
           "AV1_TEMPORAL_DELIMITER_PAYLOAD_NONEMPTY size=~D"
           (- (av1-obu-end delimiter)
              (av1-obu-payload-start delimiter))))))
    (when (null frame-obus)
      (bridge-error "AV1 access unit contains no frame header"))
    (when (> (length frame-obus) 1)
      (bridge-error
       "AV1_MULTIPLE_ACCESS_UNITS_IN_PES frame_header_count=~D"
       (length frame-obus)))
    (let ((frame-obu (first frame-obus)))
      (when (and sequence-header-p
                 (> (av1-obu-start sequence-header-p)
                    (av1-obu-start frame-obu)))
        (bridge-error
         "AV1 sequence header follows a frame header in one access unit"))
      (let ((reader
              (make-bit-reader
               access-unit
               :start (av1-obu-payload-start frame-obu)
               :end (av1-obu-end frame-obu))))
        (let ((semantics
                (read-av1-frame-semantics
                 reader reduced-header-p
                 (av1-obu-type frame-obu))))
          (let* ((sequence-header-in-access-unit-p
                   (not (null sequence-header-p)))
                 (effective-sequence-header-p
                   (or
                    sequence-header-in-access-unit-p
                    (and
                     (not temporal-delimiter-p)
                     sequence-header-in-temporal-unit-p))))
            (values
             (av1-frame-random-access-kind
              semantics effective-sequence-header-p)
             semantics
             temporal-delimiter-p
             sequence-header-in-access-unit-p)))))))

(defun decode-av1-prefix-size (octets offset end)
  "OCTETSの利用可能範囲からOBU sizeを読み、完了有無も返す。"
  (let ((value 0)
        (shift 0)
        (position offset))
    (loop repeat 8
          do
      (when (= position end)
        (return-from decode-av1-prefix-size
          (values nil offset nil)))
      (let ((current (aref octets position)))
        (setf value
              (logior value
                      (ash (logand current #x7f) shift)))
        (incf position)
        (when (zerop (logand current #x80))
          (return-from decode-av1-prefix-size
            (values value position t)))
        (incf shift 7)))
    (bridge-error "AV1 OBU size exceeds eight bytes")))

(defun av1-prefix-random-access-kind
    (access-unit payload-start payload-end reduced-header-p
     obu-type sequence-header-in-temporal-unit-p)
  "利用可能なframe header先頭からRAP種別と表示意味を返す。"
  (when (<= payload-end payload-start)
    (bridge-error "AV1 frame header OBU has an empty payload"))
  (let ((reader
          (make-bit-reader access-unit
                           :start payload-start
                           :end payload-end)))
    (let ((semantics
            (read-av1-frame-semantics
             reader reduced-header-p obu-type)))
      (values
       (av1-frame-random-access-kind
        semantics sequence-header-in-temporal-unit-p)
       semantics))))

(defun parse-av1-stream-prefix-metadata
    (access-unit configuration reduced-header-p
     &optional sequence-header-in-temporal-unit-p)
  "不完全な末尾OBUを許し、逐次開始に必要なAV1情報を返す。"
  (let ((offset 0)
        (resolved-configuration configuration)
        (resolved-reduced-header-p reduced-header-p)
        (random-access-kind nil)
        (frame-semantics nil)
        (frame-header-count 0)
        (hdr-cll-p nil)
        (sequence-header-in-access-unit-p nil)
        (temporal-delimiter-seen-p nil)
        (frame-header-found-p nil))
    (loop while (< offset (length access-unit))
          do
      (let* ((header (aref access-unit offset))
             (type (ldb (byte 4 3) header))
             (extension-p (logbitp 2 header))
             (has-size-p (logbitp 1 header))
             (header-length (if extension-p 2 1))
             (size-offset (+ offset header-length)))
        (ensure-av1-obu-type-allowed type offset)
        (unless (zerop (ldb (byte 1 7) header))
          (bridge-error "AV1 OBU forbidden bit is set at offset ~D"
                        offset))
        (unless (zerop (ldb (byte 1 0) header))
          (bridge-error "AV1 OBU reserved bit is set at offset ~D"
                        offset))
        (when (> size-offset (length access-unit))
          (loop-finish))
        (when extension-p
          (unless (zerop
                   (logand (aref access-unit (+ offset 1))
                           #x07))
            (bridge-error
             "AV1 OBU extension reserved bits are not zero at offset ~D"
             offset)))
        (multiple-value-bind
              (payload-size payload-start complete-size-p)
            (if has-size-p
                (decode-av1-prefix-size
                 access-unit size-offset (length access-unit))
                (values (- (length access-unit) size-offset)
                        size-offset t))
          (unless complete-size-p
            (loop-finish))
          (let* ((declared-end (+ payload-start payload-size))
                 (available-end
                   (min declared-end (length access-unit))))
            (when (= type +av1-obu-temporal-delimiter-type+)
              (when temporal-delimiter-seen-p
                (bridge-error
                 "AV1_MULTIPLE_TEMPORAL_DELIMITERS_IN_ACCESS_UNIT count=2"))
              (unless (zerop offset)
                (bridge-error
                 "AV1_TEMPORAL_DELIMITER_NOT_FIRST"))
              (setf temporal-delimiter-seen-p t)
              (when (and has-size-p
                         (<= declared-end (length access-unit))
                         (plusp payload-size))
                (bridge-error
                 "AV1_TEMPORAL_DELIMITER_PAYLOAD_NONEMPTY size=~D"
                 payload-size)))
            (when (and has-size-p
                       (= type 1)
                       (<= declared-end (length access-unit)))
              (when frame-header-found-p
                (bridge-error
                 "AV1 sequence header follows a frame header in one access unit"))
              (let ((sequence
                      (subseq access-unit offset declared-end)))
                (multiple-value-bind
                      (parsed-configuration width height)
                    (parse-av1-sequence-header sequence)
                  (declare (ignore width height))
                  (setf resolved-configuration
                        parsed-configuration
                        sequence-header-in-access-unit-p t
                        resolved-reduced-header-p
                        (av1-sequence-header-reduced-header-p
                         sequence)))))
            (when (and
                   has-size-p
                   (= type +av1-obu-metadata-type+)
                   (<= declared-end (length access-unit)))
              (let ((metadata
                      (make-av1-obu
                       :type type
                       :start offset
                       :end declared-end
                       :header-length header-length
                       :payload-start payload-start)))
                (when
                    (av1-metadata-obu-hdr-cll-p
                     access-unit metadata)
                  (setf hdr-cll-p t))))
            (when (member type '(3 6) :test #'=)
              (incf frame-header-count)
              (when (> frame-header-count 1)
                (bridge-error
                 "AV1_MULTIPLE_ACCESS_UNITS_IN_PES frame_header_count=~D"
                 frame-header-count))
              (cond
                ((> available-end payload-start)
                 (multiple-value-setq
                     (random-access-kind frame-semantics)
                   (av1-prefix-random-access-kind
                    access-unit payload-start available-end
                    resolved-reduced-header-p
                    type
                    (or
                     sequence-header-in-access-unit-p
                     (and
                      (not temporal-delimiter-seen-p)
                      sequence-header-in-temporal-unit-p))))
                 (setf frame-header-found-p t))
                ((<= declared-end (length access-unit))
                 (bridge-error
                  "AV1 frame header OBU has an empty payload"))))
            (when (> declared-end (length access-unit))
              (loop-finish))
            (setf offset declared-end))))
          finally
             (return
               (values resolved-configuration
                       resolved-reduced-header-p
                       random-access-kind
                       frame-header-found-p
                       frame-semantics
                       hdr-cll-p
                       temporal-delimiter-seen-p
                       sequence-header-in-access-unit-p)))))
