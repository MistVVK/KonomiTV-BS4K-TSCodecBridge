;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

;;; This file is an independent structural implementation of the syntax in
;;; AOMedia AV1 Bitstream & Decoding Process Specification sections 5.5,
;;; 5.9, 5.10, and 5.11.  It intentionally does not decode entropy-coded tile
;;; contents.  It validates every syntax element before that decoder boundary
;;; and proves that the declared tile byte ranges cover the complete FRAME OBU.

(defconstant +av1-structure-reference-slot-count+ 8)
(defconstant +av1-structure-references-per-frame+ 7)
(defconstant +av1-structure-primary-ref-none+ 7)
(defconstant +av1-structure-select-screen-content-tools+ 2)
(defconstant +av1-structure-select-integer-mv+ 2)
(defconstant +av1-structure-max-segments+ 8)
(defconstant +av1-structure-segment-feature-count+ 8)
(defconstant +av1-structure-max-tile-width+ 4096)
(defconstant +av1-structure-max-tile-area+ (* 4096 2304))
(defconstant +av1-structure-max-tile-columns+ 64)
(defconstant +av1-structure-max-tile-rows+ 64)
(defconstant +av1-structure-superres-numerator+ 8)
(defconstant +av1-structure-superres-denominator-minimum+ 9)
(defconstant +av1-structure-warped-model-precision-bits+ 16)

(defparameter +av1-structure-segment-feature-bits+
  #(8 6 6 6 6 3 0 0))

(defparameter +av1-structure-segment-feature-signed+
  #(t t t t t nil nil nil))

(defparameter +av1-structure-segment-feature-maximum+
  #(255 63 63 63 63 7 0 0))

(defun av1-structure-copy-array (array)
  "ARRAYを同じdimensionsとelement typeで複製する。"
  (let ((copy
          (make-array
           (array-dimensions array)
           :element-type (array-element-type array))))
    (dotimes (index (array-total-size array) copy)
      (setf (row-major-aref copy index)
            (row-major-aref array index)))))

(defstruct av1-operating-point-validation-state
  (idc 0 :type (unsigned-byte 12))
  (decoder-model-present-p nil :type boolean))

(defstruct av1-sequence-validation-state
  (source-payload
   (make-array 0 :element-type 'octet)
   :type octet-vector)
  (profile 0 :type (unsigned-byte 2))
  (still-picture-p nil :type boolean)
  (reduced-still-picture-header-p nil :type boolean)
  (decoder-model-info-present-p nil :type boolean)
  (equal-picture-interval-p nil :type boolean)
  (buffer-removal-time-length 0 :type (integer 0 32))
  (frame-presentation-time-length 0 :type (integer 0 32))
  (operating-points #()
                    :type (simple-array
                           av1-operating-point-validation-state (*)))
  (frame-width-bits 1 :type (integer 1 16))
  (frame-height-bits 1 :type (integer 1 16))
  (maximum-frame-width 1 :type (integer 1 65536))
  (maximum-frame-height 1 :type (integer 1 65536))
  (frame-id-numbers-present-p nil :type boolean)
  (delta-frame-id-length 0 :type (integer 0 17))
  (additional-frame-id-length 0 :type (integer 0 8))
  (use-128x128-superblock-p nil :type boolean)
  (enable-warped-motion-p nil :type boolean)
  (enable-order-hint-p nil :type boolean)
  (enable-ref-frame-mvs-p nil :type boolean)
  (force-screen-content-tools
   +av1-structure-select-screen-content-tools+
   :type (integer 0 2))
  (force-integer-mv
   +av1-structure-select-integer-mv+
   :type (integer 0 2))
  (order-hint-bits 0 :type (integer 0 8))
  (enable-superres-p nil :type boolean)
  (enable-cdef-p nil :type boolean)
  (enable-restoration-p nil :type boolean)
  (bit-depth 8 :type (member 8 10 12))
  (monochrome-p nil :type boolean)
  (plane-count 3 :type (member 1 3))
  (subsampling-x 1 :type bit)
  (subsampling-y 1 :type bit)
  (separate-uv-delta-q-p nil :type boolean)
  (film-grain-params-present-p nil :type boolean))

(defstruct (av1-segmentation-validation-state
            (:constructor %make-av1-segmentation-validation-state))
  (enabled-p nil :type boolean)
  (feature-enabled
   (make-array
    (list +av1-structure-max-segments+
          +av1-structure-segment-feature-count+)
    :element-type 'bit
    :initial-element 0)
   :type (simple-array bit (8 8)))
  (feature-data
   (make-array
    (list +av1-structure-max-segments+
          +av1-structure-segment-feature-count+)
    :element-type 'fixnum
    :initial-element 0)
   :type (simple-array fixnum (8 8))))

(defun make-av1-segmentation-validation-state ()
  "空のsegmentation validation stateを返す。"
  (%make-av1-segmentation-validation-state))

(defun copy-av1-segmentation-validation-state-deep (state)
  "STATEを入力と配列を共有しない形で複製する。"
  (%make-av1-segmentation-validation-state
   :enabled-p
   (av1-segmentation-validation-state-enabled-p state)
   :feature-enabled
   (av1-structure-copy-array
    (av1-segmentation-validation-state-feature-enabled state))
   :feature-data
   (av1-structure-copy-array
    (av1-segmentation-validation-state-feature-data state))))

(defun make-av1-identity-global-motion ()
  "7 reference分のidentity global-motion parameterを返す。"
  (let ((parameters
          (make-array '(7 6)
                      :element-type 'integer
                      :initial-element 0)))
    (dotimes (reference +av1-structure-references-per-frame+)
      (setf
       (aref parameters reference 2)
       (ash 1 +av1-structure-warped-model-precision-bits+)
       (aref parameters reference 5)
       (ash 1 +av1-structure-warped-model-precision-bits+)))
    parameters))

(defstruct (av1-reference-slot-validation-state
            (:constructor %make-av1-reference-slot-validation-state))
  (valid-p nil :type boolean)
  (showable-frame-p nil :type boolean)
  (decoded-frame-serial 0 :type (integer 0 *))
  (shown-via-show-existing-p nil :type boolean)
  (frame-id 0 :type (integer 0 *))
  (frame-type 0 :type (unsigned-byte 2))
  (order-hint 0 :type (integer 0 *))
  (upscaled-width 1 :type (integer 1 *))
  (frame-width 1 :type (integer 1 *))
  (frame-height 1 :type (integer 1 *))
  (render-width 1 :type (integer 1 *))
  (render-height 1 :type (integer 1 *))
  (profile 0 :type (unsigned-byte 2))
  (bit-depth 8 :type (member 8 10 12))
  (subsampling-x 1 :type bit)
  (subsampling-y 1 :type bit)
  (segmentation
   (make-av1-segmentation-validation-state)
   :type av1-segmentation-validation-state)
  (global-motion
   (make-av1-identity-global-motion)
   :type (simple-array integer (7 6))))

(defun make-av1-reference-slot-validation-state ()
  "未使用のreference slotを返す。"
  (%make-av1-reference-slot-validation-state))

(defun copy-av1-reference-slot-validation-state-deep (slot)
  "SLOTを入力と配列を共有しない形で複製する。"
  (%make-av1-reference-slot-validation-state
   :valid-p
   (av1-reference-slot-validation-state-valid-p slot)
   :showable-frame-p
   (av1-reference-slot-validation-state-showable-frame-p slot)
   :decoded-frame-serial
   (av1-reference-slot-validation-state-decoded-frame-serial slot)
   :shown-via-show-existing-p
   (av1-reference-slot-validation-state-shown-via-show-existing-p
    slot)
   :frame-id
   (av1-reference-slot-validation-state-frame-id slot)
   :frame-type
   (av1-reference-slot-validation-state-frame-type slot)
   :order-hint
   (av1-reference-slot-validation-state-order-hint slot)
   :upscaled-width
   (av1-reference-slot-validation-state-upscaled-width slot)
   :frame-width
   (av1-reference-slot-validation-state-frame-width slot)
   :frame-height
   (av1-reference-slot-validation-state-frame-height slot)
   :render-width
   (av1-reference-slot-validation-state-render-width slot)
   :render-height
   (av1-reference-slot-validation-state-render-height slot)
   :profile
   (av1-reference-slot-validation-state-profile slot)
   :bit-depth
   (av1-reference-slot-validation-state-bit-depth slot)
   :subsampling-x
   (av1-reference-slot-validation-state-subsampling-x slot)
   :subsampling-y
   (av1-reference-slot-validation-state-subsampling-y slot)
   :segmentation
   (copy-av1-segmentation-validation-state-deep
    (av1-reference-slot-validation-state-segmentation slot))
   :global-motion
   (av1-structure-copy-array
    (av1-reference-slot-validation-state-global-motion slot))))

(defstruct (av1-frame-validation-state
            (:constructor %make-av1-frame-validation-state))
  (reference-slots
   (make-array
    +av1-structure-reference-slot-count+
    :initial-contents
    (loop repeat +av1-structure-reference-slot-count+
          collect (make-av1-reference-slot-validation-state)))
   :type (simple-array av1-reference-slot-validation-state (8)))
  (current-frame-id nil :type (or null (integer 0 *)))
  (next-decoded-frame-serial 0 :type (integer 0 *)))

(defun make-av1-frame-validation-state ()
  "参照frameを一つも持たないvalidation stateを返す。"
  (%make-av1-frame-validation-state))

(defun copy-av1-frame-validation-state-deep (state)
  "STATEを入力とreference slotを共有しない形で複製する。"
  (%make-av1-frame-validation-state
   :reference-slots
   (make-array
    +av1-structure-reference-slot-count+
    :initial-contents
    (loop for slot across
          (av1-frame-validation-state-reference-slots state)
          collect
          (copy-av1-reference-slot-validation-state-deep slot)))
   :current-frame-id
   (av1-frame-validation-state-current-frame-id state)
   :next-decoded-frame-serial
   (av1-frame-validation-state-next-decoded-frame-serial state)))

(defstruct av1-frame-structure-result
  (semantics (make-av1-frame-semantics)
             :type av1-frame-semantics)
  (header-bytes 0 :type (integer 0 *))
  (frame-type 0 :type (unsigned-byte 2))
  (refresh-frame-flags 0 :type octet)
  (frame-width 1 :type (integer 1 *))
  (frame-height 1 :type (integer 1 *))
  (upscaled-width 1 :type (integer 1 *))
  (render-width 1 :type (integer 1 *))
  (render-height 1 :type (integer 1 *))
  (tile-columns 1 :type (integer 1 64))
  (tile-rows 1 :type (integer 1 64))
  (context-update-tile-id 0 :type (integer 0 *))
  (tile-size-bytes 0 :type (integer 0 4))
  (tile-ranges #() :type simple-vector))

(defstruct (av1-frame-structure-parser
            (:constructor %make-av1-frame-structure-parser))
  (reader (make-bit-reader #()) :type bit-reader)
  (sequence (make-av1-sequence-validation-state)
            :type av1-sequence-validation-state)
  (working-state (make-av1-frame-validation-state)
                 :type av1-frame-validation-state)
  (temporal-id 0 :type (unsigned-byte 3))
  (spatial-id 0 :type (unsigned-byte 2))
  (frame-type 0 :type (unsigned-byte 2))
  (frame-is-intra-p t :type boolean)
  (show-frame-p nil :type boolean)
  (showable-frame-p nil :type boolean)
  (error-resilient-mode-p nil :type boolean)
  (disable-cdf-update-p nil :type boolean)
  (allow-screen-content-tools-p nil :type boolean)
  (force-integer-mv-p nil :type boolean)
  (current-frame-id 0 :type (integer 0 *))
  (frame-size-override-p nil :type boolean)
  (order-hint 0 :type (integer 0 *))
  (primary-ref-frame +av1-structure-primary-ref-none+
                     :type (integer 0 7))
  (refresh-frame-flags 0 :type octet)
  (reference-indices
   (make-array +av1-structure-references-per-frame+
               :element-type '(unsigned-byte 3)
               :initial-element 0)
   :type (simple-array (unsigned-byte 3) (7)))
  (allow-high-precision-mv-p nil :type boolean)
  (frame-width 1 :type (integer 1 *))
  (frame-height 1 :type (integer 1 *))
  (upscaled-width 1 :type (integer 1 *))
  (render-width 1 :type (integer 1 *))
  (render-height 1 :type (integer 1 *))
  (mi-columns 1 :type (integer 1 *))
  (mi-rows 1 :type (integer 1 *))
  (tile-columns 1 :type (integer 1 64))
  (tile-rows 1 :type (integer 1 64))
  (tile-columns-log2 0 :type (integer 0 6))
  (tile-rows-log2 0 :type (integer 0 6))
  (context-update-tile-id 0 :type (integer 0 *))
  (tile-size-bytes 0 :type (integer 0 4))
  (base-q-index 0 :type octet)
  (delta-q-y-dc 0 :type (integer -64 63))
  (delta-q-u-dc 0 :type (integer -64 63))
  (delta-q-u-ac 0 :type (integer -64 63))
  (delta-q-v-dc 0 :type (integer -64 63))
  (delta-q-v-ac 0 :type (integer -64 63))
  (segmentation (make-av1-segmentation-validation-state)
                :type av1-segmentation-validation-state)
  (coded-lossless-p nil :type boolean)
  (all-lossless-p nil :type boolean)
  (allow-intrabc-p nil :type boolean)
  (reference-select-p nil :type boolean)
  (global-motion (make-av1-identity-global-motion)
                 :type (simple-array integer (7 6))))

(defun av1-structure-read-bits (reader count boundary)
  "READERからCOUNT bitを読み、BOUNDARYつきtruncation errorにする。"
  (when (> count (bit-reader-remaining reader))
    (bridge-error
     "AV1_FRAME_HEADER_TRUNCATED_~A bit=~D requested=~D remaining=~D"
     boundary
     (bit-reader-position reader)
     count
     (bit-reader-remaining reader)))
  (read-bits reader count))

(defun av1-structure-read-flag (reader boundary)
  "READERからboolean flagを読む。"
  (= (av1-structure-read-bits reader 1 boundary) 1))

(defun av1-structure-read-signed-bits (reader count boundary)
  "AV1 su(COUNT)を読む。"
  (let ((value
           (av1-structure-read-bits reader count boundary))
         (sign-mask (ash 1 (- count 1))))
    (if (zerop (logand value sign-mask))
        value
        (- value (* 2 sign-mask)))))

(defun av1-structure-read-uvlc (reader boundary)
  "AV1 uvlc()を読む。"
  (let ((leading-zeroes 0))
    (loop until (av1-structure-read-flag reader boundary)
          do (incf leading-zeroes))
    (if (>= leading-zeroes 32)
        (1- (ash 1 32))
        (+
         (av1-structure-read-bits reader leading-zeroes boundary)
         (1- (ash 1 leading-zeroes))))))

(defun av1-structure-read-ns (reader maximum boundary)
  "AV1 ns(MAXIMUM)を読む。"
  (unless (plusp maximum)
    (bridge-error "AV1_NS_RANGE_INVALID maximum=~D" maximum))
  (let* ((width (integer-length maximum))
         (cutoff (- (ash 1 width) maximum))
         (value
           (av1-structure-read-bits
            reader (- width 1) boundary)))
    (if (< value cutoff)
        value
        (+
         (- (ash value 1) cutoff)
         (av1-structure-read-bits reader 1 boundary)))))

(defun av1-structure-read-little-endian
    (reader byte-count boundary)
  "byte境界からlittle-endian BYTE-COUNT bytesを読む。"
  (unless (zerop (logand (bit-reader-position reader) 7))
    (bridge-error "AV1_TILE_SIZE_NOT_BYTE_ALIGNED"))
  (let ((value 0))
    (dotimes (index byte-count value)
      (setf value
            (logior
             value
             (ash
              (av1-structure-read-bits reader 8 boundary)
              (* index 8)))))))

(defun av1-structure-byte-align-zero (reader boundary)
  "次のbyte境界までzero_bitだけを受理する。"
  (loop until (zerop (logand (bit-reader-position reader) 7))
        do
    (unless
        (zerop (av1-structure-read-bits reader 1 boundary))
      (bridge-error "AV1_BYTE_ALIGNMENT_NONZERO_~A bit=~D"
                    boundary
                    (1- (bit-reader-position reader)))))
  reader)

(defun av1-structure-validate-trailing-bits (reader boundary)
  "OBU payload末尾のtrailing_one_bitとzero bitsを検証する。"
  (when (zerop (bit-reader-remaining reader))
    (bridge-error "AV1_TRAILING_BITS_MISSING_~A" boundary))
  (unless (= (av1-structure-read-bits reader 1 boundary) 1)
    (bridge-error "AV1_TRAILING_ONE_BIT_INVALID_~A" boundary))
  (loop while (plusp (bit-reader-remaining reader))
        do
    (unless (zerop (av1-structure-read-bits reader 1 boundary))
      (bridge-error "AV1_TRAILING_ZERO_BIT_INVALID_~A"
                    boundary)))
  t)

(defun av1-structure-clip (minimum maximum value)
  "VALUEをMINIMUMからMAXIMUMへclipする。"
  (min maximum (max minimum value)))

(defun av1-structure-tile-log2 (block-size target)
  "BLOCK-SIZEを倍化してTARGET以上にする最小指数を返す。"
  (loop for exponent from 0
        when (>= (ash block-size exponent) target)
          return exponent))

(defun av1-structure-obu-layer-ids (access-unit obu)
  "OBU extensionのtemporal_idとspatial_idを返す。"
  (if (= (av1-obu-header-length obu) 1)
      (values 0 0)
      (let ((extension
              (aref access-unit (+ (av1-obu-start obu) 1))))
        (values
         (ldb (byte 3 5) extension)
         (ldb (byte 2 3) extension)))))

(defun av1-structure-require-single-layer (access-unit obu)
  "Bridge入力subset外のlayer構成をfail closedにする。"
  (when
      (and
       (= (av1-obu-header-length obu) 2)
       (member (av1-obu-type obu) '(1 2) :test #'=))
    (bridge-error
     "AV1_INPUT_SUBSET_NON_LAYER_OBU_HAS_EXTENSION type=~D"
     (av1-obu-type obu)))
  (multiple-value-bind (temporal-id spatial-id)
      (av1-structure-obu-layer-ids access-unit obu)
    (unless (and (zerop temporal-id) (zerop spatial-id))
      (bridge-error
       "AV1_INPUT_SUBSET_MULTILAYER_UNSUPPORTED temporal_id=~D spatial_id=~D"
       temporal-id spatial-id))
    (values temporal-id spatial-id)))

(defun av1-structure-parse-color-configuration
    (reader profile)
  "sequence headerのcolor_configを読み、構文分岐に必要な値を返す。"
  (let* ((high-bitdepth-p
           (av1-structure-read-flag reader :sequence-color))
         (twelve-bit-p
           (and (= profile 2)
                high-bitdepth-p
                (av1-structure-read-flag
                 reader :sequence-color)))
         (bit-depth
           (cond
             (twelve-bit-p 12)
             (high-bitdepth-p 10)
             (t 8)))
         (monochrome-p
           (if (= profile 1)
               nil
               (av1-structure-read-flag
                reader :sequence-color)))
         (plane-count (if monochrome-p 1 3))
         (color-description-p
           (av1-structure-read-flag reader :sequence-color))
         (color-primaries 2)
         (transfer-characteristics 2)
         (matrix-coefficients 2)
         (subsampling-x 0)
         (subsampling-y 0))
    (when color-description-p
      (setf
       color-primaries
       (av1-structure-read-bits reader 8 :sequence-color)
       transfer-characteristics
       (av1-structure-read-bits reader 8 :sequence-color)
       matrix-coefficients
       (av1-structure-read-bits reader 8 :sequence-color)))
    (cond
      (monochrome-p
       (av1-structure-read-bits reader 1 :sequence-color-range)
       (setf subsampling-x 1
             subsampling-y 1)
       (values bit-depth monochrome-p plane-count
               subsampling-x subsampling-y nil))
      ((and (= color-primaries 1)
            (= transfer-characteristics 13)
            (= matrix-coefficients 0))
       (values bit-depth nil plane-count 0 0
               (av1-structure-read-flag
                reader :sequence-separate-uv-delta-q)))
      (t
       (av1-structure-read-bits reader 1 :sequence-color-range)
       (cond
         ((= profile 0)
          (setf subsampling-x 1
                subsampling-y 1))
         ((= profile 1)
          (setf subsampling-x 0
                subsampling-y 0))
         ((= bit-depth 12)
          (setf
           subsampling-x
           (av1-structure-read-bits
            reader 1 :sequence-subsampling))
          (setf
           subsampling-y
           (if (= subsampling-x 1)
               (av1-structure-read-bits
                reader 1 :sequence-subsampling)
               0)))
         (t
          (setf subsampling-x 1
                subsampling-y 0)))
       (when (and (= subsampling-x 1)
                  (= subsampling-y 1))
         (av1-structure-read-bits
          reader 2 :sequence-chroma-sample-position))
       (values
        bit-depth nil plane-count subsampling-x subsampling-y
        (av1-structure-read-flag
         reader :sequence-separate-uv-delta-q))))))

(defun av1-structure-parse-timing-information (reader)
  "timing_infoを読み、equal_picture_intervalを返す。"
  (let ((display-tick
          (av1-structure-read-bits
           reader 32 :sequence-timing))
        (time-scale
          (av1-structure-read-bits
           reader 32 :sequence-timing))
        (equal-picture-interval-p
          (av1-structure-read-flag
           reader :sequence-timing)))
    (when (or (zerop display-tick) (zerop time-scale))
      (bridge-error
       "AV1_SEQUENCE_TIMING_VALUE_ZERO display_tick=~D time_scale=~D"
       display-tick time-scale))
    (when equal-picture-interval-p
      (av1-structure-read-uvlc
       reader :sequence-equal-picture-interval))
    equal-picture-interval-p))

(defun av1-structure-parse-decoder-model-information (reader)
  "decoder_model_infoを読み、frame側で必要なbit長を返す。"
  (let ((buffer-delay-length
          (1+
           (av1-structure-read-bits
            reader 5 :sequence-decoder-model))))
    (av1-structure-read-bits
     reader 32 :sequence-decoder-model)
    (values
     buffer-delay-length
     (1+
      (av1-structure-read-bits
       reader 5 :sequence-decoder-model))
     (1+
      (av1-structure-read-bits
       reader 5 :sequence-decoder-model)))))

(defun parse-av1-sequence-header-validation-state
    (access-unit obu)
  "SEQUENCE_HEADER OBUを完全に読み、frame validation用stateを返す。"
  (unless (= (av1-obu-type obu) 1)
    (bridge-error
     "AV1_SEQUENCE_VALIDATOR_REQUIRES_SEQUENCE_HEADER type=~D"
     (av1-obu-type obu)))
  (av1-structure-require-single-layer access-unit obu)
  (let* ((reader
           (make-bit-reader
            access-unit
            :start (av1-obu-payload-start obu)
            :end (av1-obu-end obu)))
         (profile
           (av1-structure-read-bits
            reader 3 :sequence-profile))
         (still-picture-p
           (av1-structure-read-flag
            reader :sequence-still-picture))
         (reduced-header-p
           (av1-structure-read-flag
            reader :sequence-reduced-header))
         (decoder-model-info-present-p nil)
         (equal-picture-interval-p nil)
         (buffer-delay-length 0)
         (buffer-removal-time-length 0)
         (frame-presentation-time-length 0)
         (operating-points
           (make-array 0
                       :element-type
                       'av1-operating-point-validation-state))
         (initial-display-delay-present-p nil))
    (when (> profile 2)
      (bridge-error "AV1_SEQUENCE_PROFILE_RESERVED profile=~D"
                    profile))
    (unless (zerop profile)
      (bridge-error
       "AV1_INPUT_SUBSET_PROFILE_UNSUPPORTED profile=~D"
       profile))
    (when (and reduced-header-p (not still-picture-p))
      (bridge-error
       "AV1_REDUCED_HEADER_REQUIRES_STILL_PICTURE"))
    (if reduced-header-p
        (let ((level
                (av1-structure-read-bits
                 reader 5 :sequence-level)))
          (when (and (> level 23) (/= level 31))
            (bridge-error
             "AV1_SEQUENCE_LEVEL_RESERVED level=~D" level))
          (setf operating-points
                (make-array
                 1
                 :element-type
                 'av1-operating-point-validation-state
                 :initial-contents
                 (list
                  (make-av1-operating-point-validation-state)))))
        (let ((timing-information-present-p
                (av1-structure-read-flag
                 reader :sequence-timing-present)))
          (when timing-information-present-p
            (setf equal-picture-interval-p
                  (av1-structure-parse-timing-information
                   reader)
                  decoder-model-info-present-p
                  (av1-structure-read-flag
                   reader :sequence-decoder-model-present))
            (when decoder-model-info-present-p
              (multiple-value-setq
                  (buffer-delay-length
                   buffer-removal-time-length
                   frame-presentation-time-length)
                (av1-structure-parse-decoder-model-information
                 reader))))
          (setf initial-display-delay-present-p
                (av1-structure-read-flag
                 reader :sequence-initial-display-delay))
          (let* ((count
                   (1+
                    (av1-structure-read-bits
                     reader 5 :sequence-operating-points)))
                 (points
                 (make-array
                    count
                    :element-type
                    'av1-operating-point-validation-state
                    :initial-element
                    (make-av1-operating-point-validation-state))))
            (dotimes (index count)
              (let ((idc
                       (av1-structure-read-bits
                        reader 12 :sequence-operating-point-idc))
                     (level
                       (av1-structure-read-bits
                        reader 5 :sequence-level))
                     (decoder-model-p nil))
                (when (and (> level 23) (/= level 31))
                  (bridge-error
                   "AV1_SEQUENCE_LEVEL_RESERVED level=~D" level))
                ;; 0 means all layers.  0x101 is the explicit base-only
                ;; operating point.  Any other mask declares a layer set this
                ;; validator intentionally does not implement.
                (unless (member idc '(0 #x101) :test #'=)
                  (bridge-error
                   "AV1_INPUT_SUBSET_MULTILAYER_UNSUPPORTED operating_point_idc=~D"
                   idc))
                (when (> level 7)
                  (av1-structure-read-bits
                   reader 1 :sequence-tier))
                (when decoder-model-info-present-p
                  (setf decoder-model-p
                        (av1-structure-read-flag
                         reader
                         :sequence-operating-decoder-model))
                  (when decoder-model-p
                    (av1-structure-read-bits
                     reader buffer-delay-length
                     :sequence-decoder-buffer-delay)
                    (av1-structure-read-bits
                     reader buffer-delay-length
                     :sequence-encoder-buffer-delay)
                    (av1-structure-read-bits
                     reader 1 :sequence-low-delay-mode)))
                (when initial-display-delay-present-p
                  (when
                      (av1-structure-read-flag
                       reader
                       :sequence-operating-display-delay)
                    (av1-structure-read-bits
                     reader 4
                     :sequence-operating-display-delay)))
                (setf
                 (aref points index)
                 (make-av1-operating-point-validation-state
                  :idc idc
                  :decoder-model-present-p decoder-model-p))))
            (setf operating-points points))))
    (let* ((frame-width-bits
             (1+
              (av1-structure-read-bits
               reader 4 :sequence-frame-width-bits)))
           (frame-height-bits
             (1+
              (av1-structure-read-bits
               reader 4 :sequence-frame-height-bits)))
           (maximum-frame-width
             (1+
              (av1-structure-read-bits
               reader frame-width-bits
               :sequence-maximum-frame-width)))
           (maximum-frame-height
             (1+
              (av1-structure-read-bits
               reader frame-height-bits
               :sequence-maximum-frame-height)))
           (frame-id-numbers-present-p
             (and
              (not reduced-header-p)
              (av1-structure-read-flag
               reader :sequence-frame-id-present)))
           (delta-frame-id-length 0)
           (additional-frame-id-length 0))
      (when frame-id-numbers-present-p
        (setf
         delta-frame-id-length
         (+
          2
          (av1-structure-read-bits
           reader 4 :sequence-delta-frame-id-length))
         additional-frame-id-length
         (+
          1
          (av1-structure-read-bits
           reader 3 :sequence-additional-frame-id-length)))
        (when
            (>
             (+ delta-frame-id-length additional-frame-id-length)
             16)
          (bridge-error
           "AV1_FRAME_ID_LENGTH_EXCEEDS_16 length=~D"
           (+ delta-frame-id-length additional-frame-id-length))))
      (let ((use-128x128-superblock-p
               (av1-structure-read-flag
                reader :sequence-superblock))
             (enable-filter-intra-p
               (av1-structure-read-flag
                reader :sequence-filter-intra))
             (enable-intra-edge-filter-p
               (av1-structure-read-flag
                reader :sequence-intra-edge-filter))
             (enable-warped-motion-p nil)
             (enable-order-hint-p nil)
             (enable-ref-frame-mvs-p nil)
             (force-screen-content-tools
               +av1-structure-select-screen-content-tools+)
             (force-integer-mv
               +av1-structure-select-integer-mv+)
             (order-hint-bits 0))
        (declare
         (ignore enable-filter-intra-p
                 enable-intra-edge-filter-p))
        (unless reduced-header-p
          (av1-structure-read-bits
           reader 1 :sequence-interintra-compound)
          (av1-structure-read-bits
           reader 1 :sequence-masked-compound)
          (setf enable-warped-motion-p
                (av1-structure-read-flag
                 reader :sequence-warped-motion))
          (av1-structure-read-bits
           reader 1 :sequence-dual-filter)
          (setf enable-order-hint-p
                (av1-structure-read-flag
                 reader :sequence-order-hint))
          (when enable-order-hint-p
            (av1-structure-read-bits
             reader 1 :sequence-joint-compound)
            (setf enable-ref-frame-mvs-p
                  (av1-structure-read-flag
                   reader :sequence-reference-frame-mvs)))
          (unless
              (av1-structure-read-flag
               reader :sequence-choose-screen-content-tools)
            (setf force-screen-content-tools
                  (av1-structure-read-bits
                   reader 1
                   :sequence-force-screen-content-tools)))
          (when (> force-screen-content-tools 0)
            (unless
                (av1-structure-read-flag
                 reader :sequence-choose-integer-mv)
              (setf force-integer-mv
                    (av1-structure-read-bits
                     reader 1 :sequence-force-integer-mv))))
          (when enable-order-hint-p
            (setf order-hint-bits
                  (1+
                   (av1-structure-read-bits
                    reader 3
                    :sequence-order-hint-bits)))))
        (let ((enable-superres-p
                (av1-structure-read-flag
                 reader :sequence-superres))
              (enable-cdef-p
                (av1-structure-read-flag
                 reader :sequence-cdef))
              (enable-restoration-p
                (av1-structure-read-flag
                 reader :sequence-restoration)))
          (multiple-value-bind
                (bit-depth monochrome-p plane-count
                 subsampling-x subsampling-y
                 separate-uv-delta-q-p)
              (av1-structure-parse-color-configuration
               reader profile)
            (unless
                (and
                 (member bit-depth '(8 10) :test #'=)
                 (not monochrome-p)
                 (= subsampling-x 1)
                 (= subsampling-y 1))
              (bridge-error
               "AV1_INPUT_SUBSET_COLOR_UNSUPPORTED profile=~D bit_depth=~D monochrome=~A subsampling_x=~D subsampling_y=~D"
               profile bit-depth monochrome-p
               subsampling-x subsampling-y))
            (let ((film-grain-params-present-p
                    (av1-structure-read-flag
                     reader :sequence-film-grain)))
              (av1-structure-validate-trailing-bits
               reader :sequence)
              (make-av1-sequence-validation-state
               :source-payload
               (subseq
                access-unit
                (av1-obu-payload-start obu)
                (av1-obu-end obu))
               :profile profile
               :still-picture-p still-picture-p
               :reduced-still-picture-header-p
               reduced-header-p
               :decoder-model-info-present-p
               decoder-model-info-present-p
               :equal-picture-interval-p
               equal-picture-interval-p
               :buffer-removal-time-length
               buffer-removal-time-length
               :frame-presentation-time-length
               frame-presentation-time-length
               :operating-points operating-points
               :frame-width-bits frame-width-bits
               :frame-height-bits frame-height-bits
               :maximum-frame-width maximum-frame-width
               :maximum-frame-height maximum-frame-height
               :frame-id-numbers-present-p
               frame-id-numbers-present-p
               :delta-frame-id-length
               delta-frame-id-length
               :additional-frame-id-length
               additional-frame-id-length
               :use-128x128-superblock-p
               use-128x128-superblock-p
               :enable-warped-motion-p
               enable-warped-motion-p
               :enable-order-hint-p
               enable-order-hint-p
               :enable-ref-frame-mvs-p
               enable-ref-frame-mvs-p
               :force-screen-content-tools
               force-screen-content-tools
               :force-integer-mv force-integer-mv
               :order-hint-bits order-hint-bits
               :enable-superres-p enable-superres-p
               :enable-cdef-p enable-cdef-p
               :enable-restoration-p
               enable-restoration-p
               :bit-depth bit-depth
               :monochrome-p monochrome-p
               :plane-count plane-count
               :subsampling-x subsampling-x
               :subsampling-y subsampling-y
               :separate-uv-delta-q-p
               separate-uv-delta-q-p
               :film-grain-params-present-p
               film-grain-params-present-p))))))))

(defun av1-structure-relative-distance (parser first second)
  "order hint FIRSTとSECONDの符号付き相対距離を返す。"
  (let* ((sequence
           (av1-frame-structure-parser-sequence parser))
         (bits
           (av1-sequence-validation-state-order-hint-bits
            sequence)))
    (if (zerop bits)
        0
        (let ((difference (- first second))
               (sign-bit (ash 1 (- bits 1))))
          (-
           (logand difference (1- sign-bit))
           (logand difference sign-bit))))))

(defun av1-structure-slot (parser index)
  "PARSERのworking reference slotを返す。"
  (aref
   (av1-frame-validation-state-reference-slots
    (av1-frame-structure-parser-working-state parser))
   index))

(defun av1-structure-mark-reference-frames
    (parser id-length)
  "current_frame_idに対して古すぎるreference slotを無効化する。"
  (let* ((sequence
           (av1-frame-structure-parser-sequence parser))
         (difference-length
           (av1-sequence-validation-state-delta-frame-id-length
            sequence))
         (window (ash 1 difference-length))
         (modulus (ash 1 id-length))
         (current
           (av1-frame-structure-parser-current-frame-id parser)))
    (dotimes (index +av1-structure-reference-slot-count+)
      (let* ((slot (av1-structure-slot parser index))
             (reference
               (av1-reference-slot-validation-state-frame-id
                slot)))
        (when
            (if (> current window)
                (or (> reference current)
                    (< reference (- current window)))
                (and (> reference current)
                     (< reference
                        (+ modulus current (- window)))))
          (setf
           (av1-reference-slot-validation-state-valid-p slot)
           nil))))))

(defun av1-structure-require-valid-reference
    (parser index boundary)
  "INDEXのreference slotが現sequenceと互換なことを検証する。"
  (let ((sequence
           (av1-frame-structure-parser-sequence parser))
         (slot (av1-structure-slot parser index)))
    (unless
        (av1-reference-slot-validation-state-valid-p slot)
      (bridge-error
       "AV1_REFERENCE_SLOT_INVALID boundary=~A slot=~D"
       boundary index))
    (unless
        (and
         (=
          (av1-reference-slot-validation-state-profile slot)
          (av1-sequence-validation-state-profile sequence))
         (=
          (av1-reference-slot-validation-state-bit-depth slot)
          (av1-sequence-validation-state-bit-depth sequence))
         (=
          (av1-reference-slot-validation-state-subsampling-x slot)
          (av1-sequence-validation-state-subsampling-x sequence))
         (=
          (av1-reference-slot-validation-state-subsampling-y slot)
          (av1-sequence-validation-state-subsampling-y sequence)))
      (bridge-error
       "AV1_REFERENCE_SLOT_FORMAT_MISMATCH slot=~D"
       index))
    slot))

(defun av1-structure-find-reference-by-order
    (shifted used current backward-p latest-p)
  "set_frame_refs用に条件へ合う未使用slotを探す。"
  (let ((selected -1)
        (selected-hint 0))
    (dotimes (index +av1-structure-reference-slot-count+
                    selected)
      (let ((hint (aref shifted index)))
        (when
            (and
             (zerop (aref used index))
             (if backward-p
                 (>= hint current)
                 (< hint current))
             (or
              (minusp selected)
              (if latest-p
                  (>= hint selected-hint)
                  (< hint selected-hint))))
          (setf selected index
                selected-hint hint))))))

(defun av1-structure-set-short-references
    (parser last-index golden-index)
  "AV1 set_frame_refs processで7 reference indexを計算する。"
  (let* ((sequence
           (av1-frame-structure-parser-sequence parser))
         (order-bits
           (av1-sequence-validation-state-order-hint-bits
            sequence))
         (current-hint (ash 1 (- order-bits 1)))
         (indices
           (make-array +av1-structure-references-per-frame+
                       :element-type '(signed-byte 4)
                       :initial-element -1))
         (used
           (make-array +av1-structure-reference-slot-count+
                       :element-type 'bit
                       :initial-element 0))
         (shifted
           (make-array +av1-structure-reference-slot-count+
                       :element-type 'integer
                       :initial-element 0)))
    (setf (aref indices 0) last-index
          (aref indices 3) golden-index
          (aref used last-index) 1
          (aref used golden-index) 1)
    (dotimes (index +av1-structure-reference-slot-count+)
      (setf
       (aref shifted index)
       (+
        current-hint
        (av1-structure-relative-distance
         parser
         (av1-reference-slot-validation-state-order-hint
          (av1-structure-slot parser index))
         (av1-frame-structure-parser-order-hint parser)))))
    (unless (< (aref shifted last-index) current-hint)
      (bridge-error
       "AV1_SHORT_REFERENCE_LAST_NOT_FORWARD slot=~D"
       last-index))
    (unless (< (aref shifted golden-index) current-hint)
      (bridge-error
       "AV1_SHORT_REFERENCE_GOLDEN_NOT_FORWARD slot=~D"
       golden-index))
    ;; ALTREF is the latest backward reference.
    (let ((reference
            (av1-structure-find-reference-by-order
             shifted used current-hint t t)))
      (when (not (minusp reference))
        (setf (aref indices 6) reference
              (aref used reference) 1)))
    ;; BWDREF and ALTREF2 are the two earliest backward references.
    (dolist (reference-type '(4 5))
      (let ((reference
              (av1-structure-find-reference-by-order
               shifted used current-hint t nil)))
        (when (not (minusp reference))
          (setf (aref indices reference-type) reference
                (aref used reference) 1))))
    ;; LAST2, LAST3, BWDREF, ALTREF2, ALTREF are then filled with the
    ;; latest remaining forward references in this normative order.
    (dolist (reference-type '(1 2 4 5 6))
      (when (minusp (aref indices reference-type))
        (let ((reference
                (av1-structure-find-reference-by-order
                 shifted used current-hint nil t)))
          (when (not (minusp reference))
            (setf (aref indices reference-type) reference
                  (aref used reference) 1)))))
    ;; Any hole uses the globally earliest output order.
    (let ((earliest-index 0)
          (earliest-hint (aref shifted 0)))
      (loop for index from 1
              below +av1-structure-reference-slot-count+
            when (< (aref shifted index) earliest-hint)
              do (setf earliest-index index
                       earliest-hint (aref shifted index)))
      (dotimes (index +av1-structure-references-per-frame+)
        (when (minusp (aref indices index))
          (setf (aref indices index) earliest-index))))
    (dotimes (index +av1-structure-references-per-frame+)
      (setf
       (aref
        (av1-frame-structure-parser-reference-indices parser)
        index)
       (aref indices index)))))

(defun av1-structure-parse-superres (parser)
  "superres_paramsを読み、FrameWidthとUpscaledWidthを更新する。"
  (let* ((sequence
           (av1-frame-structure-parser-sequence parser))
         (use-superres-p
           (and
            (av1-sequence-validation-state-enable-superres-p
             sequence)
            (av1-structure-read-flag
             (av1-frame-structure-parser-reader parser)
             :superres)))
         (denominator
           (if use-superres-p
               (+
                +av1-structure-superres-denominator-minimum+
                (av1-structure-read-bits
                 (av1-frame-structure-parser-reader parser)
                 3 :superres-denominator))
               +av1-structure-superres-numerator+))
         (upscaled-width
           (av1-frame-structure-parser-frame-width parser)))
    (setf
     (av1-frame-structure-parser-upscaled-width parser)
     upscaled-width
     (av1-frame-structure-parser-frame-width parser)
     (floor
      (+
       (* upscaled-width
          +av1-structure-superres-numerator+)
       (floor denominator 2))
      denominator))
    (when
        (zerop
         (av1-frame-structure-parser-frame-width parser))
      (bridge-error "AV1_SUPERRES_FRAME_WIDTH_ZERO"))))

(defun av1-structure-compute-image-size (parser)
  "FrameWidthとFrameHeightからMiColumns/MiRowsを得る。"
  (setf
   (av1-frame-structure-parser-mi-columns parser)
   (* 2
      (ash
       (+
        (av1-frame-structure-parser-frame-width parser)
        7)
       -3))
   (av1-frame-structure-parser-mi-rows parser)
   (* 2
      (ash
       (+
        (av1-frame-structure-parser-frame-height parser)
        7)
       -3))))

(defun av1-structure-parse-frame-size (parser)
  "frame_sizeを読む。"
  (let ((sequence
          (av1-frame-structure-parser-sequence parser))
        (reader
          (av1-frame-structure-parser-reader parser)))
    (if (av1-frame-structure-parser-frame-size-override-p
         parser)
        (setf
         (av1-frame-structure-parser-frame-width parser)
         (1+
          (av1-structure-read-bits
           reader
           (av1-sequence-validation-state-frame-width-bits
            sequence)
           :frame-width))
         (av1-frame-structure-parser-frame-height parser)
         (1+
          (av1-structure-read-bits
           reader
           (av1-sequence-validation-state-frame-height-bits
            sequence)
           :frame-height)))
        (setf
         (av1-frame-structure-parser-frame-width parser)
         (av1-sequence-validation-state-maximum-frame-width
          sequence)
         (av1-frame-structure-parser-frame-height parser)
         (av1-sequence-validation-state-maximum-frame-height
          sequence)))
    (when
        (or
         (>
          (av1-frame-structure-parser-frame-width parser)
          (av1-sequence-validation-state-maximum-frame-width
           sequence))
         (>
          (av1-frame-structure-parser-frame-height parser)
          (av1-sequence-validation-state-maximum-frame-height
           sequence)))
      (bridge-error
       "AV1_FRAME_SIZE_EXCEEDS_SEQUENCE width=~D height=~D"
       (av1-frame-structure-parser-frame-width parser)
       (av1-frame-structure-parser-frame-height parser)))
    (av1-structure-parse-superres parser)
    (av1-structure-compute-image-size parser)))

(defun av1-structure-parse-render-size (parser)
  "render_sizeを読む。"
  (let ((reader
          (av1-frame-structure-parser-reader parser)))
    (if (av1-structure-read-flag
         reader :render-size)
        (setf
         (av1-frame-structure-parser-render-width parser)
         (1+
          (av1-structure-read-bits reader 16 :render-width))
         (av1-frame-structure-parser-render-height parser)
         (1+
          (av1-structure-read-bits reader 16 :render-height)))
        (setf
         (av1-frame-structure-parser-render-width parser)
         (av1-frame-structure-parser-upscaled-width parser)
         (av1-frame-structure-parser-render-height parser)
         (av1-frame-structure-parser-frame-height parser)))))

(defun av1-structure-parse-frame-size-with-references (parser)
  "frame_size_with_refsを読み、参照slotまたは明示寸法を使う。"
  (let ((found-p nil)
        (reader
          (av1-frame-structure-parser-reader parser)))
    (dotimes (reference +av1-structure-references-per-frame+)
      (when
          (and
           (not found-p)
           (av1-structure-read-flag
            reader :frame-size-with-references))
        (let* ((slot-index
                 (aref
                  (av1-frame-structure-parser-reference-indices
                   parser)
                  reference))
               (slot
                 (av1-structure-require-valid-reference
                  parser slot-index :frame-size)))
          (setf
           found-p t
           (av1-frame-structure-parser-upscaled-width parser)
           (av1-reference-slot-validation-state-upscaled-width
            slot)
           (av1-frame-structure-parser-frame-width parser)
           (av1-reference-slot-validation-state-upscaled-width
            slot)
           (av1-frame-structure-parser-frame-height parser)
           (av1-reference-slot-validation-state-frame-height
            slot)
           (av1-frame-structure-parser-render-width parser)
           (av1-reference-slot-validation-state-render-width
            slot)
           (av1-frame-structure-parser-render-height parser)
           (av1-reference-slot-validation-state-render-height
            slot)))))
    (cond
      (found-p
       (av1-structure-parse-superres parser)
       (av1-structure-compute-image-size parser))
      (t
       (av1-structure-parse-frame-size parser)
       (av1-structure-parse-render-size parser)))))

(defun av1-structure-parse-tile-information (parser)
  "tile_infoを完全に読み、tile gridとsize field幅を確定する。"
  (let* ((reader
           (av1-frame-structure-parser-reader parser))
         (sequence
           (av1-frame-structure-parser-sequence parser))
         (large-superblock-p
           (av1-sequence-validation-state-use-128x128-superblock-p
            sequence))
         (mi-columns
           (av1-frame-structure-parser-mi-columns parser))
         (mi-rows
           (av1-frame-structure-parser-mi-rows parser))
         (superblock-shift (if large-superblock-p 5 4))
         (superblock-size (+ superblock-shift 2))
         (superblock-columns
           (ash
            (+
             mi-columns
             (if large-superblock-p 31 15))
            (- superblock-shift)))
         (superblock-rows
           (ash
            (+
             mi-rows
             (if large-superblock-p 31 15))
            (- superblock-shift)))
         (maximum-tile-width-superblocks
           (ash +av1-structure-max-tile-width+
                (- superblock-size)))
         (maximum-tile-area-superblocks
           (ash +av1-structure-max-tile-area+
                (- (* 2 superblock-size))))
         (minimum-columns-log2
           (av1-structure-tile-log2
            maximum-tile-width-superblocks
            superblock-columns))
         (maximum-columns-log2
           (av1-structure-tile-log2
            1
            (min
             superblock-columns
             +av1-structure-max-tile-columns+)))
         (maximum-rows-log2
           (av1-structure-tile-log2
            1
            (min
             superblock-rows
             +av1-structure-max-tile-rows+)))
         (minimum-tiles-log2
           (max
            minimum-columns-log2
            (av1-structure-tile-log2
             maximum-tile-area-superblocks
             (* superblock-rows superblock-columns))))
         (uniform-p
           (av1-structure-read-flag reader :tile-info))
         (columns-log2 0)
         (rows-log2 0)
         (tile-columns 0)
         (tile-rows 0))
    (cond
      (uniform-p
       (setf columns-log2 minimum-columns-log2)
       (loop while (< columns-log2 maximum-columns-log2)
             while
             (av1-structure-read-flag
              reader :tile-uniform-columns)
             do (incf columns-log2))
       (let ((tile-width
               (ash
                (+
                 superblock-columns
                 (1- (ash 1 columns-log2)))
                (- columns-log2))))
         (setf tile-columns
               (ceiling superblock-columns tile-width)))
       (setf rows-log2
             (max
              (- minimum-tiles-log2 columns-log2)
              0))
       (loop while (< rows-log2 maximum-rows-log2)
             while
             (av1-structure-read-flag
              reader :tile-uniform-rows)
             do (incf rows-log2))
       (let ((tile-height
               (ash
                (+
                 superblock-rows
                 (1- (ash 1 rows-log2)))
                (- rows-log2))))
         (setf tile-rows
               (ceiling superblock-rows tile-height))))
      (t
       (let ((widest-tile 0)
             (column-start 0))
          (loop while (< column-start superblock-columns)
                do
            (let* ((maximum-width
                     (min
                      (- superblock-columns column-start)
                      maximum-tile-width-superblocks))
                   (size
                     (1+
                      (av1-structure-read-ns
                       reader maximum-width
                       :tile-nonuniform-column))))
              (setf widest-tile (max widest-tile size))
              (incf column-start size)
              (incf tile-columns)))
          (unless (= column-start superblock-columns)
            (bridge-error
             "AV1_TILE_COLUMNS_DO_NOT_COVER_FRAME end=~D expected=~D"
             column-start superblock-columns))
          (setf columns-log2
                (av1-structure-tile-log2 1 tile-columns))
          (let* ((maximum-area
                   (if (plusp minimum-tiles-log2)
                       (ash
                        (* superblock-rows superblock-columns)
                        (- (1+ minimum-tiles-log2)))
                       (* superblock-rows superblock-columns)))
                 (maximum-height
                   (max (floor maximum-area widest-tile) 1))
                 (row-start 0))
            (loop while (< row-start superblock-rows)
                  do
              (let* ((remaining
                       (- superblock-rows row-start))
                     (range (min remaining maximum-height))
                     (size
                       (1+
                        (av1-structure-read-ns
                         reader range
                         :tile-nonuniform-row))))
                (incf row-start size)
                (incf tile-rows)))
            (unless (= row-start superblock-rows)
              (bridge-error
               "AV1_TILE_ROWS_DO_NOT_COVER_FRAME end=~D expected=~D"
               row-start superblock-rows)))
         (setf rows-log2
               (av1-structure-tile-log2 1 tile-rows)))))
    (unless
        (and
         (<= 1 tile-columns +av1-structure-max-tile-columns+)
         (<= 1 tile-rows +av1-structure-max-tile-rows+))
      (bridge-error
       "AV1_TILE_GRID_OUT_OF_RANGE columns=~D rows=~D"
       tile-columns tile-rows))
    (setf
     (av1-frame-structure-parser-tile-columns parser)
     tile-columns
     (av1-frame-structure-parser-tile-rows parser)
     tile-rows
     (av1-frame-structure-parser-tile-columns-log2 parser)
     columns-log2
     (av1-frame-structure-parser-tile-rows-log2 parser)
     rows-log2)
    (if (or (plusp columns-log2) (plusp rows-log2))
        (let* ((identifier-bits (+ columns-log2 rows-log2))
               (identifier
                 (av1-structure-read-bits
                  reader identifier-bits
                  :tile-context-update))
               (tile-size-bytes
                 (1+
                  (av1-structure-read-bits
                   reader 2 :tile-size-bytes))))
          (when (>= identifier (* tile-columns tile-rows))
            (bridge-error
             "AV1_CONTEXT_UPDATE_TILE_ID_OUT_OF_RANGE id=~D count=~D"
             identifier (* tile-columns tile-rows)))
          (setf
           (av1-frame-structure-parser-context-update-tile-id
            parser)
           identifier
           (av1-frame-structure-parser-tile-size-bytes parser)
           tile-size-bytes))
        (setf
         (av1-frame-structure-parser-context-update-tile-id parser)
         0
         (av1-frame-structure-parser-tile-size-bytes parser)
         0))))

(defun av1-structure-read-delta-q (reader boundary)
  "read_delta_qを読み、signed deltaを返す。"
  (if (av1-structure-read-flag reader boundary)
      (av1-structure-read-signed-bits reader 7 boundary)
      0))

(defun av1-structure-parse-quantization (parser)
  "quantization_paramsを完全に読む。"
  (let* ((reader
           (av1-frame-structure-parser-reader parser))
         (sequence
           (av1-frame-structure-parser-sequence parser))
         (plane-count
           (av1-sequence-validation-state-plane-count sequence))
         (separate-p
           (av1-sequence-validation-state-separate-uv-delta-q-p
            sequence))
         (base
           (av1-structure-read-bits reader 8 :quantization))
         (delta-y
           (av1-structure-read-delta-q
            reader :quantization-y-dc))
         (delta-u-dc 0)
         (delta-u-ac 0)
         (delta-v-dc 0)
         (delta-v-ac 0))
    (when (> plane-count 1)
      (let ((different-uv-p
              (and
               separate-p
               (av1-structure-read-flag
                reader :quantization-different-uv))))
        (setf
         delta-u-dc
         (av1-structure-read-delta-q
          reader :quantization-u-dc)
         delta-u-ac
         (av1-structure-read-delta-q
          reader :quantization-u-ac))
        (if different-uv-p
            (setf
             delta-v-dc
             (av1-structure-read-delta-q
              reader :quantization-v-dc)
             delta-v-ac
             (av1-structure-read-delta-q
              reader :quantization-v-ac))
            (setf delta-v-dc delta-u-dc
                  delta-v-ac delta-u-ac))))
    (when (av1-structure-read-flag reader :quantization-matrix)
      (av1-structure-read-bits reader 4 :quantization-matrix-y)
      (av1-structure-read-bits reader 4 :quantization-matrix-u)
      (when separate-p
        (av1-structure-read-bits
         reader 4 :quantization-matrix-v)))
    (setf
     (av1-frame-structure-parser-base-q-index parser) base
     (av1-frame-structure-parser-delta-q-y-dc parser) delta-y
     (av1-frame-structure-parser-delta-q-u-dc parser) delta-u-dc
     (av1-frame-structure-parser-delta-q-u-ac parser) delta-u-ac
     (av1-frame-structure-parser-delta-q-v-dc parser) delta-v-dc
     (av1-frame-structure-parser-delta-q-v-ac parser) delta-v-ac)))

(defun av1-structure-primary-segmentation (parser)
  "primary referenceからsegmentation stateを複製する。"
  (let ((primary
          (av1-frame-structure-parser-primary-ref-frame parser)))
    (if (= primary +av1-structure-primary-ref-none+)
        (make-av1-segmentation-validation-state)
        (let* ((slot-index
                 (aref
                  (av1-frame-structure-parser-reference-indices
                   parser)
                  primary))
               (slot
                 (av1-structure-require-valid-reference
                  parser slot-index :primary-reference)))
          (copy-av1-segmentation-validation-state-deep
           (av1-reference-slot-validation-state-segmentation
            slot))))))

(defun av1-structure-parse-segmentation (parser)
  "segmentation_paramsを読み、次referenceへ保存するstateを作る。"
  (let* ((reader
           (av1-frame-structure-parser-reader parser))
         (primary
           (av1-frame-structure-parser-primary-ref-frame parser))
         (state
           (av1-structure-primary-segmentation parser))
         (enabled-p
           (av1-structure-read-flag reader :segmentation)))
    (setf
     (av1-segmentation-validation-state-enabled-p state)
     enabled-p)
    (cond
      ((not enabled-p)
       (setf state (make-av1-segmentation-validation-state)))
      (t
       (let ((update-data-p t))
         (unless (= primary +av1-structure-primary-ref-none+)
           (when
               (av1-structure-read-flag
                reader :segmentation-update-map)
             (av1-structure-read-bits
              reader 1 :segmentation-temporal-update))
           (setf update-data-p
                 (av1-structure-read-flag
                  reader :segmentation-update-data)))
         (when update-data-p
           (dotimes (segment +av1-structure-max-segments+)
             (dotimes
                 (feature +av1-structure-segment-feature-count+)
               (let ((feature-p
                       (av1-structure-read-flag
                        reader :segmentation-feature)))
                 (setf
                  (aref
                   (av1-segmentation-validation-state-feature-enabled
                    state)
                   segment feature)
                  (if feature-p 1 0)
                  (aref
                   (av1-segmentation-validation-state-feature-data
                    state)
                   segment feature)
                  (if feature-p
                      (let* ((bits
                               (aref
                                +av1-structure-segment-feature-bits+
                                feature))
                             (maximum
                               (aref
                                +av1-structure-segment-feature-maximum+
                                feature))
                             (value
                               (if
                                   (aref
                                    +av1-structure-segment-feature-signed+
                                    feature)
                                   (av1-structure-read-signed-bits
                                    reader (1+ bits)
                                    :segmentation-feature-data)
                                   (av1-structure-read-bits
                                    reader bits
                                    :segmentation-feature-data))))
                        (av1-structure-clip
                         (if
                             (aref
                              +av1-structure-segment-feature-signed+
                              feature)
                             (- maximum)
                             0)
                         maximum value))
                      0)))))))))
    (setf
     (av1-frame-structure-parser-segmentation parser)
     state)))

(defun av1-structure-compute-lossless (parser)
  "quantizerとsegmentationからCodedLossless/AllLosslessを計算する。"
  (let* ((segmentation
           (av1-frame-structure-parser-segmentation parser))
         (enabled
           (av1-segmentation-validation-state-feature-enabled
            segmentation))
         (data
           (av1-segmentation-validation-state-feature-data
            segmentation))
         (coded-lossless-p t))
    (dotimes (segment +av1-structure-max-segments+)
      (let ((q-index
              (av1-frame-structure-parser-base-q-index parser)))
        (when (= (aref enabled segment 0) 1)
          (setf q-index
                (av1-structure-clip
                 0 255
                 (+ q-index (aref data segment 0)))))
        (unless
            (and
             (zerop q-index)
             (zerop
              (av1-frame-structure-parser-delta-q-y-dc parser))
             (zerop
              (av1-frame-structure-parser-delta-q-u-dc parser))
             (zerop
              (av1-frame-structure-parser-delta-q-u-ac parser))
             (zerop
              (av1-frame-structure-parser-delta-q-v-dc parser))
             (zerop
              (av1-frame-structure-parser-delta-q-v-ac parser)))
          (setf coded-lossless-p nil))))
    (setf
     (av1-frame-structure-parser-coded-lossless-p parser)
     coded-lossless-p
     (av1-frame-structure-parser-all-lossless-p parser)
     (and
      coded-lossless-p
      (=
       (av1-frame-structure-parser-frame-width parser)
       (av1-frame-structure-parser-upscaled-width parser))))))

(defun av1-structure-parse-delta-parameters (parser)
  "delta_q_paramsとdelta_lf_paramsを読みdelta_q_presentを返す。"
  (let* ((reader
           (av1-frame-structure-parser-reader parser))
         (delta-q-present-p
           (and
            (plusp
             (av1-frame-structure-parser-base-q-index parser))
            (av1-structure-read-flag
             reader :delta-q))))
    (when delta-q-present-p
      (av1-structure-read-bits reader 2 :delta-q-resolution))
    (when delta-q-present-p
      (unless
          (av1-frame-structure-parser-allow-intrabc-p parser)
        (when
            (av1-structure-read-flag reader :delta-lf)
          (av1-structure-read-bits
           reader 2 :delta-lf-resolution)
          (av1-structure-read-bits
           reader 1 :delta-lf-multiple))))
    delta-q-present-p))

(defun av1-structure-parse-loop-filter (parser)
  "loop_filter_paramsを完全に読む。"
  (when
      (or
       (av1-frame-structure-parser-coded-lossless-p parser)
       (av1-frame-structure-parser-allow-intrabc-p parser))
    (return-from av1-structure-parse-loop-filter nil))
  (let* ((reader
           (av1-frame-structure-parser-reader parser))
         (sequence
           (av1-frame-structure-parser-sequence parser))
         (first-level
           (av1-structure-read-bits reader 6 :loop-filter))
         (second-level
           (av1-structure-read-bits reader 6 :loop-filter)))
    (when
        (and
         (> (av1-sequence-validation-state-plane-count sequence)
            1)
         (or (plusp first-level) (plusp second-level)))
      (av1-structure-read-bits reader 6 :loop-filter-chroma-u)
      (av1-structure-read-bits reader 6 :loop-filter-chroma-v))
    (av1-structure-read-bits reader 3 :loop-filter-sharpness)
    (when
        (av1-structure-read-flag
         reader :loop-filter-delta-enabled)
      (when
          (av1-structure-read-flag
           reader :loop-filter-delta-update)
        (dotimes (reference 8)
          (when
              (av1-structure-read-flag
               reader :loop-filter-reference-delta-update)
            (av1-structure-read-signed-bits
             reader 7 :loop-filter-reference-delta)))
        (dotimes (mode 2)
          (when
              (av1-structure-read-flag
               reader :loop-filter-mode-delta-update)
            (av1-structure-read-signed-bits
             reader 7 :loop-filter-mode-delta)))))))

(defun av1-structure-parse-cdef (parser)
  "cdef_paramsを完全に読む。"
  (let ((sequence
          (av1-frame-structure-parser-sequence parser)))
    (when
        (or
         (av1-frame-structure-parser-coded-lossless-p parser)
         (av1-frame-structure-parser-allow-intrabc-p parser)
         (not
          (av1-sequence-validation-state-enable-cdef-p
           sequence)))
      (return-from av1-structure-parse-cdef nil))
    (let* ((reader
             (av1-frame-structure-parser-reader parser))
           (plane-count
             (av1-sequence-validation-state-plane-count
              sequence))
           (cdef-bits
             (progn
               (av1-structure-read-bits
                reader 2 :cdef-damping)
               (av1-structure-read-bits
                reader 2 :cdef-bits))))
      (dotimes (index (ash 1 cdef-bits))
        (av1-structure-read-bits
         reader 4 :cdef-luma-primary)
        (av1-structure-read-bits
         reader 2 :cdef-luma-secondary)
        (when (> plane-count 1)
          (av1-structure-read-bits
           reader 4 :cdef-chroma-primary)
          (av1-structure-read-bits
           reader 2 :cdef-chroma-secondary))))))

(defun av1-structure-parse-loop-restoration (parser)
  "lr_paramsを完全に読む。"
  (let ((sequence
          (av1-frame-structure-parser-sequence parser)))
    (when
        (or
         (av1-frame-structure-parser-all-lossless-p parser)
         (av1-frame-structure-parser-allow-intrabc-p parser)
         (not
          (av1-sequence-validation-state-enable-restoration-p
           sequence)))
      (return-from av1-structure-parse-loop-restoration nil))
    (let ((reader
             (av1-frame-structure-parser-reader parser))
           (plane-count
             (av1-sequence-validation-state-plane-count
              sequence))
           (uses-restoration-p nil)
           (uses-chroma-p nil))
      (dotimes (plane plane-count)
        (let ((restoration-type
                (av1-structure-read-bits
                 reader 2 :loop-restoration-type)))
          (when (plusp restoration-type)
            (setf uses-restoration-p t)
            (when (plusp plane)
              (setf uses-chroma-p t)))))
      (when uses-restoration-p
        (if
            (av1-sequence-validation-state-use-128x128-superblock-p
             sequence)
            (av1-structure-read-bits
             reader 1 :loop-restoration-unit-shift)
            (when
                (av1-structure-read-flag
                 reader :loop-restoration-unit-shift)
              (av1-structure-read-bits
               reader 1 :loop-restoration-extra-shift)))
        (when
            (and
             (= (av1-sequence-validation-state-subsampling-x
                 sequence)
                1)
             (= (av1-sequence-validation-state-subsampling-y
                 sequence)
                1)
             uses-chroma-p)
          (av1-structure-read-bits
           reader 1 :loop-restoration-chroma-shift))))))

(defun av1-structure-skip-mode-allowed-p (parser)
  "現在のreference order hintsからskip mode可否を計算する。"
  (let ((sequence
          (av1-frame-structure-parser-sequence parser)))
    (when
        (or
         (av1-frame-structure-parser-frame-is-intra-p parser)
         (not
          (av1-frame-structure-parser-reference-select-p parser))
         (not
          (av1-sequence-validation-state-enable-order-hint-p
           sequence)))
      (return-from av1-structure-skip-mode-allowed-p nil))
    (let ((forward-index -1)
          (backward-index -1)
          (forward-hint 0)
          (backward-hint 0))
      (dotimes (index +av1-structure-references-per-frame+)
        (let* ((slot-index
                 (aref
                  (av1-frame-structure-parser-reference-indices
                   parser)
                  index))
               (hint
                 (av1-reference-slot-validation-state-order-hint
                  (av1-structure-require-valid-reference
                   parser slot-index :skip-mode)))
               (distance
                 (av1-structure-relative-distance
                  parser hint
                  (av1-frame-structure-parser-order-hint
                   parser))))
          (cond
            ((minusp distance)
             (when
                 (or
                  (minusp forward-index)
                  (plusp
                   (av1-structure-relative-distance
                    parser hint forward-hint)))
               (setf forward-index index
                     forward-hint hint)))
            ((plusp distance)
             (when
                 (or
                  (minusp backward-index)
                  (minusp
                   (av1-structure-relative-distance
                    parser hint backward-hint)))
               (setf backward-index index
                     backward-hint hint))))))
      (cond
        ((minusp forward-index) nil)
        ((not (minusp backward-index)) t)
        (t
         (loop for index from 0
                 below +av1-structure-references-per-frame+
               for slot-index =
                 (aref
                  (av1-frame-structure-parser-reference-indices
                   parser)
                  index)
               for hint =
                 (av1-reference-slot-validation-state-order-hint
                  (av1-structure-require-valid-reference
                   parser slot-index :skip-mode))
               thereis
               (minusp
                (av1-structure-relative-distance
                 parser hint forward-hint))))))))

(defun av1-structure-parse-transform-reference-skip (parser)
  "TX mode、frame reference mode、skip_mode_paramsを読む。"
  (let ((reader
          (av1-frame-structure-parser-reader parser)))
    (unless
        (av1-frame-structure-parser-coded-lossless-p parser)
      (av1-structure-read-bits reader 1 :transform-mode))
    (setf
     (av1-frame-structure-parser-reference-select-p parser)
     (and
      (not
       (av1-frame-structure-parser-frame-is-intra-p parser))
      (av1-structure-read-flag
       reader :frame-reference-mode)))
    (when (av1-structure-skip-mode-allowed-p parser)
      (av1-structure-read-bits reader 1 :skip-mode))))

(defun av1-structure-read-subexponential
    (reader symbol-count boundary)
  "AV1 decode_subexpのraw symbolを読む。"
  (let ((iteration 0)
        (base 0))
    (loop
      (let* ((bits
               (if (zerop iteration)
                   3
                   (+ 2 iteration)))
             (range (ash 1 bits)))
        (cond
          ((<= symbol-count (+ base (* 3 range)))
           (return
             (+
              base
              (av1-structure-read-ns
               reader (- symbol-count base) boundary))))
          ((av1-structure-read-flag reader boundary)
           (incf iteration)
           (incf base range))
          (t
           (return
             (+
              base
              (av1-structure-read-bits
               reader bits boundary)))))))))

(defun av1-structure-inverse-recenter (reference value)
  "AV1 inverse_recenterを計算する。"
  (cond
    ((> value (* 2 reference)) value)
    ((oddp value) (- reference (ash (1+ value) -1)))
    (t (+ reference (ash value -1)))))

(defun av1-structure-decode-unsigned-subexp-with-reference
    (reader maximum reference boundary)
  "decode_unsigned_subexp_with_refを読み値を返す。"
  (let ((value
          (av1-structure-read-subexponential
           reader maximum boundary)))
    (if (<= (ash reference 1) maximum)
        (av1-structure-inverse-recenter reference value)
        (-
         maximum 1
         (av1-structure-inverse-recenter
          (- maximum 1 reference)
          value)))))

(defun av1-structure-read-global-parameter
    (parser type parameter previous)
  "read_global_paramを読み、内部precision値を返す。"
  (let ((absolute-bits 12)
        (precision-bits 15))
    (when (< parameter 2)
      (if (= type 1)
          (setf
           absolute-bits
           (- 9
              (if
                  (av1-frame-structure-parser-allow-high-precision-mv-p
                   parser)
                  0 1))
           precision-bits
           (- 3
              (if
                  (av1-frame-structure-parser-allow-high-precision-mv-p
                   parser)
                  0 1)))
          (setf absolute-bits 12
                precision-bits 6)))
    (let* ((precision-difference
             (- +av1-structure-warped-model-precision-bits+
                precision-bits))
           (rounding
             (if (= (mod parameter 3) 2)
                 (ash
                  1 +av1-structure-warped-model-precision-bits+)
                 0))
           (subtraction
             (if (= (mod parameter 3) 2)
                 (ash 1 precision-bits)
                 0))
           (maximum (ash 1 absolute-bits))
           (reference-value
             (-
              (ash previous (- precision-difference))
              subtraction))
           (decoded
             (+
              (- maximum)
              (av1-structure-decode-unsigned-subexp-with-reference
               (av1-frame-structure-parser-reader parser)
               (1+ (* 2 maximum))
               (+ reference-value maximum)
               :global-motion-parameter))))
      (+
       (ash decoded precision-difference)
       rounding))))

(defun av1-structure-previous-global-motion (parser)
  "primary referenceからPrevGmParams相当を複製する。"
  (let ((primary
          (av1-frame-structure-parser-primary-ref-frame parser)))
    (if (= primary +av1-structure-primary-ref-none+)
        (make-av1-identity-global-motion)
        (let* ((slot-index
                 (aref
                  (av1-frame-structure-parser-reference-indices
                   parser)
                  primary))
               (slot
                 (av1-structure-require-valid-reference
                  parser slot-index :global-motion)))
          (av1-structure-copy-array
           (av1-reference-slot-validation-state-global-motion
            slot))))))

(defun av1-structure-parse-global-motion (parser)
  "global_motion_paramsを完全に読む。"
  (let ((parameters (make-av1-identity-global-motion)))
    (unless
        (av1-frame-structure-parser-frame-is-intra-p parser)
      (let ((previous
              (av1-structure-previous-global-motion parser))
            (reader
              (av1-frame-structure-parser-reader parser)))
        (dotimes
            (reference +av1-structure-references-per-frame+)
          (let ((type 0))
            (when
                (av1-structure-read-flag
                 reader :global-motion)
              (if
                  (av1-structure-read-flag
                   reader :global-motion-rotzoom)
                  (setf type 2)
                  (setf type
                        (if
                            (av1-structure-read-flag
                             reader :global-motion-translation)
                            1 3))))
            (when (>= type 2)
              (setf
               (aref parameters reference 2)
               (av1-structure-read-global-parameter
                parser type 2
                (aref previous reference 2))
               (aref parameters reference 3)
               (av1-structure-read-global-parameter
                parser type 3
                (aref previous reference 3)))
              (if (= type 3)
                  (setf
                   (aref parameters reference 4)
                   (av1-structure-read-global-parameter
                    parser type 4
                    (aref previous reference 4))
                   (aref parameters reference 5)
                   (av1-structure-read-global-parameter
                    parser type 5
                    (aref previous reference 5)))
                  (setf
                   (aref parameters reference 4)
                   (- (aref parameters reference 3))
                   (aref parameters reference 5)
                   (aref parameters reference 2))))
            (when (>= type 1)
              (setf
               (aref parameters reference 0)
               (av1-structure-read-global-parameter
                parser type 0
                (aref previous reference 0))
               (aref parameters reference 1)
               (av1-structure-read-global-parameter
                parser type 1
                (aref previous reference 1))))))))
    (setf
     (av1-frame-structure-parser-global-motion parser)
     parameters)))

(defun av1-structure-read-increasing-points
    (reader count maximum boundary)
  "film grain pointを読み、個数とx座標の単調増加を検証する。"
  (when (> count maximum)
    (bridge-error
     "AV1_FILM_GRAIN_POINT_COUNT_OUT_OF_RANGE boundary=~A count=~D maximum=~D"
     boundary count maximum))
  (let ((previous -1))
    (dotimes (index count)
      (let ((coordinate
              (av1-structure-read-bits reader 8 boundary)))
        (unless (> coordinate previous)
          (bridge-error
           "AV1_FILM_GRAIN_POINTS_NOT_INCREASING boundary=~A value=~D previous=~D"
           boundary coordinate previous))
        (setf previous coordinate))
      (av1-structure-read-bits reader 8 boundary))))

(defun av1-structure-parse-film-grain (parser)
  "film_grain_paramsを完全に読み、grain適用有無を返す。"
  (let ((sequence
           (av1-frame-structure-parser-sequence parser))
         (reader
           (av1-frame-structure-parser-reader parser)))
    (when
        (or
         (not
          (av1-sequence-validation-state-film-grain-params-present-p
           sequence))
         (and
          (not
           (av1-frame-structure-parser-show-frame-p parser))
          (not
           (av1-frame-structure-parser-showable-frame-p parser))))
      (return-from av1-structure-parse-film-grain nil))
    (unless
        (av1-structure-read-flag reader :film-grain-apply)
      (return-from av1-structure-parse-film-grain nil))
    (av1-structure-read-bits reader 16 :film-grain-seed)
    (when (= (av1-frame-structure-parser-frame-type parser) 1)
      (unless
          (av1-structure-read-flag reader :film-grain-update)
        (let ((reference
                (av1-structure-read-bits
                 reader 3 :film-grain-reference)))
          (unless
              (find
               reference
               (av1-frame-structure-parser-reference-indices
                parser)
               :test #'=)
            (bridge-error
             "AV1_FILM_GRAIN_REFERENCE_NOT_USED slot=~D"
             reference)))
        (return-from av1-structure-parse-film-grain t)))
    (let ((luma-count
             (av1-structure-read-bits
              reader 4 :film-grain-luma-points))
           (monochrome-p
             (av1-sequence-validation-state-monochrome-p
              sequence))
           (chroma-from-luma-p nil)
           (cb-count 0)
           (cr-count 0))
      (av1-structure-read-increasing-points
       reader luma-count 14 :film-grain-luma-points)
      (unless monochrome-p
        (setf chroma-from-luma-p
              (av1-structure-read-flag
               reader :film-grain-chroma-from-luma)))
      (unless
          (or
           monochrome-p
           chroma-from-luma-p
           (and
            (= (av1-sequence-validation-state-subsampling-x
                sequence)
               1)
            (= (av1-sequence-validation-state-subsampling-y
                sequence)
               1)
            (zerop luma-count)))
        (setf
         cb-count
         (av1-structure-read-bits
          reader 4 :film-grain-cb-points))
        (av1-structure-read-increasing-points
         reader cb-count 10 :film-grain-cb-points)
        (setf
         cr-count
         (av1-structure-read-bits
          reader 4 :film-grain-cr-points))
        (av1-structure-read-increasing-points
         reader cr-count 10 :film-grain-cr-points))
      (when
          (and
           (= (av1-sequence-validation-state-subsampling-x
               sequence)
              1)
           (= (av1-sequence-validation-state-subsampling-y
               sequence)
              1)
           (not (eql (zerop cb-count) (zerop cr-count))))
        (bridge-error
         "AV1_FILM_GRAIN_420_CHROMA_POINTS_MISMATCH cb=~D cr=~D"
         cb-count cr-count))
      (av1-structure-read-bits reader 2 :film-grain-scaling)
      (let* ((lag
               (av1-structure-read-bits
                reader 2 :film-grain-ar-lag))
             (luma-positions
               (* 2 lag (1+ lag)))
             (chroma-positions
               (+ luma-positions
                  (if (plusp luma-count) 1 0))))
        (when (plusp luma-count)
          (dotimes (index luma-positions)
            (av1-structure-read-bits
             reader 8 :film-grain-luma-coefficients)))
        (when (or chroma-from-luma-p (plusp cb-count))
          (dotimes (index chroma-positions)
            (av1-structure-read-bits
             reader 8 :film-grain-cb-coefficients)))
        (when (or chroma-from-luma-p (plusp cr-count))
          (dotimes (index chroma-positions)
            (av1-structure-read-bits
             reader 8 :film-grain-cr-coefficients))))
      (av1-structure-read-bits
       reader 2 :film-grain-coefficient-shift)
      (av1-structure-read-bits
       reader 2 :film-grain-scale-shift)
      (when (plusp cb-count)
        (av1-structure-read-bits reader 8 :film-grain-cb-multiplier)
        (av1-structure-read-bits reader 8 :film-grain-cb-luma)
        (av1-structure-read-bits reader 9 :film-grain-cb-offset))
      (when (plusp cr-count)
        (av1-structure-read-bits reader 8 :film-grain-cr-multiplier)
        (av1-structure-read-bits reader 8 :film-grain-cr-luma)
        (av1-structure-read-bits reader 9 :film-grain-cr-offset))
      (av1-structure-read-bits reader 1 :film-grain-overlap)
      (av1-structure-read-bits reader 1 :film-grain-clipping)
      t)))

(defun av1-structure-parse-tile-group
    (parser access-unit obu expected-start frame-obu-p)
  "一つのtile_group_obuを検証して範囲列と次のTileNumを返す。

EXPECTED-STARTで複数TYPE 4 OBUの順序と完全被覆を検証する。FRAME-OBU-P
ではAV1仕様どおり明示範囲を禁止する。entropy payloadの意味復号は
decoderの責務であり、size fieldとOBU終端までを構造検証する。"
  (let* ((reader
           (av1-frame-structure-parser-reader parser))
         (tile-count
           (*
            (av1-frame-structure-parser-tile-columns parser)
            (av1-frame-structure-parser-tile-rows parser)))
         (tile-size-bytes
           (av1-frame-structure-parser-tile-size-bytes parser))
         (range-present-p
           (and
            (> tile-count 1)
            (av1-structure-read-flag
             reader :tile-group-range-flag))))
    (when (and frame-obu-p range-present-p)
      (bridge-error
       "AV1_FRAME_OBU_TILE_RANGE_PRESENT"))
    (let* ((tile-bits
             (+
              (av1-frame-structure-parser-tile-columns-log2 parser)
              (av1-frame-structure-parser-tile-rows-log2 parser)))
           (tile-start
             (if range-present-p
                 (av1-structure-read-bits
                  reader tile-bits :tile-group-start)
                 0))
           (tile-end
             (if range-present-p
                 (av1-structure-read-bits
                  reader tile-bits :tile-group-end)
                 (1- tile-count))))
      (unless (= tile-start expected-start)
        (bridge-error
         "AV1_TILE_GROUP_START_MISMATCH expected=~D actual=~D"
         expected-start tile-start))
      (when (< tile-end tile-start)
        (bridge-error
         "AV1_TILE_GROUP_END_BEFORE_START start=~D end=~D"
         tile-start tile-end))
      (when (>= tile-end tile-count)
        (bridge-error
         "AV1_TILE_GROUP_END_OUT_OF_RANGE end=~D count=~D"
         tile-end tile-count))
      (av1-structure-byte-align-zero reader :tile-group)
      (let ((payload-end-bit
              (* (av1-obu-end obu) 8))
            (ranges
              (make-array (1+ (- tile-end tile-start)))))
        (loop for tile-index from tile-start to tile-end
              for range-index from 0
              do
          (let* ((last-tile-p (= tile-index tile-end))
                 (size
                   (if last-tile-p
                       (ash
                        (- payload-end-bit
                           (bit-reader-position reader))
                        -3)
                       (1+
                        (av1-structure-read-little-endian
                         reader tile-size-bytes
                         :tile-size))))
                 (remaining-bytes
                   (ash
                    (- payload-end-bit
                       (bit-reader-position reader))
                    -3))
                 (future-non-last
                   (max (- tile-end tile-index 1) 0))
                 (minimum-after-current
                   (+
                    size
                    (* future-non-last
                       (1+ tile-size-bytes))
                    (if last-tile-p 0 1))))
            (when (zerop size)
              (bridge-error
               "AV1_TILE_PAYLOAD_EMPTY tile=~D" tile-index))
            (when (> minimum-after-current remaining-bytes)
              (bridge-error
               "AV1_TILE_SIZE_TRUNCATED tile=~D size=~D remaining=~D"
               tile-index size remaining-bytes))
            (let* ((start-byte
                     (ash (bit-reader-position reader) -3))
                   (end-byte (+ start-byte size)))
              (when (> end-byte (av1-obu-end obu))
                (bridge-error
                 "AV1_TILE_END_EXCEEDS_OBU tile=~D end=~D obu_end=~D"
                 tile-index end-byte (av1-obu-end obu)))
              (setf
               (aref ranges range-index)
               (cons start-byte end-byte))
              (skip-bits reader (* size 8)))))
        (unless (= (bit-reader-position reader) payload-end-bit)
          (bridge-error
           "AV1_TILE_COVERAGE_INCOMPLETE consumed_bit=~D end_bit=~D"
           (bit-reader-position reader) payload-end-bit))
        (ensure-octet-range
         access-unit
         (av1-obu-payload-start obu)
         (- (av1-obu-end obu) (av1-obu-payload-start obu))
         :av1-frame-tile-coverage)
        (values ranges (1+ tile-end))))))

(defun av1-structure-parse-reference-indices (parser)
  "inter frameのreference indexとframe size分岐を読む。"
  (let* ((reader
           (av1-frame-structure-parser-reader parser))
         (sequence
           (av1-frame-structure-parser-sequence parser))
         (short-signaling-p
           (and
            (av1-sequence-validation-state-enable-order-hint-p
             sequence)
            (av1-structure-read-flag
             reader :reference-short-signaling))))
    (when short-signaling-p
        (let ((last-index
                (av1-structure-read-bits
                 reader 3 :reference-last))
              (golden-index
                (av1-structure-read-bits
                 reader 3 :reference-golden)))
          (av1-structure-require-valid-reference
           parser last-index :reference-last)
          (av1-structure-require-valid-reference
           parser golden-index :reference-golden)
          (av1-structure-set-short-references
           parser last-index golden-index)))
    (dotimes (reference +av1-structure-references-per-frame+)
      ;; ref_frame_idx and delta_frame_id_minus_1 share one normative
      ;; per-reference loop.  Keeping them interleaved is observable whenever
      ;; explicit reference signaling and frame IDs are both enabled.
      (unless short-signaling-p
          (setf
           (aref
            (av1-frame-structure-parser-reference-indices
             parser)
            reference)
           (av1-structure-read-bits
            reader 3 :reference-index)))
      (let* ((index
               (aref
                (av1-frame-structure-parser-reference-indices
                 parser)
                reference))
             (slot
               (av1-structure-require-valid-reference
                parser index :reference-index)))
        (when
            (av1-sequence-validation-state-frame-id-numbers-present-p
             sequence)
          (let* ((distance
                   (1+
                    (av1-structure-read-bits
                     reader
                     (av1-sequence-validation-state-delta-frame-id-length
                      sequence)
                     :reference-frame-id)))
                 (id-length
                   (+
                    (av1-sequence-validation-state-delta-frame-id-length
                     sequence)
                    (av1-sequence-validation-state-additional-frame-id-length
                     sequence)))
                 (expected
                   (mod
                    (+
                     (av1-frame-structure-parser-current-frame-id
                      parser)
                     (ash 1 id-length)
                     (- distance))
                    (ash 1 id-length))))
            (unless
                (=
                 expected
                 (av1-reference-slot-validation-state-frame-id
                  slot))
              (bridge-error
               "AV1_REFERENCE_FRAME_ID_MISMATCH reference=~D expected=~D actual=~D"
               reference expected
               (av1-reference-slot-validation-state-frame-id
                slot)))))))))

(defun av1-structure-parse-inter-frame-tail (parser)
  "inter frameのsize、motion header分岐を読む。"
  (let ((reader
           (av1-frame-structure-parser-reader parser))
         (sequence
           (av1-frame-structure-parser-sequence parser)))
    (av1-structure-parse-reference-indices parser)
    (cond
      ((and
        (av1-frame-structure-parser-frame-size-override-p
         parser)
        (not
         (av1-frame-structure-parser-error-resilient-mode-p
          parser)))
       (av1-structure-parse-frame-size-with-references parser))
      (t
       (av1-structure-parse-frame-size parser)
       (av1-structure-parse-render-size parser)))
    (setf
     (av1-frame-structure-parser-allow-high-precision-mv-p
      parser)
     (and
      (not
       (av1-frame-structure-parser-force-integer-mv-p parser))
      (av1-structure-read-flag
       reader :high-precision-motion-vector)))
    (unless
        (av1-structure-read-flag
         reader :interpolation-filter-switchable)
      (av1-structure-read-bits
       reader 2 :interpolation-filter))
    (av1-structure-read-bits
     reader 1 :motion-mode-switchable)
    (when
        (and
         (not
          (av1-frame-structure-parser-error-resilient-mode-p
           parser))
         (av1-sequence-validation-state-enable-ref-frame-mvs-p
          sequence))
      (av1-structure-read-bits
       reader 1 :reference-frame-mvs))))

(defun av1-structure-parse-temporal-point (parser)
  "show_frame直後のtemporal_point_infoを読む。"
  (let ((sequence
          (av1-frame-structure-parser-sequence parser)))
    (when
        (and
         (av1-frame-structure-parser-show-frame-p parser)
         (av1-sequence-validation-state-decoder-model-info-present-p
          sequence)
         (not
          (av1-sequence-validation-state-equal-picture-interval-p
           sequence)))
      (av1-structure-read-bits
       (av1-frame-structure-parser-reader parser)
       (av1-sequence-validation-state-frame-presentation-time-length
        sequence)
       :frame-presentation-time))))

(defun av1-structure-parse-decoder-model-times (parser)
  "primary_ref_frame直後のbuffer removal timeを読む。"
  (let ((reader
           (av1-frame-structure-parser-reader parser))
         (sequence
           (av1-frame-structure-parser-sequence parser)))
    (when
        (av1-sequence-validation-state-decoder-model-info-present-p
         sequence)
      (when
          (av1-structure-read-flag
           reader :buffer-removal-time-present)
        (loop for operating-point across
              (av1-sequence-validation-state-operating-points
               sequence)
              when
              (av1-operating-point-validation-state-decoder-model-present-p
               operating-point)
                do
          (let ((idc
                  (av1-operating-point-validation-state-idc
                   operating-point)))
            (when
                (or
                 (zerop idc)
                 (and
                  (logbitp
                   (av1-frame-structure-parser-temporal-id parser)
                   idc)
                  (logbitp
                   (+
                    8
                    (av1-frame-structure-parser-spatial-id parser))
                   idc)))
              (av1-structure-read-bits
               reader
               (av1-sequence-validation-state-buffer-removal-time-length
                sequence)
               :buffer-removal-time))))))))

(defun av1-structure-update-reference-state (parser)
  "完全検証済みframeをrefresh flagsに従って新stateへ保存する。"
  (let* ((state
           (av1-frame-structure-parser-working-state parser))
         (sequence
           (av1-frame-structure-parser-sequence parser))
         (slots
           (av1-frame-validation-state-reference-slots state))
         (flags
           (av1-frame-structure-parser-refresh-frame-flags parser)))
    (dotimes (index +av1-structure-reference-slot-count+)
      (when (logbitp index flags)
        (setf
         (aref slots index)
         (%make-av1-reference-slot-validation-state
          :valid-p t
          :showable-frame-p
          (av1-frame-structure-parser-showable-frame-p parser)
          :decoded-frame-serial
          (av1-frame-validation-state-next-decoded-frame-serial
           state)
          :frame-id
          (av1-frame-structure-parser-current-frame-id parser)
          :frame-type
          (av1-frame-structure-parser-frame-type parser)
          :order-hint
          (av1-frame-structure-parser-order-hint parser)
          :upscaled-width
          (av1-frame-structure-parser-upscaled-width parser)
          :frame-width
          (av1-frame-structure-parser-frame-width parser)
          :frame-height
          (av1-frame-structure-parser-frame-height parser)
          :render-width
          (av1-frame-structure-parser-render-width parser)
          :render-height
          (av1-frame-structure-parser-render-height parser)
          :profile
          (av1-sequence-validation-state-profile sequence)
          :bit-depth
          (av1-sequence-validation-state-bit-depth sequence)
          :subsampling-x
          (av1-sequence-validation-state-subsampling-x sequence)
          :subsampling-y
          (av1-sequence-validation-state-subsampling-y sequence)
          :segmentation
          (copy-av1-segmentation-validation-state-deep
           (av1-frame-structure-parser-segmentation parser))
          :global-motion
          (av1-structure-copy-array
           (av1-frame-structure-parser-global-motion parser))))))
    (setf
     (av1-frame-validation-state-current-frame-id state)
     (av1-frame-structure-parser-current-frame-id parser))
    (incf
     (av1-frame-validation-state-next-decoded-frame-serial state))
    state))

(defun av1-structure-load-parser-from-reference (parser slot)
  "show_existingで選んだSLOTのframe属性をPARSERへ反映する。"
  (let ((frame-type
          (av1-reference-slot-validation-state-frame-type slot)))
    (setf
     (av1-frame-structure-parser-frame-type parser) frame-type
     (av1-frame-structure-parser-frame-is-intra-p parser)
     (not (null (member frame-type '(0 2) :test #'=)))
     (av1-frame-structure-parser-show-frame-p parser) nil
     (av1-frame-structure-parser-showable-frame-p parser)
     (av1-reference-slot-validation-state-showable-frame-p slot)
     (av1-frame-structure-parser-current-frame-id parser)
     (av1-reference-slot-validation-state-frame-id slot)
     (av1-frame-structure-parser-order-hint parser)
     (av1-reference-slot-validation-state-order-hint slot)
     (av1-frame-structure-parser-upscaled-width parser)
     (av1-reference-slot-validation-state-upscaled-width slot)
     (av1-frame-structure-parser-frame-width parser)
     (av1-reference-slot-validation-state-frame-width slot)
     (av1-frame-structure-parser-frame-height parser)
     (av1-reference-slot-validation-state-frame-height slot)
     (av1-frame-structure-parser-render-width parser)
     (av1-reference-slot-validation-state-render-width slot)
     (av1-frame-structure-parser-render-height parser)
     (av1-reference-slot-validation-state-render-height slot)
     (av1-frame-structure-parser-refresh-frame-flags parser)
     (if (zerop frame-type) #xff 0)))
  parser)

(defun av1-structure-refresh-show-existing-key (parser slot)
  "show_existingで初回表示したdelayed keyを全reference slotへ反映する。"
  (let* ((state
           (av1-frame-structure-parser-working-state parser))
         (slots
           (av1-frame-validation-state-reference-slots state))
         (serial
           (av1-reference-slot-validation-state-decoded-frame-serial
            slot))
         (loaded
           (copy-av1-reference-slot-validation-state-deep slot)))
    (when
        (av1-reference-slot-validation-state-shown-via-show-existing-p
         slot)
      (bridge-error
       "AV1_SHOW_EXISTING_KEY_ALREADY_SHOWN serial=~D"
       serial))
    (setf
     (av1-reference-slot-validation-state-shown-via-show-existing-p
      loaded)
     t)
    (dotimes (index +av1-structure-reference-slot-count+)
      (setf
       (aref slots index)
       (copy-av1-reference-slot-validation-state-deep loaded)))
    (setf
     (av1-frame-validation-state-current-frame-id state)
     (av1-reference-slot-validation-state-frame-id loaded))
    state))

(defun av1-structure-parse-show-existing-header (parser)
  "show_existing_frameの参照、frame ID、showable条件を完全検証する。"
  (let* ((reader
           (av1-frame-structure-parser-reader parser))
         (sequence
           (av1-frame-structure-parser-sequence parser))
         (map-index
           (av1-structure-read-bits
            reader 3 :frame-to-show-map-index)))
    (when
        (and
         (av1-sequence-validation-state-decoder-model-info-present-p
          sequence)
         (not
          (av1-sequence-validation-state-equal-picture-interval-p
           sequence)))
      (av1-structure-read-bits
       reader
       (av1-sequence-validation-state-frame-presentation-time-length
        sequence)
       :show-existing-frame-presentation-time))
    (let ((display-frame-id nil))
      (when
          (av1-sequence-validation-state-frame-id-numbers-present-p
           sequence)
        (setf
         display-frame-id
         (av1-structure-read-bits
          reader
          (+
           (av1-sequence-validation-state-delta-frame-id-length
            sequence)
           (av1-sequence-validation-state-additional-frame-id-length
            sequence))
          :display-frame-id)))
      (let ((slot
              (av1-structure-require-valid-reference
               parser map-index :show-existing-frame)))
        (when
            (and
             display-frame-id
             (/=
              display-frame-id
              (av1-reference-slot-validation-state-frame-id slot)))
          (bridge-error
           "AV1_SHOW_EXISTING_FRAME_ID_MISMATCH slot=~D expected=~D actual=~D"
           map-index
           (av1-reference-slot-validation-state-frame-id slot)
           display-frame-id))
        (unless
            (av1-reference-slot-validation-state-showable-frame-p slot)
          (bridge-error
           "AV1_SHOW_EXISTING_REFERENCE_NOT_SHOWABLE slot=~D"
           map-index))
        (av1-structure-load-parser-from-reference parser slot)
        (when
            (zerop
             (av1-reference-slot-validation-state-frame-type slot))
          (av1-structure-refresh-show-existing-key parser slot))
        (make-av1-frame-semantics
         :show-existing-frame-p t
         :frame-to-show-map-index map-index
         :frame-type
         (av1-reference-slot-validation-state-frame-type slot)
         :show-frame-p nil
         :showable-frame-p
         (av1-reference-slot-validation-state-showable-frame-p
          slot))))))

(defun av1-structure-parse-uncompressed-header
    (parser &optional (obu-type 6))
  "FRAMEまたはFRAME_HEADER OBUのuncompressed_headerを末尾まで読む。"
  (let* ((reader
           (av1-frame-structure-parser-reader parser))
         (sequence
           (av1-frame-structure-parser-sequence parser))
         (reduced-p
           (av1-sequence-validation-state-reduced-still-picture-header-p
            sequence)))
    (cond
      (reduced-p
       (setf
        (av1-frame-structure-parser-frame-type parser) 0
        (av1-frame-structure-parser-frame-is-intra-p parser) t
        (av1-frame-structure-parser-show-frame-p parser) t
        (av1-frame-structure-parser-showable-frame-p parser) nil
        (av1-frame-structure-parser-error-resilient-mode-p parser) t))
      (t
       (when
           (av1-structure-read-flag
            reader :show-existing-frame)
         (when (= obu-type 6)
           (bridge-error
            "AV1_FRAME_OBU_SHOW_EXISTING_FORBIDDEN"))
         (return-from av1-structure-parse-uncompressed-header
           (av1-structure-parse-show-existing-header parser)))
       (let ((frame-type
               (av1-structure-read-bits
                reader 2 :frame-type)))
         (setf
          (av1-frame-structure-parser-frame-type parser)
          frame-type
          (av1-frame-structure-parser-frame-is-intra-p parser)
          (not
           (null
            (member frame-type '(0 2) :test #'=)))
          (av1-frame-structure-parser-show-frame-p parser)
          (av1-structure-read-flag reader :show-frame))
         (av1-structure-parse-temporal-point parser)
         (setf
          (av1-frame-structure-parser-showable-frame-p parser)
          (if
              (av1-frame-structure-parser-show-frame-p parser)
              (not (zerop frame-type))
              (av1-structure-read-flag
               reader :showable-frame)))
         (setf
          (av1-frame-structure-parser-error-resilient-mode-p
           parser)
          (if
              (or
               (= frame-type 3)
               (and
                (zerop frame-type)
                (av1-frame-structure-parser-show-frame-p
                 parser)))
              t
              (av1-structure-read-flag
               reader :error-resilient-mode))))))
    (when
        (and
         (zerop
          (av1-frame-structure-parser-frame-type parser))
         (av1-frame-structure-parser-show-frame-p parser))
      (dotimes (index +av1-structure-reference-slot-count+)
        (setf
         (av1-reference-slot-validation-state-valid-p
          (av1-structure-slot parser index))
         nil)))
    (setf
     (av1-frame-structure-parser-disable-cdf-update-p parser)
     (av1-structure-read-flag reader :disable-cdf-update))
    (let ((forced-screen
            (av1-sequence-validation-state-force-screen-content-tools
             sequence)))
      (setf
       (av1-frame-structure-parser-allow-screen-content-tools-p
        parser)
       (if
           (= forced-screen
              +av1-structure-select-screen-content-tools+)
           (av1-structure-read-flag
            reader :allow-screen-content-tools)
           (= forced-screen 1))))
    (setf
     (av1-frame-structure-parser-force-integer-mv-p parser)
     (and
      (av1-frame-structure-parser-allow-screen-content-tools-p
       parser)
      (let ((forced-integer
              (av1-sequence-validation-state-force-integer-mv
               sequence)))
        (if
            (= forced-integer
               +av1-structure-select-integer-mv+)
            (av1-structure-read-flag reader :force-integer-mv)
            (= forced-integer 1)))))
    (when
        (av1-frame-structure-parser-frame-is-intra-p parser)
      (setf
       (av1-frame-structure-parser-force-integer-mv-p parser)
       t))
    (if
        (av1-sequence-validation-state-frame-id-numbers-present-p
         sequence)
        (let* ((id-length
                 (+
                  (av1-sequence-validation-state-delta-frame-id-length
                   sequence)
                  (av1-sequence-validation-state-additional-frame-id-length
                   sequence)))
               (previous
                 (av1-frame-validation-state-current-frame-id
                  (av1-frame-structure-parser-working-state
                   parser)))
               (current
                 (av1-structure-read-bits
                  reader id-length :current-frame-id)))
          (setf
           (av1-frame-structure-parser-current-frame-id parser)
           current)
          (when
              (and
               previous
               (or
                (not
                 (zerop
                  (av1-frame-structure-parser-frame-type parser)))
                (not
                 (av1-frame-structure-parser-show-frame-p parser))))
            (let ((difference
                    (mod
                     (- current previous)
                     (ash 1 id-length))))
              (unless
                  (and
                   (plusp difference)
                   (< difference (ash 1 (- id-length 1))))
                (bridge-error
                 "AV1_CURRENT_FRAME_ID_DISCONTINUITY previous=~D current=~D"
                 previous current))))
          (av1-structure-mark-reference-frames
           parser id-length))
        (setf
         (av1-frame-structure-parser-current-frame-id parser)
         0))
    (setf
     (av1-frame-structure-parser-frame-size-override-p parser)
     (cond
       ((=
         (av1-frame-structure-parser-frame-type parser)
         3)
        t)
       (reduced-p nil)
       (t
        (av1-structure-read-flag
         reader :frame-size-override))))
    (setf
     (av1-frame-structure-parser-order-hint parser)
     (av1-structure-read-bits
      reader
      (av1-sequence-validation-state-order-hint-bits
       sequence)
      :order-hint)
     (av1-frame-structure-parser-primary-ref-frame parser)
     (if
         (or
          (av1-frame-structure-parser-frame-is-intra-p parser)
          (av1-frame-structure-parser-error-resilient-mode-p parser))
         +av1-structure-primary-ref-none+
         (av1-structure-read-bits
          reader 3 :primary-reference)))
    (av1-structure-parse-decoder-model-times parser)
    (setf
     (av1-frame-structure-parser-refresh-frame-flags parser)
     (if
         (or
          (=
           (av1-frame-structure-parser-frame-type parser)
           3)
          (and
           (zerop
            (av1-frame-structure-parser-frame-type parser))
           (av1-frame-structure-parser-show-frame-p parser)))
         #xff
         (av1-structure-read-bits
          reader 8 :refresh-frame-flags)))
    (when
        (and
         (=
          (av1-frame-structure-parser-frame-type parser)
          2)
         (=
          (av1-frame-structure-parser-refresh-frame-flags parser)
          #xff))
      (bridge-error
       "AV1_INTRA_ONLY_REFRESHES_ALL_REFERENCE_SLOTS"))
    (when
        (and
         (or
          (not
           (av1-frame-structure-parser-frame-is-intra-p parser))
          (/=
           (av1-frame-structure-parser-refresh-frame-flags parser)
           #xff))
         (av1-frame-structure-parser-error-resilient-mode-p parser)
         (av1-sequence-validation-state-enable-order-hint-p
          sequence))
      (dotimes (index +av1-structure-reference-slot-count+)
        (let ((hint
                (av1-structure-read-bits
                 reader
                 (av1-sequence-validation-state-order-hint-bits
                  sequence)
                 :error-resilient-reference-order-hint))
              (slot (av1-structure-slot parser index)))
          (unless
              (=
               hint
               (av1-reference-slot-validation-state-order-hint
                slot))
            (setf
             (av1-reference-slot-validation-state-valid-p slot)
             nil)))))
    (cond
      ((av1-frame-structure-parser-frame-is-intra-p parser)
       (av1-structure-parse-frame-size parser)
       (av1-structure-parse-render-size parser)
       (setf
        (av1-frame-structure-parser-allow-intrabc-p parser)
        (and
         (av1-frame-structure-parser-allow-screen-content-tools-p
          parser)
         (=
          (av1-frame-structure-parser-upscaled-width parser)
          (av1-frame-structure-parser-frame-width parser))
         (av1-structure-read-flag
          reader :allow-intrabc))))
      (t
       (av1-structure-parse-inter-frame-tail parser)))
    (unless
        (or reduced-p
            (av1-frame-structure-parser-disable-cdf-update-p
             parser))
      (av1-structure-read-bits
       reader 1 :disable-frame-end-cdf-update))
    (av1-structure-parse-tile-information parser)
    (av1-structure-parse-quantization parser)
    (av1-structure-parse-segmentation parser)
    (let ((delta-q-present-p
            (av1-structure-parse-delta-parameters parser)))
      (av1-structure-compute-lossless parser)
      (when
          (and delta-q-present-p
               (av1-frame-structure-parser-coded-lossless-p
                parser))
        (bridge-error
         "AV1_DELTA_Q_PRESENT_ON_CODED_LOSSLESS_FRAME")))
    (av1-structure-parse-loop-filter parser)
    (av1-structure-parse-cdef parser)
    (av1-structure-parse-loop-restoration parser)
    (av1-structure-parse-transform-reference-skip parser)
    (unless
        (or
         (av1-frame-structure-parser-frame-is-intra-p parser)
         (av1-frame-structure-parser-error-resilient-mode-p
          parser)
         (not
          (av1-sequence-validation-state-enable-warped-motion-p
           sequence)))
      (av1-structure-read-bits
       reader 1 :allow-warped-motion))
    (av1-structure-read-bits reader 1 :reduced-transform-set)
    (av1-structure-parse-global-motion parser)
    (av1-structure-parse-film-grain parser)
    (make-av1-frame-semantics
     :frame-type
     (av1-frame-structure-parser-frame-type parser)
     :show-frame-p
     (av1-frame-structure-parser-show-frame-p parser)
     :showable-frame-p
     (av1-frame-structure-parser-showable-frame-p parser))))

(defun validate-av1-frame-obu-structure
    (access-unit obu sequence-state frame-state)
  "TYPE 6 FRAME OBUを完全検証し、resultとcommit可能な次stateを返す。

FRAME-STATEは深く複製してから解析する。失敗時に入力stateは一切変更
されず、成功時だけ第二返値をcallerがcommitできる。"
  (unless (= (av1-obu-type obu) 6)
    (bridge-error
     "AV1_FRAME_STRUCTURE_REQUIRES_TYPE6_OBU type=~D"
     (av1-obu-type obu)))
  (multiple-value-bind (temporal-id spatial-id)
      (av1-structure-require-single-layer access-unit obu)
    (let* ((payload-start
             (av1-obu-payload-start obu))
           (reader
             (make-bit-reader
              access-unit
              :start payload-start
              :end (av1-obu-end obu)))
           (parser
             (%make-av1-frame-structure-parser
              :reader reader
              :sequence sequence-state
              :working-state
              (copy-av1-frame-validation-state-deep frame-state)
              :temporal-id temporal-id
              :spatial-id spatial-id))
           (semantics
             (av1-structure-parse-uncompressed-header parser)))
      (av1-structure-byte-align-zero
       reader :frame-header)
      (let ((header-bytes
              (ash
               (-
                (bit-reader-position reader)
                (* payload-start 8))
               -3)))
        (when
            (>= (bit-reader-position reader)
                (* (av1-obu-end obu) 8))
          (bridge-error
           "AV1_FRAME_OBU_TILE_GROUP_MISSING"))
        (multiple-value-bind (tile-ranges next-tile)
            (av1-structure-parse-tile-group
             parser access-unit obu 0 t)
          (unless
              (=
               next-tile
               (*
                (av1-frame-structure-parser-tile-columns parser)
                (av1-frame-structure-parser-tile-rows parser)))
            (bridge-error
             "AV1_FRAME_OBU_TILE_COVERAGE_INCOMPLETE next=~D"
             next-tile))
          ;; Reference slots are updated only after the complete tile layout
          ;; has been proven.  Entropy bytes themselves remain decoder-owned.
          (let ((next-state
                  (av1-structure-update-reference-state parser)))
            (values
             (make-av1-frame-structure-result
              :semantics semantics
              :header-bytes header-bytes
              :frame-type
              (av1-frame-structure-parser-frame-type parser)
              :refresh-frame-flags
              (av1-frame-structure-parser-refresh-frame-flags
               parser)
              :frame-width
              (av1-frame-structure-parser-frame-width parser)
              :frame-height
              (av1-frame-structure-parser-frame-height parser)
              :upscaled-width
              (av1-frame-structure-parser-upscaled-width parser)
              :render-width
              (av1-frame-structure-parser-render-width parser)
              :render-height
              (av1-frame-structure-parser-render-height parser)
              :tile-columns
              (av1-frame-structure-parser-tile-columns parser)
              :tile-rows
              (av1-frame-structure-parser-tile-rows parser)
              :context-update-tile-id
              (av1-frame-structure-parser-context-update-tile-id
               parser)
              :tile-size-bytes
              (av1-frame-structure-parser-tile-size-bytes parser)
              :tile-ranges tile-ranges)
             next-state)))))))

(defun av1-structure-obu-payload-equal-p
    (access-unit first second)
  "FIRSTとSECONDのOBU payloadがbyte-exactに一致するか返す。"
  (let ((first-length
          (- (av1-obu-end first)
             (av1-obu-payload-start first)))
        (second-length
          (- (av1-obu-end second)
             (av1-obu-payload-start second))))
    (and
     (= first-length second-length)
     (loop for index from 0 below first-length
           always
           (=
            (aref
             access-unit
             (+ (av1-obu-payload-start first) index))
            (aref
             access-unit
             (+ (av1-obu-payload-start second) index)))))))

(defun av1-structure-obu-extension-equal-p
    (access-unit first second)
  "関連OBUのextension flagとextension headerが一致するか返す。"
  (and
   (= (av1-obu-header-length first)
      (av1-obu-header-length second))
   (or
    (= (av1-obu-header-length first) 1)
    (=
     (aref access-unit (1+ (av1-obu-start first)))
     (aref access-unit (1+ (av1-obu-start second)))))))

(defun av1-structure-make-split-result
    (parser semantics header-bytes tile-ranges)
  "検証済みsplit frameから共通resultを作る。"
  (make-av1-frame-structure-result
   :semantics semantics
   :header-bytes header-bytes
   :frame-type
   (av1-frame-structure-parser-frame-type parser)
   :refresh-frame-flags
   (av1-frame-structure-parser-refresh-frame-flags parser)
   :frame-width
   (av1-frame-structure-parser-frame-width parser)
   :frame-height
   (av1-frame-structure-parser-frame-height parser)
   :upscaled-width
   (av1-frame-structure-parser-upscaled-width parser)
   :render-width
   (av1-frame-structure-parser-render-width parser)
   :render-height
   (av1-frame-structure-parser-render-height parser)
   :tile-columns
   (av1-frame-structure-parser-tile-columns parser)
   :tile-rows
   (av1-frame-structure-parser-tile-rows parser)
   :context-update-tile-id
   (av1-frame-structure-parser-context-update-tile-id parser)
   :tile-size-bytes
   (av1-frame-structure-parser-tile-size-bytes parser)
   :tile-ranges tile-ranges))

(defun validate-av1-split-frame-structure
    (access-unit obus frame-obu sequence-state frame-state)
  "TYPE 3 headerとTYPE 4 tile group列をAV1のSeenFrameHeader順で検証する。"
  (multiple-value-bind (temporal-id spatial-id)
      (av1-structure-require-single-layer access-unit frame-obu)
    (let* ((reader
             (make-bit-reader
              access-unit
              :start (av1-obu-payload-start frame-obu)
              :end (av1-obu-end frame-obu)))
           (parser
             (%make-av1-frame-structure-parser
              :reader reader
              :sequence sequence-state
              :working-state
              (copy-av1-frame-validation-state-deep frame-state)
              :temporal-id temporal-id
              :spatial-id spatial-id))
           (semantics
             (av1-structure-parse-uncompressed-header parser 3))
           (header-bytes
             (-
              (av1-obu-end frame-obu)
              (av1-obu-payload-start frame-obu)))
           (frame-position
             (position frame-obu obus :test #'eq)))
      (av1-structure-validate-trailing-bits
       reader :frame-header)
      (when
          (some
           (lambda (obu)
             (and
              (member (av1-obu-type obu) '(4 7) :test #'=)
              (< (av1-obu-start obu)
                 (av1-obu-start frame-obu))))
           obus)
        (bridge-error
         "AV1_FRAME_COMPONENT_PRECEDES_FRAME_HEADER"))
      (when
          (av1-frame-semantics-show-existing-frame-p semantics)
        (when
            (find-if
             (lambda (obu)
               (member (av1-obu-type obu) '(4 7) :test #'=))
             (nthcdr (1+ frame-position) obus))
          (bridge-error
           "AV1_SHOW_EXISTING_HAS_TILE_OR_REDUNDANT_OBU"))
        (return-from validate-av1-split-frame-structure
          (values
           (av1-structure-make-split-result
            parser semantics header-bytes #())
           (av1-frame-structure-parser-working-state parser))))
      (let* ((tile-count
               (*
                (av1-frame-structure-parser-tile-columns parser)
                (av1-frame-structure-parser-tile-rows parser)))
             (all-ranges (make-array tile-count))
             (next-tile 0)
             (seen-frame-header-p t))
        (dolist (obu (nthcdr (1+ frame-position) obus))
          (case (av1-obu-type obu)
            (7
             (unless seen-frame-header-p
               (bridge-error
                "AV1_REDUNDANT_FRAME_HEADER_WITHOUT_ACTIVE_FRAME"))
             (unless
                 (av1-structure-obu-extension-equal-p
                  access-unit frame-obu obu)
               (bridge-error
                "AV1_REDUNDANT_FRAME_HEADER_EXTENSION_MISMATCH"))
             (unless
                 (av1-structure-obu-payload-equal-p
                  access-unit frame-obu obu)
               (bridge-error
                "AV1_REDUNDANT_FRAME_HEADER_CONTENT_MISMATCH")))
            (4
             (unless seen-frame-header-p
               (bridge-error
                "AV1_TILE_GROUP_WITHOUT_ACTIVE_FRAME"))
             (unless
                 (av1-structure-obu-extension-equal-p
                  access-unit frame-obu obu)
               (bridge-error
                "AV1_TILE_GROUP_EXTENSION_MISMATCH"))
             (setf
              (av1-frame-structure-parser-reader parser)
              (make-bit-reader
               access-unit
               :start (av1-obu-payload-start obu)
               :end (av1-obu-end obu)))
             (multiple-value-bind (ranges following-tile)
                 (av1-structure-parse-tile-group
                  parser access-unit obu next-tile nil)
               (loop for range across ranges
                     for index from next-tile
                     do (setf (aref all-ranges index) range))
               (setf next-tile following-tile)
               (when (= next-tile tile-count)
                 (setf seen-frame-header-p nil))))
            (otherwise nil)))
        (unless (= next-tile tile-count)
          (bridge-error
           "AV1_SPLIT_TILE_COVERAGE_INCOMPLETE next=~D count=~D"
           next-tile tile-count))
        (let ((next-state
                (av1-structure-update-reference-state parser)))
          (values
           (av1-structure-make-split-result
            parser semantics header-bytes all-ranges)
           next-state))))))

(defun av1-structure-sequence-payload-equal-p
    (sequence-state access-unit sequence-obu)
  "保存済みsequence payloadとSEQUENCE-OBUがbyte-exactか返す。"
  (let ((payload
          (av1-sequence-validation-state-source-payload
           sequence-state))
        (length
          (-
           (av1-obu-end sequence-obu)
           (av1-obu-payload-start sequence-obu))))
    (and
     (= (length payload) length)
     (loop for index from 0 below length
           always
           (=
            (aref payload index)
            (aref
             access-unit
             (+ (av1-obu-payload-start sequence-obu) index)))))))

(defun validate-av1-frame-access-unit-structure
    (access-unit sequence-state frame-state)
  "ACCESS-UNITのbase-layer frame構造を検証してtransactional stateを返す。

TYPE 6とTYPE 3/4、show_existing、identical redundant header、Paddingを
受理する。複数layerと異なるsequence headerは入力subsetとして拒否する。"
  (let* ((obus (parse-av1-obus access-unit))
         (sequence-obus
           (remove-if-not
            (lambda (obu) (= (av1-obu-type obu) 1))
            obus))
         (frame-obus
           (remove-if-not
            (lambda (obu)
              (member (av1-obu-type obu) '(3 6) :test #'=))
            obus))
         (tile-obus
           (remove-if-not
            (lambda (obu) (= (av1-obu-type obu) 4))
            obus))
         (redundant-obus
           (remove-if-not
            (lambda (obu) (= (av1-obu-type obu) 7))
            obus)))
    (dolist (obu obus)
      (av1-structure-require-single-layer access-unit obu))
    (when (/= (length frame-obus) 1)
      (bridge-error
       "AV1_MULTIPLE_ACCESS_UNITS_IN_PES frame_header_count=~D"
       (length frame-obus)))
    (let* ((frame-obu (first frame-obus))
           (sequence-obu (first sequence-obus))
           (frame-type (av1-obu-type frame-obu))
           (repeated-sequence-p
             (and
              sequence-obu
              sequence-state
              (av1-structure-sequence-payload-equal-p
               sequence-state access-unit sequence-obu)))
           (new-sequence-p
             (and sequence-obu (not repeated-sequence-p)))
           (resolved-sequence
             (cond
               (sequence-obu
                (dolist (candidate sequence-obus)
                  (when
                      (>
                       (av1-obu-start candidate)
                       (av1-obu-start frame-obu))
                    (bridge-error
                     "AV1_SEQUENCE_HEADER_AFTER_FRAME"))
                  (unless
                      (av1-structure-obu-payload-equal-p
                       access-unit sequence-obu candidate)
                    (bridge-error
                     "AV1_INPUT_SUBSET_SEQUENCE_HEADERS_DIFFER")))
                (if repeated-sequence-p
                    sequence-state
                    (parse-av1-sequence-header-validation-state
                     access-unit sequence-obu)))
               (t
                (or
                 sequence-state
                 (bridge-error
                  "AV1_SEQUENCE_VALIDATION_STATE_MISSING")))))
           (resolved-frame-state
             (if new-sequence-p
                 (make-av1-frame-validation-state)
                 frame-state)))
      (when (and (= frame-type 6)
                 (or tile-obus redundant-obus))
        (bridge-error
         "AV1_FRAME_OBU_HAS_EXTERNAL_FRAME_COMPONENT"))
      (multiple-value-bind (result next-state)
          (if (= frame-type 6)
              (validate-av1-frame-obu-structure
               access-unit frame-obu resolved-sequence
               resolved-frame-state)
              (validate-av1-split-frame-structure
               access-unit obus frame-obu resolved-sequence
               resolved-frame-state))
        (when
            (and
             new-sequence-p
             sequence-state
             (or
              (av1-frame-semantics-show-existing-frame-p
               (av1-frame-structure-result-semantics result))
              (/=
               (av1-frame-structure-result-frame-type result)
               0)
              (not
               (av1-frame-semantics-show-frame-p
                (av1-frame-structure-result-semantics result)))))
          (bridge-error
           "AV1_NEW_CODED_SEQUENCE_REQUIRES_SHOWN_KEY_FRAME"))
        (values result next-state resolved-sequence)))))
