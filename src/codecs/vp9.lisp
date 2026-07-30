;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defconstant +vp9-frame-sync-code+ #x498342)
(defconstant +vp9-reference-slot-count+ 8)
(defconstant +vp9-refs-per-frame+ 3)

(defstruct vp9-frame-configuration
  (profile 0 :type (unsigned-byte 2))
  (bit-depth nil :type (or null (member 8 10 12)))
  (key-frame-p nil :type boolean)
  (intra-only-p nil :type boolean)
  (show-existing-frame-p nil :type boolean)
  (show-frame-p nil :type boolean)
  (error-resilient-p nil :type boolean)
  (width nil :type (or null (integer 1 65536)))
  (height nil :type (or null (integer 1 65536)))
  (render-width nil :type (or null (integer 1 65536)))
  (render-height nil :type (or null (integer 1 65536)))
  (color-space nil :type (or null (unsigned-byte 3)))
  (full-range-p nil :type boolean)
  (chroma-subsampling-x nil :type (or null bit))
  (chroma-subsampling-y nil :type (or null bit))
  (refresh-frame-flags 0 :type (unsigned-byte 8))
  (frame-to-show nil :type (or null (integer 0 7)))
  (uncompressed-header-bytes 0 :type (integer 0 *))
  (compressed-header-size 0 :type (integer 0 65535))
  (tile-columns 0 :type (integer 0 64))
  (tile-rows 0 :type (integer 0 4)))

(defstruct vp9-frame-structure
  (configuration (make-vp9-frame-configuration)
                 :type vp9-frame-configuration)
  (start 0 :type (integer 0 *))
  (end 0 :type (integer 0 *))
  (referenced-slots '() :type list)
  (tile-ranges '() :type list))

(defstruct (vp9-reference-slot
            (:constructor make-vp9-reference-slot
                (&key (valid-p nil) configuration)))
  (valid-p nil :type boolean)
  (configuration nil :type (or null vp9-frame-configuration)))

(defstruct (vp9-validation-state
            (:constructor %make-vp9-validation-state
                (slots current-configuration)))
  (slots #() :type vector)
  (current-configuration nil
                         :type (or null vp9-frame-configuration)))

(defun copy-vp9-configuration-or-nil (configuration)
  "CONFIGURATIONがあれば独立した複写を返す。"
  (and configuration
       (copy-vp9-frame-configuration configuration)))

(defun make-vp9-validation-state
    (&key slots current-configuration)
  "8個の明示reference slotを持つVP9検証stateを作る。"
  (let ((resolved-slots
          (cond
            (slots
             (unless (= (length slots) +vp9-reference-slot-count+)
               (bridge-error
                "VP9 validation state must contain exactly 8 slots"))
             (map 'vector
                  (lambda (slot)
                    (unless (typep slot 'vp9-reference-slot)
                      (bridge-error
                       "VP9 validation state contains an invalid slot"))
                    (make-vp9-reference-slot
                     :valid-p
                     (vp9-reference-slot-valid-p slot)
                     :configuration
                     (copy-vp9-configuration-or-nil
                      (vp9-reference-slot-configuration slot))))
                  slots))
            (t
             (make-array
              +vp9-reference-slot-count+
              :initial-contents
              (loop repeat +vp9-reference-slot-count+
                    collect (make-vp9-reference-slot)))))))
    (loop for slot across resolved-slots
          do
      (when (and (vp9-reference-slot-valid-p slot)
                 (null (vp9-reference-slot-configuration slot)))
        (bridge-error
         "A valid VP9 reference slot has no configuration"))
      (when (and (not (vp9-reference-slot-valid-p slot))
                 (vp9-reference-slot-configuration slot))
        (bridge-error
         "An invalid VP9 reference slot carries a configuration")))
    (%make-vp9-validation-state
     resolved-slots
     (copy-vp9-configuration-or-nil current-configuration))))

(defun copy-vp9-validation-state-deep (state)
  "STATEと全slot configurationを深く複写する。"
  (unless (typep state 'vp9-validation-state)
    (bridge-error "VP9 validation state has an invalid type"))
  (make-vp9-validation-state
   :slots (vp9-validation-state-slots state)
   :current-configuration
   (vp9-validation-state-current-configuration state)))

(defun vp9-reference-configuration (state index)
  "STATEのINDEXが有効ならconfigurationを返し、無効なら失敗する。"
  (unless (typep index '(integer 0 7))
    (bridge-error "VP9 reference slot index is invalid: ~S" index))
  (let ((slot (aref (vp9-validation-state-slots state) index)))
    (unless (and (vp9-reference-slot-valid-p slot)
                 (vp9-reference-slot-configuration slot))
      (bridge-error "VP9 reference slot ~D is not initialized" index))
    (vp9-reference-slot-configuration slot)))

(defun vp9-format-signature (configuration)
  "CONFIGURATIONのcodec formatと表示寸法を比較可能なlistで返す。"
  (list
   (vp9-frame-configuration-profile configuration)
   (vp9-frame-configuration-bit-depth configuration)
   (vp9-frame-configuration-width configuration)
   (vp9-frame-configuration-height configuration)
   (vp9-frame-configuration-render-width configuration)
   (vp9-frame-configuration-render-height configuration)
   (vp9-frame-configuration-color-space configuration)
   (vp9-frame-configuration-full-range-p configuration)
   (vp9-frame-configuration-chroma-subsampling-x configuration)
   (vp9-frame-configuration-chroma-subsampling-y configuration)))

(defun vp9-reference-image-format-compatible-p (left right)
  "LEFTとRIGHTをreference imageとして共用できるか返す。

VP9のreference buffer互換条件はbit depthとchroma subsamplingだけであり、
color space・rangeは表示metadataなので比較しない。"
  (equal
   (list
    (vp9-frame-configuration-bit-depth left)
    (vp9-frame-configuration-chroma-subsampling-x left)
    (vp9-frame-configuration-chroma-subsampling-y left))
   (list
    (vp9-frame-configuration-bit-depth right)
    (vp9-frame-configuration-chroma-subsampling-x right)
    (vp9-frame-configuration-chroma-subsampling-y right))))

(defun vp9-valid-reference-frame-size-p
    (reference-width reference-height width height)
  "REFERENCEと現在frameのVP9 scaling倍率が許可範囲か返す。"
  (and (>= (* 2 width) reference-width)
       (>= (* 2 height) reference-height)
       (<= width (* 16 reference-width))
       (<= height (* 16 reference-height))))

(defun parse-vp9-profile (reader)
  "VP9 uncompressed headerからprofileを読み、reserved bitも検証する。"
  (let* ((low (read-one-bit reader))
         (high (read-one-bit reader))
         (profile (logior low (ash high 1))))
    (when (and (= profile 3)
               (= (read-one-bit reader) 1))
      (bridge-error "VP9 profile 3 reserved_zero bit is not zero"))
    profile))

(defun parse-vp9-frame-prefix
    (frame &key (start 0) end)
  "逐次出力開始に必要な固定prefixだけを読み、frame種別を返す。

完全性やreference stateは検証せず、完成AUでは必ず
PARSE-VP9-ACCESS-UNITを別途実行する。"
  (let ((resolved-end (or end (length frame))))
    (when (<= resolved-end start)
      (bridge-error "VP9 frame prefix is empty"))
    (let ((reader
            (make-bit-reader frame
                             :start start
                             :end resolved-end)))
      (unless (= (read-bits reader 2) 2)
        (bridge-error "VP9 frame marker is invalid"))
      (let ((profile (parse-vp9-profile reader)))
        (when (= (read-one-bit reader) 1)
          (return-from parse-vp9-frame-prefix
            (make-vp9-frame-configuration
             :profile profile
             :show-existing-frame-p t
             :show-frame-p t
             :frame-to-show (read-bits reader 3))))
        (make-vp9-frame-configuration
         :profile profile
         :key-frame-p (zerop (read-one-bit reader))
         :show-frame-p (= (read-one-bit reader) 1)
         :error-resilient-p (= (read-one-bit reader) 1))))))

(defun parse-vp9-color-configuration (reader profile)
  "VP9 color_configを読み、profile固有規則を検証して返す。"
  (let ((bit-depth
          (if (>= profile 2)
              (if (= (read-one-bit reader) 1) 12 10)
              8))
        (color-space (read-bits reader 3)))
    (when (= color-space 6)
      (bridge-error "VP9 color space 6 is reserved"))
    (cond
      ((= color-space 7)
       (unless (member profile '(1 3) :test #'=)
         (bridge-error
          "VP9 RGB color space requires profile 1 or 3"))
       (unless (zerop (read-one-bit reader))
         (bridge-error
          "VP9 RGB color configuration reserved_zero bit is not zero"))
       (values bit-depth color-space t 0 0))
      ((member profile '(1 3) :test #'=)
       (let ((full-range-p (= (read-one-bit reader) 1))
             (subsampling-x (read-one-bit reader))
             (subsampling-y (read-one-bit reader)))
         (when (and (= subsampling-x 1)
                    (= subsampling-y 1))
           (bridge-error
            "VP9 profile 1 or 3 forbids 4:2:0 chroma subsampling"))
         (unless (zerop (read-one-bit reader))
           (bridge-error
            "VP9 color configuration reserved_zero bit is not zero"))
         (values bit-depth color-space full-range-p
                 subsampling-x subsampling-y)))
      (t
       (values bit-depth color-space
               (= (read-one-bit reader) 1)
               1 1)))))

(defun parse-vp9-frame-dimensions (reader)
  "VP9 frame_sizeの符号化寸法を読む。"
  (values (+ (read-bits reader 16) 1)
          (+ (read-bits reader 16) 1)))

(defun parse-vp9-render-dimensions (reader width height)
  "VP9 render_sizeを読む。"
  (if (= (read-one-bit reader) 1)
      (parse-vp9-frame-dimensions reader)
      (values width height)))

(defun validate-vp9-sync-code (reader)
  "VP9 frame sync codeを検証する。"
  (unless (= (read-bits reader 24) +vp9-frame-sync-code+)
    (bridge-error "VP9 frame sync code is invalid")))

(defun parse-vp9-loop-filter-parameters (reader)
  "VP9 loop_filter_paramsを末尾まで読む。"
  (read-bits reader 6)
  (read-bits reader 3)
  (when (= (read-one-bit reader) 1)
    (when (= (read-one-bit reader) 1)
      (loop repeat 4
            do
        (when (= (read-one-bit reader) 1)
          (read-bits reader 6)
          (read-one-bit reader)))
      (loop repeat 2
            do
        (when (= (read-one-bit reader) 1)
          (read-bits reader 6)
          (read-one-bit reader))))))

(defun parse-vp9-delta-q (reader)
  "VP9 delta_qを読む。"
  (when (= (read-one-bit reader) 1)
    (read-bits reader 4)
    (read-one-bit reader)))

(defun parse-vp9-quantization-parameters (reader)
  "VP9 quantization_paramsを末尾まで読む。"
  (read-bits reader 8)
  (parse-vp9-delta-q reader)
  (parse-vp9-delta-q reader)
  (parse-vp9-delta-q reader))

(defun parse-vp9-probability-update (reader)
  "VP9 probability updateの有無と値を読む。"
  (when (= (read-one-bit reader) 1)
    (read-bits reader 8)))

(defun parse-vp9-segmentation-feature (reader feature-index)
  "VP9 segmentation featureを1個読む。"
  (when (= (read-one-bit reader) 1)
    (ecase feature-index
      (0
       (read-bits reader 8)
       (read-one-bit reader))
      (1
       (read-bits reader 6)
       (read-one-bit reader))
      (2
       (read-bits reader 2))
      (3 nil))))

(defun parse-vp9-segmentation-parameters (reader)
  "VP9 segmentation_paramsを全segment・featureについて読む。"
  (when (= (read-one-bit reader) 1)
    (when (= (read-one-bit reader) 1)
      (loop repeat 7
            do (parse-vp9-probability-update reader))
      (when (= (read-one-bit reader) 1)
        (loop repeat 3
              do (parse-vp9-probability-update reader))))
    (when (= (read-one-bit reader) 1)
      (read-one-bit reader)
      (loop repeat 8
            do
        (loop for feature-index from 0 below 4
              do
          (parse-vp9-segmentation-feature
           reader feature-index))))))

(defun vp9-superblock-count (dimension)
  "DIMENSIONを覆う64 pixel superblock数を返す。"
  (ceiling dimension 64))

(defun vp9-tile-column-limits (width)
  "WIDTHからbitstream上のtile column log2最小・最大を返す。"
  (let ((superblock-columns (vp9-superblock-count width))
        (minimum 0)
        (maximum 1))
    (loop while (< (ash 64 minimum) superblock-columns)
          do (incf minimum))
    (loop while (>= (ash superblock-columns (- maximum)) 4)
          do (incf maximum))
    (decf maximum)
    (when (> minimum maximum)
      (bridge-error
       "VP9 frame width has no valid tile column layout: ~D"
       width))
    (values minimum maximum)))

(defun vp9-maximum-tile-row-log2 (height)
  "VP9 bitstreamで許可されるtile row log2上限を返す。"
  (declare (ignore height))
  2)

(defun parse-vp9-tile-information (reader width height)
  "VP9 tile_infoを読み、column境界と固定2-bit row構文を検証する。"
  (declare (ignore height))
  (multiple-value-bind (minimum maximum)
      (vp9-tile-column-limits width)
    (let ((column-log2 minimum))
      (loop while (< column-log2 maximum)
            do
        (if (= (read-one-bit reader) 1)
            (incf column-log2)
            (return)))
      (when (> column-log2 6)
        (bridge-error
         "VP9 tile columns exceed the decoder limit of 64"))
      (let ((row-log2
              (if (= (read-one-bit reader) 1)
                  (if (= (read-one-bit reader) 1) 2 1)
                  0)))
        (values (ash 1 column-log2)
                (ash 1 row-log2))))))

(defun make-vp9-intra-configuration
    (reader profile key-frame-p show-frame-p error-resilient-p
     &key read-refresh-flags-p)
  "VP9 key/intra-only frameのcolor・size構成を読む。"
  (validate-vp9-sync-code reader)
  (multiple-value-bind
        (bit-depth color-space full-range-p subsampling-x subsampling-y)
      (if (or key-frame-p (> profile 0))
          (parse-vp9-color-configuration reader profile)
          (values 8 1 nil 1 1))
    (let ((refresh-flags
            (if read-refresh-flags-p
                (read-bits reader 8)
                #xff)))
      (multiple-value-bind (width height)
          (parse-vp9-frame-dimensions reader)
      (multiple-value-bind (render-width render-height)
          (parse-vp9-render-dimensions reader width height)
          (values
           (make-vp9-frame-configuration
            :profile profile
            :bit-depth bit-depth
            :key-frame-p key-frame-p
            :intra-only-p (not key-frame-p)
            :show-frame-p show-frame-p
            :error-resilient-p error-resilient-p
            :width width
            :height height
            :render-width render-width
            :render-height render-height
            :color-space color-space
            :full-range-p full-range-p
            :chroma-subsampling-x subsampling-x
            :chroma-subsampling-y subsampling-y)
           refresh-flags))))))

(defun parse-vp9-inter-configuration
    (reader state profile show-frame-p error-resilient-p)
  "VP9 inter frameのreference・frame_size_with_refsを検証する。"
  (let ((current
          (vp9-validation-state-current-configuration state))
        (referenced-slots '()))
    (unless current
      (bridge-error
       "VP9 inter frame has no current decoded configuration"))
    ;; Inter frameはcolor configurationを再送しないため、profile変更は
    ;; decoder stateとの矛盾としてfail closedにする。
    (unless (= profile
               (vp9-frame-configuration-profile current))
      (bridge-error
       "VP9 inter frame profile contradicts decoder state"))
    (let ((refresh-flags (read-bits reader 8)))
      (loop repeat +vp9-refs-per-frame+
            do
        (let* ((slot-index (read-bits reader 3))
               (reference
                 (vp9-reference-configuration state slot-index)))
          (unless
              (vp9-reference-image-format-compatible-p
               reference current)
            (bridge-error
             "VP9 reference slot ~D has an incompatible format"
             slot-index))
          (push slot-index referenced-slots)
          (read-one-bit reader)))
      (setf referenced-slots (nreverse referenced-slots))
      (let ((width nil)
            (height nil))
        (dolist (slot-index referenced-slots)
          (when (= (read-one-bit reader) 1)
            (let ((reference
                    (vp9-reference-configuration
                     state slot-index)))
              (setf width
                    (vp9-frame-configuration-width reference)
                    height
                    (vp9-frame-configuration-height reference)))
            (return)))
        (unless width
          (multiple-value-setq (width height)
            (parse-vp9-frame-dimensions reader)))
        (unless
            (some
             (lambda (slot-index)
               (let ((reference
                       (vp9-reference-configuration
                        state slot-index)))
                 (vp9-valid-reference-frame-size-p
                  (vp9-frame-configuration-width reference)
                  (vp9-frame-configuration-height reference)
                  width height)))
             referenced-slots)
          (bridge-error
           "VP9 referenced frames have invalid scaling dimensions"))
        (multiple-value-bind (render-width render-height)
            (parse-vp9-render-dimensions reader width height)
          (read-one-bit reader)
          (unless (= (read-one-bit reader) 1)
            (read-bits reader 2))
          (let ((configuration
                  (copy-vp9-frame-configuration current)))
            (setf
             (vp9-frame-configuration-key-frame-p configuration)
             nil
             (vp9-frame-configuration-intra-only-p configuration)
             nil
             (vp9-frame-configuration-show-existing-frame-p
              configuration)
             nil
             (vp9-frame-configuration-show-frame-p configuration)
             show-frame-p
             (vp9-frame-configuration-error-resilient-p
              configuration)
             error-resilient-p
             (vp9-frame-configuration-width configuration)
             width
             (vp9-frame-configuration-height configuration)
             height
             (vp9-frame-configuration-render-width configuration)
             render-width
             (vp9-frame-configuration-render-height configuration)
             render-height
             (vp9-frame-configuration-refresh-frame-flags
              configuration)
             refresh-flags)
            (values configuration
                    refresh-flags
                    referenced-slots)))))))

(defun vp9-reader-byte-count (reader frame-start)
  "READERがFRAME-STARTから消費したbyte数を切り上げで返す。"
  (ceiling
   (- (bit-reader-position reader)
      (* frame-start 8))
   8))

(defun parse-vp9-complete-uncompressed-header
    (frame state &key (start 0) end)
  "VP9 uncompressed headerをheader_size_in_bytesまで完全に読む。"
  (let ((resolved-end (or end (length frame))))
    (when (<= resolved-end start)
      (bridge-error "VP9 frame is empty"))
    (let ((reader
            (make-bit-reader frame
                             :start start
                             :end resolved-end)))
      (unless (= (read-bits reader 2) 2)
        (bridge-error "VP9 frame marker is invalid"))
      (let ((profile (parse-vp9-profile reader)))
        (when (= (read-one-bit reader) 1)
          (let* ((frame-to-show (read-bits reader 3))
                 (reference
                   (vp9-reference-configuration
                    state frame-to-show))
                 (configuration
                   (copy-vp9-frame-configuration reference))
                 (header-bytes
                   (vp9-reader-byte-count reader start)))
            (unless (= profile
                       (vp9-frame-configuration-profile reference))
              (bridge-error
               "VP9 show_existing_frame profile contradicts its slot"))
            (setf
             (vp9-frame-configuration-key-frame-p configuration)
             nil
             (vp9-frame-configuration-intra-only-p configuration)
             nil
             (vp9-frame-configuration-show-existing-frame-p
              configuration)
             t
             (vp9-frame-configuration-show-frame-p configuration)
             t
             (vp9-frame-configuration-refresh-frame-flags
              configuration)
             0
             (vp9-frame-configuration-frame-to-show configuration)
             frame-to-show
             (vp9-frame-configuration-uncompressed-header-bytes
              configuration)
             header-bytes
             (vp9-frame-configuration-compressed-header-size
              configuration)
             0
             (vp9-frame-configuration-tile-columns configuration)
             0
             (vp9-frame-configuration-tile-rows configuration)
             0)
            (return-from parse-vp9-complete-uncompressed-header
              (values configuration 0 (list frame-to-show)))))
        (let ((key-frame-p (zerop (read-one-bit reader)))
              (show-frame-p (= (read-one-bit reader) 1))
              (error-resilient-p (= (read-one-bit reader) 1))
              (intra-only-p nil)
              (refresh-flags nil)
              (referenced-slots '())
              (configuration nil))
          (cond
            (key-frame-p
             (multiple-value-setq
                 (configuration refresh-flags)
               (make-vp9-intra-configuration
                reader profile t show-frame-p
                error-resilient-p)))
            (t
             (setf intra-only-p
                   (and (not show-frame-p)
                        (= (read-one-bit reader) 1)))
             (unless error-resilient-p
               (read-bits reader 2))
             (if intra-only-p
                 (multiple-value-setq
                     (configuration refresh-flags)
                   (make-vp9-intra-configuration
                    reader profile nil show-frame-p
                    error-resilient-p
                    :read-refresh-flags-p t))
                 (multiple-value-setq
                     (configuration refresh-flags
                      referenced-slots)
                   (parse-vp9-inter-configuration
                    reader state profile show-frame-p
                    error-resilient-p)))))
          (setf
           (vp9-frame-configuration-refresh-frame-flags
            configuration)
           refresh-flags)
          (unless error-resilient-p
            (read-one-bit reader)
            (read-one-bit reader))
          (read-bits reader 2)
          (parse-vp9-loop-filter-parameters reader)
          (parse-vp9-quantization-parameters reader)
          (parse-vp9-segmentation-parameters reader)
          (multiple-value-bind (tile-columns tile-rows)
              (parse-vp9-tile-information
               reader
               (vp9-frame-configuration-width configuration)
               (vp9-frame-configuration-height configuration))
            (let ((compressed-header-size
                    (read-bits reader 16))
                  (header-bytes
                    (vp9-reader-byte-count reader start)))
              (when (zerop compressed-header-size)
                (bridge-error
                 "VP9 compressed header size is zero"))
              (setf
               (vp9-frame-configuration-uncompressed-header-bytes
                configuration)
               header-bytes
               (vp9-frame-configuration-compressed-header-size
                configuration)
               compressed-header-size
               (vp9-frame-configuration-tile-columns configuration)
               tile-columns
               (vp9-frame-configuration-tile-rows configuration)
               tile-rows)
              (values configuration
                      refresh-flags
                      referenced-slots))))))))

(defun read-vp9-u32-be (octets offset end)
  "OCTETSのOFFSETからtile sizeをbig-endianで読む。"
  (when (> (+ offset 4) end)
    (bridge-error "VP9 tile size field is truncated"))
  (logior
   (ash (aref octets offset) 24)
   (ash (aref octets (+ offset 1)) 16)
   (ash (aref octets (+ offset 2)) 8)
   (aref octets (+ offset 3))))

(defun parse-vp9-tile-ranges
    (frame start end configuration)
  "FRAMEのcompressed header後をtile数どおり完全被覆して返す。"
  (let* ((header-end
           (+ start
              (vp9-frame-configuration-uncompressed-header-bytes
               configuration)))
         (compressed-end
           (+ header-end
              (vp9-frame-configuration-compressed-header-size
               configuration)))
         (tile-count
           (* (vp9-frame-configuration-tile-columns configuration)
              (vp9-frame-configuration-tile-rows configuration))))
    (when (> header-end end)
      (bridge-error "VP9 uncompressed header exceeds frame boundary"))
    (when (> compressed-end end)
      (bridge-error "VP9 compressed header exceeds frame boundary"))
    (unless (plusp tile-count)
      (bridge-error "VP9 frame has no tiles"))
    (let ((cursor compressed-end)
          (ranges '()))
      (loop for tile-index from 0 below tile-count
            do
        (if (= tile-index (- tile-count 1))
            (let ((tile-size (- end cursor)))
              (when (zerop tile-size)
                (bridge-error "VP9 final tile is empty"))
              (push (cons cursor end) ranges)
              (setf cursor end))
            (let ((tile-size
                    (read-vp9-u32-be frame cursor end)))
              (incf cursor 4)
              (when (zerop tile-size)
                (bridge-error "VP9 tile ~D is empty" tile-index))
              (when (> tile-size (- end cursor))
                (bridge-error
                 "VP9 tile ~D exceeds frame boundary"
                 tile-index))
              (push (cons cursor (+ cursor tile-size)) ranges)
              (incf cursor tile-size))))
      (unless (= cursor end)
        (bridge-error "VP9 tiles do not completely cover the frame"))
      (nreverse ranges))))

(defun update-vp9-validation-state
    (state configuration refresh-flags)
  "完全検証済みFRAMEをSTATEへ反映する。"
  (let ((next (copy-vp9-validation-state-deep state)))
    (unless (vp9-frame-configuration-show-existing-frame-p
             configuration)
      (setf
       (vp9-validation-state-current-configuration next)
       (copy-vp9-frame-configuration configuration))
      (loop for index from 0 below +vp9-reference-slot-count+
            when (logbitp index refresh-flags)
              do
        (setf
         (aref (vp9-validation-state-slots next) index)
         (make-vp9-reference-slot
          :valid-p t
          :configuration
          (copy-vp9-frame-configuration configuration)))))
    next))

(defun parse-vp9-frame-structure
    (frame state &key (start 0) end)
  "単一VP9 frameのheader・compressed header・全tileを検証する。"
  (let ((resolved-end (or end (length frame))))
    (multiple-value-bind
          (configuration refresh-flags referenced-slots)
        (parse-vp9-complete-uncompressed-header
         frame state :start start :end resolved-end)
      (let ((tile-ranges
              (cond
                ((vp9-frame-configuration-show-existing-frame-p
                  configuration)
                 (unless (= (+ start
                               (vp9-frame-configuration-uncompressed-header-bytes
                                configuration))
                            resolved-end)
                   (bridge-error
                    "VP9 show_existing_frame has trailing bytes"))
                 '())
                (t
                 (parse-vp9-tile-ranges
                  frame start resolved-end configuration)))))
        (values
         (make-vp9-frame-structure
          :configuration configuration
          :start start
          :end resolved-end
          :referenced-slots referenced-slots
          :tile-ranges tile-ranges)
         (update-vp9-validation-state
          state configuration refresh-flags))))))

(defun parse-vp9-uncompressed-header
    (frame &key (start 0) end state)
  "VP9 uncompressed headerを末尾の16-bit sizeまで検証して構成を返す。"
  (multiple-value-bind (configuration refresh-flags referenced-slots)
      (parse-vp9-complete-uncompressed-header
       frame
       (or state (make-vp9-validation-state))
       :start start
       :end end)
    (declare (ignore refresh-flags referenced-slots))
    configuration))

(defun parse-vp9-superframe-index (access-unit)
  "VP9 ACCESS-UNIT末尾のsuperframe indexを完全被覆で検証する。"
  (when (zerop (length access-unit))
    (bridge-error "VP9 access unit is empty"))
  (let ((marker (aref access-unit (- (length access-unit) 1))))
    (unless (= (logand marker #xe0) #xc0)
      (return-from parse-vp9-superframe-index
        (list (cons 0 (length access-unit)))))
    (let* ((frame-count (+ (logand marker #x07) 1))
           (magnitude (+ (ldb (byte 2 3) marker) 1))
           (index-size (+ 2 (* frame-count magnitude)))
           (index-start (- (length access-unit) index-size)))
      (when (<= index-start 0)
        (bridge-error "VP9 superframe index is truncated"))
      (unless (= (aref access-unit index-start) marker)
        (bridge-error "VP9 superframe index markers do not match"))
      (let ((ranges '())
            (frame-start 0)
            (offset (+ index-start 1)))
        (loop repeat frame-count
              do
          (let ((frame-size 0))
            (dotimes (byte-index magnitude)
              (setf frame-size
                    (logior
                     frame-size
                     (ash (aref access-unit
                                (+ offset byte-index))
                          (* byte-index 8)))))
            (when (zerop frame-size)
              (bridge-error
               "VP9 superframe contains an empty frame"))
            (when (> (+ frame-start frame-size) index-start)
              (bridge-error
               "VP9 superframe size exceeds indexed payload"))
            (push (cons frame-start (+ frame-start frame-size))
                  ranges)
            (incf frame-start frame-size)
            (incf offset magnitude)))
        (unless (= offset (- (length access-unit) 1))
          (bridge-error
           "VP9 superframe index byte coverage is invalid"))
        (unless (= frame-start index-start)
          (bridge-error
           "VP9 superframe sizes do not cover payload: declared=~D actual=~D"
           frame-start index-start))
        (nreverse ranges)))))

(defun validate-vp9-mapping-descriptors-dynamically (descriptors)
  "後段mapping moduleのvalidatorでDESCRIPTORSを検証する。"
  (let ((validator
          (find-symbol
           "VALIDATE-VP9-MAPPING-DESCRIPTORS"
           '#:konomitv-bs4k-tscodecbridge)))
    (unless (and validator (fboundp validator))
      (bridge-error "VP9 mapping descriptor validator is unavailable"))
    (funcall (symbol-function validator) descriptors)))

(defun validate-vp9-expected-configuration
    (actual expected)
  "ACTUALとmapping側EXPECTEDの完全構成が一致することを検証する。"
  (unless (typep expected 'vp9-frame-configuration)
    (bridge-error "Expected VP9 configuration has an invalid type"))
  (unless (equal (vp9-format-signature actual)
                 (vp9-format-signature expected))
    (bridge-error
     "VP9 bitstream configuration contradicts mapping configuration")))

(defun parse-vp9-access-unit
    (access-unit
     &key
       state
       mapping-descriptors
       expected-configuration)
  "VP9 ACCESS-UNITの全subframeをtransactionalに完全検証する。

既存互換の先頭2戻り値は先頭configurationとframe rangeであり、3番目に
全frame成功後のnext validation state、4番目に全frame structureを返す。"
  (when mapping-descriptors
    (validate-vp9-mapping-descriptors-dynamically
     mapping-descriptors))
  (let* ((input-state
           (or state (make-vp9-validation-state)))
         (working-state
           (copy-vp9-validation-state-deep input-state))
         (ranges
           (parse-vp9-superframe-index access-unit))
         (structures '())
         (baseline nil))
    (dolist (range ranges)
      (multiple-value-bind (structure next-state)
          (parse-vp9-frame-structure
           access-unit
           working-state
           :start (car range)
           :end (cdr range))
        (let ((configuration
                (vp9-frame-structure-configuration structure)))
          (unless baseline
            (setf baseline configuration))
          (push structure structures)
          (setf working-state next-state))))
    (when expected-configuration
      (validate-vp9-expected-configuration
       baseline expected-configuration))
    (values baseline
            ranges
            working-state
            (nreverse structures))))
