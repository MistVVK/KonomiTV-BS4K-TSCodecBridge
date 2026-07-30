;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defun make-vp9-structure-test-bits ()
  "VP9構造test用の伸長可能bit列を作る。"
  (make-array 256
              :element-type 'bit
              :adjustable t
              :fill-pointer 0))

(defun write-vp9-test-profile
    (bits profile &key (reserved-zero 0))
  "BITSへVP9 profileを記録する。"
  (append-integer-bits bits (logand profile 1) 1)
  (append-integer-bits bits (ldb (byte 1 1) profile) 1)
  (when (= profile 3)
    (append-integer-bits bits reserved-zero 1)))

(defun write-vp9-test-color-configuration
    (bits profile
     &key
       (bit-depth 8)
       (color-space 1)
       (full-range 0)
       (subsampling-x 0)
       (subsampling-y 0)
       (reserved-zero 0))
  "BITSへprofile別のVP9 color_configを記録する。"
  (when (>= profile 2)
    (append-integer-bits bits
                         (if (= bit-depth 12) 1 0)
                         1))
  (append-integer-bits bits color-space 3)
  (cond
    ((= color-space 7)
     (when (member profile '(1 3) :test #'=)
       (append-integer-bits bits reserved-zero 1)))
    (t
     (append-integer-bits bits full-range 1)
     (when (member profile '(1 3) :test #'=)
       (append-integer-bits bits subsampling-x 1)
       (append-integer-bits bits subsampling-y 1)
       (append-integer-bits bits reserved-zero 1)))))

(defun write-vp9-test-dimensions (bits width height)
  "BITSへVP9 frame_sizeと同寸法のrender_sizeを記録する。"
  (append-integer-bits bits (- width 1) 16)
  (append-integer-bits bits (- height 1) 16)
  (append-integer-bits bits 0 1))

(defun write-vp9-test-loop-filter
    (bits &key exercise-updates-p)
  "BITSへ最小または全分岐を通るloop_filter_paramsを記録する。"
  (append-integer-bits bits 0 6)
  (append-integer-bits bits 0 3)
  (append-integer-bits bits
                       (if exercise-updates-p 1 0)
                       1)
  (when exercise-updates-p
    (append-integer-bits bits 1 1)
    (loop for index from 0 below 4
          do
      (append-integer-bits bits (if (zerop index) 1 0) 1)
      (when (zerop index)
        (append-integer-bits bits 1 6)
        (append-integer-bits bits 0 1)))
    (loop for index from 0 below 2
          do
      (append-integer-bits bits (if (zerop index) 1 0) 1)
      (when (zerop index)
        (append-integer-bits bits 1 6)
        (append-integer-bits bits 1 1)))))

(defun write-vp9-test-delta-q
    (bits present-p)
  "BITSへVP9 delta_qを記録する。"
  (append-integer-bits bits (if present-p 1 0) 1)
  (when present-p
    (append-integer-bits bits 1 4)
    (append-integer-bits bits 0 1)))

(defun write-vp9-test-quantization
    (bits &key exercise-deltas-p)
  "BITSへquantization_paramsを記録する。"
  (append-integer-bits bits 0 8)
  (loop repeat 3
        do
    (write-vp9-test-delta-q bits exercise-deltas-p)))

(defun write-vp9-test-probability-update
    (bits present-p)
  "BITSへsegmentation probability updateを記録する。"
  (append-integer-bits bits (if present-p 1 0) 1)
  (when present-p
    (append-integer-bits bits 128 8)))

(defun write-vp9-test-segmentation
    (bits &key exercise-all-p)
  "BITSへ最小または全分岐を通るsegmentation_paramsを記録する。"
  (append-integer-bits bits (if exercise-all-p 1 0) 1)
  (when exercise-all-p
    (append-integer-bits bits 1 1)
    (loop for index from 0 below 7
          do
      (write-vp9-test-probability-update
       bits (zerop index)))
    (append-integer-bits bits 1 1)
    (loop for index from 0 below 3
          do
      (write-vp9-test-probability-update
       bits (zerop index)))
    (append-integer-bits bits 1 1)
    (append-integer-bits bits 0 1)
    (loop for segment-index from 0 below 8
          do
      (loop for feature-index from 0 below 4
            for enabled-p =
              (and (zerop segment-index)
                   (< feature-index 3))
            do
        (append-integer-bits bits
                             (if enabled-p 1 0)
                             1)
        (when enabled-p
          (ecase feature-index
            (0
             (append-integer-bits bits 1 8)
             (append-integer-bits bits 0 1))
            (1
             (append-integer-bits bits 1 6)
             (append-integer-bits bits 1 1))
            (2
             (append-integer-bits bits 1 2))))))))

(defun write-vp9-test-tile-information
    (bits width height column-log2 row-log2)
  "BITSへ寸法境界内のtile_infoを記録する。"
  (multiple-value-bind (minimum maximum)
      (vp9-tile-column-limits width)
    (unless (<= minimum column-log2 maximum)
      (bridge-error "Test tile column log2 is outside its boundary"))
    (loop repeat (- column-log2 minimum)
          do (append-integer-bits bits 1 1))
    (when (< column-log2 maximum)
      (append-integer-bits bits 0 1)))
  (unless (<= 0 row-log2
              (vp9-maximum-tile-row-log2 height))
    (bridge-error "Test tile row log2 is outside its boundary"))
  (append-integer-bits bits
                       (if (plusp row-log2) 1 0)
                       1)
  (when (plusp row-log2)
    (append-integer-bits bits
                         (if (= row-log2 2) 1 0)
                         1)))

(defun write-vp9-test-common-header
    (bits width height
     &key
       error-resilient-p
       (column-log2 0)
       (row-log2 0)
       exercise-optional-branches-p
       (compressed-header-size 1))
  "BITSへ全frame共通headerをheader_sizeまで記録する。"
  (unless error-resilient-p
    (append-integer-bits bits 1 1)
    (append-integer-bits bits 1 1))
  (append-integer-bits bits 0 2)
  (write-vp9-test-loop-filter
   bits :exercise-updates-p exercise-optional-branches-p)
  (write-vp9-test-quantization
   bits :exercise-deltas-p exercise-optional-branches-p)
  (write-vp9-test-segmentation
   bits :exercise-all-p exercise-optional-branches-p)
  (write-vp9-test-tile-information
   bits width height column-log2 row-log2)
  (append-integer-bits bits compressed-header-size 16))

(defun vp9-test-u32-be (value)
  "VALUEを4-byte big-endian vectorにする。"
  (octets
   (ldb (byte 8 24) value)
   (ldb (byte 8 16) value)
   (ldb (byte 8 8) value)
   (ldb (byte 8 0) value)))

(defun make-vp9-test-frame-body
    (bits tile-count
     &key
       (actual-compressed-header-size 1)
       declared-tile-sizes
       actual-tile-sizes)
  "BITSへcompressed headerとtile payloadを連結する。"
  (let ((resolved-actual-sizes
          (or actual-tile-sizes
              (loop repeat tile-count collect 1)))
        (resolved-declared-sizes
          (or declared-tile-sizes
              (loop repeat (- tile-count 1) collect 1))))
    (unless (= (length resolved-actual-sizes) tile-count)
      (bridge-error "Test actual tile count is inconsistent"))
    (unless (= (length resolved-declared-sizes)
               (- tile-count 1))
      (bridge-error "Test declared tile count is inconsistent"))
    (apply
     #'concatenate-octets
     (bit-vector-to-octets bits)
     (make-array actual-compressed-header-size
                 :element-type 'octet
                 :initial-element 0)
     (loop for tile-index from 0 below tile-count
           for actual-size in resolved-actual-sizes
           append
       (append
        (unless (= tile-index (- tile-count 1))
          (list
           (vp9-test-u32-be
            (nth tile-index
                 resolved-declared-sizes))))
        (list
         (make-array actual-size
                     :element-type 'octet
                     :initial-element
                     (+ tile-index 1))))))))

(defun make-vp9-structure-test-key-frame
    (&key
       (profile 0)
       (bit-depth 8)
       (width 64)
       (height 64)
       (column-log2 0)
       (row-log2 0)
       exercise-optional-branches-p
       (profile-reserved-zero 0)
       (color-reserved-zero 0)
       (color-space 1)
       (full-range 0)
       (subsampling-x 0)
       (subsampling-y 0)
       (compressed-header-size 1)
       (actual-compressed-header-size 1)
       declared-tile-sizes
       actual-tile-sizes)
  "完全な自作VP9 key frameを作る。"
  (let ((bits (make-vp9-structure-test-bits)))
    (append-integer-bits bits 2 2)
    (write-vp9-test-profile
     bits profile :reserved-zero profile-reserved-zero)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 1 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits +vp9-frame-sync-code+ 24)
    (write-vp9-test-color-configuration
     bits profile
     :bit-depth bit-depth
     :color-space color-space
     :full-range full-range
     :subsampling-x subsampling-x
     :subsampling-y subsampling-y
     :reserved-zero color-reserved-zero)
    (write-vp9-test-dimensions bits width height)
    (write-vp9-test-common-header
     bits width height
     :column-log2 column-log2
     :row-log2 row-log2
     :exercise-optional-branches-p
     exercise-optional-branches-p
     :compressed-header-size compressed-header-size)
    (make-vp9-test-frame-body
     bits (ash 1 (+ column-log2 row-log2))
     :actual-compressed-header-size
     actual-compressed-header-size
     :declared-tile-sizes declared-tile-sizes
     :actual-tile-sizes actual-tile-sizes)))

(defun make-vp9-structure-test-intra-frame
    (&key
       (profile 0)
       (width 64)
       (height 64)
       (refresh-flags 1)
       (compressed-header-size 1))
  "完全な自作VP9 intra-only frameを作る。"
  (let ((bits (make-vp9-structure-test-bits)))
    (append-integer-bits bits 2 2)
    (write-vp9-test-profile bits profile)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 1 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 1 1)
    (append-integer-bits bits 0 2)
    (append-integer-bits bits +vp9-frame-sync-code+ 24)
    (when (> profile 0)
      (write-vp9-test-color-configuration bits profile))
    (append-integer-bits bits refresh-flags 8)
    (write-vp9-test-dimensions bits width height)
    (write-vp9-test-common-header
     bits width height
     :compressed-header-size compressed-header-size)
    (make-vp9-test-frame-body bits 1)))

(defun make-vp9-structure-test-inter-frame
    (&key
       (profile 0)
       (width 64)
       (height 64)
       (reference-indices '(0 0 0))
       (size-reference-position 0)
       (refresh-flags 1)
       (compressed-header-size 1)
       (actual-compressed-header-size 1))
  "完全な自作VP9 inter frameを作る。"
  (unless (= (length reference-indices) 3)
    (bridge-error "Test inter frame needs three references"))
  (let ((bits (make-vp9-structure-test-bits)))
    (append-integer-bits bits 2 2)
    (write-vp9-test-profile bits profile)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 1 1)
    (append-integer-bits bits 1 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 2)
    (append-integer-bits bits refresh-flags 8)
    (dolist (reference reference-indices)
      (append-integer-bits bits reference 3)
      (append-integer-bits bits 0 1))
    (loop for index from 0 below 3
          do
      (append-integer-bits
       bits
       (if (eql index size-reference-position) 1 0)
       1)
          (when (eql index size-reference-position)
        (return))
          finally
             (append-integer-bits bits (- width 1) 16)
             (append-integer-bits bits (- height 1) 16))
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 1 1)
    (write-vp9-test-common-header
     bits width height
     :compressed-header-size compressed-header-size)
    (make-vp9-test-frame-body
     bits 1
     :actual-compressed-header-size
     actual-compressed-header-size)))

(defun make-vp9-structure-test-show-existing
    (&key (profile 0) (slot-index 0) (reserved-zero 0))
  "自作VP9 show_existing_frameを作る。"
  (let ((bits (make-vp9-structure-test-bits)))
    (append-integer-bits bits 2 2)
    (write-vp9-test-profile
     bits profile :reserved-zero reserved-zero)
    (append-integer-bits bits 1 1)
    (append-integer-bits bits slot-index 3)
    (bit-vector-to-octets bits)))

(defun make-vp9-structure-test-superframe
    (frames magnitude)
  "FRAMESへ指定MAGNITUDEのVP9 superframe indexを付ける。"
  (unless (<= 1 (length frames) 8)
    (bridge-error "Test superframe count is outside 1..8"))
  (unless (<= 1 magnitude 4)
    (bridge-error "Test superframe magnitude is outside 1..4"))
  (let ((marker
          (logior #xc0
                  (ash (- magnitude 1) 3)
                  (- (length frames) 1))))
    (dolist (frame frames)
      (unless (< (length frame)
                 (ash 1 (* magnitude 8)))
        (bridge-error "Test frame does not fit superframe magnitude")))
    (apply
     #'concatenate-octets
     (append
      frames
      (list
       (make-array
        (+ 2 (* (length frames) magnitude))
        :element-type 'octet
        :initial-contents
        (append
         (list marker)
         (loop for frame in frames
               append
           (loop for byte-index from 0 below magnitude
                 collect
             (ldb (byte 8 (* byte-index 8))
                  (length frame))))
         (list marker))))))))

(defun make-vp9-structure-test-state-with-only-slot-zero
    (configuration)
  "CONFIGURATIONをslot 0だけへ置いたstateを作る。"
  (make-vp9-validation-state
   :slots
   (make-array
    8
    :initial-contents
    (cons
     (make-vp9-reference-slot
      :valid-p t
      :configuration configuration)
     (loop repeat 7
           collect (make-vp9-reference-slot))))
   :current-configuration configuration))

(defun vp9-test-prefix-signature (configuration)
  "Prefix/full parser間で一致すべき固定header fieldを返す。"
  (list
   (vp9-frame-configuration-profile configuration)
   (vp9-frame-configuration-key-frame-p configuration)
   (vp9-frame-configuration-show-existing-frame-p configuration)
   (vp9-frame-configuration-show-frame-p configuration)
   (vp9-frame-configuration-error-resilient-p configuration)
   (vp9-frame-configuration-frame-to-show configuration)))

(define-bridge-test vp9-structure-prefix-and-full-parser-contract
  (let ((key
          (make-vp9-structure-test-key-frame
           :profile 3
           :bit-depth 12)))
    (multiple-value-bind
          (full ranges state structures)
        (parse-vp9-access-unit key)
      (declare (ignore ranges structures))
      (check-bridge-test
       (equal
        (vp9-test-prefix-signature
         (parse-vp9-frame-prefix key))
        (vp9-test-prefix-signature full)))
      (let ((inter
              (make-vp9-structure-test-inter-frame
               :profile 3)))
        (multiple-value-bind
              (inter-full inter-ranges next-state inter-structures)
            (parse-vp9-access-unit inter :state state)
          (declare
           (ignore inter-ranges next-state inter-structures))
          (check-bridge-test
           (equal
            (vp9-test-prefix-signature
             (parse-vp9-frame-prefix inter))
            (vp9-test-prefix-signature inter-full)))))
      (let ((shown
              (make-vp9-structure-test-show-existing
               :profile 3)))
        (multiple-value-bind
              (shown-full shown-ranges next-state shown-structures)
            (parse-vp9-access-unit shown :state state)
          (declare
           (ignore shown-ranges next-state shown-structures))
          (check-bridge-test
           (equal
            (vp9-test-prefix-signature
             (parse-vp9-frame-prefix shown))
            (vp9-test-prefix-signature shown-full))))
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (parse-vp9-frame-prefix
             (subseq shown 0 1)))))))))

(define-bridge-test vp9-mapping-pes-validates-every-superframe
  (let* ((key
           (make-vp9-structure-test-key-frame))
         (inter
           (make-vp9-structure-test-inter-frame))
         (valid
           (make-vp9-structure-test-superframe
            (list key inter) 1))
         (corrupt
           (make-vp9-structure-test-superframe
            (list
             key
             (make-vp9-structure-test-inter-frame
              :compressed-header-size 32
              :actual-compressed-header-size 1))
            1)))
    (multiple-value-bind (configuration state)
        (validate-vp9-pes
         (make-pes #xe0 valid 90000 :data-alignment t)
         t)
      (check-bridge-test
       (vp9-frame-configuration-key-frame-p configuration))
      (check-bridge-test
       (vp9-reference-slot-valid-p
        (aref (vp9-validation-state-slots state) 0))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-vp9-pes
         (make-pes #xe0 corrupt 90000 :data-alignment t)
         t))))))

(define-bridge-test vp9-structure-key-inter-state-is-transactional
  (let ((key-frame
          (make-vp9-structure-test-key-frame)))
    (multiple-value-bind
          (key-configuration key-ranges key-state key-structures)
        (parse-vp9-access-unit key-frame)
      (check-bridge-test
       (vp9-frame-configuration-key-frame-p
        key-configuration))
      (check-bridge-test (= (length key-ranges) 1))
      (check-bridge-test (= (length key-structures) 1))
      (loop for slot across
            (vp9-validation-state-slots key-state)
            do
        (check-bridge-test
         (vp9-reference-slot-valid-p slot)))
      (let ((inter-frame
              (make-vp9-structure-test-inter-frame
               :refresh-flags 1)))
        (multiple-value-bind
              (inter-configuration ranges next-state structures)
            (parse-vp9-access-unit
             inter-frame :state key-state)
          (declare (ignore ranges structures))
          (check-bridge-test
           (not
            (vp9-frame-configuration-key-frame-p
             inter-configuration)))
          (check-bridge-test
           (vp9-frame-configuration-key-frame-p
            (vp9-reference-slot-configuration
             (aref (vp9-validation-state-slots key-state) 0))))
          (check-bridge-test
           (not
            (vp9-frame-configuration-key-frame-p
             (vp9-reference-slot-configuration
              (aref
               (vp9-validation-state-slots next-state)
               0))))))))))

(define-bridge-test vp9-structure-show-existing-validates-slot
  (let ((key-frame
          (make-vp9-structure-test-key-frame)))
    (multiple-value-bind
          (configuration ranges state structures)
        (parse-vp9-access-unit key-frame)
      (declare (ignore configuration ranges structures))
      (multiple-value-bind
            (shown shown-ranges next-state shown-structures)
          (parse-vp9-access-unit
           (make-vp9-structure-test-show-existing)
           :state state)
        (declare (ignore shown-ranges shown-structures))
        (check-bridge-test
         (vp9-frame-configuration-show-existing-frame-p shown))
        (check-bridge-test
         (equal
          (vp9-format-signature
           (vp9-validation-state-current-configuration state))
          (vp9-format-signature
           (vp9-validation-state-current-configuration
            next-state)))))))
  (check-bridge-test
   (signals-bridge-error-p
    (lambda ()
      (parse-vp9-access-unit
       (make-vp9-structure-test-show-existing))))))

(define-bridge-test vp9-structure-superframe-validates-all-magnitudes
  (loop for magnitude from 1 to 4
        for key =
          (make-vp9-structure-test-key-frame)
        for inter =
          (make-vp9-structure-test-inter-frame
           :refresh-flags 1)
        for access-unit =
          (make-vp9-structure-test-superframe
           (list key inter) magnitude)
        do
    (multiple-value-bind
          (configuration ranges state structures)
        (parse-vp9-access-unit access-unit)
      (declare (ignore state))
      (check-bridge-test
       (vp9-frame-configuration-key-frame-p configuration))
      (check-bridge-test (= (length ranges) 2))
      (check-bridge-test (= (length structures) 2)))))

(define-bridge-test vp9-structure-superframe-validates-count-one-to-eight
  (loop for frame-count from 1 to 8
        for frames =
          (cons
           (make-vp9-structure-test-key-frame)
           (loop repeat (- frame-count 1)
                 collect
             (make-vp9-structure-test-inter-frame)))
        for access-unit =
          (make-vp9-structure-test-superframe frames 1)
        do
    (multiple-value-bind
          (configuration ranges state structures)
        (parse-vp9-access-unit access-unit)
      (declare (ignore configuration state))
      (check-bridge-test (= (length ranges) frame-count))
      (check-bridge-test
       (= (length structures) frame-count)))))

(define-bridge-test vp9-structure-profile-and-optional-branches
  (dolist (entry '((0 8 1 1)
                   (1 8 0 0)
                   (2 10 1 1)
                   (3 12 0 0)))
    (destructuring-bind
          (profile bit-depth subsampling-x subsampling-y)
        entry
      (let ((frame
              (make-vp9-structure-test-key-frame
               :profile profile
               :bit-depth bit-depth
               :subsampling-x subsampling-x
               :subsampling-y subsampling-y
               :exercise-optional-branches-p t)))
        (multiple-value-bind
              (configuration ranges state structures)
            (parse-vp9-access-unit frame)
          (declare (ignore ranges state structures))
          (check-bridge-test
           (= (vp9-frame-configuration-profile configuration)
              profile))
          (check-bridge-test
           (= (vp9-frame-configuration-bit-depth configuration)
              bit-depth))))))
  (multiple-value-bind (configuration ranges state structures)
      (parse-vp9-access-unit
       (make-vp9-structure-test-intra-frame))
    (declare (ignore ranges state structures))
    (check-bridge-test
     (vp9-frame-configuration-intra-only-p configuration)))
  (multiple-value-bind
        (configuration ranges state structures)
      (parse-vp9-access-unit
       (make-vp9-structure-test-key-frame))
    (declare (ignore configuration ranges structures))
    (multiple-value-bind
          (explicit ranges-after next-state structures-after)
        (parse-vp9-access-unit
         (make-vp9-structure-test-inter-frame
          :size-reference-position nil)
         :state state)
      (declare
       (ignore ranges-after next-state structures-after))
      (check-bridge-test
       (= (vp9-frame-configuration-width explicit) 64))
      (check-bridge-test
       (= (vp9-frame-configuration-height explicit) 64)))))

(define-bridge-test vp9-structure-tile-coverage-and-corruption
  (let ((valid
          (make-vp9-structure-test-key-frame
           :width 512
           :height 128
           :column-log2 1
           :row-log2 1)))
    (multiple-value-bind
          (configuration ranges state structures)
        (parse-vp9-access-unit valid)
      (declare (ignore ranges state))
      (check-bridge-test
       (= (vp9-frame-configuration-tile-columns
           configuration)
          2))
      (check-bridge-test
       (= (vp9-frame-configuration-tile-rows
           configuration)
          2))
      (check-bridge-test
       (= (length
           (vp9-frame-structure-tile-ranges
            (first structures)))
          4))))
  (multiple-value-bind (configuration ranges state structures)
      (parse-vp9-access-unit
       (make-vp9-structure-test-key-frame
        :width 64
        :height 64
        :row-log2 2))
    (declare (ignore ranges state))
    (check-bridge-test
     (= (vp9-frame-configuration-tile-rows configuration) 4))
    (check-bridge-test
     (= (length
         (vp9-frame-structure-tile-ranges
          (first structures)))
        4)))
  (dolist (frame
           (list
            (make-vp9-structure-test-key-frame
             :width 512
             :column-log2 1
             :declared-tile-sizes '(0)
             :actual-tile-sizes '(1 1))
            (make-vp9-structure-test-key-frame
             :width 512
             :column-log2 1
             :declared-tile-sizes '(4096)
             :actual-tile-sizes '(1 1))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (parse-vp9-access-unit frame))))))

(define-bridge-test vp9-structure-rejects-column-count-above-decoder-limit
  (check-bridge-test
   (signals-bridge-error-p
    (lambda ()
      (parse-vp9-access-unit
       (make-vp9-structure-test-key-frame
        :width 32768
        :height 64
        :column-log2 7)))))
  (let ((bits (make-vp9-structure-test-bits)))
    (loop repeat 4
          do (append-integer-bits bits 1 1))
    (append-integer-bits bits 0 2)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (parse-vp9-tile-information
         (make-bit-reader (bit-vector-to-octets bits))
         32768
         64))))))

(define-bridge-test vp9-structure-header-size-boundaries
  (dolist (frame
           (list
            (make-vp9-structure-test-key-frame
             :compressed-header-size 0)
            (make-vp9-structure-test-key-frame
             :compressed-header-size 20
             :actual-compressed-header-size 1)))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (parse-vp9-access-unit frame))))))

(define-bridge-test vp9-structure-reference-scaling-boundaries
  (multiple-value-bind (configuration ranges state structures)
      (parse-vp9-access-unit
       (make-vp9-structure-test-key-frame
        :width 64 :height 64))
    (declare (ignore configuration ranges structures))
    (dolist (dimensions
             '((32 64)
               (1024 64)
               (64 32)
               (64 1024)))
      (destructuring-bind (width height) dimensions
      (multiple-value-bind
            (inter inter-ranges next-state inter-structures)
          (parse-vp9-access-unit
           (make-vp9-structure-test-inter-frame
            :width width
            :height height
            :size-reference-position nil)
           :state state)
        (declare
         (ignore inter-ranges next-state inter-structures))
        (check-bridge-test
         (and
          (= (vp9-frame-configuration-width inter) width)
          (= (vp9-frame-configuration-height inter) height))))))
    (dolist (dimensions
             '((31 64)
               (1025 64)
               (64 31)
               (64 1025)))
      (destructuring-bind (width height) dimensions
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (parse-vp9-access-unit
             (make-vp9-structure-test-inter-frame
              :width width
              :height height
              :size-reference-position nil)
             :state state))))))))

(define-bridge-test vp9-structure-reference-format-and-profile-policy
  (multiple-value-bind (configuration ranges state structures)
      (parse-vp9-access-unit
       (make-vp9-structure-test-key-frame))
    (declare (ignore ranges structures))
    (let* ((current
             (copy-vp9-frame-configuration configuration))
           (metadata-change-state
             (make-vp9-validation-state
              :slots (vp9-validation-state-slots state)
              :current-configuration current)))
      (setf (vp9-frame-configuration-color-space current) 2
            (vp9-validation-state-current-configuration
             metadata-change-state)
            (copy-vp9-frame-configuration current))
      (multiple-value-bind
            (inter inter-ranges next-state inter-structures)
          (parse-vp9-access-unit
           (make-vp9-structure-test-inter-frame)
           :state metadata-change-state)
        (declare
         (ignore inter-ranges next-state inter-structures))
        (check-bridge-test
         (= (vp9-frame-configuration-color-space inter) 2))))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (parse-vp9-access-unit
         (make-vp9-structure-test-inter-frame :profile 2)
         :state state)))))
  (multiple-value-bind (configuration ranges state structures)
      (parse-vp9-access-unit
       (make-vp9-structure-test-key-frame
        :profile 2
        :bit-depth 10))
    (declare (ignore ranges structures))
    (let ((current
            (copy-vp9-frame-configuration configuration)))
      (setf (vp9-frame-configuration-bit-depth current) 12)
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (parse-vp9-access-unit
           (make-vp9-structure-test-inter-frame :profile 2)
           :state
           (make-vp9-validation-state
            :slots (vp9-validation-state-slots state)
            :current-configuration current)))))))
  (multiple-value-bind (configuration ranges state structures)
      (parse-vp9-access-unit
       (make-vp9-structure-test-key-frame
        :profile 1
        :subsampling-x 0
        :subsampling-y 0))
    (declare (ignore ranges structures))
    (let ((current
            (copy-vp9-frame-configuration configuration)))
      (setf
       (vp9-frame-configuration-chroma-subsampling-y current)
       1)
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (parse-vp9-access-unit
           (make-vp9-structure-test-inter-frame :profile 1)
           :state
           (make-vp9-validation-state
            :slots (vp9-validation-state-slots state)
            :current-configuration current))))))))

(define-bridge-test vp9-structure-major-branch-truncations
  (let ((key
          (make-vp9-structure-test-key-frame))
        (intra
          (make-vp9-structure-test-intra-frame)))
    (multiple-value-bind
          (configuration ranges state structures)
        (parse-vp9-access-unit key)
      (declare (ignore configuration ranges structures))
      (let ((inter
              (make-vp9-structure-test-inter-frame))
            (shown
              (make-vp9-structure-test-show-existing)))
        (dolist (entry
                 (list
                  (cons key nil)
                  (cons intra nil)
                  (cons inter state)
                  (cons shown state)))
          (loop for end from 0 below (length (car entry))
                do
            (let ((prefix (subseq (car entry) 0 end))
                  (input-state (cdr entry)))
              (check-bridge-test
               (signals-bridge-error-p
                (lambda ()
                  (parse-vp9-access-unit
                   prefix :state input-state)))))))))))

(define-bridge-test vp9-structure-reserved-and-color-rules
  (dolist (frame
           (list
            (make-vp9-structure-test-key-frame
             :profile 3
             :bit-depth 10
             :profile-reserved-zero 1)
            (make-vp9-structure-test-key-frame
             :profile 1
             :subsampling-x 1
             :subsampling-y 1)
            (make-vp9-structure-test-key-frame
             :profile 1
             :color-reserved-zero 1)
            (make-vp9-structure-test-key-frame
             :color-space 6)))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (parse-vp9-access-unit frame))))))

(define-bridge-test vp9-structure-invalid-reference-rolls-back-au
  (let ((key
          (make-vp9-structure-test-key-frame)))
    (multiple-value-bind
          (configuration ranges full-state structures)
        (parse-vp9-access-unit key)
      (declare (ignore ranges full-state structures))
      (let* ((state
               (make-vp9-structure-test-state-with-only-slot-zero
                configuration))
             (first
               (make-vp9-structure-test-inter-frame
                :refresh-flags 2))
             (second
               (make-vp9-structure-test-inter-frame
                :reference-indices '(2 2 2)))
             (access-unit
               (make-vp9-structure-test-superframe
                (list first second) 1)))
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (parse-vp9-access-unit
             access-unit :state state))))
        (check-bridge-test
         (not
          (vp9-reference-slot-valid-p
           (aref (vp9-validation-state-slots state) 1))))))))

(define-bridge-test vp9-structure-corrupt-second-frame-rolls-back-au
  (let ((key
          (make-vp9-structure-test-key-frame)))
    (multiple-value-bind
          (configuration ranges full-state structures)
        (parse-vp9-access-unit key)
      (declare (ignore ranges full-state structures))
      (let* ((state
               (make-vp9-structure-test-state-with-only-slot-zero
                configuration))
             (first
               (make-vp9-structure-test-inter-frame
                :refresh-flags 2))
             (corrupt-second
               (make-vp9-structure-test-inter-frame
                :compressed-header-size 32
                :actual-compressed-header-size 1))
             (access-unit
               (make-vp9-structure-test-superframe
                (list first corrupt-second) 1)))
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (parse-vp9-access-unit
             access-unit :state state))))
        (check-bridge-test
         (not
          (vp9-reference-slot-valid-p
           (aref (vp9-validation-state-slots state) 1))))
        (check-bridge-test
         (vp9-frame-configuration-key-frame-p
          (vp9-validation-state-current-configuration
           state)))))))

(define-bridge-test vp9-structure-superframe-index-corruption
  (let* ((key
           (make-vp9-structure-test-key-frame))
         (inter
           (make-vp9-structure-test-inter-frame))
         (valid
           (make-vp9-structure-test-superframe
            (list key inter) 1))
         (bad-marker (copy-seq valid))
         (bad-size (copy-seq valid))
         (zero-size (copy-seq valid))
         (index-start (- (length valid) 4)))
    (setf (aref bad-marker index-start) #xc0)
    (incf (aref bad-size (+ index-start 1)))
    (setf (aref zero-size (+ index-start 1)) 0)
    (dolist (access-unit
             (list bad-marker bad-size zero-size))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (parse-vp9-access-unit access-unit)))))))

(define-bridge-test vp9-structure-configuration-and-mapping-consistency
  (let* ((first
           (make-vp9-structure-test-key-frame
            :width 64))
         (second
           (make-vp9-structure-test-key-frame
            :width 128))
         (mixed
           (make-vp9-structure-test-superframe
            (list first second) 1)))
    (multiple-value-bind
          (configuration ranges state structures)
        (parse-vp9-access-unit mixed)
      (declare (ignore ranges))
      (check-bridge-test
       (= (vp9-frame-configuration-width configuration) 64))
      (check-bridge-test (= (length structures) 2))
      (check-bridge-test
       (= (vp9-frame-configuration-width
           (vp9-frame-structure-configuration
            (second structures)))
          128))
      (check-bridge-test
       (= (vp9-frame-configuration-width
           (vp9-validation-state-current-configuration state))
          128)))
    (multiple-value-bind
          (configuration ranges state structures)
        (parse-vp9-access-unit first)
      (declare (ignore ranges state structures))
      (let ((expected
              (copy-vp9-frame-configuration configuration))
            (descriptors
              (make-vp9-mapping-descriptors)))
        (setf (vp9-frame-configuration-width expected) 128)
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (parse-vp9-access-unit
             first :expected-configuration expected))))
        (setf (aref
               (descriptor-payload (second descriptors))
               5)
              2)
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (parse-vp9-access-unit
             first :mapping-descriptors descriptors))))))))
