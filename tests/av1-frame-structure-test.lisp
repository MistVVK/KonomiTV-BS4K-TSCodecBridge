;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defstruct (av1-structure-test-writer
            (:constructor make-av1-structure-test-writer))
  (bits
   (make-array 256
               :element-type 'bit
               :adjustable t
               :fill-pointer 0)
   :type (array bit (*))))

(defun av1-structure-test-write
    (writer value count)
  "自作fixtureのVALUEをMSB-firstでCOUNT bit追加する。"
  (append-integer-bits
   (av1-structure-test-writer-bits writer)
   value count)
  writer)

(defun av1-structure-test-flag (writer value)
  "自作fixtureへboolean VALUEを追加する。"
  (av1-structure-test-write writer (if value 1 0) 1))

(defun av1-structure-test-align (writer)
  "自作fixtureをzero_bitでbyte境界へ揃える。"
  (loop until
        (zerop
         (mod
          (length (av1-structure-test-writer-bits writer))
          8))
        do (av1-structure-test-flag writer nil))
  writer)

(defun av1-structure-test-octets (writer)
  "自作fixture writerをoctet vectorへ変換する。"
  (bit-vector-to-octets
   (av1-structure-test-writer-bits writer)))

(defun make-av1-structure-test-obu
    (type payload &key extension-octet)
  "TYPEとPAYLOADからsize付きlow-overhead OBUを作る。"
  (let ((header
          (logior
           (ash type 3)
           #x02
           (if extension-octet #x04 0))))
    (if extension-octet
        (concatenate-octets
         (octets header extension-octet)
         (encode-uleb128 (length payload))
         payload)
        (concatenate-octets
         (octets header)
         (encode-uleb128 (length payload))
         payload))))

(defun make-av1-structure-test-sequence
    (&key (width 64) (height 64)
          (level 0)
          enable-cdef enable-restoration film-grain)
  "通常header・profile 0の自作sequence header OBUを作る。"
  (let ((writer (make-av1-structure-test-writer))
         (width-bits (integer-length (1- width)))
         (height-bits (integer-length (1- height))))
    (av1-structure-test-write writer 0 3)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-write writer 0 5)
    (av1-structure-test-write writer 0 12)
    (av1-structure-test-write writer level 5)
    (when (> level 7)
      (av1-structure-test-flag writer nil))
    (av1-structure-test-write writer (1- width-bits) 4)
    (av1-structure-test-write writer (1- height-bits) 4)
    (av1-structure-test-write writer (1- width) width-bits)
    (av1-structure-test-write writer (1- height) height-bits)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer enable-cdef)
    (av1-structure-test-flag writer enable-restoration)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-write writer 0 2)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer film-grain)
    (av1-structure-test-flag writer t)
    (av1-structure-test-align writer)
    (make-av1-structure-test-obu
     1 (av1-structure-test-octets writer))))

(defun av1-structure-test-write-common-quantization
    (writer base-q)
  "自作frameへ3-plane quantizationとsegmentationを追加する。"
  (av1-structure-test-write writer base-q 8)
  (av1-structure-test-flag writer nil)
  (av1-structure-test-flag writer nil)
  (av1-structure-test-flag writer nil)
  (av1-structure-test-flag writer nil))

(defun av1-structure-test-write-nonlossless-tools
    (writer &key enable-cdef enable-restoration film-grain)
  "base_q > 0のfilter/CDEF/restoration/film-grain構文を追加する。"
  (av1-structure-test-flag writer nil)
  (av1-structure-test-write writer 0 6)
  (av1-structure-test-write writer 0 6)
  (av1-structure-test-write writer 0 3)
  (av1-structure-test-flag writer t)
  (av1-structure-test-flag writer t)
  (loop repeat 8
        do (av1-structure-test-flag writer nil))
  (loop repeat 2
        do (av1-structure-test-flag writer nil))
  (when enable-cdef
    (av1-structure-test-write writer 0 2)
    (av1-structure-test-write writer 0 2)
    (av1-structure-test-write writer 0 4)
    (av1-structure-test-write writer 0 2)
    (av1-structure-test-write writer 0 4)
    (av1-structure-test-write writer 0 2))
  (when enable-restoration
    (av1-structure-test-write writer 1 2)
    (av1-structure-test-write writer 0 2)
    (av1-structure-test-write writer 0 2)
    (av1-structure-test-flag writer nil))
  (av1-structure-test-flag writer nil)
  (av1-structure-test-flag writer nil)
  (when film-grain
    (av1-structure-test-flag writer t)
    (av1-structure-test-write writer #x1234 16)
    (av1-structure-test-write writer 0 4)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-write writer 0 2)
    (av1-structure-test-write writer 0 2)
    (av1-structure-test-write writer 0 2)
    (av1-structure-test-write writer 0 2)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)))

(defun make-av1-structure-test-key-frame
    (&key two-tiles enable-cdef enable-restoration film-grain
          (show-frame-p t) split-group-count redundant-header-p)
  "自作key frameをTYPE 6またはTYPE 3/4 OBU列で作る。"
  (let ((writer (make-av1-structure-test-writer))
         (base-q
           (if
               (or enable-cdef enable-restoration film-grain)
               1 0)))
    (av1-structure-test-flag writer nil)
    (av1-structure-test-write writer 0 2)
    (av1-structure-test-flag writer show-frame-p)
    (unless show-frame-p
      (av1-structure-test-flag writer t)
      (av1-structure-test-flag writer t))
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (unless show-frame-p
      (av1-structure-test-write writer #xff 8))
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer t)
    (when two-tiles
      (av1-structure-test-flag writer t))
    (when two-tiles
      (av1-structure-test-write writer 0 1)
      (av1-structure-test-write writer 0 2))
    (av1-structure-test-write-common-quantization writer base-q)
    (av1-structure-test-flag writer nil)
    (if (zerop base-q)
        (av1-structure-test-flag writer nil)
        (av1-structure-test-write-nonlossless-tools
         writer
         :enable-cdef enable-cdef
         :enable-restoration enable-restoration
         :film-grain film-grain))
    (unless (zerop base-q)
      ;; The tool writer emits reduced_tx_set before film grain only when the
      ;; non-lossless helper has not already emitted it.
      nil)
    (when (and (zerop base-q) film-grain)
      (bridge-error "Invalid self fixture configuration"))
    (cond
      (split-group-count
          (unless
              (and
               (member split-group-count '(1 2) :test #'=)
               (or (= split-group-count 1) two-tiles))
            (bridge-error
             "Invalid split key fixture group count"))
          (av1-structure-test-flag writer t)
          (av1-structure-test-align writer)
          (let* ((header-payload
                   (av1-structure-test-octets writer))
                 (header
                   (make-av1-structure-test-obu
                    3 header-payload))
                 (redundant
                   (when redundant-header-p
                     (make-av1-structure-test-obu
                      7 header-payload))))
            (if (= split-group-count 1)
                (apply
                 #'concatenate-octets
                 (remove
                  nil
                  (list
                   header
                   redundant
                   (make-av1-structure-test-obu
                    4
                    (if two-tiles
                        (octets #x00 #x01 #xaa #xbb #xcc)
                        (octets #x80))))))
                (apply
                 #'concatenate-octets
                 (remove
                  nil
                  (list
                   header
                   (make-av1-structure-test-obu
                    4 (octets #x80 #xaa #xbb))
                   redundant
                   (make-av1-structure-test-obu
                    4 (octets #xe0 #xcc))))))))
      (t
       (av1-structure-test-align writer)
       (let ((header (av1-structure-test-octets writer)))
         (make-av1-structure-test-obu
          6
          (if two-tiles
              (concatenate-octets
               header
               (octets #x00 #x01 #xaa #xbb #xcc))
              (concatenate-octets header (octets #x80)))))))))

(defun make-av1-structure-test-inter-frame ()
  "直前key slot 0を参照する自作TYPE 6 inter FRAME OBUを作る。"
  (let ((writer (make-av1-structure-test-writer)))
    (av1-structure-test-flag writer nil)
    (av1-structure-test-write writer 1 2)
    (av1-structure-test-flag writer t)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-write writer 0 3)
    (av1-structure-test-write writer 1 8)
    (loop repeat 7
          do (av1-structure-test-write writer 0 3))
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer t)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-write-common-quantization writer 0)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (loop repeat 7
          do (av1-structure-test-flag writer nil))
    (av1-structure-test-align writer)
    (make-av1-structure-test-obu
     6
     (concatenate-octets
      (av1-structure-test-octets writer)
      (octets #x80)))))

(defun make-av1-structure-test-show-existing
    (map-index &key display-frame-id)
  "TYPE 3 show_existing_frame OBUをtrailing_bits付きで作る。"
  (let ((writer (make-av1-structure-test-writer)))
    (av1-structure-test-flag writer t)
    (av1-structure-test-write writer map-index 3)
    (when display-frame-id
      (av1-structure-test-write writer display-frame-id 3))
    (av1-structure-test-flag writer t)
    (av1-structure-test-align writer)
    (make-av1-structure-test-obu
     3 (av1-structure-test-octets writer))))

(defun make-av1-structure-test-frame-id-inter-frame ()
  "明示reference indexとframe ID差分を交互に持つinter OBUを作る。"
  (let ((writer (make-av1-structure-test-writer)))
    (av1-structure-test-flag writer nil)
    (av1-structure-test-write writer 1 2)
    (av1-structure-test-flag writer t)
    (av1-structure-test-flag writer t)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-write writer 1 3)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-write writer 1 8)
    (loop repeat 7
          do
      (av1-structure-test-write writer 0 3)
      (av1-structure-test-write writer 0 2))
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer t)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer t)
    (av1-structure-test-write-common-quantization writer 0)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (av1-structure-test-flag writer nil)
    (loop repeat 7
          do (av1-structure-test-flag writer nil))
    (av1-structure-test-align writer)
    (make-av1-structure-test-obu
     6
     (concatenate-octets
      (av1-structure-test-octets writer)
      (octets #x80)))))

(defun av1-structure-test-parse-key
    (&key (width 64) two-tiles enable-cdef
          enable-restoration film-grain)
  "自作sequence + keyを検証して3返値を返す。"
  (validate-av1-frame-access-unit-structure
   (concatenate-octets
    (make-av1-structure-test-sequence
     :width width
     :enable-cdef enable-cdef
     :enable-restoration enable-restoration
     :film-grain film-grain)
    (make-av1-structure-test-key-frame
     :two-tiles two-tiles
     :enable-cdef enable-cdef
     :enable-restoration enable-restoration
     :film-grain film-grain))
   nil
   (make-av1-frame-validation-state)))

(define-bridge-test av1-frame-structure-key-and-inter-state
  (multiple-value-bind (key-result key-state sequence)
      (av1-structure-test-parse-key)
    (check-bridge-test
     (= (av1-frame-structure-result-frame-type key-result) 0))
    (check-bridge-test
     (= (av1-frame-structure-result-refresh-frame-flags key-result)
        #xff))
    (check-bridge-test
     (every
      #'av1-reference-slot-validation-state-valid-p
      (av1-frame-validation-state-reference-slots key-state)))
    (multiple-value-bind (inter-result next-state resolved-sequence)
        (validate-av1-frame-access-unit-structure
         (make-av1-structure-test-inter-frame)
         sequence key-state)
      (check-bridge-test (eq resolved-sequence sequence))
      (check-bridge-test
       (= (av1-frame-structure-result-frame-type inter-result) 1))
      (check-bridge-test
       (av1-reference-slot-validation-state-valid-p
        (aref
         (av1-frame-validation-state-reference-slots next-state)
         0))))))

(define-bridge-test av1-frame-structure-rejects-reserved-sequence-levels
  (labels ((parse-sequence (access-unit)
             (parse-av1-sequence-header-validation-state
              access-unit
              (first (parse-av1-obus access-unit))))
           (make-reduced-prefix (level)
             (let ((writer (make-av1-structure-test-writer)))
               (av1-structure-test-write writer 0 3)
               (av1-structure-test-flag writer t)
               (av1-structure-test-flag writer t)
               (av1-structure-test-write writer level 5)
               (av1-structure-test-align writer)
               (make-av1-structure-test-obu
                1 (av1-structure-test-octets writer)))))
    (dolist (level '(24 25 26 27 28 29 30))
      (dolist (access-unit
               (list
                (make-av1-structure-test-sequence :level level)
                (make-reduced-prefix level)))
        (let ((message
                (handler-case
                    (progn (parse-sequence access-unit) nil)
                  (bridge-error (condition)
                    (bridge-error-message condition)))))
          (check-bridge-test
           (and message
                (search "AV1_SEQUENCE_LEVEL_RESERVED" message))))))
    (check-bridge-test
     (typep
      (parse-sequence
       (make-av1-structure-test-sequence :level 31))
      'av1-sequence-validation-state))))

(define-bridge-test av1-frame-structure-two-tile-coverage
  (multiple-value-bind (result state sequence)
      (av1-structure-test-parse-key :width 128 :two-tiles t)
    (declare (ignore state sequence))
    (let ((ranges
            (av1-frame-structure-result-tile-ranges result)))
      (check-bridge-test (= (length ranges) 2))
      (check-bridge-test
       (= (- (cdr (aref ranges 0))
             (car (aref ranges 0)))
          2))
      (check-bridge-test
       (= (- (cdr (aref ranges 1))
             (car (aref ranges 1)))
          1))
      (check-bridge-test
       (= (cdr (aref ranges 0))
          (car (aref ranges 1)))))))

(define-bridge-test av1-frame-structure-frame-id-fields-are-interleaved
  (let* ((sequence
           (make-av1-sequence-validation-state
            :frame-id-numbers-present-p t
            :delta-frame-id-length 2
            :additional-frame-id-length 1
            :maximum-frame-width 64
            :maximum-frame-height 64))
         (state (make-av1-frame-validation-state))
         (slot
           (aref
            (av1-frame-validation-state-reference-slots state)
            0)))
    (setf
     (av1-reference-slot-validation-state-valid-p slot)
     t
     (av1-reference-slot-validation-state-frame-id slot)
     0)
    (multiple-value-bind (result next-state resolved-sequence)
        (validate-av1-frame-access-unit-structure
         (make-av1-structure-test-frame-id-inter-frame)
         sequence state)
      (check-bridge-test (eq resolved-sequence sequence))
      (check-bridge-test
       (= (av1-frame-structure-result-frame-type result) 1))
      (check-bridge-test
       (= (av1-frame-validation-state-current-frame-id next-state)
          1))
      (check-bridge-test
       (=
        (av1-reference-slot-validation-state-frame-id
         (aref
          (av1-frame-validation-state-reference-slots next-state)
          0))
        1)))))

(define-bridge-test av1-frame-structure-complete-tool-branches
  (multiple-value-bind (result state sequence)
      (av1-structure-test-parse-key
       :enable-cdef t
       :enable-restoration t
       :film-grain t)
    (declare (ignore state sequence))
    (check-bridge-test
     (= (av1-frame-structure-result-frame-width result) 64))
    (check-bridge-test
     (= (length
         (av1-frame-structure-result-tile-ranges result))
        1))))

(define-bridge-test av1-frame-structure-rejects-short-placeholder
  (check-bridge-test
   (signals-bridge-error-p
    (lambda ()
      (validate-av1-frame-access-unit-structure
       (octets #x32 #x01 #x10)
       (make-av1-sequence-validation-state)
       (make-av1-frame-validation-state))))))

(define-bridge-test av1-frame-structure-rejects-multilayer
  (let ((payload (octets #x10)))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-av1-frame-access-unit-structure
         (make-av1-structure-test-obu
          6 payload :extension-octet #x20)
         (make-av1-sequence-validation-state)
         (make-av1-frame-validation-state)))))))

(define-bridge-test av1-frame-structure-split-groups-and-padding
  (let ((sequence
          (make-av1-structure-test-sequence :width 128))
        (padding
          (make-av1-structure-test-obu
           15 (octets 0 #xff 0 1 2 3))))
    (dolist (group-count '(1 2))
      (multiple-value-bind (result state resolved-sequence)
          (validate-av1-frame-access-unit-structure
           (concatenate-octets
            sequence
            padding
            (make-av1-structure-test-key-frame
             :two-tiles t
             :split-group-count group-count
             :redundant-header-p t))
           nil
           (make-av1-frame-validation-state))
        (declare (ignore state resolved-sequence))
        (check-bridge-test
         (= (length
             (av1-frame-structure-result-tile-ranges result))
            2))))))

(define-bridge-test av1-frame-structure-split-order-and-redundancy
  (let* ((sequence
           (make-av1-structure-test-sequence :width 128))
         (frame
           (make-av1-structure-test-key-frame
            :two-tiles t
            :split-group-count 2
            :redundant-header-p t))
         (access-unit (concatenate-octets sequence frame))
         (obus (parse-av1-obus access-unit))
         (redundant
           (find 7 obus :key #'av1-obu-type :test #'=))
         (tile-groups
           (remove-if-not
            (lambda (obu) (= (av1-obu-type obu) 4))
            obus)))
    (let ((damaged (copy-seq access-unit)))
      (setf
       (aref damaged (1- (av1-obu-end redundant)))
       (logxor
        (aref damaged (1- (av1-obu-end redundant)))
        1))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-av1-frame-access-unit-structure
           damaged nil
           (make-av1-frame-validation-state))))))
    (let ((wrong-start (copy-seq access-unit)))
      (setf
       (aref
        wrong-start
        (av1-obu-payload-start (second tile-groups)))
       #x80)
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-av1-frame-access-unit-structure
           wrong-start nil
           (make-av1-frame-validation-state))))))
    (let ((missing-final
            (subseq
             access-unit
             0
             (av1-obu-start (second tile-groups)))))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-av1-frame-access-unit-structure
           missing-final nil
           (make-av1-frame-validation-state))))))))

(define-bridge-test av1-frame-structure-show-existing-state
  (multiple-value-bind (hidden-result hidden-state sequence)
      (validate-av1-frame-access-unit-structure
       (concatenate-octets
        (make-av1-structure-test-sequence)
        (make-av1-structure-test-key-frame :show-frame-p nil))
       nil
       (make-av1-frame-validation-state))
    (check-bridge-test
     (not
      (av1-frame-semantics-show-frame-p
       (av1-frame-structure-result-semantics hidden-result))))
    (check-bridge-test
     (every
      #'av1-reference-slot-validation-state-showable-frame-p
      (av1-frame-validation-state-reference-slots hidden-state)))
    (multiple-value-bind (shown-result shown-state resolved-sequence)
        (validate-av1-frame-access-unit-structure
         (make-av1-structure-test-show-existing 0)
         sequence hidden-state)
      (check-bridge-test (eq resolved-sequence sequence))
      (check-bridge-test
       (av1-frame-semantics-show-existing-frame-p
        (av1-frame-structure-result-semantics shown-result)))
      (check-bridge-test
       (= (av1-frame-structure-result-refresh-frame-flags
           shown-result)
          #xff))
      (check-bridge-test
       (every
        #'av1-reference-slot-validation-state-shown-via-show-existing-p
        (av1-frame-validation-state-reference-slots shown-state)))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-av1-frame-access-unit-structure
           (make-av1-structure-test-show-existing 1)
           sequence shown-state))))))
  (multiple-value-bind (shown-key state sequence)
      (av1-structure-test-parse-key)
    (declare (ignore shown-key))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-av1-frame-access-unit-structure
         (make-av1-structure-test-show-existing 0)
         sequence state)))))
  (check-bridge-test
   (signals-bridge-error-p
    (lambda ()
      (validate-av1-frame-access-unit-structure
       (make-av1-structure-test-show-existing 0)
       (make-av1-sequence-validation-state)
       (make-av1-frame-validation-state))))))

(define-bridge-test av1-frame-structure-show-existing-frame-id
  (let* ((sequence
           (make-av1-sequence-validation-state
            :frame-id-numbers-present-p t
            :delta-frame-id-length 2
            :additional-frame-id-length 1))
         (state (make-av1-frame-validation-state))
         (slot
           (aref
            (av1-frame-validation-state-reference-slots state)
            0)))
    (setf
     (av1-reference-slot-validation-state-valid-p slot) t
     (av1-reference-slot-validation-state-showable-frame-p slot) t
     (av1-reference-slot-validation-state-frame-id slot) 3
     (av1-reference-slot-validation-state-decoded-frame-serial slot)
     7)
    (validate-av1-frame-access-unit-structure
     (make-av1-structure-test-show-existing
      0 :display-frame-id 3)
     sequence state)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-av1-frame-access-unit-structure
         (make-av1-structure-test-show-existing
          0 :display-frame-id 2)
         sequence state))))))

(define-bridge-test av1-frame-structure-identical-sequence-repetition
  (let ((sequence-obu (make-av1-structure-test-sequence))
        (key (make-av1-structure-test-key-frame)))
    (multiple-value-bind (result state sequence)
        (validate-av1-frame-access-unit-structure
         (concatenate-octets sequence-obu sequence-obu key)
         nil
         (make-av1-frame-validation-state))
      (declare (ignore result))
      (multiple-value-bind (inter-result next-state repeated-sequence)
          (validate-av1-frame-access-unit-structure
           (concatenate-octets
            sequence-obu
            (make-av1-structure-test-inter-frame))
           sequence state)
        (declare (ignore next-state))
        (check-bridge-test (eq repeated-sequence sequence))
        (check-bridge-test
         (= (av1-frame-structure-result-frame-type inter-result)
            1))))
    (let* ((different (copy-seq sequence-obu))
           (different-obu (first (parse-av1-obus different))))
      (setf
       (aref different (1- (av1-obu-end different-obu)))
       (logxor
        (aref different (1- (av1-obu-end different-obu)))
        1))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-av1-frame-access-unit-structure
           (concatenate-octets sequence-obu different key)
           nil
           (make-av1-frame-validation-state))))))))

(define-bridge-test av1-frame-structure-truncation-is-non-destructive
  (multiple-value-bind (key-result key-state sequence)
      (av1-structure-test-parse-key)
    (declare (ignore key-result))
    (let* ((slot
             (aref
              (av1-frame-validation-state-reference-slots key-state)
              0))
           (before-order
             (av1-reference-slot-validation-state-order-hint slot))
           (inter (make-av1-structure-test-inter-frame)))
      (loop for end from 2 below (length inter)
            do
        (let ((truncated (subseq inter 0 end)))
          (setf (aref truncated 1) (- end 2))
          (check-bridge-test
           (signals-bridge-error-p
            (lambda ()
              (validate-av1-frame-access-unit-structure
               truncated sequence key-state))))))
      (check-bridge-test
       (= before-order
          (av1-reference-slot-validation-state-order-hint
           (aref
            (av1-frame-validation-state-reference-slots key-state)
            0)))))))

(define-bridge-test av1-frame-structure-detects-tile-size-and-last-tile
  (let* ((sequence
           (make-av1-structure-test-sequence :width 128))
         (frame
           (make-av1-structure-test-key-frame :two-tiles t))
         (access-unit (concatenate-octets sequence frame)))
    (let ((missing-last
            (subseq access-unit 0 (1- (length access-unit)))))
      (setf
       (aref missing-last (+ (length sequence) 1))
       (- (length frame) 3))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-av1-frame-access-unit-structure
           missing-last nil
           (make-av1-frame-validation-state))))))
    (let ((oversized (copy-seq access-unit)))
      (let* ((frame-obu
               (find
                6 (parse-av1-obus oversized)
                :key #'av1-obu-type :test #'=))
             (result
               (nth-value
                0
                (validate-av1-frame-access-unit-structure
                 oversized nil
                 (make-av1-frame-validation-state))))
             (size-offset
               (car
                (aref
                 (av1-frame-structure-result-tile-ranges result)
                 0))))
        (declare (ignore frame-obu))
        (setf (aref oversized (1- size-offset)) #x7f)
        (check-bridge-test
         (signals-bridge-error-p
          (lambda ()
            (validate-av1-frame-access-unit-structure
             oversized nil
             (make-av1-frame-validation-state)))))))))
