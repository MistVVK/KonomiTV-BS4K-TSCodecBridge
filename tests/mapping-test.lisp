;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defun make-test-vp9-key-frame (profile width height)
  "profile 0/2の完全な最小VP9 key frameを作る。"
  (let ((bits
          (make-array 64
                      :element-type 'bit
                      :adjustable t
                      :fill-pointer 0)))
    (append-integer-bits bits 2 2)
    (append-integer-bits bits (logand profile 1) 1)
    (append-integer-bits bits (ldb (byte 1 1) profile) 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 1 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits +vp9-frame-sync-code+ 24)
    (when (= profile 2)
      (append-integer-bits bits 0 1))
    (append-integer-bits bits 1 3)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits (- width 1) 16)
    (append-integer-bits bits (- height 1) 16)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 1 1)
    (append-integer-bits bits 1 1)
    (append-integer-bits bits 0 2)
    (append-integer-bits bits 0 6)
    (append-integer-bits bits 0 3)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 8)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 1)
    (multiple-value-bind (minimum maximum)
        (vp9-tile-column-limits width)
      (when (< minimum maximum)
        (append-integer-bits bits 0 1)))
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 1 16)
    (concatenate-octets
     (bit-vector-to-octets bits)
     (octets 0 1))))

(defun make-test-av1-non-reduced-access-unit ()
  "非reduced sequence headerを持つ最小AV1 key access unitを作る。"
  (let ((bits
          (make-array 128
                      :element-type 'bit
                      :adjustable t
                      :fill-pointer 0)))
    (append-integer-bits bits 0 3)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 5)
    (append-integer-bits bits 0 12)
    (append-integer-bits bits 8 5)
    (append-integer-bits bits 1 1)
    (append-integer-bits bits 5 4)
    (append-integer-bits bits 5 4)
    (append-integer-bits bits 63 6)
    (append-integer-bits bits 63 6)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 3)
    (append-integer-bits bits 0 4)
    (append-integer-bits bits 1 1)
    (append-integer-bits bits 0 2)
    (append-integer-bits bits 1 1)
    (append-integer-bits bits 1 1)
    (append-integer-bits bits 0 3)
    (append-integer-bits bits 0 3)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 2)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 1)
    (let ((sequence-payload (bit-vector-to-octets bits)))
      (concatenate-octets
       (octets #x0a)
       (encode-uleb128 (length sequence-payload))
       sequence-payload
       (octets #x32 1 #x10)))))

(define-bridge-test vp9-private-descriptor-exact-bytes
  (let ((descriptors (make-vp9-mapping-descriptors)))
    (validate-vp9-mapping-descriptors descriptors)
    (check-bridge-test
     (equalp (descriptor-payload (first descriptors))
             (octets #x56 #x50 #x30 #x39)))
    (check-bridge-test
     (equalp (descriptor-payload (second descriptors))
             (octets #x4b #x54 #x56 #x42
                     #x09 #x01 #xf0 #x00))))
  (let ((missing (make-vp9-mapping-descriptors))
        (reordered (make-vp9-mapping-descriptors))
        (registration (make-vp9-mapping-descriptors))
        (version (make-vp9-mapping-descriptors))
        (flags (make-vp9-mapping-descriptors))
        (reserved (make-vp9-mapping-descriptors)))
    (setf (aref (descriptor-payload (first registration)) 0)
          #x00
          (aref (descriptor-payload (second version)) 5)
          #x02
          (aref (descriptor-payload (second flags)) 6)
          #xf1
          (aref (descriptor-payload (second reserved)) 7)
          #x01)
    (dolist (invalid
             (list
              (list (first missing))
              (reverse reordered)
              registration
              version
              flags
              reserved))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (validate-vp9-mapping-descriptors invalid))))))
  (let ((descriptors (make-vp9-mapping-descriptors)))
    (setf (aref (descriptor-payload (second descriptors)) 4) #xff)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-vp9-mapping-descriptors descriptors))))))

(define-bridge-test vp9-profile-zero-and-two-key-frames
  (let ((profile-zero
          (parse-vp9-uncompressed-header
           (make-test-vp9-key-frame 0 640 360)))
        (profile-two
          (parse-vp9-uncompressed-header
           (make-test-vp9-key-frame 2 1920 1080))))
    (check-bridge-test
     (vp9-frame-configuration-key-frame-p profile-zero))
    (check-bridge-test
     (= (vp9-frame-configuration-profile profile-zero) 0))
    (check-bridge-test
     (= (vp9-frame-configuration-bit-depth profile-zero) 8))
    (check-bridge-test
     (= (vp9-frame-configuration-width profile-zero) 640))
    (check-bridge-test
     (= (vp9-frame-configuration-profile profile-two) 2))
    (check-bridge-test
     (= (vp9-frame-configuration-bit-depth profile-two) 10))
    (check-bridge-test
     (= (vp9-frame-configuration-height profile-two) 1080))))

(define-bridge-test vp9-superframe-index
  (let* ((first (make-test-vp9-key-frame 0 64 64))
         (second (octets #x86))
         (marker #xc1)
         (access-unit
           (concatenate
            '(simple-array (unsigned-byte 8) (*))
            first second
            (octets marker (length first) (length second) marker)))
         (ranges (parse-vp9-superframe-index access-unit)))
    (check-bridge-test (= (length ranges) 2))
    (check-bridge-test (= (cdr (first ranges)) (length first)))
    (check-bridge-test
     (= (cdr (second ranges)) (+ (length first) (length second))))))

(define-bridge-test av1-obu-start-code-and-escape
  (let* ((access-unit (octets #x0a #x03 0 0 1))
         (converted
           (convert-av1-access-unit-to-ts-format access-unit)))
    (check-bridge-test
     (equalp converted
             (octets 0 0 1 #x08 0 0 #x03 1)))))

(define-bridge-test av1-stream-transform-matches-batch
  (let* ((access-unit
           (concatenate-octets
            (octets #x12 0)
            (octets #x0a 5 0 0 1 2 3)
            (octets #x32 4 0 0 2 4)))
         (expected
           (convert-av1-access-unit-to-ts-format access-unit)))
    (loop for chunk-size from 1 to (length access-unit)
          do
      (let ((transformer (make-av1-stream-transformer))
            (chunks '()))
        (loop for start from 0 below (length access-unit)
                by chunk-size
              for end =
                (min (length access-unit)
                     (+ start chunk-size))
              do (push
                  (transform-av1-stream-chunk
                   transformer access-unit
                   :start start :end end)
                  chunks))
        (finish-av1-stream-transformer transformer)
        (check-bridge-test
         (equalp
          (apply #'concatenate-octets (nreverse chunks))
          expected))))))

(define-bridge-test av1-temporal-delimiter-and-size-are-normalized
  (let* ((access-unit
           (concatenate-octets
            (octets #x12 0)
            (octets #x0e 0 #x01 #x7f)))
         (converted
           (convert-av1-access-unit-to-ts-format access-unit)))
    (check-bridge-test
     (equalp converted
             (octets 0 0 1 #x0c 0 #x7f)))
    (check-bridge-test
     (not
      (find #x12 converted :test #'=)))))

(define-bridge-test av1-padding-and-redundant-obus-are-carried
  (let* ((access-unit
           (concatenate-octets
            (octets #x3a 3 0 0 1)
            (octets #x7a 7 0 0 2 0 0 3 4)))
         (expected
           (octets
            0 0 1 #x38 0 0 3 1
            0 0 1 #x78 0 0 3 2 0 0 3 3 4))
         (converted
           (convert-av1-access-unit-to-ts-format access-unit))
         (transformer (make-av1-stream-transformer))
         (streamed
           (transform-av1-stream-chunk transformer access-unit)))
    (finish-av1-stream-transformer transformer)
    (check-bridge-test (equalp converted expected))
    (check-bridge-test (equalp streamed expected))))

(define-bridge-test av1-non-reduced-operating-point-values
  (let ((bits
          (make-array 32
                      :element-type 'bit
                      :adjustable t
                      :fill-pointer 0)))
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 1)
    (append-integer-bits bits 0 5)
    (append-integer-bits bits 0 12)
    (append-integer-bits bits 8 5)
    (append-integer-bits bits 1 1)
    (multiple-value-bind (level tier delay)
        (parse-av1-operating-points
         (make-bit-reader (bit-vector-to-octets bits))
         nil)
      (check-bridge-test (= level 8))
      (check-bridge-test (= tier 1))
      (check-bridge-test (null delay)))))

(define-bridge-test av1-non-reduced-sequence-header
  (let ((access-unit
          (make-test-av1-non-reduced-access-unit)))
    (multiple-value-bind (configuration width height)
        (parse-av1-sequence-header access-unit)
      (check-bridge-test
       (= (av1-codec-configuration-profile configuration) 0))
      (check-bridge-test
       (= (av1-codec-configuration-level configuration) 8))
      (check-bridge-test
       (= (av1-codec-configuration-tier configuration) 1))
      (check-bridge-test (= width 64))
      (check-bridge-test (= height 64))
      (check-bridge-test
       (eq (av1-access-unit-random-access-kind
            access-unit nil)
           :key)))))

(define-bridge-test av1-input-subset-rejects-unsupported-profile-and-color
  (let ((profile-one
          (make-av1-structure-test-sequence)))
    ;; headerと1-byte sizeに続くsequence_profileの先頭bitを1にする。
    (setf (aref profile-one 2)
          (logior (aref profile-one 2) #x20))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (parse-av1-sequence-header profile-one))))
    (let ((obu (first (parse-av1-obus profile-one))))
      (check-bridge-test
       (signals-bridge-error-p
        (lambda ()
          (parse-av1-sequence-header-validation-state
           profile-one obu))))))
  (dolist
      (configuration
       (list
        (make-av1-codec-configuration :profile 1)
        (make-av1-codec-configuration
         :high-bitdepth 1 :twelve-bit 1)
        (make-av1-codec-configuration :monochrome 1)
        (make-av1-codec-configuration
         :chroma-subsampling-x 0)
        (make-av1-codec-configuration
         :chroma-subsampling-y 0)))
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (make-av1-video-descriptor configuration))))))

(define-bridge-test av1-descriptor-version-one
  (let* ((configuration
           (make-av1-codec-configuration
            :profile 0
            :level 8
            :high-bitdepth 0
            :hdr-wcg-idc 0))
         (descriptors
           (make-av1-mapping-descriptors configuration)))
    (validate-av1-mapping-descriptors descriptors)
    (check-bridge-test
     (equalp (descriptor-payload (first descriptors))
             (octets #x41 #x56 #x30 #x31)))
    (check-bridge-test
     (= (aref (descriptor-payload (second descriptors)) 0)
        #x81))))

(define-bridge-test opus-descriptor-validation
  (let ((descriptors
          (list
           (make-descriptor
            :tag #x05
            :payload (octets #x4f #x70 #x75 #x73))
           (make-descriptor
            :tag #x7f
            :payload (octets #x80 2)))))
    (check-bridge-test (= (validate-opus-descriptors descriptors) 2))
    (setf (aref (descriptor-payload (second descriptors)) 1) #xff)
    (check-bridge-test
     (signals-bridge-error-p
      (lambda ()
        (validate-opus-descriptors descriptors))))))
