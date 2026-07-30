;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector ()
  '(simple-array octet (*)))

(defconstant +ts-packet-size+ 188)

(defun concatenate-octet-vectors (&rest vectors)
  "VECTORSを単純octet vectorへ連結する。"
  (apply #'concatenate
         '(simple-array (unsigned-byte 8) (*))
         vectors))

(defun ensure-octet-range (octets offset length operation)
  "OCTETSのOFFSETからLENGTH byteが読めることを検証する。"
  (unless (and (typep octets '(array (unsigned-byte 8) (*)))
               (typep offset '(integer 0 *))
               (typep length '(integer 0 *))
               (<= (+ offset length) (length octets)))
    (bridge-error "Invalid octet range for ~A: offset=~D length=~D size=~D"
                  operation offset length (length octets))))

(declaim (inline read-u16-be read-u24-be read-u32-be))

(defun read-u16-be (octets offset)
  "OCTETSのOFFSETからbig-endian 16 bit整数を読む。"
  (ensure-octet-range octets offset 2 :read-u16-be)
  (logior (ash (aref octets offset) 8)
          (aref octets (+ offset 1))))

(defun read-u24-be (octets offset)
  "OCTETSのOFFSETからbig-endian 24 bit整数を読む。"
  (ensure-octet-range octets offset 3 :read-u24-be)
  (logior (ash (aref octets offset) 16)
          (ash (aref octets (+ offset 1)) 8)
          (aref octets (+ offset 2))))

(defun read-u32-be (octets offset)
  "OCTETSのOFFSETからbig-endian 32 bit整数を読む。"
  (ensure-octet-range octets offset 4 :read-u32-be)
  (logior (ash (aref octets offset) 24)
          (ash (aref octets (+ offset 1)) 16)
          (ash (aref octets (+ offset 2)) 8)
          (aref octets (+ offset 3))))

(defun write-u16-be (value octets offset)
  "VALUEをOCTETSのOFFSETへbig-endian 16 bitで書く。"
  (unless (typep value '(unsigned-byte 16))
    (bridge-error "Value does not fit in 16 bits: ~D" value))
  (ensure-octet-range octets offset 2 :write-u16-be)
  (setf (aref octets offset) (ldb (byte 8 8) value)
        (aref octets (+ offset 1)) (ldb (byte 8 0) value))
  octets)

(defun write-u24-be (value octets offset)
  "VALUEをOCTETSのOFFSETへbig-endian 24 bitで書く。"
  (unless (typep value '(unsigned-byte 24))
    (bridge-error "Value does not fit in 24 bits: ~D" value))
  (ensure-octet-range octets offset 3 :write-u24-be)
  (setf (aref octets offset) (ldb (byte 8 16) value)
        (aref octets (+ offset 1)) (ldb (byte 8 8) value)
        (aref octets (+ offset 2)) (ldb (byte 8 0) value))
  octets)

(defun write-u32-be (value octets offset)
  "VALUEをOCTETSのOFFSETへbig-endian 32 bitで書く。"
  (unless (typep value '(unsigned-byte 32))
    (bridge-error "Value does not fit in 32 bits: ~D" value))
  (ensure-octet-range octets offset 4 :write-u32-be)
  (setf (aref octets offset) (ldb (byte 8 24) value)
        (aref octets (+ offset 1)) (ldb (byte 8 16) value)
        (aref octets (+ offset 2)) (ldb (byte 8 8) value)
        (aref octets (+ offset 3)) (ldb (byte 8 0) value))
  octets)

(defun decode-uleb128 (octets offset &key (maximum-bytes 8))
  "OCTETSのOFFSETからULEB128を読み、値と次のoffsetを返す。"
  (unless (plusp maximum-bytes)
    (bridge-error "ULEB128 maximum byte count must be positive: ~D"
                  maximum-bytes))
  (let ((value 0)
        (shift 0)
        (position offset))
    (loop repeat maximum-bytes
          do (ensure-octet-range octets position 1 :decode-uleb128)
             (let ((current (aref octets position)))
               (setf value (logior value
                                   (ash (logand current #x7f) shift)))
               (incf position)
               (when (zerop (logand current #x80))
                 (return-from decode-uleb128 (values value position)))
               (incf shift 7)))
    (bridge-error "ULEB128 exceeds ~D bytes at offset ~D"
                  maximum-bytes offset)))

(defun encode-uleb128 (value)
  "非負整数VALUEを最短ULEB128 byte列へ変換する。"
  (unless (typep value '(integer 0 *))
    (bridge-error "ULEB128 value must be non-negative: ~D" value))
  (let ((result (make-array 1
                            :element-type 'octet
                            :adjustable t
                            :fill-pointer 0))
        (remaining value))
    (loop
      (let ((current (logand remaining #x7f)))
        (setf remaining (ash remaining -7))
        (vector-push-extend
         (if (zerop remaining)
             current
             (logior current #x80))
         result)
        (when (zerop remaining)
          (return))))
    (coerce result '(simple-array (unsigned-byte 8) (*)))))
