;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defstruct (bit-reader
            (:constructor %make-bit-reader
                (octets start-bit end-bit position)))
  (octets #() :type (array octet (*)) :read-only t)
  (start-bit 0 :type fixnum :read-only t)
  (end-bit 0 :type fixnum :read-only t)
  (position 0 :type fixnum))

(defun make-bit-reader (octets &key (start 0) end)
  "OCTETSのSTARTからENDまでをMSB-firstで読むreaderを作る。"
  (let ((resolved-end (or end (length octets))))
    (ensure-octet-range octets start (- resolved-end start)
                        :make-bit-reader)
    (%make-bit-reader octets
                      (* start 8)
                      (* resolved-end 8)
                      (* start 8))))

(defun bit-reader-remaining (reader)
  "READERで未読のbit数を返す。"
  (- (bit-reader-end-bit reader)
     (bit-reader-position reader)))

(defun read-one-bit (reader)
  "READERから1 bit読み取る。byte内はMSB-firstとする。"
  (when (zerop (bit-reader-remaining reader))
    (bridge-error "Truncated bitstream at bit ~D"
                  (bit-reader-position reader)))
  (let* ((position (bit-reader-position reader))
         (octet-position (ash position -3))
         (bit-position (- 7 (logand position 7)))
         (value (ldb (byte 1 bit-position)
                     (aref (bit-reader-octets reader)
                           octet-position))))
    (incf (bit-reader-position reader))
    value))

(defun read-bits (reader count)
  "READERからCOUNT bitをMSB-firstで読み、整数として返す。"
  (unless (typep count '(integer 0 *))
    (bridge-error "Bit count must be non-negative: ~D" count))
  (when (> count (bit-reader-remaining reader))
    (bridge-error "Truncated bitstream: requested=~D remaining=~D"
                  count (bit-reader-remaining reader)))
  (let ((value 0))
    (loop repeat count
          do (setf value (logior (ash value 1)
                                 (read-one-bit reader))))
    value))

(defun skip-bits (reader count)
  "READERの現在位置をCOUNT bit進める。"
  (unless (typep count '(integer 0 *))
    (bridge-error "Skipped bit count must be non-negative: ~D" count))
  (when (> count (bit-reader-remaining reader))
    (bridge-error "Truncated bitstream while skipping: requested=~D remaining=~D"
                  count (bit-reader-remaining reader)))
  (incf (bit-reader-position reader) count)
  reader)

(defun byte-align-bit-reader (reader)
  "READERを次のbyte境界へ進める。"
  (let ((remainder (logand (bit-reader-position reader) 7)))
    (unless (zerop remainder)
      (skip-bits reader (- 8 remainder))))
  reader)
