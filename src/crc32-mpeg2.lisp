;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defconstant +crc32-mpeg2-polynomial+ #x04c11db7)

(defun crc32-mpeg2 (octets &key (start 0) end)
  "OCTETSのSTARTからENDまでのCRC-32/MPEG-2を計算する。"
  (let ((resolved-end (or end (length octets)))
        (crc #xffffffff))
    (ensure-octet-range octets start (- resolved-end start)
                        :crc32-mpeg2)
    (loop for position from start below resolved-end
          do (setf crc (logxor crc
                               (ash (aref octets position) 24)))
             (loop repeat 8
                   do
                      (setf crc
                            (logand
                             (if (logbitp 31 crc)
                                 (logxor (ash crc 1)
                                         +crc32-mpeg2-polynomial+)
                                 (ash crc 1))
                             #xffffffff))))
    crc))

(defun valid-crc32-mpeg2-p (section)
  "CRCを含むSECTION全体のCRC-32/MPEG-2が0かを返す。"
  (zerop (crc32-mpeg2 section)))
