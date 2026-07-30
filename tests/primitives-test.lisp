;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(define-bridge-test big-endian-roundtrip
  (let ((buffer (make-array 9 :element-type 'octet :initial-element 0)))
    (write-u16-be #xabcd buffer 0)
    (write-u24-be #x123456 buffer 2)
    (write-u32-be #x89abcdef buffer 5)
    (check-bridge-test (= (read-u16-be buffer 0) #xabcd))
    (check-bridge-test (= (read-u24-be buffer 2) #x123456))
    (check-bridge-test (= (read-u32-be buffer 5) #x89abcdef))))

(define-bridge-test uleb128-roundtrip
  (dolist (value '(0 1 127 128 255 16384 4294967295))
    (let ((encoded (encode-uleb128 value)))
      (multiple-value-bind (decoded next)
          (decode-uleb128 encoded 0)
        (check-bridge-test (= decoded value))
        (check-bridge-test (= next (length encoded))))))
  (check-bridge-test
   (signals-bridge-error-p
    (lambda ()
      (decode-uleb128 (octets #x80 #x80) 0 :maximum-bytes 2)))))

(define-bridge-test msb-first-bit-reader
  (let ((reader (make-bit-reader (octets #x82 #x7f))))
    (check-bridge-test (= (read-bits reader 2) 2))
    (check-bridge-test (= (read-bits reader 4) 0))
    (check-bridge-test (= (read-bits reader 2) 2))
    (check-bridge-test (= (read-bits reader 8) #x7f))
    (check-bridge-test (= (bit-reader-remaining reader) 0))))

(define-bridge-test crc32-mpeg2-check-vector
  (let ((input
          (map '(simple-array (unsigned-byte 8) (*))
               #'char-code
               "123456789")))
    (check-bridge-test (= (crc32-mpeg2 input) #x0376e6e7))))

(define-bridge-test pes-timestamp-roundtrip
  (dolist (value '(0 1 90000 8589934591))
    (let ((encoded (encode-pes-timestamp value 2)))
      (check-bridge-test (= (decode-pes-timestamp encoded 0 2)
                            value)))))
