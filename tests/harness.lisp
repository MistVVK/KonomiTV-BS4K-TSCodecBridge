;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defvar *bridge-tests* '())

(defclass octet-collector-stream
    (sb-gray:fundamental-binary-output-stream)
  ((data
    :initform
    (make-array 1024
                :element-type 'octet
                :adjustable t
                :fill-pointer 0)
    :reader octet-collector-data)))

(defclass octet-chunk-input-stream
    (sb-gray:fundamental-binary-input-stream)
  ((data
    :initarg :data
    :reader octet-chunk-input-data)
   (position
    :initform 0
    :accessor octet-chunk-input-position)
   (chunk-size
    :initarg :chunk-size
    :reader octet-chunk-input-chunk-size)))

(defmethod stream-element-type
    ((stream octet-collector-stream))
  (declare (ignore stream))
  'octet)

(defmethod sb-gray:stream-write-byte
    ((stream octet-collector-stream) byte)
  (vector-push-extend byte (octet-collector-data stream))
  byte)

(defmethod sb-gray:stream-write-sequence
    ((stream octet-collector-stream) sequence
     &optional (start 0) end)
  (let ((actual-end (or end (length sequence))))
    (loop for position from start below actual-end
          do (vector-push-extend
              (aref sequence position)
              (octet-collector-data stream)))
    sequence))

(defmethod sb-gray:stream-read-byte
    ((stream octet-chunk-input-stream))
  (let ((position (octet-chunk-input-position stream))
        (data (octet-chunk-input-data stream)))
    (if (= position (length data))
        :eof
        (prog1 (aref data position)
          (incf (octet-chunk-input-position stream))))))

(defmethod sb-gray:stream-read-sequence
    ((stream octet-chunk-input-stream) sequence
     &optional (start 0) end)
  (let* ((data (octet-chunk-input-data stream))
         (position (octet-chunk-input-position stream))
         (actual-end (or end (length sequence)))
         (count
           (min (- actual-end start)
                (octet-chunk-input-chunk-size stream)
                (- (length data) position))))
    (replace sequence data
             :start1 start
             :start2 position
             :end2 (+ position count))
    (incf (octet-chunk-input-position stream) count)
    (+ start count)))

(defun collected-octets (stream)
  "STREAMへ書かれたbyteを単純octet vectorで返す。"
  (coerce (octet-collector-data stream)
          '(simple-array (unsigned-byte 8) (*))))

(defun register-bridge-test (name function)
  "NAMEのtestをFUNCTIONへ登録する。再load時は同名testを置換する。"
  (setf *bridge-tests*
        (remove name *bridge-tests* :key #'car :test #'eq))
  (push (cons name function) *bridge-tests*)
  name)

(defmacro define-bridge-test (name &body body)
  "NAMEのtestを定義する。"
  `(register-bridge-test
    ',name
    (lambda ()
      ,@body)))

(defmacro check-bridge-test (form &optional description)
  "FORMが真でなければBRIDGE-ERRORを通知する。"
  `(unless ,form
     (bridge-error "Test assertion failed~@[ (~A)~]: ~S"
                   ,description
                   ',form)))

(defun signals-bridge-error-p (function)
  "FUNCTIONがBRIDGE-ERRORを通知したかを返す。"
  (handler-case
      (progn
        (funcall function)
        nil)
    (bridge-error () t)))

(defun run-bridge-tests ()
  "登録済みtestをすべて実行し、失敗があればBRIDGE-ERRORを通知する。"
  (let ((passed 0)
        (failed 0))
    (dolist (test (reverse *bridge-tests*))
      (handler-case
          (progn
            (funcall (cdr test))
            (incf passed)
            (format *error-output* "PASS ~A~%" (car test)))
        (serious-condition (condition)
          (incf failed)
          (format *error-output* "FAIL ~A: ~A~%"
                  (car test) condition))))
    (format *error-output* "Tests: ~D passed, ~D failed~%"
            passed failed)
    (when (plusp failed)
      (bridge-error "~D Bridge tests failed" failed))
    t))

(defun octets (&rest values)
  "VALUESから単純octet vectorを作る。"
  (make-array (length values)
              :element-type 'octet
              :initial-contents values))

(defun append-integer-bits (bits value count)
  "BITSの末尾へVALUEの下位COUNT bitをMSB-firstで追加する。"
  (loop for position downfrom (- count 1) to 0
        do (vector-push-extend
            (ldb (byte 1 position) value)
            bits))
  bits)

(defun bit-vector-to-octets (bits)
  "0/1 vector BITSをMSB-firstのoctet vectorへ変換する。"
  (let ((result
          (make-array (ceiling (length bits) 8)
                      :element-type 'octet
                      :initial-element 0)))
    (loop for bit across bits
          for position from 0
          do (when (= bit 1)
               (setf (aref result (floor position 8))
                     (logior
                      (aref result (floor position 8))
                      (ash 1 (- 7 (mod position 8)))))))
    result))

(defun concatenate-octets (&rest vectors)
  "VECTORSを単純octet vectorへ連結する。"
  (apply #'concatenate
         '(simple-array (unsigned-byte 8) (*))
         vectors))
