;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defstruct (tstd-departure-range
            (:constructor make-tstd-departure-range
                (start end first-departure interval)))
  (start 0 :type (integer 0 *))
  (end 0 :type (integer 0 *))
  (first-departure 0 :type rational)
  (interval 0 :type (rational 0 *)))

(defstruct (tstd-pes-byte-range
            (:constructor make-tstd-pes-byte-range
                (start end kind)))
  (start 0 :type (integer 0 *))
  (end 0 :type (integer 0 *))
  (kind :header :type (member :header :payload)))

(defstruct tstd-access-unit
  (pts 0 :type (unsigned-byte 33))
  (dts 0 :type (unsigned-byte 33))
  (low-delay-mode-p nil :type boolean)
  (show-existing-frame-p nil :type boolean)
  (frame-to-show-map-index nil
                           :type (or null (unsigned-byte 3)))
  (frame-type nil :type (or null (unsigned-byte 2)))
  (show-frame-p nil :type (or null boolean))
  (refresh-frame-flags nil :type (or null octet))
  (rx-bytes-per-second 1 :type (rational 0 *))
  (multiplex-buffer-size 0 :type (rational 0 *))
  (elementary-buffer-size 0 :type (rational 0 *))
  (transport-first-arrival nil :type (or null rational))
  (mb-first-departure nil :type (or null rational))
  (mb-last-departure nil :type (or null rational))
  (es
    (make-array 1024
                :element-type 'octet
                :adjustable t
                :fill-pointer 0)
    :type (vector octet))
  (mb-departure-ranges
    (make-array 64
                :adjustable t
                :fill-pointer 0)
    :type vector))

(defstruct tstd-pes-classifier
  (active-p nil :type boolean)
  (position 0 :type (integer 0 *))
  (header-length nil :type (or null (integer 9 264)))
  (prefix
    (make-array 9
                :element-type 'octet
                :initial-element 0)
    :type octet-vector))

(defun discard-tstd-pes-classifier (classifier)
  "旧epochの未完PES分類状態を検証せず破棄する。"
  (setf
   (tstd-pes-classifier-active-p classifier) nil
   (tstd-pes-classifier-position classifier) 0
   (tstd-pes-classifier-header-length classifier) nil)
  (fill (tstd-pes-classifier-prefix classifier) 0)
  classifier)

(defun start-tstd-pes-classifier (classifier)
  "新しいAV1 PESのheader分類状態を開始する。"
  (discard-tstd-pes-classifier classifier)
  (setf
   (tstd-pes-classifier-active-p classifier) t)
  classifier)

(defun finish-tstd-pes-classifier (classifier)
  "現在PESが最低限のheaderとES payloadを持つことを検証する。"
  (when (tstd-pes-classifier-active-p classifier)
    (let ((header-length
            (tstd-pes-classifier-header-length classifier)))
      (unless (and header-length
                   (> (tstd-pes-classifier-position classifier)
                      header-length))
        (bridge-error "TSTD_AV1_PES_PAYLOAD_INCOMPLETE"))))
  (setf (tstd-pes-classifier-active-p classifier) nil)
  classifier)

(defun validate-tstd-pes-prefix (classifier)
  "収集済み9-byte PES prefixがAV1出力契約どおりか検証する。"
  (let ((prefix (tstd-pes-classifier-prefix classifier)))
    (unless (and (= (aref prefix 0) 0)
                 (= (aref prefix 1) 0)
                 (= (aref prefix 2) 1)
                 (= (aref prefix 3) #xbd))
      (bridge-error "TSTD_AV1_PES_PREFIX_INVALID"))
    (setf
     (tstd-pes-classifier-header-length classifier)
     (+ 9 (aref prefix 8))))
  classifier)

(defun classify-tstd-video-packet-byte-ranges (classifier packet)
  "PACKET内PES byteをheader/payload別の連続範囲として返す。"
  (let ((payload-offset (ts-payload-offset packet))
        (ranges '())
        (range-start nil)
        (range-kind nil))
    (unless payload-offset
      (return-from classify-tstd-video-packet-byte-ranges '()))
    (unless (tstd-pes-classifier-active-p classifier)
      (bridge-error "TSTD_AV1_PES_CONTINUATION_WITHOUT_START"))
    (loop for packet-offset from payload-offset below +ts-packet-size+
          for pes-position =
            (tstd-pes-classifier-position classifier)
          for header-length =
            (tstd-pes-classifier-header-length classifier)
          do
      (when (< pes-position 9)
        (setf
         (aref (tstd-pes-classifier-prefix classifier)
               pes-position)
         (aref packet packet-offset))
        (when (= pes-position 8)
          (validate-tstd-pes-prefix classifier)
          (setf header-length
                (tstd-pes-classifier-header-length classifier))))
      (let ((kind
              (if (and header-length
                       (>= pes-position header-length))
                  :payload
                  :header)))
        (cond
          ((null range-start)
           (setf range-start packet-offset
                 range-kind kind))
          ((not (eq range-kind kind))
           (push
            (make-tstd-pes-byte-range
             range-start packet-offset range-kind)
            ranges)
           (setf range-start packet-offset
                 range-kind kind))))
      (incf (tstd-pes-classifier-position classifier)))
    (when range-start
      (push
       (make-tstd-pes-byte-range
        range-start +ts-packet-size+ range-kind)
       ranges))
    (nreverse ranges)))

(defun classify-tstd-video-packet-es-ranges (classifier packet)
  "互換APIとしてPACKET内PES payloadの連続範囲だけを返す。"
  (loop for range in
          (classify-tstd-video-packet-byte-ranges
           classifier packet)
        when (eq (tstd-pes-byte-range-kind range) :payload)
          collect
          (cons
           (tstd-pes-byte-range-start range)
           (tstd-pes-byte-range-end range))))

(defun map-simple-av1-decoder-byte-ranges
    (function ts-access-unit)
  "単純octet vectorのAV1 decoder byte範囲をFUNCTIONへ渡す。"
  (let ((position 0)
        (length (length ts-access-unit)))
    (declare
     (type function function)
     (type (simple-array octet (*)) ts-access-unit)
     (type fixnum position length)
     (optimize (speed 3) (safety 1)))
    (when (zerop length)
      (bridge-error "TSTD_AV1_ACCESS_UNIT_EMPTY"))
    (loop while (< position length)
          do
      (unless (and (<= (+ position 3) length)
                   (= (aref ts-access-unit position) 0)
                   (= (aref ts-access-unit (+ position 1)) 0)
                   (= (aref ts-access-unit (+ position 2)) 1))
        (bridge-error
         "TSTD_AV1_START_CODE_MISSING offset=~D"
         position))
      (incf position 3)
      (let ((next-start
              (or
               (loop for candidate fixnum from position
                       to (- length 3)
                     when
                       (and
                        (= (aref ts-access-unit candidate) 0)
                        (= (aref ts-access-unit
                                 (+ candidate 1))
                           0)
                        (= (aref ts-access-unit
                                 (+ candidate 2))
                           1))
                       do (return candidate))
               length)))
        (declare (type fixnum next-start))
        (when (= next-start position)
          (bridge-error
           "TSTD_AV1_OBU_EMPTY offset=~D"
           position))
        (let ((range-start position))
          (declare (type fixnum range-start))
          (loop while (< position next-start)
                do
            (cond
              ((and
                (<= (+ position 4) next-start)
                (= (aref ts-access-unit position) 0)
                (= (aref ts-access-unit (+ position 1)) 0)
                (= (aref ts-access-unit (+ position 2)) 3))
               (let ((following
                       (aref ts-access-unit (+ position 3))))
                 (when (> following 3)
                   (bridge-error
                    "TSTD_AV1_EMULATION_PREVENTION_INVALID offset=~D"
                    (+ position 2)))
                 ;; 00 00 03 xxは03だけを除いた2範囲になる。
                 (funcall function
                          range-start (+ position 2))
                 (funcall function
                          (+ position 3) (+ position 4))
                 (incf position 4)
                 (setf range-start position)))
              (t
               (when (and
                      (<= (+ position 3) next-start)
                      (= (aref ts-access-unit position) 0)
                      (= (aref ts-access-unit (+ position 1)) 0)
                      (<= (aref ts-access-unit
                                (+ position 2))
                          3))
                 (bridge-error
                  "TSTD_AV1_EMULATION_PREVENTION_MISSING offset=~D"
                  position))
               (incf position))))
          (when (< range-start next-start)
            (funcall function range-start next-start)))))
    ts-access-unit))

(defun map-av1-decoder-byte-ranges (function ts-access-unit)
  "AV1 decoderへ渡す連続byte範囲をFUNCTIONへ渡す。"
  (let ((simple-access-unit
          (if (typep
               ts-access-unit
               '(simple-array octet (*)))
              ts-access-unit
              (make-array
               (length ts-access-unit)
               :element-type 'octet
               :initial-contents ts-access-unit))))
    (map-simple-av1-decoder-byte-ranges
     function simple-access-unit))
  ts-access-unit)

(defun av1-decoder-byte-ranges (ts-access-unit)
  "TS-ACCESS-UNITからdecoderへ渡す連続byte範囲を返す。"
  (let ((ranges '()))
    (map-av1-decoder-byte-ranges
     (lambda (start end)
       (push (cons start end) ranges))
     ts-access-unit)
    (nreverse ranges)))

(defun map-av1-decoder-byte-indices (function ts-access-unit)
  "AV1 decoderへ渡す各byteの符号化ES上indexをFUNCTIONへ渡す。"
  (dolist (range (av1-decoder-byte-ranges ts-access-unit))
    (loop for position from (car range) below (cdr range)
          do (funcall function position)))
  ts-access-unit)

(defun count-av1-decoder-bytes (ts-access-unit)
  "start codeとemulation preventionを除いたAV1 byte数を返す。"
  (loop for (start . end) in
          (av1-decoder-byte-ranges ts-access-unit)
        sum (- end start)))
