;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defconstant +buffer-packet-count+ 64
  "一度に読み込むMPEG-TS packet数。")

(defconstant +stream-buffer-size+
  (* +buffer-packet-count+ +ts-packet-size+)
  "fast pathで再利用するoctet bufferのbyte数。")

(defun %read-octets
    (input buffer &optional (start 0) end)
  "INPUTからBUFFERのSTART以降へENDまで読み、読み取ったbyte数を返す。"
  (handler-case
      (- (read-sequence buffer input :start start :end end)
         start)
    (stream-error (cause)
      (error 'bridge-io-error
             :message (format nil "Input stream failed: ~A" cause)
             :operation :read
             :cause cause))))

(defun %write-octets (output buffer end)
  "BUFFERの先頭からENDまでをOUTPUTへ書く。"
  (handler-case
      (write-sequence buffer output :end end)
    (stream-error (cause)
      (error 'bridge-io-error
             :message (format nil "Output stream failed: ~A" cause)
             :operation :write
             :cause cause))))

(defun copy-binary-stream (input output)
  "INPUTのoctet列を一個の固定bufferでOUTPUTへbyte-exactに転送する。"
  (let ((buffer (make-array +stream-buffer-size+
                            :element-type 'octet)))
    (loop
      for count = (%read-octets input buffer)
      while (plusp count)
      do (%write-octets output buffer count))
    (handler-case
        (finish-output output)
      (stream-error (cause)
        (error 'bridge-io-error
               :message (format nil "Output flush failed: ~A" cause)
               :operation :flush
               :cause cause))))
  nil)

(defun validate-and-copy-ts-stream (input output)
  "INPUTをTSとして全PID検証し、固定bufferでbyte-exactにOUTPUTへ転送する。"
  (let ((buffer
          (make-array (+ +stream-buffer-size+
                         (- +ts-packet-size+ 1))
                      :element-type 'octet))
        (packet
          (make-array +ts-packet-size+
                      :element-type 'octet))
        (continuity-validator
          (make-payload-continuity-validator))
        (carry 0))
    (loop
      for count =
        (%read-octets
         input buffer carry (+ carry +stream-buffer-size+))
      for total = (+ carry count)
      for complete-end =
        (- total (mod total +ts-packet-size+))
      do (loop for offset from 0 below complete-end
                 by +ts-packet-size+
               do (replace packet buffer
                           :start2 offset
                           :end2 (+ offset +ts-packet-size+))
                  (validate-ts-packet packet)
                  (validate-ts-packet-integrity
                   continuity-validator packet))
         (when (plusp complete-end)
           (%write-octets output buffer complete-end))
         (setf carry (- total complete-end))
         (when (plusp carry)
           (replace buffer buffer
                    :start1 0
                    :start2 complete-end
                    :end2 total))
      when (zerop count)
        do (return))
    (when (plusp carry)
      (bridge-error
       "EOF truncates a transport packet: ~D trailing bytes"
       carry))
    (handler-case
        (finish-output output)
      (stream-error (cause)
        (error 'bridge-io-error
               :message (format nil "Output flush failed: ~A" cause)
               :operation :flush
               :cause cause))))
  nil)

(defun process-ts-stream
    (input output video-codec audio-codec
     &key program-number transport-rate-kbps
       stream-anchor-v1-p
       (stream-anchor-maximum-distance-ticks
         +stream-anchor-default-maximum-distance-ticks+))
  "INPUTを188-byte単位で厳格処理してOUTPUTへ書く。"
  (let* ((buffer
           (make-array (+ +stream-buffer-size+
                          (- +ts-packet-size+ 1))
                       :element-type 'octet))
         (packet
           (make-array +ts-packet-size+
                       :element-type 'octet))
         (anchor-finalizer
           (when stream-anchor-v1-p
             (make-stream-anchor-finalizer
              output
              :program-number program-number
              :maximum-distance-ticks
              stream-anchor-maximum-distance-ticks)))
         (processor-output
           (if anchor-finalizer
               (make-instance
                'stream-anchor-output-stream
                :finalizer anchor-finalizer)
               output))
         (processor
           (make-bridge-processor
            processor-output video-codec audio-codec
            :program-number program-number
            :transport-rate-kbps transport-rate-kbps))
         (carry 0))
    (loop
      for count =
        (%read-octets
         input buffer carry (+ carry +stream-buffer-size+))
      for total = (+ carry count)
      for complete-end =
        (- total (mod total +ts-packet-size+))
      do (loop for offset from 0 below complete-end
                 by +ts-packet-size+
               do (replace packet buffer
                           :start2 offset
                           :end2 (+ offset +ts-packet-size+))
                  (process-bridge-packet
                   processor (copy-seq packet)))
         (setf carry (- total complete-end))
         (when (plusp carry)
           (replace buffer buffer
                    :start1 0
                    :start2 complete-end
                    :end2 total))
      when (zerop count)
        do (return))
    (when (plusp carry)
      (bridge-error
       "EOF truncates a transport packet: ~D trailing bytes"
       carry))
    (finish-bridge-processor processor)
    (when anchor-finalizer
      (finish-stream-anchor-output-stream processor-output)))
  nil)
