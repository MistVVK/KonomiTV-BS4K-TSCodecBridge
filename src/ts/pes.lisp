;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defstruct pes-header
  (stream-id 0 :type octet)
  (packet-length 0 :type (unsigned-byte 16))
  (data-alignment-p nil :type boolean)
  (pts nil :type (or null (unsigned-byte 33)))
  (dts nil :type (or null (unsigned-byte 33)))
  (payload-offset 0 :type fixnum))

(defun validate-pes-timestamp-field (octets offset expected-prefix)
  "PES timestamp fieldのprefixとmarker bitを検証する。"
  (ensure-octet-range octets offset 5 :pes-timestamp)
  (unless (= (ldb (byte 4 4) (aref octets offset))
             expected-prefix)
    (bridge-error "PES timestamp prefix is invalid at offset ~D" offset))
  (unless (and (logbitp 0 (aref octets offset))
               (logbitp 0 (aref octets (+ offset 2)))
               (logbitp 0 (aref octets (+ offset 4))))
    (bridge-error "PES timestamp marker bit is missing at offset ~D"
                  offset)))

(defun decode-pes-timestamp (octets offset expected-prefix)
  "OCTETSのPES timestamp fieldから33 bit値を読む。"
  (validate-pes-timestamp-field octets offset expected-prefix)
  (logior (ash (ldb (byte 3 1) (aref octets offset)) 30)
          (ash (aref octets (+ offset 1)) 22)
          (ash (ldb (byte 7 1) (aref octets (+ offset 2))) 15)
          (ash (aref octets (+ offset 3)) 7)
          (ldb (byte 7 1) (aref octets (+ offset 4)))))

(defun encode-pes-timestamp (value prefix)
  "33 bit VALUEをPREFIX付きPES timestamp fieldへ変換する。"
  (unless (typep value '(unsigned-byte 33))
    (bridge-error "PES timestamp does not fit in 33 bits: ~D" value))
  (unless (typep prefix '(unsigned-byte 4))
    (bridge-error "PES timestamp prefix does not fit in 4 bits: ~D"
                  prefix))
  (let ((result (make-array 5 :element-type 'octet)))
    (setf (aref result 0)
          (logior (ash prefix 4)
                  (ash (ldb (byte 3 30) value) 1)
                  1)
          (aref result 1) (ldb (byte 8 22) value)
          (aref result 2)
          (logior (ash (ldb (byte 7 15) value) 1) 1)
          (aref result 3) (ldb (byte 8 7) value)
          (aref result 4)
          (logior (ash (ldb (byte 7 0) value) 1) 1))
    result))

(defun parse-pes-header (pes)
  "PES byte列を検証し、PES-HEADERを返す。"
  (ensure-octet-range pes 0 9 :parse-pes-header)
  (unless (= (read-u24-be pes 0) 1)
    (bridge-error "PES start code prefix is invalid"))
  (unless (= (ldb (byte 2 6) (aref pes 6)) 2)
    (bridge-error "PES optional header marker is invalid"))
  (let* ((packet-length (read-u16-be pes 4))
         (header-data-length (aref pes 8))
         (payload-offset (+ 9 header-data-length))
         (pts-dts-flags (ldb (byte 2 6) (aref pes 7)))
         (pts nil)
         (dts nil))
    (ensure-octet-range pes 9 header-data-length
                        :parse-pes-header-data)
    (when (and (plusp packet-length)
               (/= (+ packet-length 6) (length pes)))
      (bridge-error "PES packet length mismatch: declared=~D actual=~D"
                    packet-length (- (length pes) 6)))
    (case pts-dts-flags
      (0 nil)
      (2
       (when (< header-data-length 5)
         (bridge-error "PES PTS field is truncated"))
       (setf pts (decode-pes-timestamp pes 9 2)))
      (3
       (when (< header-data-length 10)
         (bridge-error "PES PTS/DTS fields are truncated"))
       (setf pts (decode-pes-timestamp pes 9 3)
             dts (decode-pes-timestamp pes 14 1)))
      (otherwise
       (bridge-error "PES PTS_DTS_flags value is forbidden: ~D"
                     pts-dts-flags)))
    (make-pes-header
     :stream-id (aref pes 3)
     :packet-length packet-length
     :data-alignment-p (logbitp 2 (aref pes 6))
     :pts pts
     :dts dts
     :payload-offset payload-offset)))

(defun make-pes (stream-id payload pts
                 &key dts data-alignment)
  "STREAM-ID、PAYLOAD、PTSからPES packetを作る。"
  (unless (typep stream-id 'octet)
    (bridge-error "PES stream id does not fit in 8 bits: ~D" stream-id))
  (unless (typep pts '(unsigned-byte 33))
    (bridge-error "PES PTS is required and must fit in 33 bits: ~S" pts))
  (let* ((timestamp-length (if dts 10 5))
         (header-length (+ 9 timestamp-length))
         (result (make-array (+ header-length (length payload))
                             :element-type 'octet))
         (declared-length (+ 3 timestamp-length (length payload)))
         (packet-length (if (> declared-length #xffff)
                            0
                            declared-length)))
    (write-u24-be 1 result 0)
    (setf (aref result 3) stream-id)
    (write-u16-be packet-length result 4)
    (setf (aref result 6) (logior #x80
                                  (if data-alignment #x04 0))
          (aref result 7) (if dts #xc0 #x80)
          (aref result 8) timestamp-length)
    (replace result
             (encode-pes-timestamp pts (if dts 3 2))
             :start1 9)
    (when dts
      (replace result
               (encode-pes-timestamp dts 1)
               :start1 14))
    (replace result payload :start1 header-length)
    result))

(defun rewrite-pes-identification (pes stream-id)
  "PESのstream_idを更新しdata_alignment_indicatorを1にする。"
  (parse-pes-header pes)
  (let ((result (copy-seq pes)))
    (setf (aref result 3) stream-id
          (aref result 6) (logior (aref result 6) #x04))
    result))
