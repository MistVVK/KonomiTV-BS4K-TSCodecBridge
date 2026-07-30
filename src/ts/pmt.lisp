;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defstruct descriptor
  (tag 0 :type octet)
  (payload #() :type octet-vector))

(defstruct pmt-stream
  (stream-type 0 :type octet)
  (elementary-pid 0 :type (unsigned-byte 13))
  (descriptors '() :type list))

(defstruct program-map-table
  (program-number 0 :type (unsigned-byte 16))
  (version 0 :type (unsigned-byte 5))
  (current-next-p t :type boolean)
  (section-number 0 :type octet)
  (last-section-number 0 :type octet)
  (pcr-pid +ts-null-pid+ :type (unsigned-byte 13))
  (program-descriptors '() :type list)
  (streams '() :type list))

(defun parse-descriptor-loop (octets start end)
  "OCTETSのSTARTからENDまでをDESCRIPTOR listへ変換する。"
  (let ((descriptors '())
        (offset start))
    (loop while (< offset end)
          do (when (> (+ offset 2) end)
               (bridge-error "Descriptor header is truncated at offset ~D"
                             offset))
             (let* ((tag (aref octets offset))
                    (length (aref octets (+ offset 1)))
                    (payload-start (+ offset 2))
                    (payload-end (+ payload-start length)))
               (when (> payload-end end)
                 (bridge-error "Descriptor payload is truncated at offset ~D"
                               offset))
               (push (make-descriptor
                      :tag tag
                      :payload (subseq octets payload-start payload-end))
                     descriptors)
               (setf offset payload-end)))
    (nreverse descriptors)))

(defun descriptor-loop-length (descriptors)
  "DESCRIPTORSを直列化したbyte数を返す。"
  (loop for descriptor in descriptors
        for payload = (descriptor-payload descriptor)
        do (when (> (length payload) 255)
             (bridge-error "Descriptor payload exceeds 255 bytes: ~D"
                           (length payload)))
        sum (+ 2 (length payload))))

(defun write-descriptor-loop (descriptors octets start)
  "DESCRIPTORSをOCTETSのSTARTから書き、次のoffsetを返す。"
  (let ((offset start))
    (dolist (descriptor descriptors offset)
      (let ((payload (descriptor-payload descriptor)))
        (setf (aref octets offset) (descriptor-tag descriptor)
              (aref octets (+ offset 1)) (length payload))
        (replace octets payload :start1 (+ offset 2))
        (incf offset (+ 2 (length payload)))))))

(defun descriptor-identifier-p (descriptor identifier)
  "DESCRIPTORが4 byte IDENTIFIERのregistration descriptorかを返す。"
  (and (= (descriptor-tag descriptor) #x05)
       (= (length (descriptor-payload descriptor)) 4)
       (equalp (descriptor-payload descriptor) identifier)))

(defun parse-pmt-section (section)
  "PMT SECTIONをPROGRAM-MAP-TABLEへ変換する。"
  (validate-long-psi-section section #x02)
  (unless (= (logand (aref section 8) #xe0) #xe0)
    (bridge-error "PMT PCR PID reserved bits are invalid"))
  (unless (= (logand (aref section 10) #xf0) #xf0)
    (bridge-error "PMT program info length reserved bits are invalid"))
  (let* ((streams-end (- (length section) 4))
         (program-info-length
           (logior (ash (logand (aref section 10) #x0f) 8)
                   (aref section 11)))
         (program-info-end (+ 12 program-info-length))
         (streams '()))
    (when (> program-info-end streams-end)
      (bridge-error "PMT program descriptor loop is truncated"))
    (let ((offset program-info-end))
      (loop while (< offset streams-end)
            do (when (> (+ offset 5) streams-end)
                 (bridge-error "PMT elementary stream header is truncated"))
               (let* ((stream-type (aref section offset))
                      (elementary-pid
                        (logior
                         (ash (logand (aref section (+ offset 1)) #x1f) 8)
                         (aref section (+ offset 2))))
                      (info-length
                        (logior
                         (ash (logand (aref section (+ offset 3)) #x0f) 8)
                         (aref section (+ offset 4))))
                      (info-start (+ offset 5))
                      (info-end (+ info-start info-length)))
                 (unless (= (logand (aref section (+ offset 1)) #xe0)
                            #xe0)
                   (bridge-error
                    "PMT elementary PID reserved bits are invalid at offset ~D"
                    offset))
                 (unless (= (logand (aref section (+ offset 3)) #xf0)
                            #xf0)
                   (bridge-error
                    "PMT ES info length reserved bits are invalid at offset ~D"
                    offset))
                 (when (> info-end streams-end)
                   (bridge-error "PMT elementary descriptor loop is truncated"))
                 (push (make-pmt-stream
                        :stream-type stream-type
                        :elementary-pid elementary-pid
                        :descriptors
                        (parse-descriptor-loop section info-start info-end))
                       streams)
                 (setf offset info-end))))
    (make-program-map-table
     :program-number (read-u16-be section 3)
     :version (ldb (byte 5 1) (aref section 5))
     :current-next-p (logbitp 0 (aref section 5))
     :section-number (aref section 6)
     :last-section-number (aref section 7)
     :pcr-pid (logior (ash (logand (aref section 8) #x1f) 8)
                      (aref section 9))
     :program-descriptors
     (parse-descriptor-loop section 12 program-info-end)
     :streams (nreverse streams))))

(defun pmt-stream-serialized-length (stream)
  "STREAMをPMT ES entryとして直列化したbyte数を返す。"
  (+ 5 (descriptor-loop-length (pmt-stream-descriptors stream))))

(defun build-pmt-section (table)
  "PROGRAM-MAP-TABLEからCRC付きPMT SECTIONを作る。"
  (let* ((program-info-length
           (descriptor-loop-length
            (program-map-table-program-descriptors table)))
         (streams-length
           (loop for stream in (program-map-table-streams table)
                 sum (pmt-stream-serialized-length stream)))
         (section-length (+ 13 program-info-length streams-length))
         (section (make-array (+ section-length 3)
                              :element-type 'octet
                              :initial-element 0)))
    (when (> section-length 1021)
      (bridge-error "PMT section exceeds 1021 bytes: ~D"
                    section-length))
    (setf (aref section 0) #x02
          (aref section 1)
          (logior #xb0 (ldb (byte 4 8) section-length))
          (aref section 2) (ldb (byte 8 0) section-length))
    (write-u16-be (program-map-table-program-number table)
                  section 3)
    (setf (aref section 5)
          (logior #xc0
                  (ash (program-map-table-version table) 1)
                  (if (program-map-table-current-next-p table) 1 0))
          (aref section 6) (program-map-table-section-number table)
          (aref section 7) (program-map-table-last-section-number table)
          (aref section 8)
          (logior #xe0
                  (ldb (byte 5 8)
                       (program-map-table-pcr-pid table)))
          (aref section 9)
          (ldb (byte 8 0)
               (program-map-table-pcr-pid table))
          (aref section 10)
          (logior #xf0 (ldb (byte 4 8) program-info-length))
          (aref section 11) (ldb (byte 8 0) program-info-length))
    (let ((offset
            (write-descriptor-loop
             (program-map-table-program-descriptors table)
             section 12)))
      (dolist (stream (program-map-table-streams table))
        (let ((info-length
                (descriptor-loop-length
                 (pmt-stream-descriptors stream))))
          (setf (aref section offset)
                (pmt-stream-stream-type stream)
                (aref section (+ offset 1))
                (logior #xe0
                        (ldb (byte 5 8)
                             (pmt-stream-elementary-pid stream)))
                (aref section (+ offset 2))
                (ldb (byte 8 0)
                     (pmt-stream-elementary-pid stream))
                (aref section (+ offset 3))
                (logior #xf0 (ldb (byte 4 8) info-length))
                (aref section (+ offset 4))
                (ldb (byte 8 0) info-length))
          (setf offset
                (write-descriptor-loop
                 (pmt-stream-descriptors stream)
                 section (+ offset 5))))))
    (write-u32-be (crc32-mpeg2 section :end (- (length section) 4))
                  section (- (length section) 4))
    section))

(defun increment-pmt-version (table)
  "TABLEのversionを5 bitで1増やす。"
  (setf (program-map-table-version table)
        (logand (+ (program-map-table-version table) 1) #x1f))
  table)
