;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defconstant +ts-sync-byte+ #x47)
(defconstant +ts-null-pid+ #x1fff)

(defun ts-assignable-pid-p (pid)
  "PIDがnetwork、program map、elementary streamへ割当可能なら真。"
  (<= #x0010 pid #x1ffe))

(defun ts-pcr-pid-p (pid)
  "PIDがPCR_PIDとして予約値を避けた値またはno-PCR値なら真。"
  (or (= pid 0)
      (= pid 1)
      (ts-assignable-pid-p pid)
      (= pid +ts-null-pid+)))

(defun validate-ts-adaptation-field (packet)
  "PACKETのadaptation field長とflag依存field境界を検証する。"
  (let ((control (ldb (byte 2 4) (aref packet 3))))
    (unless (member control '(2 3) :test #'=)
      (return-from validate-ts-adaptation-field packet))
    (let* ((length (aref packet 4))
           (end (+ 5 length)))
      (when (or (and (= control 2) (/= length 183))
                (and (= control 3) (> length 182)))
        (bridge-error
         "Transport packet adaptation field length is invalid: control=~D length=~D"
         control length))
      (when (zerop length)
        (return-from validate-ts-adaptation-field packet))
      (let ((flags (aref packet 5))
            (position 6))
        (flet ((consume-fixed (count name)
                 (when (> (+ position count) end)
                   (bridge-error
                    "Transport packet ~A field is truncated"
                    name))
                 (incf position count))
               (consume-length-prefixed (name)
                 (when (>= position end)
                   (bridge-error
                    "Transport packet ~A length is truncated"
                    name))
                 (let ((count (aref packet position)))
                   (incf position)
                   (when (> (+ position count) end)
                     (bridge-error
                      "Transport packet ~A field is truncated"
                      name))
                   (incf position count))))
          (when (logbitp 4 flags)
            (consume-fixed 6 "PCR"))
          (when (logbitp 3 flags)
            (consume-fixed 6 "OPCR"))
          (when (logbitp 2 flags)
            (consume-fixed 1 "splice countdown"))
          (when (logbitp 1 flags)
            (consume-length-prefixed "private data"))
          (when (logbitp 0 flags)
            (consume-length-prefixed "adaptation extension"))))))
  packet)

(defun validate-ts-packet (packet)
  "PACKETが188 byteの有効なMPEG-TS packet headerを持つことを検証する。"
  (unless (= (length packet) +ts-packet-size+)
    (bridge-error "Transport packet size is not 188 bytes: ~D"
                  (length packet)))
  (unless (= (aref packet 0) +ts-sync-byte+)
    (bridge-error "Transport packet sync byte is invalid: 0x~2,'0X"
                  (aref packet 0)))
  (when (zerop (ldb (byte 2 4) (aref packet 3)))
    (bridge-error "Transport packet adaptation_field_control is zero"))
  (validate-ts-adaptation-field packet)
  packet)

(declaim
 (inline ts-transport-error-p
         ts-payload-unit-start-p
         ts-transport-priority-p
         ts-pid
         ts-scrambling-control
         ts-adaptation-field-control
         ts-continuity-counter
         ts-has-adaptation-field-p
         ts-has-payload-p))

(defun ts-transport-error-p (packet)
  "PACKETのtransport_error_indicatorを返す。"
  (logbitp 7 (aref packet 1)))

(defun ts-payload-unit-start-p (packet)
  "PACKETのpayload_unit_start_indicatorを返す。"
  (logbitp 6 (aref packet 1)))

(defun ts-transport-priority-p (packet)
  "PACKETのtransport_priorityを返す。"
  (logbitp 5 (aref packet 1)))

(defun ts-pid (packet)
  "PACKETの13 bit PIDを返す。"
  (logior (ash (logand (aref packet 1) #x1f) 8)
          (aref packet 2)))

(defun ts-scrambling-control (packet)
  "PACKETのtransport_scrambling_controlを返す。"
  (ldb (byte 2 6) (aref packet 3)))

(defun ts-adaptation-field-control (packet)
  "PACKETのadaptation_field_controlを返す。"
  (ldb (byte 2 4) (aref packet 3)))

(defun ts-continuity-counter (packet)
  "PACKETのcontinuity_counterを返す。"
  (logand (aref packet 3) #x0f))

(defun ts-has-adaptation-field-p (packet)
  "PACKETがadaptation fieldを持つかを返す。"
  (member (ts-adaptation-field-control packet) '(2 3) :test #'=))

(defun ts-has-payload-p (packet)
  "PACKETがpayloadを持つかを返す。"
  (member (ts-adaptation-field-control packet) '(1 3) :test #'=))

(defun ts-adaptation-field-length (packet)
  "PACKETのadaptation field payload長を返す。"
  (if (ts-has-adaptation-field-p packet)
      (aref packet 4)
      0))

(defun ts-payload-offset (packet)
  "PACKETのpayload先頭offsetを返す。payloadがなければNILを返す。"
  (unless (ts-has-payload-p packet)
    (return-from ts-payload-offset nil))
  (let ((offset
          (if (ts-has-adaptation-field-p packet)
              (+ 5 (ts-adaptation-field-length packet))
              4)))
    (when (> offset +ts-packet-size+)
      (bridge-error "Transport packet adaptation field exceeds packet: ~D"
                    offset))
    offset))

(defun ts-adaptation-flags (packet)
  "PACKETのadaptation field flagsを返す。flags byteがなければ0を返す。"
  (if (and (ts-has-adaptation-field-p packet)
           (plusp (ts-adaptation-field-length packet)))
      (aref packet 5)
      0))

(defun ts-discontinuity-indicator-p (packet)
  "PACKETのdiscontinuity_indicatorを返す。"
  (logbitp 7 (ts-adaptation-flags packet)))

(defun ts-random-access-indicator-p (packet)
  "PACKETのrandom_access_indicatorを返す。"
  (logbitp 6 (ts-adaptation-flags packet)))

(defun ts-elementary-stream-priority-indicator-p (packet)
  "PACKETのelementary_stream_priority_indicatorを返す。"
  (logbitp 5 (ts-adaptation-flags packet)))

(defun ts-pcr-flag-p (packet)
  "PACKETのPCR_flagを返す。"
  (logbitp 4 (ts-adaptation-flags packet)))

(defun ts-pcr (packet)
  "PACKETのPCRを27MHz tickで返す。PCRがなければNILを返す。"
  (unless (ts-pcr-flag-p packet)
    (return-from ts-pcr nil))
  (when (< (ts-adaptation-field-length packet) 7)
    (bridge-error "Transport packet PCR is truncated"))
  (unless (= (logand (aref packet 10) #x7e) #x7e)
    (bridge-error "Transport packet PCR reserved bits are invalid"))
  (let ((base (logior (ash (aref packet 6) 25)
                      (ash (aref packet 7) 17)
                      (ash (aref packet 8) 9)
                      (ash (aref packet 9) 1)
                      (ldb (byte 1 7) (aref packet 10))))
        (extension (logior (ash (logand (aref packet 10) 1) 8)
                           (aref packet 11))))
    (when (> extension 299)
      (bridge-error "Transport packet PCR extension is invalid: ~D"
                    extension))
    (+ (* base 300) extension)))

(defun set-ts-continuity-counter (packet value)
  "PACKETのcontinuity_counterをVALUEへ更新する。"
  (unless (typep value '(unsigned-byte 4))
    (bridge-error "Continuity counter does not fit in 4 bits: ~D" value))
  (setf (aref packet 3)
        (logior (logand (aref packet 3) #xf0)
                value))
  packet)

(defun set-ts-payload-unit-start (packet enabled)
  "PACKETのpayload_unit_start_indicatorをENABLEDにする。"
  (setf (aref packet 1)
        (if enabled
            (logior (aref packet 1) #x40)
            (logand (aref packet 1) #xbf)))
  packet)

(defun make-ts-packet (pid continuity-counter payload
                       &key payload-unit-start
                         random-access
                         elementary-stream-priority
                         transport-priority)
  "PIDとPAYLOADからstuffing付きTS packetを1個作る。"
  (unless (typep pid '(unsigned-byte 13))
    (bridge-error "PID does not fit in 13 bits: ~D" pid))
  (unless (typep continuity-counter '(unsigned-byte 4))
    (bridge-error "Continuity counter does not fit in 4 bits: ~D"
                  continuity-counter))
  (when (> (length payload) 184)
    (bridge-error "Payload does not fit in one transport packet: ~D"
                  (length payload)))
  (when (and (or random-access
                 elementary-stream-priority)
             (> (length payload) 182))
    (bridge-error
     "Adaptation flags packet payload leaves no flags byte: ~D"
     (length payload)))
  (let* ((needs-adaptation (or random-access
                               elementary-stream-priority
                               (< (length payload) 184)))
         (packet (make-array +ts-packet-size+
                             :element-type 'octet
                             :initial-element #xff))
         (adaptation-length
           (if needs-adaptation
               (- 183 (length payload))
               0))
         (payload-offset
           (if needs-adaptation
               (+ 5 adaptation-length)
               4)))
    (setf (aref packet 0) +ts-sync-byte+
          (aref packet 1) (logior (if payload-unit-start #x40 0)
                                  (if transport-priority #x20 0)
                                  (ldb (byte 5 8) pid))
          (aref packet 2) (ldb (byte 8 0) pid)
          (aref packet 3) (logior (if needs-adaptation #x30 #x10)
                                  continuity-counter))
    (when needs-adaptation
      (setf (aref packet 4) adaptation-length)
      (when (plusp adaptation-length)
        (setf (aref packet 5)
              (logior (if random-access #x40 0)
                      (if elementary-stream-priority #x20 0)))))
    (replace packet payload :start1 payload-offset)
    packet))

(defun packetize-payload (pid payload
                          &key
                            (continuity-counter 0)
                            payload-unit-start
                            random-access
                            elementary-stream-priority
                            transport-priority)
  "任意長PAYLOADをPIDのTS packet列へ分割する。"
  (let ((packets '())
        (offset 0)
        (counter continuity-counter)
        (first-p t))
    (when (zerop (length payload))
      (bridge-error "Cannot packetize an empty payload"))
    (loop while (< offset (length payload))
          for remaining = (- (length payload) offset)
          for capacity = (if (and first-p
                                  (or random-access
                                      elementary-stream-priority))
                             182
                             184)
          for count = (min capacity remaining)
          for chunk = (subseq payload offset (+ offset count))
          do (push (make-ts-packet
                    pid counter chunk
                    :payload-unit-start (and first-p payload-unit-start)
                    :random-access (and first-p random-access)
                    :elementary-stream-priority
                    (and first-p elementary-stream-priority)
                    :transport-priority
                    (and first-p transport-priority))
                   packets)
             (setf offset (+ offset count)
                   counter (logand (+ counter 1) #x0f)
                   first-p nil))
    (nreverse packets)))
