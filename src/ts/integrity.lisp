;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defconstant +ts-pid-count+ #x2000
  "13-bit PID空間の要素数。")

(defstruct
    (payload-continuity-validator
     (:constructor make-payload-continuity-validator ()))
  (seen
    (make-array +ts-pid-count+
                :element-type 'bit
                :initial-element 0)
    :type simple-bit-vector)
  (counters
    (make-array +ts-pid-count+
                :element-type 'octet
                :initial-element 0)
    :type octet-vector)
  (last-packets
    (make-array (* +ts-pid-count+ +ts-packet-size+)
                :element-type 'octet
                :initial-element 0)
    :type octet-vector))

(declaim
 (inline payload-continuity-last-packet-equal-p
         remember-payload-continuity-packet))

(defun payload-continuity-last-packet-equal-p
    (validator pid packet)
  "VALIDATORに保存したPIDの直前payload packetとPACKETを比較する。"
  (let ((last-packets
          (payload-continuity-validator-last-packets validator))
        (start (* pid +ts-packet-size+)))
    (loop for source-index fixnum from 0 below +ts-packet-size+
          for saved-index fixnum from start
          always
          (= (aref packet source-index)
             (aref last-packets saved-index)))))

(defun remember-payload-continuity-packet
    (validator pid counter packet)
  "VALIDATORへPIDの直前payload packetとCOUNTERを保存する。"
  (let ((start (* pid +ts-packet-size+)))
    (setf
     (sbit
      (payload-continuity-validator-seen validator)
      pid)
     1
     (aref
      (payload-continuity-validator-counters validator)
      pid)
     counter)
    (replace
     (payload-continuity-validator-last-packets validator)
     packet
     :start1 start
     :end1 (+ start +ts-packet-size+)))
  validator)

(defun validate-payload-continuity
    (validator packet)
  "PACKETのpayload continuityをPID単位でfail closed検証する。"
  (let ((pid (ts-pid packet))
        (seen
          (payload-continuity-validator-seen validator)))
    ;; Null packetのcontinuity_counterには連続性を要求しない。
    (when (= pid +ts-null-pid+)
      (return-from validate-payload-continuity nil))
    (unless (ts-has-payload-p packet)
      ;; adaptation-only discontinuityは次のpayloadを新基準にする。
      (when (ts-discontinuity-indicator-p packet)
        (setf (sbit seen pid) 0))
      (return-from validate-payload-continuity nil))
    (let ((counter (ts-continuity-counter packet))
          (discontinuity-p
            (ts-discontinuity-indicator-p packet)))
      (when (= (sbit seen pid) 1)
        (let ((last-counter
                (aref
                 (payload-continuity-validator-counters validator)
                 pid)))
          (when (= counter last-counter)
            (cond
              ((payload-continuity-last-packet-equal-p
                validator pid packet)
               (return-from validate-payload-continuity :duplicate))
              ((not discontinuity-p)
               (bridge-error
                "Conflicting duplicate transport packet on PID 0x~4,'0X"
                pid))))
          (when (and
                 (not discontinuity-p)
                 (/= counter
                     (logand (+ last-counter 1) #x0f)))
            (bridge-error
             "Transport continuity error on PID 0x~4,'0X: expected=~D actual=~D"
             pid
             (logand (+ last-counter 1) #x0f)
             counter))))
      (remember-payload-continuity-packet
       validator pid counter packet)))
  nil)

(defun validate-ts-packet-integrity
    (validator packet)
  "PACKETのTEI、scrambling、payload continuityを検証する。"
  (when (ts-transport-error-p packet)
    (bridge-error
     "Transport error indicator is set on PID 0x~4,'0X"
     (ts-pid packet)))
  (unless (zerop (ts-scrambling-control packet))
    (bridge-error
     "Scrambled transport packet is unsupported on PID 0x~4,'0X"
     (ts-pid packet)))
  (validate-payload-continuity validator packet))
