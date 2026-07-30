;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defconstant +tstd-system-clock-rate+ 27000000)
(defconstant +tstd-pcr-modulus+ (* (ash 1 33) 300))
(defconstant +tstd-pcr-half-modulus+ (ash +tstd-pcr-modulus+ -1))
(defconstant +tstd-timestamp-modulus+ (ash 1 33))
(defconstant +tstd-timestamp-half-modulus+
  (ash +tstd-timestamp-modulus+ -1))
(defconstant +tstd-pcr-residual-limit+ 27/2)

(defstruct (tstd-arrival-clock
            (:constructor %make-tstd-arrival-clock
                (transport-rate-bps)))
  (transport-rate-bps 0 :type (integer 1 *))
  (first-pcr-byte-index nil :type (or null integer))
  (first-pcr-unwrapped nil :type (or null integer))
  (last-pcr-raw nil :type (or null integer))
  (last-pcr-unwrapped nil :type (or null integer)))

(defun make-tstd-arrival-clock (transport-rate-kbps)
  "正整数KBIT/SからCBR arrival clockを作る。"
  (unless (and (integerp transport-rate-kbps)
               (plusp transport-rate-kbps))
    (bridge-error
     "TSTD_TRANSPORT_RATE_INVALID kbps=~S"
     transport-rate-kbps))
  (%make-tstd-arrival-clock (* transport-rate-kbps 1000)))

(defun reset-tstd-arrival-clock-epoch (clock)
  "PCR discontinuity後のarrival/PCR対応を初期化する。"
  (setf
   (tstd-arrival-clock-first-pcr-byte-index clock) nil
   (tstd-arrival-clock-first-pcr-unwrapped clock) nil
   (tstd-arrival-clock-last-pcr-raw clock) nil
   (tstd-arrival-clock-last-pcr-unwrapped clock) nil)
  clock)

(defun tstd-byte-arrival-time (clock byte-index)
  "TS先頭を0秒とするBYTE-INDEXのCBR arrival秒を有理数で返す。"
  (unless (and (integerp byte-index) (not (minusp byte-index)))
    (bridge-error "TSTD_BYTE_INDEX_INVALID index=~S" byte-index))
  (/ (* byte-index 8)
     (tstd-arrival-clock-transport-rate-bps clock)))

(defun tstd-packet-arrival-time (clock packet-index)
  "PACKET-INDEXの先頭byte arrival秒を返す。"
  (tstd-byte-arrival-time
   clock
   (* packet-index +ts-packet-size+)))

(defun tstd-pcr-anchor-byte-index (packet-index)
  "PCR clock基準に使うpacket内byte位置を返す。"
  (+ (* packet-index +ts-packet-size+) 10))

(defun unwrap-tstd-pcr (clock pcr)
  "CLOCKの直前値へPCRを42-bit wrap込みで展開する。"
  (let ((last-raw (tstd-arrival-clock-last-pcr-raw clock))
        (last-unwrapped
          (tstd-arrival-clock-last-pcr-unwrapped clock)))
    (if last-raw
        (let* ((forward
                 (mod (- pcr last-raw) +tstd-pcr-modulus+))
               (delta
                 (if (>= forward +tstd-pcr-half-modulus+)
                     (- forward +tstd-pcr-modulus+)
                     forward)))
          (+ last-unwrapped delta))
        pcr)))

(defun observe-tstd-pcr (clock packet-index pcr)
  "PCRと明示CBR arrivalの残差を13.5 system tick以内で検証する。"
  (let ((byte-index (tstd-pcr-anchor-byte-index packet-index))
         (unwrapped (unwrap-tstd-pcr clock pcr))
         (first-byte
           (tstd-arrival-clock-first-pcr-byte-index clock))
         (first-pcr
           (tstd-arrival-clock-first-pcr-unwrapped clock)))
    (when first-pcr
      (let* ((expected
               (+ first-pcr
                  (/ (* (- byte-index first-byte)
                        8
                        +tstd-system-clock-rate+)
                     (tstd-arrival-clock-transport-rate-bps
                      clock))))
             (residual (abs (- unwrapped expected))))
        (when (> residual +tstd-pcr-residual-limit+)
          (bridge-error
           "TSTD_PCR_CBR_RESIDUAL_EXCEEDED residual_ticks=~A limit_ticks=~A"
           residual
           +tstd-pcr-residual-limit+))))
    (unless first-pcr
      (setf
       (tstd-arrival-clock-first-pcr-byte-index clock)
       byte-index
       (tstd-arrival-clock-first-pcr-unwrapped clock)
       unwrapped))
    (setf
     (tstd-arrival-clock-last-pcr-raw clock) pcr
     (tstd-arrival-clock-last-pcr-unwrapped clock) unwrapped)
    unwrapped))

(defun tstd-timestamp-time (clock timestamp)
  "直近PCR epochへ33-bit TIMESTAMPを展開しCBR arrival秒へ写像する。"
  (let ((pcr
          (or
           (tstd-arrival-clock-last-pcr-unwrapped clock)
           (bridge-error "TSTD_PCR_ANCHOR_MISSING")))
        (first-pcr
          (or
           (tstd-arrival-clock-first-pcr-unwrapped clock)
           (bridge-error "TSTD_PCR_ANCHOR_MISSING")))
        (first-byte
          (or
           (tstd-arrival-clock-first-pcr-byte-index clock)
           (bridge-error "TSTD_PCR_ANCHOR_MISSING"))))
    (let* ((pcr-timestamp (floor pcr 300))
           (pcr-modulo
             (mod pcr-timestamp +tstd-timestamp-modulus+))
           (forward
             (mod
              (- timestamp pcr-modulo)
              +tstd-timestamp-modulus+))
           (delta
             (if (>= forward +tstd-timestamp-half-modulus+)
                 (- forward +tstd-timestamp-modulus+)
                 forward))
           (timestamp-pcr (+ pcr (* delta 300))))
      (+ (tstd-byte-arrival-time clock first-byte)
         (/ (- timestamp-pcr first-pcr)
            +tstd-system-clock-rate+)))))
