;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defun section-total-length (prefix)
  "3 byte以上のSECTION PREFIXからsection全長を返す。"
  (ensure-octet-range prefix 0 3 :section-total-length)
  (let ((section-length
          (logior (ash (logand (aref prefix 1) #x0f) 8)
                  (aref prefix 2))))
    (when (> section-length 1021)
      (bridge-error "PSI section_length exceeds 1021: ~D"
                    section-length))
    (+ section-length 3)))

(defun validate-long-psi-section (section expected-table-id)
  "CRC付きlong PSI SECTIONの共通headerを検証する。"
  (ensure-octet-range section 0 8 :validate-long-psi-section)
  (unless (= (aref section 0) expected-table-id)
    (bridge-error "Unexpected PSI table id: expected=0x~2,'0X actual=0x~2,'0X"
                  expected-table-id (aref section 0)))
  (unless (logbitp 7 (aref section 1))
    (bridge-error "PSI section_syntax_indicator is zero"))
  (when (logbitp 6 (aref section 1))
    (bridge-error "PSI zero bit is not zero"))
  (unless (= (logand (aref section 1) #x30) #x30)
    (bridge-error "PSI section header reserved bits are invalid"))
  (unless (= (logand (aref section 5) #xc0) #xc0)
    (bridge-error "PSI version header reserved bits are invalid"))
  (unless (= (section-total-length section) (length section))
    (bridge-error "PSI section length mismatch: declared=~D actual=~D"
                  (section-total-length section)
                  (length section)))
  (unless (valid-crc32-mpeg2-p section)
    (bridge-error "PSI section CRC32/MPEG-2 is invalid"))
  section)

(defstruct (section-assembler
            (:constructor make-section-assembler (pid)))
  (pid 0 :type (unsigned-byte 13) :read-only t)
  (buffer (make-array 0
                      :element-type 'octet
                      :adjustable t
                      :fill-pointer 0)
          :type (vector octet))
  (expected-length nil :type (or null fixnum))
  (last-continuity-counter nil :type (or null (unsigned-byte 4)))
  (last-packet nil :type (or null octet-vector)))

(defun reset-section-buffer (assembler)
  "ASSEMBLERの作成中sectionだけを破棄する。"
  (setf (fill-pointer (section-assembler-buffer assembler)) 0
        (section-assembler-expected-length assembler) nil)
  assembler)

(defun append-section-range (assembler octets start end)
  "OCTETSのSTARTからENDをASSEMBLERの作成中sectionへ追加する。"
  (loop for position from start below end
        do (vector-push-extend
            (aref octets position)
            (section-assembler-buffer assembler)))
  (when (and (null (section-assembler-expected-length assembler))
             (>= (length (section-assembler-buffer assembler)) 3))
    (setf (section-assembler-expected-length assembler)
          (section-total-length
           (section-assembler-buffer assembler))))
  assembler)

(defun complete-section-p (assembler)
  "ASSEMBLERがsection全体を保持しているかを返す。"
  (let ((expected (section-assembler-expected-length assembler)))
    (and expected
         (= (length (section-assembler-buffer assembler))
            expected))))

(defun oversized-section-p (assembler)
  "ASSEMBLERのbufferが宣言section長を超えたかを返す。"
  (let ((expected (section-assembler-expected-length assembler)))
    (and expected
         (> (length (section-assembler-buffer assembler))
            expected))))

(defun take-complete-section (assembler)
  "ASSEMBLERから完成sectionを取り出してbufferを空にする。"
  (unless (complete-section-p assembler)
    (bridge-error "Attempted to take an incomplete PSI section"))
  (let ((section
          (coerce (section-assembler-buffer assembler)
                  '(simple-array (unsigned-byte 8) (*)))))
    (reset-section-buffer assembler)
    section))

(defun append-exact-section-tail (assembler packet start end)
  "既存sectionへPACKET内のtailを加え、完成sectionまたはNILを返す。"
  (when (> end start)
    (append-section-range assembler packet start end))
  (when (oversized-section-p assembler)
    (bridge-error "PSI pointer tail exceeds declared section length"))
  (cond
    ((complete-section-p assembler)
     (take-complete-section assembler))
    ((plusp (length (section-assembler-buffer assembler)))
     (bridge-error "PSI pointer starts a new section before the old section ends"))
    (t nil)))

(defun consume-new-sections (assembler packet start end)
  "PACKETのSTARTからENDに並ぶ新規sectionを消費する。"
  (let ((sections '())
        (position start))
    (loop while (< position end)
          do (when (= (aref packet position) #xff)
               (loop for stuffing-position from position below end
                     unless (= (aref packet stuffing-position) #xff)
                       do (bridge-error
                           "Non-stuffing data follows PSI stuffing"))
               (return))
             (let ((remaining (- end position)))
               (cond
                 ((< remaining 3)
                  (append-section-range assembler packet position end)
                  (setf position end))
                 (t
                  (let ((total-length
                          (section-total-length
                           (subseq packet position (+ position 3)))))
                    (cond
                      ((<= total-length remaining)
                       (push (subseq packet
                                     position
                                     (+ position total-length))
                             sections)
                       (incf position total-length))
                      (t
                       (append-section-range assembler
                                             packet position end)
                       (setf position end))))))))
    (nreverse sections)))

(defun validate-section-continuity (assembler packet)
  "PACKETのcontinuity counterを検査し、duplicateならNILを返す。"
  (let ((last-counter
          (section-assembler-last-continuity-counter assembler))
        (counter (ts-continuity-counter packet))
        (last-packet (section-assembler-last-packet assembler)))
    (when (and last-counter
               (= counter last-counter))
      (if (and last-packet (equalp packet last-packet))
          (return-from validate-section-continuity nil)
          (bridge-error "Conflicting duplicate PSI packet on PID 0x~4,'0X"
                        (section-assembler-pid assembler))))
    (when (and last-counter
               (not (ts-discontinuity-indicator-p packet))
               (/= counter (logand (+ last-counter 1) #x0f)))
      (bridge-error
       "PSI continuity error on PID 0x~4,'0X: expected=~D actual=~D"
       (section-assembler-pid assembler)
       (logand (+ last-counter 1) #x0f)
       counter))
    (setf (section-assembler-last-continuity-counter assembler) counter
          (section-assembler-last-packet assembler) (copy-seq packet))
    t))

(defun exact-section-packet-duplicate-p (assembler packet)
  "PACKETが直前payload packetと完全に同じなら真を返す。"
  (and (ts-has-payload-p packet)
       (section-assembler-last-continuity-counter assembler)
       (= (ts-continuity-counter packet)
          (section-assembler-last-continuity-counter assembler))
       (section-assembler-last-packet assembler)
       (equalp packet
               (section-assembler-last-packet assembler))))

(defun reset-section-transport-state (assembler)
  "PSI discontinuity後のsectionとcontinuity基準を破棄する。"
  (reset-section-buffer assembler)
  (setf (section-assembler-last-continuity-counter assembler) nil
        (section-assembler-last-packet assembler) nil)
  assembler)

(defun consume-section-continuation (assembler packet start end)
  "継続PACKETから宣言長までを読み、残りの0xFF stuffingを検証する。"
  (let ((position start))
    (loop while (and (< position end)
                     (not (complete-section-p assembler)))
          do
      (let* ((expected
               (section-assembler-expected-length assembler))
             (needed
               (if expected
                   (- expected
                      (length
                       (section-assembler-buffer assembler)))
                   (- 3
                      (length
                       (section-assembler-buffer assembler)))))
             (count (min needed (- end position))))
        (append-section-range
         assembler packet position (+ position count))
        (incf position count)))
    (when (complete-section-p assembler)
      (loop for stuffing-position from position below end
            unless (= (aref packet stuffing-position) #xff)
              do (bridge-error
                  "Non-stuffing data follows a completed PSI section"))
      (return-from consume-section-continuation
        (list (take-complete-section assembler))))
    nil))

(defun feed-section-packet (assembler packet)
  "ASSEMBLERへTS PACKETを与え、完成したPSI sectionのlistを返す。"
  (validate-ts-packet packet)
  (unless (= (ts-pid packet)
             (section-assembler-pid assembler))
    (bridge-error "Unexpected PID for PSI assembler: expected=0x~4,'0X actual=0x~4,'0X"
                  (section-assembler-pid assembler)
                  (ts-pid packet)))
  (when (ts-transport-error-p packet)
    (bridge-error "Transport error indicator is set on PSI PID 0x~4,'0X"
                  (section-assembler-pid assembler)))
  (when (exact-section-packet-duplicate-p assembler packet)
    (return-from feed-section-packet nil))
  (when (ts-discontinuity-indicator-p packet)
    (reset-section-transport-state assembler))
  (unless (ts-has-payload-p packet)
    (return-from feed-section-packet nil))
  (unless (validate-section-continuity assembler packet)
    (return-from feed-section-packet nil))
  (let ((payload-offset (ts-payload-offset packet)))
    (unless payload-offset
      (return-from feed-section-packet nil))
    (cond
      ((ts-payload-unit-start-p packet)
       (let* ((pointer (aref packet payload-offset))
              (tail-start (+ payload-offset 1))
              (section-start (+ tail-start pointer))
              (sections '()))
         (when (> section-start +ts-packet-size+)
           (bridge-error "PSI pointer field exceeds transport packet: ~D"
                         pointer))
         (if (plusp (length (section-assembler-buffer assembler)))
             (let ((completed
                     (append-exact-section-tail
                      assembler packet tail-start section-start)))
               (when completed
                 (push completed sections)))
             (reset-section-buffer assembler))
         (nconc (nreverse sections)
                (consume-new-sections assembler
                                      packet
                                      section-start
                                      +ts-packet-size+))))
      (t
       (when (zerop (length (section-assembler-buffer assembler)))
         (return-from feed-section-packet nil))
       (consume-section-continuation
        assembler packet payload-offset +ts-packet-size+)))))
