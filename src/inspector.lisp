;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defstruct pid-inspection
  (pid 0 :type (unsigned-byte 13))
  (packet-count 0 :type fixnum)
  (payload-packet-count 0 :type fixnum)
  (last-continuity-counter nil
                           :type (or null (unsigned-byte 4)))
  (last-payload-packet nil :type (or null octet-vector))
  (pcr-values '() :type list)
  (pts-values '() :type list)
  (dts-values '() :type list)
  (pes-header-buffer
    (make-array 32
                :element-type 'octet
                :adjustable t
                :fill-pointer 0)
    :type (vector octet))
  (pes-header-active-p nil :type boolean)
  (pes-header-recognized-p nil :type boolean)
  (pes-header-expected-length nil :type (or null fixnum)))

(defstruct ts-inspection
  (packet-count 0 :type fixnum)
  (pids (make-hash-table :test #'eql) :type hash-table)
  (pat-sections '() :type list)
  (pmt-sections '() :type list))

(defun ts-octets-to-packets (octets)
  "OCTETSを厳格に188-byte TS packet listへ変換する。"
  (unless (zerop (mod (length octets) +ts-packet-size+))
    (bridge-error "Transport stream byte length is not divisible by 188: ~D"
                  (length octets)))
  (loop for offset from 0 below (length octets)
          by +ts-packet-size+
        collect
        (subseq octets offset (+ offset +ts-packet-size+))))

(defun ensure-pid-inspection (inspection pid)
  "INSPECTION内のPID統計を返し、なければ作る。"
  (or (gethash pid (ts-inspection-pids inspection))
      (setf (gethash pid (ts-inspection-pids inspection))
            (make-pid-inspection :pid pid))))

(defun inspect-payload-continuity (state packet)
  "STATEへPACKETのpayload continuityを検証・記録する。"
  (unless (ts-has-payload-p packet)
    (when (ts-discontinuity-indicator-p packet)
      (setf (pid-inspection-last-continuity-counter state) nil
            (pid-inspection-last-payload-packet state) nil))
    (return-from inspect-payload-continuity nil))
  (let ((last-counter
          (pid-inspection-last-continuity-counter state))
        (last-packet
          (pid-inspection-last-payload-packet state))
        (counter (ts-continuity-counter packet)))
    (when (and last-counter (= counter last-counter))
      (cond
        ((and last-packet (equalp last-packet packet))
         (return-from inspect-payload-continuity :duplicate))
        ((not (ts-discontinuity-indicator-p packet))
         (bridge-error
          "Inspector finds a conflicting duplicate on PID 0x~4,'0X"
          (pid-inspection-pid state)))))
    (when (and last-counter
               (not (ts-discontinuity-indicator-p packet))
               (/= counter (logand (+ last-counter 1) #x0f)))
      (bridge-error
       "Inspector continuity error on PID 0x~4,'0X: expected=~D actual=~D"
       (pid-inspection-pid state)
       (logand (+ last-counter 1) #x0f)
       counter))
    (setf (pid-inspection-last-continuity-counter state) counter
          (pid-inspection-last-payload-packet state)
          (copy-seq packet))
    nil))

(defun reset-inspector-pes-header (state)
  "STATEのPES optional header逐次検査状態を初期化する。"
  (setf (fill-pointer
         (pid-inspection-pes-header-buffer state))
        0
        (pid-inspection-pes-header-active-p state) nil
        (pid-inspection-pes-header-recognized-p state) nil
        (pid-inspection-pes-header-expected-length state) nil)
  state)

(defun append-inspector-pes-header-octets (state packet)
  "PACKETのpayloadを必要なPES header長までSTATEへ追加する。"
  (let ((buffer (pid-inspection-pes-header-buffer state))
        (offset (ts-payload-offset packet)))
    (loop while (and offset
                     (< offset +ts-packet-size+)
                     (or (null
                          (pid-inspection-pes-header-expected-length
                           state))
                         (< (length buffer)
                            (pid-inspection-pes-header-expected-length
                             state))))
          do (vector-push-extend (aref packet offset) buffer)
             (incf offset)
             (when (and (= (length buffer) 3)
                        (/= (read-u24-be buffer 0) 1))
               (reset-inspector-pes-header state)
               (return))
             (when (= (length buffer) 3)
               (setf (pid-inspection-pes-header-recognized-p state)
                     t))
             (when (= (length buffer) 9)
               (unless (= (ldb (byte 2 6) (aref buffer 6)) 2)
                 (bridge-error
                  "Inspector finds an invalid PES optional header"))
               (setf
                (pid-inspection-pes-header-expected-length state)
                (+ 9 (aref buffer 8))))))
  state)

(defun inspect-complete-pes-header-timestamps (state)
  "STATEに完成したPES headerがあればPTS/DTSを返して状態を閉じる。"
  (let ((expected
          (pid-inspection-pes-header-expected-length state))
        (buffer (pid-inspection-pes-header-buffer state)))
    (unless (and expected (>= (length buffer) expected))
      (return-from inspect-complete-pes-header-timestamps
        (values nil nil)))
    (let ((flags (ldb (byte 2 6) (aref buffer 7))))
      (multiple-value-prog1
          (case flags
            (0 (values nil nil))
            (2
             (values
              (decode-pes-timestamp buffer 9 2)
              nil))
            (3
             (values
              (decode-pes-timestamp buffer 9 3)
              (decode-pes-timestamp buffer 14 1)))
            (otherwise
             (bridge-error
              "Inspector finds a forbidden PTS_DTS_flags value: ~D"
              flags)))
        (reset-inspector-pes-header state)))))

(defun inspect-pes-start-timestamps (state packet)
  "PACKET列からPES optional headerを逐次復元してPTS/DTSを返す。"
  (when (ts-discontinuity-indicator-p packet)
    (reset-inspector-pes-header state))
  (when (ts-payload-unit-start-p packet)
    (when (and (pid-inspection-pes-header-active-p state)
               (pid-inspection-pes-header-recognized-p state))
      (bridge-error "Inspector finds a truncated PES optional header"))
    (reset-inspector-pes-header state)
    (when (ts-has-payload-p packet)
      (setf (pid-inspection-pes-header-active-p state) t)))
  (when (and (pid-inspection-pes-header-active-p state)
             (ts-has-payload-p packet))
    (append-inspector-pes-header-octets state packet))
  (inspect-complete-pes-header-timestamps state))

(defun inspect-packet-metadata (state packet)
  "STATEへPACKETのPCR/PTS/DTSを記録する。"
  (when (ts-pcr-flag-p packet)
    (push (ts-pcr packet)
          (pid-inspection-pcr-values state)))
  (multiple-value-bind (pts dts)
      (inspect-pes-start-timestamps state packet)
    (when pts
      (push pts (pid-inspection-pts-values state)))
    (when dts
      (push dts (pid-inspection-dts-values state)))))

(defun install-inspector-pmt-assemblers
    (table pmt-assemblers)
  "PAT TABLEに列挙されたPMT PIDのassemblerを作る。"
  (dolist (program
           (program-association-table-programs table))
    (unless (zerop (pat-program-program-number program))
      (let ((pid (pat-program-pid program)))
        (unless (gethash pid pmt-assemblers)
          (setf (gethash pid pmt-assemblers)
                (make-section-assembler pid)))))))

(defun reverse-inspection-values (inspection)
  "INSPECTIONの時系列listを入力順へ戻す。"
  (setf (ts-inspection-pat-sections inspection)
        (nreverse (ts-inspection-pat-sections inspection))
        (ts-inspection-pmt-sections inspection)
        (nreverse (ts-inspection-pmt-sections inspection)))
  (maphash
   (lambda (pid state)
     (declare (ignore pid))
     (setf (pid-inspection-pcr-values state)
           (nreverse (pid-inspection-pcr-values state))
           (pid-inspection-pts-values state)
           (nreverse (pid-inspection-pts-values state))
           (pid-inspection-dts-values state)
           (nreverse (pid-inspection-dts-values state))))
   (ts-inspection-pids inspection))
  inspection)

(defun inspect-ts-octets (octets)
  "OCTETSのsync/CC/CRC/PCR/PTS/DTSを検証して統計を返す。"
  (let ((inspection (make-ts-inspection))
        (pat-assembler (make-section-assembler 0))
        (pmt-assemblers (make-hash-table :test #'eql)))
    (dolist (packet (ts-octets-to-packets octets))
      (validate-ts-packet packet)
      (when (ts-transport-error-p packet)
        (bridge-error "Inspector finds transport_error_indicator"))
      (let* ((pid (ts-pid packet))
             (state (ensure-pid-inspection inspection pid)))
        (incf (ts-inspection-packet-count inspection))
        (incf (pid-inspection-packet-count state))
        (when (ts-has-payload-p packet)
          (incf (pid-inspection-payload-packet-count state)))
        (unless (eq (inspect-payload-continuity state packet)
                    :duplicate)
          (inspect-packet-metadata state packet))
        (when (zerop pid)
          (dolist (section
                   (feed-section-packet pat-assembler packet))
            (let ((table (parse-pat-section section)))
              (push section
                    (ts-inspection-pat-sections inspection))
              (install-inspector-pmt-assemblers
               table pmt-assemblers))))
        (let ((pmt-assembler (gethash pid pmt-assemblers)))
          (when pmt-assembler
            (dolist (section
                     (feed-section-packet pmt-assembler packet))
              (parse-pmt-section section)
              (push section
                    (ts-inspection-pmt-sections inspection)))))))
    (when (incomplete-section-buffer-p pat-assembler)
      (bridge-error "Inspector finds an incomplete PAT"))
    (maphash
     (lambda (pid assembler)
       (when (incomplete-section-buffer-p assembler)
         (bridge-error
          "Inspector finds an incomplete PMT on PID 0x~4,'0X"
          pid)))
     pmt-assemblers)
    (maphash
     (lambda (pid state)
       (when (and (pid-inspection-pes-header-active-p state)
                  (pid-inspection-pes-header-recognized-p state))
         (bridge-error
          "Inspector finds a truncated PES optional header on PID 0x~4,'0X"
          pid)))
     (ts-inspection-pids inspection))
    (reverse-inspection-values inspection)))

(defun non-target-packets-byte-exact-p
    (input-octets output-octets target-pids)
  "TARGET-PIDS以外のpacket列がbyte-exactかつ同順かを返す。"
  (let ((input
          (remove-if
           (lambda (packet)
             (member (ts-pid packet) target-pids :test #'=))
           (ts-octets-to-packets input-octets)))
        (output
          (remove-if
           (lambda (packet)
             (member (ts-pid packet) target-pids :test #'=))
           (ts-octets-to-packets output-octets))))
    (and (= (length input) (length output))
         (every #'equalp input output))))
