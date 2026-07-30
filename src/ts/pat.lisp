;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defstruct pat-program
  (program-number 0 :type (unsigned-byte 16))
  (pid 0 :type (unsigned-byte 13)))

(defstruct program-association-table
  (transport-stream-id 0 :type (unsigned-byte 16))
  (version 0 :type (unsigned-byte 5))
  (current-next-p t :type boolean)
  (section-number 0 :type octet)
  (last-section-number 0 :type octet)
  (programs '() :type list))

(defun parse-pat-section (section)
  "PAT SECTIONをPROGRAM-ASSOCIATION-TABLEへ変換する。"
  (validate-long-psi-section section #x00)
  (let* ((entries-end (- (length section) 4))
         (entries-length (- entries-end 8))
         (programs '()))
    (unless (zerop (mod entries-length 4))
      (bridge-error "PAT program loop is not a multiple of four bytes: ~D"
                    entries-length))
    (loop for offset from 8 below entries-end by 4
          for program-number = (read-u16-be section offset)
          for pid = (logior
                     (ash (logand (aref section (+ offset 2)) #x1f) 8)
                     (aref section (+ offset 3)))
          do (unless (= (logand (aref section (+ offset 2)) #xe0)
                        #xe0)
               (bridge-error
                "PAT program PID reserved bits are invalid at offset ~D"
                offset))
             (push (make-pat-program
                    :program-number program-number
                    :pid pid)
                   programs))
    (make-program-association-table
     :transport-stream-id (read-u16-be section 3)
     :version (ldb (byte 5 1) (aref section 5))
     :current-next-p (logbitp 0 (aref section 5))
     :section-number (aref section 6)
     :last-section-number (aref section 7)
     :programs (nreverse programs))))

(defun build-pat-section (table)
  "PROGRAM-ASSOCIATION-TABLEからCRC付きPAT SECTIONを作る。"
  (let* ((programs (program-association-table-programs table))
         (section-length (+ 9 (* 4 (length programs))))
         (section (make-array (+ section-length 3)
                              :element-type 'octet
                              :initial-element 0)))
    (when (> section-length 1021)
      (bridge-error "PAT section exceeds 1021 bytes: ~D"
                    section-length))
    (setf (aref section 0) #x00
          (aref section 1)
          (logior #xb0 (ldb (byte 4 8) section-length))
          (aref section 2) (ldb (byte 8 0) section-length))
    (write-u16-be
     (program-association-table-transport-stream-id table)
     section 3)
    (setf (aref section 5)
          (logior #xc0
                  (ash (program-association-table-version table) 1)
                  (if (program-association-table-current-next-p table)
                      1
                      0))
          (aref section 6)
          (program-association-table-section-number table)
          (aref section 7)
          (program-association-table-last-section-number table))
    (loop for program in programs
          for offset from 8 by 4
          do (write-u16-be (pat-program-program-number program)
                           section offset)
             (setf (aref section (+ offset 2))
                   (logior #xe0
                           (ldb (byte 5 8)
                                (pat-program-pid program)))
                   (aref section (+ offset 3))
                   (ldb (byte 8 0)
                        (pat-program-pid program))))
    (write-u32-be (crc32-mpeg2 section :end (- (length section) 4))
                  section (- (length section) 4))
    section))
