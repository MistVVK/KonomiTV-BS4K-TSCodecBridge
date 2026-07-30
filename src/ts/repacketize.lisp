;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defun template-adaptation-prefix-length (packet)
  "PACKETからflag依存fieldを含む保持必須byte数だけを返す。"
  (unless (ts-has-adaptation-field-p packet)
    (return-from template-adaptation-prefix-length 0))
  (let ((length (ts-adaptation-field-length packet)))
    (when (zerop length)
      (return-from template-adaptation-prefix-length 0))
    (let ((flags (aref packet 5))
          (position 6)
          (end (+ 5 length)))
      (flet ((consume-fixed (count)
               (incf position count))
             (consume-length-prefixed ()
               (let ((count (aref packet position)))
                 (incf position (+ 1 count)))))
        (when (logbitp 4 flags)
          (consume-fixed 6))
        (when (logbitp 3 flags)
          (consume-fixed 6))
        (when (logbitp 2 flags)
          (consume-fixed 1))
        (when (logbitp 1 flags)
          (consume-length-prefixed))
        (when (logbitp 0 flags)
          (consume-length-prefixed)))
      ;; validate-ts-packet後に呼ばれるが、意味field後のstuffing値も
      ;; 厳格に確認してからのみpayload headroomへ転用する。
      (loop for index from position below end
            unless (= (aref packet index) #xff)
              do (bridge-error
                  "ADAPTATION_STUFFING_INVALID offset=~D value=0x~2,'0X"
                  index
                  (aref packet index)))
      (- position 5))))

(defun template-payload-capacity (packet adaptation-flags)
  "PACKETのadaptation情報を保持したときのpayload容量を返す。"
  (let ((prefix-length
          (max (template-adaptation-prefix-length packet)
               (if (plusp adaptation-flags) 1 0))))
    (if (plusp prefix-length)
        (- 183 prefix-length)
        184)))

(defun copy-template-header (template packet payload-unit-start)
  "TEMPLATEのPIDとtransport priorityをPACKETへ複写する。"
  (setf (aref packet 0) +ts-sync-byte+
        (aref packet 1)
        (logior (logand (aref template 1) #x3f)
                (if payload-unit-start #x40 0))
        (aref packet 2) (aref template 2))
  packet)

(defun make-payload-packet-from-template
    (template payload continuity-counter
     &key payload-unit-start (adaptation-flags 0)
          (clear-adaptation-flags 0))
  "TEMPLATEのadaptation情報を維持しPAYLOAD packetを作る。"
  (let* ((prefix-length
           (max (template-adaptation-prefix-length template)
                (if (plusp adaptation-flags) 1 0)))
         (capacity
           (if (plusp prefix-length)
               (- 183 prefix-length)
               184))
         (needs-adaptation
           (or (plusp prefix-length)
               (< (length payload) 184)))
         (adaptation-length
           (if needs-adaptation
               (max prefix-length
                    (- 183 (length payload)))
               0))
         (payload-offset
           (if needs-adaptation
               (+ 5 adaptation-length)
               4))
         (packet
           (make-array +ts-packet-size+
                       :element-type 'octet
                       :initial-element #xff)))
    (when (> (length payload) capacity)
      (bridge-error
       "Payload exceeds preserved transport packet capacity: payload=~D capacity=~D"
       (length payload) capacity))
    (copy-template-header template packet payload-unit-start)
    (setf (aref packet 3)
          (logior (if needs-adaptation #x30 #x10)
                  continuity-counter))
    (when needs-adaptation
      (setf (aref packet 4) adaptation-length)
      (let ((original-length
              (template-adaptation-prefix-length template)))
        (when (plusp original-length)
          (replace packet template
                   :start1 5
                   :start2 5
                   :end2 (+ 5 original-length))))
      (when (plusp adaptation-length)
        (setf (aref packet 5)
              (logior (if (plusp
                           (template-adaptation-prefix-length template))
                          (logandc2
                           (aref packet 5)
                           clear-adaptation-flags)
                          0)
                      adaptation-flags))))
    (replace packet payload :start1 payload-offset)
    packet))

(defun make-adaptation-only-from-template
    (template continuity-counter &key (clear-adaptation-flags 0))
  "TEMPLATEのadaptation情報を保持したadaptation-only packetを作る。"
  (unless (ts-has-adaptation-field-p template)
    (return-from make-adaptation-only-from-template nil))
  (let ((original-length (ts-adaptation-field-length template))
         (packet
           (make-array +ts-packet-size+
                       :element-type 'octet
                       :initial-element #xff)))
    (copy-template-header template packet nil)
    (setf (aref packet 3) (logior #x20 continuity-counter)
          (aref packet 4) 183)
    (cond
      ((plusp original-length)
       (replace packet template
                :start1 5
                :start2 5
                :end2 (+ 5 original-length))
       (setf (aref packet 5)
             (logandc2 (aref packet 5)
                       clear-adaptation-flags)))
      (t
       (setf (aref packet 5) 0)))
    packet))

(defun make-extra-payload-packet
    (template payload continuity-counter)
  "TEMPLATEのPIDとtransport priorityを使って追加payload packetを作る。"
  (make-ts-packet
   (ts-pid template)
   continuity-counter
   payload
   :transport-priority
   (ts-transport-priority-p template)))
