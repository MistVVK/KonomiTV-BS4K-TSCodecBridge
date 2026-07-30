;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defparameter +vp9-registration-identifier+
  (make-array 4
              :element-type 'octet
              :initial-contents '(#x56 #x50 #x30 #x39)))

(define-ts-codec-mapping :vp9
  (:registration "VP09")
  (:mapping-version 1)
  (:stream-type #x06)
  (:pes-stream-id #xe0)
  (:private-descriptor
   #x80
   (#x4b #x54 #x56 #x42 #x09 #x01 #xf0 #x00)))

(defun apply-vp9-mapping-to-stream (stream)
  "PMT STREAMをVP9 private mapping v1へ書き換える。"
  (setf (pmt-stream-stream-type stream) +vp9-stream-type+
        (pmt-stream-descriptors stream)
        (append (make-vp9-mapping-descriptors)
                (remove-if
                 (lambda (descriptor)
                   (member (descriptor-tag descriptor)
                           '(#x05 #x80)
                           :test #'=))
                 (pmt-stream-descriptors stream))))
  stream)

(defun validate-vp9-pes (pes key-frame-p &key state)
  "VP9 mapping v1のPES headerと全access unitを検証する。

戻り値は既存互換の先頭configurationと、全subframe成功後のnext state。"
  (let ((header (parse-pes-header pes)))
    (unless (= (pes-header-stream-id header) +vp9-pes-stream-id+)
      (bridge-error "VP9 PES stream id is invalid: 0x~2,'0X"
                    (pes-header-stream-id header)))
    (unless (pes-header-data-alignment-p header)
      (bridge-error "VP9 PES data_alignment_indicator is zero"))
    (unless (pes-header-pts header)
      (bridge-error "VP9 PES does not carry a PTS"))
    (let ((payload-offset (pes-header-payload-offset header)))
      (ensure-octet-range pes payload-offset
                          (- (length pes) payload-offset)
                          :validate-vp9-payload)
      (multiple-value-bind
            (configuration ranges next-state structures)
          (parse-vp9-access-unit
           (subseq pes payload-offset)
           :state state)
        (declare (ignore ranges structures))
        (unless (eq
                 (vp9-frame-configuration-key-frame-p configuration)
                 key-frame-p)
          (bridge-error "VP9 key-frame indication does not match payload"))
        (values configuration next-state)))))
