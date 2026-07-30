;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defparameter +opus-registration-identifier+
  (make-array 4
              :element-type 'octet
              :initial-contents '(#x4f #x70 #x75 #x73)))

(defun valid-opus-channel-configuration-p (value)
  "DVB Opus descriptorのchannel configurationを検証する。"
  (or (<= 1 value 8)
      (<= #x82 value #x88)))

(defun validate-opus-descriptors (descriptors)
  "FFmpeg 8形式のOpus descriptor列を検証しchannel configを返す。"
  (when (< (length descriptors) 2)
    (bridge-error "Opus descriptors are missing"))
  (let ((registration (first descriptors))
        (extension (second descriptors)))
    (unless (descriptor-identifier-p
             registration
             +opus-registration-identifier+)
      (bridge-error "Opus registration descriptor is invalid"))
    (unless (and (= (descriptor-tag extension) #x7f)
                 (= (length (descriptor-payload extension)) 2)
                 (= (aref (descriptor-payload extension) 0) #x80))
      (bridge-error "Opus DVB extension descriptor is invalid"))
    (let ((channel-configuration
            (aref (descriptor-payload extension) 1)))
      (unless (valid-opus-channel-configuration-p channel-configuration)
        (bridge-error "Opus channel configuration is unsupported: 0x~2,'0X"
                      channel-configuration))
      channel-configuration)))
