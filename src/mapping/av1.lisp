;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defparameter +av1-registration-identifier+
  (make-array 4
              :element-type 'octet
              :initial-contents '(#x41 #x56 #x30 #x31)))

(defstruct av1-codec-configuration
  (profile 0 :type (unsigned-byte 3))
  (level 0 :type (unsigned-byte 5))
  (tier 0 :type bit)
  (high-bitdepth 0 :type bit)
  (twelve-bit 0 :type bit)
  (monochrome 0 :type bit)
  (chroma-subsampling-x 1 :type bit)
  (chroma-subsampling-y 1 :type bit)
  (chroma-sample-position 0 :type (unsigned-byte 2))
  (hdr-wcg-idc 3 :type (unsigned-byte 2))
  (low-delay-mode-p nil :type boolean)
  (initial-presentation-delay nil
                              :type (or null (integer 0 15))))

(defun ensure-av1-input-subset-configuration (configuration)
  "CONFIGURATIONがBridgeの固定profile 0・8/10bit 4:2:0 subsetか検証する。"
  (unless
      (and
       (zerop (av1-codec-configuration-profile configuration))
       (zerop (av1-codec-configuration-twelve-bit configuration))
       (zerop (av1-codec-configuration-monochrome configuration))
       (= (av1-codec-configuration-chroma-subsampling-x configuration) 1)
       (= (av1-codec-configuration-chroma-subsampling-y configuration) 1))
    (bridge-error
     "AV1_INPUT_SUBSET_CONFIGURATION_UNSUPPORTED profile=~D high_bitdepth=~D twelve_bit=~D monochrome=~D subsampling_x=~D subsampling_y=~D"
     (av1-codec-configuration-profile configuration)
     (av1-codec-configuration-high-bitdepth configuration)
     (av1-codec-configuration-twelve-bit configuration)
     (av1-codec-configuration-monochrome configuration)
     (av1-codec-configuration-chroma-subsampling-x configuration)
     (av1-codec-configuration-chroma-subsampling-y configuration)))
  configuration)

(defun make-av1-video-descriptor (configuration)
  "CONFIGURATIONからAOM draft version 1 descriptorを作る。"
  (ensure-av1-input-subset-configuration configuration)
  (let ((delay
          (av1-codec-configuration-initial-presentation-delay
           configuration))
        (payload
          (make-array 4 :element-type 'octet :initial-element 0)))
    (setf (aref payload 0) #x81
          (aref payload 1)
          (logior
           (ash (av1-codec-configuration-profile configuration) 5)
           (av1-codec-configuration-level configuration))
          (aref payload 2)
          (logior
           (ash (av1-codec-configuration-tier configuration) 7)
           (ash (av1-codec-configuration-high-bitdepth configuration) 6)
           (ash (av1-codec-configuration-twelve-bit configuration) 5)
           (ash (av1-codec-configuration-monochrome configuration) 4)
           (ash
            (av1-codec-configuration-chroma-subsampling-x configuration)
            3)
           (ash
            (av1-codec-configuration-chroma-subsampling-y configuration)
            2)
           (av1-codec-configuration-chroma-sample-position
            configuration))
          (aref payload 3)
          (logior
           (ash (av1-codec-configuration-hdr-wcg-idc configuration) 6)
           (if delay #x10 0)
           (or delay 0)))
    (make-descriptor :tag #x80 :payload payload)))

(defun make-av1-mapping-descriptors (configuration)
  "CONFIGURATIONからAV01 registrationとAV1 video descriptorを作る。"
  (list
   (make-descriptor
    :tag #x05
    :payload (copy-seq +av1-registration-identifier+))
   (make-av1-video-descriptor configuration)))

(defun validate-av1-mapping-descriptors (descriptors)
  "AV1 descriptorの順序、marker、version、reserved bitを検証する。"
  (when (< (length descriptors) 2)
    (bridge-error "AV1 mapping descriptors are missing"))
  (let ((registration (first descriptors))
        (video (second descriptors)))
    (unless (descriptor-identifier-p
             registration
             +av1-registration-identifier+)
      (bridge-error "AV1 registration descriptor is invalid"))
    (unless (and (= (descriptor-tag video) #x80)
                 (= (length (descriptor-payload video)) 4))
      (bridge-error "AV1 video descriptor is invalid"))
    (let ((payload (descriptor-payload video)))
      (unless (= (aref payload 0) #x81)
        (bridge-error "AV1 video descriptor marker or version is invalid"))
      (when (logbitp 5 (aref payload 3))
        (bridge-error "AV1 video descriptor reserved bit is not zero"))
      (when (and (not (logbitp 4 (aref payload 3)))
                 (plusp (logand (aref payload 3) #x0f)))
        (bridge-error
         "AV1 video descriptor reserved delay bits are not zero")))
    t))
