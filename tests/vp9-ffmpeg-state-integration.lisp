;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defun read-vp9-ivf-octets (pathname)
  "PATHNAMEのIVFをoctet vectorとして完全に読む。"
  (with-open-file
      (stream pathname
              :direction :input
              :element-type '(unsigned-byte 8))
    (let ((result
            (make-array (file-length stream)
                        :element-type '(unsigned-byte 8))))
      (unless (= (read-sequence result stream)
                 (length result))
        (bridge-error "VP9 IVF is truncated: ~A" pathname))
      result)))

(defun vp9-ivf-little-endian-integer
    (octets offset octet-count)
  "OCTETSのOFFSETからOCTET-COUNT byteをlittle endian整数として読む。"
  (when (> (+ offset octet-count)
           (length octets))
    (bridge-error "VP9 IVF integer exceeds the input boundary"))
  (loop for index below octet-count
        sum
        (ash (aref octets (+ offset index))
             (* index 8))))

(defun vp9-ivf-signature-p
    (octets offset expected)
  "OCTETSのOFFSETにASCII EXPECTEDがあるかを返す。"
  (and
   (<= (+ offset (length expected))
       (length octets))
   (loop for character across expected
         for index from offset
         always
         (= (aref octets index)
            (char-code character)))))

(defun vp9-parse-without-state-fails-p (access-unit)
  "参照を必要とするACCESS-UNITがstate無しでfail closedになるかを返す。"
  (handler-case
      (progn
        (parse-vp9-access-unit access-unit)
        nil)
    (bridge-error () t)))

(defun run-vp9-ffmpeg-state-integration
    (pathname)
  "固定FFmpeg/libvpx-vp9のIVFをAU順にstate継承して完全検証する。"
  (let ((octets (read-vp9-ivf-octets pathname))
        (minimum-header-size 32))
    (unless (>= (length octets) minimum-header-size)
      (bridge-error "VP9 IVF header is truncated"))
    (unless (vp9-ivf-signature-p octets 0 "DKIF")
      (bridge-error "VP9 IVF signature is invalid"))
    (unless (zerop
             (vp9-ivf-little-endian-integer octets 4 2))
      (bridge-error "VP9 IVF version is unsupported"))
    (let* ((header-size
             (vp9-ivf-little-endian-integer octets 6 2))
           (width
             (vp9-ivf-little-endian-integer octets 12 2))
           (height
             (vp9-ivf-little-endian-integer octets 14 2))
           (declared-frame-count
             (vp9-ivf-little-endian-integer octets 24 4))
           (offset header-size)
           (frame-count 0)
           (state nil)
           (saw-inter-frame-p nil))
      (unless (= header-size minimum-header-size)
        (bridge-error "VP9 IVF header size is unsupported: ~D"
                      header-size))
      (unless (vp9-ivf-signature-p octets 8 "VP90")
        (bridge-error "IVF does not contain VP9"))
      (unless (and (plusp width) (plusp height))
        (bridge-error "VP9 IVF dimensions are invalid"))
      (loop while (< offset (length octets))
            do
        (when (> (+ offset 12)
                 (length octets))
          (bridge-error "VP9 IVF frame header is truncated"))
        (let* ((frame-size
                 (vp9-ivf-little-endian-integer
                  octets offset 4))
               (frame-start (+ offset 12))
               (frame-end (+ frame-start frame-size)))
          (unless (plusp frame-size)
            (bridge-error "VP9 IVF contains an empty access unit"))
          (when (> frame-end (length octets))
            (bridge-error "VP9 IVF access unit is truncated"))
          (let ((access-unit
                  (subseq octets frame-start frame-end)))
            (when (= frame-count 1)
              (unless
                  (vp9-parse-without-state-fails-p
                   access-unit)
                (bridge-error
                 "Second libvpx-vp9 AU unexpectedly passed without state")))
            (multiple-value-bind
                  (configuration ranges next-state structures)
                (if state
                    (parse-vp9-access-unit
                     access-unit :state state)
                    (parse-vp9-access-unit access-unit))
              (unless (and configuration
                           (consp ranges)
                           next-state
                           (consp structures))
                (bridge-error
                 "VP9 AU parser did not return the full state contract"))
              (when (zerop frame-count)
                (unless
                    (vp9-frame-configuration-key-frame-p
                     configuration)
                  (bridge-error
                   "First libvpx-vp9 AU is not a key frame"))
                (unless
                    (and
                     (= (vp9-frame-configuration-width
                         configuration)
                        width)
                     (= (vp9-frame-configuration-height
                         configuration)
                        height))
                  (bridge-error
                   "VP9 IVF and key-frame dimensions disagree")))
              (when (= frame-count 1)
                (when
                    (or
                     (vp9-frame-configuration-key-frame-p
                      configuration)
                     (vp9-frame-configuration-show-existing-frame-p
                      configuration)
                     (vp9-frame-configuration-intra-only-p
                      configuration))
                  (bridge-error
                   "Second libvpx-vp9 AU is not an inter frame")))
              (when
                  (and
                   (not
                    (vp9-frame-configuration-key-frame-p
                     configuration))
                   (not
                    (vp9-frame-configuration-show-existing-frame-p
                     configuration))
                   (not
                    (vp9-frame-configuration-intra-only-p
                     configuration)))
                (setf saw-inter-frame-p t))
              (when (and state (eq state next-state))
                (bridge-error
                 "VP9 parser reused the mutable input state"))
              (setf state next-state)))
          (setf offset frame-end)
          (incf frame-count)))
      (unless (= offset (length octets))
        (bridge-error
         "VP9 IVF trailing boundary is inconsistent"))
      (unless (>= frame-count 2)
        (bridge-error
         "VP9 IVF must contain at least two access units"))
      (unless saw-inter-frame-p
        (bridge-error
         "VP9 IVF does not contain an inter frame"))
      (unless (= declared-frame-count frame-count)
        (bridge-error
         "VP9 IVF frame count differs: header=~D parsed=~D"
         declared-frame-count frame-count))
      (format
       t
       "VP9 FFmpeg/libvpx state integration passed: ~D AU, ~Dx~D.~%"
       frame-count width height))))
