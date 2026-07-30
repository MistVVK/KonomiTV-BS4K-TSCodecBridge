;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defconstant +opus-clock-rate+ 48000)
(defconstant +opus-maximum-packet-samples+ 5760)
(defconstant +opus-maximum-frame-byte-count+ 1275)

(defun validate-opus-frame-byte-count (length)
  "Opus 1 frameのbyte長をRFC 6716上限内で検証する。"
  (unless (<= 0 length +opus-maximum-frame-byte-count+)
    (bridge-error "Opus frame byte length is invalid: ~D" length))
  length)

(defun decode-opus-frame-size (packet offset end)
  "PACKETのOFFSETにあるOpus frame sizeを読み、sizeと次offsetを返す。"
  (when (>= offset end)
    (bridge-error "Opus frame size is truncated"))
  (let ((first (aref packet offset)))
    (cond
      ((< first 252)
       (values first (+ offset 1)))
      (t
       (when (>= (+ offset 1) end)
         (bridge-error "Opus two-byte frame size is truncated"))
       (values (+ first
                  (* 4 (aref packet (+ offset 1))))
               (+ offset 2))))))

(defun opus-frame-duration-samples (toc)
  "Opus TOC byteから1 frameの48kHz sample数を返す。"
  (let ((configuration (ash toc -3)))
    (cond
      ((< configuration 12)
       (case (mod configuration 4)
         (0 480)
         (1 960)
         (2 1920)
         (otherwise 2880)))
      ((< configuration 16)
       (if (evenp configuration) 480 960))
      (t
       (case (mod configuration 4)
         (0 120)
         (1 240)
         (2 480)
         (otherwise 960))))))

(defun validate-opus-cbr-payload (length frame-count)
  "CBR Opus packetのframe payload境界を検証する。"
  (unless (zerop (mod length frame-count))
    (bridge-error
     "Opus CBR payload is not divisible by its frame count: bytes=~D frames=~D"
     length frame-count))
  (validate-opus-frame-byte-count (/ length frame-count)))

(defun validate-opus-vbr-payload (packet offset end frame-count)
  "VBR Opus packetのframe size列とpayload境界を検証する。"
  (let ((position offset)
        (declared-total 0))
    (loop repeat (- frame-count 1)
          do (multiple-value-bind (frame-size next)
                 (decode-opus-frame-size packet position end)
               (validate-opus-frame-byte-count frame-size)
               (incf declared-total frame-size)
               (setf position next)))
    (let ((payload-length (- end position)))
      (when (> declared-total payload-length)
        (bridge-error
         "Opus VBR frame sizes exceed packet payload: declared=~D available=~D"
         declared-total payload-length))
      (validate-opus-frame-byte-count
       (- payload-length declared-total)))))

(defun parse-opus-code-three (packet end)
  "code 3のOpus PACKETを検証しframe数を返す。"
  (when (< end 2)
    (bridge-error "Opus code 3 packet is truncated"))
  (let* ((frame-control (aref packet 1))
         (frame-count (logand frame-control #x3f))
         (vbr-p (logbitp 7 frame-control))
         (padding-p (logbitp 6 frame-control))
         (position 2)
         (padding 0))
    (unless (<= 1 frame-count 48)
      (bridge-error "Opus packet frame count is invalid: ~D" frame-count))
    (when padding-p
      (loop
        (when (>= position end)
          (bridge-error "Opus packet padding length is truncated"))
        (let ((value (aref packet position)))
          (incf position)
          (cond
            ((= value 255)
             (incf padding 254))
            (t
             (incf padding value)
             (return))))))
    (when (> (+ position padding) end)
      (bridge-error "Opus packet padding exceeds packet boundary"))
    (let ((payload-end (- end padding)))
      (if vbr-p
          (validate-opus-vbr-payload
           packet position payload-end frame-count)
          (validate-opus-cbr-payload
           (- payload-end position) frame-count)))
    frame-count))

(defun validate-opus-packet (packet)
  "1個のraw Opus PACKETを検証し48kHz sample数を返す。"
  (when (zerop (length packet))
    (bridge-error "Opus packet is empty"))
  (let* ((toc (aref packet 0))
         (code (logand toc 3))
         (frame-count
           (case code
             (0
              (validate-opus-frame-byte-count
               (- (length packet) 1))
              1)
             (1
              (validate-opus-cbr-payload
               (- (length packet) 1) 2)
              2)
             (2
              (multiple-value-bind (first-size payload-start)
                  (decode-opus-frame-size packet 1 (length packet))
                (validate-opus-frame-byte-count first-size)
                (let ((payload-length (- (length packet)
                                         payload-start)))
                  (when (> first-size payload-length)
                    (bridge-error
                     "Opus code 2 frame sizes exceed packet payload"))
                  (validate-opus-frame-byte-count
                   (- payload-length first-size)))
                2))
             (otherwise
              (parse-opus-code-three packet (length packet)))))
         (samples (* frame-count
                     (opus-frame-duration-samples toc))))
    (when (> samples +opus-maximum-packet-samples+)
      (bridge-error "Opus packet duration exceeds 120 ms: ~D samples"
                    samples))
    samples))

(defun decode-opus-control-packet-length (payload offset)
  "FFmpeg Opus control headerのlacingを読み、packet長と次offsetを返す。"
  (let ((position offset)
        (packet-length 0))
    (loop
      (when (>= position (length payload))
        (bridge-error "Opus control lacing is truncated"))
      (let ((value (aref payload position)))
        (incf position)
        (incf packet-length value)
        (unless (= value 255)
          (return))))
    (when (zerop packet-length)
      (bridge-error "Opus control header declares an empty packet"))
    (values packet-length position)))

(defun parse-ffmpeg-opus-control-payload (payload)
  "FFmpeg MPEG-TS Opus control payloadを検証し総sample数を返す。"
  (let ((position 0)
        (total-samples 0)
        (packet-count 0))
    (loop while (< position (length payload))
          do (when (> (+ position 2) (length payload))
               (bridge-error "Opus control header is truncated"))
             (unless (= (aref payload position) #x7f)
               (bridge-error
                "Opus control sync byte is invalid at offset ~D"
                position))
             (let ((flags (aref payload (+ position 1))))
               (unless (= (logand flags #xe7) #xe0)
                 (bridge-error
                  "Opus control flags are invalid at offset ~D: 0x~2,'0X"
                  position flags))
               (incf position 2)
               (multiple-value-bind (packet-length after-lacing)
                   (decode-opus-control-packet-length payload position)
                 (setf position after-lacing)
                 (let ((trim-start nil)
                       (trim-end nil))
                   (when (logbitp 4 flags)
                     (when (> (+ position 2) (length payload))
                       (bridge-error "Opus trim_start is truncated"))
                     (setf trim-start (read-u16-be payload position))
                     (incf position 2))
                   (when (logbitp 3 flags)
                     (when (> (+ position 2) (length payload))
                       (bridge-error "Opus trim_end is truncated"))
                     (setf trim-end (read-u16-be payload position))
                     (incf position 2))
                   (when (> (+ position packet-length)
                            (length payload))
                     (bridge-error
                      "Opus packet exceeds PES payload at offset ~D"
                      position))
                   (let* ((packet
                            (subseq payload
                                    position
                                    (+ position packet-length)))
                          (samples (validate-opus-packet packet)))
                     (when (and trim-start (> trim-start samples))
                       (bridge-error
                        "Opus trim_start exceeds packet duration: ~D"
                        trim-start))
                     (when (and trim-end (> trim-end samples))
                       (bridge-error
                        "Opus trim_end exceeds packet duration: ~D"
                        trim-end))
                     (when (> (+ (or trim-start 0)
                                 (or trim-end 0))
                              samples)
                       (bridge-error
                        "Opus aggregate trim exceeds packet duration"))
                     (incf total-samples samples)
                     (incf packet-count)
                     (incf position packet-length))))))
    (when (zerop packet-count)
      (bridge-error "Opus PES contains no control packet"))
    total-samples))

(defun validate-ffmpeg-opus-pes (pes)
  "FFmpeg private PESのOpus payloadを検証し総sample数を返す。"
  (let ((header (parse-pes-header pes)))
    (unless (= (pes-header-stream-id header) #xbd)
      (bridge-error "Opus PES stream id is invalid: 0x~2,'0X"
                    (pes-header-stream-id header)))
    (unless (pes-header-pts header)
      (bridge-error "Opus PES does not carry a PTS"))
    (parse-ffmpeg-opus-control-payload
     (subseq pes (pes-header-payload-offset header)))))
