;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defparameter *cli-version* "0.1.0"
  "公開CLIのversion。")

(defstruct bridge-options
  (action :process
          :type (member :process :pass-through :help :version
                        :mapping-version))
  (video-codec :passthrough
               :type (member :passthrough :vp9 :av1))
  (audio-codec :aac
               :type (member :aac :opus))
  (program-number nil :type (or null (integer 1 65535)))
  (transport-rate-kbps nil :type (or null (integer 1 *))))

(defun parse-video-codec (value)
  "VALUEを映像codec keywordへ変換する。"
  (cond
    ((string= value "passthrough") :passthrough)
    ((string= value "vp9") :vp9)
    ((string= value "av1") :av1)
    (t
     (bridge-error "Unsupported video codec: ~A" value))))

(defun parse-audio-codec (value)
  "VALUEを音声codec keywordへ変換する。"
  (cond
    ((string= value "aac") :aac)
    ((string= value "opus") :opus)
    (t
     (bridge-error "Unsupported audio codec: ~A" value))))

(defun parse-program-number (value)
  "VALUEを非0の16 bit program numberへ変換する。"
  (handler-case
      (multiple-value-bind (number end)
          (parse-integer value :junk-allowed t)
        (unless (and number
                     (= end (length value))
                     (<= 1 number 65535))
          (bridge-error "Invalid program number: ~A" value))
        number)
    (parse-error ()
      (bridge-error "Invalid program number: ~A" value))))

(defun parse-transport-rate-kbps (value)
  "VALUEを正整数のCBR transport rate kbit/sへ変換する。"
  (handler-case
      (multiple-value-bind (number end)
          (parse-integer value :junk-allowed t)
        (unless (and number
                     (= end (length value))
                     (plusp number))
          (bridge-error
           "Invalid transport rate in kbit/s: ~A"
           value))
        number)
    (parse-error ()
      (bridge-error
       "Invalid transport rate in kbit/s: ~A"
       value))))

(defun parse-command-line (arguments)
  "ARGUMENTSを検証し、BRIDGE-OPTIONSを返す。"
  (let ((mapping-version-count
          (count "--mapping-version" arguments :test #'string=)))
    (when (> mapping-version-count 1)
      (bridge-error
       "--mapping-version may be specified only once"))
    (when (and (= mapping-version-count 1)
               (not (equal arguments '("--mapping-version"))))
      (bridge-error
       "--mapping-version cannot be combined with other arguments")))
  (cond
    ((equal arguments '("--pass-through"))
     (make-bridge-options :action :pass-through))
    ((equal arguments '("--help"))
     (make-bridge-options :action :help))
    ((equal arguments '("--version"))
     (make-bridge-options :action :version))
    ((equal arguments '("--mapping-version"))
     (make-bridge-options :action :mapping-version))
    (t
     (let ((remaining arguments)
           (video-codec :passthrough)
           (audio-codec :aac)
           (program-number nil)
           (transport-rate-kbps nil)
           (seen-video-p nil)
           (seen-audio-p nil)
           (seen-program-p nil)
           (seen-transport-rate-p nil))
       (loop while remaining
             for option = (pop remaining)
             do (cond
                  ((string= option "--video-codec")
                   (when seen-video-p
                     (bridge-error
                      "--video-codec may be specified only once"))
                   (unless remaining
                     (bridge-error "--video-codec requires a value"))
                   (setf video-codec
                         (parse-video-codec (pop remaining))
                         seen-video-p t))
                  ((string= option "--audio-codec")
                   (when seen-audio-p
                     (bridge-error
                      "--audio-codec may be specified only once"))
                   (unless remaining
                     (bridge-error "--audio-codec requires a value"))
                   (setf audio-codec
                         (parse-audio-codec (pop remaining))
                         seen-audio-p t))
                  ((string= option "--program-number")
                   (when seen-program-p
                     (bridge-error
                      "--program-number may be specified only once"))
                   (unless remaining
                     (bridge-error "--program-number requires a value"))
                   (setf program-number
                         (parse-program-number (pop remaining))
                         seen-program-p t))
                  ((string= option "--transport-rate-kbps")
                   (when seen-transport-rate-p
                     (bridge-error
                      "--transport-rate-kbps may be specified only once"))
                   (unless remaining
                     (bridge-error
                      "--transport-rate-kbps requires a value"))
                   (setf
                    transport-rate-kbps
                    (parse-transport-rate-kbps (pop remaining))
                    seen-transport-rate-p t))
                  (t
                   (bridge-error "Unknown command-line argument: ~A"
                                 option))))
       (cond
         ((eq video-codec :av1)
          (unless transport-rate-kbps
            (bridge-error
             "--transport-rate-kbps is required with --video-codec av1")))
         (transport-rate-kbps
          (bridge-error
           "--transport-rate-kbps is only valid with --video-codec av1")))
       (make-bridge-options
        :action :process
        :video-codec video-codec
        :audio-codec audio-codec
        :program-number program-number
        :transport-rate-kbps transport-rate-kbps)))))

(defun advanced-codec-selection-p (options)
  "OPTIONSがsemantic TS処理を必要とするかを返す。"
  (or (not (eq (bridge-options-video-codec options)
               :passthrough))
      (eq (bridge-options-audio-codec options) :opus)))

(defun %print-help (stream)
  "STREAMへCLIの英語helpを出力する。"
  (format stream
          "Usage: ts-codec-bridge.elf [OPTIONS]~%~
           ~%~
           Options:~%~
             --video-codec passthrough|vp9|av1  Video mapping (default: passthrough)~%~
             --audio-codec aac|opus             Audio mapping (default: aac)~%~
             --program-number 1..65535           Select one program in a multi-program TS~%~
             --transport-rate-kbps RATE           Required positive CBR kbit/s for AV1 only~%~
             AV1 input contract: FFmpeg 8.1.2/libaom realtime, base-layer OBU subset.~%~
             AV1 output targets the pinned 2026-03-25 AOM MPEG-2 TS draft;~%~
             input acceptance is the subset documented in spec/av1-mpeg2-ts-working-draft.md.~%~
             --pass-through                     Validated byte-exact TS copy~%~
             --help                             Show this help~%~
             --version                          Show the CLI version~%~
             --mapping-version                  Show the TS mapping version~%"))
