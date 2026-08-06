;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(defun %open-binary-input ()
  "標準入力fdをoctet streamとして開く。"
  (sb-sys:make-fd-stream 0
                         :input t
                         :element-type 'octet
                         :buffering :none
                         :auto-close nil))

(defun %open-binary-output ()
  "標準出力fdを低遅延のbuffered octet streamとして開く。"
  (sb-sys:make-fd-stream 1
                         :output t
                         :element-type 'octet
                         :buffering :full
                         :auto-close nil))

(defun %run-pass-through ()
  "標準入力の全PIDを厳格検証してbyte-exactに転送する。"
  (let ((input (%open-binary-input))
        (output (%open-binary-output)))
    (validate-and-copy-ts-stream input output)))

(defun %run-processor (options)
  "OPTIONSに従って標準入出力のTSを処理する。"
  (let ((input (%open-binary-input))
        (output (%open-binary-output)))
    (if (advanced-codec-selection-p options)
        (process-ts-stream
         input output
         (bridge-options-video-codec options)
         (bridge-options-audio-codec options)
         :program-number
         (bridge-options-program-number options)
         :transport-rate-kbps
         (bridge-options-transport-rate-kbps options)
         :stream-anchor-v1-p
         (bridge-options-stream-anchor-v1-p options)
         :stream-anchor-maximum-distance-ticks
         (bridge-options-stream-anchor-maximum-distance-ticks
          options))
        (validate-and-copy-ts-stream input output))))

(defun main (&optional (arguments (rest sb-ext:*posix-argv*)))
  "ARGUMENTSでCLIを実行し、process終了codeを返す。"
  (handler-case
      (let ((options (parse-command-line arguments)))
        (case (bridge-options-action options)
          (:process
           (%run-processor options)
           0)
          (:pass-through
           (%run-pass-through)
           0)
          (:help
           (%print-help *standard-output*)
           0)
          (:version
           (format *standard-output* "~A~%" *cli-version*)
           0)
          (:mapping-version
           (format *standard-output* "~D~%"
                   +vp9-mapping-version+)
           0)
          (otherwise
           (bridge-error "Internal command dispatch failure."))))
    (bridge-error (condition)
      (format *error-output*
              "ts-codecbridge: error: ~A~%"
              condition)
      1)
    (serious-condition (condition)
      (format *error-output*
              "ts-codecbridge: fatal: ~A~%"
              condition)
      1)))

(defun entry-point ()
  "保存実行形式のentry point。"
  (let ((exit-code (main)))
    (finish-output *standard-output*)
    (finish-output *error-output*)
    (sb-ext:exit :code exit-code)))
