;;;; SPDX-License-Identifier: 0BSD

(require :asdf)

(defvar *tscodecbridge-sblint-analysis* nil)

(defun required-positive-number (name default)
  "環境変数NAMEを正のnumberとして読む。"
  (let* ((text (or (uiop:getenv name) default))
         (value
           (read-from-string text nil nil)))
    (unless (and (realp value) (plusp value))
      (uiop:die "~A must be a positive number: ~A"
                name text))
    value))

(defun required-non-negative-integer (name default)
  "環境変数NAMEを非負integerとして読む。"
  (let* ((text (or (uiop:getenv name) default))
         (value
           (parse-integer text :junk-allowed nil)))
    (unless (>= value 0)
      (uiop:die "~A must be non-negative: ~A"
                name text))
    value))

(defun required-positive-integer (name default)
  "環境変数NAMEを正のintegerとして読む。"
  (let ((value
          (required-non-negative-integer name default)))
    (unless (plusp value)
      (uiop:die "~A must be positive: ~A"
                name value))
    value))

(defun soak-mode ()
  "SOAK_MODEをkeywordへ変換する。"
  (let ((text (string-downcase
               (or (uiop:getenv "SOAK_MODE") "all"))))
    (cond
      ((string= text "pass-through") :pass-through)
      ((string= text "vp9") :vp9)
      ((string= text "av1") :av1)
      ((string= text "all") :all)
      (t (uiop:die "Unsupported SOAK_MODE: ~A" text)))))

(defun print-soak-result (result)
  "RESULT plistを1行1項目で表示する。"
  (format t "Soak result:~%")
  (loop for (name value) on result by #'cddr
        do (format t "  ~A: ~A~%" name value)))

(unless *tscodecbridge-sblint-analysis*
  (let* ((project-root
         (uiop:ensure-directory-pathname
          (uiop:getenv-absolute-directory "PROJECT_ROOT")))
       (system-file
         (merge-pathnames
          "konomitv-bs4k-tscodecbridge.asd"
          project-root))
       (duration
         (required-positive-number
          "SOAK_DURATION_SECONDS" "300"))
       (rss-interval
         (required-positive-number
          "SOAK_RSS_SAMPLE_SECONDS" "10"))
       (rss-growth-limit
         (required-non-negative-integer
          "SOAK_RSS_GROWTH_LIMIT_KIB" "16384"))
       (rss-slope-limit
         (required-non-negative-integer
          "SOAK_RSS_SLOPE_LIMIT_KIB_PER_HOUR" "1024"))
       (consing-reads
         (required-positive-integer
          "SOAK_CONSING_READS" "10000"))
       (latency-repetitions
         (required-positive-integer
          "SOAK_LATENCY_REPETITIONS" "3"))
       (latency-reads
         (required-positive-integer
          "SOAK_LATENCY_READS" "10000")))
  (asdf:load-asd system-file)
  (asdf:load-system "konomitv-bs4k-tscodecbridge/tests")
  (let ((results
          (uiop:symbol-call
           :konomitv-bs4k-tscodecbridge
           :run-soak-suite
           (soak-mode)
           duration
           :rss-sample-seconds rss-interval
           :rss-growth-limit-kib rss-growth-limit
           :rss-slope-limit-kib-per-hour rss-slope-limit
           :consing-read-count consing-reads
           :latency-repetitions latency-repetitions
           :latency-read-count latency-reads)))
    (dolist (result results)
      (print-soak-result result))
      (unless (every
               (lambda (result)
                 (getf result :passed))
               results)
        (uiop:quit 1)))))
