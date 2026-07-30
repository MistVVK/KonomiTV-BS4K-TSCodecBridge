;;;; SPDX-License-Identifier: 0BSD

(require :asdf)

(defvar *tscodecbridge-sblint-analysis* nil)

(unless *tscodecbridge-sblint-analysis*
  (let* ((project-root
         (uiop:ensure-directory-pathname
          (uiop:getenv-absolute-directory "PROJECT_ROOT")))
       (system-file
         (merge-pathnames
          "konomitv-bs4k-tscodecbridge.asd"
          project-root))
       (sample-text
         (or (uiop:getenv "BENCHMARK_SAMPLES") "100"))
       (sample-count
         (parse-integer sample-text :junk-allowed nil)))
  (asdf:load-asd system-file)
  (asdf:load-system "konomitv-bs4k-tscodecbridge/tests")
  (let ((passed t))
    (dolist (benchmark
             '((:vp9 :run-large-pes-benchmark)
               (:av1 :run-large-av1-pes-benchmark)))
      (let ((result
              (uiop:symbol-call
               :konomitv-bs4k-tscodecbridge
               (second benchmark)
               sample-count)))
        (format t "~A large zero-length PES benchmark:~%"
                (first benchmark))
        (loop for (name value) on result by #'cddr
              do (format t "  ~A: ~A~%" name value))
        (unless (getf result :p99-under-2ms-p)
          (setf passed nil))))
      (unless passed
        (format *error-output*
                "One or more large PES performance gates failed.~%")
        (uiop:quit 1)))))
