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
          project-root)))
  (asdf:load-asd system-file)
  (asdf:load-system "konomitv-bs4k-tscodecbridge/tests")
  (uiop:symbol-call
   :konomitv-bs4k-tscodecbridge
   :generate-public-fixtures
   project-root)
    (format *error-output* "Public fixtures generated.~%")))
