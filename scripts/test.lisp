;;;; SPDX-License-Identifier: 0BSD

(in-package #:cl-user)

(require :asdf)

(defvar *tscodecbridge-sblint-analysis* nil)

(unless *tscodecbridge-sblint-analysis*
  (let* ((project-root
         (uiop:ensure-directory-pathname
          (uiop:getenv-absolute-directory "PROJECT_ROOT")))
       (system-file
         (merge-pathnames "konomitv-bs4k-tscodecbridge.asd"
                          project-root)))
  (asdf:load-asd system-file)
  (handler-bind ((warning #'error))
    (asdf:test-system "konomitv-bs4k-tscodecbridge/tests"))
    (format *error-output* "ASDF tests passed.~%")))
