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
    (asdf:load-system "konomitv-bs4k-tscodecbridge" :force t))
    (format *error-output* "SBCL clean compile passed.~%")))
