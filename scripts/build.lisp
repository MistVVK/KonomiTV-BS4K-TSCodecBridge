;;;; SPDX-License-Identifier: 0BSD

(in-package #:cl-user)

(require :asdf)

(defvar *tscodecbridge-sblint-analysis* nil)

(unless *tscodecbridge-sblint-analysis*
  (let* ((project-root
         (uiop:ensure-directory-pathname
          (pathname (uiop:getenv "PROJECT_ROOT"))))
       (output-path
         (pathname (uiop:getenv "OUTPUT_PATH")))
       (system-file
         (merge-pathnames "konomitv-bs4k-tscodecbridge.asd"
                          project-root)))
  (asdf:load-asd system-file)
  (handler-bind ((warning #'error))
    (asdf:load-system "konomitv-bs4k-tscodecbridge" :force t))
    (sb-ext:save-lisp-and-die
     output-path
     :toplevel
     (symbol-function
      (find-symbol "ENTRY-POINT" "KONOMITV-BS4K-TSCODECBRIDGE"))
     :executable t
     :save-runtime-options t
     :purify t)))
