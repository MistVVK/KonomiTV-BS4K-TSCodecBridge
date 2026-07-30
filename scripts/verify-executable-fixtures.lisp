;;;; SPDX-License-Identifier: 0BSD

(require :asdf)

(defvar *tscodecbridge-sblint-analysis* nil)

(defun read-binary-file (pathname)
  "PATHNAMEを単純octet vectorとして読む。"
  (with-open-file
      (stream pathname
              :direction :input
              :element-type '(unsigned-byte 8))
    (let ((result
            (make-array (file-length stream)
                        :element-type '(unsigned-byte 8))))
      (unless (= (read-sequence result stream)
                 (length result))
        (uiop:die "Binary file is truncated: ~A" pathname))
      result)))

(unless *tscodecbridge-sblint-analysis*
  (let* ((project-root
         (uiop:ensure-directory-pathname
          (uiop:getenv-absolute-directory "PROJECT_ROOT")))
       (system-file
         (merge-pathnames
          "konomitv-bs4k-tscodecbridge.asd"
          project-root))
       (vp9-actual
         (pathname
          (uiop:getenv "VP9_EXECUTABLE_OUTPUT")))
       (av1-actual
         (pathname
          (uiop:getenv "AV1_EXECUTABLE_OUTPUT"))))
  (asdf:load-asd system-file)
  (asdf:load-system "konomitv-bs4k-tscodecbridge/tests")
  (dolist (test-case
           `(("valid-vp9-opus.ts" :vp9 :opus ,vp9-actual)
             ("valid-av1-aac.ts" :av1 :aac ,av1-actual)))
    (destructuring-bind
        (fixture video-codec audio-codec actual-path)
        test-case
      (let* ((input
               (uiop:symbol-call
                :konomitv-bs4k-tscodecbridge
                :read-fixture-octets fixture))
             (expected
               (uiop:symbol-call
                :konomitv-bs4k-tscodecbridge
                :run-octet-processor
                input video-codec audio-codec))
             (actual (read-binary-file actual-path)))
        (unless (equalp expected actual)
          (uiop:die
           "Saved executable output differs from source semantics: ~A"
           fixture))
          (uiop:symbol-call
           :konomitv-bs4k-tscodecbridge
           :inspect-ts-octets actual)))))

  (format t "Saved executable semantic fixture outputs are valid.~%"))
