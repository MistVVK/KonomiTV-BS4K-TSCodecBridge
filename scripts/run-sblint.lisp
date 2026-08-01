;;;; SPDX-License-Identifier: 0BSD

(in-package #:cl-user)

(require :asdf)

(defvar *tscodecbridge-sblint-analysis* nil)

(define-condition sblint-dependency-order-error (error)
  ()
  (:report
   "SBLint dependency order does not cover all Lisp sources"))

(defun collect-lisp-files (directory)
  "DIRECTORY以下の全Lisp sourceをnamestring順で返す。"
  (sort
   (append
    (uiop:directory-files directory "*.lisp")
    (mapcan #'collect-lisp-files
            (uiop:subdirectories directory)))
   #'string<
   :key #'namestring))

(defun ordered-project-lisp-files (project-root)
  "ASDFと同じ依存順で全project Lisp sourceを返す。"
  (mapcar
   (lambda (relative-path)
     (merge-pathnames relative-path project-root))
   '("src/package.lisp"
     "src/conditions.lisp"
     "src/octets.lisp"
     "src/bit-reader.lisp"
     "src/crc32-mpeg2.lisp"
     "src/ts/packet.lisp"
     "src/ts/integrity.lisp"
     "src/ts/section.lisp"
     "src/ts/pat.lisp"
     "src/ts/pmt.lisp"
     "src/ts/pes.lisp"
     "src/ts/repacketize.lisp"
     "src/codecs/av1.lisp"
     "src/codecs/av1-frame-structure.lisp"
     "src/codecs/vp9.lisp"
     "src/codecs/opus.lisp"
     "src/mapping/dsl.lisp"
     "src/mapping/av1.lisp"
     "src/mapping/vp9-private-v1.lisp"
     "src/mapping/opus.lisp"
     "src/tstd/arrival.lisp"
     "src/tstd/classify.lisp"
     "src/tstd/model.lisp"
     "src/processor.lisp"
     "src/stream-anchor.lisp"
     "src/inspector.lisp"
     "src/cli.lisp"
     "src/stream.lisp"
     "src/main.lisp"
     "tests/package.lisp"
     "tests/harness.lisp"
     "tests/av1-frame-structure-test.lisp"
     "tests/primitives-test.lisp"
     "tests/transport-test.lisp"
     "tests/mapping-test.lisp"
     "tests/vp9-structure-test.lisp"
     "tests/vp9-ffmpeg-state-integration.lisp"
     "tests/processor-test.lisp"
     "tests/stream-anchor-test.lisp"
     "tests/tstd-test.lisp"
     "tests/audit-test.lisp"
     "tests/fixture-generator.lisp"
     "tests/fixture-test.lisp"
     "tests/performance.lisp"
     "tests/soak.lisp"
     "tests/runner.lisp")))

(defun validate-ordered-project-files (project-root ordered-files)
  "ORDERED-FILESがsrc/testsのLisp sourceを過不足なく含むか検証する。"
  (let ((discovered
          (append
           (collect-lisp-files
            (merge-pathnames "src/" project-root))
           (collect-lisp-files
            (merge-pathnames "tests/" project-root)))))
    (unless (and (= (length ordered-files)
                    (length discovered))
                 (null (set-difference ordered-files
                                       discovered
                                       :test #'equal))
                 (null (set-difference discovered
                                       ordered-files
                                       :test #'equal)))
      (error 'sblint-dependency-order-error)))
  ordered-files)

(defun project-script-files (project-root)
  "scripts以下の全Lisp sourceを自己検査対象として返す。"
  (collect-lisp-files
   (merge-pathnames "scripts/" project-root)))

(unless *tscodecbridge-sblint-analysis*
  (setf *tscodecbridge-sblint-analysis* t)
  (let* ((project-root
         (uiop:ensure-directory-pathname
          (uiop:getenv-absolute-directory "PROJECT_ROOT")))
       (sblint-root
         (uiop:ensure-directory-pathname
          (uiop:getenv-absolute-directory "SBLINT_SOURCE_DIR")))
       (sblint-system-file
         (merge-pathnames "sblint.asd" sblint-root))
       (project-system-file
         (merge-pathnames "konomitv-bs4k-tscodecbridge.asd"
                          project-root))
       (ordered-files
         (validate-ordered-project-files
          project-root
          (ordered-project-lisp-files project-root)))
       (targets
         (append
          (list project-system-file)
          ordered-files
          (project-script-files project-root))))
  (asdf:load-asd sblint-system-file)
  ;; 外部 lint tool の load warning は対象sourceのfindingではないため、
  ;; toolのversionに依存せず、この区間だけ隔離する。
  (handler-bind ((warning #'muffle-warning))
    (asdf:load-system "swank")
    (asdf:load-system "sblint"))
  (format *error-output* "SBLint toolchain loaded.~%")
  (let ((violation-count 0))
    (dolist (target targets)
      (format *error-output* "SBLint target: ~A~%" target)
      (incf violation-count
            (uiop:symbol-call
             "SBLINT"
             "RUN-LINT-FILE"
             target)))
    (format *error-output*
            "SBLint reported ~D finding(s).~%"
            violation-count)
    (cond
      ((zerop violation-count)
       (format *error-output* "SBLint passed with zero findings.~%"))
      (t
       (format *error-output*
               "SBLint failed with ~D finding(s).~%"
               violation-count)
         (finish-output *error-output*)
         (uiop:quit 1))))))
