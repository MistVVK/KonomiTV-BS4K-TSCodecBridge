;;;; SPDX-License-Identifier: 0BSD

(in-package #:cl-user)

(require :asdf)

(defvar *tscodecbridge-sblint-analysis* nil)
(defvar *fixed-cl-swank-load-active* nil)

(defparameter *fixed-cl-swank-summary-warnings*
  '(("COMMON-LISP::SIMPLE-WARNING"
     "undefined variable: SWANK:*COMMUNICATION-STYLE*")
    ("COMMON-LISP::SIMPLE-WARNING"
     "undefined variable: SWANK:*SWANK-DEBUGGER-CONDITION*")
    ("SB-INT::SIMPLE-STYLE-WARNING"
     "undefined function: ASDF/INTERFACE::MAKE-TEMPORARY-PACKAGE")
    ("SB-INT::SIMPLE-STYLE-WARNING"
     "undefined function: SWANK::SLIME-STREAM-P"))
  "固定cl-swank load後にSBCLが通知する既知summary。")

(defparameter *fixed-cl-swank-deprecated-functions*
  '("UIOP/BACKWARD-DRIVER:COERCE-PATHNAME"
    "ASDF/BACKWARD-INTERNALS:LOAD-SYSDEF"
    "ASDF/BACKWARD-INTERFACE:SYSTEM-DEFINITION-PATHNAME")
  "固定cl-swankが使用する既知deprecated function。")

(define-condition sblint-dependency-order-error (error)
  ()
  (:report
   "SBLint dependency order does not cover all Lisp sources"))

(define-condition unexpected-cl-swank-warning (error)
  ((warning
    :initarg :warning
    :reader unexpected-cl-swank-warning-condition))
  (:report
   (lambda (condition stream)
     (format
      stream
      "Unexpected warning while loading fixed cl-swank: ~A"
      (unexpected-cl-swank-warning-condition condition)))))

(defun warning-condition-type-name (warning)
  "WARNINGのcondition型をpackage名付き文字列で返す。"
  (let* ((condition-type (type-of warning))
         (condition-package
           (and (symbolp condition-type)
                (symbol-package condition-type))))
    (unless (and (symbolp condition-type)
                 condition-package)
      (error "Warning condition has an unsupported type: ~S"
             condition-type))
    (format nil
            "~A::~A"
            (package-name condition-package)
            (symbol-name condition-type))))

(defun fixed-cl-swank-pathname-p (pathname)
  "PATHNAMEが固定Jammy cl-swank配下なら真を返す。"
  (when pathname
    (let ((path-string
            (handler-case
                (namestring pathname)
              (error () nil))))
      (and path-string
           (search
            "/usr/share/common-lisp/source/slime/"
            path-string
            :test #'char=)))))

(defun message-prefix-p (prefix message)
  "MESSAGEがPREFIXで始まるなら真を返す。"
  (and (<= (length prefix) (length message))
       (string= prefix message :end2 (length prefix))))

(defun message-suffix-p (suffix message)
  "MESSAGEがSUFFIXで終わるなら真を返す。"
  (let ((start (- (length message) (length suffix))))
    (and (not (minusp start))
         (string= suffix message :start2 start))))

(defun message-contains-all-p (message fragments)
  "MESSAGEがFRAGMENTSを全て含むなら真を返す。"
  (every
   (lambda (fragment)
     (search fragment message :test #'char=))
   fragments))

(defun normalize-warning-message (message)
  "MESSAGE内の表示用空白列を単一spaceへ正規化する。"
  (format
   nil
   "~{~A~^ ~}"
   (remove
    ""
    (uiop:split-string
     message
     :separator
     '(#\Space #\Tab #\Newline #\Return #\Page))
    :test #'string=)))

(defun known-cl-swank-redefinition-warning-p
    (condition-type message)
  "CONDITION-TYPEとMESSAGEが既知redefinition warningなら真を返す。"
  (some
   (lambda (type-and-suffix)
     (and
      (string= condition-type (car type-and-suffix))
      (message-prefix-p "redefining " message)
      (message-suffix-p (cdr type-and-suffix) message)))
   '(("SB-KERNEL::REDEFINITION-WITH-DEFMACRO" . " in DEFMACRO")
     ("SB-KERNEL::REDEFINITION-WITH-DEFUN" . " in DEFUN")
     ("SB-KERNEL::REDEFINITION-WITH-DEFMETHOD" . " in DEFMETHOD"))))

(defun known-cl-swank-inlining-warning-p
    (condition-type message)
  "CONDITION-TYPEとMESSAGEが既知inlining warningなら真を返す。"
  (and
   (string= condition-type
            "SB-C::INLINING-DEPENDENCY-FAILURE")
   (message-prefix-p "Previously compiled calls to " message)
   (message-contains-all-p
    message
    '("could not be"
      "inlined because the structure definition for "))
   (or
    (search "SWANK::KEYWORD-ARG" message :test #'char=)
    (search "SWANK::OPTIONAL-ARG" message :test #'char=))))

(defun known-cl-swank-package-warning-p
    (condition-type message)
  "CONDITION-TYPEとMESSAGEが既知package varianceなら真を返す。"
  (and
   (string= condition-type "SB-INT::PACKAGE-AT-VARIANCE")
   (message-prefix-p
    "SWANK-REPL also exports the following symbols:"
    message)
   (message-contains-all-p
    message
    '("SWANK-REPL:CREATE-REPL"
      "SWANK-REPL:LISTENER-GET-VALUE"
      "SWANK-REPL:CLEAR-REPL-VARIABLES"
      "SWANK-REPL:LISTENER-SAVE-VALUE"
      "SWANK-REPL:LISTENER-EVAL"
      "SWANK-REPL:REDIRECT-TRACE-OUTPUT"))))

(defun known-cl-swank-deprecation-warning-p
    (condition-type message)
  "CONDITION-TYPEとMESSAGEが既知deprecation warningなら真を返す。"
  (and
   (string=
    condition-type
    "UIOP/VERSION::DEPRECATED-FUNCTION-STYLE-WARNING")
   (message-prefix-p
    "DEPRECATED-FUNCTION-STYLE-WARNING: Using deprecated function "
    message)
   (search
    " -- please update your code to use a newer API."
    message
    :test #'char=)
   (some
    (lambda (function-name)
      (search function-name message :test #'char=))
    *fixed-cl-swank-deprecated-functions*)))

(defun known-cl-swank-path-warning-p
    (condition-type message)
  "固定cl-swank pathname内で許可する型とmessageの組を検証する。"
  (or
   (known-cl-swank-redefinition-warning-p
    condition-type message)
   (known-cl-swank-inlining-warning-p
    condition-type message)
   (known-cl-swank-package-warning-p
    condition-type message)
   (known-cl-swank-deprecation-warning-p
    condition-type message)))

(defun known-cl-swank-warning-values-p
    (condition-type message fixed-path-p)
  "実測した型、message、pathnameの組だけを許可する。"
  (let ((normalized-message
          (normalize-warning-message message)))
    (or
     (and fixed-path-p
          (known-cl-swank-path-warning-p
           condition-type normalized-message))
     (member
      (list condition-type normalized-message)
      *fixed-cl-swank-summary-warnings*
      :test #'equal))))

(defun validate-fixed-cl-swank-warning-policy ()
  "warning allowlistの型、message、pathname負例を自己検査する。"
  (let
      ((path-cases
         '(("SB-KERNEL::REDEFINITION-WITH-DEFMACRO"
            "redefining SWANK::WITH-BINDINGS in DEFMACRO")
           ("SB-KERNEL::REDEFINITION-WITH-DEFUN"
            "redefining SWANK/SBCL::SBCL-WITH-XREF-P in DEFUN")
           ("SB-KERNEL::REDEFINITION-WITH-DEFMETHOD"
            "redefining THREAD-FOR-EVALUATION (T T) in DEFMETHOD")
           ("SB-C::INLINING-DEPENDENCY-FAILURE"
            "Previously compiled calls to SWANK::KEYWORD-ARG.KEYWORD could not be inlined because the structure definition for SWANK::KEYWORD-ARG was not yet seen.")
           ("SB-C::INLINING-DEPENDENCY-FAILURE"
            "Previously compiled calls to SWANK::OPTIONAL-ARG.ARG-NAME could not be inlined because the structure definition for SWANK::OPTIONAL-ARG was not yet seen.")
           ("SB-INT::PACKAGE-AT-VARIANCE"
            "SWANK-REPL also exports the following symbols: SWANK-REPL:CREATE-REPL SWANK-REPL:LISTENER-GET-VALUE SWANK-REPL:CLEAR-REPL-VARIABLES SWANK-REPL:LISTENER-SAVE-VALUE SWANK-REPL:LISTENER-EVAL SWANK-REPL:REDIRECT-TRACE-OUTPUT")
           ("UIOP/VERSION::DEPRECATED-FUNCTION-STYLE-WARNING"
            "DEPRECATED-FUNCTION-STYLE-WARNING: Using deprecated function UIOP/BACKWARD-DRIVER:COERCE-PATHNAME -- please update your code to use a newer API.")
           ("UIOP/VERSION::DEPRECATED-FUNCTION-STYLE-WARNING"
            "DEPRECATED-FUNCTION-STYLE-WARNING: Using deprecated function ASDF/BACKWARD-INTERNALS:LOAD-SYSDEF -- please update your code to use a newer API.")
           ("UIOP/VERSION::DEPRECATED-FUNCTION-STYLE-WARNING"
            "DEPRECATED-FUNCTION-STYLE-WARNING: Using deprecated function ASDF/BACKWARD-INTERFACE:SYSTEM-DEFINITION-PATHNAME -- please update your code to use a newer API.")))
       (negative-path-cases
         '(("SB-KERNEL::REDEFINITION-WITH-DEFMACRO"
            "redefining SWANK::WITH-BINDINGS in DEFUN")
           ("SB-KERNEL::REDEFINITION-WITH-DEFUN"
            "unrelated compiler warning")
           ("SB-C::INLINING-DEPENDENCY-FAILURE"
            "Previously compiled calls to OTHER::VALUE could not be inlined because the structure definition for OTHER::VALUE was not yet seen.")
           ("SB-INT::PACKAGE-AT-VARIANCE"
            "SWANK-REPL also exports an unmeasured symbol.")
           ("UIOP/VERSION::DEPRECATED-FUNCTION-STYLE-WARNING"
            "DEPRECATED-FUNCTION-STYLE-WARNING: Using deprecated function OTHER:UNKNOWN -- please update your code to use a newer API."))))
    (dolist (test-case path-cases)
      (destructuring-bind (condition-type message) test-case
        (unless
            (known-cl-swank-warning-values-p
             condition-type message t)
          (error "Known cl-swank warning policy positive failed: ~S"
                 test-case))
        (when
            (known-cl-swank-warning-values-p
             condition-type message nil)
          (error "cl-swank warning escaped its fixed pathname: ~S"
                 test-case))
        (when
            (known-cl-swank-warning-values-p
             "COMMON-LISP::WARNING" message t)
          (error "cl-swank warning accepted a mismatched type: ~S"
                 test-case))))
    (dolist (test-case *fixed-cl-swank-summary-warnings*)
      (destructuring-bind (condition-type message) test-case
        (unless
            (known-cl-swank-warning-values-p
             condition-type message nil)
          (error "Known cl-swank summary positive failed: ~S"
                 test-case))
        (when
            (known-cl-swank-warning-values-p
             "COMMON-LISP::WARNING" message nil)
          (error "cl-swank summary accepted a mismatched type: ~S"
                 test-case))
        (when
            (known-cl-swank-warning-values-p
             condition-type
             (concatenate 'string message " altered")
             nil)
          (error "cl-swank summary accepted an altered message: ~S"
                 test-case))))
    (dolist (test-case negative-path-cases)
      (destructuring-bind (condition-type message) test-case
        (when
            (known-cl-swank-warning-values-p
             condition-type message t)
          (error "cl-swank warning policy negative failed: ~S"
                 test-case))))))

(defun handle-fixed-cl-swank-warning (warning)
  "固定cl-swank由来の既知WARNINGだけをmuffleする。"
  (if (and
       *fixed-cl-swank-load-active*
       (known-cl-swank-warning-values-p
        (warning-condition-type-name warning)
        (princ-to-string warning)
        (or
         (fixed-cl-swank-pathname-p *compile-file-truename*)
         (fixed-cl-swank-pathname-p *load-truename*))))
      (muffle-warning warning)
      (error 'unexpected-cl-swank-warning :warning warning)))

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
  ;; cl-swank自身の既知warningだけを対象sourceのlintより前に隔離する。
  (validate-fixed-cl-swank-warning-policy)
  (let ((*fixed-cl-swank-load-active* t))
    (handler-bind ((warning #'handle-fixed-cl-swank-warning))
      (asdf:load-system "swank")))
  (asdf:load-system "sblint")
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
