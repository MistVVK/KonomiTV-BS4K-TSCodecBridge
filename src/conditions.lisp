;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(define-condition bridge-error (error)
  ((message
    :initarg :message
    :reader bridge-error-message
    :type string))
  (:documentation "Bridgeが処理を安全に継続できないときの基底condition。")
  (:report
   (lambda (condition stream)
     (write-string (bridge-error-message condition) stream))))

(define-condition bridge-io-error (bridge-error)
  ((operation
    :initarg :operation
    :reader bridge-io-error-operation
    :type keyword)
   (cause
    :initarg :cause
    :reader bridge-io-error-cause
    :type serious-condition))
  (:documentation "バイナリstreamの入出力失敗を表すcondition。"))

(defun bridge-error (format-control &rest format-arguments)
  "FORMAT-CONTROLとFORMAT-ARGUMENTSから独自conditionを通知する。"
  (error 'bridge-error
         :message (apply #'format nil format-control format-arguments)))
