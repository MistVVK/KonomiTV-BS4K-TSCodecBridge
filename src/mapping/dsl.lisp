;;;; SPDX-License-Identifier: 0BSD

(in-package #:konomitv-bs4k-tscodecbridge)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mapping-clause-value (name clauses)
    "CLAUSESからNAMEで始まる宣言の値を返す。"
    (let ((clause (assoc name clauses)))
      (unless clause
        (bridge-error "Codec mapping clause is missing: ~S" name))
      (rest clause)))

  (defun ascii-octets-at-macro-expansion (text)
    "ASCII TEXTをmacro展開時にoctet listへ変換する。"
    (unless (= (length text) 4)
      (bridge-error "Registration identifier must be four characters: ~S"
                    text))
    (loop for character across text
          for code = (char-code character)
          do (unless (<= code #x7f)
               (bridge-error "Registration identifier is not ASCII: ~S"
                             text))
          collect code)))

(defmacro define-ts-codec-mapping (name &body clauses)
  "codec mapping宣言からdescriptor writerとvalidatorを展開する。"
  (let* ((stem (string-upcase (symbol-name name)))
         (registration
           (first (mapping-clause-value :registration clauses)))
         (registration-octets
           (ascii-octets-at-macro-expansion registration))
         (version
           (first (mapping-clause-value :mapping-version clauses)))
         (stream-type
           (first (mapping-clause-value :stream-type clauses)))
         (pes-stream-id
           (first (mapping-clause-value :pes-stream-id clauses)))
         (private-clause
           (mapping-clause-value :private-descriptor clauses))
         (private-tag (first private-clause))
         (private-payload (second private-clause))
         (version-name
           (intern (format nil "+~A-MAPPING-VERSION+" stem) *package*))
         (stream-type-name
           (intern (format nil "+~A-STREAM-TYPE+" stem) *package*))
         (pes-stream-id-name
           (intern (format nil "+~A-PES-STREAM-ID+" stem) *package*))
         (make-name
           (intern (format nil "MAKE-~A-MAPPING-DESCRIPTORS" stem)
                   *package*))
         (validate-name
           (intern (format nil "VALIDATE-~A-MAPPING-DESCRIPTORS" stem)
                   *package*)))
    `(progn
       (defconstant ,version-name ,version)
       (defconstant ,stream-type-name ,stream-type)
       (defconstant ,pes-stream-id-name ,pes-stream-id)
       (defun ,make-name ()
         ,(format nil "~A mapping descriptor列を新しく作る。" stem)
         (list
          (make-descriptor
           :tag #x05
           :payload
           (make-array 4
                       :element-type 'octet
                       :initial-contents ',registration-octets))
          (make-descriptor
           :tag ,private-tag
           :payload
           (make-array ,(length private-payload)
                       :element-type 'octet
                       :initial-contents ',private-payload))))
       (defun ,validate-name (descriptors)
         ,(format nil "~A mapping descriptor列をbyte単位で検証する。" stem)
         (when (< (length descriptors) 2)
           (bridge-error "~A mapping descriptors are missing" ,stem))
         (let ((registration-descriptor (first descriptors))
               (private-descriptor (second descriptors)))
           (unless (and
                    (= (descriptor-tag registration-descriptor) #x05)
                    (equalp
                     (descriptor-payload registration-descriptor)
                     (make-array
                      4
                      :element-type 'octet
                      :initial-contents ',registration-octets)))
             (bridge-error "~A registration descriptor is invalid"
                           ,stem))
           (unless (and
                    (= (descriptor-tag private-descriptor) ,private-tag)
                    (equalp
                     (descriptor-payload private-descriptor)
                     (make-array
                      ,(length private-payload)
                      :element-type 'octet
                      :initial-contents ',private-payload)))
             (bridge-error "~A private descriptor is invalid"
                           ,stem)))
         t))))
