;;;; SPDX-License-Identifier: 0BSD

(in-package #:asdf-user)

(defsystem "konomitv-bs4k-tscodecbridge"
  :version "0.1.0"
  :description "MPEG-TS codec bridge for KonomiTV-BS4K"
  :author "MistVVK"
  :license "0BSD"
  :in-order-to ((test-op
                 (test-op "konomitv-bs4k-tscodecbridge/tests")))
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "conditions")
   (:file "octets")
   (:file "bit-reader")
   (:file "crc32-mpeg2")
   (:module "ts"
    :serial t
    :components
    ((:file "packet")
     (:file "integrity")
     (:file "section")
     (:file "pat")
     (:file "pmt")
     (:file "pes")
     (:file "repacketize")))
   (:module "codecs"
   :serial t
    :components
    ((:file "av1")
     (:file "av1-frame-structure")
     (:file "vp9")
     (:file "opus")))
   (:module "mapping"
    :serial t
    :components
   ((:file "dsl")
     (:file "av1")
     (:file "vp9-private-v1")
     (:file "opus")))
   (:module "tstd"
    :serial t
    :components
    ((:file "arrival")
     (:file "classify")
     (:file "model")))
   (:file "processor")
   (:file "inspector")
   (:file "cli")
   (:file "stream")
   (:file "main")))

(defsystem "konomitv-bs4k-tscodecbridge/tests"
  :description "KonomiTV-BS4K TS Codec Bridge tests"
  :depends-on ("konomitv-bs4k-tscodecbridge")
  :pathname "tests"
  :serial t
  :components
  ((:file "package")
   (:file "harness")
   (:file "av1-frame-structure-test")
   (:file "primitives-test")
   (:file "transport-test")
   (:file "mapping-test")
   (:file "vp9-structure-test")
   (:file "vp9-ffmpeg-state-integration")
   (:file "processor-test")
   (:file "tstd-test")
   (:file "audit-test")
   (:file "fixture-generator")
   (:file "fixture-test")
   (:file "performance")
   (:file "soak")
   (:file "runner"))
  :perform
  (test-op (operation component)
    (declare (ignore operation component))
    (uiop:symbol-call
     :konomitv-bs4k-tscodecbridge
     :run-tests)))

(defsystem "konomitv-bs4k-tscodecbridge/executable"
  :description "Saved executable for KonomiTV-BS4K TS Codec Bridge"
  :depends-on ("konomitv-bs4k-tscodecbridge")
  :build-operation "program-op"
  :build-pathname "build/ts-codec-bridge.elf"
  :entry-point "konomitv-bs4k-tscodecbridge:entry-point")
