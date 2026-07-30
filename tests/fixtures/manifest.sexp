;; SPDX-License-Identifier: 0BSD

(:generator "tests/fixture-generator.lisp"
 :fixtures
  ((:file "valid-vp9-opus.ts"
   :kind :valid
   :sha256 "46ab5930ff281db2d9f7f7edc046069ebeafa12e40debe6d3dea1b6a8131adb0"
   :contains (:pat :pmt :pcr :vp9 :two-opus :timed-id3 :data))
  (:file "valid-vp9-superframe.ts"
   :kind :valid
   :sha256 "80a1937b5a28f67eaa5472ce41bce07ac04be6ff58092629e6ebc307dda8f6ac"
   :contains (:pat :pmt :pcr :vp9 :mixed-size-superframe))
  (:file "valid-av1-aac.ts"
   :kind :valid
   :sha256 "37e7dfcaca45eac368f1c8b858587ff1f16e5f0b68bf326dfc66bf9dae800428"
   :contains (:pat :pmt :av1 :aac :timed-id3 :data))
  (:file "corrupt-sync.ts"
   :kind :invalid
   :sha256 "be802370f49b75aeccd4360b8479055b82ee5cf63fffeb4d275d5909c5f8e024"
   :fault :sync-byte)
  (:file "corrupt-truncated-packet.ts"
   :kind :invalid
   :sha256 "7a19f9eec152ecda6e45251b2f9d5c638058d83acb17719b4ba6580207be4008"
   :fault :truncated-transport-packet)
  (:file "corrupt-pmt-crc.ts"
   :kind :invalid
   :sha256 "b8ed4644f86513ea82e556b585182d32e747d6e38748a48eb306c72234f1adc8"
   :fault :pmt-crc)
  (:file "corrupt-video-cc.ts"
   :kind :invalid
   :sha256 "d58a61ebf3ef98a9ab7a00819c68a679ea4a229b1c5a6ddea958a4e31dea1173"
   :fault :video-continuity-counter)
  (:file "corrupt-opus-lacing.ts"
   :kind :invalid
   :sha256 "27dcc49d6e6ab8a332e43bc27fc108cd68e25eefb5a2469cc979c501c586a88f"
   :fault :opus-control-lacing)
  (:file "corrupt-vp9-reference-scale.ts"
   :kind :invalid
   :sha256 "98bd95bb728eb82f2b71f0d3ba30f9f562602e360be61984dbefb60a38f4fb28"
   :fault :vp9-reference-scale)
  (:file "corrupt-vp9-second-subframe.ts"
   :kind :invalid
   :sha256 "e8b76370bdbe296df78c67d5a551beb5f0c9c7707836a2468e2d9b3922eee1dd"
   :fault :vp9-second-subframe)
  (:file "corrupt-vp9-compressed-header-size.ts"
   :kind :invalid
   :sha256 "a7b2f2ffe78ab90b989bbffed7264ec0adc36e88b8f7db019c569757199c83aa"
   :fault :vp9-compressed-header-size)
  (:file "corrupt-vp9-tile-size.ts"
   :kind :invalid
   :sha256 "26cfd12a08c5cc833d0e938b40ca9d3d19e129617d16dfd018a3dd325a970105"
   :fault :vp9-tile-size)
  (:file "corrupt-vp9-reserved-color-space.ts"
   :kind :invalid
   :sha256 "f532ae64750e266c9341aae6a775184a8ea99bd1687af06fd8d4a99547fde575"
   :fault :vp9-reserved-color-space)))
