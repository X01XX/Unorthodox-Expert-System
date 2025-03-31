;;;; Run tests.
(defun rule-tests ()
  (format t "~&rule-tests beginning")

  ; Test rule-new.
  (let (rulx sta1 sta2)
    (setf sta1 (state-from 's0101))
    (setf sta2 (state-from 's0110))

    ; Test making a rule.
    (setf rulx (rule-new (sample-new :initial sta1 :result sta2)))
    (assert (rule-p rulx))

    (assert (mask-eq (rule-m00 rulx) (mask-from 'm1000)))
    (assert (mask-eq (rule-m01 rulx) (mask-from 'm0010)))
    (assert (mask-eq (rule-m11 rulx) (mask-from 'm0100)))
    (assert (mask-eq (rule-m10 rulx) (mask-from 'm0001)))

    (format t "~&  rule-new OK")
  )

  ; Test rule-from.
  (let (rulx errx)
    ; Test string too short.
    (setf errx (rule-from-str "[]"))
    (assert (and (err-p errx) (string= (err-str errx) "String is too short")))

    ; Test malformed bit position. is not b, B, x or X.
    (setf errx (rule-from-str "[00/1]"))
    (assert (and (err-p errx) (string= (err-str errx) "Too few characters in a bit position")))

    ; Test string for invalid combination.
    (setf errx (rule-from-str "[00/1M]"))
    (assert (and (err-p errx) (string= (err-str errx) "Invalid character")))

    ; Test string for invalid combination.
    (setf errx (rule-from-str "[000]"))
    (assert (and (err-p errx) (string= (err-str errx) "Too many characters in a bit position")))

    ; Test string for invalid combination.
    (setf errx (rule-from-str "[00/01"))
    (assert (and (err-p errx) (string= (err-str errx) "String must end with a ]")))

    ; Test rule generation.
    (setf rulx (rule-from "[00/01/11/10_X0/X1/X0/X1_XX/XX/Xx/xX]"))
    (assert (and (rule-p rulx)))
    (assert (mask-eq (rule-m00 rulx) (mask-from 'm1000_1010_1100)))
    (assert (mask-eq (rule-m01 rulx) (mask-from 'm0100_0101_0011)))
    (assert (mask-eq (rule-m11 rulx) (mask-from 'm0010_0101_1100)))
    (assert (mask-eq (rule-m10 rulx) (mask-from 'm0001_1010_0011)))

    (format t "~&  rule-from OK")
  )

  ; Test rule-str.
  (let (strx)
    (setf strx (rule-str (rule-from "[00/01/11/10/X0/X1/x0/x1/XX/xx/Xx/xX]")))

    (assert (and (stringp strx) (string= strx "[00/01/11/10_X0/X1/X0/X1_XX/XX/Xx/Xx]")))

    (format t "~&  rule-str OK")
  )

  ; Test rule-union.
  (let (rulx rul1 rul2)

    ; Init rules.
    (setf rul1 (rule-from "[00/00/00_01/01/01_11/11/11_10/10/10_X0/X0/X0_X1/X1/X1_XX/XX/XX_Xx/Xx/Xx]"))
    (setf rul2 (rule-from "[00/11/10_01/11/10_11/00/01_10/00/01_X0/00/10_X1/11/01_XX/00/11_Xx/01/10]"))

    (setf rulx (rule-union rul1 rul2))
    (assert (and (rule-p rulx) (rule-eq rulx (rule-from "[00/XX/X0_01/X1/Xx_11/XX/X1_10/X0/Xx_X0/X0/X0_X1/X1/X1_XX/XX/XX_Xx/Xx/Xx]"))))

    (format t "~&  rule-union OK")
  )

  ; Test rule-is-valid-union.
  (let (rulx rul1 rul2 boolx)

    ; Test invalid unions.
    (setf rul1 (rule-from-str "[00]"))
    (setf rul2 (rule-from-str "[01]"))
    (setf rulx (rule-union rul1 rul2))
    (assert (null rulx))
    (assert (and (bool-p boolx) (null boolx)))

    (setf rul1 (rule-from-str "[11]"))
    (setf rul2 (rule-from-str "[10]"))
    (setf rulx (rule-union rul1 rul2))
    (assert (null rulx))

    ; Test valid unions.
    (setf rul1 (rule-from-str "[00/00/00_01/01/01_11/11/11_10/10/10_X0/X0/X0_X1/X1/X1_XX/XX/XX_Xx/Xx/Xx]"))
    (setf rul2 (rule-from-str "[00/XX/X0_01/Xx/X1_11/XX/X1_10/X0/Xx_X0/00/10_X1/11/01_XX/00/11_Xx/01/10]"))
    (setf rulx (rule-union rul1 rul2))
    (assert (rule-p rulx))

    (format t "~&  rule-is-valid-union OK")
  )

  ; Test rule-intersection.
  (let (rulx rul1 rul2)

    ; Init rules.
    (setf rul1 (rule-from "[00/00/00_01/01/01_11/11/11_10/10/10_X0/X0/X0_X1/X1/X1_XX/XX/XX_Xx/Xx/Xx]"))
    (setf rul2 (rule-from "[00/XX/X0_01/Xx/X1_11/XX/X1_10/X0/Xx_X0/00/10_X1/11/01_XX/00/11_Xx/01/10]"))

    ; Test good intersection.
    (setf rulx (rule-intersection rul1 rul2))
    ;(format t "~&rul ~A" (rule-str rul))
    (assert (and (rule-p rulx) (rule-eq rulx (rule-from "[00/00/00_01/01/01_11/11/11_10/10/10_X0/00/10_X1/11/01_XX/00/11_Xx/01/10]"))))

    (format t "~&  rule-intersection OK")
  )

  ; Test rule-eq.
  (let (boolx rul1 rul2 rul3)
    ; Init rules.
    (setf rul1 (rule-from "[00/00/00]"))
    (setf rul2 (rule-from "[00/11/10]"))
    (setf rul3 (rule-from "[00/11/10]"))

    ; Test equal rules.
    (setf boolx (rule-eq rul2 rul3))
    (assert (and (bool-p boolx) boolx))

    ; Test not equal rules.
    (setf boolx (rule-eq rul1 rul2))
    (assert (and (bool-p boolx) (null boolx)))

    (format t "~&  rule-eq OK")
  )

  ; Test rule-num-bits.
  (let (rul1 num1)
    (setf rul1 (rule-from "[00/01/11/10_x0/x1/Xx/XX]"))
    (setf num1 (rule-num-bits rul1))
    ;(format t "~&num1 ~A" num1)

    (assert (= num1 8))

    (format t "~&  rule-num-bits OK")
  )

  ; Test rule-initial-region.
  (let (rul1 reg1)
    (setf rul1 (rule-from "[00/01/11/10_x0/x1/Xx/XX]"))
    (setf reg1 (rule-initial-region rul1))

    (assert (region-eq reg1 (region-from 'r0011_xxxx)))

    (format t "~&  rule-initial-region OK")
  )

  ; Test rule-result-region.
  (let (rul1 reg1)
    (setf rul1 (rule-from "[00/01/11/10_x0/x1/Xx/XX]"))
    (setf reg1 (rule-result-region rul1))
    ;(format t "~&rule ~A result region  ~A" rul1 (region-str-bits reg1))

    ;; Check for lowercase x in bit position 1.
    (assert (string= (region-str-bits reg1) "0110_01xX"))

    (format t "~&  rule-result-region OK")
  )

  ; Test rule-subset-of
  (let (boolx rul1 rul2)
    ; Init rules.
    (setf rul1 (rule-from "[00/X0/XX_01/X1/Xx_11/X1/XX_10/X0/Xx_X0/X1/XX/Xx]"))
    (setf rul2 (rule-from "[00/00/00_01/01/01_11/11/11_10/10/10_X0/X1/XX/Xx]"))

    ; Test valid subsets.
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert boolx)

    ; Test 16 invalid subsets.
    (setf rul1 (rule-from "[01]"))
    (setf rul2 (rule-from "[00]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from "[11]"))
    (setf rul2 (rule-from "[00]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from "[10]"))
    (setf rul2 (rule-from "[00]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from "[00]"))
    (setf rul2 (rule-from "[01]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from "[11]"))
    (setf rul2 (rule-from "[01]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from "[10]"))
    (setf rul2 (rule-from "[01]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from "[00]"))
    (setf rul2 (rule-from "[11]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from "[01]"))
    (setf rul2 (rule-from "[11]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from "[10]"))
    (setf rul2 (rule-from "[11]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from "[00]"))
    (setf rul2 (rule-from "[10]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from "[01]"))
    (setf rul2 (rule-from "[10]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from "[11]"))
    (setf rul2 (rule-from "[10]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (format t "~&  rule-subset-of OK")
  )

  ; Test rule-new-region-to-region.
  (let (rul1 reg1 reg2)
    (setf reg1 (region-from 'r000_111_xxx_Xx))
    (setf reg2 (region-from 'r01x_01x_01x_xX))

    (setf rul1 (rule-new-region-to-region reg1 reg2))
    ;(format t "~&rul1 ~A" (rule-str rul1))

    (assert (rule-eq rul1 (rule-from "[00/01/00_10/11/11/x0_x1/xx/XX/XX]")))

    (format t "~&  rule-new-region-to-region OK")
  )


  ; Test rule-mask-off-ones.
  (let (rul1 rul2 msk1)
    (setf rul1 (rule-from "[X1/X0/XX/Xx]"))
    (setf msk1 (mask-from 'm1111))
    (setf rul2 (rule-mask-off-ones rul1 msk1))
    ;(format t "~&rul2 ~A" rul2)
    (assert (rule-eq rul2 (rule-from "[01/00/00/01]")))

    (format t "~&  rule-mask-off-ones OK")
  )

  ; Test rule-mask-off-zeros.
  (let (rul1 rul2 msk1)
    (setf rul1 (rule-from "[X1/X0/XX/Xx]"))
    (setf msk1 (mask-from 'm1111))
    (setf rul2 (rule-mask-off-zeros rul1 msk1))
    ;(format t "~&rul2 ~A" rul2)
    (assert (rule-eq rul2 (rule-from "[11/10/11/10]")))

    (format t "~&  rule-mask-off-zeros OK")
  )

  ; Test rule-combine-sequence.
  (let (rul1 rul2 rul3)
    ; Test two rules that intersect.
    (setf rul1 (rule-from "[01/00/xx/11]"))
    (setf rul2 (rule-from "[11/xx/00/10]"))

    (setf rul3 (rule-combine-sequence rul1 rul2))
    ;(format t "~&rul3 ~A" rul3)
    (assert (rule-eq rul3 (rule-from "[01/00/00/10]")))

    ; Test two rules that do not intersect.
    (setf rul1 (rule-from "[00/11/01/XX]"))
    (setf rul2 (rule-from "[11/00/10/XX]"))

    (setf rul3 (rule-combine-sequence rul1 rul2))
    ;(format t "~&rul3 ~A" rul3)
    (assert (rule-eq rul3 (rule-from "[01/10/00/XX]")))

    (format t "~&  rule-combine-sequence OK")
  )

  ; Test rule-restrict-initial-region.
  (let (rul1 rul2 reg1)
    (setf reg1 (region-from 'r0x____1x____0x____1x____01x______01x______01x______01x))
    (setf rul1 (rule-from  "[00/00_11/11_01/01_10/10_x0/x0/x0_x1/x1/x1_xx/xx/xx_Xx/Xx/Xx]"))

    (setf rul2 (rule-restrict-initial-region rul1 reg1))
    ;(format t "~&rul2 ~A" rul2)

    (assert (rule-eq rul2 (rule-from "[00/00_11/11_01/01_10/10_00/10/x0_01/11/x1_00/11/XX_01/10/Xx]")))

    (format t "~&  rule-restrict-initial-region OK")
  )

  ; Test rule-restrict-result-region.
  (let (rul1 rul2 reg1)
    (setf rul1 (rule-from  "[00/00_11/11_01/01_10/10_x0/x0_x1/x1_xx/xx/xx_Xx/Xx/Xx]"))
    (setf reg1 (region-from 'r0x____1x____1x____0x____0x____1x____01x______01x))

    (setf rul2 (rule-restrict-result-region rul1 reg1))
    ;(format t "~&rul2 ~A" rul2)

    (assert (rule-eq rul2 (rule-from "[00/00_11/11_01/01_10/10_x0/x0_x1/X1_00/11/xx_10/01/Xx]")))

    (format t "~&  rule-restrict-result-region OK")
  )

  ; Test rule-changes.
  (let (rul1 reg1 reg2 wanted)
    (setf reg1 (region-from 'r000_111_xxx))
    (setf reg2 (region-from 'r01x_01x_01x))

    (setf rul1 (rule-new-region-to-region reg1 reg2))
    ;(format t "~&rul1       ~A" rul1)

    (setf wanted (rule-changes rul1))
    ;(format t "~& wanted ~A" wanted)

    ;                                                     "000_111_xxx"
    ;                                                     "01x_01x_01x"
    (assert (mask-eq (change-m01 wanted) (mask-from 'm010_000_010)))
    (assert (mask-eq (change-m10 wanted) (mask-from 'm000_100_100)))

    (format t "~&  rule-changes OK")
  )

  ; Test rule-order-bad.
  (let (rul1 rul2 wanted bx)
    (setf wanted (change-new :m01 (mask-from 'm10) :m10 (mask-from 'm01)))
    (setf rul1 (rule-from "[01/00]"))
    (setf rul2 (rule-from "[11/10]"))
    ;(format t "~&rul1        ~A" rul1)
    ;(format t "~&rul2        ~A" rul2)
    ;(format t "~&combine 1 = ~A" (rule-combine-sequence rul1 rul2))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))
;   (format t "~& rule order is bad ~A ~A bad is ~A" rul1 rul2 bx)
    (assert (not bx))

    (setf wanted (change-new :m01 (mask-from 'm01) :m10 (mask-from 'm10)))
    (setf rul1 (rule-from "[10/00]"))
    (setf rul2 (rule-from "[00/01]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))
    (assert (not bx))

    (setf wanted (change-new :m01 (mask-from 'm0) :m10 (mask-from 'm1)))
    (setf rul1 (rule-from "[10]"))
    (setf rul2 (rule-from "[10]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))
    (assert (not bx))

    (setf wanted (change-new :m01 (mask-from 'm01) :m10 (mask-from 'm10)))
    (setf rul1 (rule-from "[10/00]"))
    (setf rul2 (rule-from "[01/01]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))
    (assert bx)

    (setf wanted (change-new :m01 (mask-from 'm01) :m10 (mask-from 'm10)))
    (setf rul1 (rule-from "[10/00]"))
    (setf rul2 (rule-from "[11/01]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))
    (assert bx)

    (setf wanted (change-new :m01 (mask-from 'm1) :m10 (mask-from 'm0)))
    (setf rul1 (rule-from "[01]"))
    (setf rul2 (rule-from "[01]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))
    (assert (not bx))

    (setf wanted (change-new :m01 (mask-from 'm10) :m10 (mask-from 'm01)))
    (setf rul1 (rule-from "[01/00]"))
    (setf rul2 (rule-from "[00/10]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))
    (assert bx)

    (setf wanted (change-new :m01 (mask-from 'm10) :m10 (mask-from 'm01)))
    (setf rul1 (rule-from "[01/00]"))
    (setf rul2 (rule-from "[10/10]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))
    (assert bx)

    (setf wanted (change-new :m01 (mask-from 'm1) :m10 (mask-from 'm0)))
    (setf rul1 (rule-from "[01]"))
    (setf rul2 (rule-from "[00]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))

    (setf wanted (change-new :m01 (mask-from 'm1) :m10 (mask-from 'm0)))
    (setf rul1 (rule-from "[01]"))
    (setf rul2 (rule-from "[01]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))

    (setf wanted (change-new :m01 (mask-from 'm1) :m10 (mask-from 'm0)))
    (setf rul1 (rule-from "[01]"))
    (setf rul2 (rule-from "[11]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))

    (setf wanted (change-new :m01 (mask-from 'm1) :m10 (mask-from 'm0)))
    (setf rul1 (rule-from "[01]"))
    (setf rul2 (rule-from "[10]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))

    (format t "~&  rule-order-bad OK")
  )

  ; Test rule-mutually-exclusive.
  (let (rul1 rul2 wanted bx)

    (setf wanted (change-new :m01 (mask-from 'm10) :m10 (mask-from 'm01)))
    (setf rul1 (rule-from "[01/01]"))
    (setf rul2 (rule-from "[10/10]"))

    (setf bx (rule-mutually-exclusive rul1 rul2 wanted))
    ;(format t "~& rule mutually-exclusive ~A ~A is ~A" rul1 rul2 bx)
    (assert bx)

    (setf wanted (change-new :m01 (mask-from 'm10) :m10 (mask-from 'm01)))
    (setf rul1 (rule-from "[01/01]"))
    (setf rul2 (rule-from "[11/10]"))

    (setf bx (rule-mutually-exclusive rul1 rul2 wanted))
    (assert (not bx))

    (setf wanted (change-new :m01 (mask-from 'm10) :m10 (mask-from 'm01)))
    (setf rul1 (rule-from "[01/11]"))
    (setf rul2 (rule-from "[00/10]"))

    (setf bx (rule-mutually-exclusive rul1 rul2 wanted))
    (assert bx)

    (format t "~&  rule-mutually-excusive OK")
  )

  (format t "~&rule-tests done")
  t
)
