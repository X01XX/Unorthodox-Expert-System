;;;; Run tests.
(defun value-tests ()
  (format t "~&value-tests beginning")

  ; Test value-new.
  (let (valx)
    ; Test valid value parameters.
    (setf valx (value-new :num-bits 4 :bits 7))
    (assert (value-p valx))
    (assert (and (value-p valx) (= (value-num-bits valx) 4) (= (value-bits valx) 7)))

    (format t "~&  value-new OK")
  )

  ; Test value-from.
  (let (valx errx)

    ; Test string does not start with the v character.
    (setf errx (value-from-str "x1"))
    ;(format t "~A" errx)
    (assert (and (err-p errx) (string-equal (err-str errx) "value-from-str: Value X1 Should begin with a v character")))

    ; Test string for invalid binary digit.
    (setf errx (value-from-str "v012"))
    (assert (and (err-p errx) (string-equal (err-str errx) "value-from-str: Invalid binary digit 2")))

    ; Test valid binary string.
    (setf valx (value-from 'v1101))
    ;(format t "~&~A" valx)
    ;(format t "~&~A" (value-num-bits valx))
    (assert (and (value-p valx) (= (value-num-bits valx) 4)))

    ; Test valid binary string.
    (setf valx (value-from 'V1101))
    (assert (and (value-p valx) (= (value-num-bits valx) 4)))

    (format t "~&  value-from OK")
  )

  ; Test string-add-underscores.
  (let (strx errx)
    (setf errx (string-add-underscores-na "1_010"))
    (assert (and (err-p errx) (string= (err-str errx) "Argument contains underscores")))

    ; Test empty string.
    (setf strx (string-add-underscores ""))
    (assert (and (stringp strx) (string= strx "")))

    (setf strx (string-add-underscores "12345"))
    (assert (and (stringp strx) (string= strx "1_2345")))

    (format t "~&  string-add-underscores OK")
  )

  ; Test value-str.
  (let (strx)
    (setf strx (value-str (value-from 'v0101_1000)))
    (assert (and (stringp strx) (string= strx "v0101_1000")))

    (format t "~&  value-str OK")
  )

  ; Test value-zerop.
  (let (boolx)
    ; Test a non-zero value.
    (setf boolx (value-zerop (value-from 'v01)))
    (assert (null boolx))

    ; Test a zero value.
    (setf boolx (value-zerop (value-from 'v00)))
    (assert (and (bool-p boolx) boolx))

    (format t "~&  value-zerop OK")
  )

  ; Test value-num-ones.
  (let (numx)
    ; Test a non-zero value.
    (setf numx (value-num-ones (value-from 'v10101)))
    (assert (and (integerp numx) (= numx 3)))

    ; Test a zero value.
    (setf numx (value-num-ones (value-from 'v00)))
    (assert (and (integerp numx) (zerop numx)))

    (format t "~&  value-num-ones OK")
  )

  ; Test value-not.
  (let (valx)
    ; Test a value.
    (setf valx (value-not (value-from 'v0101_1010)))
    (assert (and (value-p valx) (value-eq valx (value-from 'v1010_0101))))

    (format t "~&  value-not OK")
  )

  ; Test value-is-adjacent.
  (let (boolx val1 val2 val3 val4)
    (setf val1 (value-from 'v0000_0001))
    (setf val2 (value-from 'v0000_0010))
    (setf val3 (value-from 'v0000_0011))
    (setf val4 (value-from 'v0011_0011))

    ; Test two values that are adjacent.
    (setf boolx (value-is-adjacent val1 val3))
    (assert (and (bool-p boolx) boolx))

    ; Test two values that are not adjacent.
    (setf boolx (value-is-adjacent val1 val2))
    (assert (and (bool-p boolx) (not boolx)))

    (format t "~&  value-is-adjacent OK")
  )

  ; Test value-eq.
  (let (boolx val1 val2 val3)
    (setf val1 (value-from 'v01))
    (setf val2 (value-from 'v10))
    (setf val3 (value-from 'v01))

    ; Test two values that are eq.
    (setf boolx (value-eq val1 val3))
    (assert (and (bool-p boolx) boolx))

    ; Test two values that are not adjacent.
    (setf boolx (value-eq val1 val2))
    (assert (and (bool-p boolx) (not boolx)))

    (format t "~&  value-eq OK")
  )

  ; Test value-or.
  (let (valx val1 val2 val6)
    (setf val1 (value-from 'v0001))
    (setf val2 (value-from 'v0010))
    (setf val6 (value-from 'v0110))

    ; Test or of three values.
    (setf valx (value-or val1 val2 val6))
    ;(format t "~& val: ~A" val)
    (assert (and (value-p valx) (value-eq valx (value-from 'v0111))))

    (format t "~&  value-or OK")
  )

  ; Test value-and.
  (let (valx val2 val7 vala)
    (setf val7 (value-from 'v0111))
    (setf val2 (value-from 'v0110))
    (setf vala (value-from 'v1010))

    ; Test or of two values.
    (setf valx (value-and val7 val2 vala))
    ;(format t "~& val: ~A" val)
    (assert (and (value-p valx) (value-eq valx (value-from 'v0010))))

    (format t "~&  value-and OK")
  )

  ; Test value-xor.
  (let (valx val1 val2)
    (setf val1 (value-from 'v0011))
    (setf val2 (value-from 'v0110))

    ; Test or of two values.
    (setf valx (value-xor val1 val2))
    ;(format t "~& val: ~A" val)
    (assert (and (value-p valx) (value-eq valx (value-from 'v0101))))

    (format t "~&  value-xor OK")
  )

  ; Test value-eqv.
  (let (valx val1 val2)
    (setf val1 (value-from 'v0011))
    (setf val2 (value-from 'v0110))

    ; Test eqv of two values.
    (setf valx (value-eqv val1 val2))
    ;(format t "~& val: ~A" val)
    (assert (and (value-p valx) (value-eq valx (value-from 'v1010))))

    (format t "~&  value-eqv OK")
  )

  ; Test value-split.
  (let (lstx)
    ; Test splitting 5, to (1, 4).
    (setf lstx (value-split (value-from 'v0101)))
    (assert (and (listp lstx) (= (length lstx) 2)))
    (assert (member (value-from 'v0001) lstx :test #'value-eq))
    (assert (member (value-from 'v0100) lstx :test #'value-eq))

    ; Test splitting 0.
    (setf lstx (value-split (value-from 'v0000)))
    (assert (and (listp lstx) (= (length lstx) 0)))

    (format t "~&  value-split OK")
  )

  ; Test value-between.
  (let (boolx val1 val8 val9 valf)
    (setf val1 (value-from 'v0001))
    (setf val8 (value-from 'v1000))
    (setf val9 (value-from 'v1001))
    (setf valf (value-from 'v1111))

    ; Test a value between two others.
    (setf boolx (value-between :target val9 :from val1 :to valf))
    (assert (and (bool-p boolx) boolx))

    ; Test a value not between two others.
    (setf boolx (value-between :target val8 :from val1 :to valf))
    (assert (and (bool-p boolx) (null boolx)))

    (format t "~&  value-between OK")
  )

  ; Test value-msb.
  (let (valx)
    ; Test one-bit value.
    (setf valx (value-msb (value-from 'v0)))
    (assert (and (value-p valx) (value-eq valx (value-from 'v1))))

    ; Test three-bit value.
    (setf valx (value-msb (value-from 'v000)))
    (assert (and (value-p valx) (value-eq valx (value-from 'v100))))

    ; Test four-bit value.
    (setf valx (value-msb (value-from 'v0000)))
    (assert (and (value-p valx) (value-eq valx (value-from 'v1000))))

    (format t "~&  value-msb OK")
  )

  ; Test value-shift.
  (let (valx val5)
    (setf val5 (value-from 'v0101))

    ; Test shift left by two.
    (setf valx (value-shift val5 2))
    (assert (and (value-p valx) (value-eq valx (value-from 'v0100))))

    ; Test shift right by two.
    (setf valx (value-shift val5 -2))
    ;(format t "~&val ~A" val);
    (assert (and (value-p valx) (value-eq valx (value-from 'v0001))))

    (format t "~&  value-shift OK")
  )

  (format t "~&value-tests done")
  t
)
