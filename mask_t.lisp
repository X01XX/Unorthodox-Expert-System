;;; Run tests.
(defun mask-tests ()
  (format t "~&mask-tests beginning")

  ; Test mask-new.
  (let (mskx)
    ; Test creating a valid mask.
    (setf mskx (mask-new (value-from 'v0000_0011)))
    (assert (mask-p mskx))
    (assert (and (mask-p mskx) (= (value-bits (mask-value mskx)) 3)))
 
    (format t "~&  mask-new OK")
  )

  ; Test mask-str.
  (let (strx mskx)
     (setf mskx (mask-from 'm0001))
     (setf strx (mask-str mskx))
     (assert (and (stringp strx) (string= strx "m0001")))
 
     (format t "~&  mask-str OK")
  )

  ; Test mask-num-bits.
  (let (numx msk1)
     (setf msk1 (mask-from 'm0101))
 
     ; Test a valid mask.
     (setf numx (mask-num-bits msk1))
     (assert (and (integerp numx) (= numx 4)))
 
     (format t "~&  mask-num-bits OK")
  )

  ; Test mask-eq.
  (let (boolx msk1 msk2 msk3)
    (setf msk1 (mask-from 'm0001))
    (setf msk2 (mask-from 'm0010))
    (setf msk3 (mask-from 'm0001))

    ; Test two masks that are eq.
    (setf boolx (mask-eq msk1 msk3))
    (assert (and (bool-p boolx) boolx))

    ; Test two masks that are not eq.
    (setf boolx (mask-eq msk1 msk2))
    (assert (and (bool-p boolx) (not boolx)))

    (format t "~&  mask-eq OK")
  )

 
  ; Test mask-from.
  (let (mskx)
     (setf mskx (mask-from 'm0010_0011))
     (assert (mask-p mskx))
     (assert (= (value-num-bits (mask-value mskx)) 8))
     (assert (= (value-bits (mask-value mskx)) #x23))
 
     (format t "~&  mask-from OK")
  )

  ; Test mask-msb.
  (let (mskx)
    ; Test one-bit mask.
    (setf mskx (mask-msb (mask-from 'm0)))
    (assert (mask-eq mskx (mask-from 'm1)))

    ; Test two-bit mask.
    (setf mskx (mask-msb (mask-from 'm00)))
    (assert (mask-eq mskx (mask-from 'm10)))

    ; Test three-bit mask.
    (setf mskx (mask-msb (mask-from 'm000)))
    (assert (mask-eq mskx (mask-from 'm100)))

    ; Test four-bit mask.
    (setf mskx (mask-msb (mask-from 'm0000)))
    (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm1000))))

    (format t "~&  mask-msb OK")
  )

  ; Test mask-shift.
  (let (mskx msk5)
    (setf msk5 (mask-from 'm0101))

    ; Test shift left by two.
    (setf mskx (mask-shift msk5 2))
    (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm0100))))

    ; Test shift right by two.
    (setf mskx (mask-shift msk5 -2))
    ;(format t "~&msk ~A" msk);
    (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm0001))))

    (format t "~&  mask-shift OK")
  )

  ; Test mask-zerop.
  (let (mskx)
    ; Test a non-zero mask.
    (setf mskx (mask-zerop (mask-from 'm0000_0001)))
    (assert (null mskx))

    ; Test a zero mask.
    (setf mskx (mask-zerop (mask-from 'm0000_0000)))
    (assert (and (bool-p mskx) mskx))

    (format t "~&  mask-zerop OK")
  )

  ; Test mask-and.
  (let (valx msk3 msk6 sta6)
    (setf msk3 (mask-from 'm0011))
    (setf msk6 (mask-from 'm0110))
    (setf sta6 (state-from 's0110))

    ; Test and of two masks.
    (setf valx (mask-and msk3 msk6))
    ;(format t "~& msk: ~A" msk)
    (assert (and (value-p valx) (value-eq valx (value-from 'v0010))))

    ; Test and of a mask and a state.
    (setf valx (mask-and msk3 sta6))
    ;(format t "~& msk: ~A" msk)
    (assert (and (value-p valx) (value-eq valx (value-from 'v0010))))

    (format t "~&  mask-and OK")
  )

  ; Test mask-and-not.
  (let (valx msk3 msk6 sta6)
    (setf msk3 (mask-from 'm0011))
    (setf msk6 (mask-from 'm0110))
    (setf sta6 (state-from 's0110))

    ; Test and-not of two masks.
    (setf valx (mask-and-not msk3 msk6))
    ;(format t "~& msk: ~A" msk)
    (assert (and (value-p valx) (value-eq valx (value-from 'v0001))))

    ; Test and-not of a mask and a state.
    (setf valx (mask-and-not msk3 sta6))
    ;(format t "~& msk: ~A" msk)
    (assert (and (value-p valx) (value-eq valx (value-from 'v0001))))

    (format t "~&  mask-and-not OK")
  )

  ; Test mask-or.
  (let (valx msk3 msk6)
    (setf msk3 (mask-from 'm0011))
    (setf msk6 (mask-from 'm0110))

    ; Test or of two masks.
    (setf valx (mask-or msk3 msk6))
    ;(format t "~& msk: ~A" msk)
    (assert (and (value-p valx) (value-eq valx (value-from 'v0111))))

    (format t "~&  mask-or OK")
  )

  ; Test mask-not.
  (let (mskx)
    (setf mskx (mask-new (mask-not (mask-from 'm0101_1010))))
    (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm1010_0101))))

    (format t "~&  mask-not OK")
  )

  ; Test mask-subset-of.
  (let (boolx msk1 msk2)
    (setf msk1 (mask-from 'm0101_1010))
    (setf msk2 (mask-from 'm0100_0010))

    (setf boolx (mask-subset-of :sub-mask msk2 :sup-mask msk1))
    (assert boolx)

    (setf boolx (mask-subset-of :sub-mask msk1 :sup-mask msk2))
    (assert (not boolx))

    (format t "~&  mask-subset-of OK")
  )

  (format t "~&mask-tests done")
  t
)
