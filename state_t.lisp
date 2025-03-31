;;; Run tests.
(defun state-tests ()
  (format t "~&state-tests beginning")

  ; Test state-new.
  (let (stax)
    (setf stax (state-new (value-from 'v0000_0011)))
    (assert (state-p stax))
    (assert (and (state-p stax) (= (value-bits (state-value stax)) 3)))
    (format t "~&  state-new OK")
  )

  ; Test state-from.
  (let (stax)
     (setf stax (state-from 's0010_0011))
     (assert (state-p stax))
     (assert (= (value-num-bits (state-value stax)) 8))
     (assert (= (value-bits (state-value stax)) #x23))
 
     (format t "~&  state-from OK")
  )

  ; Test state-str.
  (let (strx sta1)
     (setf sta1 (state-from 's0001))
     (setf strx (state-str sta1))
     (assert (and (stringp strx) (string-equal strx "s0001")))
     (format t "~&  state-str OK")
  )
 
  ; Test state-num-bits.
  (let (numx sta1)
     (setf sta1 (state-from 's0101))
     (setf numx (state-num-bits sta1))
     (assert (and (integerp numx) (= numx 4)))
     (format t "~&  state-num-bits OK")
  )

  ; Test state-eq.
  (let (boolx sta1 sta2 sta3)
    (setf sta1 (state-from 's0001))
    (setf sta2 (state-from 's0010))
    (setf sta3 (state-from 's0001))

    ; Test two states that are eq.
    (setf boolx (state-eq sta1 sta3))
    (assert (and (bool-p boolx) boolx))

    ; Test two states that are not eq.
    (setf boolx (state-eq sta1 sta2))
    (assert (and (bool-p boolx) (not boolx)))

    (format t "~&  state-eq OK")
  )

  ; Test state-xor.
  (let (sta1 sta2 msk1 valx)
 
     (setf sta1 (state-from 's0101_1010))
     (setf sta2 (state-from 's0001_0001))
 
     (setf msk1 (mask-from 'm0010_0010))
 
     ; Test state xor state.
     (setf valx (state-xor sta1 sta2))
     (assert (and (value-p valx) (value-eq valx (value-from 'v0100_1011))))
 
     ; Test state xor mask.
     (setf valx (state-xor sta1 msk1))
     (assert (and (value-p valx) (value-eq valx (value-from 'v0111_1000))))
 
     (format t "~&  state-xor OK")
  )

  ; Test state-between.
  (let (sta5 sta2 sta7 boolx) 
 
     (setf sta5 (state-from 's0101))
     (setf sta2 (state-from 's0010))
     (setf sta7 (state-from 's0111))
 
     ; Test square between two others.
     (setf boolx (state-between sta7 sta5 sta2))
     (assert (and (bool-p boolx) boolx))
 
     ; Test square not between two others.
     (setf boolx (state-between sta5 sta2 sta7))
     (assert (and (bool-p boolx) (not boolx)))
 
     (format t "~&  state-between OK")
  )

  ; Test state-ne.
  (let (boolx sta1 sta2 sta3)
    (setf sta1 (state-from 's0001))
    (setf sta2 (state-from 's0010))
    (setf sta3 (state-from 's0001))

    ; Test two states that are eq.
    (setf boolx (state-ne sta1 sta3))
    (assert (and (bool-p boolx) (not boolx)))

    ; Test two states that are not eq.
    (setf boolx (state-eq sta1 sta2))
    (assert (and (bool-p boolx) (not boolx)))

    (format t "~&  state-ne OK")
  )

  ; Test state-and.
  (let (sta1 sta2 msk1 valx)
 
     (setf sta1 (state-from 's0101_1010))
     (setf sta2 (state-from 's0011_0011))
 
     (setf msk1 (mask-from 'm0110_0110))
 
     ; Test state and state.
     (setf valx (state-and sta1 sta2))
     (assert (and (value-p valx) (value-eq valx (value-from 'v0001_0010))))
 
     ; Test state and mask.
     (setf valx (state-and sta1 msk1))
     (assert (and (value-p valx) (value-eq valx (value-from 'v0100_0010))))
 
     (format t "~&  state-and OK")
  )

  ; Test state-or.
  (let (sta1 sta2 msk1 valx)
 
     (setf sta1 (state-from 's0101_1010))
     (setf sta2 (state-from 's0001_0001))
 
     (setf msk1 (mask-from 'm0010_0010))
 
     ; Test state or state.
     (setf valx (state-or sta1 sta2))
     (assert (and (value-p valx) (value-eq valx (value-from 'v0101_1011))))
 
     ; Test state or mask.
     (setf valx (state-or sta1 msk1))
     (assert (and (value-p valx) (value-eq valx (value-from 'v0111_1010))))
 
     (format t "~&  state-or OK")
  )

  (format t "~&state-tests done")
  t
)
