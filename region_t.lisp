;; Run tests.
(defun region-tests ()
 (format t "~&region-tests beginning")

 ; Test region-new
 (let (states regx)
   (setf states (list (state-from 's0001) (state-from 's0010) (state-from 's0100)))
   (setf regx (region-new states))
   (assert (region-p regx))
   (assert (and (region-p regx) (string= (region-str regx) "r0xxX+")))

   (format t "~&  region-new OK")
 )

 ; Test high state.
 (let (regx stax states)
   ; Test one-state region.
   (setf regx (region-from 'r10X1))
   (setf stax (region-high-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's1011))))

   ; Test two-state region.
   (setf regx (region-from 'r01X0))
   (setf stax (region-high-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's0110))))

   ; Test three-state region.
   (setf states (list (state-from 's0001) (state-from 's0010) (state-from 's0100)))
   (setf regx (region-new states))
   (setf stax (region-high-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's0111))))

   (format t "~&  region-high-state OK")
 )

 ; Test low state.
 (let (regx stax states)
   ; Test one-state region.
   (setf regx (region-from 'r10X1))
   (setf stax (region-low-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's1001))))

   ; Test two-state region.
   (setf regx (region-from 'r01X1))
   (setf stax (region-low-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's0101))))

   ; Test three-state region.
   (setf states (list (state-from 's0001) (state-from 's0011) (state-from 's0101)))
   (setf regx (region-new states))
   (setf stax (region-low-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's0001))))

   (format t "~&  region-low-state OK")
 )

 ; Test region-x-mask.
 (let (regx mskx)
   ; Test single-state region.
   (setf regx (region-from 'r0101))
   (setf mskx (region-x-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm0000))))

   ; Test two-state region.
   (setf regx (region-from 'r01X1))
   (setf mskx (region-x-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm0010))))

   ; Test three-state region.
   (setf regx (region-new (list (state-from 's0001)
                                (state-from 's0010)
                                (state-from 's0101))))
   (setf mskx (region-x-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm0111))))

   (format t "~&  region-x-mask OK")
 )

 ; Test region-1-mask.
 (let (regx mskx)
   ; Test single-state region.
   (setf regx (region-from 'r0101))
   (setf mskx (region-1-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm0101))))

   ; Test two-state region.
   (setf regx (region-from 'r01X1))
   (setf mskx (region-1-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm0101))))

   ; Test three-state region.
   (setf regx (region-new (list (state-from 's1000)
                                (state-from 's1010)
						        (state-from 's1100))))
   (setf mskx (region-1-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm1000))))

   (format t "~&  region-1-mask OK")
 )

 ; Test region-0-mask.
 (let (regx mskx)
   ; Test single-state region.
   (setf regx (region-from 'r0101))
   (setf mskx (region-0-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm1010))))

   ; Test two-state region.
   (setf regx (region-from 'r01X1))
   (setf mskx (region-0-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm1000))))

   ; Test three-state region.
   (setf regx (region-new (list (state-from 's1000)
                                (state-from 's1010)
						        (state-from 's1100))))
   (setf mskx (region-0-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm0001))))

   (format t "~&  region-0-mask OK")
 )

 ; Test region-second-state.
 (let (regx stax)
   ; Test single-state region.
   (setf regx (region-from 'r0101))
   (setf stax (region-second-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's0101))))

   ; Test two-state region.
   (setf regx (region-from 'r01X1))
   (setf stax (region-second-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's0101))))

   ; Test three-state region.
   (setf regx (region-new (list (state-from 's0001)
                                (state-from 's0010)
                                (state-from 's0111))))
   (setf stax (region-second-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's0110))))

   (format t "~&  region-second-state OK")
 )

 ; Test region-str.
 (let (strx) 
   (setf strx (region-str (region-from 'r01Xx)))

   (assert (and (stringp strx) (string-equal strx "r01Xx")))

   (format t "~&  region-str OK")
 )

 ; Test region-from.
 (let (errx)
   ; Test arg with invalid character.
   (setf errx (region-from-str "01X3"))
   ;(format t "~&~A" errx)
   (assert (and (err-p errx) (string-equal (err-str errx) "region-from-str: Region 01X3 Should begin with an r character")))

   ; Test arg with no valid character.
   (setf errx (region-from-str "r"))
   ;(format t "~&~A" errx)
   (assert (and (err-p errx) (string= (err-str errx) "region-from-str: No valid character found")))

   ; Test good string.
   (assert (string-equal (region-str (region-from 'r01Xx)) "r01Xx"))

   (format t "~&  region-from OK")
 )

 ; Test region-eq.
 (let (boolx reg1 reg2 reg3)
   (setf reg1 (region-from 'r01Xx))
   (setf reg2 (region-from 'r01xx))
   (setf reg3 (region-from 'rXx01))

   ; Test true condition.
   (setf boolx (region-eq reg1 reg2))
   (assert (and (bool-p boolx) boolx))

   ; Test false condition.
   (setf boolx (region-eq reg1 reg3))
   (assert (and (bool-p boolx) (not boolx)))

   (format t "~&  region-eq OK")
 )

 ; Test region-intersection.
 (let (reg1 reg2 reg3)
   (setf reg1 (region-from 'r01Xxx))
   (setf reg2 (region-from 'rXx01x))

   (setf reg3 (region-intersection reg1 reg2))
   ;(format t "~&reg3 ~A" (region-str reg3))
   (assert (and (region-p reg3) (region-eq reg3 (region-from 'r0_101x))))

   (format t "~&  region-intersection OK")
 )

 ; Test region-union.
 (let (reg1 reg2 reg3)
   (setf reg1 (region-from 'r01Xx_0101))
   (setf reg2 (region-from 'r1001_xx01))

   (setf reg3 (region-union reg1 reg2))
   ;(format t "~&reg3 ~A" (region-str reg3))
   (assert (and (region-p reg3) (region-eq reg3 (region-from 'rxxxx_xx01))))

   (format t "~&  region-union OK")
 )

  ; Test region-edge-mask.
  (let (reg1 msk1)
    (setf reg1 (region-from 'r01Xx_0101))
    (setf msk1 (region-edge-mask reg1))
    ;(format t "~&mask ~A" (mask-str msk1))
    (assert (and (mask-p msk1) (mask-eq msk1 (mask-from 'm1100_1111))))

    (format t "~&  region-edge-mask OK")
  )

  ; Test region-distance.
  (let (reg1 reg2 dist)

    ; Test non-intersecting, non-adjacent regions.
    (setf reg1 (region-from 'r0XX1))
    (setf reg2 (region-from 'r1XX0))
    (setf dist (region-distance reg1 reg2))
    (assert (and (integerp dist) (= dist 2)))

    ; Test intersecting regions.
    (setf reg2 (region-from 'rX01X))
    (setf dist (region-distance reg1 reg2))
    (assert (and (integerp dist) (= dist 0)))

    ; Test adjacent regions.
    (setf reg2 (region-from 'r1XX1))
    (setf dist (region-distance reg1 reg2))
    (assert (and (integerp dist) (= dist 1)))

    ; Test subset/superset regions.
    (setf reg1 (region-from 'r0XX1))
    (setf reg2 (region-from 'r01X1))
    (setf dist (region-distance reg1 reg2))
    ;(format t "~&reg1 ~A reg2 ~A distance ~D" (region-str reg1) (region-str reg2) dist)
    (assert (and (integerp dist) (= dist 0)))

    (format t "~&  region-distance OK")
  )

  ; Test region-intersects.
  (let (reg1 reg2 reg3 boolx)
    (setf reg1 (region-from 'r0XX1))
    (setf reg2 (region-from 'rX101))
    (setf reg3 (region-from 'rXX00))
 
    ; Test true condition.
    (setf boolx (region-intersects reg1 reg2))
    (assert (and (bool-p boolx) boolx))
 
    ; Test false condition.
    (setf boolx (region-intersects reg1 reg3))
    (assert (and (bool-p boolx) (not boolx)))

    (format t "~&  region-intersects OK")
  )

  ; Test region-superset-of.
  (let (reg1 reg2 reg3 reg4 boolx)
    (setf reg1 (region-from 'r0XX1))
    (setf reg2 (region-from 'r01x1))
    (setf reg3 (region-from 'rXX00))
    (setf reg4 (region-from 'r1X00))

    ; Test subset.
    (setf boolx (region-superset-of :sup reg1 :sub reg2))
    (assert boolx)

    ; Test not subset.
    (setf boolx (region-superset-of :sup reg1 :sub reg3))
    (assert (not boolx))

    ; Test not subset, no intersection.
    (setf boolx (region-superset-of :sup reg1 :sub reg4))
    (assert (not boolx))

    (format t "~&  region-superset-of OK")
  )
 
  ; Test region-set-to-ones.
  (let (reg1 reg2 mskx)
    (setf reg1 (region-from 'r0XX1))
    (setf mskx (mask-from 'm1100))

    (setf reg2 (region-set-to-ones reg1 mskx))
    (assert (region-eq reg2 (region-from 'r11X1)))

    (format t "~&  region-set-to-ones OK")
  )
 
  ; Test region-set-to-zeros.
  (let (reg1 reg2 mskx)
    (setf reg1 (region-from 'r1XX1))
    (setf mskx (mask-from 'm1100))

    (setf reg2 (region-set-to-zeros reg1 mskx))
    (assert (region-eq reg2 (region-from 'r00X1)))

    (format t "~&  region-set-to-zeros OK")
  )
 
  ; Test region-subtract.
  (let (reg1 reg2 regstr)
    (setf reg1 (region-from 'r0XX1))
    (setf reg2 (region-from 'r0011))

    ; Test subtracting a subset region.
    (setf regstr (region-subtract :min-reg reg1 :sub-reg reg2))
    ;(format t "~&regstr 1: ~A" regstr)
    (assert (= (regionstore-length regstr) 2))
    (assert (regionstore-member regstr (region-from 'r01X1)))
    (assert (regionstore-member regstr (region-from 'r0x01)))

    ; Test subtracting an intersecting region.
    (setf reg2 (region-from 'rX01x))
    (setf regstr (region-subtract :min-reg reg1 :sub-reg reg2))
    ;(format t "~&regstr 2: ~A" regstr)
    (assert (= (regionstore-length regstr) 2))
    (assert (regionstore-member regstr (region-from 'r01X1)))
    (assert (regionstore-member regstr (region-from 'r0x01)))

    ; Test subtracting a non-intersecting region.
    (setf reg2 (region-from 'rX010))
    (setf regstr (region-subtract :min-reg reg1 :sub-reg reg2))
    ;(format t "~&regstr 3: ~A" regstr)
    (assert (= (regionstore-length regstr) 1))
    (assert (regionstore-member regstr (region-from 'r0XX1)))

    ; Test subtracting a superset region.
    (setf reg2 (region-from 'rXXXX))
    (setf regstr (region-subtract :min-reg reg1 :sub-reg reg2))
    ;(format t "~&regstr 4: ~A" regstr)
    (assert (= (regionstore-length regstr) 0))

    (format t "~&  region-subtract OK")
  )
 
  ; Test region-list-p.
  (let (lst1)
    (assert (region-list-p lst1))

    (assert (not (region-list-p 1)))
    
    (setf lst1 (list (region-from 'rXXXX)))
    (assert (region-list-p lst1))

    (setf lst1 (list (region-from 'rXXXX) 1))
    (assert (not (region-list-p lst1)))

    (setf lst1 (list (region-from 'r01XX) (region-from 'r10XX)))
    (assert (region-list-p lst1))

    (setf lst1 (list (region-from 'r01XX) (region-from 'r10X)))
    (assert (region-list-p lst1))

    (format t "~&  region-list-p OK")
  )

  ; Test region-list-some-num-bits-p.
  (let (lst1)
    (assert (region-list-same-num-bits-p lst1))

    (assert (not (region-list-same-num-bits-p 1)))
    
    (setf lst1 (list (region-from 'rXXXX)))
    (assert (region-list-same-num-bits-p lst1))

    (setf lst1 (list (region-from 'rXXXX) 1))
    (assert (not (region-list-same-num-bits-p lst1)))

    (setf lst1 (list (region-from 'r01XX) (region-from 'r10XX)))
    (assert (region-list-same-num-bits-p lst1))

    (setf lst1 (list (region-from 'r01XX) (region-from 'r10X)))
    (assert (not (region-list-same-num-bits-p lst1)))

    (format t "~&  region-list-same-num-bits-p OK")
  )

  ;; Test region-edge-dif-mask.
  (let (reg1 reg2 msk1)
    (setf reg1 (region-from 'rX01X))
    (setf reg2 (region-from 'rX101))

    (setf msk1 (region-edge-dif-mask reg1 reg2))
    (assert (mask-p msk1))
    (assert (= (mask-num-ones msk1) 2))

    (format t "~&  region-edge-dif-mask OK")
  )

  ; Test region-unwanted-changes.
  (let (reg1 unwanted)
    (setf reg1 (region-from 'r01x_01x_01x))
     ;                  0->1 m100_100_100 
     ;                  1->0 m010_010_010

    (setf unwanted (region-unwanted-changes reg1))

    (assert (mask-eq (change-m01 unwanted) (mask-from 'm100_100_100)))
    (assert (mask-eq (change-m10 unwanted) (mask-from 'm010_010_010)))

    (format t "~&  region-unwanted-changes OK")
  )

 (format t "~&region-tests done")
 t
)

