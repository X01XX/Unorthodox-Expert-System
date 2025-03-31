;;; Run regionscorr tests.
(defun regionscorr-tests ()

  (format t "~&regionscorr-tests beginning")

  ;; Test regionscorr-new.
  (let (regcorr1)
    ; Test new, empty, regionscorr.
    (setf regcorr1 (regionscorr-new nil))
    (assert (regionscorr-p regcorr1))

    (format t "~&  regionscorr-new OK")
  )

  ;; Test regionscorr-intersect.
  (let (regcorr1 regcorr2 regcorr3)
    (setf regcorr1 (regionscorr-new (list (region-from 'r0X) (region-from 'r1X))))
    (setf regcorr2 (regionscorr-new (list (region-from 'r00) (region-from 'rX0))))
    (setf regcorr3 (regionscorr-new (list (region-from 'r00) (region-from 'r01))))
  
    (assert (regionscorr-intersects regcorr1 regcorr2))
    (assert (not (regionscorr-intersects regcorr1 regcorr3)))

    (format t "~&  regionscorr-intersects OK")
  )

  ;; Test regionscorr-intersection.
  (let (regcorr1 regcorr2 regcorr3)
    (setf regcorr1 (regionscorr-new (list (region-from 'r0X) (region-from 'r1X))))
    (setf regcorr2 (regionscorr-new (list (region-from 'r00) (region-from 'rX0))))

    (setf regcorr3 (regionscorr-intersection regcorr1 regcorr2))
    (assert regcorr3)
    (assert (= (regionscorr-length regcorr3) 2))

    (assert (region-eq (regionstore-first-region (regionscorr-regionstore regcorr3)) (region-from 'r00)))

    (assert (region-eq (regionstore-last-region (regionscorr-regionstore regcorr3)) (region-from 'r10)))

    (format t "~&  regionscorr-intersection OK")
  )

  ;; Test regionscorr-eq.
  (let (regcorr1 regcorr2 regcorr3)
    (setf regcorr1 (regionscorr-new (list (region-from 'r0X) (region-from 'r1X))))
    (setf regcorr2 (regionscorr-new (list (region-from 'r0X) (region-from 'r1X))))
    (setf regcorr3 (regionscorr-new (list (region-from 'r00) (region-from 'rX0))))
  
    (assert (regionscorr-eq regcorr1 regcorr2))

    (assert (not (regionscorr-eq regcorr1 regcorr3)))

    (format t "~&  regionscorr-eq OK")
  )

  ;; Test regionscorr-subtract.
  (let (regcorr1 regcorr2 list1)
    (setf regcorr1 (regionscorr-new (list (region-from 'r0X) (region-from 'r1X))))
    (setf regcorr2 (regionscorr-new (list (region-from 'r00) (region-from 'rX0))))
    (setf list1 (regionscorr-subtract :min regcorr1 :sub regcorr2))

    (assert (= 2 (regionscorrstore-length list1)))

    (assert (regionscorrstore-member list1 (regionscorr-new (list (region-from 'r0X) (region-from 'r11)))))

    (assert (regionscorrstore-member list1 (regionscorr-new (list (region-from 'r01) (region-from 'r1X)))))

    (format t "~&  regionscorr-subtract OK")
  )

  ;; Test regionscorr-x-maskscorr.
  (let (regcorr1 mskx)
    (setf regcorr1 (regionscorr-new (list (region-from 'r0X) (region-from 'r1X))))
    (setf mskx (regionscorr-x-maskscorr regcorr1))
    (assert (maskscorr-eq mskx (maskscorr-new (list (mask-from 'm01) (mask-from 'm01)))))

    (format t "~&  regionscorr-x-maskscorr OK")
  )

  ;; Test regionscorr-1-maskscorr.
  (let (regcorr1 mskx)
    (setf regcorr1 (regionscorr-new (list (region-from 'r0X) (region-from 'r1X))))
    (setf mskx (regionscorr-1-maskscorr regcorr1))
    (assert (maskscorr-eq mskx (maskscorr-new (list (mask-from 'm00) (mask-from 'm10)))))

    (format t "~&  regionscorr-1-maskscorr OK")
  )

  ;; Test regionscorr-0-maskscorr.
  (let (regcorr1 mskx)
    (setf regcorr1 (regionscorr-new (list (region-from 'r0X) (region-from 'r1X))))
    (setf mskx (regionscorr-0-maskscorr regcorr1))
    (assert (maskscorr-eq mskx (maskscorr-new (list (mask-from 'm10) (mask-from 'm00)))))

    (format t "~&  regionscorr-0-maskscorr OK")
  )

  ;; Test regionscorr-set-to-zeros.
  (let (regcorr1 msk1 regcorr2)
    (setf regcorr1 (regionscorr-new (list (region-from 'r01X) (region-from 'r10X))))
    (setf msk1 (maskscorr-new (list (mask-from 'm111) (mask-from 'm010))))
    (setf regcorr2 (regionscorr-set-to-zeros regcorr1 msk1))
    (assert (regionscorr-eq regcorr2 (regionscorr-new (list (region-from 'r000) (region-from 'r10X)))))

    (format t "~&  regionscorr-set-to-zeros OK")
  )

  ;; Test regionscorr-set-to-ones.
  (let (regcorr1 msk1 regcorr2)
    (setf regcorr1 (regionscorr-new (list (region-from 'r01X) (region-from 'r10X))))
    (setf msk1 (maskscorr-new (list (mask-from 'm111) (mask-from 'm010))))
    (setf regcorr2 (regionscorr-set-to-ones regcorr1 msk1))
    (assert (regionscorr-eq regcorr2 (regionscorr-new (list (region-from 'r111) (region-from 'r11X)))))

    (format t "~&  regionscorr-set-to-ones OK")
  )

  ;; Test regionscorr-translate-to.
  (let (regcorr1 regcorr2 regcorr3)
    (setf regcorr1 (regionscorr-new (list (region-from 'r0_1XX1) (region-from 'r1_0XX0))))
    (setf regcorr2 (regionscorr-new (list (region-from 'r1_01XX) (region-from 'r1_00XX))))
    (setf regcorr3 (regionscorr-translate-to regcorr1 regcorr2))
    (assert (regionscorr-eq regcorr3 (regionscorr-new (list (region-from 'r1_01X1) (region-from 'r1_00X0)))))

    (format t "~&  regionscorr-translate-to OK")
  )

  ;; Test regionscorr-from.
  (let (regs1 regs2 regs3)
    (setf regs1 (regionscorr-from '(RC ())))
    (assert (= (regionscorr-length regs1) 0))

    (setf regs2 (regionscorr-from '(RC (r1010))))
    (assert (= (regionscorr-length regs2) 1))

    (setf regs3 (regionscorr-from '(RC (r1011 r111))))
    (assert (= (regionscorr-length regs3) 2))

    (format t "~&  regionscorr-from OK")
  )
   
  (format t "~&regionscorr-tests done")
  t
)

