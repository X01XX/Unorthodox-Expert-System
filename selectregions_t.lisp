;;; Run selectregions tests.
(defun selectregions-tests ()

  (format t "~&selectregions-tests beginning")

  ; Test selectregions-new.
  (let (selectregions1)
    ; Test new, empty, selectregions.
    (setf selectregions1 (selectregions-new
			   (regionscorr-new (list (region-from 'r0X) (region-from 'r1X))) (rate-from '(RT 0 -3))))
    ;(format t "~&~A" selectregions1)
    (assert (selectregions-p selectregions1))
    (format t "~&  selectregions-new OK")
  )

  ;; Test selectregions-from.
  (let (sr1)
    
    (setf sr1 (selectregions-from '(SR (RC (rx0xx1 rXXXX_XXX1_1XXX_XXXX)) (RT 2 0))))
    (assert (selectregions-p sr1))
    (assert (= (selectregions-net-value sr1) 2))
    (assert (= (regionscorr-length (selectregions-regionscorr sr1)) 2))

    (format t "~&  selectregions-from OK")
  )

  (format t "~&selectregions-tests done")
  t
)

