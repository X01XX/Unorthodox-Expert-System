;;;; Run maskscorr tests.
(defun maskscorr-tests ()
  (format t "~&maskscorr-tests beginning")

  ;; Test maskscorr-new
  (let (msksc1)
    (setf msksc1 (maskscorr-new (list (mask-from 'm0000_0001) (mask-from 'm0000_0001_0001_0000))))
    (assert (maskscorr-p msksc1))
    (assert (= (maskscorr-length msksc1) 2))
    (assert (mask-eq (maskscorr-first-mask msksc1) (mask-from 'm0000_0001)))
    (assert (mask-eq (maskscorr-last-mask  msksc1) (mask-from 'm0000_0001_0001_0000)))

    (format t "~&  maskscorr-new OK")
  )

  ;; Test maskscorr-eq
  (let (msksc1 msksc2 msksc3)
    (setf msksc1 (maskscorr-new (list (mask-from 'm0000_0001) (mask-from 'm0000_0001_0001_0000))))
    (setf msksc2 (maskscorr-new (list (mask-from 'm0000_0001) (mask-from 'm0000_0001_0001_0000))))
    (setf msksc3 (maskscorr-new (list (mask-from 'm0000_0001) (mask-from 'm0001_0001_0001_0000))))

    (assert (maskscorr-eq msksc1 msksc2))
    (assert (not (maskscorr-eq msksc1 msksc3)))

    (format t "~&  maskscorr-eq OK")
  )

  ;; Test maskscorr-num-ones.
  (let (msksc1)
    (setf msksc1 (maskscorr-new (list (mask-from 'm0000_0001) (mask-from 'm0000_0001_0001_0000))))
    (assert (= (maskscorr-num-ones msksc1) 3))

    (format t "~&  maskscorr-num-ones OK")
  )

  (format t "~&maskscorr-tests done")
)
