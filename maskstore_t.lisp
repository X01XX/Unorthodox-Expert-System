;;; Run tests.
(defun maskstore-tests ()
  (format t "~&maskstore-tests beginning")

  ; Test maskstore-new.
  (let (store1)
    (setf store1 (maskstore-new nil))
    (assert (maskstore-p store1))

    (format t "~&  maskstore-new OK")
  )


  (format t "~&maskstore-tests done")
  t
)
