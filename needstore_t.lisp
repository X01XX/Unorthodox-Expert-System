
;;; Run tests.
(defun needstore-tests ()
  (format t "~&needstore-tests beginning")

  ; Test needstore-new.
  (let (store1)
    (setf store1 (needstore-new nil))
    (assert (needstore-p store1))

    (format t "~&  needstore-new OK")
  )


  (format t "~&needstore-tests done")
  t
)
