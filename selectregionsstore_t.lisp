;;; Run tests.
(defun selectregionsstore-tests ()
  (format t "~&selectregionsstore-tests beginning")

  ; Test selectregionsstore-new.
  (let (store1)
    (setf store1 (selectregionsstore-new nil))
    (assert (selectregionsstore-p store1))

    (format t "~&  selectregionsstore-new OK")
  )


  (format t "~&selectregionsstore-tests done")
  t
)
