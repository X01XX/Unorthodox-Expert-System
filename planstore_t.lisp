;;; Run tests.
(defun planstore-tests ()
  (format t "~&planstore-tests beginning")

  ; Test planstore-new.
  (let (store1)

    (setf store1 (planstore-new nil))
    (assert (planstore-p store1))

    (format t "~&  planstore-new OK")
  )

  (format t "~&planstore-tests done")
  t
)

