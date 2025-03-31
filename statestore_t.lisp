;;; Run tests.
(defun statestore-tests ()
  (format t "~&statestore-tests beginning")

  ; Test statestore-new.
  (let (ssx ss1 len)

    ; A state list.
    (setf ss1 (list (state-from 's0001) (state-from 's0010) (state-from 's1000)))
 
    ; Test a good state list.
    (setf ssx (statestore-new ss1))
    (assert (statestore-p ssx))

    (setf len (statestore-length ssx))
    ;(format t "~&len ~A" len)
    (assert (= len 3))
 
    (format t "~&  statestore-new OK")
  )

  ; Test statestore-length.
  (let (ssx len)
    (setf ssx (statestore-new (list (state-from 's0001) (state-from 's0010))))
 
    (setf len (statestore-length ssx))
    ;(format t "~&len ~A" len)
    (assert (= len 2))

    (format t "~&  statestore-length OK")
  )

  ; Test statestore-remove-unneeded.
  (let (str1 str2)
    (setf str1 (statestore-new (list (state-from 's0001) (state-from 's0010) (state-from 's0111) (state-from 's0100))))

    (setf str2 (statestore-remove-unneeded str1))
    ;(format t "~&str ~A" str2)
    (assert (= (statestore-length str2) 3))
    (assert (statestore-member str2 (state-from 's0001)))
    (assert (statestore-member str2 (state-from 's0010)))
    (assert (statestore-member str2 (state-from 's0111)))

    (format t "~&  statestore-remove-unneeded OK")
  )

  (format t "~&statestore-tests done")
  t
)
