;;;; Run tests for change struct functions.

(defun change-tests ()
  (format t "~&change-tests beginning")

  ; Test change-new

  ; Test change-num-changes.
  (let (cng1 msk01 msk10 num)
    (setf msk01 (mask-from 'm1001))
    (setf msk10 (mask-from 'm0100))

    (setf cng1 (change-new :m01 msk01 :m10 msk10))
    (assert (change-p cng1))

    (setf num (change-num-changes cng1))
    (assert (= num 3))

    (format t "~&  change-num-changes OK")
  )

  ; Test change-split.
  (let (cng1 cng-lst)
    (setf cng1 (change-new :m01 (mask-from 'm1001) :m10 (mask-from 'm0110)))
    (setf cng-lst (change-split cng1))
    ;(format t "~&cng-lst ~A" cng-lst)

    (assert (= (length cng-lst) 4))
    (assert (member (change-new :m01 (mask-from 'm0000) :m10 (mask-from 'm0100)) cng-lst :test #'change-eq))
    (assert (member (change-new :m01 (mask-from 'm0000) :m10 (mask-from 'm0010)) cng-lst :test #'change-eq))
    (assert (member (change-new :m01 (mask-from 'm1000) :m10 (mask-from 'm0000)) cng-lst :test #'change-eq))
    (assert (member (change-new :m01 (mask-from 'm0001) :m10 (mask-from 'm0000)) cng-lst :test #'change-eq))

    (format t "~&  change-split OK")
  )

  (format t "~&change-tests done")
)
