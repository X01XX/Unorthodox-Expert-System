;;; Run tests.
(defun domainstore-tests ()
  (format t "~&domainstore-tests beginning")

  ; Test domainstore-new.
  (let (domstr)
    (setf domstr (domainstore-new))
    (assert (domainstore-p domstr))

    (format t "~&  domainstore-new OK")
  )

  (format t "~&domainstore-tests done")
  t
)
