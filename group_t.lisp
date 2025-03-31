;; Run group tests.
(defun group-tests ()
  (format t "~&group-tests beginning")

  ; Test group-new
  (let (grp0 errx)
    (setf grp0 (group-new (region-from 'rxxxx) *pn-one* t (rulestore-new (list (rule-from-str "[Xx/XX/XX/XX]")))))
    (assert (group-p grp0))

    (setf grp0 (group-new (region-from 'rxxx0) *pn-two* nil 
          (rulestore-new (list (rule-from-str "[XX/Xx/XX/01]") (rule-from-str "[XX/Xx/XX/00]")))))
    (assert (group-p grp0))

    (setf grp0 (group-new (region-from 'rxxx0) *pn-none* nil (rulestore-new nil)))
    (assert (group-p grp0))

    (setf errx (group-new-na (region-from 'rxxx0) *pn-two* t 
      (rulestore-new (list (rule-from-str "[XX/Xx/XX/01]") 
                           (rule-from-str "[XX/Xx/XX/11]")))))
    (assert (and (err-p errx) (string= (err-str errx) "Rulestore initial regions do not match")))

    (format t "~&  group-new OK")
  )

  (format t "~&group-tests done")
)
