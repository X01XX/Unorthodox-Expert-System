
(defun rulestore-tests ()
  (format t "~&rulestore-tests beginning")

  ; Test rulestore-new.
  (let (store1)
    (setf store1 (rulestore-new (list (rule-from "[Xx/XX/XX/XX]"))))
    (assert (rulestore-p store1))

    (format t "~&  rulestore-new OK")
  )

  ; Test rulestore-eq.
  (let (boolx store1 store2)
    (setf store1 (rulestore-new (list (rule-from "[Xx/XX/XX/XX]") (rule-from "[X1/XX/XX/XX]"))))
    (setf store2 (rulestore-new (list (rule-from "[X1/XX/XX/XX]") (rule-from "[Xx/XX/XX/XX]"))))
    (setf boolx (rulestore-eq store1 store2))
    (assert boolx)

    (setf store2 (rulestore-new (list (rule-from "[X1/XX/XX/XX]") (rule-from "[Xx/X0/XX/XX]"))))
    (setf boolx (rulestore-eq store1 store2))
    (assert (not boolx))

    (format t "~&  rulestore-eq OK")
  )

  ; Test rulestore-subset-of.
  (let (boolx store1 store2)
    (setf store1 (rulestore-new (list (rule-from "[Xx/XX/XX/XX]") (rule-from "[X1/XX/XX/XX]"))))
    (setf store2 (rulestore-new (list (rule-from "[11/00/11/XX]"))))
    (setf boolx (rulestore-subset-of :sup store1 :sub store2))
    (assert boolx)

    (setf store1 (rulestore-new (list (rule-from "[Xx/XX/XX/XX]") (rule-from "[X1/XX/XX/XX]"))))
    (setf store2 (rulestore-new (list (rule-from "[10/00/11/XX]"))))
    (setf boolx (rulestore-subset-of :sup store1 :sub store2))
    (assert boolx)

    (setf store1 (rulestore-new (list (rule-from "[Xx/XX/XX/XX]") (rule-from "[X1/XX/XX/XX]"))))
    (setf store2 (rulestore-new (list (rule-from "[10/00/11/XX]") (rule-from "[11/00/11/XX]"))))
    (setf boolx (rulestore-subset-of :sup store1 :sub store2))
    (assert boolx)

    (setf boolx (rulestore-subset-of :sup store2 :sub store1))
    (assert (not boolx))

    (format t "~&  rulestore-subset-of OK")
  )

  ;; Test rulestore-from-str.
  (let (ruls1 ruls2 ruls3)
    (setf ruls1 (rulestore-from-str "[]"))
    (assert (= (rulestore-length ruls1) 0))

    (setf ruls2 (rulestore-from-str "[[01/10]]"))
    (assert (= (rulestore-length ruls2) 1))

    (setf ruls3 (rulestore-from-str "[[01/10], [11/Xx/XX]]"))
    (assert (= (rulestore-length ruls3) 2))

    (format t "~&  rulestore-from-str OK")
  )

  (format t "~&rulestore-tests done")
  t
)
