;;; Run tests.
(defun groupstore-tests ()
  (format t "~&groupstore-tests beginning")

  ; Test groupstore-new.
  (let (store1)
    (setf store1 (groupstore-new (list 
      (group-new (region-from 'rxxxx) *pn-one* t (rulestore-new (list
        (rule-union
	  (rule-new (sample-new :initial (state-from 's0000) :result (state-from 's0001)))
	  (rule-new (sample-new :initial (state-from 's1111) :result (state-from 's1110))))))))))
 
    (assert (groupstore-p store1))

    ;(format t "~&groupstore ~A" (groupstore-str store1))
    (format t "~&  groupstore-new OK")
  )


  (format t "~&groupstore-tests done")
  t
)
