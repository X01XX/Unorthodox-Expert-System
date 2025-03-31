; Extraneous useful functions.

(defvar true t)
(defvar false nil)

(defun bool-p (val)
  (or (eq val true) (eq val false))
)

; The opposite of eq.
(defun neq (arg1 arg2) ; -> bool
  (not (eq arg1 arg2))
)

;;; Remove comments from a string.
;;; semicolon to \n, delete a line that is all comment.
(defun remove-comments (str) ; -> string
    (let ((ret "") skip (token ""))
        (loop for char across str do
            (if (char= #\; char) (setf skip t))

            (when (char= #\NewLine char) 
                (setf skip nil)
                (setf token (string-right-trim '(#\Space) token))
                (when (string/= token "")
                    (setf ret (concatenate 'string ret token))
                    (setf ret (concatenate 'string ret (coerce (list #\NewLine) 'string)))
                    (setf token "")
                )
            )
            (when (and (not skip) (char/= #\NewLine char)) 
                (setf token (concatenate 'string token (coerce (list char) 'string)))
            )

        )
        (setf token (string-right-trim '(#\Space) token))
        (when (string/= token "")
            (setf ret (concatenate 'string ret token))
        )
        ret
    )
)

;;; Parse a string, using commas and spaces as separators, between balanced brackets.
(defun parse-str(str) ; -> list of string tokens.
    (let (ret skip (token "") (left 0) (right 0))
        (loop for char across str do
            (if (char= char #\[)
                (incf left)
            )
            (when (char= char #\])
                (incf right)
                (if (> right left)
                    (error "brackets unbalanced"))
            )
            (cond ((and (= left right) (or (char= char #\Space) (char= char #\,) (char= char #\NewLine)))
                   (when (> (length token) 0)
                           (push token ret)
                           (setf token "")))

                  (t (setf token (concatenate 'string token (coerce (list char) 'string))))
            )
        )
        (if (> (length token) 0)
              (push token ret))
        (reverse ret)
    )
)

(defun bool-p (blx) ; -> bool
  (and (symbolp blx) (or (string-equal (symbol-name blx) "t") (string-equal (symbol-name blx) "nil")))
)

(defun xor (b1 b2) ; -> bool
  (or (and b1 (not b2)) (and (not b1) b2))
)

