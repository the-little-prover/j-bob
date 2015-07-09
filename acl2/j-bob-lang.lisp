(in-package "ACL2")

(defun if->implies (exp hyps)
  (case-match exp
    (('if Q A E)
     (append
       (if->implies A `(,@hyps ,Q))
       (if->implies E `(,@hyps (not ,Q)))))
    (('equal X Y)
     `((:rewrite :corollary
         (implies (and ,@hyps)
           (equal ,X ,Y)))))
    (& '())))

(defmacro dethm (name args body)
  (declare (ignore args))
  (let ((rules (if->implies body '())))
    `(defthm ,name ,body
       :rule-classes ,rules)))

(defun size (x)
  (if (atom x)
    '0
    (+ '1 (size (car x)) (size (cdr x)))))
