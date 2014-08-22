;2009-03-14
;Dorai Sitaram, ds26gte at yahoo dot com
;last change 2009-03-17

(defmacro do-in-topological-order (fn &rest deps)
  (let ((all-items (remove-duplicates (apply #'append deps)))
        (f (gensym))
        (callf (gensym)))
    `(let ((,f ,fn)
           ,@(mapcar (lambda (x)
                       `(,x (vector ',x nil nil)))
                     all-items))
      ,@(mapcar
         (lambda (dep)
           (destructuring-bind (x &rest befores) dep
             `(setf (svref ,x 1) (list ,@befores))))
         deps)
      (labels ((,callf (x)
                (unless (svref x 2)
                  (setf (svref x 2) t)
                  (mapc #',callf (svref x 1))
                  (funcall ,f (svref x 0)))))
        ,@(mapcar
           (lambda (x) `(,callf ,x))
           all-items)))))
