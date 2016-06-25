(let (

    cadr  (lambda (xs) (car (cdr xs)))
    caddr (lambda (xs) (car (cdr (cdr xs))))

)
(rec eval (expr)
  (cond
    (is_number expr) expr
    (= () expr) expr
    1 (let (f    (car   expr)
            lhs  (eval (cadr  expr))
            rhs  (eval (caddr expr)))
      (cond
        (= f '+) (+ lhs rhs)
        (= f '-) (- lhs rhs)
        (= f '*) (* lhs rhs)
        (= f '/) (/ lhs rhs)

        )))))