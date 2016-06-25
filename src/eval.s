(let (

    cadr  (lambda (xs) (car (cdr xs)))
    caddr (lambda (xs) (car (cdr (cdr xs))))

    dispatch_binop (lambda (op)
      (cond
        (= op '+) +
        (= op '-) -
        (= op '*) *
        (= op '/) /
        1 ()
      ))

)
(rec eval (expr)
  (cond
    (is_number expr) expr
    (= () expr) expr
    1 (let (op (dispatch_binop (car expr)))
      (cond
        (not (= op ())) (op (eval (cadr expr)) (eval (caddr expr)))
        )))))