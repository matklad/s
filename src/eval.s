(let (

    cadr (lambda (xs) (car (cdr xs)))
    caar (lambda (xs) (car (car xs)))

    caddr (lambda (xs) (car (cdr (cdr xs))))
    cadar (lambda (xs) (car (cdr (car xs))))

    lookup (rec find (x xs)
             (cond
               (= () xs)        ()
               (= x (caar xs))  (cadar xs)
               1                (find x (cdr xs)))
    )

    dispatch_binop (lambda (op)
      (lookup op (list
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '< <)
      ))
    )

)

(rec eval (expr)
(let (
    dispatch_builtin (lambda (f)
      (cond
        (= f 'if)
          (lambda (args)
            (let (
              cond_ (car   args)
              tru   (cadr  args)
              fls   (caddr args)
            )
            (eval (if (eval cond_) tru fls))))

        1 ())
    )
)
(cond
    (is_number expr) expr
    (= () expr) expr
    1 (let (
        binop (dispatch_binop (car expr))
        builtin (dispatch_builtin (car expr))
      )
      (cond
        (not (= binop ())) (binop (eval (cadr expr)) (eval (caddr expr)))
        (not (= builtin ())) (builtin (cdr expr))
        ))))))