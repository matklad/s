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

    initial_env (lambda (v) (lookup v (list
      (list '+ +)
      (list '- -)
      (list '* *)
      (list '/ /)
      (list '= =)
      (list '< <)
    )))

    eval (rec eval (env expr)
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
                    (eval env (if (eval env cond_) tru fls))))

                1 ())
            )
        )
        (cond
            (is_number expr) expr
            (is_atom expr) (env expr)
            (= () expr) expr
            1 (let (
                builtin (dispatch_builtin (car expr))
              )
              (cond
                (not (= builtin ())) (builtin (cdr expr))
                1 ((eval env (car expr)) (eval env (cadr expr)) (eval env (caddr expr)))
                )))))
)

(lambda (expr) (eval initial_env expr)))