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

    binop (lambda (op) (lambda (eval args)
      (op (eval (car args)) (eval (cadr args))))
    )

    initial_env (lambda (v) (lookup v (list
      (list '+ (binop +))
      (list '- (binop -))
      (list '* (binop *))
      (list '/ (binop /))
      (list '= (binop =))
      (list '< (binop <))
      (list 'if (lambda (eval args)
        (let (
          cond_ (car   args)
          tru   (cadr  args)
          fls   (caddr args)
        )
        (eval (if (eval cond_) tru fls))
        )))

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
            (= () expr) expr
            (is_atom expr) (env expr)
            1 (let (
                fn   (car expr)
                args (cdr expr)
              )
              ((eval env fn) (lambda (e) (eval env e)) args)))))
)

(lambda (expr) (eval initial_env expr)))