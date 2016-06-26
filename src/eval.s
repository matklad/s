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

    binop (lambda (op) (lambda (eval env args)
      (op (eval env (car args)) (eval env (cadr args))))
    )

    initial_env (lambda (v) (lookup v (list
      (list '+ (binop +))
      (list '- (binop -))
      (list '* (binop *))
      (list '/ (binop /))
      (list '= (binop =))
      (list '< (binop <))
      (list 'if (lambda (eval env args)
        (let (
          cond_ (car   args)
          tru   (cadr  args)
          fls   (caddr args)
        )

        (eval env (if (eval env cond_) tru fls))))
      )
      (list 'lambda (lambda (leval lenv largs)
        (let (
          formal (caar largs)
          body   (cadr largs)
        )

        (lambda (ceval cenv cargs)
          (let (
            actual (ceval cenv (car cargs))
            benv (lambda (y) (if (= y formal) actual (lenv y)))
          )

          (leval benv body)))))
      )


    )))

    eval (rec eval (env expr)
      (cond
        (is_number expr) expr
        (= () expr) expr
        (is_atom expr) (env expr)
        1 (let (
            fn   (car expr)
            args (cdr expr)
          )
          ((eval env fn) eval env args)))
    )
)

(lambda (expr) (eval initial_env expr)))