(let (

    cadr (lambda (xs) (car (cdr xs)))
    caar (lambda (xs) (car (car xs)))

    caddr (lambda (xs) (car (cdr (cdr xs))))
    cadar (lambda (xs) (car (cdr (car xs))))

    map (rec map (f xs)
      (if (= () xs)
        ()
        (cons (f (car xs)) (map f (cdr xs))))
    )

    zip (rec zip (xs ys)
      (if (= () xs)
        ()
        (cons
          (list (car xs) (car ys))
          (zip  (cdr xs) (cdr ys))))
    )

    union (rec union (xs ys)
      (if (= () xs) ys (union (cdr xs) (cons (car xs) ys)))
    )

    lookup (rec find (x xs)
      (cond
        (= () xs)        ()
        (= x (caar xs))  (cadar xs)
        1                (find x (cdr xs)))
    )

    mapping_to_env (lambda (mapping)
      (lambda (x) (lookup x mapping))
    )

    function (lambda (f) (lambda (eval env args)
      (f (map (lambda (x) (eval env x)) args)))
    )

    function2 (lambda (f)
      (function (lambda (args) (f (car args) (cadr args))))
    )

    binop (lambda (op) (function2 op))

    special_forms (list
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
          formals (car  largs)
          body    (cadr largs)
        )

        (lambda (ceval cenv cargs)
          (let (
            tagged_actuals (map
              (lambda (x) (list (ceval cenv x) ()))
              cargs
            )

            mapping (zip formals tagged_actuals)

            benv (lambda (y)
              (let (param (lookup y mapping))
              (if (= () param)
                (lenv y)
                (car param)))
            )
          )

        (leval benv body)))))
      )
    )

    builtin_fns (lambda (eval)
      (let (
        fix_closure (eval (mapping_to_env special_forms)
          '((lambda (q) (lambda (f) (f (lambda (x)
            (((q q) f) x)))))

            (lambda (q) (lambda (f) (f (lambda (x)
           (((q q) f) x))))))
        )
      )
      (list
        (list '+ (binop +))
        (list '- (binop -))
        (list '* (binop *))
        (list '/ (binop /))
        (list '= (binop =))
        (list '< (binop <))
        (list 'fix fix_closure))
      )
    )

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

(lambda (expr)
  (let (
    mapping (union special_forms (builtin_fns eval))
    env (mapping_to_env mapping)
  )

  (eval env expr))
))