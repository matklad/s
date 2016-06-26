(let (
    abort (lambda () (/ 0 0))

    cadr (lambda (xs) (car (cdr xs)))
    caar (lambda (xs) (car (car xs)))
    cddr (lambda (xs) (cdr (cdr xs)))

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

    lift2 (lambda (op) (function2 op))

    lift (lambda (op) (function (lambda (args) (op (car args)))))

    special_forms (list
      (list 'cond (lambda (eval env args)
        ((rec go (clauses)
            (if (= () clauses) (abort)
            (let (
              cond_ (car  clauses)
              expr  (cadr clauses)
              rest  (cddr clauses)
            )

            (if (eval env cond_) (eval env expr)

            (go rest)))))
          args
        ))
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

    macro (lambda (m) (lambda (eval env forms)
      (eval env (m forms))
    ))

    macros (list
      (list 'rec (macro (lambda (forms)
        (let (
          name    (car   forms)
          formals (cadr  forms)
          body    (caddr forms)
          fixn    (if (= () (cdr formals)) 'fix 'fix2)
        )

        (list
          fixn
          (list 'lambda (list name) (list 'lambda formals body))
        ))))
      )

      (list 'if (macro (lambda (forms)
        (list
          'cond
          (car forms) (cadr forms)
          1           (caddr forms)
        )))
      )

      (list 'let (macro (lambda (forms)
        (let (
          bindings (car  forms)
          body     (cadr forms)
        )

        ((rec go (bindings)
          (if (= () bindings) body
          (let (
            name (car  bindings)
            init (cadr bindings)
            rest (cddr bindings)
          )
          (list
            (list 'lambda (list name) (go rest))
            init
          )))
        ) bindings))))
      )
    )

    builtin_fns (lambda (eval)
      (let (
        bare_env (mapping_to_env special_forms)

        fix_closure (eval bare_env
          '((lambda (q) (lambda (f) (f (lambda (x)
            (((q q) f) x)))))

            (lambda (q) (lambda (f) (f (lambda (x)
           (((q q) f) x))))))
        )

        fix2_closure (eval bare_env
          '((lambda (q) (lambda (f) (f (lambda (x y)
            (((q q) f) x y)))))

            (lambda (q) (lambda (f) (f (lambda (x y)
           (((q q) f) x y))))))
        )
      )

      (list
        (list '+ (lift2 +))
        (list '- (lift2 -))
        (list '* (lift2 *))
        (list '/ (lift2 /))
        (list '= (lift2 =))
        (list '< (lift2 <))

        (list 'not (lift not))

        (list 'car  (lift  car))
        (list 'cdr  (lift  cdr))
        (list 'cons (lift2 cons))

        (list 'fix fix_closure)
        (list 'fix2 fix2_closure)
        (list 'quote (lambda (eval env forms) (car forms))))
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
    mapping (union (union special_forms (builtin_fns eval)) macros)
    env (mapping_to_env mapping)
  )

  (eval env expr))
))