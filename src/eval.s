(lambda (expr)
  (cond
    (is_number expr) expr
    (= () expr) expr))