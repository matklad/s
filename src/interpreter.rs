use std::fmt;
use std::rc::Rc;
use std::collections::HashMap;
use ast::{Expr, BinOpKind};


#[derive(Clone)]
pub enum Value {
    Number(i64),
    Closure(Rc<Fn(Vec<Value>) -> Result<Value, ()>>)
}


impl Value {
    pub fn closure<F: Fn(Vec<Value>) -> Result<Value, ()> + 'static>(f: F) -> Value {
        Value::Closure(Rc::new(f))
    }
}


impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Number(i) => i.fmt(f),
            Value::Closure(_) => "#closure".fmt(f),
        }
    }
}


impl Expr {
    pub fn eval(&self) -> Result<Value, ()> {
        let build_in = {
            let mut m = HashMap::new();
            {
                let empty_env = mk_env(|_| None);
                let mut insert = |name: &str, code: &str| m.insert(
                    name.to_owned(),
                    eval(&code.parse::<Expr>().unwrap(), &empty_env).unwrap()
                );

                insert("fix", "
((lambda (q) (lambda (f) (f (lambda (x)
  (((q q) f) x)))))

 (lambda (q) (lambda (f) (f (lambda (x)
  (((q q) f) x))))))"
                );

                insert("fix2", "
((lambda (q) (lambda (f) (f (lambda (x y)
  (((q q) f) x y)))))

 (lambda (q) (lambda (f) (f (lambda (x y)
  (((q q) f) x y))))))"
                );
            }
            m
        };

        let env = mk_env(move |x| build_in.get(x).cloned());
        eval(self, &env)
    }
}


type Env = Rc<Fn(&str) -> Option<Value>>;


fn mk_env<F: Fn(&str) -> Option<Value> + 'static>(f: F) -> Env {
    Rc::new(f)
}


fn eval(expr: &Expr, env: &Env) -> Result<Value, ()> {
    fn eval_number(expr: &Expr, env: &Env) -> Result<i64, ()> {
        match try!(eval(expr, env)) {
            Value::Number(i) => Ok(i),
            _ => Err(()),
        }
    }

    match *expr {
        Expr::Number(i) => Ok(Value::Number(i)),
        Expr::Var(ref v) => env(v).ok_or(()),
        Expr::BinOp { kind, ref lhs, ref rhs } => {
            let i = try!(eval_number(lhs, env));
            let j = try!(eval_number(rhs, env));
            let result = match kind {
                BinOpKind::Add => i + j,
                BinOpKind::Sub => i - j,
                BinOpKind::Mul => i * j,
                BinOpKind::Div => i / j,
                BinOpKind::Eq => if i == j { 1 } else { 0 },
                BinOpKind::Lt => if i < j { 1 } else { 0 },
            };
            Ok(Value::Number(result))
        }
        Expr::If { ref cond, ref tru, ref fls } => match try!(eval_number(cond, env)) {
            0 => eval(fls, env),
            _ => eval(tru, env),
        },
        Expr::Call { ref fun, ref args } => {
            let fun = try!(eval(fun, env));
            let args = try!(args.iter().map(|arg| eval(arg, env)).collect());
            if let Value::Closure(ref f) = fun {
                f(args)
            } else {
                Err(())
            }
        },
        Expr::Fun { ref args, ref body } => {
            let env = env.clone();
            let args = args.to_owned();
            let body = (**body).clone();
            Ok(Value::closure(move |values| {
                let env = env.clone();
                let args = args.clone();
                if values.len() != args.len() {
                    return Err(());
                }
                let new_env: Env = Rc::new(move |y| {
                    if let Some(i) = args.iter().position(|x| x == y) {
                        Some(values[i].clone())
                    } else {
                        env(y)
                    }
                });
                eval(&body, &new_env)
            }))
        }
    }
}


#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use ast::Expr;
    use super::{eval, Value, mk_env};


    fn parse(expr: &str) -> Expr {
        expr.parse().unwrap()
    }


    fn eval_cmp(expr: &str, result: &str) {
        let expr = parse(expr);
        let actual_result = expr.eval().unwrap().to_string();
        assert_eq!(result, actual_result);
    }


    fn eval_invalid(expr: &str) {
        let expr = expr.parse::<Expr>().unwrap();
        assert!(expr.eval().is_err());
    }


    #[test]
    fn integers() {
        eval_cmp("92", "92");
        eval_cmp("0", "0");
        eval_cmp("-1", "-1");
    }


    #[test]
    fn var() {
        let env = mk_env(|x| if x == "foo" { Some(Value::Number(92)) } else { None });
        assert_eq!(eval(&parse("foo"), &env).unwrap().to_string(), "92")
    }


    #[test]
    fn var_arif() {
        let env = mk_env(|x| Some(Value::Number(match x {
            "x" => 1,
            "y" => 2,
            "z" => 3,
            _ => return None
        })));

        assert_eq!(eval(&parse("(/ (+ x y) z)"), &env).unwrap().to_string(), "1")
    }


    #[test]
    fn binops() {
        eval_cmp("(- (* (+ 1 2) (/ 6 2)) 3)", "6");
        eval_cmp("(= 6 (- (* (+ 1 2) (/ 6 2)) 3))", "1");
        eval_cmp("(< 0 0)", "0");
        eval_cmp("(< 0 1)", "1");
    }


    #[test]
    fn fake_fn() {
        let closure = Value::Closure(Rc::new(|args| match args[0] {
            Value::Number(i) => Ok(Value::Number(i + 1)),
            _ => Err(())
        }));
        let env = mk_env(move |x| if x == "f" { Some(closure.clone()) } else { None });
        assert_eq!(eval(&parse("(f 91)"), &env).unwrap().to_string(), "92")
    }


    #[test]
    fn simple_functions() {
        eval_cmp("((lambda (x) x) 92)", "92");
        eval_cmp("((lambda (x) (* x x)) 92)", "8464")
    }


    #[test]
    fn twice() {
        eval_cmp("((
            (lambda (f) (lambda (x) (f (f x))))
            (lambda (x) (+ x 1))
        ) 90)", "92")
    }


    #[test]
    fn if_() {
        eval_cmp("(if (< 1 2) 92 62)", "92")
    }


    #[test]
    fn fact() {
        eval_cmp("
((

((lambda (q) (lambda (f) (f (lambda (x)
  (((q q) f) x)))))

 (lambda (q) (lambda (f) (f (lambda (x)
  (((q q) f) x))))))

(lambda (F) (lambda (n) (if (= 0 n) 1 (* n (F (- n 1)))))))

5)
        ", "120");

        eval_cmp("
((fix (lambda (F)
    (lambda (n) (if (= 0 n) 1 (* n (F (- n 1)))))))
6)
        ", "720");
    }


    #[test]
    fn comb() {
        eval_cmp("
(((fix (lambda (C)
       (lambda (n) (lambda (k)
                   (if (= k 0) 1
                       (if (= k n) 1
                           (+
                            ((C (- n 1)) k)
                            ((C (- n 1)) (- k 1))))
                       )))))
  7)
 3)
       ", "35")
    }


    #[test]
    fn comb2() {
        eval_cmp("
((fix2 (lambda (C)
         (lambda (n k)
           (if (= k 0) 1
               (if (= k n) 1
                   (+
                    (C (- n 1) k)
                    (C (- n 1) (- k 1))))
               ))))
  7 3)
        ", "35");
    }


    #[test]
    fn sum() {
        eval_cmp("((lambda (x y) (- x y)) 94 2)", "92")
    }


    #[test]
    fn invalid() {
        eval_invalid("foo");
        //        eval_invalid("(1 2)");
    }
}