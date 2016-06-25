use std::fmt;
use std::rc::Rc;
use std::collections::HashMap;
use ast::Expr;


#[derive(Clone)]
pub enum Value {
    Number(i64),
    Closure(Rc<Fn(&Env, &[Expr]) -> Result<Value, ()>>)
}


pub type Env = Rc<Fn(&str) -> Option<Value>>;


impl Value {
    pub fn closure<F: Fn(&Env, &[Expr]) -> Result<Value, ()> + 'static>(f: F) -> Value {
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
        let env = builtin();
        eval(self, &env)
    }
}


fn builtin() -> Env {
    let mut map = HashMap::new();
    {
        let empty_env = mk_env(|_| None);
        let mut insert = |name: &str, code: &str| map.insert(
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
    {
        use std::ops::{Add, Sub, Mul, Div};

        macro_rules! insert_binop (
            ($name:expr, $op:expr) => {
                map.insert($name.to_string(), Value::closure(|env, args| {
                    if args.len() != 2 {
                        return Err(());
                    }
                    let lhs = try!(eval_number(&args[0], env));
                    let rhs = try!(eval_number(&args[1], env));
                    Ok(Value::Number($op(lhs, rhs)))
                }));
            }
        );

        insert_binop!("+", Add::add);
        insert_binop!("-", Sub::sub);
        insert_binop!("*", Mul::mul);
        insert_binop!("/", Div::div);
        insert_binop!("=", |x, y| if x == y { 1 } else { 0 });
        insert_binop!("<", |x, y| if x < y { 1 } else { 0 });
    }
    mk_env(move |x| map.get(x).cloned())
}


fn mk_env<F: Fn(&str) -> Option<Value> + 'static>(f: F) -> Env {
    Rc::new(f)
}


fn eval_number(expr: &Expr, env: &Env) -> Result<i64, ()> {
    match try!(eval(expr, env)) {
        Value::Number(i) => Ok(i),
        _ => Err(()),
    }
}


fn eval(expr: &Expr, env: &Env) -> Result<Value, ()> {
    match *expr {
        Expr::Number(i) => Ok(Value::Number(i)),
        Expr::Var(ref v) => env(v).ok_or(()),
        Expr::If { ref cond, ref tru, ref fls } => match try!(eval_number(cond, env)) {
            0 => eval(fls, env),
            _ => eval(tru, env),
        },
        Expr::Call { ref fun, ref args } => {
            let fun = try!(eval(fun, env));
            if let Value::Closure(ref f) = fun {
                f(env, args)
            } else {
                Err(())
            }
        },
        Expr::Fun { ref args, ref body } => {
            let env = env.clone();
            let args = args.to_owned();
            let body = (**body).clone();
            Ok(Value::closure(move |val_env, values| {
                let env = env.clone();
                let args = args.clone();
                if values.len() != args.len() {
                    return Err(());
                }
                let values: Vec<Value> = try!(values.iter().map(|v| eval(v, val_env)).collect());
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
    use super::{eval, Value, mk_env, builtin};


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
        let initial = builtin();
        let env = mk_env(move |x| Some(Value::Number(match x {
            "x" => 1,
            "y" => 2,
            "z" => 3,
            _ => return initial(x)
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
        let closure = Value::Closure(Rc::new(|env, args| match try!(eval(&args[0], env)) {
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