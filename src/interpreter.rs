use std::fmt;
use std::rc::Rc;
use std::collections::HashMap;
use sexpr::Sexpr;


#[derive(Clone)]
pub enum Value {
    Sexpr(Sexpr),
    Closure(Rc<Fn(&Env, &[Sexpr]) -> Result<Value, ()>>)
}


pub type Env = Rc<Fn(&str) -> Option<Value>>;


impl Value {
    pub fn closure<F: Fn(&Env, &[Sexpr]) -> Result<Value, ()> + 'static>(f: F) -> Value {
        Value::Closure(Rc::new(f))
    }

    pub fn number(i: i64) -> Value {
        Value::Sexpr(Sexpr::Number(i))
    }
}


impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Sexpr(ref s) => s.fmt(f),
            Value::Closure(_) => "#closure".fmt(f),
        }
    }
}


impl Sexpr {
    pub fn eval(&self) -> Result<Value, ()> {
        let env = builtin();
        eval(&env, self)
    }
}


fn builtin() -> Env {
    fn insert_closure<F: Fn(&Env, &[Sexpr]) -> Result<Value, ()> + 'static>(
        map: &mut HashMap<String, Value>,
        name: &str,
        f: F
    ) {
        map.insert(name.to_owned(), Value::closure(f));
    }

    let mut map = HashMap::new();

    {
        insert_closure(&mut map, "quote", |_, args| {
            if args.len() != 1 {
                return Err(());
            }
            Ok(Value::Sexpr(args[0].clone()))
        });
        insert_closure(&mut map, "if", |env, args| {
            if args.len() != 3 {
                return Err(());
            }
            match try!(eval_number(env, &args[0])) {
                0 => eval(env, &args[2]),
                _ => eval(env, &args[1]),
            }
        });

        insert_closure(&mut map, "lambda", |env, args| {
            if args.len() != 2 {
                return Err(());
            }
            let formals: Vec<String> = if let Sexpr::List(ref args) = args[0] {
                try!(args.iter().map(|arg| {
                    if let Sexpr::Atom(ref name) = *arg {
                        Ok(name.to_owned())
                    } else {
                        Err(())
                    }
                }).collect())
            } else {
                return Err(());
            };

            let body = args[1].clone();
            let env = env.clone();

            Ok(Value::closure(move |val_env, values| {
                let env = env.clone();
                let formals = formals.clone();
                if values.len() != formals.len() {
                    return Err(());
                }
                let values: Vec<Value> = try!(values.iter().map(|v| eval(val_env, v)).collect());
                let new_env: Env = Rc::new(move |y| {
                    if let Some(i) = formals.iter().position(|x| x == y) {
                        Some(values[i].clone())
                    } else {
                        env(y)
                    }
                });
                eval(&new_env, &body)
            }))
        })
    }
    {
        fn insert_sugar<F: Fn(&[Sexpr]) -> Result<Sexpr, ()> + 'static>(
            map: &mut HashMap<String, Value>,
            name: &str,
            f: F
        ) {
            insert_closure(map, name, move |env, args| {
                let desugared = try!(f(args));
                eval(env, &desugared)
            });
        }

        macro_rules! atom (
            ($e:expr) => { Sexpr::Atom($e.to_owned()) }
        );

        macro_rules! list (
            ( $($ee:expr),* ) => { Sexpr::List(vec![$($ee),*]) }
        );
        insert_sugar(&mut map, "rec", |args| {
            if args.len() != 3 {
                return Err(());
            }

            let name = if let Sexpr::Atom(ref name) = args[0] {
                Sexpr::Atom(name.clone())
            } else {
                return Err(());
            };
            let formals = args[1].clone();
            let fix = if let Sexpr::List(ref names) = formals {
                match names.len() {
                    1 => "fix",
                    2 => "fix2",
                    _ => return Err(())
                }
            } else {
                return Err(());
            };
            let body = args[2].clone();

            Ok(list!(
                atom!(fix),
                list!(
                    atom!("lambda"),
                    list!(name),
                    list!(
                        atom!("lambda"),
                        formals,
                        body
                    )
                )
            ))
        });

        insert_sugar(&mut map, "let", |args| {
            if args.len() != 2 {
                return Err(());
            }
            let mut names = vec![];
            let mut inits = vec![];
            let bindings = if let Sexpr::List(ref bindings) = args[0] {
                bindings
            } else {
                return Err(());
            };

            if bindings.len() % 2 != 0 {
                return Err(());
            }

            for i in 0..bindings.len() / 2 {
                names.push(if let Sexpr::Atom(ref name) = bindings[2 * i] {
                    Sexpr::Atom(name.clone())
                } else {
                    return Err(());
                });
                inits.push(bindings[2 * i + 1].clone());
            }


            let body = args[1].clone();

            let lambda = list!(
                atom!("lambda"),
                Sexpr::List(names),
                body
            );

            let mut call = vec![lambda];
            call.extend(inits.into_iter());

            Ok(Sexpr::List(call))
        });
    }

    {
        let bare_map = map.clone();
        let bare_env = mk_env(move |x| bare_map.get(x).cloned());
        let mut insert = |name: &str, code: &str| map.insert(
            name.to_owned(),
            eval(&bare_env, &code.parse::<Sexpr>().unwrap()).unwrap()
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
                insert_closure(&mut map, $name, |env, args| {
                    if args.len() != 2 {
                        return Err(());
                    }
                    let lhs = try!(eval_number(env, &args[0]));
                    let rhs = try!(eval_number(env, &args[1]));
                    Ok(Value::number($op(lhs, rhs)))
                });
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


fn eval_number(env: &Env, expr: &Sexpr) -> Result<i64, ()> {
    match try!(eval(env, expr)) {
        Value::Sexpr(Sexpr::Number(i)) => Ok(i),
        _ => Err(()),
    }
}


fn eval(env: &Env, expr: &Sexpr) -> Result<Value, ()> {
    match *expr {
        Sexpr::Number(i) => Ok(Value::number(i)),
        Sexpr::Atom(ref v) => env(v).ok_or(()),
        Sexpr::List(ref args) => {
            if args.is_empty() {
                return Err(());
            }
            let fun = try!(eval(env, &args[0]));
            if let Value::Closure(ref f) = fun {
                f(env, &args[1..])
            } else {
                Err(())
            }
        },
    }
}


#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use sexpr::Sexpr;
    use super::{eval, Value, mk_env, builtin};


    fn parse(expr: &str) -> Sexpr {
        expr.parse().unwrap()
    }


    fn eval_cmp(expr: &str, result: &str) {
        let expr = parse(expr);
        println!("{:?}", expr);
        let actual_result = expr.eval().unwrap().to_string();
        assert_eq!(result, actual_result);
    }


    fn eval_invalid(expr: &str) {
        let expr = expr.parse::<Sexpr>().unwrap();
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
        let env = mk_env(|x| if x == "foo" { Some(Value::number(92)) } else { None });
        assert_eq!(eval(&env, &parse("foo")).unwrap().to_string(), "92")
    }


    #[test]
    fn var_arif() {
        let initial = builtin();
        let env = mk_env(move |x| Some(Value::number(match x {
            "x" => 1,
            "y" => 2,
            "z" => 3,
            _ => return initial(x)
        })));

        assert_eq!(eval(&env, &parse("(/ (+ x y) z)")).unwrap().to_string(), "1")
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
        let closure = Value::Closure(Rc::new(|env, args| match try!(eval(env, &args[0])) {
            Value::Sexpr(Sexpr::Number(i)) => Ok(Value::number(i + 1)),
            _ => Err(())
        }));
        let env = mk_env(move |x| if x == "f" { Some(closure.clone()) } else { None });
        assert_eq!(eval(&env, &parse("(f 91)")).unwrap().to_string(), "92")
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

        eval_cmp("
((rec F (n) (if (= 0 n) 1 (* n (F (- n 1)))))
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
    fn comb3() {
        eval_cmp("
((rec C (n k)
      (if (= k 0) 1
          (if (= k n) 1
              (+
               (C (- n 1) k)
               (C (- n 1) (- k 1))))
          ))
  7 3)
        ", "35");
    }


    #[test]
    fn sum() {
        eval_cmp("((lambda (x y) (- x y)) 94 2)", "92")
    }


    #[test]
    fn quoting() {
        eval_cmp("(quote (lambda (x) x))", "(lambda (x) x)");
        eval_cmp("(quote (quote quote))", "(quote quote)");
        eval_cmp("'('quote)", "((quote quote))");
        eval_cmp("((lambda (x) x) '(1 2 3))", "(1 2 3)")
    }


    #[test]
    fn zero_args() {
        eval_cmp("((lambda () 92))", "92")
    }


    #[test]
    fn let_() {
        eval_cmp("(let (a 94 b 2) (- a b))", "92");
    }


    #[test]
    fn invalid() {
        eval_invalid("foo");
        //        eval_invalid("(1 2)");
    }
}