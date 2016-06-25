use std::fmt;
use std::rc::Rc;
use std::collections::HashMap;
use sexpr::Sexpr;


pub struct Error(&'static str);

macro_rules! err (
    ($e:expr) => { ::eval::Error($e) }
);

macro_rules! bail (
    ($e:expr) => { return Err(err!($e)) }
);


impl ::std::error::Error for Error {
    fn description(&self) -> &'static str { self.0 }
}


impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}


impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}


#[derive(Clone)]
pub enum Value {
    Sexpr(Sexpr),
    Closure(Rc<Fn(&Env, &[Sexpr]) -> Result<Value, Error>>)
}


pub type Env = Rc<Fn(&str) -> Option<Value>>;


impl Value {
    pub fn closure<F: Fn(&Env, &[Sexpr]) -> Result<Value, Error> + 'static>(f: F) -> Value {
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
    pub fn eval(&self) -> Result<Value, Error> {
        let env = builtin();
        eval(&env, self)
    }
}


fn builtin() -> Env {
    fn insert_closure<F: Fn(&Env, &[Sexpr]) -> Result<Value, Error> + 'static>(
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
                bail!("Wrong syntax, expected `(quote form)`!")
            }
            Ok(Value::Sexpr(args[0].clone()))
        });

        insert_closure(&mut map, "cond", |env, args| {
            if args.len() % 2 != 0 {
                bail!("Wrong syntax, expected even number of cond branches!");
            }
            for i in 0..args.len() / 2 {
                let cond = &args[2 * i];
                let expr = &args[2 * i + 1];
                if try!(eval_number(env, cond)) != 0 {
                    return eval(env, expr);
                }
            }
            bail!("Unexhaustive cond");
        });

        insert_closure(&mut map, "lambda", |env, args| {
            if args.len() != 2 {
                bail!("Wrong syntax, expected `(lambda args body)`!");
            }
            let formals: Vec<String> = if let Sexpr::List(ref args) = args[0] {
                try!(args.iter().map(|arg| {
                    if let Sexpr::Atom(ref name) = *arg {
                        Ok(name.to_owned())
                    } else {
                        bail!("Wrong syntax, expected atom argument!");
                    }
                }).collect())
            } else {
                bail!("Wrong syntax, expected arguments list!");
            };

            let body = args[1].clone();
            let env = env.clone();

            Ok(Value::closure(move |val_env, values| {
                let env = env.clone();
                let formals = formals.clone();
                if values.len() != formals.len() {
                    bail!("Wrong number of arguments supplied!");
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
        fn insert_sugar<F: Fn(&[Sexpr]) -> Result<Sexpr, Error> + 'static>(
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
                bail!("Invalid syntax, expected `(rec name args body)`!")
            }

            let name = if let Sexpr::Atom(ref name) = args[0] {
                Sexpr::Atom(name.clone())
            } else {
                bail!("Invalid syntax, expected atom name!");
            };
            let formals = args[1].clone();
            let fix = if let Sexpr::List(ref names) = formals {
                match names.len() {
                    1 => "fix",
                    2 => "fix2",
                    _ => bail!("Expected one or two arguments for rec!")
                }
            } else {
                bail!("Invalid syntax, expected list of arguments!")
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
                bail!("Invalid syntax, expected `(let bindings body)`!");
            }
            let mut names = vec![];
            let mut inits = vec![];
            let bindings = if let Sexpr::List(ref bindings) = args[0] {
                bindings
            } else {
                bail!("Invalid syntax, expected list of bindings!");
            };

            if bindings.len() % 2 != 0 {
                bail!("Invalid syntax, odd number of bindings!");
            }

            for i in 0..bindings.len() / 2 {
                names.push(if let Sexpr::Atom(ref name) = bindings[2 * i] {
                    Sexpr::Atom(name.clone())
                } else {
                    bail!("Invalid syntax, expected atom name!");
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

        insert_sugar(&mut map, "if", |args| {
            if args.len() != 3 {
                bail!("Wrong syntax, expected `(if cond tru fls)`!")
            }
            let cond = args[0].clone();
            let tru = args[1].clone();
            let fls = args[2].clone();

            Ok(list!(
                atom!("cond"),
                cond, tru,
                Sexpr::Number(1), fls
            ))
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

        fn insert_function<F: Fn(&[Value]) -> Result<Value, Error> + 'static>(
            map: &mut HashMap<String, Value>,
            name: &str,
            f: F
        ) {
            insert_closure(map, name, move |env, args| {
                let args: Vec<Value> = try!(args.iter().map(|a| eval(env, a)).collect());
                f(&args)
            })
        }

        macro_rules! insert_binop (
            ($name:expr, $op:expr) => {
                insert_function(&mut map, $name, |args| {
                    if args.len() != 2 {
                        bail!("Expected two arguments for a binary operator!");
                    }
                    let (lhs, rhs) = (&args[0], &args[1]);
                    match (lhs, rhs) {
                        (&Value::Sexpr(Sexpr::Number(lhs)), &Value::Sexpr(Sexpr::Number(rhs))) => Ok(Value::number($op(lhs, rhs))),
                        _ => bail!("Expected numbers in a binary operator!")
                    }
                });
            }
        );

        insert_binop!("+", Add::add);
        insert_binop!("-", Sub::sub);
        insert_binop!("*", Mul::mul);
        insert_binop!("/", Div::div);
        insert_binop!("<", |x, y| if x < y { 1 } else { 0 });

        insert_function(&mut map, "=", |args| {
            if args.len() != 2 {
                bail!("Expected two arguments for comparison");
            }
            Ok(match (&args[0], &args[1]) {
                (&Value::Sexpr(ref lhs), &Value::Sexpr(ref rhs)) => Value::number(
                    if lhs == rhs { 1 } else { 0 }
                ),
                _ => Value::number(0)
            })
        });

        insert_function(&mut map, "is_number", |args| {
            if args.len() != 1 {
                bail!("Expected one argument for `is_number`!");
            }

            Ok(Value::number(if let Value::Sexpr(Sexpr::Number(_)) = args[0] { 1 } else { 0 }))
        })
    }
    mk_env(move |x| map.get(x).cloned())
}


fn mk_env<F: Fn(&str) -> Option<Value> + 'static>(f: F) -> Env {
    Rc::new(f)
}


fn eval_number(env: &Env, expr: &Sexpr) -> Result<i64, Error> {
    match try!(eval(env, expr)) {
        Value::Sexpr(Sexpr::Number(i)) => Ok(i),
        _ => bail!("Expected a number!"),
    }
}


fn eval(env: &Env, expr: &Sexpr) -> Result<Value, Error> {
    match *expr {
        Sexpr::Number(i) => Ok(Value::number(i)),
        Sexpr::Atom(ref v) => env(v).ok_or(err!("Unbound variable!")),
        Sexpr::List(ref args) => {
            if args.is_empty() {
                return Ok(Value::Sexpr(Sexpr::List(Vec::new())));
            }
            let fun = try!(eval(env, &args[0]));
            if let Value::Closure(ref f) = fun {
                f(env, &args[1..])
            } else {
                bail!("Closure expected")
            }
        },
    }
}


#[cfg(test)]
mod special_test {
    use std::rc::Rc;
    use sexpr::Sexpr;
    use super::{mk_env, Value, eval, builtin};


    fn eval_invalid(expr: &str) {
        let expr = expr.parse::<Sexpr>().unwrap();
        assert!(expr.eval().is_err());
    }


    #[test]
    fn invalid() {
        eval_invalid("foo");
        eval_invalid("(1 2)");
    }


    #[test]
    fn var() {
        let env = mk_env(|x| if x == "foo" { Some(Value::number(92)) } else { None });
        assert_eq!(eval(&env, &"foo".parse().unwrap()).unwrap().to_string(), "92")
    }


    #[test]
    fn fake_fn() {
        let closure = Value::Closure(Rc::new(|env, args| match try!(eval(env, &args[0])) {
            Value::Sexpr(Sexpr::Number(i)) => Ok(Value::number(i + 1)),
            _ => bail!("Not a number!")
        }));
        let env = mk_env(move |x| if x == "f" { Some(closure.clone()) } else { None });
        assert_eq!(eval(&env, &"(f 91)".parse().unwrap()).unwrap().to_string(), "92")
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

        assert_eq!(eval(&env, &"(/ (+ x y) z)".parse().unwrap()).unwrap().to_string(), "1")
    }
}


#[cfg(test)]
mod eval_tests {
    use sexpr::Sexpr;
    use super::{Value, Error};


    fn eval(expr: &str) -> Result<Value, Error> {
        let expr: Sexpr = expr.parse().expect("Syntax error");
        expr.eval()
    }


    fn meta_eval(expr: &str) -> Result<Value, Error> {
        let interpreter = include_str!("eval.s");
        eval(&format!("({} '{})", interpreter, expr))
    }


    fn eval_cmp(expr: &str, result: &str) {
        println!("{}", expr);
        let actual_result = eval(expr).expect("Eval Error").to_string();
        //        let actual_result = meta_eval(expr).expect("Eval Error").to_string();
        assert_eq!(result, actual_result);
    }


    #[test]
    fn smoke() {
        eval_cmp("92", "92");
    }


    #[test]
    fn integers() {
        eval_cmp("0", "0");
        eval_cmp("-1", "-1");
    }


    #[test]
    fn binops() {
        eval_cmp("(- (* (+ 1 2) (/ 6 2)) 3)", "6");
        eval_cmp("(= 6 (- (* (+ 1 2) (/ 6 2)) 3))", "1");
        eval_cmp("(< 0 0)", "0");
        eval_cmp("(< 0 1)", "1");
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
    fn self_evaluating_empty_list() {
        eval_cmp("()", "()");
    }


    #[test]
    fn polymorphic_equality() {
        eval_cmp("(= () ())", "1");
        eval_cmp("(= '(1 2 (3 4)) '(1 2 (3 4)))", "1");
        eval_cmp("(= '(1 2 (3 4)) '(1 2 (3 4 5)))", "0");
    }


    #[test]
    fn cond() {
        eval_cmp("(cond
                    0 0
                    (- 2 2) 1
                    (+ 0 1) (+ 1 1)
                    (/ 0 0) 92)",
                 "2");
    }
}