use std::str::FromStr;

use ast::Expr;
use sexpr::Sexpr;


impl FromStr for Expr {
    type Err = ();

    fn from_str(input: &str) -> Result<Expr, ()> {
        input.parse::<Sexpr>().and_then(|s| read_sexpr(&s))
    }
}


fn read_sexpr(sexpr: &Sexpr) -> Result<Expr, ()> {
    match *sexpr {
        Sexpr::Atom(ref data) => {
            Ok(if let Ok(i) = data.parse::<i64>() {
                Expr::Number(i)
            } else {
                Expr::Var(data.to_owned())
            })
        }
        Sexpr::List(ref args) => {
            if args.is_empty() {
                return Err(());
            }

            if let Sexpr::Atom(ref op) = args[0] {
                match op.as_ref() {
                    "lambda" => return {
                        if args.len() != 3 {
                            return Err(());
                        }

                        let formals: Vec<String> = if let Sexpr::List(ref elements) = args[1] {
                            try!(elements.iter().map(|arg| match *arg {
                                Sexpr::Atom(ref name) => Ok(name.to_owned()),
                                _ => Err(()),
                            }).collect())
                        } else {
                            return Err(());
                        };
                        let body = try!(read_sexpr(&args[2]));
                        Ok(Expr::fun(formals, body))
                    },
                    "if" => return {
                        if args.len() != 4 {
                            return Err(());
                        }

                        let cond = try!(read_sexpr(&args[1]));
                        let tru = try!(read_sexpr(&args[2]));
                        let fls = try!(read_sexpr(&args[3]));
                        Ok(Expr::if_(cond, tru, fls))
                    },
                    _ => {}
                }
            }

            let fun = try!(read_sexpr(&args[0]));
            let actuals: Vec<Expr> = try!(args[1..].iter().map(|a| read_sexpr(a)).collect());
            Ok(Expr::call(fun, actuals))
        },
    }
}


#[cfg(test)]
mod tests {
    use ast::Expr;


    fn idempotence(input: &str) {
        let roundtrip = input.parse::<Expr>().unwrap().to_string();
        assert_eq!(input, roundtrip);
    }


    #[test]
    fn integers() {
        idempotence("92");
        idempotence("0");
        idempotence("-1");
    }


    #[test]
    fn vars() {
        idempotence("λ");
        idempotence("x");
        idempotence("->");
    }


    #[test]
    fn binops() {
        idempotence("(+ 1 2)");
        idempotence("(* 1 2)");
        idempotence("(/ 1 2)");
        idempotence("(- (* (+ 1 2) (/ 6 2)) 3)");
        idempotence("(< 0 (= 1 1))");
    }


    #[test]
    fn lambda() {
        idempotence("(lambda (x) (* x x))")
    }


    #[test]
    fn calls() {
        idempotence("(foo bar)");
        idempotence("((foo bar) (quux 1))");
    }
}
