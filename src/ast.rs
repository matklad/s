use std::fmt;


#[derive(Clone, Debug)]
pub enum Expr {
    Number(i64),
    Var(String),
    Call {
        fun: Box<Expr>,
        args: Vec<Expr>,
    },
}


impl Expr {

    pub fn call(fun: Expr, args: Vec<Expr>) -> Expr {
        Expr::Call { fun: Box::new(fun), args: args }
    }

}


impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expr::Number(i) => i.fmt(f),
            Expr::Var(ref v) => v.fmt(f),
            Expr::Call { ref fun, ref args } => write!(f, "({} {})", fun, args.iter().map(Expr::to_string).collect::<Vec<String>>().join(" ")),
        }
    }
}


