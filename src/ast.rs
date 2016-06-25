use std::fmt;


#[derive(Clone, Debug)]
pub enum Expr {
    Number(i64),
    Var(String),
    BinOp {
        kind: BinOpKind,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        tru: Box<Expr>,
        fls: Box<Expr>,

    },
    Call {
        fun: Box<Expr>,
        args: Vec<Expr>,
    },
    Fun {
        args: Vec<String>,
        body: Box<Expr>
    }
}


#[derive(Clone, Copy, Debug)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
}


impl Expr {
    pub fn binop(kind: BinOpKind, lhs: Expr, rhs: Expr) -> Expr {
        Expr::BinOp {
            kind: kind,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn if_(cond: Expr, tru: Expr, fls: Expr) -> Expr {
        Expr::If {
            cond: Box::new(cond),
            tru: Box::new(tru),
            fls: Box::new(fls),
        }
    }

    pub fn call(fun: Expr, args: Vec<Expr>) -> Expr {
        Expr::Call { fun: Box::new(fun), args: args }
    }

    pub fn fun(args: Vec<String>, body: Expr) -> Expr {
        Expr::Fun { args: args, body: Box::new(body) }
    }
}


impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expr::Number(i) => i.fmt(f),
            Expr::Var(ref v) => v.fmt(f),
            Expr::BinOp { kind, ref lhs, ref rhs } => write!(f, "({} {} {})", kind, lhs, rhs),
            Expr::If { ref cond, ref tru, ref fls } => write!(f, "(if {} {} {})", cond, tru, fls),
            Expr::Call { ref fun, ref args } => write!(f, "({} {})", fun, args.iter().map(Expr::to_string).collect::<Vec<String>>().join(" ")),
            Expr::Fun { ref args, ref body } => {
                write!(f, "(lambda ({}) {})", args.join(" "), body)
            }
        }
    }
}


impl fmt::Display for BinOpKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BinOpKind::Add => '+',
            BinOpKind::Sub => '-',
            BinOpKind::Mul => '*',
            BinOpKind::Div => '/',
            BinOpKind::Eq => '=',
            BinOpKind::Lt => '<',
        }.fmt(f)
    }
}
