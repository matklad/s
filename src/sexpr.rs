use std::str::FromStr;
use std::fmt;


#[derive(Debug)]
pub enum Sexpr {
    Atom(String),
    List(Vec<Sexpr>)
}


impl FromStr for Sexpr {
    type Err = ();

    fn from_str(s: &str) -> Result<Sexpr, ()> {
        match parse(s) {
            Some((expr, leftover)) => if leftover.trim().is_empty() { Ok(expr) } else { Err(()) },
            _ => Err(()),
        }
    }
}


impl fmt::Display for Sexpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Sexpr::Atom(ref s) => try!(s.fmt(f)),
            Sexpr::List(ref xs) => {
                try!('('.fmt(f));
                let mut first = true;
                for x in xs {
                    if !first {
                        try!(' '.fmt(f));
                    }
                    try!(x.fmt(f));
                    first = false;
                }
                try!(')'.fmt(f));
            }
        }
        Ok(())
    }
}


fn parse(s: &str) -> Option<(Sexpr, &str)> {
    let s = s.trim_left();
    match s.chars().next() {
        None => None,
        Some(')') => None,
        Some('(') => {
            let mut s = &s[1..];
            let mut args = vec![];
            while let Some((arg, leftover)) = parse(s) {
                s = leftover;
                args.push(arg);
            }
            s = s.trim_left();
            if s.chars().next() != Some(')') {
                return None;
            }
            s = &s[1..];

            Some((Sexpr::List(args), s))
        },
        Some(_) => {
            let idx = s.find(|c: char| c == '(' || c == ')' || c.is_whitespace()).unwrap_or(s.len());
            assert!(idx > 0);
            let (atom, leftover) = s.split_at(idx);
            let expr = Sexpr::Atom(atom.to_owned());
            Some((expr, leftover))
        },
    }
}


#[cfg(test)]
mod tests {
    use super::*;


    fn idempotence(input: &str) {
        let roundtrip = input.parse::<Sexpr>().unwrap().to_string();
        assert_eq!(input, roundtrip)
    }


    #[test]
    fn test_atoms() {
        idempotence("λ");
        idempotence("92");
        idempotence("hello");
    }


    #[test]
    fn parenthesis() {
        idempotence("()");
        idempotence("(111)");
        idempotence("((()))");
        idempotence("(+ (* 1 2) (^ 3 4))");
    }
}
