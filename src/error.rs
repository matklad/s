use std::fmt;


pub struct Error(pub &'static str);

macro_rules! err (
    ($e:expr) => { ::error::Error($e) }
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
