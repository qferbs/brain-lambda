use std::error;
use std::fmt;

#[derive(Debug, Clone)]
pub struct TokenError<'a>(pub &'a str);

impl<'a> fmt::Display for TokenError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Invalid token: {:?}", self)
    }
}

impl error::Error for TokenError<'_> {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct ParsingError<'a>(pub &'a str);

impl<'a> error::Error for ParsingError<'a> {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl<'a> fmt::Display for ParsingError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Invalid token: {:?}", self)
    }
}