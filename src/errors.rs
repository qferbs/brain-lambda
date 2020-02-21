use std::error;
use std::fmt;

#[derive(Debug, Clone)]
pub struct TokenError<'a> {
    pub string: &'a str
}

impl fmt::Display for TokenError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Invalid token: {:?}", self.string)
    }
}

impl error::Error for TokenError<'_> {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct ParsingError {
    pub string: String
}

impl fmt::Display for ParsingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parsing Error: {:?}", self.string)
    }
}