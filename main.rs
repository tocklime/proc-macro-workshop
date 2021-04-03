// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run


use sorted::sorted;

use std::fmt::{self, Display};
use std::io;

#[sorted]
pub enum Error {
    Fmt(fmt::Error),
    Io(io::Error),
}

impl Display for Error {
    #[sorted::check]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        #[sorted]
        match self {
            Error::Io(e) => write!(f, "{}", e),
            Error::Fmt(e) => write!(f, "{}", e),
        }
    }
}

fn main() {}
