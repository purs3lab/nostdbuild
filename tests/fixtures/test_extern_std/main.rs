extern crate std as std1;

// `Error` requires `Debug + Display`, and a bin target requires `main`. Both are
// checked during analysis, not during expansion, so this fixture never actually
// compiled — the plugin pass used to stop before either was raised.
#[derive(Debug)]
struct ParseError;

impl core::fmt::Display for ParseError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str("parse error")
    }
}

impl crate::std1::error::Error for ParseError {}

fn main() {}
