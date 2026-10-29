use std::fmt;
use std::ops::Deref;

/// An OCaml `string`: bytes, not necessarily UTF-8. Its [`fmt::Debug`] is
/// OCaml's `String.escaped`, quoted, and its [`fmt::Display`] is lossy UTF-8.
#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Str(pub Box<[u8]>);

impl Str {
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }

    pub fn to_str(&self) -> Result<&str, std::str::Utf8Error> {
        std::str::from_utf8(&self.0)
    }
}

impl Deref for Str {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        &self.0
    }
}

impl From<&[u8]> for Str {
    fn from(bytes: &[u8]) -> Self {
        Str(bytes.into())
    }
}

impl From<Vec<u8>> for Str {
    fn from(bytes: Vec<u8>) -> Self {
        Str(bytes.into_boxed_slice())
    }
}

impl From<&str> for Str {
    fn from(s: &str) -> Self {
        Str(s.as_bytes().into())
    }
}

impl From<String> for Str {
    fn from(s: String) -> Self {
        Str(s.into_bytes().into_boxed_slice())
    }
}

impl PartialEq<str> for Str {
    fn eq(&self, other: &str) -> bool {
        *self.0 == *other.as_bytes()
    }
}

impl PartialEq<&str> for Str {
    fn eq(&self, other: &&str) -> bool {
        *self.0 == *other.as_bytes()
    }
}

impl fmt::Debug for Str {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("\"")?;
        for &b in self.0.iter() {
            match b {
                b'"' => f.write_str("\\\"")?,
                b'\\' => f.write_str("\\\\")?,
                b'\n' => f.write_str("\\n")?,
                b'\t' => f.write_str("\\t")?,
                b'\r' => f.write_str("\\r")?,
                b'\x08' => f.write_str("\\b")?,
                b' '..=b'~' => write!(f, "{}", b as char)?,
                _ => write!(f, "\\{b:03}")?,
            }
        }
        f.write_str("\"")
    }
}

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&String::from_utf8_lossy(&self.0))
    }
}
