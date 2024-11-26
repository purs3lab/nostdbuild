#![cfg_attr(any(feature = "foo", not(feature = "foo")), no_std)]

#[cfg(not(feature = "foo"))]
pub fn add(left: usize, right: usize) -> usize {
  left + right
}

#[cfg(test)]
mod tests{
  use super::*;

  #[test]
  fn it_works() {
    let result = add(2, 2);
    if result == 4 {
        assert_eq!(result, 4);
    } else {
        println!("doesn't work")
    }
    assert_eq!(result, 4);
  }
}
