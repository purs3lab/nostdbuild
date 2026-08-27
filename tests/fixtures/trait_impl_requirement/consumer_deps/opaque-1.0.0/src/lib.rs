#![no_std]

pub fn norm_squared<T: deep::ComplexField>(v: T) -> T {
    v.norm_squared()
}
