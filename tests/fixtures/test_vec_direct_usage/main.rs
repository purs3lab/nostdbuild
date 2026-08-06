fn main() {
    // Annotated because the element type is otherwise unconstrained (`E0282`),
    // which the plugin pass only reaches now that it runs through type checking.
    let mut v: std::vec::Vec<u8> = std::vec::Vec::new();
    v.push(1);
}