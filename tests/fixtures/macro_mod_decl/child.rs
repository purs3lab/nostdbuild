// child module (declared via macro inside a feature="parent"-gated parent).
macro_rules! decl {
    ( $($item:item)* ) => { $( $item )* };
}
decl! {
    mod grandchild;
}
