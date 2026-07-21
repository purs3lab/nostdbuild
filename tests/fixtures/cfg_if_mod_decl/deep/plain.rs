// Not gated by anything: a control. Its std usage must stay hard std.
pub fn write_it() {
    use std::io::Write;
    let _ = std::io::stdout().flush();
}
