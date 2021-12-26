#[test]
fn bad_attributes() {
    let test = trybuild::TestCases::new();
    test.compile_fail("tests/bad_attributes/*.rs");
}
