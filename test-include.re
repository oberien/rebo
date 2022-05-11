static MY_INCLUDED_STATIC = "uiae";

fn included_fn() -> int {
  MY_STATIC
}

static DEFINED_IN_INCLUDE = 1;
fn defined_in_include() -> int { 2 };

assert_eq(5, DEFINED_AFTER_INCLUDE);
assert_eq(7, defined_after_include());

"this is the inclusion result"
