# Primitives

```rust
// unit - rebo's void type
let unit = ();
assert(unit == ());

// int
assert_eq(1 + 2, 3);

// float use similarity equality
let sum = 0.1 + 0.2;
let expected = 0.3;
assert(sum == expected);
assert(sum.to_bits() != expected.to_bits());

// bool

// strings
// string lengths
let s = "uiae";
assert(s.len_utf8() == 4);
assert(s.len_utf16() == 4);
assert(s.len_utf32() == 4);
assert(s.len_grapheme_clusters() == 4);
assert(s.len_legacy_grapheme_clusters() == 4);

// format strings
let foo = 4;
assert(f"{foo * 10 + 2} is the answer" == "42 is the answer");
assert(f"{1337:#8x}" == "   0x539");
assert(f"{"uiae":?}" == "\"uiae\"");

// string slicing
let s = "abcdef";
assert(s.slice(1) == "bcdef");
assert(s.slice(-1) == "f");
assert(s.slice(1, 3) == "bc");
assert(s.slice(0, -1) == "abcde");
assert(s.slice(1, -1) == "bcde");
assert(s.slice(-2, -1) == "e");
let s = "αβγδεζ";
assert(s.slice(1) == "βγδεζ");
assert(s.slice(-1) == "ζ");
assert(s.slice(1, 3) == "βγ");
assert(s.slice(0, -1) == "αβγδε");
assert(s.slice(1, -1) == "βγδε");
assert(s.slice(-2, -1) == "ε");
```
