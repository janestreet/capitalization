"Capitalization: naming conventions for multiple-word identifiers"
==================================================================

This library provides helper functions for formatting words using common naming
conventions, such as snake_case, camelCase, and PascalCase.

# Example

```ocaml
# open Core

# print_endline "\n";
  let identifier_words = [ "foo"; "bar"; "baz" ] in
  List.iter Capitalization.all ~f:(fun capitalization ->
    let identifier = Capitalization.apply_to_words capitalization identifier_words in
    let capitalization_string = Capitalization.to_string capitalization in
    print_endline [%string "%{identifier} (%{capitalization_string})"]);
  print_endline ""

FooBarBaz (PascalCase)
fooBarBaz (camelCase)
foo_bar_baz (snake_case)
Foo_bar_baz (Capitalized_snake_case)
Foo_Bar_Baz (Pascal_Snake_Case)
FOO_BAR_BAZ (SCREAMING_SNAKE_CASE)
fOo_bAr_bAz (aLtErNaTiNg_sNaKe_cAsE)
foo-bar-baz (kebab-case)
Foo-bar-baz (Capitalized-kebab-case)
Foo-Bar-Baz (Pascal-Kebab-Case)
FOO-BAR-BAZ (SCREAMING-KEBAB-CASE)
fOo-bAr-bAz (aLtErNaTiNg-kEbAb-cAsE)
Foo bar baz (Sentence case)
Foo Bar Baz (Title Case)
foo bar baz (lower sentence case)
FOO BAR BAZ (UPPER SENTENCE CASE)
fOo bAr bAz (aLtErNaTiNg sEnTeNcE CaSe)

- : unit = ()
```
