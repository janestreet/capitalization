open! Base

(** [Capitalization] is a helper library for renaming strings according to case
    conventions. *)

type t =
  | Pascal_case (* PascalCase *)
  | Camel_case (* camelCase *)
  | Snake_case (* snake_case *)
  | Capitalized_snake_case (* Capitalized_snake_case *)
  | Pascal_snake_case (* Pascal_Snake_Case *)
  | Screaming_snake_case (* SCREAMING_SNAKE_CASE *)
  | Alternating_snake_case (* aLtErNaTiNg_sNaKe_cAsE *)
  | Kebab_case (* kebab-case *)
  | Capitalized_kebab_case (* Capitalized-kebab-case *)
  | Pascal_kebab_case (* Pascal-Kebab-Case *)
  | Screaming_kebab_case (* SCREAMING-KEBAB-CASE *)
  | Alternating_kebab_case (* aLtErNaTiNg-kEbAb-cAsE *)
  | Sentence_case (* Sentence case *)
  | Title_case (* Title Case *)
  | Lower_sentence_case (* lower sentence case *)
  | Upper_sentence_case (* UPPER SENTENCE CASE *)
  | Alternating_sentence_case (* aLtErNaTiNg sEnTeNcE CaSe *)
[@@deriving equal, enumerate, sexp_of]

include Stringable.S with type t := t

(** Re-capitalizes the input string according the convention in [t].

    The input string is assumed to use '_' to separate words, but can contain any mix of
    upper/lower case characters. *)
val apply_to_snake_case : t -> string -> string

(** Capitalizes the input words according to the convention in [t]. *)
val apply_to_words : t -> string list -> string

(** Contains a list of all of the options, for showing in error messages *)
val can_be : string list Lazy.t
