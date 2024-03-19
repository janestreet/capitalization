open! Base

type t =
  | Pascal_case
  | Camel_case
  | Snake_case
  | Capitalized_snake_case
  | Pascal_snake_case
  | Screaming_snake_case
  | Alternating_snake_case
  | Kebab_case
  | Capitalized_kebab_case
  | Pascal_kebab_case
  | Screaming_kebab_case
  | Alternating_kebab_case
  | Sentence_case
  | Title_case
  | Lower_sentence_case
  | Upper_sentence_case
  | Alternating_sentence_case
[@@deriving equal, enumerate, sexp_of]

let to_string = function
  | Pascal_case -> "PascalCase"
  | Camel_case -> "camelCase"
  | Snake_case -> "snake_case"
  | Capitalized_snake_case -> "Capitalized_snake_case"
  | Pascal_snake_case -> "Pascal_Snake_Case"
  | Screaming_snake_case -> "SCREAMING_SNAKE_CASE"
  | Alternating_snake_case -> "aLtErNaTiNg_sNaKe_cAsE"
  | Kebab_case -> "kebab-case"
  | Pascal_kebab_case -> "Pascal-Kebab-Case"
  | Capitalized_kebab_case -> "Capitalized-kebab-case"
  | Screaming_kebab_case -> "SCREAMING-KEBAB-CASE"
  | Alternating_kebab_case -> "aLtErNaTiNg-kEbAb-cAsE"
  | Sentence_case -> "Sentence case"
  | Title_case -> "Title Case"
  | Lower_sentence_case -> "lower sentence case"
  | Upper_sentence_case -> "UPPER SENTENCE CASE"
  | Alternating_sentence_case -> "aLtErNaTiNg sEnTeNcE CaSe"
;;

let can_be = lazy (List.map all ~f:to_string)

let of_string = function
  | "PascalCase" -> Pascal_case
  | "camelCase" -> Camel_case
  | "snake_case" -> Snake_case
  | "Capitalized_snake_case" -> Capitalized_snake_case
  | "Pascal_Snake_Case" -> Pascal_snake_case
  | "SCREAMING_SNAKE_CASE" -> Screaming_snake_case
  | "aLtErNaTiNg_sNaKe_cAsE" -> Alternating_snake_case
  | "kebab-case" -> Kebab_case
  | "Pascal-Kebab-Case" -> Pascal_kebab_case
  | "Capitalized-kebab-case" -> Capitalized_kebab_case
  | "SCREAMING-KEBAB-CASE" -> Screaming_kebab_case
  | "aLtErNaTiNg-kEbAb-cAsE" -> Alternating_kebab_case
  | "Sentence case" -> Sentence_case
  | "Title Case" -> Title_case
  | "lower sentence case" -> Lower_sentence_case
  | "UPPER SENTENCE CASE" -> Upper_sentence_case
  | "aLtErNaTiNg sEnTeNcE CaSe" -> Alternating_sentence_case
  | s ->
    raise_s
      (List
         [ Atom "Capitalization.of_string: invalid indentation string"
         ; List [ Atom "s"; Atom s ]
         ; List [ Atom "can_be"; [%sexp_of: string list] (Lazy.force can_be) ]
         ])
;;

module Case = struct
  type t =
    | Upper
    | Lower

  let apply t c =
    match t with
    | Upper -> Char.uppercase c
    | Lower -> Char.lowercase c
  ;;
end

module Position = struct
  type t =
    | First_word_start
    | Subsequent_word_start
    | Other
end

module Parity = struct
  type t =
    | Odd
    | Even

  let other = function
    | Odd -> Even
    | Even -> Odd
  ;;
end

module What_to_capitalize = struct
  type t =
    | Everything
    | Nothing
    | First_word_only
    | All_words_but_first
    | Every_word
    | Alternating_letters

  let case_for_position t ~(position : Position.t) ~(parity : Parity.t) : Case.t =
    match t with
    | Everything -> Upper
    | Nothing -> Lower
    | First_word_only ->
      (match position with
       | First_word_start -> Upper
       | Subsequent_word_start | Other -> Lower)
    | All_words_but_first ->
      (match position with
       | Subsequent_word_start -> Upper
       | First_word_start | Other -> Lower)
    | Every_word ->
      (match position with
       | First_word_start | Subsequent_word_start -> Upper
       | Other -> Lower)
    | Alternating_letters ->
      (match parity with
       | Even -> Lower
       | Odd -> Upper)
  ;;
end

let what_to_capitalize : t -> What_to_capitalize.t = function
  | Snake_case | Kebab_case | Lower_sentence_case -> Nothing
  | Capitalized_snake_case | Capitalized_kebab_case | Sentence_case -> First_word_only
  | Camel_case -> All_words_but_first
  | Alternating_snake_case | Alternating_kebab_case | Alternating_sentence_case ->
    Alternating_letters
  | Pascal_case | Pascal_snake_case | Pascal_kebab_case | Title_case -> Every_word
  | Screaming_snake_case | Screaming_kebab_case | Upper_sentence_case -> Everything
;;

let separator = function
  | Snake_case
  | Capitalized_snake_case
  | Pascal_snake_case
  | Screaming_snake_case
  | Alternating_snake_case -> Some '_'
  | Kebab_case
  | Capitalized_kebab_case
  | Pascal_kebab_case
  | Screaming_kebab_case
  | Alternating_kebab_case -> Some '-'
  | Pascal_case | Camel_case -> None
  | Sentence_case
  | Title_case
  | Lower_sentence_case
  | Upper_sentence_case
  | Alternating_sentence_case -> Some ' '
;;

let case_for_position t ~position ~parity =
  What_to_capitalize.case_for_position (what_to_capitalize t) ~position ~parity
;;

let apply_to_snake_case' s ~case_for_position ~separator =
  let output_size =
    match separator with
    | None -> String.length s - String.count s ~f:([%equal: char] '_')
    | Some _ -> String.length s
  in
  let buffer = Buffer.create output_size in
  let (_ : Position.t * Parity.t) =
    String.fold
      s
      ~init:(Position.First_word_start, Parity.Even)
      ~f:(fun (position, parity) -> function
      | '_' ->
        Option.iter separator ~f:(Buffer.add_char buffer);
        Subsequent_word_start, Parity.other parity
      | c ->
        Buffer.add_char
          buffer
          (match (case_for_position ~position ~parity : Case.t) with
           | Upper -> Char.uppercase c
           | Lower -> Char.lowercase c);
        Other, Parity.other parity)
  in
  Buffer.contents buffer
;;

let apply_to_snake_case t s =
  apply_to_snake_case' s ~case_for_position:(case_for_position t) ~separator:(separator t)
;;

let apply_to_words' words ~case_for_position ~separator =
  let num_chars = List.sum (module Int) words ~f:String.length in
  let output_size =
    match separator with
    | None -> num_chars
    | Some _ -> num_chars + List.length words
  in
  let buffer = Buffer.create output_size in
  List.iteri words ~f:(fun idx word ->
    let start_pos : Position.t =
      match idx with
      | 0 -> First_word_start
      | _ ->
        Option.iter separator ~f:(Buffer.add_char buffer);
        Subsequent_word_start
    in
    String.iteri word ~f:(fun idx c ->
      let case =
        match idx with
        | 0 -> case_for_position ~position:start_pos ~parity:Parity.Even
        | _ ->
          let parity =
            match Int.equal (Int.rem idx 2) 0 with
            | true -> Parity.Even
            | false -> Parity.Odd
          in
          case_for_position ~position:Other ~parity
      in
      Buffer.add_char buffer (Case.apply case c)));
  Buffer.contents buffer
;;

let apply_to_words t words =
  apply_to_words' words ~case_for_position:(case_for_position t) ~separator:(separator t)
;;
