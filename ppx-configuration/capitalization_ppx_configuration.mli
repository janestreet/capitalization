open! Base
open! Ppxlib

module Single_word : sig
  type t =
    { capitalization : Capitalization.Single_word.t
    ; (* Metadata for errors *)
      arg_location : Location.t
    ; ppx_name : string
    }

  (** Raises a custom message for conditions where single word options are incompatible.

      The error message raised will include potential compatible multi-word options and be
      located at the location of the arg *)
  val raise_incompatible : t -> ('a, Stdlib.Format.formatter, unit, 'a) format4 -> 'b
end

type t =
  | Multi_word of Capitalization.t
  | Single_word of Single_word.t

(** Defines the [~capitalize:"string"] ppx argument.

    - valid multi-word options are listed on error
    - single word options are also considered valid *)
val argument : ppx_name:string -> t option Deriving.Args.param

val argument_name : string

(** Applies the capitalization to an ocaml identifier.

    Raises if the identifier has multiple words and only a single word capitalization was
    given. The location of the error will be the capitalize arg and the error lists
    compatible multi-word capitalization schemes. *)
val apply_to_snake_case_exn : t -> string -> string
