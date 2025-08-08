open! Base
open! Ppxlib

let raise_invalid_arg message ~loc ~ppx_name ~options =
  let options_name, options = options in
  (* This inserts break hints between the options for much nicer multi-line error
     formatting.

     The compiler seems to do this automatically for error extension nodes but we need to
     do it ourselves when raising located errors, like here. *)
  let options_format =
    Stdlib.Scanf.format_from_string (String.concat options ~sep:",@ ") ""
  in
  Location.raise_errorf
    ~loc
    "@[<v>%s: %(%)@ (@[%s: %(%))@]@]"
    ppx_name
    message
    options_name
    options_format
;;

module Single_word = struct
  type t =
    { capitalization : Capitalization.Single_word.t
    ; (* Metadata for errors *)
      arg_location : Location.t
    ; ppx_name : string
    }

  let potential_options single_word =
    List.filter Capitalization.all ~f:(fun capitalization ->
      [%equal: Capitalization.Single_word.t]
        single_word
        (Capitalization.first_word_behavior capitalization))
  ;;

  let raise_incompatible t message =
    let { capitalization; ppx_name; arg_location } = t in
    let options =
      potential_options capitalization |> List.map ~f:Capitalization.to_string
    in
    raise_invalid_arg
      ~loc:arg_location
      message
      ~ppx_name
      ~options:("potential options", options)
  ;;

  let apply_exn t s =
    match String.mem s '_' with
    | false -> Capitalization.Single_word.apply_to_word t.capitalization s
    | true ->
      raise_incompatible t "capitalize argument must specify multiple word behavior"
  ;;
end

type t =
  | Multi_word of Capitalization.t
  | Single_word of Single_word.t

let of_string_opt { loc; txt } ~ppx_name =
  match Capitalization.of_string txt with
  | x -> Some (Multi_word x)
  | exception _ ->
    (match Capitalization.Single_word.of_string txt with
     | x -> Some (Single_word { capitalization = x; arg_location = loc; ppx_name })
     | exception _ -> None)
;;

let argument_name = "capitalize"

let argument ~ppx_name =
  let open Deriving.Args in
  arg
    argument_name
    (map1 (estring __') ~f:(fun s ->
       match of_string_opt s ~ppx_name with
       | Some x -> x
       | None ->
         let can_be = Lazy.force Capitalization.can_be in
         raise_invalid_arg
           "invalid capitalize argument"
           ~loc:s.loc
           ~ppx_name
           ~options:("can be", can_be)))
;;

let apply_to_snake_case_exn t s =
  match t with
  | Multi_word c -> Capitalization.apply_to_snake_case c s
  | Single_word x -> Single_word.apply_exn x s
;;
