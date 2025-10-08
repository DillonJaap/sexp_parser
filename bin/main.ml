(* S-Expression Formal Grammar *)
(* 
  sexpr ::= list | symbol | number
  list ::= "(" list-rest
  list-rest ::= ")" | "." sexpr ")" | sexpr list-rest
  symbol ::= /[^s\]+/
  number ::= /-?[0-9][1-9]*(\.[0-9]+)?/
*)

open Base
open Angstrom

let sexp_parser = many @@ choice [ char 'a'; char 'b'; char 'c' ]

let () =
  let open Stdlib.Printf in
  let input = "abc" in
  match parse_string ~consume:All sexp_parser input with
  | Ok result ->
      printf "%s"
        (List.fold result ~init:"" ~f:(fun acc cur -> acc ^ String.of_char cur))
  | Error msg -> printf "%s" msg
