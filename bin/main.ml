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

let sexp_parser =
  let open Char in
  satisfy (fun ch -> ch = 't' || ch = 'a')

let () =
  let open Stdlib.Printf in
  let input = "ta" in
  match parse_string ~consume:All sexp_parser input with
  | Ok result -> printf "%c" result
  | Error msg -> printf "%s" msg
