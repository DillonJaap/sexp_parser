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

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

(* let is_non_zero = function *)
(*   | '1' .. '9' -> true *)
(*   | _ -> false *)
(* ;; *)

let number =
  let* sign = option "" (string "-") in
  let* int_part = take_while is_digit in
  let* decimal_part =
    option ""
    @@
    let* dec = string "." in
    let* digits = take_while is_digit in
    return (dec ^ digits)
  in
  return
    (match sign ^ int_part ^ decimal_part with
     | s when String.is_prefix s ~prefix:"." -> "0" ^ s
     | s -> s)
  <?> "number"
;;

let _symbol = string
let sexp_parser = number

let () =
  let open Stdlib.Printf in
  let input = ".1" in
  match parse_string ~consume:All sexp_parser input with
  | Ok result -> printf "%s" result
  | Error msg -> printf "%s" msg
;;
