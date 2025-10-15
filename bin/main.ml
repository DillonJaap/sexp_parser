(* S-Expression Formal Grammar *)
(* 
  sexpr ::= list | symbol | number
  list ::= "(" list-rest
  list-rest ::= ")" | "." sexpr ")" | sexpr list-rest
  symbol ::= /[^s\]+/
  number ::= /-?[0-9][1-9]*(\.[0-9]+)?/
*)

(* TODO: dot notation *)
open Base
open Angstrom

type sexpr =
  | Number of string
  | Symbol of string
  | List of sexpr list
[@@deriving show]

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

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
     | s when String.is_prefix s ~prefix:"." -> Number ("0" ^ s)
     | s -> Number s)
  <?> "number"
;;

let symbol =
  (take_till @@ fun ch -> Char.is_whitespace ch)
  >>| fun str -> Symbol str
;;

let sexp =
  fix (fun sexp ->
    let list_rest =
      fix (fun list_rest ->
        choice
          [ (* end of list *)
            char ')' *> return []
          ; (* recursive list elements *)
            (let* x = sexp in
             let* xs = list_rest in
             return (x :: xs))
          ])
    in
    let list = char '(' *> list_rest >>| fun xs -> List xs in
    choice [ number; symbol; list ])
;;

(* choice *)
(*   [ char ')' *> List [] *)
(*   ; List (char '.' *> sexp <* char ')') *)
(*   ; List (List.append sexp list_rest) *)
(*   ] *)

let () =
  let open Stdlib.Printf in
  let input = ".1" in
  match parse_string ~consume:All sexp input with
  | Ok result -> printf "%s" (show_sexpr result)
  | Error msg -> printf "%s" msg
;;
