open Core
open Angstrom

type operator =
  | Add
  | Multiply
  | Subtract
  | Divide
[@@deriving show]

let pratt = function
  | Multiply | Divide -> 1
  | Add | Subtract -> 0
;;

type expr =
  | Number of string
  | Operation of operator * expr * expr
  | Paren
[@@deriving show]

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let ws =
  skip_while (function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false)
;;

let token p = ws *> p <* ws

(* number parser *)
let number =
  let* sign = option "" (string "-") in
  let* int_part = take_while1 is_digit in
  let* decimal_part =
    option ""
    @@ let* _ = char '.' in
       let* digits = take_while1 is_digit in
       return ("." ^ digits)
  in
  return (Number (sign ^ int_part ^ decimal_part)) <?> "number"
;;

let operation expr =
  let* lvalue = expr in
  let* op =
    token
    @@ choice
         [ char '+' *> return Add
         ; char '*' *> return Multiply
         ; char '-' *> return Subtract
         ; char '/' *> return Divide
         ]
  in
  let* rvalue = expr in
  return @@ Operation (op, lvalue, rvalue)
;;

let expr = fix @@ fun expr -> choice [ number; operation expr ]

let parse_and_print input =
  match parse_string ~consume:All expr input with
  | Ok result -> Stdio.printf "Parsed: %s\n%!" (show_expr result)
  | Error msg -> Stdio.printf "Error: %s\n%!" msg
;;
