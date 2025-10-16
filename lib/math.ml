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

type token =
  | Number of string
  | Operator of operator
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

let get_token p = ws *> p <* ws

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

let operation =
  let* tk =
    get_token
    @@ choice
         [ char '+' *> return Add
         ; char '*' *> return Multiply
         ; char '-' *> return Subtract
         ; char '/' *> return Divide
         ]
  in
  return (Operator tk)
;;

let expr = many @@ choice [ number; operation ]

let parse_and_print input =
  match parse_string ~consume:All expr input with
  | Ok result ->
    Stdio.printf "Parsed: %s\n%!" (show_token result)
  | Error msg -> Stdio.printf "Error: %s\n%!" msg
;;
