open Base
open Angstrom

(* -------------------------------------------------------------------------- *)
(* AST *)
(* -------------------------------------------------------------------------- *)

type sexpr =
  | Number of string
  | Symbol of string
  | Cons of sexpr * sexpr
  | Null
[@@deriving show]

(* -------------------------------------------------------------------------- *)
(* Helpers *)
(* -------------------------------------------------------------------------- *)

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_delim = function
  | ' ' | '\t' | '\n' | '\r' | '(' | ')' -> true
  | _ -> false
;;

let ws =
  skip_while (function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false)
;;

let token p = ws *> p <* ws

(* -------------------------------------------------------------------------- *)
(* Atom parsers *)
(* -------------------------------------------------------------------------- *)
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

let symbol =
  take_while1 (fun ch -> not (is_delim ch))
  >>| (fun str -> Symbol str)
  <?> "symbol"
;;

(* -------------------------------------------------------------------------- *)
(* S-expression Parser *)
(* -------------------------------------------------------------------------- *)
let sexp : sexpr Angstrom.t =
  fix (fun sexp ->
    let list_rest =
      fix (fun list_rest ->
        choice
          [ (* end of list *)
            token (char ')') *> return Null
          ; (* dotted pair *)
            (let* _ = token (char '.') in
             let* tail = sexp in
             let* _ = token (char ')') in
             return tail)
          ; (* recursive list elements *)
            (let* x = token sexp in
             let* xs = list_rest in
             return (Cons (x, xs)))
          ])
    in
    let list = token (char '(') *> list_rest <?> "list" in
    token (choice [ list; number; symbol ]))
  <?> "sexp"
;;

let parse_and_print input =
  match parse_string ~consume:All sexp input with
  | Ok result ->
    Stdio.printf "Parsed: %s\n%!" (show_sexpr result)
  | Error msg -> Stdio.printf "Error: %s\n%!" msg
;;

let () =
  parse_and_print "(+ (1 3))";
  parse_and_print "(a b . c)";
  parse_and_print "(1 (2 3) 4)";
  parse_and_print "(x)";
  parse_and_print "()"
;;
