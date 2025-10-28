(* AST Parsing *)

type node =
  | Number of float
  | Infix of
      { operator : string
      ; lvalue : node
      ; rvalue : node
      }
[@@deriving show]

let rec parse_number n = Number (Float.of_string n)

and parse_infix lexer lvalue operator =
  Infix { operator; lvalue; rvalue = parse_expr lexer }

and parse_expr lexer =
  let token, lexer = Math_lexer.advance lexer in
  match token with
  | Math_lexer.Number n -> parse_number n
  | Math_lexer.Operator o -> parse_infix lexer lvalue o
  (* TODO handle parens *)
  | _ -> Number 1.0
;;

