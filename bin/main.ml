open Lib.Sexpr

let () =
  parse_and_print "(+ (1 3))";
  parse_and_print "(a b . c)";
  parse_and_print "(1 (2 3) 4)";
  parse_and_print "(x)";
  parse_and_print "()"
;;
