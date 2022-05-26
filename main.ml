let lexbuf = Lexing.from_channel stdin

let ast = Parser.input Lexer.main lexbuf

let transi = Ast.recupTransitions ast
let config = Ast.init Sys.argv.(1) ast
let _ = Ast.lancer config transi