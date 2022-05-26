let lexbuf = Lexing.from_channel stdin

let ast = Parser.input Lexer.main lexbuf (*cree automate*)

let transi = Ast.recupTransitions ast (*cree automate*)
let config = Ast.init Sys.argv.(1) ast (*initialise config avec arg du terminal*)
let _ = Ast.lancer config transi