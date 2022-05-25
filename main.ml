let lexbuf = Lexing.from_channel stdin

let ast = Parser.input Lexer.main lexbuf

let transi = Ast.recupTransitions ast
let config = Ast.init "abcbca" ast
let _ = Ast.lancer config transi