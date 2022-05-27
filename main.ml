let lexbuf = Lexing.from_channel stdin
let ast =
try
	Parser.input Lexer.main lexbuf (*cree automate*)
with
	Parser.Error -> failwith "erreur syntaxique"
let transi = Ast.recupTransitions ast (*cree liste de transitions*)
let () = print_string (Ast.stringTransitionsList (Ast.recupTransitions ast )) (* affiche la liste des transitions, surtout pour la partie 3, pour vérifier les transitions *)
let config = Ast.init Sys.argv.(1) ast (*initialise config avec arg du terminal*)
let _ = Ast.lancer config transi (* executer l'automate sur le mot *)