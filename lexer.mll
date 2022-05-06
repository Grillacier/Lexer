{
open Parser
}

let espaces = [ ' ' '\t' '\n' ]
let lettre = ['0'-'9''a'-'z''A'-'Z']

rule main = parse
  | lettre+		{ ID (Lexing.lexeme lexbuf) }
  | espaces		{ main lexbuf }
  | ','		    { COMMA }
  | ':'		    { COLONS }
  | ';'		    { SEMICOLONS }
  | "input"		{ INPUT }
  | "symbols"	{ SYMBOLS }
  | "states"    { STATES }
  | "initial"	{ INITIAL }
  | "stack"	    { STACK }
  | "state"	    { STATE }
  | "states"	{ STATES }
  | "transitions" { TRANSITIONS }
  | _			{ failwith "unexpected character" }
