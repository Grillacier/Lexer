{
open Parser
}

let layout = [ ' ', '\t', '\n' ]
let input = ['a'-'z']
let stack_symbol = ['A'-'Z']
let state = ['0'-'9']

rule main = parse
  | layout		                 { main lexbuf }
  | stack_symbol                 { STACK (Lexing.lexeme lexbuf) }
  | state+                       { STATE (int_of_string(Lexing.lexeme lexbuf)) }
  | input                        { INPUT (Lexing.lexeme lexbuf) }
  | " "                          { SPACE }
  | "\n"                         { RETOUR }
  | ')'			                 { RPAREN }
  | '('			                 { LPAREN }
  | ','		                     { COMMA }
  | ';'		                     { SEMI }
  | "input symbols : "           { INP }
  | "stack symbols : "           { STK }
  | "states : "                  { STT }
  | "initial state: "            { INIT_STT }
  | "initial stack: "            { INIT_STK }
  | "\ntransitions:"             { TRANSI }
  | _			                 { failwith "unexpected character" }
