{
open Parser
}

let layout = [ ' ' '\t' '\n' ]
let input = ['a'-'z']
let stack_symbol = ['A'-'Z']
let state = ['0'-'9']

rule main = parse
  | layout		                 { main lexbuf } (*string sans les char de layout*)
  | stack_symbol as i            { STACK (i) }
  | state as i                   { STATE (i) }
  | input as i                   { INPUT (i) }
  | ')'			                 { RPAREN }
  | '('			                 { LPAREN }
  | ','		                     { COMMA }
  | ';'		                     { SEMI }
  | "input symbols:"             { INP }
  | "stack symbols:"             { STK }
  | "states:"                    { STT }
  | "initial state:"             { INIT_STT }
  | "initial stack:"             { INIT_STK }
  | "transitions:"               { TRANSI }
  | eof                          { EOF }
  | _			                 { failwith "unexpected character" }
