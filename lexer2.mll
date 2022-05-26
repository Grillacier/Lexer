{
open Parser2
}

let layout = [ ' ' '\t' '\n' ]
let input = ['a'-'z']
let stack_symbol = ['A'-'Z']
let states = ['0'-'9']

rule main = parse
  | layout		                 { main lexbuf } (*string sans les char de layout*)
  | stack_symbol as i            { STACK (i) } (*recupere valeur de i*)
  | states as i                  { STATE (i) }
  | input as i                   { INPUT (i) }
  | ','		                     { COMMA }
  | ":"                          { COLONS }
  | "input symbols"              { INP }
  | "stack symbols"              { STK }
  | "states"                     { STT }
  | "initial state"              { INIT_STT }
  | "initial stack"              { INIT_STK }
  | "program"                    { PROG }
  | "case"                       { CASE }
  | "state"                      { STATE }
  | "of"                         { OF }
  | "begin"                      { BEGIN }
  | "next"                       { NEXT }
  | "push"                       { PUSH }
  | "pop"                        { POP }
  | "change"                     { CHANGE } (*change x : change l'etat en x*)
  | "reject"                     { REJECT } (*arrete execution avec message de refus*)
  | "end"                        { END }
  | eof                          { EOF }
  | _			                 { failwith "unexpected character" }
