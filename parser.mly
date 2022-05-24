%{
open Ast
%}

%token STACK INPUT INPUT SPACE RETOUR RPAREN LPAREN COMMA SEMI INP STK STT INIT_STT INIT_STK TRANSI EOF

%start<Ast.automate> input


%%

input : a = automate EOF { a }
automate:
INP x1 = expression_input RETOUR STK x2 = expression_stack RETOUR
 STT x3 = expression_state RETOUR INIT_STT x4 = INPUT RETOUR
 INIT_STK x5 = STACK RETOUR TRANSI                          { Before(x1, x2, x3, x4, x5) }

expression_stack:
x = STACK                                                   { Var(x) }
| l = expression_stack COMMA SPACE r = expression_stack     { Stack(l, r) }
| l = expression_stack COMMA r = expression_stack           { Stack(l, r) }

expression_input:
x = INPUT                                                   { Var(x) }
| l = expression_input COMMA SPACE r = expression_input     { Input(l, r) }
| l = expression_input COMMA r = expression_input           { Input(l, r) }

expression_state:
x = STATE                                                   { Int(x) }
| l = expression_state COMMA SPACE r = expression_state     { State(l, r) }
| l = expression_state COMMA r = expression_state           { State(l, r) }

expression_transi:
| LPAREN etape_avant = STATE COMMA  input = input_final COMMA
 avant = STACK COMMA etape_apres = STATE COMMA apres = push { Transi(etape_avant, input, avant, etape_apres, apres) }

input_final:
x = INPUT                                                   { Input_finale (x) }
| x = ""                                                    { Input_finale (x) }

push:
x = STACK                                                   { Var(x) }
| x = STACK SEMI x2 = STACK                                 { Pile(x, x2) }


expression:
x=ID  { Var x }
| l=expression COMMA r=expression { Or (l, r) }