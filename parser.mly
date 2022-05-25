%{
open Ast
%}

%token RPAREN LPAREN COMMA SEMI INP STK STT INIT_STT INIT_STK TRANSI EOF (* COLON *)
%token<char> STATE INPUT STACK


%start<Ast.automate> input


%%

input : a = automate EOF { a }
automate:
INP inputSymbol = expression_input STK stackSymbol = expression_stack
 STT states = expression_state INIT_STT initialState = state_init
 INIT_STK initialStack = stack_init TRANSI transitions = list(transition)
                                                      {if(List.length transitions = 0) then failwith "aucune transition"
                                                       else Automate(inputSymbol, stackSymbol, states, initialState, initialStack, transitions) }

state_init:
x = separated_list(COMMA, STATE)                       {if (List.length x = 1) then
                                                            match x with
                                                            | [] -> failwith "pas d'état initial"
                                                            | a::b -> a
                                                        else failwith "il ne peut pas y avoir plus de 1 état initial"
                                                        }

stack_init:
x = separated_list(COMMA, STACK)                      {if (List.length x = 1) then
                                                            match x with
                                                            | [] -> failwith "pas de stack initial"
                                                            | a::b -> a
                                                       else failwith "il ne peut pas y avoir plus de 1 état initial"
                                                      }

expression_stack:
x = separated_list(COMMA, STACK)                      {match x with
                                                       | [] -> failwith "no stack"
                                                       | a::b -> x
                                                      }

expression_input:
x = separated_list(COMMA, INPUT)                      {match x with
                                                       | [] -> failwith "no input"
                                                       | a::b -> x
                                                      }

expression_state:
x = separated_list(COMMA, STATE)                      {match x with
                                                       | [] -> failwith "no state"
                                                       | a::b -> x
                                                      }

transition:
LPAREN z1 = STATE COMMA z2 = option(INPUT) COMMA z3 = STACK COMMA z4 = STATE COMMA
z5 = separated_list(SEMI, STACK) RPAREN                          {match z2 with
                                                                  | None -> Transition(z1, ' ', z3, z4, z5)
                                                                  | Some a -> Transition(z1, a, z3, z4, z5)
                                                                 }

