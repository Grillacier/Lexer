%{
open Ast
type instruction = Push of char | Pop | Change of char | Reject
                   | Next of (char * instruction) | Top of (char * instruction)
type program = (char * instruction)

let rec nextList n c =
            match n with
            |[] -> []
            |a::b -> Next(c, a)::(nextList b c)

let rec topList t c =
            match t with
            |[] -> []
            |a::b -> Top(c, a)::(topList b c)

let rec print_instr i =
        match i with
        |Push c -> print_string("Push "); print_char(c)
        |Pop -> print_string("Pop")
        |Change c -> print_string("Change "); print_char(c)
        |Reject -> print_string("Reject")
        |Next (c, i) -> print_string("Next "); print_char(c); print_string(" "); print_instr i
        |Top (c, i) -> print_string("Top "); print_char(c); print_string(" "); print_instr i

let rec print_list_instr l =
        match l with
        |[] -> print_string("\n")
        |a::b -> print_instr a; print_string("\n"); print_list_instr b

let rec print_program l =
    match l with
    |[] -> ()
    |(s, instr)::b -> print_char(s); print_string(" "); print_list_instr instr; print_string("\n"); print_program b


let rec pushNext (state:char) (input:char) (stack:char list) (push:char) =
            match stack with
            |[] -> []
            |a::b -> Transition(state, input, a, state, a::[push])::(pushNext state input b push)

let rec popNext (state:char) (input:char) (stack:char list) =
            match stack with
            |[] -> []
            |a::b -> Transition(state, input, a, state, [])::(popNext state input b)

let rec changeNext (state:char) (input:char) (stack:char list) (newState:char) =
            match stack with
            |[] -> []
            |a::b -> Transition(state, input, a, newState, [a])::(changeNext state input b newState)

let rec pushTop (state:char) (input:char list) (stack:char) (push:char) =
            match input with
            |[] -> []
            |a::b -> Transition(state, a, stack, state, stack::[push])::(pushTop state b stack push)

(*let rec popTop (state:char) (input:char list) (stack:char) =
            match input with
            |[] -> [Transition(state, ' ', stack, state, [])]
            |a::b -> (popTop state b stack)*)

let rec changeTop (state:char) (input:char list) (stack:char) (newState:char) =
            match input with
            |[] -> []
            |a::b -> Transition(state, a, stack, newState, [stack])::(changeTop state b stack newState)

type transition = Transition of all * all * all * all * liste
(*transforme une instruction en transition*)
let rec instr2trans (instr:instruction) (state:char) (stack_list:char list) (input_list:char list) =
        match instr with
        |Next (c, i) -> (match i with
                          |Push symbol -> pushNext state c stack_list symbol
                          |Pop -> popNext state c stack_list
                          |Change s -> changeNext state c stack_list s
                          |Reject -> [Transition(state, c, List.hd stack_list, state, stack_list)]
                          |Top (character, inst) -> (match inst with
                                                     | Push symbol2 -> [Transition(state, c, character, state, [symbol2])]
                                                     | Pop -> [Transition(state, c, character, state, [])]
                                                     | Change s -> [Transition(state, c, character, s, [c])]
                                                     |Reject -> [Transition(state, c, List.hd stack_list, state, stack_list)]
                                                     |_ -> failwith "On ne peut pas avoir plus de deux instructions imbriquées")
                          |Next (character, inst) -> failwith("on ne peut pas avoir un next suivi d'un next"))
        |Top (c, i) -> (match i with
                          |Push symbol -> pushTop state input_list c symbol
                          |Pop -> [Transition(state, ' ', c, state, [])]
                          |Change s -> changeTop state input_list c s
                          |Reject -> [Transition(state, List.hd input_list, c, state, stack_list)]
                          |Next (character, inst) -> (match inst with
                                                      |Push symbol2 ->[Transition(state, character, c, state, [symbol2])]
                                                      |Pop -> [Transition(state, character, c, state, [])]
                                                      |Change s -> [Transition(state, character, c, s, [character])]
                                                      |Reject -> [Transition(state, List.hd input_list, c, state, stack_list)]
                                                      |_ -> failwith "On ne peut pas avoir plus de deux instructions imbriquées" )
                          |Top (character, inst) -> failwith("on ne peut pas avoir un top suivi d'un top"))
        |_ -> failwith("ce n'est pas une instruction")

(*transforme une liste d'instructions en liste de listes de transitions*)
let rec list_instr2trans (l:instruction list) (state:char) (stack_list:char list) (input_list:char list) =
        match l with
        |[] -> []
        |a::b -> instr2trans a state stack_list input_list::(list_instr2trans b state stack_list input_list)

(*transforme liste de tuples (state,insruction list) en liste de transitions*)
let rec list_instr_state2trans (l:(char * instruction list) list)  (stack_list:char list) (input_list:char list) =
        match l with
        |[] -> []
        |(s, i)::b -> List.flatten (list_instr2trans i s stack_list input_list)::(list_instr_state2trans b stack_list input_list)


%}

%token COMMA COLONS INP STK STT INIT_STT INIT_STK PROG CASE STATE OF BEGIN NEXT TOP PUSH POP CHANGE REJECT END EOF
%token<char> STATES INPUT STACK


%start<Ast.automate> input


%%

input : a = automate EOF {a}
automate:
INP COLONS inputSymbol = expression_input
STK COLONS stackSymbol = expression_stack
STT COLONS states = expression_state
INIT_STT COLONS initialState = state_init
INIT_STK COLONS initialStack = stack_init PROG COLONS p=prog
                                                       {let trans = List.flatten (list_instr_state2trans p stackSymbol inputSymbol) in

                                                       if(List.length trans = 0) then failwith "aucune transition"
                                                       else Automate(inputSymbol, stackSymbol, states, initialState, initialStack, trans) }

state_init:
x = separated_list(COMMA, STATES)                     {if (List.length x = 1) then
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
x = separated_list(COMMA, STATES)                     {match x with
                                                       | [] -> failwith "no state"
                                                       | a::b -> x
                                                       }

prog:
    CASE STATE OF l=list(states) {l}

states:
    s=STATES COLONS BEGIN CASE a=action END {(s, List.flatten(a))}

action:
    |NEXT OF n=list(nextInstr) {n}
    |TOP OF t=list(topInstr) {t}

nextInstr:
    |i=INPUT COLONS BEGIN CASE TOP OF s=STACK COLONS l=list(instructions) END {nextList (topList l s) i}
    |i=INPUT COLONS inst=instructions {[Next(i, inst)]}

topInstr:
    |s=STACK COLONS BEGIN CASE NEXT OF i=INPUT COLONS l=list(instructions) END {topList (nextList l i) s}
    |s=STACK COLONS i=instructions {[Top(s, i)]}

instructions:
    |PUSH s = STACK {Push(s)}
    |POP {Pop}
    |CHANGE s = STATES {Change(s)}
    |REJECT {Reject}