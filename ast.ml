type all = char

type liste = all list (* char list *)

type mot = string

type config = Config of all * liste * mot (* stateEnCours * pile * motRestant *)

type transition = Transition of all * all * all * all * liste (* stateEnCours * symboleConsomme * stackConsomme * stateApres * stackAjoute *)

type transitionlist = transition list

(* inputSymbols * stackSymbols * states * initialState * initialStack transitionList *)
type automate = Automate of liste * liste * liste * all * all * transitionlist

let rec stringList l =
    match l with
    | [] -> ""
    | a::b -> String.make 1 a ^ " " ^ stringList b

let rec stringPil l =
    match l with
    | [] -> ""
    | a::b -> String.make 1 a ^ stringList b

let rec recupTransitions ( a : automate ) =
    match a with
    | Automate ( inputSymbols, stackSymbols, states, initialState, initialStack, transitionList ) ->
        transitionList

let ajoutPile (pil : liste) (ajout : liste) =
    match pil with
    | [] -> failwith "pile vide"
    | a::b -> ajout @ b

let rec stringResteStackAjoute l =
    match l with
    | [] -> ""
    | a::b -> ";" ^ String.make 1 a

let stringStackAjoute l =
    match l with
    | [] -> ""
    | a::b ->
        match b with
        | [] -> String.make 1 a
        | c::d -> String.make 1 a ^ stringResteStackAjoute b

let stringTransition (t: transition) =
    match t with
    | Transition(stateEnCours, symboleConsomme, stackConsomme, stateApres, stackAjoute)->
        "(" ^ String.make 1 stateEnCours ^ "," ^ String.make 1 symboleConsomme ^ "," ^ String.make 1 stackConsomme ^ "," ^ String.make 1 stateApres ^ "," ^ stringStackAjoute stackAjoute ^ ")"

let rec transition (c : config) (t: transitionlist) =
    match c with
    | Config( stateEnCoursPil, pil, motRestant ) ->
        match t with
            | [] -> failwith "Aucune transition ne s'applique"
            | a::b ->
                match a with
                | Transition ( stateEnCours, symboleConsomme, stackConsomme, stateApres, stackAjoute ) ->
                    if (stateEnCoursPil = stateEnCours && stackConsomme = List.hd pil && symboleConsomme = ' ' ) then
                        Config(stateApres, ajoutPile pil (List.rev stackAjoute), motRestant)
                    else if (stateEnCoursPil = stateEnCours && stackConsomme = List.hd pil && (String.make 1 symboleConsomme = String.sub motRestant 0 1)) then
                        Config(stateApres, ajoutPile pil (List.rev stackAjoute), String.sub motRestant 1 (String.length motRestant - 1))
                    else
                        transition c b

let rec lancer (c : config) (t: transitionlist) =
    match c with
    | Config( stateEnCoursPil, pil, motRestant ) ->
        print_string("\n");
        print_string("state : " ^ String.make 1 stateEnCoursPil ^ "\n");
        print_string("pile : " ^ stringPil pil ^ "\n");
        print_string("mot restant : " ^ motRestant ^ "\n\n");
        if(List.length pil = 0 && String.length motRestant > 0) then failwith "Pile vide avec un mot non vide"
        else if(List.length pil > 1 && String.length motRestant = 0) then failwith "Pile non vide avec un mot vide"
        else if(List.length pil = 0 && String.length motRestant = 0) then print_string "Fin, l'automate a bien reconnu le mot\n"
        else
        let config = transition c t in
            lancer config t

let stringErrorTransition ( t : transition ) ( e : string ) =
    "error : " ^ e ^ " dans la transition : " ^ (stringTransition t)

let rec testStack ( stackAjoute : liste ) ( stack : liste ) =
    match stackAjoute with
    | [] -> true
    | a::b ->
        if( List.mem a stack = false ) then false
        else testStack b stack

let rec testTransi (l : transitionlist) (t : transition) (input : liste) (stack : liste) (state : liste) =
    match l with
    | [] -> true
    | a::b ->
        match a with
        | Transition ( stateEnCours, symboleConsomme, stackConsomme, stateApres, stackAjoute ) ->
            match t with
            | Transition ( stateEnCours2, symboleConsomme2, stackConsomme2, stateApres2, stackAjoute2 ) ->
                if(stateEnCours = stateEnCours2 && (symboleConsomme = symboleConsomme2 || symboleConsomme = ' ' || symboleConsomme2 = ' ') && stackConsomme = stackConsomme2) then
                    failwith "non déterministe"
                else if ( List.mem stateEnCours state = false ) then failwith (stringErrorTransition a "Etat en cours (dans la transition) de la transition non reconnu")
                else if ( List.mem symboleConsomme input = false && symboleConsomme != ' ' ) then failwith (stringErrorTransition a "Symbole consommé non reconnu")
                else if ( List.mem stackConsomme stack = false ) then failwith (stringErrorTransition a "Stack consommé non reconnu")
                else if ( List.mem stateApres state = false ) then failwith (stringErrorTransition a "Etat suivant non reconnu")
                else if ( testStack stackAjoute stack = false ) then failwith (stringErrorTransition a "Stack ajoute non reconnu")
                else
                    testTransi b t input stack state

let rec testTransitions(l : transitionlist) (input : liste) (stack : liste) (state : liste) = (* Teste si l'automate est déterministe et que chaque transition respecte l'ensemble des input, stack et state *)
    match l with
    | [] -> true
    | a::b -> testTransi b a input stack state && testTransitions b input stack state

let init (m : mot ) (a : automate) =
    match a with
    | Automate (inputSymbols, stackSymbols, states, initialState, initialStack, transi) ->
        if(List.mem initialState states = false) then failwith "état initial non dans l’ensemble des états"
        else if(List.mem initialStack stackSymbols = false) then failwith "symbole de pile initial non dans l'ensemble des symboles de pile"
        else if(testTransitions transi inputSymbols stackSymbols states = true) then
            Config(initialState, [initialStack], m)
        else
            failwith "quelque chose s'est mal passé"
