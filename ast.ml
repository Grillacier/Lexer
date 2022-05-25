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

let stringTransition (t: transition) =
    match t with
    | Transition(stateEnCours, symboleConsomme, stackConsomme, stateApres, stackAjoute)->
        "(" ^ String.make 1 stateEnCours ^ "," ^ String.make 1 symboleConsomme ^ "," ^ String.make 1 stackConsomme ^ "," ^ String.make 1 stateApres ^ "," ^stringList stackAjoute ^ ")"

let rec transition (c : config) (t: transitionlist) (i:int) =
    match c with
    | Config( stateEnCoursPil, pil, motRestant ) ->
        match t with
            | [] -> failwith "Aucune transition ne s'applique"
            | a::b ->
                match a with
                | Transition ( stateEnCours, symboleConsomme, stackConsomme, stateApres, stackAjoute ) ->
                    if(String.length motRestant = 0 && stateEnCoursPil = stateEnCours && stackConsomme = List.hd pil && symboleConsomme = ' ' ) then
                        Config(stateApres, ajoutPile pil [], motRestant)
                    else if (stateEnCoursPil = stateEnCours && stackConsomme = List.hd pil && String.make 1 symboleConsomme = String.sub motRestant 0 1 ) then
                        Config(stateApres, ajoutPile pil (List.rev stackAjoute), String.sub motRestant 1 (String.length motRestant - 1))
                    else
                        transition c b (i+1)

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
        let config = transition c t 1 in
            lancer config t


let init (m : mot ) (a : automate) =
    match a with
    | Automate (inputSymbols, stackSymbols, states, initialState, initialStack, transi) ->
        Config(initialState, [initialStack], m)
