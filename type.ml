type typ = Bool | Int | Rat | Undefined | Pointeur of typ | Typenommes of typ*string | Enregistrement of (typ*string) list

let rec string_of_type t = 
  match t with
  | Bool ->  "Bool"
  | Int  ->  "Int"
  | Rat  ->  "Rat"
  | Undefined -> "Undefined"
  | Pointeur t2 -> "Pointeur sur " ^ (string_of_type t2) (*ajouté pour la gestion des pointeurs*)
  | Typenommes(t2,n) ->  n ^ " : " ^ (string_of_type t2) (*ajouté pour la gestion des types nommés*)
  | Enregistrement l ->"{" ^ (List.fold_right (fun (t2,n) texte -> "" ^ n ^ " " ^(string_of_type t2) ^ " " ^texte) l "") ^ " }"

(*modifié pour prendre en compte les pointeurs et les types nommés*)
let rec est_compatible t1 t2 =
  match t1, t2 with
  | Bool, Bool -> true
  | Int, Int -> true
  | Rat, Rat -> true 
  | Undefined, Undefined -> true
  | Pointeur(Undefined), Pointeur(_) -> true
  | Pointeur(_), Pointeur(Undefined) ->true
  | Pointeur t3, Pointeur t4 -> est_compatible t3 t4
  | Typenommes(t3,_), t4 ->est_compatible t3 t4
  | t3, Typenommes(t4,_) -> est_compatible t3 t4
  | Enregistrement l1, Enregistrement l2 -> est_compatible_list (List.map fst l1) (List.map fst l2)
  | _ -> false 
and
est_compatible_list lt1 lt2 =
  try
    List.for_all2 est_compatible lt1 lt2
  with Invalid_argument _ -> false

let%test _ = est_compatible Bool Bool
let%test _ = est_compatible Int Int
let%test _ = est_compatible Rat Rat
let%test _ = not (est_compatible Int Bool)
let%test _ = not (est_compatible Bool Int)
let%test _ = not (est_compatible Int Rat)
let%test _ = not (est_compatible Rat Int)
let%test _ = not (est_compatible Bool Rat)
let%test _ = not (est_compatible Rat Bool)
let%test _ = not (est_compatible Undefined Int)
let%test _ = not (est_compatible Int Undefined)
let%test _ = not (est_compatible Rat Undefined)
let%test _ = not (est_compatible Bool Undefined)
let%test _ = not (est_compatible Undefined Int)
let%test _ = not (est_compatible Undefined Rat)
let%test _ = not (est_compatible Undefined Bool)
(*tests ajoutés*)
let%test _ = not (est_compatible (Pointeur Int) Int)
let%test _ = not (est_compatible Int (Pointeur Int))
let%test _ = not (est_compatible Rat (Pointeur Rat))
let%test _ = not (est_compatible Bool (Pointeur Bool))
let%test _ = not (est_compatible (Pointeur Int) Int)
let%test _ = not (est_compatible (Pointeur Rat) Rat)
let%test _ = not (est_compatible (Pointeur Bool) Bool)
let%test _ = not (est_compatible (Typenommes(Rat, "X")) Bool)
let%test _ = est_compatible (Typenommes(Int, "X")) Int
let%test _ = est_compatible (Typenommes(Int, "X")) (Typenommes(Int, "X"))
let%test _ = not (est_compatible (Pointeur Rat) (Typenommes(Bool, "X")))
let%test _ = not (est_compatible (Pointeur Bool) Bool)
let%test _ = not (est_compatible (Enregistrement([(Bool,"X");(Rat,"Y");(Int,"O")])) (Enregistrement([(Bool,"X");(Bool,"Y");(Int,"O")])))
let%test _ = not (est_compatible (Enregistrement([(Int,"X");(Rat,"Y");(Int,"O")])) (Enregistrement([(Int,"Z");(Bool,"Y");(Int,"O")])))


let%test _ = est_compatible_list [] []
let%test _ = est_compatible_list [Int ; Rat] [Int ; Rat]
let%test _ = est_compatible_list [Bool ; Rat ; Bool] [Bool ; Rat ; Bool]
let%test _ = not (est_compatible_list [Int] [Int ; Rat])
let%test _ = not (est_compatible_list [Int] [Rat ; Int])
let%test _ = not (est_compatible_list [Int ; Rat] [Rat ; Int])
let%test _ = not (est_compatible_list [Bool ; Rat ; Bool] [Bool ; Rat ; Bool ; Int])
(*tests ajoutés*)
let%test _ = est_compatible_list [ (Typenommes(Rat, "X")); Rat] [ (Typenommes(Rat, "X")); Rat]
let%test _ = est_compatible_list [ (Typenommes(Int, "X")); Int] [ (Typenommes(Int, "X")); Int]
let%test _ = est_compatible_list [Rat ; (Typenommes(Pointeur Int, "R")) ; Bool] [Rat ; (Typenommes(Pointeur Int, "R")) ; Bool]
let%test _ = not (est_compatible_list [Bool ; (Typenommes(Pointeur Int, "D")) ; Int] [Bool ; (Typenommes(Pointeur Int, "R")) ; Bool])
let%test _ = est_compatible_list [Bool ; (Typenommes(Pointeur Int, "R")) ; Bool] [Bool ; (Typenommes(Pointeur Int, "R")) ; Bool]

(*modifié pour prendre en compte les pointeurs et les types nommés*)
let rec getTaille t =
  match t with
  | Int -> 1
  | Bool -> 1
  | Rat -> 2
  | Undefined -> 0
  | Pointeur _ -> 1  
  | Typenommes(t2,_) -> getTaille t2 
  | Enregistrement l -> List.fold_right (fun (t_champs,_) somme -> (getTaille t_champs) + somme) l 0

let%test _ = getTaille Int = 1
let%test _ = getTaille Bool = 1
let%test _ = getTaille Rat = 2
(*tests ajoutés*)
let%test _ = getTaille (Pointeur Int) = 1
let%test _ = getTaille (Typenommes(Rat, "X")) = 2
let%test _ = getTaille (Typenommes(Int, "Y")) = 1
let%test _ = getTaille (Typenommes(Bool, "Z")) = 1
let%test _ = getTaille (Typenommes(Pointeur Int, "R")) = 1
let%test _ = getTaille (Enregistrement([(Int,"X")])) = 1
let%test _ = getTaille (Enregistrement([(Rat,"X")])) = 2
let%test _ = getTaille (Enregistrement([(Bool,"X")])) = 1
let%test _ = getTaille (Enregistrement([(Bool,"X");(Rat,"Y");(Int,"O")])) = 4



