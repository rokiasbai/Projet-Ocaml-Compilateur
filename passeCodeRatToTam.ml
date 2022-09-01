(* Module de la passe de gestion des identifiants *)
module PasseCodetRatToTam : Passe.Passe with type t1 = Ast.AstPlacement.programme and type t2 = string =
struct

  open Tds
  open Ast
  open Type
  open AstType
  open Code

  type t1 = Ast.AstPlacement.programme
  type t2 = string


(* get_type : Tds.info_ast -> typ *)
(* Paramètre info_ast : l'info de laquelle on veut extraire le type*)
(* renvoie le type contenu dans l'info_ast*)
let get_type infoast = match info_ast_to_info infoast with 
  |InfoVar (_,t,_, _) -> t
  |InfoConst (_, _) -> Int
  |InfoFun (_, t, _ ) -> t
  |InfoTypedef (_,t) -> t
  |InfoChamps(_,t,_,_) -> t


(* get_adresse : Tds.info_ast -> string *)
(* Paramètre info_ast (InfoVar): l'info de la variable de laquelle on veut extraire l'adresse *)
(* renvoie l'adresse de l'info_ast*)
let get_adresse infoast = match info_ast_to_info infoast with
  |InfoVar (_,_,dep, reg) -> string_of_int (dep) ^ "[" ^reg^"]"
  |_-> failwith "erreur interne"


(* getTailleAffectable : AstType.affectable -> int *)
(* Paramètre a : le pointeur *)
(* renvoie la taille du type pointé "final"*)
let rec getTailleAffectable a=
  match a with
  |Deref a2 -> getTailleAffectable a2
  |Ident ia -> getTaille(get_type ia)
  |Acces(_,ia)-> getTaille (get_type ia)


(* analyse_code_typedef :  AstType.typedef  -> string *)
(* Paramètre a : le typedef à encoder *)
(* renvoie le code tam de du typedef *)
let analyse_code_typedef (AstPlacement.Typedef _) =
  ""


(* get_adresse_affecatble :  AstType.affectable  -> int*string *)
(* Paramètre a : le typedef à encoder *)
(* renvoie l'adresse (couple deplacement,registre) de l'affectable *)
let rec get_adresse_affectable a =
  match a with
  |Deref a2 ->get_adresse_affectable a2
  |Ident ia -> (match info_ast_to_info ia with
    |InfoVar(_,_,dep,reg) ->(dep,reg)
    |_ -> failwith "erreur interne")
  |Acces(a2,ia) ->
    let (dep,reg) = get_adresse_affectable a2 in
    (match info_ast_to_info ia with
      | InfoChamps(_,_,_,dep_champs) -> (dep_champs + dep,reg)
      | _ -> failwith "ereur interne"
    )


(* analyse_code_affectable :  AstType.affectable  -> string *)
(* Paramètre a : l'affectable à encoder *)
(* renvoie le code tam de l'affectable a *)
let rec analyse_code_affectable a = match a with
  |Deref a2 -> (analyse_code_affectable a2) ^ "LOADI (1)\n"
  |_ ->
   let (dep,reg) = (get_adresse_affectable a) in
   "LOADA " ^ string_of_int (dep) ^ "[" ^reg^"]\n"


(* analyse_code_expression :  AstType.expression  -> string *)
(* Paramètre e : l'expression à encoder *)
(* renvoie le code tam de l'expression e *)
let rec analyse_code_expression e =
  match e with
  |AstType.AppelFonction(ia,el) -> 
    (* on récupère le nom de la fonctiondans l'info_ast *)
    let nom = (match info_ast_to_info ia with 
      |InfoFun (n, _, _) ->  n
      |_-> failwith "erreur interne" ) in
    (*on analyse les expressions de tous les arguments avant d'appeler la fonction*)
    (List.fold_right (fun exp exps_tam -> (analyse_code_expression exp) ^ exps_tam) el "") ^ "CALL (SB) " ^ nom ^ "\n"
  (*|AstType.Ident ia -> "LOAD (" ^ string_of_int (getTaille(get_type ia)) ^ ") " ^ get_adresse ia ^ "\n" *)
  |AstType.Null -> "LOADL 0\n"
  |AstType.New t -> "LOADL " ^ (string_of_int(getTaille t)) ^ "\nSUBR MAlloc\n"
  |AstType.Affectable a -> (analyse_code_affectable a) ^ "LOADI (" ^ (string_of_int(getTailleAffectable a)) ^ ")\n"
  |AstType.Adresse ia -> "LOADA " ^ get_adresse ia ^ "\n"
  |AstType.Booleen(bool)-> if (bool) then "LOADL 1\n" else "LOADL 0\n" 
  |AstType.Entier(int) ->"LOADL " ^ string_of_int int ^ "\n"
  |AstType.Unaire(unaire,exp) -> (match unaire with 
    |Numerateur ->(analyse_code_expression exp) ^ "POP (0) 1\n"
    |Denominateur ->(analyse_code_expression exp) ^ "POP (1) 1\n") 
  |AstType.Binaire(binaire,exp1,exp2)->
    let codebinaire =
    match binaire with
      |PlusInt -> "SUBR IAdd\n"
      |PlusRat ->"CALL (SB) RAdd\n"
      |MultInt ->"SUBR IMul\n"
      |MultRat ->"CALL (SB) RMul\n"
      |Fraction ->""
      |EquInt -> "SUBR IEq\n" 
      |EquBool -> "SUBR IEq\n" 
      |Inf -> "SUBR ILss\n"
    in analyse_code_expression exp1 ^ analyse_code_expression exp2 ^ codebinaire
  |AstType.CreationEnregistrement l -> List.fold_right (fun exp code_tam ->(analyse_code_expression exp)^code_tam) l ""

(* analyse_code_instruction : AstType.instruction  -> string *)
(* Paramètre e : l'instruction à encoder *)
(* Paramètre tr : le type de retour de la fonction si l'instruction est dans une fonction ou Undefined sinon *)
(* Paramètre tp : la liste des types des paramètres de la fonction si l'instruction est dans une fonction ou [Undefined] sinon *)
(* renvoie le code tam de l'instruction i *)
let rec analyse_code_instruction tr tp i =
  match i with
  | AstType.Declaration (ia, e) -> "PUSH "^ string_of_int (getTaille(get_type ia)) ^ "\n" ^ (analyse_code_expression e) ^ "STORE (" ^string_of_int (getTaille(get_type ia)) ^ ") " ^ get_adresse ia ^ "\n"
  (*| AstType.Affectation (ia,e) -> analyse_code_expression e ^ "STORE (" ^string_of_int (getTaille(get_type ia)) ^ ") " ^ get_adresse ia ^ "\n" *)
  | AstType.Affectation(a,e) -> (analyse_code_expression e) ^ (analyse_code_affectable a) ^ "STOREI (" ^ (string_of_int(getTailleAffectable a)) ^ ")\n"
  | AstType.Retour (e) ->  analyse_code_expression e ^ "RETURN (" ^ string_of_int (getTaille tr) ^ ") " ^ string_of_int (List.fold_right (fun t q -> q + (getTaille t)) tp 0) ^ "\n" (* à compléter/modifier? *)
  | AstType.Empty -> ""
  | AstType.AffichageInt (e) -> analyse_code_expression e ^ "SUBR IOut\n"
  | AstType.AffichageRat (e) -> analyse_code_expression e ^ "CALL (SB) ROut\n"
  | AstType.AffichageBool (e) ->analyse_code_expression e ^ "SUBR BOut\n"
  | AstType.Conditionnelle (e, b1,b2) -> let et1 = getEtiquette() in
    let et2 = getEtiquette() in
    analyse_code_expression e ^ "JUMPIF (0) " ^ et1 ^ "\n" ^ (analyse_code_bloc b1 tr tp) ^ "JUMP " ^ et2 ^ "\n" ^ et1 ^ "\n" ^ (analyse_code_bloc b2 tr tp) ^ et2 ^ "\n"
  | AstType.TantQue (e,b) ->  let et1 = getEtiquette() in
    let et2 = getEtiquette() in
    et1 ^ "\n" ^ analyse_code_expression e ^ "JUMPIF (0) " ^ et2 ^ "\n" ^ (analyse_code_bloc b tr tp) ^ "JUMP " ^ et1 ^ "\n" ^ et2 ^ "\n"
  | AstType.Typedefinst (_) -> "" (*ne retourne rien*)
  | AstType.AdditionInt (a,e) ->  analyse_code_affectable a ^ "LOADI (" ^ (string_of_int(getTailleAffectable a)) ^ ")\n" ^ analyse_code_expression e ^ "SUBR IAdd\n" ^ analyse_code_affectable a ^ "STOREI (" ^ (string_of_int(getTailleAffectable a)) ^ ")\n"
  | AstType.AdditionRat (a,e) ->  analyse_code_affectable a ^ "LOADI (" ^ (string_of_int(getTailleAffectable a)) ^ ")\n" ^ analyse_code_expression e ^ "CALL (SB) RAdd\n" ^ analyse_code_affectable a ^ "STOREI (" ^ (string_of_int(getTailleAffectable a)) ^ ")\n"
  

(* analyse_code_bloc : AstType.bloc  -> string *)
(* Paramètre b : le bloc à encoder *)
(* Paramètre tr : le type de retour de la fonction si c'est le bloc d'une fonction ou Undefined sinon *)
(* Paramètre tp : la liste des types des paramètres de la fonction si c'est le bloc d'une fonction ou [Undefined] sinon *)
(* renvoie le code tam du bloc b *)
and analyse_code_bloc b tr tp  = 
  let code_instructions = (List.fold_right (fun i instructions_tam -> (analyse_code_instruction tr tp i)  ^ instructions_tam) b "")
  in let taille_var_bloc = List.fold_right (fun i somme -> (match i with 
  | AstType.Declaration (ia, _) -> getTaille(get_type ia)
  |_ -> 0)
  +somme) b 0 in 
  code_instructions ^ "POP (0) " ^  string_of_int (taille_var_bloc) ^ "\n"


(* analyse_code_fonction : AstPlacement.Fonction -> string *)
(* Paramètre  AstPlacement.Fonction(ia, _ ,b): la fonction à encoder *)
(* renvoie le code tam de la fonction *)
let analyse_code_fonction (AstPlacement.Fonction(ia, _ ,b)) =
  let (nom, tr, tp) = match info_ast_to_info ia with 
  |InfoFun (n, tr, tp) -> (n,tr,tp)
  |_-> failwith "erreur interne" in 
  nom ^ "\n" ^ (analyse_code_bloc b tr tp) ^ "\nHALT\n"


(* analyser: AstPlacement.Programme -> string *)
(* Paramètre  AstPlacement.Programme(fonctions,b): le programme à encoder *)
(* renvoie le code tam de la fonction *)
let analyser (AstPlacement.Programme (ltd, fonctions,b)) =
  let codefonctions = List.fold_right (fun f fonctions_tam -> (analyse_code_fonction f) ^ "\n" ^ fonctions_tam) fonctions "" in
  let codetypedefs = List.fold_right (fun td typedef_tam -> (analyse_code_typedef td) ^ "\n" ^ typedef_tam) ltd "" in
  getEntete() ^  (codefonctions) ^ (codetypedefs) ^ "\nmain\n" ^ (analyse_code_bloc b Undefined [Undefined] )^ "\nHALT"

end


 