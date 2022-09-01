(* Module de la passe de placement mémoire*)
module PassePlacementRat : Passe.Passe with type t1 = Ast.AstType.programme and type t2 = Ast.AstPlacement.programme =
struct

  open Tds
  open Ast
  open Type

  type t1 = Ast.AstType.programme
  type t2 = Ast.AstPlacement.programme


(* get_type : Tds.info_ast -> typ *)
(* Paramètre info_ast : l'info de laquelle on veut extraire le type*)
(* renvoie le type contenu dans l'info_ast*)
let get_type infoast = match info_ast_to_info infoast with 
  |InfoVar (_,t,_, _) -> t
  |InfoConst (_, _) -> Int
  |InfoFun (_, t, _ ) -> t
  |InfoTypedef (_,t) ->t
  |InfoChamps(_,t,_,_)->t


(*analyse_placement_typedef :  AstType.typedef)  -> (AstPlacement.typedef*)
(*Paramètre : le typedef à analyser*)
(*retourne simplement le nouveau typedef sans changement*)
let analyse_placement_typedef (AstType.Typedef ia) =
  AstPlacement.Typedef ia


(* analyse_placement_instruction : int -> string -> AstType.instruction  -> (AstPlacement.instruction*int) *)
(* Paramètre dep : l'adresse ie le déplacement (int) par rapport au registre *)
(* Paramètre reg : le registre *)
(* Paramètre i : l'instruction à analyser *)
(* attribue une zone mémoire pour chaque instruction *)
let rec analyse_placement_instruction dep reg i =
  match i with
  | AstType.Declaration (ia, e) -> let t = getTaille(get_type ia) in
    modifier_adresse_info dep reg ia;
    (AstType.Declaration(ia,e),t+dep) 
  | AstType.Affectation (a,e) -> (AstType.Affectation (a,e),dep)
  | AstType.Retour (e) -> (AstType.Retour (e), dep)
  | AstType.Empty -> (AstType.Empty,dep)
  | AstType.AffichageInt (e) -> (AstType.AffichageInt e,dep)
  | AstType.AffichageRat (e) -> (AstType.AffichageRat e,dep)
  | AstType.AffichageBool (e) -> (AstType.AffichageBool e,dep)
  | AstType.Conditionnelle (e, b1,b2) -> 
    let ab1= analyse_placement_bloc reg dep b1 in let ab2= analyse_placement_bloc reg dep b2 in 
    (AstType.Conditionnelle (e,ab1,ab2), dep)
  | AstType.TantQue (e,b) ->  let ab = analyse_placement_bloc reg dep b in (AstType.TantQue (e, ab), dep)
  | AstType.Typedefinst (ia)-> (AstType.Typedefinst ia,dep)
  | AstType.AdditionInt (a,e) -> (AstType.AdditionInt (a,e),dep)
  | AstType.AdditionRat (a,e) -> (AstType.AdditionRat (a,e),dep)


(* analyse_placement_bloc : string -> int -> AstType.bloc -> AstPlacement.bloc *)
(* Paramètre reg : le registre *)
(* Paramètre dep : le déplacement par rapport au registre *)
(* attribue la mémoire pour toutes les instructions du bloc *)
and analyse_placement_bloc reg dep b = 
  List.fold_right (fun i li ->let (ni, ndep)= analyse_placement_instruction dep reg i in (ni::(analyse_placement_bloc reg ndep li)) ) b [] 


(* analyse_placement_parametres : int -> info_ast list -> AstPlacement.bloc *)
(* Paramètre dep : le déplacement par rapport au registre*)
(* Paramètre ial : la liste des info_ast des paramètres de la fonction*)
(* attribue une zone mémoire à tous les paramètres d'une fonction *)
let rec analyse_placement_parametres dep ialp =
  List.fold_right (fun ia q -> let taille = getTaille(get_type ia) in 
  modifier_adresse_info (dep-taille) "LB" ia; 
  ia::(analyse_placement_parametres (dep-taille) q)) ialp[]


(* analyse_placement_fonction : AstType.Fonction -> AstPlacement.Fonction *)
(* Paramètre  : la fonction à analyser*)
(* attribue les zones mémoire aux variables de la fonction *)
let analyse_placement_fonction (AstType.Fonction(ia, ialp,b))  =
  let np = analyse_placement_parametres 0 (List.rev ialp) in
  let nb = analyse_placement_bloc "LB" (3) b in 
  AstPlacement.Fonction (ia,np, nb)


(* analyse : AstType.Ponction -> AstPlacement.Programme *)
(* Paramètre  : le programmeà analyser*)
(* attribue une zone mémoire à toutes les variables du programme *)
let analyser (AstType.Programme (ltd, fonctions,b)) =
  let nf = List.map analyse_placement_fonction fonctions in 
  let ntd = List.map analyse_placement_typedef ltd in
  let nb = analyse_placement_bloc "SB" 0 b in 
  AstPlacement.Programme (ntd, nf, nb)
  
end
