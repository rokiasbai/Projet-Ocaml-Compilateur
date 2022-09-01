(* Module de la passe de gestion des identifiants *)
module PasseTypeRat : Passe.Passe with type t1 = Ast.AstTds.programme and type t2 = Ast.AstType.programme =
struct

  open Tds
  open Exceptions
  open Ast
  open AstType
  open Type

  type t1 = Ast.AstTds.programme
  type t2 = Ast.AstType.programme


(* type_reel : typ -> typ *)
(* Paramètre typ : le type dont on veut obtenir le type réel*)
(* renvoie le type "réel", c'est à dire le même type sans les types nommés qui sont remplacés par le type qu'ils nomment *)
let rec type_reel t =
  match t with
  |Typenommes(t2,_)->type_reel t2
  |Pointeur t2 -> Pointeur (type_reel t2)
  |t2 -> t2
  

(* analyse_type_typedef : AstTds.typedef -> (AstType.typedef *)
(* modifie le type dans le pointeur et *)
(* Erreur si types incohérents et inattendus *)
let analyse_type_typedef (AstTds.Typedef(ia, t)) =
  modifier_type_info t ia;
  let t2=type_reel t in 
  if (est_compatible t t2) then AstType.Typedef (ia) else failwith ("erreur interne")


(* analyse_type_affectable : AstTds.affectable -> (AstType.affectable*typ) *)
(* Paramètre e : l'affectable à analyser *)
(* Vérifie la cohérence des types, supprime la surcharge et tranforme l'affectable
en une affectable de type AstType.affectable *)
(* Erreur si types incohérents et inattendus *)
let rec analyse_type_affectable a = 
  match a with
  |AstTds.Ident ia -> let tia = 
    (*retourne la nouvelle affectable et ajoute le bon type de retour en fonction
    de l'info dans le pointeur*)
    match info_ast_to_info ia with 
    |InfoVar (_,t,_,_) -> t
    |InfoConst (_,_) -> Int
    |_-> failwith "erreur interne"
    in (Ident (ia), tia)
  |AstTds.Deref a2 -> let (na,tp) = (analyse_type_affectable a2) in
    (match tp with
     |Pointeur nt -> (Deref na,nt)
     |_ -> raise (TypePointeurInattendu tp)
    )
  |AstTds.Acces(a2,ia) -> (match info_ast_to_info(ia) with
    |InfoChamps(_,type_champs,type_enregistrement,_) -> 
      let (na,tp) = analyse_type_affectable a2 in
      if est_compatible type_enregistrement tp then
        (Acces(na,ia),type_champs)
      else failwith ((string_of_type type_enregistrement) ^ " ! " ^ (string_of_type tp)) (*raise (TypeInattendu(tp,type_enregistrement))*)
    |_ -> failwith "erreur interne")


(* analyse_type_expression : AstTds.expression -> (AstType.expression*typ) *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la cohérence des types, supprime la surcharge et tranforme l'expression
en une expression de type AstType.expression *)
(* Erreur si types incohérents et inattendus *)
let rec analyse_type_expression e = 
  match e with
  |AstTds.AppelFonction (ia, le) -> 
    (* renvoie liste de (exp,typ) *)
    let ale = List.map (analyse_type_expression) le in 
    (*stocke les types du pointeur info_ast*)
      let (typeparams, typeretour) = (match info_ast_to_info ia with 
        |InfoFun (_,tr,tp) -> (tp,tr)
        |_ -> failwith "erreur interne") in 
        let sale = List.map snd ale in 
        (*vérifie la compatibilité des types et retourne la nouvelle 
        expression et ajoute le type de retour *)
          if (est_compatible_list typeparams (List.map snd ale)) then 
            (AppelFonction (ia,List.map fst ale),typeretour)
          else 
            raise (TypesParametresInattendus(typeparams,sale))
  |AstTds.Null -> (Null,Pointeur(Undefined)) 
  |AstTds.New t ->(New t, Pointeur t)
  |AstTds.Affectable a -> let (na,t) = (analyse_type_affectable a) in (Affectable na,t)
  |AstTds.Adresse ia -> (match (info_ast_to_info ia) with
    |InfoVar (_,t,_,_) -> (Adresse ia, Pointeur t)
    |InfoConst (_,_) -> (Adresse ia, Pointeur Int)
    |_ -> failwith "erreur interne" )
  |AstTds.Booleen(bool)-> (Booleen (bool), Bool)
  |AstTds.Entier(int) -> (Entier (int), Int)
  |AstTds.Unaire(u,e) -> 
  (*analyser l'expression *)
  let (ne,t) = (analyse_type_expression e) in
  (*supprimer la surchage en différenciant le type denominateur du type numérateur*)
    (match u with
      |Numerateur ->if (est_compatible t Rat) then (AstType.Unaire(Numerateur,ne),Int)
      else raise (TypeInattendu(t,Rat))
      |Denominateur ->
        if (est_compatible t Rat) then (AstType.Unaire(Denominateur,ne),Int)
        else raise (TypeInattendu(t,Rat)))
  |AstTds.Binaire(b,e1,e2)->
        (*analyser la 1ère expression *)
        let (ne1,type1) = (analyse_type_expression e1) in
        (*analyser la 2ème expression *)
        let (ne2,type2) = (analyse_type_expression e2) in

        let t1 = type_reel type1 in
        let t2 = type_reel type2 in
        (*supprimer la surchage en différenciant les opérations par leur type*)
    (match b with
      |Plus -> (match t1,t2 with
          |Int,Int -> (Binaire(PlusInt,ne1,ne2), Int)
          |Rat,Rat -> (Binaire(PlusRat,ne1,ne2), Rat)
          |_,_ -> raise (TypeBinaireInattendu(b,t1,t2))
        )
      |Mult -> (match t1,t2 with
          |Int,Int -> (Binaire(MultInt,ne1,ne2), Int)
          |Rat,Rat -> (Binaire(MultRat,ne1,ne2), Rat)
          |_,_ -> raise (TypeBinaireInattendu(b,t1,t2))
          )
      |Equ -> (match (type_reel t1),(type_reel t2) with
          |Int,Int -> (Binaire(EquInt,ne1,ne2), Bool)
          |Bool,Bool -> (Binaire(EquBool,ne1,ne2), Bool)
          |Typenommes(_,n1),Typenommes(_,n2) when n1 = n2 -> (Binaire(EquBool,ne1,ne2), Bool)
          |_,_ ->  raise (TypeBinaireInattendu(b,t1,t2))
          )
      |Inf -> (match t1,t2 with
          |Int,Int -> (Binaire(Inf,ne1,ne2),Bool)
          |_,_ -> raise (TypeBinaireInattendu(b,t1,t2))
          )
      |Fraction -> match t1,t2 with
          |Int,Int ->(Binaire(Fraction,ne1,ne2),Rat)
          |_,_ -> raise (TypeBinaireInattendu(b,t1,t2))
          )
    |CreationEnregistrement l ->let nl = List.map analyse_type_expression l in
      (CreationEnregistrement (List.map fst nl),Enregistrement (List.map (fun (_,t) -> (t,"")) nl ) )
  

(* analyse_tds_instruction : typ -> AstSyntax.instruction -> AstTds.instruction *)
(* Paramètre tf : type de retour de la fonction pour l'instruction Retour *)
(* Paramètre i : l'instruction à analyser *)
(*Vérifie la cohérence des types, supprime la surcharge et tranforme l'instruction
en une instruction de type AstType.instruction *)
(* Erreur si types incohérents et inattendus *)
let rec analyse_type_instruction tf i =
  match i with
  |AstTds.Addition (a, e) ->
    (*analyser les expressions *)
    (let (na,ta)=analyse_type_affectable a in 
    let (ne,te)=analyse_type_expression e in 
    if (est_compatible ta te) then 
      if (est_compatible te Int) then 
        AstType.AdditionInt(na,ne)
      else if (est_compatible te Rat) then 
        AdditionRat(na,ne)
      else failwith("erreur interne")
    else raise (TypeInattendu(te,ta)))
  | AstTds.Declaration (t, ia, e) -> 
    (*modifier le type dans l'info ast *)
    modifier_type_info t ia;
    (*analyser l'expression et vérifier la compatibilité du type de la fonction 
    et le type dans l'info ast *)
    let (ne,te)= analyse_type_expression e in 
    if (te==Undefined) then AstType.Declaration (ia,ne) 
    (*si compatibilité, renvoie la nouvelle instruction avce la nouvelle expression analysée*)
    else if (est_compatible t te) then AstType.Declaration (ia,ne) 
    else raise (TypeInattendu(te, t))
  |AstTds.Affectation (a,e)-> 
    let (ne,te)= analyse_type_expression e in 
    let (ia,tia)= analyse_type_affectable a in 
    if (est_compatible te tia) then AstType.Affectation (ia,ne) else raise (TypeInattendu(te, tia))
  |AstTds.Empty -> AstType.Empty
  |AstTds.Affichage (e) -> 
    (*analyser l'expression *)
    let (ne,te) = analyse_type_expression e in 
    (*renvoie la bonne instruction en fonction du type de l'expression analysée*)
      (match (type_reel te) with 
      |Int -> AffichageInt (ne)
      |Bool ->AffichageBool (ne)
      |Rat->AffichageRat (ne)
      |_-> failwith "erreur interne")
  |AstTds.Conditionnelle (e,b1,b2)-> 
    (*analyser la'expression *)
    let (ne,te)=analyse_type_expression e in 
    (*supprimer la surchage *)
    (match te with 
    |Bool -> (*analyser le premier bloc*)
    let ab1 = analyse_type_bloc tf b1 in 
    (*analyser le deuxieme bloc *)
    let ab2 = analyse_type_bloc tf b2 in AstType.Conditionnelle (ne, ab1,ab2)
    |_ -> raise (TypeInattendu (te,Bool)))
  |AstTds.TantQue (e,b) -> 
    (*analyser l'expression et le bloc et supprimer la surcharge *)
    let (ne,te) = analyse_type_expression e in 
    (match te with 
    |Bool -> let ab = analyse_type_bloc tf b in AstType.TantQue (ne,ab)
    |_ -> raise (TypeInattendu (te, Bool)))
  |AstTds.Retour (exp) -> let (ne,te)= analyse_type_expression exp in 
    (match tf with (*rajouter tf dans paramètres*)
    |None -> raise RetourDansMain
    |Some t -> if (est_compatible t te) then Retour (ne) else raise (TypeInattendu (te,t)))
  |AstTds.Typedefinst(ia, t) -> 
    (*modifier le type dans le pointeur*)
    modifier_type_info t ia;
    (*vérifier que le type existe vraiment*)
    let t2=type_reel t in 
    if (est_compatible t t2) then AstType.Typedefinst (ia) else failwith ("erreur")
  
  
(* analyse_type_bloc : typ -> AstTds.bloc -> AstType.bloc *)
(* Paramètre tff : le type de retour de l'instruction Retour si jamais elle existe dans 
la liste d'instructions (bloc) *)
(* Paramètre li : liste d'instructions à analyser *)
(*Vérifie la cohérence des types, supprime la surcharge et tranforme le bloc
en un bloc de type AstType.bloc *)
(* Erreur si types incohérents et inattendus *)
and analyse_type_bloc tff li =
    (*analyser toutes les instructions du bloc *)
    List.map (analyse_type_instruction tff) li 


(* analyse_type_fonction : AstTds.fonction -> AstType.fonction *)
(* Paramètre : la fonction à analyser *)
(*Vérifie la cohérence des types, supprime la surcharge et tranforme le bloc
en un bloc de type AstType.bloc *)
(* Erreur si types incohérents et inattendus *)
let analyse_type_fonction (AstTds.Fonction(tr, ia, pl,b)) =
  (*Séparer pl en deux:typeparam x et les infos ast y  *)
  let x = (List.map type_reel (List.map fst pl)) in let y = (List.map snd pl) in let ab = analyse_type_bloc (Some tr) b in 
  (*on modifie les types des paramètres de la foncton pour qu'ils corespondent aux types réels*)
  modifier_type_fonction_info tr x ia;
  (*extraire les infos relatives à la fonction de l'info_ast *)
  let (typeparam, typeretour) =match info_ast_to_info ia with 
  |InfoFun (_, t, tp) -> (List.map type_reel tp,t)
  |_-> failwith "erreur interne" in 
  (*modifier les types dans l'info ast *)
  modifier_type_fonction_info tr x ia; 
  (*supprimer la surcharge en vérifiant la cohérence des types et renvoyer la bonne fonction*)
  if(est_compatible_list typeparam x && est_compatible typeretour tr) then 
  AstType.Fonction(ia, y, ab) 
  else if (est_compatible_list typeparam x ) then 
  raise (TypeInattendu (typeretour, tr)) 
  else 
  failwith "erreur interne"


(* analyser : AstTds.ast -> AstType.ast *)
(* Paramètre : le programme à analyser *)
(*Vérifie la cohérence des types, supprime la surcharge et tranforme le bloc
en un bloc de type AstType.bloc *)
(* Erreur si types incohérents et inattendus *)
let analyser (AstTds.Programme(ltd, lf, b)) =
  (*analyser toutes les fonctions du programme *)
  let nlf  = List.map (analyse_type_fonction) lf in 
  (*analyser tous les typedef au début du programme *)
  let nltd = List.map (analyse_type_typedef) ltd in
  (*analyser le bloc càd la liste d'instruction *)
  let ab = analyse_type_bloc None b in
  (*renvoie le programme avec les bonnes analyses *)
  AstType.Programme(nltd, nlf, ab)

end
  