(* Module de la passe de gestion des identifiants *)
module PasseTdsRat : Passe.Passe with type t1 = Ast.AstSyntax.programme and type t2 = Ast.AstTds.programme =
struct

  open Tds
  open Exceptions
  open Ast
  open AstTds
  open Type

  type t1 = Ast.AstSyntax.programme
  type t2 = Ast.AstTds.programme

(* analyse_placement_type : typ ->unit*)
(* Si le type en paramètre est un enregistrement, *)
(*affecte à chacun de ses champs son déplacement par rapport au début de l'enregistrement*)
(* ne fait rien sinon *)
let analyse_placement_type t tds=
  match t with
  |Enregistrement l -> 
    (* on commence par creer la liste des déplacements de chaque champs*)
    let (liste_dep,_) = List.fold_left (fun (deja_places,somme) (type_champs,_) ->
    (deja_places@[somme],somme + (getTaille type_champs)) ) ([],0) l in
    (* on récupère ensuite la liste des info_ast*)
    let liste_ia = List.map (fun n ->
    (match chercherGlobalement tds n with
      |None -> failwith "erreur interne"
      |Some ia -> (match (info_ast_to_info ia) with 
        |InfoChamps(_,_,_,_) -> ia 
        |_ -> failwith "erreur interne"))) (List.map snd l) in
    (*on peut maintenant attribuer chaque déplacement à l'info_ast de son champs*)
    List.fold_right2 (fun ia dep instructions -> modifier_adresse_info_champs dep ia; instructions) liste_ia liste_dep ();
  |_ -> ()


(* analyse_tds_type : typ -> tds -> unit *)
(* Paramètre typ : le type à "analyser" *)
(* Paramètre tds : la tds *)
(* si c'est un enregistrement, ajoute les champs à la tds*)
(* ne fait rien sinon *)
let analyse_tds_type t tds=
  match t with
  |Enregistrement l ->List.fold_right (fun (t_champs,n) q ->
    match chercherLocalement tds n with
    |None ->
      let info = InfoChamps(n,t_champs,t,0) in
      let ia = info_to_info_ast info in
      ajouter tds n ia; q;
    |Some _ ->raise (DoubleDeclaration n)) l ();
      analyse_placement_type (Enregistrement l) tds;
  |_-> ()


(* retrouver_type : typ -> tds -> typ *)
(* Paramètre typ : le type à "analyser" *)
(* Paramètre tds : la tds *)
(* renvoie le même type si ce n'est pas un type nommé *)
(* Si c'est un type nommé, renvoie un type nommé avec son type réel qu'on recherche dans la tds au lieu de Undefined *)
let rec retrouver_type t tds=
  match t with
  |Typenommes(_,n) -> (match (chercherGlobalement tds n) with
    | None -> raise (IdentifiantNonDeclare n)
    | Some ia -> (match (info_ast_to_info ia) with
      |InfoTypedef(_,t3) -> Typenommes(t3,n)
      |_ -> raise (MauvaiseUtilisationIdentifiant n)
      )
    )
  |Enregistrement l -> Enregistrement (List.map (fun (t2,n) -> (retrouver_type t2 tds,n)) l)
  |autre_type -> autre_type


(* analyse_tds_typedef : AstSyntax.typedef -> AstTds.typedef *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : le typedef à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le typedef
en un typedef de type AstTds.typedef *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_typedef tds (AstSyntax.Typedef(n,t))=
  match chercherGlobalement tds n with
  | None ->
      analyse_tds_type t tds;
      let info = InfoTypedef (n,t) in
      (* Création du pointeur sur l'information *)
      let ia = info_to_info_ast info in
      (* Ajout de l'information (pointeur) dans la tds *)
      ajouter tds n ia;
      (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information 
      et l'expression remplacée par l'expression issue de l'analyse *)
      Typedef (ia, retrouver_type t tds) 
  | Some _ ->
      (* L'identifiant est trouvé dans la tds locale, 
      il a donc déjà été déclaré dans le bloc courant *) 
      raise (DoubleDeclaration n)


(* analyse_tds_affectable : AstSyntax.affectable -> AstTds.affectable *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'affectable à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'affectable
en une affectable de type AstTds.affectable *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_affectable tds a =
  match a with
  |AstSyntax.Ident n -> (match (chercherGlobalement tds n) with
   |None -> raise (IdentifiantNonDeclare n)
   |Some ia -> (match info_ast_to_info(ia) with 
   (* L'identifiant est trouvé dans la tds glbale, 
       vérifier la bonne utilisation de l'identifiant et renvoyer
       le nouvel affectable*) 
   |InfoVar (_,_,_,_) -> Ident ia
   |_-> raise (MauvaiseUtilisationIdentifiant n)))
  |AstSyntax.Deref a2 -> Deref (analyse_tds_affectable tds a2)
  |AstSyntax.Acces(a2,n) -> match (chercherGlobalement tds n) with
    |None -> raise (IdentifiantNonDeclare n)
    |Some ia -> (match info_ast_to_info(ia) with
      |InfoChamps(_,_,_,_) -> Acces(analyse_tds_affectable tds a2,ia)
      |_ -> raise (MauvaiseUtilisationIdentifiant n))


(* analyse_tds_expression : AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tds e =  
  match e with
  |AstSyntax.AppelFonction(n,el) -> 
  (match chercherGlobalement tds n with 
      (* L'identifiant n'est pas trouvé dans la tds globale. 
      il n'a donc pas été déclaré avant*)
      |None -> raise (IdentifiantNonDeclare n)
      |Some ia -> match info_ast_to_info(ia) with 
        (* L'identifiant est trouvé dans la tds glbale, 
            vérifier la bonne utilisation de l'identifier et renvoyer
            la nouvelle expression avec le pointeur info ast ainsi que l'analyse de 
            toutes les expressions de la liste el*) 
        |InfoFun (_,_,_)-> AppelFonction(ia,(List.map (analyse_tds_expression tds) el))
        |_ -> raise (MauvaiseUtilisationIdentifiant n))
  |AstSyntax.Null ->Null
  |AstSyntax.New t -> New (retrouver_type t tds)
  |AstSyntax.Affectable a -> (match a with
    | AstSyntax.Ident n -> (match (chercherGlobalement tds n) with
      |None -> raise (IdentifiantNonDeclare n)
      |Some ia -> (match info_ast_to_info(ia) with 
        |InfoConst (_,value) -> Entier value
        |_ -> Affectable (analyse_tds_affectable tds a)) )
    | _ -> Affectable (analyse_tds_affectable tds a))
  |AstSyntax.Adresse n -> (match chercherGlobalement tds n with 
    |None -> raise (IdentifiantNonDeclare n)
    |Some ia -> match info_ast_to_info(ia) with 
    (* L'identifiant est trouvé dans la tds glbale, 
          vérifier la bonne utilisation de l'identifiant et renvoyer
          la nouvelle expression*) 
      |InfoVar (_,_,_,_) -> Adresse ia
      |_ -> raise (MauvaiseUtilisationIdentifiant n ))
  |AstSyntax.Booleen(bool)-> Booleen bool
  |AstSyntax.Entier(int) ->Entier int
  |AstSyntax.Unaire(unaire,exp) -> (Unaire (unaire,analyse_tds_expression tds exp))
  |AstSyntax.Binaire(binaire,exp,expp)->(Binaire(binaire,analyse_tds_expression tds exp,analyse_tds_expression tds expp))
  |AstSyntax.CreationEnregistrement(l) ->CreationEnregistrement(List.map (analyse_tds_expression tds)l)


(* analyse_tds_instruction : AstSyntax.instruction -> tds -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds i =
  match i with
  | AstSyntax.Addition (a,e) ->
    (*analyser l'affectable*)
    let na = analyse_tds_affectable tds a in
    (*analyser l'expression*)
    let ne = analyse_tds_expression tds e in
    (*retourner la nouvelle addition*)
    Addition (na, ne)
  | AstSyntax.Declaration (t, n, e) ->
  analyse_tds_type t tds;
    let nt = retrouver_type t tds in
    begin
      match chercherLocalement tds n with
        | None ->
          (* L'identifiant n'est pas trouvé dans la tds locale, 
          il n'a donc pas été déclaré dans le bloc courant *)
          (* Vérification de la bonne utilisation des identifiants dans l'expression *)
          (* et obtention de l'expression transformée *) 
          let ne = analyse_tds_expression tds e in
          (* Création de l'information associée à l'identfiant *)
          let info = InfoVar (n,nt, 0, "") in
          (* Création du pointeur sur l'information *)
          let ia = info_to_info_ast info in
          (* Ajout de l'information (pointeur) dans la tds *)
          ajouter tds n ia;
          (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information 
          et l'expression remplacée par l'expression issue de l'analyse *)
          Declaration (nt, ia, ne) 
        | Some _ ->
          (* L'identifiant est trouvé dans la tds locale, 
          il a donc déjà été déclaré dans le bloc courant *) 
          raise (DoubleDeclaration n)
    end
  | AstSyntax.Affectation(a,e) -> 
    let na = (analyse_tds_affectable tds a) in
    let ne = (analyse_tds_expression tds e) in
    Affectation(na,ne)
  | AstSyntax.Constante (n,v) -> 
    begin
      match chercherLocalement tds n with
      | None -> 
      (* L'identifiant n'est pas trouvé dans la tds locale, 
      il n'a donc pas été déclaré dans le bloc courant *)
      (* Ajout dans la tds de la constante *)
      ajouter tds n (info_to_info_ast (InfoConst (n,v))); 
      (* Suppression du noeud de déclaration des constantes devenu inutile *)
      Empty
      | Some _ ->
        (* L'identifiant est trouvé dans la tds locale, 
        il a donc déjà été déclaré dans le bloc courant *) 
        raise (DoubleDeclaration n)
    end
  | AstSyntax.Affichage e -> 
    (* Vérification de la bonne utilisation des identifiants dans l'expression *)
    (* et obtention de l'expression transformée *)
    let ne = analyse_tds_expression tds e in
    (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
    Affichage (ne)
  | AstSyntax.Conditionnelle (c,t,e) -> 
    (* Analyse de la condition *)
    let nc = analyse_tds_expression tds c in
    (* Analyse du bloc then *)
    let tast = analyse_tds_bloc tds t in
    (* Analyse du bloc else *)
    let east = analyse_tds_bloc tds e in
    (* Renvoie la nouvelle structure de la conditionnelle *)
    Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) -> 
    (* Analyse de la condition *)
    let nc = analyse_tds_expression tds c in
    (* Analyse du bloc *)
    let bast = analyse_tds_bloc tds b in
    (* Renvoie la nouvelle structure de la boucle *)
    TantQue (nc, bast)
  | AstSyntax.Retour (e) -> 
    (* Analyse de l'expression *)
    let ne = analyse_tds_expression tds e in
    Retour (ne)
  | AstSyntax.Typedefinst (n,t) ->
    let nt = retrouver_type t tds in
    match chercherLocalement tds n with
      | None ->
        let info = InfoTypedef (n,nt) in
        (* Création du pointeur sur l'information *)
        let ia = info_to_info_ast info in
        (* Ajout de l'information (pointeur) dans la tds *)
        ajouter tds n ia;
        (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information 
        et l'expression remplacée par l'expression issue de l'analyse *)
        Typedefinst (ia, nt) 
      | Some _ ->
        (* L'identifiant est trouvé dans la tds locale, 
        il a donc déjà été déclaré dans le bloc courant *) 
        raise (DoubleDeclaration n)

      
(* analyse_tds_bloc : AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc
en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale 
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc 
  Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tdsbloc) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli


(* analyse_tds_parametres : (typ,string)-> (typ,info_ast) *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : le premier paramètre à analyser (son type, string) *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'info
en une info_ast *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_parametres tds (t,n) = 
  (* on retrouve le type réel si c'est un type nommé *)
  let nt = retrouver_type t tds in
  match chercherLocalement tds n with 
  (*la TDS de la fonction n'a pas été trouvé dans la TDS globale*)
  (*construire le pointeur avec comme info une info Var pour le paramètre *)
    |None -> let ia = info_to_info_ast (InfoVar (n,nt,0,"")) in
              (* Ajout dans la tds du paramètre *)
              ajouter tds n ia;
              (*renvoyer le couple t et le pointeur *)
              (nt,ia)
  (*la TDS de la fonction a été trouvée dans la TDS Globale donc déjà déclarée avant *)
    |Some _ -> raise (DoubleDeclaration n)


(* analyse_tds_fonction : AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li))  =
  match chercherGlobalement maintds n with 
  (*la TDS de la fonction n'a pas été trouvé dans la TDS globale*)
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale 
  pointant sur la table du bloc parent maintds*)
  |None -> let tds = creerTDSFille (maintds) in
    let nt = retrouver_type t tds in
    (*analyse des paramètres de la fonction *)
    let ap = List.map (analyse_tds_parametres tds) lp in
    (*extraire de la liste des couples le type des paramètres *)
    let tp = List.map fst lp in
    (*construire le pointeur avec comme info une info Fun *)
    let ia = info_to_info_ast (InfoFun(n,nt,tp)) in
    (* Ajout dans la tds de la fonction *)
    ajouter maintds n ia;
    (*Analyse du bloc de la fonction *)
    let ab=analyse_tds_bloc tds li in
    (*renvoyer de la nouvelle Fonction avec comme paramètres les nouvelles analyses*)
    Fonction(nt,ia,ap,ab)
  (*la TDS de la fonction a été trouvée dans la TDS Globale donc déjà déclarée avant *)
  |Some _ -> raise (DoubleDeclaration n)


(* analyser : AstSyntax.ast -> AstTds.ast *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.ast *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (ltd,fonctions,prog)) =
 (*créer la TDS Mère *)
  let tds = creerTDSMere () in
  (*Analyser les typedefs de la liste ltd une à une *)
  let nltd = List.map (analyse_tds_typedef tds) ltd in
  (*Analyser les fonctions de la liste fonctions une à une *)
  let nlf = List.map (analyse_tds_fonction tds) fonctions in 
  (*Analyser le bloc prog qui est une liste d'insctructions *)
  let nb = analyse_tds_bloc tds prog in
  (*renvoyer le nouveau programme avec les nouvelles analyses *)
  Programme (nltd,nlf,nb)

end
