open Type

(* Interface des arbres abstraits *)
module type Ast =
sig
   type expression
   type instruction
   type fonction
   type programme
   type affectable
   type typedef
end


(* *************************************** *)
(* AST après la phase d'analyse syntaxique *)
(* *************************************** *)
module AstSyntax =
struct

(* Opérateurs unaires de Rat *)
type unaire = Numerateur | Denominateur

(* Opérateurs binaires de Rat *)
type binaire = Fraction | Plus | Mult | Equ | Inf

(* ajouté pour la gestion des pointeurs : *)
type affectable = 
  (* déréférencement d'une valeur pointée *)
  | Deref of affectable
  (* Accès à un identifiant *)
  | Ident of string
  (* Accès à un champs d'un enregistrement *)
  | Acces of affectable*string
  
(* Expressions de Rat *)
type expression =
  (* Appel de fonction représenté par le nom de la fonction et la liste des paramètres réels *)
  | AppelFonction of string * expression list
  (* Accès à un identifiant représenté par son nom *)
  (*| Ident of string   supprimé pour la gestion des pointeurs *)
  (* Booléen *)
  | Booleen of bool
  (* Entier *)
  | Entier of int
  (* Opération unaire représentée par l'opérateur et l'opérande *)
  | Unaire of unaire * expression
  (* Opération binaire représentée par l'opérateur, l'opérande gauche et l'opérande droite *)
  | Binaire of binaire * expression * expression
  (* Acces à un affectable (objet qui a une valeur modifiable*)
  | Affectable of affectable  (* ajouté pour la gestion des pointeurs *)
  (* Adresse de la variable *)
  | Adresse of string  (* ajouté pour la gestion des pointeurs *)
  (* initialisation d'un pointeur *)
  | New of typ  (* ajouté pour la gestion des pointeurs *)
  (* Pointeur Null *)
  | Null       (* ajouté pour la gestion des pointeurs *)
  (* Création d'un enregistrement *)
  | CreationEnregistrement of expression list

(* Instructions de Rat *)
type bloc = instruction list
and instruction =
  (* Déclaration de variable représentée par son type, son nom et l'expression d'initialisation *)
  | Declaration of typ * string * expression
  (* Affectation d'une variable représentée par son nom et la nouvelle valeur affectée *)
  | Affectation of affectable * expression   (* modifié pour la gestion des pointeurs *)
  (* Déclaration d'une constante représentée par son nom et sa valeur (entier) *)
  | Constante of string * int
  (* Affichage d'une expression *)
  | Affichage of expression
  (* Conditionnelle représentée par la condition, le bloc then et le bloc else *)
  | Conditionnelle of expression * bloc * bloc
  (*Boucle TantQue représentée par la conditin d'arrêt de la boucle et le bloc d'instructions *)
  | TantQue of expression * bloc
  (* return d'une fonction *)
  | Retour of expression
  (*déclaration d'un nouveau type*)
  | Typedefinst of string * typ
  (*Assignation de l'addition *)
  | Addition of  affectable * expression 

(* Structure des fonctions de Rat *)
(* type de retour - nom - liste des paramètres (association type et nom) - corps de la fonction *)
type fonction = Fonction of typ * string * (typ * string) list * bloc

(* Structure d'un typedef Rat *)
(* nom du type et son type *)
type typedef = Typedef of string * typ

(* Structure d'un programme Rat *)
(* liste de fonction - programme principal *)
type programme = Programme of typedef list * fonction list * bloc


end


(* ********************************************* *)
(* AST après la phase d'analyse des identifiants *)
(* ********************************************* *)
module AstTds =
struct


(* ajouté pour la gestion des pointeurs : *)
type affectable = 
  (* déréférencement d'une valeur pointée *)
  | Deref of affectable
  (* Accès à un identifiant *)
  | Ident of Tds.info_ast
  (* Accès à un champs d'un enregistrement *)
  | Acces of affectable*Tds.info_ast

(* Expressions existantes dans notre langage *)
(* ~ expression de l'AST syntaxique où les noms des identifiants ont été
remplacés par les informations associées aux identificateurs *)
type expression =
  | AppelFonction of Tds.info_ast * expression list
  (*| Ident of Tds.info_ast   supprimé pour la gestion des pointeurs *)
  | Affectable of affectable  (* ajouté pour la gestion des pointeurs *)
  | Adresse of Tds.info_ast  (* le nom de l'identifiant est remplacé par ses informations *) (* ajouté pour la gestion des pointeurs *)
  | New of typ  (* ajouté pour la gestion des pointeurs *)
  | Null       (* ajouté pour la gestion des pointeurs *)
  | Booleen of bool
  | Entier of int
  | Unaire of AstSyntax.unaire * expression
  | Binaire of AstSyntax.binaire * expression * expression
  | CreationEnregistrement of expression list

(* instructions existantes dans notre langage *)
(* ~ instruction de l'AST syntaxique où les noms des identifiants ont été
remplacés par les informations associées aux identificateurs
+ suppression de nœuds (const) *)
type bloc = instruction list
and instruction =
  | Declaration of typ * Tds.info_ast * expression (* le nom de l'identifiant est remplacé par ses informations *)
  (*| Affectation of  Tds.info_ast * expression (* le nom de l'identifiant est remplacé par ses informations *) supprimé pour la gestion des pointeurs *)
  |Affectation of affectable * expression  (* ajouté pour la gestion des pointeurs *)
  | Affichage of expression
  | Conditionnelle of expression * bloc * bloc
  | TantQue of expression * bloc
  | Retour of expression
  | Empty (* les nœuds ayant disparus: Const *)
  | Typedefinst of Tds.info_ast * typ (*ajouté pour la gestion des types nommés*)
  | Addition of  affectable * expression (* ajouté pour l'assignation de l'addition *)


(* Structure des fonctions dans notre langage *)
(* type de retour - informations associées à l'identificateur (dont son nom) - liste des paramètres (association type et information sur les paramètres) - corps de la fonction *)
type fonction = Fonction of typ * Tds.info_ast * (typ * Tds.info_ast ) list * bloc

(* Structure d'un typedef Rat *)
(* nom du type et son type *)
type typedef = Typedef of Tds.info_ast * typ

(* Structure d'un programme Rat *)
(* liste de fonction - programme principal *)
type programme = Programme of typedef list * fonction list * bloc



end


(* ******************************* *)
(* AST après la phase de typage *)
(* ******************************* *)
module AstType =
struct


(* ajouté pour la gestion des pointeurs : *)
type affectable = 
  | Deref of affectable
  | Ident of Tds.info_ast
  | Acces of affectable*Tds.info_ast
  
  
(* Opérateurs unaires de Rat - résolution de la surcharge *)
type unaire = Numerateur | Denominateur

(* Opérateurs binaires existants dans Rat - résolution de la surcharge *)
type binaire = Fraction | PlusInt | PlusRat | MultInt | MultRat | EquInt | EquBool | Inf

(* Expressions existantes dans Rat *)
(* = expression de AstTds *)
type expression =
  | AppelFonction of Tds.info_ast * expression list
  (*| Ident of Tds.info_ast   supprimé pour la gestion des pointeurs *)
  | Affectable of affectable  (* ajouté pour la gestion des pointeurs *)
  | Adresse of Tds.info_ast  (* le nom de l'identifiant est remplacé par ses informations *) (* ajouté pour la gestion des pointeurs *)
  | New of typ  (* ajouté pour la gestion des pointeurs *)
  | Null       (* ajouté pour la gestion des pointeurs *)
  | Booleen of bool
  | Entier of int
  | Unaire of unaire * expression
  | Binaire of binaire * expression * expression
  | CreationEnregistrement of expression list

(* instructions existantes Rat *)
(* = instruction de AstTds + informations associées aux identificateurs, mises à jour *)
(* + résolution de la surcharge de l'affichage *)
type bloc = instruction list
 and instruction =
  | Declaration of Tds.info_ast * expression
  (*| Affectation of  Tds.info_ast * expression (* le nom de l'identifiant est remplacé par ses informations *) supprimé pour la gestion des pointeurs *)
  |Affectation of affectable * expression  (* ajouté pour la gestion des pointeurs *)
  | AffichageInt of expression
  | AffichageRat of expression
  | AffichageBool of expression
  | Conditionnelle of expression * bloc * bloc
  | TantQue of expression * bloc
  | Retour of expression
  | Empty (* les nœuds ayant disparus: Const *)
  | Typedefinst of Tds.info_ast
  | AdditionInt of  affectable * expression (* ajouté pour l'assignation de l'addition*)
  | AdditionRat of  affectable * expression (* ajouté pour l'assignation de l'addition *)

(* informations associées à l'identificateur (dont son nom), liste des paramètres, corps *)
type fonction = Fonction of Tds.info_ast * Tds.info_ast list * bloc

(* Structure d'un typedef Rat *)
(* nom du type et son type *)
type typedef = Typedef of Tds.info_ast

(* Structure d'un programme Rat *)
(* liste de fonction - programme principal *)
type programme = Programme of typedef list * fonction list * bloc


let taille_variables_declarees i =
  match i with
  | Declaration (info,_) ->
    begin
    match Tds.info_ast_to_info info with
    | InfoVar (_,t,_,_) -> getTaille t
    | _ -> failwith "internal error"
    end
  | _ -> 0 ;;

end

(* ******************************* *)
(* AST après la phase de placement *)
(* ******************************* *)
module AstPlacement =
struct
type typenomme =
    | Typedef of Tds.info_ast *typ
(* Expressions existantes dans notre langage *)
(* = expression de AstType  *)
type expression = AstType.expression

(* instructions existantes dans notre langage *)
(* = instructions de AstType  *)
type bloc = instruction list
 and instruction = AstType.instruction

(* informations associées à l'identificateur (dont son nom), liste de paramètres, corps, expression de retour *)
(* Plus besoin de la liste des paramètres mais on la garde pour les tests du placements mémoire *)
type fonction = Fonction of Tds.info_ast * Tds.info_ast list * bloc

(* Structure d'un typedef Rat *)
(* nom du type et son type *)
type typedef = Typedef of Tds.info_ast

(* Structure d'un programme Rat *)
(* liste de fonction - programme principal *)
type programme = Programme of typedef list * fonction list * bloc


end
