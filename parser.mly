/* Imports. 
analyseur syntaxique
*/

%{

open Type
open Ast.AstSyntax
%}


%token <int> ENTIER
%token <string> ID
%token RETURN
%token PV
%token AO
%token AF
%token PF
%token PO
%token EQUAL
%token CONST
%token PRINT
%token IF
%token ELSE
%token WHILE
%token BOOL
%token INT
%token RAT
%token CALL 
%token CO
%token CF
%token SLASH
%token NUM
%token DENOM
%token TRUE
%token FALSE
%token PLUS
%token MULT
%token INF
%token EOF
%token NEW  (* ajouté pour la gestion des pointeurs *)
%token NULL  (* ajouté pour la gestion des pointeurs *)
%token ADRESSE  (* ajouté pour la gestion des pointeurs *)
%token ADD  (* ajouté pour l'assignation de l'addition *)
%token <string> TID  (* ajouté pour la gestion des types nommés *)
%token TYPEDEF (*ajouté pour la gestion des types nommés*)
%token POINT (*ajouté pour la gestion des enregistrements*)
%token STRUCT (*ajouté pour la gestion des enregistrements*)

(* Type de l'attribut synthétisé des non-terminaux *)
%type <programme> prog
%type <instruction list> bloc
%type <fonction> fonc
%type <instruction list> is
%type <instruction> i
%type <typ> typ
%type <(typ*string) list> dp
%type <expression> e 
%type <expression list> cp
%type <affectable> a  (* ajouté pour la gestion des pointeurs *)
%type <typedef list> td  (* ajouté pour la gestion des types nommés *)


(* Type et définition de l'axiome *)
%start <Ast.AstSyntax.programme> main

%%

main : lfi = prog EOF     {lfi}

prog :
| ltd = td lf = fonc  lfi = prog   {let (Programme (ltd1,lf1,li))=lfi in (Programme (ltd@ltd1,lf::lf1,li))}
| ID li = bloc                     {Programme ([],[],li)}

fonc : t=typ n=ID PO p=dp PF AO li=is AF {Fonction(t,n,p,li)}

bloc : AO li = is AF      {li}

is :
|                         {[]}
| i1=i li=is              {i1::li}

i :
| t=typ n=ID EQUAL e1=e PV          {Declaration (t,n,e1)}
(*| n=ID EQUAL e1=e PV                {Affectation (n,e1)} supprimé lors de l'ajout des pointeurs *)
| a1=a EQUAL e1=e PV                {Affectation (a1,e1)}  (* ajouté pour la gestion des pointeurs *)
| CONST n=ID EQUAL e=ENTIER PV      {Constante (n,e)}
| PRINT e1=e PV                     {Affichage (e1)}
| IF exp=e li1=bloc ELSE li2=bloc   {Conditionnelle (exp,li1,li2)}
| WHILE exp=e li=bloc               {TantQue (exp,li)}
| RETURN exp=e PV                   {Retour (exp)}
| TYPEDEF n=TID EQUAL t=typ PV      {Typedefinst (n,t)} (*ajouté pour la gestion des types nommés*)
| a2=a ADD expp=e PV                {Addition (a2,expp)} (*ajouté pour la gestion de l'opérateur d'assignation d'addition*)

dp :
|                         {[]}
| t=typ n=ID lp=dp        {(t,n)::lp}

typ :
| BOOL                {Bool}
| INT                 {Int}
| RAT                 {Rat}
| t=typ MULT          {Pointeur t} (* ajouté pour la gestion des pointeurs *)
| n=TID               {Typenommes (Undefined, n)} (*ajouté pour la gestion des types nommés*)
| STRUCT AO dp1=dp AF {Enregistrement dp1} (*ajouté pour la gestion des enregistrements*)


e : 
| CALL n=ID PO lp=cp PF   {AppelFonction (n,lp)}
| CO e1=e SLASH e2=e CF   {Binaire(Fraction,e1,e2)}
(* | n=ID                    {Ident n} supprimé lors de l'ajout des pointeurs *)
| a1=a                    {Affectable a1}  (* ajouté pour la gestion des pointeurs *)
| ADRESSE n=ID            {Adresse n}  (* ajouté pour la gestion des pointeurs *)
| PO NEW t=typ PF         {New t}  (* ajouté pour la gestion des pointeurs *)
| NULL                    {Null}  (* ajouté pour la gestion des pointeurs *)
| TRUE                    {Booleen true}
| FALSE                   {Booleen false}
| e=ENTIER                {Entier e}
| NUM e1=e                {Unaire(Numerateur,e1)}
| DENOM e1=e              {Unaire(Denominateur,e1)}
| PO e1=e PLUS e2=e PF    {Binaire (Plus,e1,e2)}
| PO e1=e MULT e2=e PF    {Binaire (Mult,e1,e2)}
| PO e1=e EQUAL e2=e PF   {Binaire (Equ,e1,e2)}
| PO e1=e INF e2=e PF     {Binaire (Inf,e1,e2)}
| PO exp=e PF             {exp}
| AO cp1=cp AF            {CreationEnregistrement cp1} (*ajouté pour la gestion des enregistrements*)

cp :
|               {[]}
| e1=e le=cp    {e1::le}

(* ajouté pour la gestion des pointeurs :*)
a :
|n=ID                   {Ident n}
|PO MULT a1=a PF        {Deref a1}
|PO a1=a POINT n=ID PF  {Acces (a1,n)} (*ajouté pour la gestion des enregistrements*)

(*ajouté pour la gestion des types nommés*)
td : 
|                                           {[]}
| TYPEDEF n=TID  EQUAL t=typ PV ltd=td      { (Typedef(n,t))::ltd}
