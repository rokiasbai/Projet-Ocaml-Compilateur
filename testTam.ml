
open Compilateur

(* Changer le chemin d'accès du jar. *)
let runtamcmde = "java -jar ../../runtam.jar"
(* let runtamcmde = "java -jar /mnt/n7fs/.../tools/runtam/runtam.jar" *)

(* Execute the TAM code obtained from the rat file and return the ouptut of this code *)
let runtamcode cmde ratfile =
  let tamcode = compiler ratfile in
  let (tamfile, chan) = Filename.open_temp_file "test" ".tam" in
  output_string chan tamcode;
  close_out chan;
  let ic = Unix.open_process_in (cmde ^ " " ^ tamfile) in
  let printed = input_line ic in
  close_in ic;
  Sys.remove tamfile;    (* à commenter si on veut étudier le code TAM. *)
  String.trim printed

(* Compile and run ratfile, then print its output *)
let runtam ratfile =
  print_string (runtamcode runtamcmde ratfile)

(* requires ppx_expect in jbuild, and `opam install ppx_expect` *)
let%expect_test "testprintint" =
  runtam "../../fichiersRat/src-rat-tam-test/testprintint.rat";
  [%expect{| 42 |}]

let%expect_test "testprintbool" =
  runtam "../../fichiersRat/src-rat-tam-test/testprintbool.rat";
  [%expect{| true |}]

let%expect_test "testprintrat" =
   runtam "../../fichiersRat/src-rat-tam-test/testprintrat.rat";
   [%expect{| [4/5] |}]

let%expect_test "testaddint" =
  runtam "../../fichiersRat/src-rat-tam-test/testaddint.rat";
  [%expect{| 42 |}]

let%expect_test "testaddrat" =
  runtam "../../fichiersRat/src-rat-tam-test/testaddrat.rat";
  [%expect{| [7/6] |}]

let%expect_test "testmultint" =
  runtam "../../fichiersRat/src-rat-tam-test/testmultint.rat";
  [%expect{| 440 |}]

let%expect_test "testmultrat" =
  runtam "../../fichiersRat/src-rat-tam-test/testmultrat.rat";
  [%expect{| [14/3] |}]

let%expect_test "testnum" =
  runtam "../../fichiersRat/src-rat-tam-test/testnum.rat";
  [%expect{| 4 |}]

let%expect_test "testdenom" =
  runtam "../../fichiersRat/src-rat-tam-test/testdenom.rat";
  [%expect{| 7 |}]

let%expect_test "testwhile1" =
  runtam "../../fichiersRat/src-rat-tam-test/testwhile1.rat";
  [%expect{| 19 |}]

let%expect_test "testif1" =
  runtam "../../fichiersRat/src-rat-tam-test/testif1.rat";
  [%expect{| 18 |}]

let%expect_test "testif2" =
  runtam "../../fichiersRat/src-rat-tam-test/testif2.rat";
  [%expect{| 21 |}]

let%expect_test "factiter" =
  runtam "../../fichiersRat/src-rat-tam-test/factiter.rat";
  [%expect{| 120 |}]

let%expect_test "complique" =
  runtam "../../fichiersRat/src-rat-tam-test/complique.rat";
  [%expect{| [9/4][27/14][27/16][3/2] |}]

let%expect_test "factfun1" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun1.rat";
  [%expect{| 1 |}]

let%expect_test "factfun2" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun2.rat";
  [%expect{| 7 |}]

let%expect_test "factfun3" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun3.rat";
  [%expect{| 10 |}]

let%expect_test "factfun4" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun4.rat";
  [%expect{| 10 |}]

let%expect_test "factfun5" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun5.rat";
  [%expect{| |}]

let%expect_test "factfun6" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun6.rat";
  [%expect{|truetrue|}]

let%expect_test "factfuns" =
  runtam "../../fichiersRat/src-rat-tam-test/testfuns.rat";
  [%expect{| 28 |}]

let%expect_test "factrec" =
  runtam "../../fichiersRat/src-rat-tam-test/factrec.rat";
  [%expect{| 120 |}]

  
(* tests pour les pointeurs : *)

let%expect_test "exemplesujet" =
  runtam "../../fichiersRat/pointeur/exemplesujet.rat";
  [%expect{| 5 |}] 

let%expect_test "ex2TDpointeur" =
  runtam "../../fichiersRat/pointeur/ex2TDpointeur.rat";
  [%expect{| 4 |}] 

let%expect_test "testadresse1" =
  runtam "../../fichiersRat/pointeur/testadresse1.rat";
  [%expect{| 18 |}]

let%expect_test "testaffectation" =
  runtam "../../fichiersRat/pointeur/testaffectation1.rat";
  [%expect{| 4 |}]

let%expect_test "testpointeurdansexpression" =
  runtam "../../fichiersRat/pointeur/testpointeurdansexpression.rat";
  [%expect{| 4 |}]


(* tests pour les types nommés : *)
let%expect_test "exemple1" =
  runtam "../../fichiersRat/typesnommes/exemple1.rat";
  [%expect{| 3 |}]

  let%expect_test "exemple2" =
  runtam "../../fichiersRat/typesnommes/exemple2.rat";
  [%expect{| 7 |}]

  let%expect_test "typesnommesimbriques" =
  runtam "../../fichiersRat/typesnommes/typesnommesimbriques.rat";
  [%expect{| 4 |}]

(*tests pour l'opération d'assignation d'addition *)
let%expect_test "test" = 
  runtam "../../fichiersRat/assignation-addition/test1.rat";
  [%expect{| [1/1] |}]

let%expect_test "exemplesujetxxx" =
  runtam "../../fichiersRat/assignation-addition/test2.rat";
 [%expect{| 7 |}] 

(* tests pour les enregistrements :*)

let%expect_test "exemple_sujet_avec_types_nommes" =
  runtam "../../fichiersRat/enregistrement/exemple_sujet_avec_types_nommes.rat";
  [%expect{| 4835 |}]

(*let%expect_test "exemple_sujet_types_nommes_imbriques" =
  runtam "../../fichiersRat/enregistrement/exemple_sujet_types_nommes_imbriques.rat";
  [%expect{| 12 |}]*)

let%expect_test "exemple_sujet1" =
  runtam "../../fichiersRat/enregistrement/exemple_sujet1.rat";
  [%expect{| 3[1/4] |}]

(*let%expect_test "type_recursif" =
  runtam "../../fichiersRat/enregistrement/type_recursif.rat";
  [%expect{| 123 |}]*)