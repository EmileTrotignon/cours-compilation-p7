[@@@warning "-50"]
open Lexer
open Parser

(*
    Ce module implémente une boucle interactive pour le langage
    "Marthe", un langage d'expressions arithmétiques avec un
    opérateurs de sommation.

    Pour le compiler, utilisez par exemple la commande :

    ocamlopt -o marthe unix.cmxa marthe.ml

    Vous pouvez aussi l'évaluer pas à pas dans une boucle interactive
    ocaml, liée au module standard Unix. Pour cela, dans le mode
    Tuareg d'emacs, lancez la fonction "Run Caml Toplevel" du menu
    Tuareg avec la commande :

    ocaml unix.cma

*)

(*

   Le langage Marthe est un langage minimaliste dont voici quelques
   exemples de programmes corrects syntaxiquement:

   - "73"
   - "6 * 7"
   - "1 + (2 * 3)"
   - "1 + 2 * 3"
   - "sum (x, 1, 10, x * x)"
   - "sum (x, 1, 10, sum (y, 1, 10, x + y))"
   - "1
      + 2 *         3"
   - "sum (x, 1, 10 * 10, y)"

   Nous allons écrire une boucle interactive incluant un interpréteur
   de Marthe ainsi qu'un compilateur de Marthe vers une machine
   virtuelle minimaliste à pile.

*)

(* Cette fonction implémente une boucle interactive à
  l'aide de trois fonctions:
  - [read ()] demande une chaîne [s] à l'utilisateur ;
  - [eval s] évalue le programme Marthe ;
  - [print r] affiche la valeur résultat de l'évaluation de [s]. *)
let loop read eval print =
  let rec aux () =
    try
      let p = read () in
      let v = eval p in
      print v;
      aux ()
    with exn ->
      (* On utilise le module de la bibliothèque standard de Caml
          pour afficher l'exception. L'échappement ANSI "\033[31m"
          permet d'afficher en rouge l'erreur dans un terminal.
          http://tldp.org/HOWTO/Bash-Prompt-HOWTO/x329.html
      *)
      Printf.printf "\027[31mError:%s\n\027[0m" (Printexc.to_string exn);
      aux ()
  in
  aux ()

(*

   Pour définir la syntaxe d'un langage, on utilise une *grammaire*.

   Une grammaire définit deux choses:
   - un ensemble de mots (les programmes syntaxiquement corrects) ;
   - la structure de ces mots.

   Ainsi, le premier problème à résoudre est de transformer une chaîne
   de caractères (qui est une donnée toute plate, sans structure) en
   un *arbre de syntaxe abstraite* (qui met en avant la structure des
   programmes). Avoir une structure d'arbre permet d'*interpréter* les
   programmes récursivement. Typiquement, on veut évaluer un expression
   de la forme "e₁ + e₂" en évaluant e₁ en un entier n₁ puis e₂ en
   un entier n₂ puis en faisant la somme de n₁ et n₂.

   Voici des exemples de programmes Marthe pour illustrent ces idées:

   * Exemple 1:
   On veut traduire "1 + 2" en "EAdd (EInt 1, EInt 2)".
   La sous-chaîne "1" a été reconnue comme "EInt 1".
   La sous-chaîne "2" a été reconnue comme "EInt 2".
   La sous-chaîne "1 + 2" a été reconnue en "EPlus (EInt 1, EInt 2)".

   * Exemple 2:
   On veut traduire "1 +    2   * 3" en "EAdd (EInt 1, EMul (EInt 2, EInt 3))".
   Le nombre d'espaces n'a pas de signification dans la grammaire du langage
   Marthe. Il y a donc des caractères importants et des caractères à ignorer.
   Nous allons voir que la phase d'analyse lexicale sert à se concentrer sur
   les symboles importants.

   * Exemple 3:
   On veut transformer "1 + 2 * 3" en "EAdd (EInt 1, EMul (EInt 2, EInt 3))".
   Attention, ici, il serait incorrect de traduire "1 + 2 * 3" en
   "EMul (EAdd (EInt 1, EInt 2), EInt 3)" parce que l'on donnerait
   alors la priorité à l'addition devant la multiplication!

   Ces exemples nous apprennent plusieurs choses:

   1. Le type de la fonction "read" doit être quelque chose comme:

                            string → expression

   où "expression" est un type de données arborescent.

   2. L'analyse syntaxique peut être précédée d'une première phase qui
      élimine les caractères non significatifs.

   3. L'analyse syntaxique doit prendre en charge la reconnaissance des
      priorités des opérateurs.

*)

(* [read] transforme une chaîne de caractères d'entrées en un arbre
    de syntaxe abstraite en composant l'analyse lexicale et l'analyse
    syntaxique. *)
(* L'analyse lexixale produit une liste de lexèmes à partir de la
    chaîne de caractères d'entrée. Il s'agit essentiellement d'un
    automate fini implémentée à la main. *)


(* Les arbres de syntaxe abstraite du langage Marthe.

    Sont donnés ici en exemple des chaînes de caractères produisant
    un arbre dont la racine est le constructeur de données de la
    même ligne. *)

(* Un interprète produit la valeur entière correspondante
    à l'évaluation d'un arbre de syntaxe. *)
let interpret : e -> int =
  (* Le paramètre [env] est une liste associative
      contenant la valeur associée aux indices de
      sommation.

      La fonction d'évaluation est définie par
      cas sur la forme de l'arbre. *)
  let rec aux env = function
    (* Pour évaluer une expression de la forme "e1 + e2",
        on évalue [e1] en un entier, on évalue [e2] en
        un autre entier, puis on fait la somme des deux
        entiers. *)
    | EPlus (e1, e2) -> aux env e1 + aux env e2
    | EMinus (e1, e2) -> aux env e1 - aux env e2
    (* Même raisonnement pour la multiplication. *)
    | EMult (e1, e2) -> aux env e1 * aux env e2
    | EDiv (e1, e2) -> aux env e1 / aux env e2
    (* Une expression qui est un entier s'évalue en cet entier. *)
    | EInt x -> x
    (* Pour évaluer une expression de la forme
        "sum (x, start, stop, body)". *)
    | ESum (x, start, stop, body) ->
        (* On évalue [start]. *)
        let vstart = aux env start
        (* On évalue [stop]. *)
        and vstop = aux env stop in
        (* On itère sur toutes les valeurs [i] de
           [start] à [stop] et on accumule les sommes
           intermédiaires dans la variable [accu]. *)
        let rec iter i accu =
          if i > vstop then accu
          else
            (* L'évaluation de [body] se fait dans un
               environnement où l'indice [x] est associé
               à la valeur [i]. *)
            iter (i + 1) (accu + aux ((x, i) :: env) body)
        in
        iter vstart 0
    (* Une expression qui est variable s'évalue en la valeur
        associée à cette variable dans l'environnement. *)
    | EVar x -> List.assoc x env
  in
  aux []

(* En rejoignant toutes les composantes, on obtient une boucle interactive qui
    utilise notre interprète pour évaluer le programme Marthe. *)
let interactive_loop () =
  loop
    (fun () -> read lexer parse)
    interpret
    (fun x -> Printf.printf ":- %d\n" x)

let eval s = interpret (parse (lexer s))

(* Test de l'interprète. *)
let test_interpreter () =
  Printf.printf "-*- Interpreter -*-\n";

  (* Tests positifs. *)
  List.iter
    (do_test valid (fun x -> x) eval)
    [
      "1" --> 1;
      "1 + 1" --> 2;
      "6 * 7" --> 42;
      "1 + 2 * 3" --> 7;
      "sum (i, 1, 10, i)" --> 55;
      "sum (i, 1, 10, sum (j, 1, i, i * j))" --> 1705;
    ];

  (* Tests négatifs. *)
  let fail = 42 in

  List.iter
    (do_test invalid (fun x -> x) eval)
    [ "i" --> fail; "sum (i, 1, 10, j)" --> fail ]

(* Exercice de programmation

    Étendre l'interprète pour traiter la division et la soustraction.
*)

(* Nous allons maintenant définir une compilation des programmes
    marthe vers la machine suivante : *)
type machine = {
  (* Le pointeur de code courant. *)
  mutable pc : int;
  (* Le code est une liste d'instructions. *)
  code : instruction array;
  (* Des emplacements mémoires pour un nombre borné de variables. *)
  variables : int array;
  (* Des accumulateurs pour sommer en itérant sur ces variables. *)
  accumulators : int array;
  (* Une pile d'entiers pour stocker les valeurs intermédiaires. *)
  mutable sp : int;
  stack : int array;
}

and instruction =
  (* Pousse une valeur entière sur la pile. *)
  | Push of int
  (* Dépile la valeur au sommet de la pile. *)
  | Pop
  (* Dépile deux valeurs et empile leur somme. *)
  | Add
  (* Dépile deux valeurs et empile leur produit. *)
  | Mul
  (* Met l'accumulateur numéro i à zéro. *)
  | ResetAccu of int
  (* Pousse l'accumulateur numéro i au sommet de la pile. *)
  | PushAccu of int
  (* Augmente l'accumulateur numéro i de la valeur sur la pile. *)
  | AddAccu of int
  (* Pousse la valeur de la variable i sur pile. *)
  | GetVar of int
  (* Affecte la valeur au sommet de la pile à la variable numéro i. *)
  | SetVar of int
  (* Incrémente à la variable i. *)
  | IncVar of int
  (* Déplace le pointeur de code en i tant que l'indice n'a pas
      atteint sa valeur limite stockée au sommet de la pile. *)
  | Loop of int * int
  (* Arrête la machine et renvoie l'entier au sommet de la pile. *)
  | Halt

(* Initialisation d'une machine virtuelle. *)
let vm_init stack_size nb_variables code : machine =
  {
    pc = 0;
    sp = -1;
    code;
    variables = Array.make nb_variables 0;
    accumulators = Array.make nb_variables 0;
    stack = Array.make stack_size 0;
  }

(* Cette exception sera lancée pour interrompre le
    calcul. *)
exception Exit of int

(* Interprète le code d'une machine en un entier
    (si le code est correct et ne dépasse pas les capacités
    de la machine, en termes de nombre de variables et de
    profondeur de pile). *)
let vm_interpret : machine -> int =
 fun vm ->
  (* Les opérations standards sur les piles. *)
  let top () = vm.stack.(vm.sp) in
  let pop () =
    let x = top () in
    vm.sp <- vm.sp - 1;
    x
  in
  let push x =
    vm.sp <- vm.sp + 1;
    vm.stack.(vm.sp) <- x
  in

  let interpret_instruction = function
    | Push x -> push x
    | Pop -> vm.sp <- vm.sp - 1
    | Add ->
        (* Notez l'optimisation: on travaille en place sur la pile. *)
        vm.stack.(vm.sp - 1) <- vm.stack.(vm.sp - 1) + vm.stack.(vm.sp);
        vm.sp <- vm.sp - 1
    | Mul ->
        vm.stack.(vm.sp - 1) <- vm.stack.(vm.sp - 1) * vm.stack.(vm.sp);
        vm.sp <- vm.sp - 1
    | ResetAccu x -> vm.accumulators.(x) <- 0
    | PushAccu x -> push vm.accumulators.(x)
    | AddAccu x -> vm.accumulators.(x) <- vm.accumulators.(x) + pop ()
    | IncVar x -> vm.variables.(x) <- vm.variables.(x) + 1
    | SetVar x -> vm.variables.(x) <- pop ()
    | GetVar x -> push vm.variables.(x)
    | Loop (v, x) -> if vm.variables.(v) <= top () then vm.pc <- x - 1
    | Halt -> raise (Exit (pop ()))
  in
  try
    while true do
      interpret_instruction vm.code.(vm.pc);
      vm.pc <- vm.pc + 1
    done;
    (* On peut sortir de la boucle précédente uniquement
        si son corps lance une exception. On ne peut donc
        pas atteindre le point de code suivant. Cette branche
        est du code mort. *)
    assert false
  with Exit x -> x

(* Voici une fonction de compilation des arbres de syntaxe
    abstraite vers un code pour la machine précédente. *)
let compile : e -> instruction array =
 fun e ->
  (* [nb_idx] est le dernier indice utilisé pour nommer les
      variables de sommation.

      [variable_idx] est une liste associative des noms de variable
      du code source vers leurs indices associés.

      [pos] est la position courante dans le code machine
      produit.

      La fonction de compilation fait un parcours en profondeur
      de l'arbre de syntaxe abstraite et pour chaque sous-arbre A
      produit un code machine C tel que l'évaluation de C place
      le résultat de l'évaluation du sous-arbre A au sommet de
      la pile de la machine.
  *)
  let rec aux nb_idx variable_idx pos = function
    | EInt x -> (pos + 1, [ Push x ])
    | EPlus (e1, e2) ->
        let pos, instrs_e1 = aux nb_idx variable_idx pos e1 in
        let pos, instrs_e2 = aux nb_idx variable_idx pos e2 in
        (pos + 1, instrs_e1 @ instrs_e2 @ [ Add ])
    | EMinus (_, _) -> failwith "to be implemented"
    | EMult (e1, e2) ->
        let pos, instrs_e1 = aux nb_idx variable_idx pos e1 in
        let pos, instrs_e2 = aux nb_idx variable_idx pos e2 in
        (pos + 1, instrs_e1 @ instrs_e2 @ [ Mul ])
    | EDiv (_, _) -> failwith "to be implemented"
    | ESum (x, start, stop, body) ->
        let variable_idx' = (x, nb_idx + 1) :: variable_idx in
        let nb_idx' = nb_idx + 1 in
        let pos, instrs_start = aux nb_idx variable_idx pos start in
        let pos, set_x = (pos + 1, [ SetVar nb_idx' ]) in
        let pos, instrs_stop = aux nb_idx variable_idx pos stop in
        let pos, init_accu = (pos + 1, [ ResetAccu nb_idx' ]) in
        let pos_body = pos in
        let pos, instrs_body = aux nb_idx' variable_idx' pos body in
        ( pos + 2,
          instrs_start @ set_x @ instrs_stop @ init_accu @ instrs_body
          @ [ AddAccu nb_idx' ]
          @ [ IncVar nb_idx'; Loop (nb_idx', pos_body); Pop; PushAccu nb_idx' ]
        )
    | EVar x -> (pos + 1, [ GetVar (List.assoc x variable_idx) ])
  in
  let _, code = aux 0 [] 0 e in
  Array.of_list (code @ [ Halt ])

let lot_of_variables = 128

let stack_size = 1024

let vm_eval s =
  let vm = vm_init stack_size lot_of_variables (compile (parse (lexer s))) in
  vm_interpret vm

(* Tests du compilateur. *)

let test_compiler () =
  Printf.printf "-*- Compiler -*-\n";

  (* Tests positifs. *)
  let check s = s --> eval s in

  List.iter
    (do_test valid (fun x -> x) vm_eval)
    [
      check "1";
      check "1 + 2";
      check "1 + 2 * 3";
      check "sum (x, 1, 10, x + x)";
      check "1 + sum (x, 10 * 10, 10 * 10 * 10, x * sum (i, x, 2 * x, i + x))";
    ]

let benchmark_interactive_loop () =
  let time f =
    let start = Unix.gettimeofday () in
    let y = f () in
    let stop = Unix.gettimeofday () in
    (stop -. start, y)
  in
  let eval e =
    let interpreted_time, x = time (fun () -> interpret e) in
    let vm = vm_init stack_size lot_of_variables (compile e) in
    let compiled_time, y = time (fun () -> vm_interpret vm) in
    (interpreted_time, compiled_time, x, y)
  in
  let print (interpreted_time, compiled_time, x, y) =
    Printf.printf "(Interpreted: %f, Compiled: %f) :- %d\n" interpreted_time
      compiled_time x;
    if x <> y then Printf.printf "Warning: VM found %d\n" y
  in
  loop (fun () -> read lexer parse) eval print

let test_suite () =
  test_lexer ();
  test_parser ();
  test_interpreter ();
  test_compiler ()

let batch () = Printf.printf "%d\n" (interpret (read lexer parse))

let batch_mode = ref false

let () =
  Arg.parse
    (Arg.align
       [
         ( "-bench",
           Arg.Unit benchmark_interactive_loop,
           " Launch a toplevel that uses several evaluation strategies." );
         ("--", Arg.Set batch_mode, " Only interpret stdin.");
         ("-test", Arg.Unit test_suite, " Launch the test suite of the program.");
       ])
    ignore "marthe [options]";
  if !batch_mode then batch () else interactive_loop ()
