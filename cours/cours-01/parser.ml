open Lexer

type e =
  | EInt of int (* Ex: "42", "31"              *)
  | EVar of string (* Ex: "x", "y", "foo"         *)
  | EPlus of e * e (* Ex: "1 + 2", "2 * 3 + 4"    *)
  | EMinus of e * e
  | EMult of e * e (* Ex: "1 * 2", "(1 + 2) * 3"  *)
  | EDiv of e * e
  | ESum of string * e * e * e

(* Ex: "sum (x, 1, 10, x * x)" *)

exception ParseError of string * token

(* On se donne la grammaire suivante pour les arbres de syntaxe
    de Marthe:

    phrase ::= expression EOF

    expression ::=
      term PLUS expression
    | term

    term ::=
      factor STAR term
    | factor

    factor ::=
      INT(x)
    | VAR(x)
    | SUM LPAREN VAR COMMA expression COMMA expression COMMA expression RPAREN
    | LPAREN expression RPAREN

    La fonction [parse] transforme une liste de lexèmes en un arbre du type [e]
    via l'analyse induite par la grammaire.
*)

let parse : token list -> e =
 fun tokens ->
  (* On utilise trois fonctions pour se construire une abstraction
      au-dessus de la liste des lexèmes restant à traiter. À l'aide
      des trois fonctions suivantes, on lit cette liste de gauche
      à droite au fur et à mesure de l'analyse, qui accepte ou non
      ces lexèmes comme étant à une position valide vis-à-vis de
      la grammaire. *)
  let accept, current, next =
    (* En utilisant une référence locale, on s'assure que seules
        les trois fonctions suivantes peuvent modifier la variable
        [token_stream]. *)
    let token_stream = ref tokens in

    (* La fonction [next] supprime le lexème en tête de la liste
        des lexèmes à traiter. *)
    let next () =
      match !token_stream with
      | [] -> raise (ParseError ("No more tokens", EOF))
      | _ :: tokens -> token_stream := tokens
    in

    (* La fonction [current] renvoie le lexème courant. *)
    let current () =
      match !token_stream with [] -> assert false | tok :: _ -> tok
    in

    (* [accept t] vérifie que le lexème courante est [t] et
        passe alors au lexème suivant. *)
    let accept token =
      if current () <> token then raise (ParseError ("Unexpected token", token));
      next ()
    in
    (accept, current, next)
  in

  (* L'analyseur syntaxique suit un algorithme récursif et
      descendant que nous verrons dans une prochaine séance
      de cours.

      Il est défini par 4 fonctions mutuellement récursives
      correspondant à chaque cas de la grammaire définie plus
      haut.
  *)

  (* Une phrase est une expression suivie obligatoirement
      par la fin de l'entrée. *)
  let rec phrase () =
    let e = expression () in
    accept EOF;
    e
  (* Pour analyser une expression, ... *)
  and expression () =
    (* ... on commence par analyser un terme. *)
    let e = term () in
    match current () with
    (* Si ce terme est suivi par un "Plus", on
        est dans la seconde règle de la grammaire,
        on doit donc accepter ce "Plus" et passer à
        la suite pour reconnaître une expression. *)
    | Plus ->
        next ();
        EPlus (e, expression ())
    | Minus ->
      next();
      EMinus(e, expression ())
    (* Dans les autres cas, nous étions dans
        la première règle et nous avons reconnu
        une expression [e]. Le travail est terminé. *)
    | _ -> e
  (* Pour analyser un terme, on suit le même schéma que pour
      les expressions. *)
  and term () =
    let t = factor () in
    match current () with
    | Star ->
        next ();
        EMult (t, term ())
    | Slash ->
      next ();
        EDiv (t, term ())
    | _ -> t
  (* Pour décider dans quelle règle on se trouve, ... *)
  and factor () =
    (* on commence par observer le lexème courant. *)
    match current () with
    (* C'est une parenthèse ouvrante ? C'est la règle 4. *)
    | Lparen ->
        next ();
        (* On doit reconnaître une expression ... *)
        let e = expression () in
        (* ... suivie d'une parenthèse fermante. *)
        accept Rparen;
        e
    (* C'est le mot-clé "sum" ? C'est la règle 3. *)
    | Sum ->
        next ();
        (* On attend une parenthèse ouvrante. *)
        accept Lparen;
        (* Puis, un identificateur. *)
        let id =
          match current () with
          | Id s ->
              next ();
              s
          | token -> raise (ParseError ("Expecting an identifier", token))
        in
        (* Une virgule. *)
        accept Comma;
        (* L'expression correspondante à l'initialisation de la variable
            de sommation. *)
        let start = expression () in
        (* Une virgule. *)
        accept Comma;
        (* L'expression correspondante à la valeur finale de la variable
            de sommation. *)
        let stop = expression () in
        (* Une virgule. *)
        accept Comma;
        (* L'expression correspondante au corps de la sommation. *)
        let body = expression () in
        (* Et enfin, une parenthèse fermante. *)
        accept Rparen;
        ESum (id, start, stop, body)
    (* C'est un identificateur ? C'est la règle 2. *)
    | Id x ->
        next ();
        EVar x
    (* C'est un entier ? C'est la règle 1. *)
    | Int x ->
        next ();
        EInt x
    (* Les autres cas sont des cas d'erreur. *)
    | token -> raise (ParseError ("Unexpected token", token))
  in

  phrase ()

let test_parser () =
  Printf.printf "-*- Parser -*-\n";
  let display_tokens t = String.concat " " (List.map string_of_token t) in
  (* Tests positifs. *)
  List.iter
    (do_test valid display_tokens parse)
    [
      [ Int 1; EOF ] --> EInt 1;
      [ Int 1; Plus; Int 41; EOF ] --> EPlus (EInt 1, EInt 41);
      [ Int 1; Star; Int 41; EOF ] --> EMult (EInt 1, EInt 41);
      lexer "1 + 2 * 3" --> EPlus (EInt 1, EMult (EInt 2, EInt 3));
      lexer "1 * 2 + 3" --> EPlus (EMult (EInt 1, EInt 2), EInt 3);
      lexer "sum (x, 1, 2, x * x)"
      --> ESum ("x", EInt 1, EInt 2, EMult (EVar "x", EVar "x"));
    ];

  (* Tests négatifs. *)

  (* Une valeur bidon de type [e]. *)
  let fail = EInt (-42) in

  List.iter
    (do_test invalid display_tokens parse)
    [
      [ EOF ] --> fail;
      lexer "1 + 2 *" --> fail;
      lexer "1 * (2)) + 3" --> fail;
      lexer "sum (x, 1, 2, x * x" --> fail;
    ]

(* Exercices de programmation

    Étendre l'analyse syntaxique pour intégrer
    la division et la soustraction. Comment reconnaissez-vous
    2 - 3 - 4 ? Comme "(2 - 3) - 4", ce qui est correct ou
    plutôt comme "2 - (3 - 4)", ce qui est incorrect ?
*)
