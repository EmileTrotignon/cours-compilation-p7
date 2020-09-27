

let read lexer parse =
  Printf.printf "marthe> %!";
  let s = input_line stdin in
  let tokens = lexer s in
  parse tokens

(* Les lexèmes, aussi appelés "terminaux", sur lesquels
    la grammaire du langage est définie. *)
type token =
  | Int of int (* Ex: "42", "0", "231", ...  *)
  | Id of string (* Ex: "x", "abc", "foo"      *)
  | Sum (* "sum" *)
  | Plus (* "+" *)
  | Minus (* "-" *)
  | Star (* "*" *)
  | Slash (* "/" *)
  | Lparen (* "(" *)
  | Rparen (* ")" *)
  | Comma (* "," *)
  | EOF

(* La fin de l'entrée.        *)

let string_of_token = function
  | Int x -> "Int(" ^ string_of_int x ^ ")"
  | Id x -> "Id(" ^ x ^ ")"
  | Sum -> "Sum"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Star -> "Star"
  | Slash -> "Slash"
  | Lparen -> "Lparen"
  | Rparen -> "Rparen"
  | Comma -> "Comma"
  | EOF -> "EOF"

exception LexingError of string

let lexer : string -> token list =
  fun s ->
   let at_the_end i = i >= String.length s in
   let almost_at_the_end i = i + 1 >= String.length s in
 
   (* Itère sur la chaîne en partant de l'indice [start],
       et avance tant que le caractère [c] est tel que
       [char_class c = true]. *)
   let word char_class =
     let rec aux start i =
       let return stop = (String.sub s start (i - start), stop) in
       if at_the_end i then return (i + 1)
       else if char_class s.[i] then aux start (i + 1)
       else return i
     in
     fun start -> aux start start
   in
 
   (* Les classes de caractères. *)
   let is_digit c = c >= '0' && c <= '9' in
   let is_letter c = c >= 'a' && c <= 'z' in
   (* Les mots sur ces classes de caractères. *)
   let number = word is_digit in
   let identifier = word is_letter in
 
   (* La fonction récursive suivante itère sur la chaîne
       à partir de [i] et tente de reconnaître un lexème. *)
   let rec aux i =
     (* Par défaut, pour continuer sur le caractère suivant, on augmente
         l'indice et on fait un appel récursif. Dans certains cas,
         l'indice [where] est fourni. *)
     let continue ?(where = i + 1) () = aux where in
 
     (* Pour retourner un lexème reconnu, on le met en tête
         de la liste des tokens produite par les appels récursifs. *)
     let produce_and_continue ?where token = token :: continue ?where () in
 
     let rec continue_comment i =
       if almost_at_the_end i then
         raise (LexingError "Unclosed comment")
       else if s.[i] = '*' && s.[i + 1] = ')' then aux (i + 2)
       else continue_comment (i + 1)
     in
 
     if at_the_end i then (* Le lexème EOF marque la fin de l'entrée. *)
       [ EOF ]
     else
       (* Sinon, on peut décider quel lexème essayer de reconnaître
           à l'aide du premier caractère croisé. *)
       match s.[i] with
       (* On saute les espaces. *)
       | ' ' | '\t' -> continue ()
       (* Les symboles. *)
       | '*' -> produce_and_continue Star
       | '+' -> produce_and_continue Plus
       | '-' -> produce_and_continue Minus
       | '/' -> produce_and_continue Slash
       | '(' ->
           if (not (almost_at_the_end i)) && s.[i + 1] = '*' then
             continue_comment (i + 1)
           else produce_and_continue Lparen
       | ')' -> produce_and_continue Rparen
       | ',' -> produce_and_continue Comma
       (* Les nombres. *)
       | c when is_digit c ->
           let n, eo_num = number i in
           (* [i] est l'indice du dernier caractère du nombre
               reconnu. *)
           produce_and_continue ~where:eo_num (Int (int_of_string n))
       (* Les identificateurs. *)
       | c when is_letter c ->
           let s, eo_id = identifier i in
           (* [i] est l'indice du dernier caractère de
               l'identificateur reconnu. *)
           produce_and_continue ~where:eo_id (if s = "sum" then Sum else Id s)
       (* Sinon, le caractère n'est pas accepté par le lexeur. *)
       | _ -> raise (LexingError "Invalid character")
   in
   aux 0
 
 (* Tests de l'analyseur lexical. *)
 let test_title s =
   let max_test_title_len = 30 in
   let s = String.escaped s in
   if String.length s > max_test_title_len then
     String.sub s 0 max_test_title_len ^ "..."
   else s
 
 let ok s = Printf.printf "\027[1;32m[OK] `%s'\027[0m\n" (test_title s)
 
 let ko s = Printf.printf "\027[1;31m[KO] `%s'\027[0m\n" (test_title s)
 
 let ( --> ) input output = (input, output)
 
 let do_test positivity display test (input, expected) =
   try
     if positivity (test input = expected) then ok (display input)
     else ko (display input)
   with _ -> if positivity true then ko (display input) else ok (display input)
 
 let valid x = x
 
 let invalid x = not x
 
 let test_lexer () =
   Printf.printf "-*- Lexer -*-\n";
   (* Tests positifs. *)
   List.iter
     (do_test valid (fun s -> s) lexer)
     [
       "1" --> [ Int 1; EOF ];
       "42" --> [ Int 42; EOF ];
       "231" --> [ Int 231; EOF ];
       "+" --> [ Plus; EOF ];
       "*" --> [ Star; EOF ];
       "(" --> [ Lparen; EOF ];
       ")" --> [ Rparen; EOF ];
       "," --> [ Comma; EOF ];
       "sum" --> [ Sum; EOF ];
       "a" --> [ Id "a"; EOF ];
       "sumx" --> [ Id "sumx"; EOF ];
       "(  )" --> [ Lparen; Rparen; EOF ];
       "()" --> [ Lparen; Rparen; EOF ];
       "42," --> [ Int 42; Comma; EOF ];
       "" --> [ EOF ];
     ];
 
   (* Tests négatifs. *)
   List.iter
     (do_test invalid (fun s -> s) lexer)
     [ "#" --> []; "!" --> []; "\n" --> [] ]
 
 (* Exercices de programmation:
 
     Étendre l'analyse lexicale, pour
     1. Ignorer les tabulations ;
     2. Rajouter la gestion des symboles '-' et '/' ;
     3. Ignorer des commentaires écrits entre '(*' et '*)'.
 
 *)