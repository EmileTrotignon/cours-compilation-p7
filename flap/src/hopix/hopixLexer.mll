{ (* -*- tuareg -*- *)
  open Lexing
  open Error
  open Position
  open HopixParser

  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf
  
  let char_of_string_atom atom =
   " " 

  let error lexbuf =
    error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)

(*
  let unqote s =
    String.sub s 1 (String.length (s - 2))

  let get_char_quoted s =
    let s' = String.sub s 1 (String.length (s - 2))


  let char_of_string_atom atom =
   ' '*)
}

let digit = ['0' - '9']
let lower_case_letter = ['a' - 'z']
let upper_case_letter = ['A' - 'Z']
let letter = lower_case_letter | upper_case_letter
let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']
(*let atom_code = ('\' digit (digit ?)  (digit ?))
let atom_raw = ()*)
let printable = [' ' - '~']
let atom = printable
let string_atom = atom
let char = "'" atom "'"
let lpar = "("
let rpar = ")"
let andand = "&&"
let pipepipe = "||"
let equal_question = "=?" 
let lbrack_equal_question = "<=?"
let rbrack_equal_question = ">=?"
let lbrack_question = "<?"
let rbrack_question = ">?"
let open_com = "/*"
let close_com = "*/"
let dot = "."
let exclamation = "!"
let pipe = "|"
let colon = ":"
let equal = "="
let plus = "+"
let minus = "-"
let star = "*"
let slash = "-"
let langle = "<"
let rangle = ">"
let var_id = lower_case_letter ((letter | digit | '_')*)

let number = '-'? ((digit+) | "0x" ['0' - '9' 'a' - 'f' 'A' - 'F'])

rule comment depth = parse
  | open_com  { comment (depth + 1) lexbuf }
  | close_com { if depth = 0 then 
                  ( token lexbuf )
                else 
                  ( comment (depth  - 1) lexbuf ) }
  | _         { comment depth lexbuf }

and string accumulator = parse
 | "\""        { STRING(String.concat "" (List.map char_of_string_atom (List.rev accumulator))) }
 | string_atom { string ((Lexing.lexeme lexbuf) :: accumulator) lexbuf }

and token = parse
  (** Layout *)
  | newline               { next_line_and token lexbuf }
  | blank+                { token lexbuf }
  | open_com              { comment 0 lexbuf }
  (* char *)
  | "'" printable "'"     { CHAR(Lexing.lexeme_char lexbuf 1)}
  | "\""                  { string [] lexbuf }
  | "let"                 { LET }
  | var_id                { VAR_ID(Lexing.lexeme lexbuf)}
  | number                { INT(Int64.of_int (int_of_string (Lexing.lexeme lexbuf))) }
  (* atomic lexemes *)
  | lpar                  { LPAR }
  | rpar                  { RPAR }
  | pipepipe              { PIPEPIPE }
  | equal_question        { EQUALQUESTION }
  | lbrack_equal_question { LBRACKEQUALQUESTION }
  | rbrack_equal_question { RBRACKEQUALQUESTION }
  | lbrack_question       { LBRACKQUESTION }
  | rbrack_question       { RBRACKQUESTION }
  | dot                   { DOT }
  | exclamation           { EXCLAMATION }
  | pipe                  { PIPE }
  | colon                 { COLON }
  | equal                 { EQUAL }
  | plus                  { PLUS }
  | minus                 { MINUS }
  | star                  { STAR }
  | slash                 { SLASH }
  | langle                { LANGLE }
  | rangle                { RANGLE }
  | eof             { EOF }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }

