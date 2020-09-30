open HopixAST
open Printf
let fresh_identifier =
  let count = ref (-1) in
  fun () -> incr count; Id ("id" ^ string_of_int !count)



  let string_of_token token =
    HopixParserTokens.(
    match token with
    | LOWERCASE_ID(s) -> sprintf "LOWERCASE_ID(%s)" s
    | UPPERCASE_ID(s) -> sprintf "UPPERCASE_ID(%s)" s
    | CONSTR_ID(s) -> sprintf "CONSTR_ID(%s)" s
    | LABEL_ID(s) -> sprintf "LABEL_ID(%s)" s
    | TYPE_CON(s) -> sprintf "TYPE_CON(%s)" s
    | TYPE_VARIABLE(s) -> sprintf "TYPE_VARIABLE(%s)" s
    | INT(i) -> sprintf "INT(%d)" (Int64.to_int i)
    | CHAR(c) -> sprintf "INT(%c)" c
    | STRING(s) -> sprintf "STRING(%s)" s
    | TYPE -> "TYPE"
    | LET -> "LET"
    | BACKSLASH -> "BACKSLASH"
    | UNDERSCORE -> "UNDERSCORE"
    | AND -> "AND"
    | IF -> "IF"
    | ELSE -> "ELSE"
    | WHILE -> "WHILE"
    | DO -> "DO"
    | FOR -> "FOR"
    | TO -> "TO"
    | SWITCH -> "SWITCH"
    | REF -> "REF"
    | SEMICOLON -> "SEMICOLON"
    | ANDAND -> "ANDAND"
    | PIPEPIPE -> "PIPEPIPE"
    | EQUALQUESTION -> "EQUALQUESTION"
    | LANGLEEQUALQUESTION -> "LANGLEEQUALQUESTION"
    | RANGLEEQUALQUESTION -> "RANGLEEQUALQUESTION"
    | LANGLEQUESTION -> "LANGLEQUESTION"
    | RANGLEQUESTION -> "RANGLEQUESTION"
    | DOT -> "DOT"
    | EXCLAMATION -> "EXCLAMATION"
    | PIPE -> "PIPE"
    | COLON -> "COLON"
    | EQUAL -> "EQUAL"
    | PLUS -> "PLUS"
    | MINUS -> "MINUS"
    | STAR -> "STAR"
    | SLASH -> "SLASH"
    | LANGLE -> "LANGLE"
    | RANGLE -> "RANGLE"
    | ARROW -> "ARROW"
    | COMMA -> "COMMA"
    | LPAR -> "LPAR"
    | RPAR -> "RPAR"
    | LCBRACK -> "LCBRACK"
    | RCBRACK -> "RCBRACK"
    | EOF -> "EOF")