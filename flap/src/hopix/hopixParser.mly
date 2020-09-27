%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position


  let add_par s = 
    "(" ^ s ^ ")"

  let binop_name binop =
    Id(add_par 
        (match binop with
        | PLUS                -> "+"
        | MINUS               -> "-"
        | STAR                -> "*"
        | SLASH               -> "/"
        | EQUALQUESTION       -> "=?"
        | LBRACKEQUALQUESTION -> "<=?"
        | RBRACKEQUALQUESTION -> ">=?"
        | LBRACKQUESTION      -> "<?"
        | RBRACKQUESTION      -> ">?"
        | ANDAND              -> "&&"
        | PIPEPIPE            -> "||"
        | _                   -> failwith "not a binop (should never be reached no matter user input)"))
%}

%token<string> VAR_ID
%token<string> CONSTR_ID
%token<string> LABEL_ID
%token<string> TYPE_CON
%token<string> TYPE_VARIABLE
%token<Mint.t>    INT
%token<char>   CHAR
%token<string> STRING

%token         TYPE
%token         LET
%token         FUN
%token         AND

%token         IF
%token         ELSE
%token         WHILE
%token         DO
%token         FOR
%token         TO
%token         SWITCH

%token         REF

%token         ANDAND
%token         PIPEPIPE
%token         EQUALQUESTION
%token         LBRACKEQUALQUESTION
%token         RBRACKEQUALQUESTION
%token         LBRACKQUESTION
%token         RBRACKQUESTION
%token         DOT
%token         EXCLAMATION
%token         PIPE
%token         COLON
%token         EQUAL
%token         PLUS
%token         MINUS
%token         STAR
%token         SLASH
%token         LANGLE
%token         RANGLE

%token         LPAR
%token         RPAR
%token         LCBRACK
%token         RCBRACK

%token         EOF

%left         ANDAND
%left         PIPEPIPE
%left         EQUALQUESTION
%left         LBRACKEQUALQUESTION
%left         RBRACKEQUALQUESTION
%left         LBRACKQUESTION
%left         RBRACKQUESTION
%left         PLUS
%left         MINUS
%left         STAR
%left         SLASH

%start<HopixAST.t> program

%%

program:
| definitions = list(located(definition)) EOF { definitions }

definition:
| LET id=located(identifier) EQUAL e=located(expr) { DefineValue(SimpleValue(id, None, e)) }

identifier:
| id=VAR_ID { Id(id) }

literal:
| c = CHAR   { LChar c }
| s = STRING { LString s }
| i = INT    { LInt i }


expr:
| e = simple_expr { e }
| e = binop(expr, prio_0, expr) { e }

simple_expr:
| e = atomic_expr { e }
| e = binop(simple_expr, prio_1, simple_expr) { e }

atomic_expr:
| l = located(literal)        { Literal l }
(* | id = VAR_ID        { Var (Id id) } *)
| LPAR e = expr RPAR { e }
| e = binop(atomic_expr, prio_2, atomic_expr) { e }

%inline binop(E1, OP, E2):
| e1 = located(E1) b = located(OP) e2 = located(E2) { 
    Apply(
      {value=Apply(b, e1); position= join e1.position e2.position},
       e2) 
  }
%inline prio_2:
(* int -> int -> int *)
| p = located(STAR)  { Variable(with_val (binop_name STAR) p, None) }
| p = located(SLASH) { Variable(with_val (binop_name SLASH) p, None) }

%inline prio_1:
(* int -> int -> int *)
| p = located(MINUS) { Variable(with_val (binop_name MINUS) p, None) }
| p = located(PLUS)  { Variable(with_val (binop_name PLUS) p, None) }
(* int -> int -> bool *)
| p = located(EQUALQUESTION)       { Variable(with_val (binop_name EQUALQUESTION) p, None) }
| p = located(LBRACKEQUALQUESTION) { Variable(with_val (binop_name LBRACKEQUALQUESTION) p, None) }
| p = located(RBRACKEQUALQUESTION) { Variable(with_val (binop_name RBRACKEQUALQUESTION) p, None) }
| p = located(LBRACKQUESTION)      { Variable(with_val (binop_name LBRACKQUESTION) p, None) }
| p = located(RBRACKQUESTION)      { Variable(with_val (binop_name RBRACKQUESTION) p, None) }

%inline prio_0:
(* bool -> bool -> bool *)
| p = located(ANDAND)   { Variable(with_val (binop_name ANDAND) p, None) }
| p = located(PIPEPIPE) { Variable(with_val (binop_name PIPEPIPE) p, None) }

%inline located(X): 
  x=X { Position.with_poss $startpos $endpos x }

