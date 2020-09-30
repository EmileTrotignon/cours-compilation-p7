%token<string> LOWERCASE_ID
%token<string> UPPERCASE_ID
%token<string> CONSTR_ID
%token<string> LABEL_ID
%token<string> TYPE_CON
%token<string> TYPE_VARIABLE
%token<Mint.t> INT
%token<char>   CHAR
%token<string> STRING

%token         TYPE
%token         LET
%token         FUN
%token         EXTERN
%token         BACKSLASH
%token         AND
%token         AMPERSAND

%token         UNDERSCORE

%token         IF
%token         ELSE
%token         WHILE
%token         DO
%token         FOR
%token         TO
%token         SWITCH

%token         REF

%token         SEMICOLON

%token         DOUBLEAMPERSAND
%token         PIPEPIPE
%token         EQUALQUESTION
%token         LANGLEEQUALQUESTION
%token         RANGLEEQUALQUESTION
%token         LANGLEQUESTION
%token         RANGLEQUESTION
%token         DOT
%token         EXCLAMATION
%token         PIPE
%token         COLON
%token         EQUAL
%token         PLUS
%token         MINUS
%token         STAR
%token         SLASH
%token         ARROW
%token         COMMA


%token         LANGLE
%token         RANGLE
%token         LBRACK 
%token         RBRACK 
%token         LPAR
%token         RPAR
%token         LCBRACK
%token         RCBRACK

%token         EOF

%right UPPERCASE_ID
%left LPAR

%left COMMA
%left SEMICOLON
%left ARROW

%left REF

%left DOUBLEAMPERSAND
%left PIPEPIPE

%left EQUALQUESTION
%left LANGLEEQUALQUESTION
%left RANGLEEQUALQUESTION
%left LANGLEQUESTION
%left RANGLEQUESTION

%left PLUS
%left MINUS

%left STAR
%left SLASH
%%