%{ (* Emacs, open this with -*- tuareg -*- *)
open AST
%}

%token<int> INT
%token<string> ID
%token PLUS STAR LPAR RPAR SUM COMMA EOF

%start<AST.exp> phrase

%left PLUS
%left STAR
%left S
%%

phrase: e=exp EOF
{
  e
}

exp: 
x=INT
{ LInt x }
| x=ID
{ Id x }
| e1=exp PLUS e2=exp
{ Add (e1, e2) }
| e1=exp STAR e2=exp
{ Mul (e1, e2) }
| LPAR e=exp RPAR
{ e }
| SUM LPAR n=ID COMMA e1=exp COMMA e2=exp COMMA e3=exp RPAR
{ Sum(n, e1, e2, e3)}