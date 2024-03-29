(** The Hopix programming language. *)

open SyntacticAnalysis

let name = "hopix"

module AST = HopixAST

type ast = HopixAST.t
let parse lexer_init input =
  SyntacticAnalysis.process ~lexer_init
    ~lexer_fun:(fun buf ->
      let tok = HopixLexer.token buf in
      (* Printf.printf "%s\n" (HopixASTHelper.string_of_token tok) ;*)
      tok)
    ~parser_fun:(fun lexer lexbuf ->
      try HopixParser.program lexer lexbuf
      with HopixParser.Error ->
        Error.error "parsing" (Position.cpos lexbuf) "Syntax error.")
    ~input

let parse_filename filename =
  if Options.get_use_sexp_in () then
    ExtStd.Stdlib.file_content filename
    |> Sexplib.Sexp.of_string |> HopixAST.program_of_sexp
  else parse (Lexing.from_channel ~with_positions:true) (open_in filename)

let extension = ".hopix"

let executable_format = false

let parse_string = parse Lexing.from_string

let print_ast ast =
  if Options.get_use_sexp_out () then
    HopixAST.sexp_of_program ast |> Sexplib.Sexp.to_string
  else HopixPrettyPrinter.(to_string program ast)

let print_expression e = HopixPrettyPrinter.(to_string expression e)

include HopixInterpreter
include HopixTypechecker
