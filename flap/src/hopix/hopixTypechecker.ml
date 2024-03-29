open HopixAST
(** This module implements a type checker for Hopix. *)

open HopixTypes

let initial_typing_environment = HopixTypes.initial_typing_environment

type typing_environment = HopixTypes.typing_environment

let type_error = HopixTypes.type_error

let pattern_error pos =
  type_error pos "This pattern is not compatible with the matched value."

let instantiate_type_scheme pos a b =
  try instantiate_type_scheme a b
  with InvalidInstantiation (expected, given) ->
    type_error pos
      (Printf.sprintf
         "Invalid number of types in instantiation: %d given while %d were \
          expected."
         given expected)

let lookup_type_scheme_of_label_for_record pos label tenv =
  try lookup_type_scheme_of_label label tenv
  with UnboundLabel ->
    type_error pos
      (Printf.sprintf "There is no type definition for the label `%s'."
         (match label with LId s -> s))

let lookup_type_scheme_of_label_for_field pos label tenv =
  try lookup_type_scheme_of_label label tenv
  with UnboundLabel ->
    type_error pos
      (Printf.sprintf "Label `%s' is unbound." (match label with LId s -> s))

let lookup_type_scheme_of_value pos x env =
  try lookup_type_scheme_of_value pos x env
  with UnboundIdentifier (pos, id) ->
    type_error pos
      (Printf.sprintf "Unbound value `%s'." (match id with Id s -> s))

let check_expected_field_type pos label aty_1 aty_2 =
  if aty_1 <> aty_2 then
    type_error pos
      (Printf.sprintf
         "The field `%s` as type `%s' while it should have type `%s'."
         (match label with LId s -> s)
         (print_aty aty_1) (print_aty aty_2))

let located f x = f (Position.position x) (Position.value x)

let aty_list_of_ty_list_option ty_list_option =
  List.map aty_of_ty
    (List.map Position.value (Option.value ~default:[] ty_list_option))

let rec arg_list_of_function aty =
  match aty with
  | ATyArrow (t1, t2) ->
      let args, result = arg_list_of_function t2 in
      (t1 :: args, result)
  | _                 -> ([], aty)

(** [check_program_is_fully_annotated ast] performs a syntactic check
 that the programmer wrote sufficient type annotations for [typecheck]
 to run correctly. *)
let check_program_is_fully_annotated ast =
  (*
      We check the presence of type ascriptions on:
      - variables
      - tagged values patterns
   *)
  let rec program p = List.iter (located definition) p
  and definition _ = function
    | DefineValue vdef -> value_definition vdef
    | _                -> ()
  and value_definition = function
    (* A toplevel definition for a value. *)
    | SimpleValue (x, s, e) ->
        if s = None then missing_type_annotation (Position.position x);
        located expression e
    (* A toplevel definition for mutually recursive functions. *)
    | RecFunctions fs -> List.iter function_definition fs
  and function_definition = function
    | f, s, FunctionDefinition (_, e) ->
        if s = None then missing_type_annotation (Position.position f);
        located expression e
  and expression pos = function
    | Define (vdef, e) ->
        value_definition vdef;
        located expression e
    | Apply (a, b) -> List.iter (located expression) [ a; b ]
    | Tuple ts -> List.iter (located expression) ts
    | Record (fields, a) ->
        if a = None then type_error pos "A type annotation is missing.";
        List.iter (fun (_, e) -> located expression e) fields
    | TypeAnnotation ({ Position.value = Fun (FunctionDefinition (_, e)) }, _)
      ->
        located expression e
    | Fun (FunctionDefinition (_, _)) ->
        type_error pos "An anonymous function must be annotated."
    | Field (e, _) | TypeAnnotation (e, _) | Ref e | Read e ->
        located expression e
    | Sequence es -> List.iter (located expression) es
    | Tagged (_, a, es) ->
        if a = None then type_error pos "A type annotation is missing.";
        List.iter (located expression) es
    | For (_, e1, e2, e3) -> List.iter (located expression) [ e1; e2; e3 ]
    | IfThenElse (c, t, f) -> List.iter (located expression) [ c; t; f ]
    | Case (e, bs) ->
        located expression e;
        List.iter (located branch) bs
    | Assign (e1, e2) | While (e1, e2) ->
        located expression e1;
        located expression e2
    | Literal _ | Variable _ -> ()
  and pattern pos = function
    | PTypeAnnotation ({ Position.value = PWildcard | PVariable _ }, _) -> ()
    | PRecord (fields, a) ->
        if a = None then type_error pos "A type annotation is missing.";
        List.iter (fun (_, p) -> located pattern p) fields
    | PTuple ps -> List.iter (located pattern) ps
    | PTypeAnnotation (p, _) -> located pattern p
    | PVariable _ | PWildcard -> missing_type_annotation pos
    | PTaggedValue (_, a, ps) ->
        if a = None then type_error pos "A type annotation is missing.";
        List.iter (located pattern) ps
    | POr ps | PAnd ps -> List.iter (located pattern) ps
    | PLiteral _ -> ()
  and branch _ = function
    | Branch (p, e) ->
        located pattern p;
        located expression e
  and missing_type_annotation pos =
    type_error pos "A type annotation is missing."
  in
  program ast

let invalid_instantiation pos given expected =
  type_error pos
    (Printf.sprintf
       "Invalid number of types in instantiation: %d given while %d were \
        expected."
       given expected)

(* [typecheck tenv ast] checks that [ast] is a well-formed program
    under the typing environment [tenv]. *)
let typecheck tenv ast : typing_environment =
  check_program_is_fully_annotated ast;

  let rec program p =
    List.fold_left (fun env x -> located (definition env) x) tenv p
  and definition tenv _ = function
    | DefineValue vdef         -> value_definition tenv vdef
    | DefineType (t, ts, tdef) ->
        let ts = List.map Position.value ts in
        HopixTypes.bind_type_definition (Position.value t) ts tenv tdef
    | DeclareExtern (x, s)     ->
        let s = located (type_scheme tenv) s in
        bind_value (Position.value x) s tenv
  and type_scheme tenv pos (ForallTy (ts, ty)) =
    let ts = List.map Position.value ts in
    let tenv = bind_type_variables pos tenv ts in
    Scheme (ts, internalize_ty tenv ty)
  and bind_type_variables pos tenv ts =
    List.iter
      (fun v ->
        if HopixTypes.is_type_variable_defined pos tenv v then
          type_error pos
            (Printf.sprintf
               "The type variable `%s' is already bound in the environment."
               HopixPrettyPrinter.(to_string type_variable v)))
      ts;
    HopixTypes.bind_type_variables pos tenv ts
  and value_definition (tenv : typing_environment) = function
    | SimpleValue (x, Some s, e) ->
        let pos = Position.position s in
        let (Scheme (ts, aty) as s) = located (type_scheme tenv) s in
        let tenv' = bind_type_variables pos tenv ts in
        check_expression_monotype tenv' aty e;
        bind_value (Position.value x) s tenv
    | SimpleValue (_, _, _)      ->
        assert false (* By check_program_is_fully_annotated. *)
    | RecFunctions fs            -> recursive_definitions tenv fs
  and recursive_definitions tenv recdefs =
    let tenv =
      List.fold_left
        (fun tenv (f, fs, _) ->
          match fs with
          | None    -> assert false (* By check_program_is_fully_annotated. *)
          | Some fs ->
              let f = Position.value f in
              let fs = located (type_scheme tenv) fs in
              let fs = refresh_type_scheme fs in
              bind_value f fs tenv)
        tenv recdefs
    in
    List.iter
      (fun (f, fs, d) ->
        match fs with
        | None    -> assert false
        | Some fs ->
            let pos = Position.position f in
            let fs = located (type_scheme tenv) fs in
            check_function_definition pos tenv fs d)
      recdefs;
    tenv
  (* [check_function_definition tenv fdef] checks that the
      function definition [fdef] is well-typed with respect to the
      type annotations written by the programmer. We assume that
      [tenv] already contains the type scheme of the function [f]
      defined by [fdef] as well as all the functions which are
      mutually recursively defined with [f]. *)
  and check_function_definition pos tenv aty = function
    | FunctionDefinition (p, e) -> (
        match aty with
        | Scheme (ts, ATyArrow (_, out)) ->
            let tenv = bind_type_variables pos tenv ts in
            let tenv, _ = located (pattern tenv None) p in
            check_expression_monotype tenv out e
        | _ -> type_error pos "A function must have an arrow type." )
  (* [check_expected_type pos xty ity] verifies that the expected
      type [xty] is syntactically equal to the inferred type [ity]
      and raises an error otherwise. *)
  and check_expected_type pos xty ity =
    if xty <> ity then
      type_error pos
        (Printf.sprintf "Type error:\nExpected:\n  %s\nGiven:\n  %s\n"
           (print_aty xty) (print_aty ity))
  (* [check_expression_monotype tenv xty e] checks if [e] has
      the monotype [xty] under the context [tenv]. *)
  and check_expression_monotype tenv xty e : unit =
    let pos = Position.position e in
    let ity = located (type_of_expression tenv) e in
    check_expected_type pos xty ity
  (* [type_of_expression tenv pos e] computes a type for [e] if it exists. *)
  and type_of_expression tenv pos =
    let rec type_of_literal lit : aty =
      match lit with
      | LInt _    -> ATyCon (TCon "int", [])
      | LString _ -> ATyCon (TCon "string", [])
      | LChar _   -> ATyCon (TCon "char", [])
    and type_of_variable (id, ty_args) =
      let aty_args = aty_list_of_ty_list_option ty_args in
      let scheme =
        lookup_type_scheme_of_value (Position.position id) (Position.value id)
          tenv
      in
      instantiate_type_scheme pos scheme aty_args
    and type_of_tagged tenv (constr, ty_args, args) =
      let ty_args = aty_list_of_ty_list_option ty_args in
      try
        let scheme =
          lookup_type_scheme_of_constructor (Position.value constr) tenv
        in
        let aty = instantiate_type_scheme pos scheme ty_args in
        let args', result = arg_list_of_function aty in
        let seq () =
          let rec aux args' args () =
            match (args', args) with
            | [], []              -> Seq.Nil
            | a' :: a's, a :: as_ ->
                Seq.Cons
                  ( ( a',
                      Position.with_pos (Position.position a)
                        (type_of_expression' tenv a) ),
                    aux a's as_ )
            | _                   ->
                check_expected_type pos aty result;
                assert false
          in
          aux args' args ()
        in
        Seq.iter
          (fun (aty, aty_l) ->
            check_expected_type (Position.position aty_l) aty
              (Position.value aty_l))
          seq;
        result
      with UnboundConstructor ->
        type_error pos
          ( "Unbound constructor `"
          ^ (match Position.value constr with KId s -> s)
          ^ "'." )
    and type_of_record tenv (l, ty_args) =
      let aty_args = aty_list_of_ty_list_option ty_args in
      let l' =
        List.map
          (fun (label, expr) ->
            (Position.value label, type_of_expression' tenv expr))
          l
      in
      List.iter
        (fun (label, aty) ->
          let scheme = lookup_type_scheme_of_label_for_record pos label tenv in
          let aty_field =
            match instantiate_type_scheme pos scheme aty_args with
            | ATyArrow (_, aty) -> aty
            | _                 -> failwith "ERROR TODO 6"
          in
          check_expected_field_type pos label aty aty_field)
        l';
      let a =
        List.find
          (fun (_, (_, type_info)) ->
            match type_info with
            | Abstract -> false
            | Sum _    -> false
            | Record _ -> true)
          tenv.type_constructors
      in
      match a with
      | type_constructor, (_, _) -> ATyCon (type_constructor, aty_args)
    and type_of_field tenv (expr, (label : label Position.located)) =
      let t_expr = type_of_expression' tenv expr in
      match t_expr with
      | ATyCon (_, aty_args) -> (
          match
            instantiate_type_scheme pos
              (lookup_type_scheme_of_label_for_field pos label.value tenv)
              aty_args
          with
          | ATyArrow (_, aty) -> aty
          | _                 -> failwith "ERROR TODO 7" )
      | _                    -> failwith "ERROR TODO 5"
    and type_of_tuple tenv tuple =
      ATyTuple (List.map (type_of_expression' tenv) tuple)
    and type_of_sequence tenv seq =
      match seq with
      | []        -> failwith "empty sec"
      | [ e ]     -> type_of_expression' tenv e
      | e :: seq' ->
          let t_e = type_of_expression' tenv e in
          check_expected_type (Position.position e) t_e
            (ATyCon (TCon "unit", []));
          type_of_sequence tenv seq'
    and type_of_define tenv (value_def, expression) =
      let tenv' = value_definition tenv value_def in
      type_of_expression' tenv' expression
    and type_of_fun tenv (p_arg, body) =
      let tenv', aty_arg = pattern' tenv None p_arg in
      ATyArrow (aty_arg, type_of_expression' tenv' body)
    and type_of_apply tenv (f, args) =
      match type_of_expression' tenv f with
      | ATyArrow (t_expected_arg, t_result) ->
          let t_real_arg = type_of_expression' tenv args in
          check_expected_type (Position.position args) t_expected_arg t_real_arg;
          t_result
      | _aty -> type_error pos "Only functions can be applied."
    and type_of_ref tenv ref =
      let inner_type = type_of_expression' tenv ref in
      ATyCon (TCon "mut", [ inner_type ])
    and type_of_assign tenv (e1, e2) =
      ( match type_of_expression' tenv e1 with
      | ATyCon (TCon "mut", [ inner_type ]) ->
          check_expected_type (Position.position e2) inner_type
            (type_of_expression' tenv e2)
      | _ -> failwith "not a ref" );
      ATyCon (TCon "unit", [])
    and type_of_read tenv read =
      match type_of_expression' tenv read with
      | ATyCon (TCon "mut", [ inner_type ]) -> inner_type
      | _ -> failwith "not a ref"
    and type_of_case tenv (expr, (cases : branch Position.located list)) =
      let aty_matched = type_of_expression' tenv expr in
      let l =
        List.map
          (fun branch ->
            match Position.value branch with
            | Branch (pattern, body) ->
                let tenv', _ = pattern' tenv (Some aty_matched) pattern in
                (*check_expected_type pos aty_pattern aty_matched;*)
                type_of_expression' tenv' body)
          cases
      in
      match l with [] -> assert false | x :: _ -> x
    and type_of_if_then_else tenv (cond, body, body_else) =
      let t_cond = type_of_expression' tenv cond in
      let t_body = type_of_expression' tenv body in
      let t_body_else = type_of_expression' tenv body_else in
      check_expected_type pos t_cond (ATyCon (TCon "bool", []));
      check_expected_type pos t_body t_body_else;
      t_body
    and type_of_while tenv (cond, body) =
      let t_cond = type_of_expression' tenv cond in
      let t_body = type_of_expression' tenv body in
      check_expected_type cond.position t_cond (ATyCon (TCon "bool", []));
      check_expected_type body.position t_body (ATyCon (TCon "unit", []));
      t_body
    and type_of_for tenv (var_name, low_bound, high_bound, body) =
      let t_low = type_of_expression' tenv low_bound in
      let t_high = type_of_expression' tenv high_bound in
      check_expected_type
        (Position.position low_bound)
        t_low
        (ATyCon (TCon "int", []));
      check_expected_type
        (Position.position high_bound)
        t_high
        (ATyCon (TCon "int", []));
      let tenv' =
        {
          tenv with
          values =
            (Position.value var_name, Scheme ([], ATyCon (TCon "int", [])))
            :: tenv.values;
        }
      in
      let t_body = type_of_expression' tenv' body in
      check_expected_type pos t_body (ATyCon (TCon "unit", []));
      t_body
    in
    function
    | Literal lit -> type_of_literal lit.value
    | Variable (id, ty_args) -> type_of_variable (id, ty_args)
    | Tagged (constr, ty_args, e) -> type_of_tagged tenv (constr, ty_args, e)
    | Record (l, ty_args) -> type_of_record tenv (l, ty_args)
    | Field (expr, label) -> type_of_field tenv (expr, label)
    | Tuple exprs -> type_of_tuple tenv exprs
    | Sequence seq -> type_of_sequence tenv seq
    | Define (value_definition, expression) ->
        type_of_define tenv (value_definition, expression)
    | Fun (FunctionDefinition (arg_p, body)) -> type_of_fun tenv (arg_p, body)
    | Apply (f, arg) -> type_of_apply tenv (f, arg)
    | Ref expression -> type_of_ref tenv expression
    | Assign (e1, e2) -> type_of_assign tenv (e1, e2)
    | Read expression -> type_of_read tenv expression
    | Case (expr, branches) -> type_of_case tenv (expr, branches)
    | IfThenElse (cond, body, body_else) ->
        type_of_if_then_else tenv (cond, body, body_else)
    | While (cond, body) -> type_of_while tenv (cond, body)
    | For (id, bound_low, bound_high, body) ->
        type_of_for tenv (id, bound_low, bound_high, body)
    | TypeAnnotation (e, ta) ->
        let aty_annotation = aty_of_ty' ta in
        let aty_expr = type_of_expression' tenv e in
        check_expected_type pos aty_expr aty_annotation;
        aty_annotation
  and type_of_expression' tenv expr =
    type_of_expression tenv expr.position expr.value
  (*and patterns tenv = function
    | []      -> (tenv, [])
    | p :: ps ->
        let tenv, ty = located (pattern tenv) p in
        let tenv, tys = patterns tenv ps in
        (tenv, ty :: tys)*)
  (* [pattern tenv pos p] computes a new environment completed with
      the variables introduced by the pattern [p] as well as the type
      of this pattern. *)
  and pattern' tenv value_aty p =
    pattern tenv value_aty (Position.position p) (Position.value p)
  and pattern tenv (value_aty : aty option) pos p : typing_environment * aty =
    let rec type_of_pvariable tenv value_aty id =
      match value_aty with
      | None     ->
          let var = fresh () in
          (bind_value id (Scheme ([ var ], ATyVar var)) tenv, ATyVar var)
      | Some aty -> (bind_value id (monotype aty) tenv, aty)
    and type_of_pliteral tenv value_aty literal =
      match (Position.value literal, value_aty) with
      | LInt _, Some (ATyCon (TCon "int", [])) | LInt _, None ->
          (tenv, ATyCon (TCon "int", []))
      | LChar _, Some (ATyCon (TCon "char", [])) | LChar _, None ->
          (tenv, ATyCon (TCon "char", []))
      | LString _, Some (ATyCon (TCon "string", [])) | LString _, None ->
          (tenv, ATyCon (TCon "string", []))
      | _ -> pattern_error pos
    and type_of_ptagged tenv value_aty (constructor, ty_args, patterns) =
      let ty_args = aty_list_of_ty_list_option ty_args in
      try
        let scheme_pattern =
          lookup_type_scheme_of_constructor (Position.value constructor) tenv
        in

        let aty = instantiate_type_scheme pos scheme_pattern ty_args in
        let args', result = arg_list_of_function aty in
        let tenv_ref = ref tenv in
        let seq () =
          let rec aux args' args tenv () =
            match (args', args) with
            | [], []              -> Seq.Nil
            | a' :: a's, a :: as_ ->
                let tenv', aty_p = pattern' tenv None a in
                tenv_ref := tenv';
                Seq.Cons
                  ( (a', Position.with_pos (Position.position a) aty_p),
                    aux a's as_ tenv' )
            | _                   -> assert false
          in
          aux args' patterns tenv ()
        in
        Seq.iter
          (fun (aty, pat_ty) ->
            let aty_l = Position.value pat_ty in
            check_expected_type (Position.position pat_ty) aty aty_l)
          seq;
        ( match value_aty with
        | None     -> ()
        | Some aty -> check_expected_type pos aty result );
        (!tenv_ref, result)
      with UnboundConstructor ->
        type_error pos
          ( "Unbound constructor `"
          ^ (match Position.value constructor with KId s -> s)
          ^ "'." )
    and type_of_precord tenv value_aty (fields, ty_args) =
      let aty_args = aty_list_of_ty_list_option ty_args in
      let tenv_ref = ref tenv in
      List.iter
        (fun (label, pattern) ->
          let label = Position.value label in
          let scheme = lookup_type_scheme_of_label_for_record pos label tenv in
          let aty_field =
            match instantiate_type_scheme pos scheme aty_args with
            | ATyArrow (_, aty) -> aty
            | _                 -> assert false
          in
          let tenv', aty = pattern' tenv (Some aty_field) pattern in
          tenv_ref := tenv';
          check_expected_field_type pos label aty aty_field) (* ? *)
        fields;
      let a =
        List.find
          (fun (_, (_, type_info)) ->
            match type_info with
            | Abstract -> false
            | Sum _    -> false
            | Record _ -> true)
          tenv.type_constructors
      in
      let aty_result =
        match a with
        | type_constructor, (_, _) -> ATyCon (type_constructor, aty_args)
      in
      ( match value_aty with
      | None     -> ()
      | Some aty -> if aty <> aty_result then pattern_error pos );
      (!tenv_ref, aty_result)
    and type_of_ptuple tenv value_aty patterns =
      match value_aty with
      | Some (ATyTuple aty_list) ->
          let tenv_acc = ref tenv in
          let result =
            List.map2
              (fun aty_value pattern ->
                let tenv', aty' = pattern' !tenv_acc (Some aty_value) pattern in
                tenv_acc := tenv';
                aty')
              aty_list patterns
          in
          (!tenv_acc, ATyTuple result)
      | Some _                   -> pattern_error pos
      | None                     ->
          let tenv_acc = ref tenv in
          let result =
            List.map
              (fun pattern ->
                let tenv', aty' = pattern' !tenv_acc None pattern in
                tenv_acc := tenv';
                aty')
              patterns
          in
          (!tenv_acc, ATyTuple result)
    and type_of_por tenv value_aty patterns =
      let tenv_ref = ref tenv in
      let aty_ref = ref None in
      List.iter
        (fun pattern ->
          let tenv', aty = pattern' !tenv_ref value_aty pattern in
          ( match !aty_ref with
          | None              -> ()
          | Some previous_aty -> check_expected_type pos previous_aty aty );
          tenv_ref := tenv';
          aty_ref := Some aty)
        patterns;
      (!tenv_ref, Option.get !aty_ref)
    and type_of_pand tenv value_aty patterns =
      type_of_por tenv value_aty patterns
    in
    match p with
    | PVariable id -> type_of_pvariable tenv value_aty (Position.value id)
    | PWildcard -> (
        match value_aty with
        | None     -> (tenv, ATyVar (fresh ()))
        | Some aty -> (tenv, aty) )
    | PTypeAnnotation (pattern, ty) ->
        let aty = aty_of_ty (Position.value ty) in
        ( match value_aty with
        | None      -> ()
        | Some aty' -> check_expected_type pos aty aty' );
        pattern' tenv (Some aty) pattern
    | PLiteral literal -> type_of_pliteral tenv value_aty literal
    | PTaggedValue (constructor, ty_args, patterns) ->
        type_of_ptagged tenv value_aty (constructor, ty_args, patterns)
    | PRecord (fields, ty_args) ->
        type_of_precord tenv value_aty (fields, ty_args)
    | PTuple patterns -> type_of_ptuple tenv value_aty patterns
    | POr patterns -> type_of_por tenv value_aty patterns
    | PAnd patterns -> type_of_pand tenv value_aty patterns
  in
  program ast

let print_typing_environment = HopixTypes.print_typing_environment
