(** This module implements a compiler from Hobix to Fopix. *)

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)

module Source = Hobix
module S = Source.AST
module Target = Fopix
module T = Target.AST

(**

   The translation from Hobix to Fopix turns anonymous
   lambda-abstractions into toplevel functions and applications into
   function calls. In other words, it translates a high-level language
   (like OCaml) into a first order language (like C).

   To do so, we follow the closure conversion technique.

   The idea is to make explicit the construction of closures, which
   represent functions as first-class objects. A closure is a block
   that contains a code pointer to a toplevel function [f] followed by all
   the values needed to execute the body of [f]. For instance, consider
   the following OCaml code:

   let f =
     let x = 6 * 7 in
     let z = x + 1 in
     fun y -> x + y * z

   The values needed to execute the function "fun y -> x + y * z" are
   its free variables "x" and "z". The same program with explicit usage
   of closure can be written like this:

   let g y env = env[1] + y * env[2]
   let f =
      let x = 6 * 7 in
      let z = x + 1 in
      [| g; x; z |]

   (in an imaginary OCaml in which arrays are untyped.)

   Once closures are explicited, there are no more anonymous functions!

   But, wait, how to we call such a function? Let us see that on an
   example:

   let f = ... (* As in the previous example *)
   let u = f 0

   The application "f 0" must be turned into an expression in which
   "f" is a closure and the call to "f" is replaced to a call to "g"
   with the proper arguments. The argument "y" of "g" is known from
   the application: it is "0". Now, where is "env"? Easy! It is the
   closure itself! We get:

   let g y env = env[1] + y * env[2]
   let f =
      let x = 6 * 7 in
      let z = x + 1 in
      [| g; x; z |]
   let u = f[0] 0 f

   (Remark: Did you notice that this form of "auto-application" is
   very similar to the way "this" is defined in object-oriented
   programming languages?)

*)

(**
   Helpers functions.
*)

let error pos msg = Error.error "compilation" pos msg

let make_fresh_variable =
  let r = ref 0 in
  fun ?(prefix = "") () ->
    incr r;
    T.Id (Printf.sprintf "%s_%d" prefix !r)

(*

int equal_string(const char* s1, const char* s2) {
  return (strcmp (s1, s2) == 0 ? 1 : 0);
}

int equal_char(char c1, char c2) {
  return (c1 == c2 ? 1 : 0);
}

void print_string(const char* s) {
  printf("%s", s);
}

void print_int(int64_t n) {
    printf("%d", n);
    //fprintf(stderr, "Students! This is your job!\n");
}

void observe_int(int64_t n) {
  print_int(n);
}

intptr_t* allocate_block (int64_t n) {
  return (intptr_t* )malloc (n * sizeof (int64_t));
  }

  intptr_t read_block (intptr_t* block, int64_t n) {
    return block[n];
  }
  
  int64_t write_block (intptr_t* block, int64_t n, intptr_t v) {
    block[n] = v;
    return 0;
  }
  
*)
let is_primitive = function
  | "`+`" | "`-`" | "`*`" | "`/`" | "`<?`" | "`>?`" | "`<=?`" | "`>=?`" | "`=?`"
  | "`&&`" | "`||`" | "write_block" | "read_block" | "allocate_block"
  | "observe_int" | "print_int" | "print_string" | "equal_char" | "equal_string"
    ->
      true
  | _ -> false

let make_fresh_function_identifier =
  let r = ref 0 in
  fun ?(prefix = "") () ->
    incr r;
    T.FunId (Printf.sprintf "%s_%d" prefix !r)

let define e f =
  let x = make_fresh_variable ~prefix:"ignore" () in
  T.Define (x, e, f x)

let rec defines ds e =
  match ds with [] -> e | (x, d) :: ds -> T.Define (x, d, defines ds e)

let seq a b = define a (fun _ -> b)

let rec seqs = function
  | []      -> assert false
  | [ x ]   -> x
  | x :: xs -> seq x (seqs xs)

let allocate_block e = T.(FunCall (FunId "allocate_block", [ e ]))

let write_block e i v = T.(FunCall (FunId "write_block", [ e; i; v ]))

let read_block e i = T.(FunCall (FunId "read_block", [ e; i ]))

let lint i = T.(Literal (LInt (Int64.of_int i)))

(** [free_variables e] returns the list of free variables that
     occur in [e].*)
let free_variables =
  let module M = struct
    include Set.Make (struct
      type t = S.identifier

      let compare = compare
    end)

    let unions = List.fold_left union empty
  end in
  let rec unions f = function
    | []      -> M.empty
    | [ s ]   -> f s
    | s :: xs -> M.union (f s) (unions f xs)
  in
  let rec fvs = function
    | S.Literal _ -> M.empty
    | S.Variable x -> M.singleton x
    | S.While (cond, e) -> fvs e
    | S.Define (vd, a) -> (
        match vd with
        | SimpleValue (name, value) ->
            M.union (M.remove name (fvs a)) (fvs value)
        | RecFunctions li           ->
            let names, values = List.split li in
            M.union (M.unions (List.map fvs values)) (M.of_list names) )
    | S.ReadBlock (a, b) -> unions fvs [ a; b ]
    | S.Apply (a, b) -> unions fvs (a :: b)
    | S.WriteBlock (a, b, c) | S.IfThenElse (a, b, c) -> unions fvs [ a; b; c ]
    | S.AllocateBlock a -> fvs a
    | S.Fun (xs, e) ->
        (* Maybe switch args here *)
        M.diff (fvs e) (M.of_list xs)
    | S.Switch (a, b, c) ->
        let c = match c with None -> [] | Some c -> [ c ] in
        unions fvs ((a :: ExtStd.Array.present_to_list b) @ c)
  in
  fun e -> M.elements (fvs e)

type environment = {
  vars : (HobixAST.identifier, FopixAST.expression) Dict.t;
  externals : (HobixAST.identifier, int) Dict.t;
}

open Printf
(**

    A closure compilation environment relates an identifier to the way
    it is accessed in the compiled version of the function's
    body.

    Indeed, consider the following example. Imagine that the following
    function is to be compiled:

    fun x -> x + y

    In that case, the closure compilation environment will contain:

    x -> x
    y -> "the code that extract the value of y from the closure environment"

    Indeed, "x" is a local variable that can be accessed directly in
    the compiled version of this function's body whereas "y" is a free
    variable whose value must be retrieved from the closure's
    environment.

*)

let initial_environment () = { vars = Dict.empty; externals = Dict.empty }

let bind_external id n env =
  { env with externals = Dict.insert id n env.externals }

let bind_var id e env = { env with vars = Dict.insert id e env.vars }

let is_external id env = Dict.lookup id env.externals <> None

let reset_vars env = { env with vars = Dict.empty }

(** Precondition: [is_external id env = true]. *)
let arity_of_external id env =
  match Dict.lookup id env.externals with Some n -> n | None -> assert false

(* By is_external. *)

(** [translate p env] turns an Hobix program [p] into a Fopix program
    using [env] to retrieve contextual information. *)
let translate (p : S.t) env =
  let rec program env defs =
    let env, defs = ExtStd.List.foldmap definition env defs in
    (List.flatten defs, env)
  and definition env = function
    | S.DeclareExtern (id, n) ->
        let env = bind_external id n env in
        (env, [ T.ExternalFunction (function_identifier id, n) ])
    | S.DefineValue vd        -> (env, value_definition env vd)
  and value_definition env = function
    | S.SimpleValue (x, e) ->
        let fs, e = expression (reset_vars env) e in
        fs @ [ T.DefineValue (identifier x, e) ]
    | S.RecFunctions fdefs ->
        let fs, defs = define_recursive_functions fdefs in
        fs @ List.map (fun (x, e) -> T.DefineValue (x, e)) defs
  and define_recursive_functions rdefs =
    let s_ids, s_exprs = List.split rdefs in
    let defs, expressions =
      List.split
      @@ List.map (fun (name, expr) -> expression ~self:name env expr) rdefs
    in
    let defs =
      List.flatten
        ( List.map
            (fun (name, expr) ->
              T.DefineValue
                ( identifier name,
                  allocate_block
                    (T.Literal
                       (T.LInt
                          (Mint.of_int (List.length (free_variables expr) + 1))))
                ))
            rdefs
        :: defs )
    in
    (defs, List.combine (List.map identifier s_ids) expressions)
  and expression ?self env =
    let t_expr_of_s_id env = function
      | S.Id id as x ->
          if is_primitive id then T.Literal (T.LFun (T.FunId id))
          else
            let xc =
              match Dict.lookup x env.vars with
              | None   -> T.Variable (identifier x)
              | Some e -> e
            in
            xc
    in
    function
    | S.Literal l               ->
        (* print_endline "literal"; *)
        ([], T.Literal (literal l))
    | S.While (cond, e)         ->
        (* print_endline "while"; *)
        let cfs, cond = expression env cond in
        let efs, e = expression env e in
        (cfs @ efs, T.While (cond, e))
    | S.Variable x              ->
        (* print_endline "variable"; *)
        ([], t_expr_of_s_id env x)
    | S.Define (vdef, a)        -> (
        (* print_endline "define"; *)
        match vdef with
        | SimpleValue (id, value) ->
            let defs, t_value = expression env value in
            let defs', t_expr = expression env a in
            (defs @ defs', T.Define (identifier id, t_value, t_expr))
        | _                       -> failwith "TODO 3" )
    | S.Apply (a, bs)           -> (
        (* print_endline "apply"; *)
        let defs, expr = expression env a in
        let defss, exprs' = List.split (List.map (expression env) bs) in

        ( defs @ List.concat defss,
          match expr with
          | T.Literal (T.LFun id) -> T.FunCall (id, exprs')
          | _                     ->
              T.UnknownFunCall
                (read_block expr (T.Literal (T.LInt Int64.zero)), expr :: exprs')
        ) )
    | S.IfThenElse (a, b, c)    ->
        (* print_endline "ifthenelse"; *)
        let afs, a = expression env a in
        let bfs, b = expression env b in
        let cfs, c = expression env c in
        (afs @ bfs @ cfs, T.IfThenElse (a, b, c))
    | S.Fun (x, e) as f         -> (
        (* print_endline "fun"; *)
        let fvs = free_variables f in
        let closure_id = make_fresh_variable ~prefix:"closure" () in
        let get_i =
          let i = ref 0 in
          fun () ->
            i := !i + 1;
            !i
        in
        let my_bind env id =
          bind_var id
            (read_block (T.Variable closure_id)
               (T.Literal (T.LInt (Mint.of_int (get_i ())))))
            env
        in
        let block_id =
          match self with
          | None      -> make_fresh_variable ~prefix:"block" ()
          | Some name -> identifier name
        in
        let env =
          match self with
          | Some id -> bind_var id (T.Variable block_id) env
          | None    -> env
        in
        let env' = List.fold_left my_bind env fvs in
        let defs, expr = expression env' e in
        let f_id = make_fresh_function_identifier ~prefix:"lambda" () in
        let set_block =
          seqs
            ( write_block (T.Variable block_id) (T.Literal (T.LInt Int64.zero))
                (T.Literal (T.LFun f_id))
              :: List.mapi
                   (fun i fv ->
                     write_block (T.Variable block_id)
                       (T.Literal (T.LInt (Mint.of_int (i + 1))))
                       (t_expr_of_s_id env fv))
                   fvs
            @ [ T.Variable block_id ] )
        in
        ( T.DefineFunction (f_id, closure_id :: List.map identifier x, expr)
          :: defs,
          match self with
          | None   ->
              T.Define
                ( block_id,
                  allocate_block
                    (T.Literal (T.LInt (Mint.of_int (List.length fvs + 1)))),
                  set_block )
          | Some _ -> set_block ) )
    | S.AllocateBlock a         ->
        (* print_endline "AllocateBlock"; *)
        let afs, a = expression env a in
        (afs, allocate_block a)
    | S.WriteBlock (a, b, c)    ->
        (* print_endline "WriteBlock"; *)
        let afs, a = expression env a in
        let bfs, b = expression env b in
        let cfs, c = expression env c in
        (afs @ bfs @ cfs, T.FunCall (T.FunId "write_block", [ a; b; c ]))
    | S.ReadBlock (a, b)        ->
        (* print_endline "ReadBlock"; *)
        let afs, a = expression env a in
        let bfs, b = expression env b in
        (afs @ bfs, T.FunCall (T.FunId "read_block", [ a; b ]))
    | S.Switch (a, bs, default) ->
        (* print_endline "switch"; *)
        let afs, a = expression env a in
        let bsfs, bs =
          ExtStd.List.foldmap
            (fun bs t ->
              match ExtStd.Option.map (expression env) t with
              | None           -> (bs, None)
              | Some (bs', t') -> (bs @ bs', Some t'))
            [] (Array.to_list bs)
        in
        let dfs, default =
          match default with
          | None   -> ([], None)
          | Some e ->
              let bs, e = expression env e in
              (bs, Some e)
        in
        (afs @ bsfs @ dfs, T.Switch (a, Array.of_list bs, default))
  and expressions env = function
    | []      -> ([], [])
    | e :: es ->
        let efs, es = expressions env es in
        let fs, e = expression env e in
        (fs @ efs, e :: es)
  and literal = function
    | S.LInt x    -> T.LInt x
    | S.LString s -> T.LString s
    | S.LChar c   -> T.LChar c
  and identifier (S.Id x) = T.Id x
  and function_identifier (S.Id x) = T.FunId x in

  program env p
