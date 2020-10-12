open Position
open Error
open HopixAST

(** [error pos msg] reports execution error messages. *)
let error positions msg = errorN "execution" positions msg

(** Every expression of Hopix evaluates into a [value].

   The [value] type is not defined here. Instead, it will be defined
   by instantiation of following ['e gvalue] with ['e = environment].
   Why? The value type and the environment type are mutually recursive
   and since we do not want to define them simultaneously, this
   parameterization is a way to describe how the value type will use
   the environment type without an actual definition of this type.

*)

type 'e gvalue =
  | VInt of Mint.t
  | VChar of char
  | VString of string
  | VUnit
  | VTagged of constructor * 'e gvalue list
  | VTuple of 'e gvalue list
  | VRecord of (label * 'e gvalue) list
  | VLocation of Memory.location
  | VClosure of 'e * pattern located * expression located
  | VPrimitive of string * ('e gvalue Memory.t -> 'e gvalue -> 'e gvalue)

(** Two values for booleans. *)
let ptrue = VTagged (KId "True", [])

let pfalse = VTagged (KId "False", [])

(**
    We often need to check that a value has a specific shape.
    To that end, we introduce the following coercions. A
    coercion of type [('a, 'e)] coercion tries to convert an
    Hopix value into a OCaml value of type ['a]. If this conversion
    fails, it returns [None].
*)

type ('a, 'e) coercion = 'e gvalue -> 'a option

let fail = None

let ret x = Some x

let value_as_int = function VInt x -> ret x | _ -> fail

let value_as_char = function VChar c -> ret c | _ -> fail

let value_as_string = function VString s -> ret s | _ -> fail

let value_as_tagged = function VTagged (k, vs) -> ret (k, vs) | _ -> fail

let value_as_tuple = function VTuple li -> ret li | _ -> fail

let value_as_record = function VRecord fs -> ret fs | _ -> fail

let value_as_location = function VLocation l -> ret l | _ -> fail

let value_as_closure = function
  | VClosure (e, p, b) -> ret (e, p, b)
  | _                  -> fail

let value_as_primitive = function VPrimitive (p, f) -> ret (p, f) | _ -> fail

let value_as_closure_or_primitive = function
  | VClosure (e, p, b) -> ret (VClosure (e, p, b))
  | VPrimitive (p, f)  -> ret (VPrimitive (p, f))
  | _                  -> fail

let value_as_bool = function
  | VTagged (KId "True", [])  -> true
  | VTagged (KId "False", []) -> false
  | _                         -> assert false

type ('a, 'e) wrapper = 'a -> 'e gvalue
(**
   It is also very common to have to inject an OCaml value into
   the types of Hopix values. That is the purpose of a wrapper.
 *)

let int_as_value x = VInt x

let bool_as_value b = if b then ptrue else pfalse

(**

  The flap toplevel needs to print the result of evaluations. This is
   especially useful for debugging and testing purpose. Do not modify
   the code of this function since it is used by the testsuite.

*)
let print_value m v =
  (* To avoid to print large (or infinite) values, we stop at depth 5. *)
  let max_depth = 5 in
  let rec print_value d v =
    if d >= max_depth then "..."
    else
      match v with
      | VInt x              -> Mint.to_string x
      | VChar c             -> "'" ^ Char.escaped c ^ "'"
      | VString s           -> "\"" ^ String.escaped s ^ "\""
      | VUnit               -> "()"
      | VLocation a         -> print_array_value d (Memory.dereference m a)
      | VTagged (KId k, []) -> k
      | VTagged (KId k, vs) -> k ^ print_tuple d vs
      | VTuple vs           -> print_tuple d vs
      | VRecord fs          ->
          "{"
          ^ String.concat ", "
              (List.map
                 (fun (LId f, v) -> f ^ " = " ^ print_value (d + 1) v)
                 fs)
          ^ "}"
      | VClosure _          -> "<fun>"
      | VPrimitive (s, _)   -> Printf.sprintf "<primitive: %s>" s
  and print_tuple d vs =
    "(" ^ String.concat ", " (List.map (print_value (d + 1)) vs) ^ ")"
  and print_array_value d block =
    let r = Memory.read block in
    let n = Mint.to_int (Memory.size block) in
    "[ "
    ^ String.concat ", "
        List.(
          map
            (fun i -> print_value (d + 1) (r (Mint.of_int i)))
            (ExtStd.List.range 0 (n - 1)))
    ^ " ]"
  in
  print_value 0 v

let print_values m vs = String.concat "; " (List.map (print_value m) vs)

module Environment : sig
  type t
  (** Evaluation environments map identifiers to values. *)

  val empty : t
  (** The empty environment. *)

  val bind : t -> identifier -> t gvalue -> t
  (** [bind env x v] extends [env] with a binding from [x] to [v]. *)

  val update : Position.t -> identifier -> t -> t gvalue -> unit
  (** [update pos x env v] modifies the binding of [x] in [env] so
      that [x ↦ v] ∈ [env]. *)

  val lookup : Position.t -> identifier -> t -> t gvalue
  (** [lookup pos x env] returns [v] such that [x ↦ v] ∈ env. *)

  exception UnboundIdentifier of identifier * Position.t
  (** [UnboundIdentifier (x, pos)] is raised when [update] or
      [lookup] assume that there is a binding for [x] in [env],
      where there is no such binding. *)

  val last : t -> (identifier * t gvalue * t) option
  (** [last env] returns the latest binding in [env] if it exists. *)

  val print : t gvalue Memory.t -> t -> string
  (** [print env] returns a human readable representation of [env]. *)
end = struct
  type t = EEmpty | EBind of identifier * t gvalue ref * t

  let empty = EEmpty

  let bind e x v = EBind (x, ref v, e)

  exception UnboundIdentifier of identifier * Position.t

  let lookup' pos x =
    let rec aux = function
      | EEmpty          -> raise (UnboundIdentifier (x, pos))
      | EBind (y, v, e) -> if x = y then v else aux e
    in
    aux

  let lookup pos x e = !(lookup' pos x e)

  let update pos x e v = lookup' pos x e := v

  let last = function EBind (x, v, e) -> Some (x, !v, e) | EEmpty -> None

  let print_binding m (Id x, v) = x ^ " = " ^ print_value m !v

  let print m e =
    let b = Buffer.create 13 in
    let push x v = Buffer.add_string b (print_binding m (x, v)) in
    let rec aux = function
      | EEmpty               -> Buffer.contents b
      | EBind (x, v, EEmpty) ->
          push x v;
          aux EEmpty
      | EBind (x, v, e)      ->
          push x v;
          Buffer.add_string b "\n";
          aux e
    in
    aux e
end

type value = Environment.t gvalue
(**
    We have everything we need now to define [value] as an instantiation
    of ['e gvalue] with ['e = Environment.t], as promised.
*)

(**
   The following higher-order function lifts a function [f] of type
   ['a -> 'b] as a [name]d Hopix primitive function, that is, an
   OCaml function of type [value -> value].
*)
let primitive name ?(error = fun () -> assert false) coercion wrapper f : value
    =
  VPrimitive
    ( name,
      fun x ->
        match coercion x with None -> error () | Some x -> wrapper (f x) )

type runtime = { memory : value Memory.t; environment : Environment.t }

type observable = {
  new_memory : value Memory.t;
  new_environment : Environment.t;
}

(** [primitives] is an environment that contains the implementation
    of all primitives (+, <, ...). *)
let primitives =
  let intbin name out op =
    let error m v =
      Printf.eprintf "Invalid arguments for `%s': %s\n" name (print_value m v);
      assert false
      (* By typing. *)
    in
    VPrimitive
      ( name,
        fun m -> function
          | VInt x ->
              VPrimitive
                ( name,
                  fun m -> function VInt y -> out (op x y) | v -> error m v )
          | v      -> error m v )
  in
  let bind_all what l x =
    List.fold_left
      (fun env (x, v) -> Environment.bind env (Id x) (what x v))
      x l
  in
  (* Define arithmetic binary operators. *)
  let binarith name = intbin name (fun x -> VInt x) in
  let binarithops =
    Mint.[ ("`+`", add); ("`-`", sub); ("`*`", mul); ("`/`", div) ]
  in
  (* Define arithmetic comparison operators. *)
  let cmparith name = intbin name bool_as_value in
  let cmparithops =
    [
      ("`=?`", ( = ));
      ("`<?`", ( < ));
      ("`>?`", ( > ));
      ("`>=?`", ( >= ));
      ("`<=?`", ( <= ));
    ]
  in
  let boolbin name out op =
    VPrimitive
      ( name,
        fun _ x ->
          VPrimitive
            (name, fun _ y -> out (op (value_as_bool x) (value_as_bool y))) )
  in
  let boolarith name = boolbin name (fun x -> if x then ptrue else pfalse) in
  let boolarithops = [ ("`||`", ( || )); ("`&&`", ( && )) ] in
  let generic_printer =
    VPrimitive
      ( "print",
        fun m v ->
          output_string stdout (print_value m v);
          flush stdout;
          VUnit )
  in
  let print s =
    output_string stdout s;
    flush stdout;
    VUnit
  in
  let print_int =
    VPrimitive
      ( "print_int",
        fun _ -> function
          | VInt x -> print (Mint.to_string x)
          | _      -> assert false
        (* By typing. *) )
  in
  let print_string =
    VPrimitive
      ( "print_string",
        fun _ -> function VString x -> print x | _ -> assert false
        (* By typing. *) )
  in
  let bind' x w env = Environment.bind env (Id x) w in
  Environment.empty
  |> bind_all binarith binarithops
  |> bind_all cmparith cmparithops
  |> bind_all boolarith boolarithops
  |> bind' "print" generic_printer
  |> bind' "print_int" print_int
  |> bind' "print_string" print_string
  |> bind' "true" ptrue |> bind' "false" pfalse |> bind' "nothing" VUnit

let initial_runtime () =
  {
    memory = Memory.create (640 * 1024 (* should be enough. -- B.Gates *));
    environment = primitives;
  }

let rec evaluate runtime ast =
  try
    let runtime' = List.fold_left definition runtime ast in
    (runtime', extract_observable runtime runtime')
  with Environment.UnboundIdentifier (Id x, pos) ->
    Error.error "interpretation" pos (Printf.sprintf "`%s' is unbound." x)

(** [definition pos runtime d] evaluates the new definition [d]
    into a new runtime [runtime']. In the specification, this
    is the judgment:

                        E, M ⊢ dv ⇒ E', M'

*)
and definition runtime ({ value = d; position = p } : HopixAST.elt) =
  match d with
  | DefineType _    -> runtime
  | DeclareExtern _ -> failwith "todo"
  | DefineValue def -> (
      match def with
      | SimpleValue (id, _, e_loc) ->
          let v = expression p runtime.environment runtime.memory e_loc.value in
          {
            environment = Environment.bind runtime.environment id.value v;
            memory = runtime.memory;
          }
      | RecFunctions _             -> failwith "todo" )

and expression' environment memory e : value =
  expression (position e) environment memory (value e)

(** [expression pos runtime e] evaluates into a value [v] if

                          E, M ⊢ e ⇓ v, M'

   and E = [runtime.environment], M = [runtime.memory].
*)
and expression pos environment memory e : value =
  match e with
  | Literal lit -> eval_literal environment memory lit.value
  | Variable (id, _) -> eval_variable environment memory id
  | Tagged (constr, _, e) -> eval_tagged environment memory constr.value e
  | Record (l, _) -> eval_record environment memory l
  | Field (expr, label) -> eval_field environment memory (expr, label)
  | Tuple exprs -> eval_tuple environment memory exprs
  | Sequence seq -> eval_sequence environment memory seq
  | Define (value_definition, expression) ->
      eval_define environment memory (value_definition, expression)
  | Fun (FunctionDefinition (arg_p, body)) ->
      eval_fun environment memory (arg_p, body)
  | Apply (f, arg) -> eval_apply environment memory (f, arg)
  | Ref expression -> eval_ref environment memory expression
  | Assign (e1, e2) -> eval_assign environment memory (e1, e2)
  | Read expression -> eval_read environment memory expression
  | Case (expr, branches) -> eval_case environment memory (expr.value, branches)
  | IfThenElse (cond, body, body_else) ->
      eval_if_then_else environment memory (cond, body, body_else)
  | While (cond, body) -> eval_while environment memory (cond, body)
  | For (id, bound_low, bound_high, body) ->
      eval_for environment memory (id, bound_low, bound_high, body)
  | TypeAnnotation (e, _) -> expression' environment memory e

(** This function returns the difference between two runtimes. *)
and extract_observable runtime runtime' =
  let rec substract new_environment env env' =
    if env == env' then new_environment
    else
      match Environment.last env' with
      | None              -> assert false (* Absurd. *)
      | Some (x, v, env') ->
          let new_environment = Environment.bind new_environment x v in
          substract new_environment env env'
  in
  {
    new_environment =
      substract Environment.empty runtime.environment runtime'.environment;
    new_memory = runtime'.memory;
  }

and eval_literal env mem lit : value =
  match lit with
  | LInt i    -> VInt i
  | LString s -> VString s
  | LChar c   -> VChar c

and eval_variable env mem id : value =
  Environment.lookup id.position id.value env

and eval_tagged env mem var = failwith "todo"

and eval_record env mem  = function 
| [] -> failwith "record must not be empty"
| [(_,f)] -> expression' env mem f
| (_,_)::es -> eval_record env mem es 

and eval_field env mem (expr,label) = 
  let label_name label =
    match label with LId(s)-> s
  in
match value_as_record (expression' env mem expr) with
| None -> failwith "field access must be done on a record"
| Some fs -> snd (List.find (fun (lab, value) -> String.equal (label_name lab) (label_name label.value)) fs)

and eval_tuple env mem tuple =
  match tuple with
  | [] | [ _; _ ] -> failwith "tuple should be at least length 2"
  | _             -> VTuple (List.map (expression' env mem) tuple)

and eval_sequence env mem seq =
  match seq with
  | []      -> failwith "sequence must not be empty"
  | [ e ]   -> expression' env mem e
  | e :: es ->
      let _ = expression' env mem e in
      eval_sequence env mem es

and eval_define env mem (arg_p, body) = failwith "todo"


and eval_fun env mem (p_arg, body) = VClosure (env, p_arg, body)

and eval_apply env mem (f, args) =
  let f_value = expression' env mem f in
  let arg_value = expression' env mem args in
  match value_as_closure_or_primitive f_value with
  | Some (VPrimitive (_, f)) -> f mem arg_value
  | Some (VClosure (env, pattern, body)) -> (
      match bind_value_to_pattern env pattern arg_value with
      | Some bound_env -> expression' bound_env mem body
      | None           -> failwith "pattern and function arg did not match" )
  | Some _ -> failwith "unreacheable"
  | None -> failwith "type error"

and eval_ref env mem ref =
  let v = expression' env mem ref in
  VLocation (Memory.allocate mem Int64.one v)

and eval_assign env mem (e1, e2) =
  let v1 = expression' env mem e1 in
  match value_as_location v1 with
  | None     -> failwith "type error"
  | Some loc ->
      Memory.write
        (Memory.dereference mem loc)
        Int64.zero (expression' env mem e2);
      VUnit

and eval_read env mem read =
  let v = expression' env mem read in
  match value_as_location v with
  | None     -> failwith "type error"
  | Some loc -> Memory.read (Memory.dereference mem loc) Int64.zero

and eval_case env mem case = failwith "todo"

and eval_if_then_else env mem ifthenelse = failwith "todo"

and eval_while env mem while_ = failwith "todo"

and eval_for env mem for_ = failwith "todo"

and bind_value_to_pattern env pattern value =
  match pattern.value with
  | PVariable id -> Some (Environment.bind env id.value value)
  | PWildcard -> Some env
  | PTypeAnnotation (pattern, _) -> bind_value_to_pattern env pattern value
  | PLiteral literal -> (
      match (literal.value, value) with
      | LInt i, VInt i'       -> if i = i' then Some env else None
      | LChar c, VChar c'     -> if c = c' then Some env else None
      | LString s, VString s' -> if s = s' then Some env else None
      | _, _                  -> None )
  | PTaggedValue (constructor, _, patterns) -> (
      match value with
      | VTagged (constructor', values) ->
          if constructor.value = constructor' then
            bind_value_to_pattern env
              (with_pos dummy (PTuple patterns))
              (VTuple values)
          else None
      | _ -> None )
  | PRecord (fields, _) -> failwith "todo"
  | PTuple patterns -> (
      match (patterns, value_as_tuple value) with
      | _, None                 -> None
      | [], _ | _, Some []      -> failwith "empty tuple pattern"
      | [ p ], Some [ v ]       -> bind_value_to_pattern env p v
      | p :: ps, Some (v :: vs) -> (
          match bind_value_to_pattern env p v with
          | None      -> None
          | Some env' ->
              bind_value_to_pattern env'
                (with_pos dummy (PTuple ps))
                (VTuple vs) ) )
  | POr patterns -> (
      match
        List.find_opt
          (function Some _ -> true | None -> false)
          (List.map
             (fun pattern -> bind_value_to_pattern env pattern value)
             patterns)
      with
      | Some _ -> Some env
      | None   -> None )
  | PAnd patterns ->
      if
        List.for_all
          (function Some _ -> true | None -> false)
          (List.map
             (fun pattern -> bind_value_to_pattern env pattern value)
             patterns)
      then Some env
      else None

(** This function displays a difference between two runtimes. *)
let print_observable (_ : runtime) observation =
  Environment.print observation.new_memory observation.new_environment
