(** This module implements a compiler from Retrolix to X86-64 *)

(** In more details, this module performs the following tasks:
   - turning accesses to local variables and function parameters into stack
     loads and stores ;
   - generating initialization code and reserving space in the .data section for
     global variables ;
   - reserving space in the .data section for literal strings.
 *)

(* TODO tail recursion *)

let list_option_get = List.filter_map Fun.id

let string_of_src : X86_64_AST.src -> string =
  X86_64_PrettyPrinter.to_string X86_64_PrettyPrinter.operand

let string_of_dst : X86_64_AST.dst -> string =
  X86_64_PrettyPrinter.to_string X86_64_PrettyPrinter.operand

let string_of_cc = X86_64_PrettyPrinter.to_string X86_64_PrettyPrinter.condcode

let error ?(pos = Position.dummy) msg = Error.error "compilation" pos msg

module Source = Retrolix
(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)

module Target = X86_64
module S = Source.AST
module T = Target.AST

module Str = struct
  type t = string

  let compare = Stdlib.compare
end

module StrMap = Map.Make (Str)
module StrSet = Set.Make (Str)

(** {2 Low-level helpers} *)

let scratchr = X86_64_Architecture.scratch_register

let scratch = `Reg scratchr

let rsp = `Reg X86_64_Architecture.RSP

let rbp = `Reg X86_64_Architecture.RBP

let rdi = `Reg X86_64_Architecture.RDI

let rax = `Reg X86_64_Architecture.RAX

let rdx = `Reg X86_64_Architecture.RDX

(** [align n b] returns the smallest multiple of [b] larger than [n]. *)
let align n b =
  let m = n mod b in
  if m = 0 then n else n + b - m

(** {2 Label mangling and generation} *)

let hash x = string_of_int (Hashtbl.hash x)

let label_for_string_id id = ".S_" ^ string_of_int id

let label_of_retrolix_label (s : string) = s

let label_of_function_identifier (S.FId s) = label_of_retrolix_label s

let data_label_of_global (S.Id s) = label_of_retrolix_label s

let init_label_of_global (xs : S.identifier list) = ".I_" ^ hash xs

let label_of_internal_label_id (id : T.label) = ".X_" ^ id

let fresh_label : unit -> T.label =
  let r = ref 0 in
  fun () ->
    incr r;
    label_of_internal_label_id (string_of_int !r)

let fresh_string_label : unit -> string =
  let r = ref 0 in
  fun () ->
    let n = !r in
    incr r;
    label_for_string_id n

(** {2 Environments} *)

type environment = {
  externals : S.FIdSet.t;
      (** All the external functions declared in the retrolix program. *)
  globals : S.IdSet.t;
      (** All the global variables found in the Retrolix program, each with a
        unique integer. *)
  data_lines : T.line list;
      (** All the lines to be added to the .data section of the complete file. *)
}

let make_environment ~externals ~globals () =
  let open T in
  let data_lines =
    S.IdSet.fold
      (fun (S.Id id_s as id) lines ->
        Label (data_label_of_global id)
        :: Instruction (Comment id_s)
        :: Directive (Quad [ Lit Mint.zero ])
        :: lines)
      globals []
  in

  let data_lines =
    S.FIdSet.fold
      (fun (S.FId f) lines -> Directive (Extern f) :: lines)
      externals data_lines
  in

  { externals; globals; data_lines }

let is_external env (f : S.rvalue) =
  match f with
  | `Immediate (S.LFun f) -> S.FIdSet.mem f env.externals
  | _                     -> false

let is_global env f = S.IdSet.mem f env.globals

let register_string s env =
  let open T in
  let l = fresh_string_label () in
  ( l,
    { env with data_lines = Label l :: Directive (String s) :: env.data_lines }
  )

(* The following function is here to please Flap's architecture. *)
let initial_environment () =
  make_environment ~externals:S.FIdSet.empty ~globals:S.IdSet.empty ()

let register_globals global_set env =
  let open T in
  let globals, data_lines =
    S.IdSet.fold
      (fun (S.Id id_s as id) (globals, lines) ->
        ( S.IdSet.add id globals,
          Label (data_label_of_global id)
          :: Instruction (Comment id_s)
          :: Directive (Quad [ Lit Mint.zero ])
          :: lines ))
      global_set
      (S.IdSet.empty, env.data_lines)
  in
  { env with globals; data_lines }

(** {2 Abstract instruction selectors and calling conventions} *)

module type InstructionSelector = sig
  val mov : dst:T.dst -> src:T.src -> T.line list
  (** [mov ~dst ~src] generates the x86-64 assembly listing to copy [src] into
        [dst]. *)

  val add : dst:T.dst -> srcl:T.src -> srcr:T.src -> T.line list
  (** [add ~dst ~srcl ~srcr] generates the x86-64 assembly listing to store
        [srcl + srcr] into [dst]. *)

  val sub : dst:T.dst -> srcl:T.src -> srcr:T.src -> T.line list
  (** [sub ~dst ~srcl ~srcr] generates the x86-64 assembly listing to store
        [srcl - srcr] into [dst]. *)

  val mul : dst:T.dst -> srcl:T.src -> srcr:T.src -> T.line list
  (** [mul ~dst ~srcl ~srcr] generates the x86-64 assembly listing to store
        [srcl * srcr] into [dst]. *)

  val div : dst:T.dst -> srcl:T.src -> srcr:T.src -> T.line list
  (** [div ~dst ~srcl ~srcr] generates the x86-64 assembly listing to store
        [srcl / srcr] into [dst]. *)

  val andl : dst:T.dst -> srcl:T.src -> srcr:T.src -> T.line list
  (** [andl ~dst ~srcl ~srcr] generates the x86-64 assembly listing to store
        [srcl & srcr] into [dst]. *)

  val orl : dst:T.dst -> srcl:T.src -> srcr:T.src -> T.line list
  (** [orl ~dst ~srcl ~srcr] generates the x86-64 assembly listing to store
        [srcl | srcr] into [dst]. *)

  val conditional_jump :
    cc:T.condcode ->
    srcl:T.src ->
    srcr:T.src ->
    ll:T.label ->
    lr:T.label ->
    T.line list
  (** [conditional_jump ~cc ~srcl ~srcr ~ll ~lr] generates the x86-64 assembly
        listing to test whether [srcl, srcr] satisfies the relation described by
        [cc] and jump to [ll] if they do or to [lr] when they do not. *)

  val switch :
    ?default:T.label -> discriminant:T.src -> cases:T.label array -> T.line list
  (** [switch ~default ~discriminant ~cases] generates the x86-64 assembly
       listing to jump to [cases.(discriminant)], or to the (optional) [default]
       label when discriminant is larger than [Array.length cases].

       The behavior of the program is undefined if [discriminant < 0], or if
       [discriminant >= Array.length cases] and no [default] has been given. *)
end

module type FrameManager = sig
  type frame_descriptor
  (** The abstract data structure holding the information necessary to
        implement the calling convention. *)

  val frame_descriptor :
    params:S.identifier list -> locals:S.identifier list -> frame_descriptor
  (** Generate a frame descriptor for the function with parameter [params] and
        locals [locals]. *)

  val location_of : frame_descriptor -> S.identifier -> T.address
  (** [location_of fd v] computes the address of [v] according to the frame
        descriptor [fd]. Note that [v] might be a local variable, a function
        parameter, or a global variable. *)

  val function_prologue : frame_descriptor -> T.line list
  (** [function_prologue fd] generates the x86-64 assembly listing to setup
        a stack frame according to the frame descriptor [fd]. *)

  val function_epilogue : frame_descriptor -> T.line list
  (** [function_epilogue fd] generates the x86-64 assembly listing to setup a
        stack frame according to the frame descriptor [fd]. *)

  val call :
    frame_descriptor ->
    kind:[ `Normal | `Tail ] ->
    f:T.src ->
    args:T.src list ->
    T.line list
  (** [call fd ~kind ~f ~args] generates the x86-64 assembly listing to setup
        a call to the function at [f], with arguments [args], with [kind]
        specifying whether this should be a normal or tail call.  *)
end

(** {2 Code generator} *)

(** This module implements an x86-64 code generator for Retrolix using the
    provided [InstructionSelector] and [FrameManager].  *)
module Codegen (IS : InstructionSelector) (FM : FrameManager) = struct
  let translate_label (S.Label l) = label_of_retrolix_label l

  let translate_variable fd v = `Addr (FM.location_of fd v)

  let translate_literal lit env =
    match lit with
    | S.LInt i    -> (T.Lit i, env)
    | S.LFun f    -> (T.Lab (label_of_function_identifier f), env)
    | S.LChar c   -> (T.Lit (Mint.of_int @@ Char.code c), env)
    | S.LString s ->
        let l, env = register_string s env in
        (T.Lab l, env)

  let translate_register (S.RId s) = X86_64_Architecture.register_of_string s

  let translate_lvalue fi lv =
    match lv with
    | `Variable v   -> translate_variable fi v
    | `Register reg -> `Reg (translate_register reg)

  let translate_rvalue fi rv env =
    match rv with
    | `Immediate lit ->
        let lit, env = translate_literal lit env in
        (`Imm lit, env)
    | (`Variable _ | `Register _) as lv -> (translate_lvalue fi lv, env)

  let translate_rvalues fi rvs env =
    List.fold_right
      (fun rv (rvs, env) ->
        let rv, env = translate_rvalue fi rv env in
        (rv :: rvs, env))
      rvs ([], env)

  let translate_label_to_operand (S.Label l) = `Imm (T.Lab l)

  let translate_cond cond =
    match cond with
    | S.GT  -> T.G
    | S.LT  -> T.L
    | S.GTE -> T.GE
    | S.LTE -> T.LE
    | S.EQ  -> T.E

  let translate_instruction fd ins env : T.line list * environment =
    let open T in
    match ins with
    | S.Call (f, args, is_tail) ->
        let kind = if is_tail then `Tail else `Normal in
        let f, env = translate_rvalue fd f env in
        let args, env = translate_rvalues fd args env in
        (FM.call fd ~kind ~f ~args, env)
    | S.Assign (dst, op, args) ->
        let dst = translate_lvalue fd dst in
        let args, env = translate_rvalues fd args env in
        let inss =
          match (op, args) with
          | S.Add, [ srcl; srcr ] -> IS.add ~dst ~srcl ~srcr
          | S.Sub, [ srcl; srcr ] -> IS.sub ~dst ~srcl ~srcr
          | S.Mul, [ srcl; srcr ] -> IS.mul ~dst ~srcl ~srcr
          | S.Div, [ srcl; srcr ] -> IS.div ~dst ~srcl ~srcr
          | S.And, [ srcl; srcr ] -> IS.andl ~dst ~srcl ~srcr
          | S.Or, [ srcl; srcr ]  -> IS.orl ~dst ~srcl ~srcr
          | S.Copy, [ src ]       -> IS.mov ~dst ~src
          | _                     -> error "Unknown operator or bad arity"
        in
        (inss, env)
    | S.Ret -> (FM.function_epilogue fd @ insns [ T.Ret ], env)
    | S.Jump l -> (insns [ T.jmpl ~tgt:(translate_label l) ], env)
    | S.ConditionalJump (cond, args, ll, lr) ->
        let cc = translate_cond cond in
        let srcl, srcr, env =
          match args with
          | [ src1; src2 ] ->
              let src1, env = translate_rvalue fd src1 env in
              let src2, env = translate_rvalue fd src2 env in
              (src1, src2, env)
          | _              -> failwith
                                "translate_exp: conditional jump with invalid \
                                 arity"
        in
        ( IS.conditional_jump ~cc ~srcl ~srcr ~ll:(translate_label ll)
            ~lr:(translate_label lr),
          env )
    | S.Switch (discriminant, cases, default) ->
        let discriminant, env = translate_rvalue fd discriminant env in
        let cases = Array.map translate_label cases in
        let default = ExtStd.Option.map translate_label default in
        (IS.switch ?default ~discriminant ~cases, env)
    | S.Comment s -> (insns [ Comment s ], env)
    | S.Exit ->
        ( IS.mov ~src:(liti 0) ~dst:rdi
          @ FM.call fd ~kind:`Normal ~f:(`Imm (Lab "exit")) ~args:[],
          env )

  let translate_labelled_instruction fi (body, env) (l, ins) =
    let ins, env = translate_instruction fi ins env in
    (List.rev ins @ (T.Label (translate_label l) :: body), env)

  let translate_labelled_instructions fi env inss =
    let inss, env =
      List.fold_left (translate_labelled_instruction fi) ([], env) inss
    in
    (List.rev inss, env)

  let translate_fun_def ~name ?(desc = "") ~params ~locals gen_body =
    let open T in
    let fd = FM.frame_descriptor ~params ~locals in

    let prologue = FM.function_prologue fd in

    let body, env = gen_body fd in

    ( Directive (PadToAlign { pow = 3; fill = 0x90 })
      :: Label name
      :: (if desc = "" then prologue else Instruction (Comment desc) :: prologue)
      @ body,
      env )

  let translate_block ~name ?(desc = "") ~params (locals, body) env =
    translate_fun_def ~name ~desc ~params ~locals (fun fi ->
        translate_labelled_instructions fi env body)

  let translate_definition def (body, env) =
    match def with
    | S.DValues (xs, block) ->
        let ids = ExtPPrint.to_string RetrolixPrettyPrinter.identifiers xs in
        let name = init_label_of_global xs in
        let def, env =
          translate_block ~name
            ~desc:("Initializer for " ^ ids ^ ".")
            ~params:[] block env
        in
        (def @ body, env)
    | S.DFunction ((S.FId id as f), params, block) ->
        let def, env =
          translate_block
            ~desc:("Retrolix function " ^ id ^ ".")
            ~name:(label_of_function_identifier f)
            ~params block env
        in
        (def @ body, env)
    | S.DExternalFunction (S.FId id) -> (T.(Directive (Extern id)) :: body, env)

  let generate_main _ p =
    let open T in
    let body =
      List.rev
        [
          Directive (PadToAlign { pow = 3; fill = 0x90 });
          Label "main";
          Instruction (Comment "Program entry point.");
          Instruction (T.subq ~src:(`Imm (Lit 8L)) ~dst:rsp);
        ]
    in

    (* Call all initialization stubs *)
    let body =
      let call body def =
        match def with
        | S.DValues (ids, _) ->
            let l = init_label_of_global ids in
            Instruction (T.calld ~tgt:(Lab l)) :: body
        | S.DFunction _ | S.DExternalFunction _ -> body
      in
      List.fold_left call body p
    in

    let body =
      T.insns [ T.calld ~tgt:(Lab "exit"); T.movq ~src:(liti 0) ~dst:rdi ]
      @ body
    in

    Directive (Global "main") :: List.rev body

  (** [translate p env] turns a Retrolix program into a X86-64 program. *)
  let translate (p : S.t) (env : environment) : T.t * environment =
    let env = register_globals (S.globals p) env in
    let pt, env = List.fold_right translate_definition p ([], env) in
    let main = generate_main env p in
    let p =
      (T.data_section :: env.data_lines) @ (T.text_section :: main) @ pt
    in
    (T.remove_unused_labels p, env)
end

(** {2 Concrete instructions selectors and calling conventions} *)

module InstructionSelector : InstructionSelector = struct
  open T

  let mov ~(dst : dst) ~(src : src) =
    Instruction
      (Comment
         (Printf.sprintf "mov ~dst:%S ~src:%S" (string_of_dst dst)
            (string_of_src src)))
    ::
    ( match (dst, src) with
    | (`Addr _ as dst), (`Addr _ as src) ->
        [
          Instruction (movq ~src ~dst:scratch);
          Instruction (movq ~src:scratch ~dst);
        ]
    | _ -> [ Instruction (movq ~dst ~src) ] )
    @ [ Instruction (Comment "end mov") ]

  let bin ins ~dst ~srcl ~srcr = failwith "Students! This is your job! 3"

  let add ~dst ~srcl ~srcr =
    [
      Instruction
        (Comment
           (Printf.sprintf "add ~dst:%S ~srcl:%S ~srcr:%S" (string_of_dst dst)
              (string_of_src srcl) (string_of_src srcr)));
      Instruction (zeroq scratch);
      Instruction (addq ~src:srcr ~dst:scratch);
      Instruction (addq ~src:srcl ~dst:scratch);
      Instruction (movq ~src:scratch ~dst);
      Instruction (Comment "end add");
    ]

  let sub ~(dst : T.dst) ~(srcl : T.src) ~(srcr : T.src) =
    [
      Instruction
        (Comment
           (Printf.sprintf "sub ~dst:%S ~srcl:%S ~srcr:%S" (string_of_dst dst)
              (string_of_src srcl) (string_of_src srcr)));
      Instruction (zeroq scratch);
      Instruction (subq ~src:srcr ~dst:scratch);
      Instruction (addq ~src:srcl ~dst:scratch);
      Instruction (movq ~src:scratch ~dst);
      Instruction (Comment "end sub");
    ]

  let mul ~dst ~srcl ~srcr =
    [
      Instruction
        (Comment
           (Printf.sprintf "mul ~dst:%S ~srcl:%S ~srcr:%S" (string_of_dst dst)
              (string_of_src srcl) (string_of_src srcr)));
      Instruction (zeroq scratch);
      Instruction (incq ~dst:scratch);
      Instruction (imulq ~src:srcl ~dst:scratch);
      Instruction (imulq ~src:srcr ~dst:scratch);
      Instruction (movq ~src:scratch ~dst);
      Instruction (Comment "end mul");
    ]

  let div ~dst ~srcl ~srcr =
    [
      Instruction
        (Comment
           (Printf.sprintf "div ~dst:%S ~srcl:%S ~srcr:%S" (string_of_dst dst)
              (string_of_src srcl) (string_of_src srcr)));
      Instruction (pushq ~src:rax);
      Instruction (pushq ~src:rdx);
      Instruction (zeroq rdx);
      Instruction (movq ~src:srcl ~dst:rax);
      Instruction (movq ~src:srcr ~dst:scratch);
      Instruction (idivq ~src:scratch);
      Instruction (movq ~src:rax ~dst);
      Instruction (popq ~dst:rdx);
      Instruction (popq ~dst:rax);
    ]

  let andl ~dst ~srcl ~srcr =
    [
      Instruction
        (Comment
           (Printf.sprintf "andl ~dst:%S ~srcl:%S ~srcr:%S" (string_of_dst dst)
              (string_of_src srcl) (string_of_src srcr)));
      Instruction (zeroq scratch);
      Instruction (incq ~dst:scratch);
      Instruction (andq ~src:srcr ~dst:scratch);
      Instruction (andq ~src:srcl ~dst:scratch);
      Instruction (movq ~src:scratch ~dst);
      Instruction (Comment "end andl");
    ]

  let orl ~(dst : dst) ~(srcl : src) ~(srcr : src) =
    [
      Instruction
        (Comment
           (Printf.sprintf "orl ~dst:%S ~srcl:%S ~srcr:%S" (string_of_dst dst)
              (string_of_src srcl) (string_of_src srcr)));
      Instruction (zeroq scratch);
      Instruction (orq ~src:srcr ~dst:scratch);
      Instruction (orq ~src:srcl ~dst:scratch);
      Instruction (movq ~src:scratch ~dst);
      Instruction (Comment "end orl");
    ]

  (*val conditional_jump :
    cc:T.condcode ->
    srcl:T.src -> srcr:T.src -> ll:string -> lr:string -> T.line list*)
  let conditional_jump ~(cc : T.condcode) ~(srcl : T.src) ~(srcr : T.src)
      ~(ll : string) ~(lr : string) =
    [
      Instruction
        (Comment
           (Printf.sprintf
              "conditional_jump ~cc:%S ~srcl:%S ~srcr:%S ~ll:%S ~lr:%S"
              (string_of_cc cc) (string_of_src srcl) (string_of_src srcr) ll lr));
      Instruction (movq ~src:srcl ~dst:scratch);
      Instruction (cmpq ~src1:srcr ~src2:scratch);
      Instruction (jcc ~cc ~tgt:(Lab ll));
      Instruction (jmpd ~tgt:(Lab lr));
      Instruction (Comment "end conditional_jump");
    ]

  (*val switch :
    ?default:string ->
    discriminant:T.src -> cases:string array -> T.line list*)
  let switch ?(default : string option) ~(discriminant : src)
      ~(cases : string array) =
    List.concat
      ( Array.to_list
          (Array.mapi
             (fun i case ->
               [
                 Instruction
                   (cmpq ~src2:discriminant ~src1:(`Imm (Lit (Mint.of_int i))));
                 Instruction (jccl ~cc:E ~tgt:case);
               ])
             cases)
      @
      match default with
      | None         -> []
      | Some default -> [ [ Instruction (jmpl ~tgt:default) ] ] )
end

module FrameManager (IS : InstructionSelector) : FrameManager = struct
  type frame_descriptor = {
    param_count : int;  (** Number of parameters. *)
    locals_space : int;
        (** Amount of space dedicated to local variables in the stack frame. *)
    stack_map : Mint.t S.IdMap.t;
        (** Maps stack-allocated variable names to stack slots expressed as
            frame-pointer relative offsets. *)
  }

  (** [empty_frame fd] returns [true] if and only if the stack frame described
        by [fd] is empty. *)
  let empty_frame fd = fd.param_count = 0 && fd.locals_space = 0

  (** [stack_usage_after_prologue fd] returns the size, in bytes, of the stack
        space after the function prologue. *)
  let stack_usage_after_prologue fd =
    Mint.size_in_bytes
    + ((if empty_frame fd then 0 else 1) * Mint.size_in_bytes)
    + fd.locals_space

  (*val frame_descriptor :
    params:S.identifier list -> locals:S.identifier list -> frame_descriptor*)
  let frame_descriptor ~(params : S.identifier list)
      ~(locals : S.identifier list) : frame_descriptor =
    (* Student! Implement me! *)
    {
      param_count = List.length params;
      locals_space = 8 * List.length locals;
      stack_map =
        S.IdMap.add_seq
          (List.to_seq
             (List.mapi (fun i id -> (id, Int64.of_int ((i + 2) * -8))) params))
          (S.IdMap.add_seq
             (List.to_seq
                (List.mapi
                   (fun i id -> (id, Int64.of_int ((i + 1) * 8)))
                   locals))
             S.IdMap.empty);
    }

  (* val location_of : frame_descriptor -> S.identifier -> T.address*)
  let location_of (fd : frame_descriptor) (id : S.identifier) : T.address =
    let addr = S.IdMap.find_opt id fd.stack_map in
    match addr with
    | None      ->
        {
          offset = Some (Lab (match id with Id s -> s));
          base = Some RIP;
          idx = None;
          scale = `One;
        }
    | Some addr ->
        {
          offset = Some (Lit (Int64.neg addr));
          base = Some RBP;
          idx = None;
          scale = `One;
        }

  let function_prologue (fd : frame_descriptor) =
    (* Student! Implement me! *)
    list_option_get
      T.
        [
          Some (Instruction (Comment "start prolog"));
          Some (Instruction (pushq ~src:rbp));
          Some (Instruction (movq ~src:rsp ~dst:rbp));
          ( if fd.locals_space <> 0 then
            Some
              (Instruction
                 (subq ~src:(`Imm (Lit (Mint.of_int fd.locals_space))) ~dst:rsp))
          else None );
          Some (Instruction (Comment "end prolog"));
        ]

  let function_epilogue fd =
    (* Student! Implement me! *)
    list_option_get
      T.
        [
          Some (Instruction (Comment "start epilog"));
          ( if fd.locals_space <> 0 then
            Some
              (Instruction
                 (addq ~src:(`Imm (Lit (Mint.of_int fd.locals_space))) ~dst:rsp))
          else None );
          Some (Instruction (popq ~dst:rbp));
          Some (Instruction (Comment "end epilog"));
        ]

  (*val call :
    frame_descriptor ->
    kind:[ `Normal | `Tail ] -> f:T.src -> args:T.src list -> T.line list*)
  let call (fd : frame_descriptor) ~(kind : [< `Normal | `Tail ]) ~(f : T.src)
      ~(args : T.src list) =
    T.(
      Instruction
        (Comment
           (Printf.sprintf "call ~kind:%S ~f:%S ~args:%S"
              (match kind with `Normal -> "`Normal" | `Tail -> "`Tail")
              (string_of_src f)
              (String.concat ", " (List.map string_of_src args))))
      ::
      ( match kind with
      | `Normal ->
          let alignement = (fd.locals_space + (fd.param_count * 8)) mod 16 in
          List.concat
            [
              ( if alignement <> 0 then
                [
                  Instruction
                    (subq ~src:(`Imm (Lit (Mint.of_int alignement))) ~dst:rsp);
                ]
              else [] );
              List.map (fun arg -> Instruction (pushq ~src:arg)) (List.rev args);
              [ Instruction (calldi ~tgt:f) ];
              ( if alignement <> 0 then
                [
                  Instruction
                    (addq ~src:(`Imm (Lit (Mint.of_int alignement))) ~dst:rsp);
                ]
              else [] );
              List.map (fun _ -> Instruction (popq ~dst:scratch)) args;
              [ Instruction (Comment "end call") ];
            ]
      | `Tail   ->
          let alignement = fd.param_count * 8 mod 16 in
          List.concat
            [
              function_epilogue fd;
              ( if alignement <> 0 then
                [
                  Instruction
                    (subq ~src:(`Imm (Lit (Mint.of_int alignement))) ~dst:rsp);
                ]
              else [] );
              List.map (fun arg -> Instruction (pushq ~src:arg)) (List.rev args);
              [ Instruction (jmpdi ~tgt:f) ];
              ( if alignement <> 0 then
                [
                  Instruction
                    (addq ~src:(`Imm (Lit (Mint.of_int alignement))) ~dst:rsp);
                ]
              else [] );
              List.map (fun _ -> Instruction (popq ~dst:scratch)) args;
              [ Instruction Ret; Instruction (Comment "end call") ];
            ] ))
end

module CG = Codegen (InstructionSelector) (FrameManager (InstructionSelector))

let translate = CG.translate
