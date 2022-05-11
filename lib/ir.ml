type imm = I32 of int32 [@@deriving show]
type id = string [@@deriving show]
type operand = Imm of imm | Id of id [@@deriving show]
type arith_op = Add | Sub | Mul | Div [@@deriving show]
type rel_op = Lt | Le | Eq | Neq | Ge | Gt [@@deriving show]

type stmt =
  | Label of id
  | Function of id
  | Assign of (id * operand)
  | Arith of (id * operand * arith_op * operand)
  | Ref of (id * id)
  | Load of (id * id)
  | Store of (id * operand)
  | Goto of id
  | If of (operand * rel_op * operand * id)
  | Return of operand
  | Dec of (id * int32)
  | Arg of operand
  | Call of (id * id)
  | Param of id
  | Read of id
  | Write of operand
[@@deriving show]

module Parse = struct
  open Angstrom

  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_ws = function ' ' | '\t' -> true | _ -> false

  let is_id_mid (c : char) =
    match c with
    | ':' | '=' | '+' | '-' | '*' | '/' | '&' -> false
    | _ -> not (is_ws c)

  let is_id_begin (c : char) =
    match c with 'a' .. 'z' -> is_id_mid c | _ -> false

  let any_ws = skip_many (satisfy is_ws)

  let peek_wb =
    peek_char >>= function
    | None -> return ()
    | Some c when is_ws c -> return ()
    | _ -> fail "Expected word boundary"

  let i32_p =
    any_ws
    *> ( option '+' (char '+' <|> char '-') >>= fun c ->
         take_while1 is_digit >>= fun x ->
         let s = String.make 1 c ^ x in
         match Int32.of_string_opt s with
         | Some i -> return i
         | None -> fail ("Invalid 32-bit integer: " ^ s) )

  let imm_p = any_ws *> ((fun x -> I32 x) <$> char '#' *> i32_p)

  let id_p =
    any_ws
    *> ((fun c s -> String.make 1 c ^ s)
       <$> satisfy is_id_begin <*> take_while is_id_mid <* peek_wb)

  let operand_p =
    any_ws
    *> ((fun x -> Imm x) <$> imm_p <|> ((fun x -> Id x) <$> id_p) <* peek_wb)

  let arith_op_p =
    any_ws
    *> (string "+" *> return Add
       <|> string "-" *> return Sub
       <|> string "*" *> return Mul
       <|> string "/" *> return Div
       <* peek_wb)

  let rel_op_p =
    any_ws
    *> (string "<=" *> return Le
       <|> string "==" *> return Eq
       <|> string "!=" *> return Neq
       <|> string ">=" *> return Ge
       <|> string "<" *> return Lt
       <|> string ">" *> return Gt
       <* peek_wb)

  let keyword (s : string) = any_ws *> (string s <* peek_wb)
  let label_p = (fun x -> Label x) <$> keyword "LABEL" *> id_p <* keyword ":"

  let function_p =
    (fun x -> Function x) <$> keyword "FUNCTION" *> id_p <* keyword ":"

  let assign_p =
    (fun x y -> Assign (x, y)) <$> id_p <* keyword ":=" <*> operand_p

  let arith_p =
    (fun x y op z -> Arith (x, y, op, z))
    <$> id_p <* keyword ":=" <*> operand_p <*> arith_op_p <*> operand_p

  let ref_p =
    (fun x y -> Ref (x, y))
    <$> id_p <* keyword ":=" <* any_ws <* char '&' <*> id_p

  let load_p =
    (fun x y -> Load (x, y))
    <$> id_p <* keyword ":=" <* any_ws <* char '*' <*> id_p

  let store_p =
    (fun x y -> Store (x, y))
    <$> char '*' *> any_ws *> id_p
    <* keyword ":=" <*> operand_p

  let goto_p = (fun x -> Goto x) <$> keyword "GOTO" *> id_p

  let if_p =
    (fun x op y z -> If (x, op, y, z))
    <$> keyword "IF" *> operand_p
    <*> rel_op_p <*> operand_p <* keyword "GOTO" <*> id_p

  let return_p = (fun x -> Return x) <$> keyword "RETURN" *> operand_p
  let dec_p = (fun x size -> Dec (x, size)) <$> keyword "DEC" *> id_p <*> i32_p
  let arg_p = (fun x -> Arg x) <$> keyword "ARG" *> operand_p

  let call_p =
    (fun x f -> Call (x, f)) <$> id_p <* keyword ":=" <* keyword "CALL" <*> id_p

  let param_p = (fun x -> Param x) <$> keyword "PARAM" *> id_p
  let read_p = (fun x -> Read x) <$> keyword "READ" *> id_p
  let write_p = (fun x -> Write x) <$> keyword "WRITE" *> operand_p

  let stmt_p =
    label_p <|> function_p <|> arith_p <|> call_p <|> ref_p <|> load_p
    <|> store_p <|> assign_p <|> goto_p <|> if_p <|> return_p <|> dec_p
    <|> arg_p <|> param_p <|> read_p <|> write_p

  let stmt_of_string (s : string) : (stmt, string) result =
    parse_string ~consume:All stmt_p (String.trim s)
end

let stmt_of_string = Parse.stmt_of_string

module SymMap = Map.Make (String)

type rel_addr = RelSp of int32 | RelBp of int32 [@@deriving show]

type symbol =
  | Fun of {
      rev_args : id list;
      entry_addr : int32;
      end_addr : int32;
      stack_size : int32;
      arg_size : int32;
    }
  | Var of { of_fun : id; rel_addr : rel_addr; size : int32 }
  | Label of { of_fun : id; entry_addr : int32 }
[@@deriving show]

let pretty_stmt_array f arr =
  let width = String.length @@ string_of_int @@ Array.length arr in
  let pretty i stmt =
    Printf.fprintf f "%*d | %s\n" width (i + 1) @@ show_stmt stmt
  in
  Array.iteri pretty arr

let pretty_symtbl f symtbl =
  let width = SymMap.fold (fun id _ w -> max (String.length id) w) symtbl 0 in
  let pretty id symbol =
    Printf.fprintf f "%-*s | %s\n" width id @@ show_symbol symbol
  in
  SymMap.iter pretty symtbl

type program = { codetbl : stmt array; symtbl : symbol SymMap.t }

let compile seq =
  let module IdSet = Set.Make (String) in
  let module State = struct
    type fstate =
      | FS of {
          param_allowed : bool;
          fn : id;
          rev_args : id list;
          stack_size : int32;
          arg_size : int32;
        }

    type pstate = NextFunc | InFunc of fstate

    type state =
      | State of {
          codetbl : stmt array;
          lineno : int;
          pstate : pstate;
          symtbl : symbol SymMap.t;
          labels : IdSet.t;
          funcs : IdSet.t;
        }

    let initial codetbl =
      State
        {
          codetbl;
          lineno = 0;
          pstate = NextFunc;
          symtbl = SymMap.empty;
          labels = IdSet.empty;
          funcs = IdSet.empty;
        }
  end in
  let open State in
  let module S = Utils.StateEither (struct
    type s = State.state
    type e = string
  end) in
  let open S in
  let open Utils.MonadExtra (S) in
  let get_ps = get >>= function State { pstate; _ } -> pure pstate in
  let put_ps ps =
    get >>= function State st -> put @@ State { st with pstate = ps }
  in
  let get_fstate =
    get_ps >>= function
    | NextFunc -> fail "Expected a function scope"
    | InFunc fs -> pure fs
  in
  let put_fstate fs =
    get_ps >>= function
    | NextFunc -> fail "Expected a function scope"
    | InFunc _ -> put_ps @@ InFunc fs
  in
  let get_lineno = get >>= function State { lineno; _ } -> pure lineno in
  let get_symtbl = get >>= function State { symtbl; _ } -> pure symtbl in
  let put_symtbl s =
    get >>= function State st -> put @@ State { st with symtbl = s }
  in
  let next_line =
    get >>= function
    | State ({ codetbl; lineno; _ } as st) ->
        pure
          (if lineno >= 0 && lineno < Array.length codetbl then
           Some codetbl.(lineno)
          else None)
        <* put @@ State { st with lineno = lineno + 1 }
  in
  let sym_add_fail s a =
    get_symtbl >>= fun symtbl ->
    if SymMap.mem s symtbl then fail @@ "Symbol " ^ s ^ " is already defined"
    else put_symtbl (SymMap.add s a symtbl)
  in
  let sym_update s f =
    get_symtbl >>= fun symtbl -> put_symtbl (SymMap.update s f symtbl)
  in
  let sym_lookup_opt s =
    get_symtbl >>= fun symtbl -> pure @@ SymMap.find_opt s symtbl
  in
  let start_fun s =
    get_lineno >>= fun lineno ->
    sym_add_fail s
    @@ Fun
         {
           rev_args = [];
           entry_addr = Int32.of_int @@ (lineno - 1);
           end_addr = -1l;
           stack_size = 0l;
           arg_size = 0l;
         }
    >> put_ps
       @@ InFunc
            (FS
               {
                 param_allowed = true;
                 fn = s;
                 rev_args = [];
                 stack_size = 0l;
                 arg_size = 0l;
               })
  in
  let end_fun =
    get_fstate >>= function
    | FS { fn; rev_args; stack_size; arg_size; _ } ->
        get_lineno >>= fun lineno ->
        (sym_update fn @@ function
         | Some (Fun f) ->
             Some
               (Fun
                  {
                    f with
                    rev_args;
                    stack_size;
                    arg_size;
                    end_addr = Int32.of_int @@ (lineno - 1);
                  })
         | _ -> failwith "Unexpected case")
        >> put_ps NextFunc
  in
  let grow_stack size =
    get_fstate >>= function
    | FS ({ stack_size; _ } as fs) ->
        put_fstate @@ FS { fs with stack_size = Int32.add stack_size size }
  in
  let dec_var id size =
    if Int32.compare size 0l <= 0 || Int32.rem size 4l <> 0l then
      fail @@ "Variable size must be a positive multiple of 4, not "
      ^ Int32.to_string size
    else
      get_fstate >>= function
      | FS { fn = of_fun; stack_size; _ } ->
          sym_add_fail id @@ Var { of_fun; rel_addr = RelSp stack_size; size }
          >> grow_stack size
  in
  let use_or_new_var id =
    sym_lookup_opt id >>= function
    | Some (Var _) -> pure ()
    | Some _ -> fail @@ "Not a variable: " ^ id
    | None -> dec_var id 4l
  in
  let check_operand x =
    match x with Imm _ -> pure () | Id id -> use_or_new_var id
  in
  let new_param id =
    let size = 4l in
    get_fstate >>= function
    | FS { param_allowed = false; _ } ->
        fail
          "Parameter declaration must immediately follow the corresponding \
           function declaration"
    | FS ({ fn = of_fun; rev_args; arg_size; _ } as fs) ->
        sym_add_fail id @@ Var { of_fun; rel_addr = RelBp arg_size; size }
        >> put_fstate
           @@ FS
                {
                  fs with
                  rev_args = id :: rev_args;
                  arg_size = Int32.add arg_size size;
                }
  in
  let new_label s =
    get_fstate >>= function
    | FS { fn = of_fun; _ } ->
        get_lineno >>= fun lineno ->
        sym_add_fail s
        @@ Label { of_fun; entry_addr = Int32.of_int (lineno - 1) }
  in
  let ref_label id =
    get >>= function
    | State ({ labels; _ } as st) ->
        put @@ State { st with labels = IdSet.add id labels }
  in
  let ref_func id =
    get >>= function
    | State ({ funcs; _ } as st) ->
        put @@ State { st with funcs = IdSet.add id funcs }
  in
  let rec do_compile _ =
    next_line >>= fun mline ->
    get_ps >>= fun ps ->
    (match (ps, mline) with
    | NextFunc, Some (Function id) -> start_fun id
    | NextFunc, Some _ -> fail "Expected function definition"
    | NextFunc, None -> pure ()
    | InFunc _, Some stmt -> (
        (match stmt with
        | Label id -> new_label id
        | Function id -> end_fun >> start_fun id
        | Assign (id, x) -> use_or_new_var id >> check_operand x
        | Arith (id, x, _, y) ->
            use_or_new_var id >> check_operand x >> check_operand y
        | Ref (x, y) -> use_or_new_var x >> use_or_new_var y
        | Load (x, y) -> use_or_new_var x >> use_or_new_var y
        | Store (x, y) -> use_or_new_var x >> check_operand y
        | Goto id -> ref_label id
        | If (x, _, y, id) -> check_operand x >> check_operand y >> ref_label id
        | Return x -> check_operand x
        | Dec (id, size) -> dec_var id size
        | Arg op -> check_operand op
        | Call (x, fn) -> use_or_new_var x >> ref_func fn
        | Param id -> new_param id
        | Read id -> use_or_new_var id
        | Write x -> check_operand x)
        <*
        match stmt with
        | Param _ | Function _ -> pure ()
        | _ -> (
            get_fstate >>= function
            | FS fs -> put_fstate @@ FS { fs with param_allowed = false }))
    | InFunc _, None -> end_fun)
    >> if Option.is_some mline then do_compile () else pure ()
  in
  let final_check = function
    | State { symtbl; labels; funcs; _ } -> (
        match
          List.find_opt
            (fun id ->
              match SymMap.find_opt id symtbl with
              | Some (Label _) -> false
              | _ -> true)
            (IdSet.elements labels)
        with
        | Some id -> Error ("Unresolved label: " ^ id)
        | _ -> (
            match
              List.find_opt
                (fun id ->
                  match SymMap.find_opt id symtbl with
                  | Some (Fun _) -> false
                  | _ -> true)
                (IdSet.elements funcs)
            with
            | Some id -> Error ("Unresolved function: " ^ id)
            | _ -> (
                match SymMap.find_opt "main" symtbl with
                | Some (Fun _) -> Ok ()
                | Some _ -> Error "main is not a function"
                | None -> Error "main function not found")))
  in
  match run (State.initial @@ Array.of_seq seq) (do_compile ()) with
  | Error e, State { lineno; _ } ->
      Error ("Line " ^ string_of_int lineno ^ ": " ^ e)
  | Ok _, (State { codetbl; symtbl; _ } as st) -> (
      match final_check st with
      | Ok _ -> Ok { codetbl; symtbl }
      | Error e -> Error e)

type emu_err_kind =
  | NoProgram
  | BadProgram
  | Segv
  | MemAlign
  | IOErr
  | Unsafe
  | Misc
[@@deriving show]

type emulator_error = { kind : emu_err_kind; text : string option }
[@@deriving show]

type registers = {
  mutable pc : int32;
  mutable sp : int32;
  mutable bp : int32;
  (* Top of argument stack (bottom is sp); initially sp *)
  mutable ap : int32;
}
[@@deriving show]

let regs_of_zero () = { pc = 0l; sp = 0l; bp = 0l; ap = 0l }

let zero_regs regs =
  regs.pc <- 0l;
  regs.sp <- 0l;
  regs.bp <- 0l;
  regs.ap <- 0l

type actv_rec = {
  fn : id;
  (* TODO: maybe use a register to hold return value instead *)
  ret_val_addr : int32;
  saved_regs : registers;
}
[@@deriving show]

exception EmulatorErrorWrapper of emulator_error
exception Halt of int32

let word_size = 4l
let m_fail kind text = raise @@ EmulatorErrorWrapper { kind; text }

class machine ~mem_words ~input ~output =
  object (self)
    val mutable m_regs : registers = regs_of_zero ()
    val mutable m_input : int32 Seq.t = input
    val mutable m_output : int32 -> unit = output
    val m_memory : int32 array = Array.make (Int32.to_int mem_words) 0l
    val mutable m_call_stack : actv_rec list = []
    val mutable m_executed_count : int = 0
    val mutable m_program : program option = None
    val m_addr_lo = 0l
    val m_addr_hi = Int32.mul word_size mem_words

    method private input_i32 () =
      match Seq.uncons m_input with
      | Some (a, input') ->
          m_input <- input';
          a
      | _ -> m_fail IOErr (Some "No more input")

    method private output_i32 v = output v

    method private reset () =
      (* Array.fill m_memory 0 (Array.length m_memory) 0l; *)
      zero_regs m_regs;
      m_regs.sp <- m_addr_hi;
      m_regs.bp <- m_addr_hi;
      m_regs.ap <- m_addr_hi;
      m_executed_count <- 0;
      m_call_stack <- []

    method private get_program : program =
      match m_program with None -> m_fail NoProgram None | Some p -> p

    method private lookup_or_fail id =
      match SymMap.find_opt id self#get_program.symtbl with
      | Some v -> v
      | _ -> m_fail BadProgram @@ Some ("No such symbol: " ^ id)

    method private setup_call id ret_val_addr =
      match self#lookup_or_fail id with
      | Fun { entry_addr; stack_size; _ } ->
          let saved_regs =
            {
              sp = m_regs.sp;
              bp = m_regs.bp;
              pc = m_regs.pc;
              (* Restore ap to sp upon return, so that caller can go on to call other functions *)
              ap = m_regs.sp;
            }
          in
          m_regs.pc <- entry_addr;
          m_regs.bp <- m_regs.ap;
          let new_sp = Int32.sub m_regs.ap stack_size in
          m_regs.sp <- new_sp;
          m_regs.ap <- new_sp;
          m_call_stack <- { fn = id; saved_regs; ret_val_addr } :: m_call_stack
      | _ -> m_fail BadProgram @@ Some ("Not a function: " ^ id)

    method load ?(entry = "main") =
      function
      | { symtbl; _ } as p -> (
          match SymMap.find_opt entry symtbl with
          | Some (Fun _) ->
              m_program <- Some p;
              self#reset ();
              self#setup_call entry 0l
          | Some _ ->
              m_fail BadProgram
              @@ Some (Printf.sprintf "Entry %s is not a function" entry)
          | None ->
              m_fail BadProgram
              @@ Some (Printf.sprintf "Entry function %s not found" entry))

    method private lookup_label id =
      match self#lookup_or_fail id with
      | Label _ as l -> l
      | _ -> m_fail BadProgram @@ Some ("Not a label: " ^ id)

    method private lookup_var id =
      match self#lookup_or_fail id with
      | Var _ as v -> v
      | _ -> m_fail BadProgram @@ Some ("Not a variable: " ^ id)

    method private lookup_func id =
      match self#lookup_or_fail id with
      | Fun _ as f -> f
      | _ -> m_fail BadProgram @@ Some ("Not a function: " ^ id)

    method private current_actv_rec () =
      match m_call_stack with
      | a :: _ -> a
      | _ -> m_fail Misc @@ Some "No activation record available???"

    method private current_actv_rec_with_func () =
      match self#current_actv_rec () with
      | { fn; _ } as ar -> (ar, self#lookup_func fn)

    method private addr_to_idx_checked addr =
      if Int32.rem addr 4l <> 0l then
        m_fail MemAlign
        @@ Some ("Unaligned memory access: " ^ Int32.to_string addr)
      else
        let idx = Int32.to_int (Int32.div addr 4l) in
        if idx < 0 || idx >= Array.length m_memory then
          m_fail Segv @@ Some ("Address out of bound: " ^ Int32.to_string addr)
        else idx

    method private fetch_instr () =
      let codetbl = self#get_program.codetbl in
      let idx = Int32.to_int m_regs.pc in
      if idx < 0 || idx >= Array.length codetbl then
        m_fail Segv @@ Some ("PC out of bound: " ^ Int32.to_string m_regs.pc)
      else
        let instr = codetbl.(idx) in
        m_regs.pc <- Int32.add m_regs.pc 1l;
        instr

    method private read_mem addr = m_memory.(self#addr_to_idx_checked addr)

    method private write_mem addr value =
      m_memory.(self#addr_to_idx_checked addr) <- value

    method private addr_of_var id =
      match self#lookup_var id with
      | Var { rel_addr; _ } -> (
          match rel_addr with
          | RelBp rel -> Int32.add m_regs.bp rel
          | RelSp rel -> Int32.add m_regs.sp rel)
      | _ -> failwith "Impossible"

    (* TODO: add checks for whether variable is in current function *)
    method private read_var id = self#read_mem (self#addr_of_var id)

    (* TODO: add checks for whether variable is in current function *)
    method private write_var id value =
      self#write_mem (self#addr_of_var id) value

    method private eval_operand =
      function Imm (I32 i32) -> i32 | Id id -> self#read_var id

    method private local_jump id =
      match self#lookup_label id with
      | Label { of_fun = src_fun; entry_addr; _ } -> (
          match self#current_actv_rec () with
          | { fn = dest_fun; _ } ->
              if src_fun = dest_fun then m_regs.pc <- entry_addr
              else
                m_fail Unsafe
                @@ Some
                     (Printf.sprintf
                        "Jump from instruction %ld (function %s) to label %s \
                         (%s, %ld) is across functions"
                        m_regs.pc src_fun id dest_fun entry_addr))
      | _ -> failwith "Impossible"

    method private do_arith =
      function
      | Add -> Int32.add
      | Sub -> Int32.sub
      | Mul -> Int32.mul
      | Div -> Int32.div
    (* TODO: warning about division? *)

    method private do_cmp op x y =
      (match op with
      | Lt -> ( < )
      | Le -> ( <= )
      | Eq -> ( = )
      | Neq -> ( <> )
      | Ge -> ( >= )
      | Gt -> ( > ))
        (Int32.compare x y) 0

    method private fetch_and_run () =
      (* Calculate cost of an instruction *)
      let dummy, normal, call, goto, if_t, if_f, return =
        (1, 1, 0, 0, 0, 1, 1)
      in
      let halt = ref None in
      let instr = self#fetch_instr () in
      let d_ec =
        match instr with
        | Label _ | Dec _ | Param _ | Function _ -> dummy
        | Assign (id, x) ->
            self#write_var id @@ self#eval_operand x;
            normal
        | Arith (id, x, op, y) ->
            self#write_var id
            @@ self#do_arith op (self#eval_operand x) (self#eval_operand y);
            normal
        (* x = &y *)
        | Ref (x, y) ->
            self#write_var x @@ self#addr_of_var y;
            normal
        (* x = *y *)
        (* TOOD: an alternative: add new operand kinds: REF, INDIR *)
        | Load (x, y) ->
            self#write_var x @@ self#read_mem @@ self#eval_operand (Id y);
            normal (* *x = y *)
        | Store (x, y) ->
            self#write_mem (self#eval_operand (Id x)) @@ self#eval_operand y;
            normal
        | Goto id ->
            self#local_jump id;
            goto
        | If (x, op, y, id)
          when self#do_cmp op (self#eval_operand x) (self#eval_operand y) ->
            self#local_jump id;
            if_t
        | If _ -> if_f
        | Return x -> (
            let vx = self#eval_operand x in
            match m_call_stack with
            | { saved_regs; ret_val_addr; fn = _ } :: tl ->
                m_call_stack <- tl;
                m_regs <- saved_regs;
                if tl = [] then (
                  halt := Some vx;
                  return)
                else (
                  self#write_mem ret_val_addr vx;
                  return)
            | _ ->
                m_fail Misc
                @@ Some "Impossible: return when call stack is empty?")
        | Arg x ->
            let vx = self#eval_operand x in
            m_regs.ap <- Int32.sub m_regs.ap word_size;
            self#write_mem m_regs.ap vx;
            normal
        | Call (x, fn) ->
            self#setup_call fn (self#addr_of_var x);
            call
        | Read id ->
            self#write_var id @@ self#input_i32 ();
            normal
        | Write x ->
            self#output_i32 @@ self#eval_operand x;
            normal
      in
      m_executed_count <- m_executed_count + d_ec;
      (* if d_ec <> 0 then *)
      (*   Printf.printf "EC = %d, Instruction %s\n" m_executed_count *)
      (*   @@ show_stmt instr; *)
      ignore @@ Option.map (fun ret -> raise @@ Halt ret) !halt

    method run () =
      try
        while true do
          self#fetch_and_run ()
        done;
        failwith "Impossible"
      with
      | Halt res -> Ok res
      | EmulatorErrorWrapper e -> Error e

    method executed_count = m_executed_count
  end
