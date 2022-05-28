type imm = I32 of int32
type id
type operand = Imm of imm | Id of id
type arith_op = Add | Sub | Mul | Div
type rel_op = Lt | Le | Eq | Neq | Ge | Gt

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

val show_imm : imm -> string
val show_id : id -> string
val show_operand : operand -> string
val show_arith_op : arith_op -> string
val show_rel_op : rel_op -> string
val show_stmt : stmt -> string
val stmt_of_string : string -> (stmt, string) result

type symbol
type program = { codetbl : stmt array; symtbl : symbol Map.Make(String).t }

val pretty_stmt_array : out_channel -> stmt array -> unit
val pretty_symtbl : out_channel -> symbol Map.Make(String).t -> unit
val compile : stmt Seq.t -> (program, string) result

type emu_err_kind =
  | NoProgram
  | BadProgram
  | Arith
  | Segv
  | MemAlign
  | IOErr
  | Unsafe
  | Misc

type registers = {
  mutable pc : int32;
  mutable sp : int32;
  mutable bp : int32;
  mutable ap : int32;
}

type emulator_error = {
  kind : emu_err_kind;
  text : string option;
  regs : registers;
}

type actv_rec = { fn : id; ret_val_addr : int32; saved_regs : registers }
type division_flavor = Trunc | RoundDown

class machine :
  mem_words:int32
  -> input:int32 Seq.t
  -> output:(int32 -> unit)
  -> object
       method set_division_flavor : division_flavor -> unit
       method load : ?entry:string -> program -> unit
       method run : unit -> (int32, emulator_error) result
       method set_verbosity : bool -> unit
       method executed_count : int
       method view_registers : registers
     end

val show_emu_err_kind : emu_err_kind -> string
val show_emulator_error : emulator_error -> string
val show_registers : registers -> string
val show_actv_rec : actv_rec -> string
