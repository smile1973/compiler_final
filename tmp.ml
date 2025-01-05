open Format
open X86_64
open Ast

let debug = ref false

let new_label =
  let c = ref 0 in
  fun s -> incr c; Printf.sprintf "%s%d" s !c

let push_stack n = if n mod 16 = 0 then n else 16 * (n / 16 + 1)

let tag_none = 0
let tag_bool = 1
let tag_int = 2
let tag_string = 3
let tag_list = 4

type value =
  | Vnone
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vlist of value array

let malloc_wrapper () = "
my_malloc:
    pushq %rbp
    movq %rsp, %rbp
    andq $-16, %rsp    
    call malloc
    movq %rbp, %rsp
    popq %rbp
    ret
"

let allocate_memory size =
  movq (imm size) (reg rdi) ++
  call "my_malloc"

let create_int n =
  allocate_memory 16 ++
  movq (imm tag_int) (ind rax) ++
  movq (imm64 n) (ind ~ofs:8 rax)

let create_none =
  allocate_memory 16 ++         (* 分配16位元組 *)
  movq (imm tag_none) (ind rax) ++ (* 設置標籤 *)
  movq (imm 0) (ind ~ofs:8 rax)    (* 設置值為0 *)

let create_bool b =
  allocate_memory 16 ++
  movq (imm tag_bool) (ind rax) ++
  movq (imm (if b then 1 else 0)) (ind ~ofs:8 rax)

(* 第 0-7 位元組：存放標籤（tag）。
第 8-15 位元組：存放數據（data）。 *)
(* 將 src 指向的記憶體地址中的值載入到寄存器 dst 中 *)
let get_tag_value src dst =
  movq (ind src) (reg dst)

(* 從 src 指向的記憶體地址的 第 8 個位元組 開始讀取值，並將該值存入寄存器 dst 中 *)
let get_data_value src dst =
  movq (ind ~ofs:8 src) (reg dst)

(* + - * / % 運算 *)
let eval_binop op =
  match op with
  | Badd -> addq (reg rbx) (reg rax)
  | Bsub -> subq (reg rbx) (reg rax)
  | Bmul -> imulq (reg rbx) (reg rax)
  | Bdiv -> 
      cqto ++
      idivq (reg rbx)
  | Bmod ->
      cqto ++
      idivq (reg rbx) ++
      movq (reg rdx) (reg rax)
  | _ -> nop

(* 負號 & NOT操作 *)
let eval_unop = function
  | Uneg -> negq (reg rax)
  | Unot ->
      testq (reg rax) (reg rax) ++
      movq (imm 0) (reg rax) ++
      sete (reg al) ++
      movzbq (reg al) rax

let eval_boolop op =
  match op with
  | Band -> andq (reg rbx) (reg rax)
  | Bor -> orq (reg rbx) (reg rax)
  | _ -> failwith "Unsupported boolean operation"

let eval_comparison op =
  match op with
  | Beq -> cmpq (reg rbx) (reg rax) ++ sete (reg al)
  | Bneq -> cmpq (reg rbx) (reg rax) ++ setne (reg al)
  | Blt -> cmpq (reg rbx) (reg rax) ++ setl (reg al)
  | Ble -> cmpq (reg rbx) (reg rax) ++ setle (reg al)
  | Bgt -> cmpq (reg rbx) (reg rax) ++ setg (reg al)
  | Bge -> cmpq (reg rbx) (reg rax) ++ setge (reg al)
  | _ -> failwith "Unsupported comparison operation"

let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  { text =
      (* ==================框架================== *)
      inline ".extern malloc\n" ++
      inline (malloc_wrapper()) ++
      globl "main" ++
      label "main" ++
      pushq (reg rbp) ++
      movq (reg rsp) (reg rbp) ++
      
      (* Test: (15 + 27) * 2 *)
      create_int 15L ++
      pushq (reg rax) ++
      
      create_int 27L ++
      movq (reg rax) (reg rbx) ++
      popq rax ++
      
      get_data_value rax rax ++
      get_data_value rbx rbx ++
      eval_binop Badd ++
      
      pushq (reg rax) ++
      create_int 2L ++
      movq (reg rax) (reg rbx) ++
      popq rax ++
      
      get_data_value rbx rbx ++
      eval_binop Bmul ++
      
      movq (reg rax) (reg rsi) ++
      movq (ilab "format_int") (reg rdi) ++
      movq (imm 0) (reg rax) ++
      call "printf" ++

      (* Test: 5 == 5 *)
      (* create_int 5L ++
      pushq (reg rax) ++
      create_int 5L ++
      movq (reg rax) (reg rbx) ++
      popq rax ++
      get_data_value rax rax ++
      get_data_value rbx rbx ++
      eval_comparison Beq ++
      movq (reg rax) (reg rsi) ++
      movq (ilab "format_int") (reg rdi) ++
      movq (imm 0) (reg rax) ++
      call "printf" ++ *)
            
      (* ==================清理和返回================== *)
      movq (imm 0) (reg rax) ++
      movq (reg rbp) (reg rsp) ++
      popq rbp ++
      ret;
    data = 
      label "error_msg" ++ 
      string "error\n" ++
      label "format_int" ++ 
      string "%d\n"
  }