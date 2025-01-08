open Format
open X86_64
open Ast

let debug = ref false

(* 堆分配包裝函數保持不變 *)
let malloc_wrapper = 
  let code = [
    globl "my_malloc";
    label "my_malloc";
    pushq (reg rbp);
    movq (reg rsp) (reg rbp);
    andq (imm (-16)) (reg rsp);
    call "malloc";
    movq (reg rbp) (reg rsp);
    popq rbp;
    ret;
  ] in
  List.fold_left (++) nop code

(* 統一的print_value函數，處理所有類型的值 *)
let print_value_wrapper =
  let code = 
    globl "print_value" ++
    label "print_value" ++
    pushq (reg rbp) ++
    movq (reg rsp) (reg rbp) ++
    pushq (reg rbx) ++                 (* 保存rbx *)
    
    (* rdi包含要打印的值的指針 *)
    movq (reg rdi) (reg rbx) ++         (* 保存指針到rbx *)
    movq (ind ~ofs:0 rbx) (reg rdx) ++  (* 獲取類型標籤 *)
    
    (* 比較類型標籤 *)
    cmpq (imm 1) (reg rdx) ++           (* 是布林值? *)
    jne "print_value_not_bool" ++
    
    (* 打印布林值 *)
    movq (ind ~ofs:8 rbx) (reg rdi) ++  (* 取出布林值 *)
    call "print_bool" ++
    jmp "print_value_end" ++
    
    (* 檢查是否為整數 *)
    label "print_value_not_bool" ++
    cmpq (imm 2) (reg rdx) ++           (* 是整數? *)
    jne "print_value_end" ++            (* 如果不是，直接結束 *)
    
    (* 打印整數 *)
    movq (ind ~ofs:8 rbx) (reg rdi) ++  (* 取出整數值 *)
    call "print_int" ++
    
    (* 結束處理 *)
    label "print_value_end" ++
    popq rbx ++                         (* 恢復rbx *)
    movq (reg rbp) (reg rsp) ++
    popq rbp ++
    ret
  in
  code

let print_int_wrapper =
  let format_str = ".Sprint_int:\n\t.string \"%d\\n\"\n" in
  let code = [
    globl "print_int";
    label "print_int";
    pushq (reg rbp);
    movq (reg rsp) (reg rbp);
    andq (imm (-16)) (reg rsp);
    movq (imm 0) (reg rax);
    movq (reg rdi) (reg rsi);
    leaq (lab ".Sprint_int") (rdi);
    call "printf";
    movq (reg rbp) (reg rsp);
    popq rbp;
    ret;
  ] in
  format_str, List.fold_left (++) nop code
let print_bool_wrapper =
  let format_true = ".Sprint_true:\n\t.string \"True\\n\"\n" in
  let format_false = ".Sprint_false:\n\t.string \"False\\n\"\n" in
  let code = [
    globl "print_bool";
    label "print_bool";
    pushq (reg rbp);
    movq (reg rsp) (reg rbp);
    andq (imm (-16)) (reg rsp);
    cmpq (imm 0) (reg rdi);
    je "print_bool_false";
    movq (imm 0) (reg rax);
    leaq (lab ".Sprint_true") (rdi);
    jmp "print_bool_end";
    label "print_bool_false";
    movq (imm 0) (reg rax);
    leaq (lab ".Sprint_false") (rdi);
    label "print_bool_end";
    call "printf";
    movq (reg rbp) (reg rsp);
    popq rbp;
    ret;
  ] in
  (format_true ^ format_false), List.fold_left (++) nop code


let create_int n =
  if !debug then Printf.printf "Compiling int: %Ld\n" n;
  let size = 16 in  (* 8 bytes for tag + 8 bytes for value *)
  movq (imm size) (reg rdi) ++  (* 分配大小參數 *)
  call "my_malloc" ++
  movq (imm 2) (ind ~ofs:0 rax) ++  (* 設置類型標籤為2(整數) *)
  movq (imm64 n) (ind ~ofs:8 rax)   (* 設置值 *)

let create_bool b =
  let size = 16 in
  let value = if b then 1L else 0L in
  movq (imm size) (reg rdi) ++
  call "my_malloc" ++
  movq (imm 1) (ind ~ofs:0 rax) ++  (* 類型標籤 1 表示布林值 *)
  movq (imm64 value) (ind ~ofs:8 rax)

let label_counter = ref 0
let new_label () =
  let lbl = Printf.sprintf "label_%d" !label_counter in
  incr label_counter;
  lbl

(* 編譯表達式 *)
let rec compile_expr = function
  | TEcst (Cint n) -> create_int n
  | TEcst (Cbool b) -> create_bool b

  | TEbinop (op, e1, e2) ->
    let compile_compare op_type e1 e2 =
      compile_expr e1 ++                   (* 計算第一個表達式，結果在 rax *)
      pushq (reg rax) ++                   (* 保存第一個結果到棧上 *)
      compile_expr e2 ++                   (* 計算第二個表達式，結果在 rax *)
      movq (reg rax) (reg rcx) ++          (* 保存第二個結果到 rcx *)
      popq rax ++                          (* 恢復第一個結果到 rax *)
      movq (ind ~ofs:8 rax) (reg rax) ++   (* 取出第一個數的值 *)
      movq (ind ~ofs:8 rcx) (reg rcx) ++   (* 取出第二個數的值 *)
      (match op_type with
      | "and" ->
        let lbl_end = new_label () in
        cmpq (imm 0) (reg rax) ++
        jne lbl_end ++
        andq (reg rcx) (reg rax) ++
        jmp lbl_end ++
        label lbl_end 
      | "or" ->
        let lbl_end = new_label () in
        cmpq (imm 0) (reg rax) ++
        jne lbl_end ++
        orq (reg rcx) (reg rax) ++
        jmp lbl_end ++
        label lbl_end 
      | "eq" ->
        cmpq (reg rcx) (reg rax) ++
        sete (reg al) ++
        movzbq (reg al) (rax)
      | "neq" ->
        cmpq (reg rcx) (reg rax) ++
        setne (reg al) ++
        movzbq (reg al) (rax)
      | "gt" ->
        cmpq (reg rcx) (reg rax) ++ 
        setg (reg al) ++
        movzbq (reg al) (rax)
      | "ge" ->
        cmpq (reg rcx) (reg rax) ++
        setge (reg al) ++
        movzbq (reg al) (rax)
      | "lt" ->
        cmpq (reg rcx) (reg rax) ++
        setl (reg al) ++
        movzbq (reg al) (rax)
      | "le" ->
        cmpq (reg rcx) (reg rax) ++
        setle (reg al) ++
        movzbq (reg al) (rax)
      | _ -> failwith "Unsupported operation") ++
      pushq (reg rax) ++                   (* 保存結果 *)
      movq (imm 16) (reg rdi) ++           (* 分配16字節空間 *)
      call "my_malloc" ++
      popq rdx ++                          (* 恢復結果 *)
      movq (imm 1) (ind ~ofs:0 rax) ++     (* 設置類型標籤為布林值 *)
      movq (reg rdx) (ind ~ofs:8 rax) in
    let compile_binary_op op_type e1 e2 =
      compile_expr e1 ++                   (* 計算第一個表達式，結果在 rax *)
      pushq (reg rax) ++                   (* 保存第一個結果到棧上 *)
      compile_expr e2 ++                   (* 計算第二個表達式，結果在 rax *)
      movq (reg rax) (reg rcx) ++          (* 保存第二個結果到 rcx *)
      popq rax ++                          (* 恢復第一個結果到 rax *)
      movq (ind ~ofs:8 rax) (reg rax) ++   (* 取出第一個數的值 *)
      movq (ind ~ofs:8 rcx) (reg rcx) ++   (* 取出第二個數的值 *)
      (match op_type with
        | "add" -> addq (reg rcx) (reg rax)
        | "sub" -> subq (reg rcx) (reg rax)
        | "mul" -> imulq (reg rcx) (reg rax)
        | "div" ->
            cmpq (imm 0) (reg rcx) ++        (* 檢查除數是否為 0 *)
            failwith "error : divided with 0" ++
            cqto ++                          (* 擴展 RAX -> RDX:RAX *)
            idivq (reg rcx)
        | "mod" ->
            cmpq (imm 0) (reg rcx) ++        (* 檢查除數是否為 0 *)
            failwith "error : mod with 0" ++
            cqto ++                          (* 擴展 RAX -> RDX:RAX *)
            idivq (reg rcx) ++
            movq (reg rdx) (reg rax)
        | _ -> failwith "Unsupported operation") ++
      pushq (reg rax) ++                   (* 保存結果 *)
      movq (imm 16) (reg rdi) ++           (* 分配16字節空間 *)
      call "my_malloc" ++
      popq rdx ++                          (* 恢復結果 *)
      movq (imm 2) (ind ~ofs:0 rax) ++     (* 設置類型標籤為整數 *)
      movq (reg rdx) (ind ~ofs:8 rax) in    
    begin match op with
    | Badd -> compile_binary_op "add" e1 e2
    | Bsub -> compile_binary_op "sub" e1 e2
    | Bmul -> compile_binary_op "mul" e1 e2
    | Bdiv -> compile_binary_op "div" e1 e2
    | Bmod -> compile_binary_op "mod" e1 e2
    | Beq -> compile_compare "eq" e1 e2
    | Bneq -> compile_compare "neq" e1 e2
    | Bgt -> compile_compare "gt" e1 e2
    | Bge -> compile_compare "ge" e1 e2
    | Blt -> compile_compare "lt" e1 e2
    | Ble -> compile_compare "le" e1 e2
    | Band -> compile_compare "and" e1 e2
    | Bor -> compile_compare "or" e1 e2
    end
  | _ -> failwith "Unsupported expression type"

(* 編譯語句 *)
let rec compile_stmt = function
  | TSprint e ->
    compile_expr e ++                (* 計算表達式，結果在rax *)
    movq (reg rax) (reg rdi) ++
    call "print_value"
  | TSblock stmts -> 
      List.fold_left (fun code stmt -> code ++ compile_stmt stmt) nop stmts
  | _ -> nop

(* 編譯函數 *)
let compile_fun (fn, body) =
  if !debug then Printf.printf "Compiling function: %s\n" fn.fn_name;
  match fn.fn_name with
  | "main" ->
      globl "main" ++
      label "main" ++
      pushq (reg rbp) ++
      movq (reg rsp) (reg rbp) ++
      compile_stmt body ++     (* 編譯函數體 *)
      movq (imm 0) (reg rax) ++  (* 返回值0 *)
      movq (reg rbp) (reg rsp) ++
      popq rbp ++
      ret
  | _ -> nop

let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  let format_str1, print_wrap1 = print_int_wrapper in
  let format_str2, print_wrap2 = print_bool_wrapper in
  if !debug then Printf.printf "Compiling file with %d definitions\n" (List.length p);
  let code = List.fold_left (fun code def -> code ++ compile_fun def) nop p in
  { 
    text = malloc_wrapper ++         (* malloc包裝函數 *)
            print_wrap1 ++           (* int print *)
            print_wrap2 ++           (* bool print *)
            print_value_wrapper ++   (* general print *)
            code;
    data = inline (format_str1 ^ format_str2);  (* 格式字符串 *)
  }