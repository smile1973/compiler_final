open Format
open X86_64
open Ast

let debug = ref false

exception Error of string
let error s = raise (Error s)

(* 記憶體配置大小 *)
let size_addr = 8        (* 指標大小: 8 bytes *)
let size_int = 8         (* 整數大小: 8 bytes *)

(* 型別標籤 *)
let tag_none = 0
let tag_bool = 1
let tag_int = 2
let tag_string = 3
let tag_list = 4

(* 產生新的標籤 *)
let new_label =
  let r = ref 0 in
  fun () -> incr r; "L" ^ string_of_int !r

(* 字串表管理 *)
let string_literals = ref ([] : (string * string) list)
let string_counter = ref 0

(* malloc 包裝函數,確保16位元組對齊 *)
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

(* 分配記憶體的輔助函數 *)
let allocate_memory size =
  movq (imm size) (reg rdi) ++
  call "my_malloc"

(* 建立基本值 *)
let create_none =
  allocate_memory (2 * size_int) ++  (* tag + dummy *)
  movq (imm tag_none) (ind rax)      (* 設定 tag *)

let create_bool b =
  allocate_memory (2 * size_int) ++   (* tag + value *)
  movq (imm tag_bool) (ind rax) ++    (* 設定 tag *)
  movq (imm (if b then 1 else 0)) (ind ~ofs:size_int rax)  (* 設定值 *)

let create_int n =
  allocate_memory (2 * size_int) ++   (* tag + value *)
  movq (imm tag_int) (ind rax) ++     (* 設定 tag *)
  movq (imm n) (ind ~ofs:size_int rax) (* 設定值 *)

let create_string s =
  incr string_counter;
  let string_label = Printf.sprintf "string_%d" !string_counter in
  string_literals := (string_label, s) :: !string_literals;
  
  allocate_memory (2 * size_int + String.length s + 1) ++  (* tag + length + string + \0 *)
  movq (imm tag_string) (ind rax) ++          (* 設定 tag *)
  movq (imm (String.length s)) (ind ~ofs:size_int rax) ++   (* 設定長度 *)
  leaq (lab string_label) (rsi) ++        (* 載入字串位址 *)
  leaq (ind ~ofs:(2 * size_int) rax) (rdi) ++  (* 目標位址 *)
  pushq (reg rax) ++                          (* 保存字串物件指標 *)
  call "strcpy" ++                            (* 複製字串 *)
  popq rax                                    (* 恢復字串物件指標 *)

(* 遞迴編譯表達式 *)
let rec compile_expr expr = 
  begin match expr with
  | TEcst c -> 
      begin match c with
      | Cnone -> create_none
      | Cbool b -> create_bool b
      | Cint n -> create_int (Int64.to_int n)
      | Cstring s -> create_string s
      end

  | TEunop (op, e) ->
      begin match op with
      | Uneg ->  (* 負號 *)
          compile_expr e ++
          movq (ind ~ofs:size_int rax) (reg rsi) ++
          negq (reg rsi) ++
          allocate_memory (2 * size_int) ++
          movq (imm tag_int) (ind rax) ++
          movq (reg rsi) (ind ~ofs:size_int rax)
          
      | Unot ->  (* 邏輯非 *)
          let l_true = new_label () in
          let l_end = new_label () in
          compile_expr e ++
          (* 檢查該值是否為假 *)
          test_value_false ++
          cmpq (imm 0) (reg rax) ++
          je l_true ++
          create_bool false ++
          jmp l_end ++
          label l_true ++
          create_bool true ++
          label l_end
      end
  
  | TEbinop (op, e1, e2) ->
      begin match op with
      | Badd -> (* 加法 *)
          compile_expr e1 ++
          pushq (reg rax) ++
          compile_expr e2 ++
          movq (reg rax) (reg r11) ++
          popq rax ++
          
          (* 檢查類型並進行相應操作 *)
          movq (ind rax) (reg rcx) ++    (* 取得左運算元類型 *)
          movq (ind r11) (reg rdx) ++    (* 取得右運算元類型 *)
          
          let l_string = new_label () in
          let l_list = new_label () in
          let l_end = new_label () in
          
          (* 根據類型選擇操作 *)
          cmpq (imm tag_int) (reg rcx) ++
          jne l_string ++
          cmpq (imm tag_int) (reg rdx) ++
          jne l_string ++
          
          (* 整數加法 *)
          movq (ind ~ofs:size_int rax) (reg rsi) ++
          movq (ind ~ofs:size_int r11) (reg rdi) ++
          addq (reg rdi) (reg rsi) ++
          allocate_memory (2 * size_int) ++
          movq (imm tag_int) (ind rax) ++
          movq (reg rsi) (ind ~ofs:size_int rax) ++
          jmp l_end ++
          
          (* 字串連接 *)
          label l_string ++
          cmpq (imm tag_string) (reg rcx) ++
          jne l_list ++
          cmpq (imm tag_string) (reg rdx) ++
          jne l_list ++
          
          pushq (reg r11) ++
          pushq (reg rax) ++
          leaq (ind ~ofs:(2 * size_int) rax) (rdi) ++
          call "strlen" ++
          movq (reg rax) (reg r12) ++
          popq rax ++
          popq r11 ++
          
          pushq (reg r12) ++
          pushq (reg r11) ++
          pushq (reg rax) ++
          leaq (ind ~ofs:(2 * size_int) r11) (rdi) ++
          call "strlen" ++
          movq (reg rax) (reg r13) ++
          
          addq (reg r12) (reg r13) ++
          addq (imm 1) (reg r13) ++  (* 加上結尾的\0 *)
          
          movq (reg r13) (reg rdi) ++
          call "malloc" ++
          
          popq r12 ++  (* 原字串1 *)
          popq r11 ++  (* 原字串2 *)
          popq r14 ++  (* 長度1 *)
          
          pushq (reg rax) ++  (* 保存新字串位置 *)
          
          movq (reg rax) (reg rdi) ++
          leaq (ind ~ofs:(2 * size_int) r12) (rsi) ++
          call "strcpy" ++
          
          popq rax ++
          pushq (reg rax) ++
          
          addq (reg r14) (reg rdi) ++
          leaq (ind ~ofs:(2 * size_int) r11) (rsi) ++
          call "strcat" ++
          
          popq rax ++
          
          movq (imm tag_string) (ind rax) ++
          movq (reg r13) (ind ~ofs:size_int rax) ++
          jmp l_end ++
          
          (* 列表連接 - 待實現 *)
          label l_list ++
          
          label l_end

      (* 減法 *)
      | Bsub ->
          compile_expr e1 ++
          pushq (reg rax) ++
          compile_expr e2 ++
          movq (reg rax) (reg r11) ++
          popq rax ++
          (* 檢查兩邊都是整數 *)
          movq (ind rax) (reg rcx) ++
          movq (ind r11) (reg rdx) ++
          cmpq (imm tag_int) (reg rcx) ++
          jne "error" ++
          cmpq (imm tag_int) (reg rdx) ++
          jne "error" ++
          (* 執行減法 *)
          movq (ind ~ofs:size_int rax) (reg rsi) ++
          movq (ind ~ofs:size_int r11) (reg rdi) ++
          subq (reg rdi) (reg rsi) ++
          allocate_memory (2 * size_int) ++
          movq (imm tag_int) (ind rax) ++
          movq (reg rsi) (ind ~ofs:size_int rax)

      (* 乘法 *)
      | Bmul ->
          compile_expr e1 ++
          pushq (reg rax) ++
          compile_expr e2 ++
          movq (reg rax) (reg r11) ++
          popq rax ++
          (* 檢查兩邊都是整數 *)
          movq (ind rax) (reg rcx) ++
          movq (ind r11) (reg rdx) ++
          cmpq (imm tag_int) (reg rcx) ++
          jne "error" ++
          cmpq (imm tag_int) (reg rdx) ++
          jne "error" ++
          (* 執行乘法 *)
          movq (ind ~ofs:size_int rax) (reg rsi) ++
          movq (ind ~ofs:size_int r11) (reg rdi) ++
          imulq (reg rdi) (reg rsi) ++
          allocate_memory (2 * size_int) ++
          movq (imm tag_int) (ind rax) ++
          movq (reg rsi) (ind ~ofs:size_int rax)

      (* 除法 *)
      | Bdiv ->
          compile_expr e1 ++
          pushq (reg rax) ++
          compile_expr e2 ++
          movq (reg rax) (reg r11) ++
          popq rax ++
          (* 檢查兩邊都是整數 *)
          movq (ind rax) (reg rcx) ++
          movq (ind r11) (reg rdx) ++
          cmpq (imm tag_int) (reg rcx) ++
          jne "error" ++
          cmpq (imm tag_int) (reg rdx) ++
          jne "error" ++
          (* 檢查除數不為0 *)
          movq (ind ~ofs:size_int r11) (reg rcx) ++
          cmpq (imm 0) (reg rcx) ++
          je "error_div_by_zero" ++
          (* 執行除法 *)
          movq (ind ~ofs:size_int rax) (reg rax) ++
          cqto ++
          idivq (reg rcx) ++
          movq (reg rax) (reg rsi) ++
          allocate_memory (2 * size_int) ++
          movq (imm tag_int) (ind rax) ++
          movq (reg rsi) (ind ~ofs:size_int rax)

      (* 取模 *)
      | Bmod ->
          compile_expr e1 ++
          pushq (reg rax) ++
          compile_expr e2 ++
          movq (reg rax) (reg r11) ++
          popq rax ++
          (* 檢查兩邊都是整數 *)
          movq (ind rax) (reg rcx) ++
          movq (ind r11) (reg rdx) ++
          cmpq (imm tag_int) (reg rcx) ++
          jne "error" ++
          cmpq (imm tag_int) (reg rdx) ++
          jne "error" ++
          (* 檢查除數不為0 *)
          movq (ind ~ofs:size_int r11) (reg rcx) ++
          cmpq (imm 0) (reg rcx) ++
          je "error_div_by_zero" ++
          (* 執行取模 *)
          movq (ind ~ofs:size_int rax) (reg rax) ++
          cqto ++
          idivq (reg rcx) ++
          movq (reg rdx) (reg rsi) ++  (* 取餘數 *)
          allocate_memory (2 * size_int) ++
          movq (imm tag_int) (ind rax) ++
          movq (reg rsi) (ind ~ofs:size_int rax)

      (* 比較運算子 *)
      | Beq | Bneq | Blt | Ble | Bgt | Bge as op ->
          compile_expr e1 ++
          pushq (reg rax) ++
          compile_expr e2 ++
          movq (reg rax) (reg r11) ++
          popq rax ++
          compare_values op

      (* 邏輯運算子 *)
      | Band ->
          let l_false = new_label () in
          let l_end = new_label () in
          compile_expr e1 ++
          test_value_false ++           (* 檢查第一個運算子 *)
          cmpq (imm 0) (reg rax) ++
          jne l_false ++              (* 如果為假，直接返回假 *)
          pushq (reg rax) ++          (* 保存第一個運算子的值 *)
          compile_expr e2 ++
          test_value_false ++           (* 檢查第二個運算子 *)
          jmp l_end ++
          label l_false ++
          create_bool false ++
          label l_end ++
          addq (imm 8) (reg rsp)     (* 清理堆疊 *)

      | Bor ->
          let l_true = new_label () in
          let l_end = new_label () in
          compile_expr e1 ++
          test_value_false ++           (* 檢查第一個運算子 *)
          cmpq (imm 0) (reg rax) ++
          je l_true ++               (* 如果為真，直接返回真 *)
          pushq (reg rax) ++         (* 保存第一個運算子的值 *)
          compile_expr e2 ++
          test_value_false ++           (* 檢查第二個運算子 *)
          jmp l_end ++
          label l_true ++
          create_bool true ++
          label l_end ++
          addq (imm 8) (reg rsp)    (* 清理堆疊 *)
      end
  | _ -> failwith "error" end
    

and compare_values op =
  let l_true = new_label () in
  let l_end = new_label () in
  (* 先檢查兩個值的類型是否相同 *)
  movq (ind rax) (reg rcx) ++
  movq (ind r11) (reg rdx) ++
  cmpq (reg rcx) (reg rdx) ++
  jne l_end ++  (* 如果類型不同，返回false *)
  
  (* 根據類型進行不同的比較 *)
  begin match op with
  | Beq | Bneq -> 
      (* 所有類型都可以用 == 和 != 比較 *)
      movq (ind ~ofs:size_int rax) (reg rsi) ++
      movq (ind ~ofs:size_int r11) (reg rdi) ++
      cmpq (reg rdi) (reg rsi) ++
      begin match op with
      | Beq -> je l_true
      | Bneq -> jne l_true
      | _ -> nop
      end
  | _ ->
      (* 其他比較運算只支援數值類型 *)
      cmpq (imm tag_int) (reg rcx) ++
      jne l_end ++
      movq (ind ~ofs:size_int rax) (reg rsi) ++
      movq (ind ~ofs:size_int r11) (reg rdi) ++
      cmpq (reg rdi) (reg rsi) ++
      begin match op with
      | Blt -> jl l_true
      | Ble -> jle l_true
      | Bgt -> jg l_true
      | Bge -> jge l_true
      | _ -> nop
      end
  end ++
  create_bool false ++  (* 預設返回 false *)
  jmp l_end ++
  label l_true ++
  create_bool true ++
  label l_end

(* 測試值是否為假的函數 *)
and test_value_false =
  let l_true = new_label () in
  let l_false = new_label () in
  let l_end = new_label () in
  
  movq (ind rax) (reg rcx) ++  (* 取得型別標籤 *)
  
  (* 檢查 none *)
  cmpq (imm tag_none) (reg rcx) ++
  jne l_true ++
  movq (imm 1) (reg rax) ++    (* none 視為假 *)
  jmp l_end ++
  
  (* 檢查 bool *)
  label l_true ++
  cmpq (imm tag_bool) (reg rcx) ++
  jne l_false ++
  movq (ind ~ofs:size_int rax) (reg rcx) ++
  movq (reg rcx) (reg rax) ++
  jmp l_end ++
  
  (* 其他類型一律視為真 *)
  label l_false ++
  movq (imm 0) (reg rax) ++
  
  label l_end