
open Format
open X86_64
open Ast

let debug = ref false
let string_table : (string, string) Hashtbl.t = Hashtbl.create 64
let function_table : (string, string list) Hashtbl.t = Hashtbl.create 64
let var_table : (string, int) Hashtbl.t = Hashtbl.create 64
let current_offset = ref (-64)

let error_handler =
  label "error_label" ++
  movq (imm 0) (reg rax) ++  (* printf需要rax=0 *)
  leaq (lab ".LCerror") (rdi) ++
  call "printf" ++
  (* 印出換行 *)
  movq (imm 10) (reg rdi) ++
  call "putchar" ++
  (* 回傳錯誤代碼1 *)
  movq (imm 1) (reg rdi) ++
  call "exit"

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
    
    cmpq (imm 0) (reg rdx) ++
    jne "print_value_not_none" ++
    call "print_none" ++
    jmp "print_value_end" ++

    (* 布林 *)
    label "print_value_not_none" ++
    cmpq (imm 1) (reg rdx) ++           (* 是布林值? *)
    jne "print_value_not_bool" ++
    movq (ind ~ofs:8 rbx) (reg rdi) ++  (* 取出布林值 *)
    call "print_bool" ++
    jmp "print_value_end" ++
    
    (* 整數 *)
    label "print_value_not_bool" ++
    cmpq (imm 2) (reg rdx) ++           (* 是整數? *)
    jne "print_value_not_int" ++
    movq (ind ~ofs:8 rbx) (reg rdi) ++  (* 取出整數值 *)
    call "print_int" ++
    jmp "print_value_end" ++

    (* 字串 *)
    label "print_value_not_int" ++
    cmpq (imm 3) (reg rdx) ++           (* 是字串? *)
    jne "print_value_not_str" ++            (* 如果不是，直接結束 *)
    leaq (ind ~ofs:16 rbx) (rdi) ++     (* 字串內容的位址 *)
    call "print_string" ++
    jmp "print_value_end" ++
    
    (* list *)
    label "print_value_not_str" ++
    cmpq (imm 4) (reg rdx) ++   
    jne "print_value_end" ++ 

    pushq (reg r12) ++    (* 保存 r12 用於儲存長度 *)
    pushq (reg r13) ++    (* 保存 r13 用於儲存當前索引 *)
    pushq (reg rbx) ++

    (* print [ *)
    leaq (lab ".LCstart") (rdi) ++
    call "printf" ++

    movq (ind ~ofs:8 rbx) (reg r12) ++  (* r12 = list長度 *)
    xorq (reg r13) (reg r13) ++         (* r13 = 目前的索引 = 0 *)

    label "print_list_loop" ++
    cmpq (reg r12) (reg r13) ++         (* 比較索引和長度 *)
    je "print_list_end" ++              (* 如果相等，結束迴圈 *)

    (* 如果不是第一個元素，印出逗號 *)
    cmpq (imm 0) (reg r13) ++
    je "print_list_element" ++
    leaq (lab ".LCcomma") (rdi) ++
    call "printf" ++

    (* 印出當前元素 *)
    label "print_list_element" ++
    movq (reg r13) (reg rcx) ++             (* 把索引放入 rcx *)
    imulq (imm 16) (reg rcx) ++             (* 乘以 16 (每個元素大小) *)
    addq (imm 16) (reg rcx) ++              (* 加上基本偏移 16 *)
    movq (ind rbx ~index:rcx) (reg rdi) ++  (* 取得元素 *)
    call "print_value" ++

    (* 增加index並繼續迴圈 *)
    incq (reg r13) ++
    jmp "print_list_loop" ++

    (* 印出 ] 並結束 *)
    label "print_list_end" ++
    leaq (lab ".LCend") (rdi) ++
    call "printf" ++
    
    popq rbx ++
    popq r13 ++
    popq r12 ++

    (* 結束處理 *)
    label "print_value_end" ++
    popq rbx ++
    movq (reg rbp) (reg rsp) ++
    popq rbp ++
    ret
  in
  code
let print_int_wrapper =
  let code = [
    globl "print_int";
    label "print_int";
    pushq (reg rbp);
    movq (reg rsp) (reg rbp);
    andq (imm (-16)) (reg rsp);
    movq (imm 0) (reg rax);
    movq (reg rdi) (reg rsi);
    leaq (lab ".LCd") (rdi);
    call "printf";
    movq (reg rbp) (reg rsp);
    popq rbp;
    ret;
  ] in
  List.fold_left (++) nop code
let print_bool_wrapper =
  let code = [
    globl "print_bool";
    label "print_bool";
    pushq (reg rbp);
    movq (reg rsp) (reg rbp);
    andq (imm (-16)) (reg rsp);
    cmpq (imm 0) (reg rdi);
    je "print_bool_false";
    movq (imm 0) (reg rax);
    leaq (lab ".LCtrue") (rdi);
    jmp "print_bool_end";
    label "print_bool_false";
    movq (imm 0) (reg rax);
    leaq (lab ".LCfalse") (rdi);
    label "print_bool_end";
    call "printf";
    movq (reg rbp) (reg rsp);
    popq rbp;
    ret;
  ] in
  List.fold_left (++) nop code
let print_string_wrapper =
  let code = [
    globl "print_string";
    label "print_string";
    pushq (reg rbp);
    movq (reg rsp) (reg rbp);
    andq (imm (-16)) (reg rsp);    (* 16-byte 對齊 *)
    movq (imm 0) (reg rax);        (* printf 需要 rax = 0 *)
    movq (reg rdi) (reg rsi);      (* 字串指標移到 rsi *)
    leaq (lab ".LCs") (rdi);       (* 格式字串 "%s" *)
    call "printf";
    movq (reg rbp) (reg rsp);
    popq rbp;
    ret;
  ] in
  List.fold_left (++) nop code
let print_none_wrapper =
  let code = [
    globl "print_none";
    label "print_none";
    pushq (reg rbp);
    movq (reg rsp) (reg rbp);
    andq (imm (-16)) (reg rsp);      (* 16-byte 對齊 *)
    movq (imm 0) (reg rax);          (* printf 需要 rax = 0 *)
    leaq (lab ".LCnone") (rsi);      (* 字串指標移到 rsi *)
    leaq (lab ".LCs") (rdi);         (* 格式字串 "%s" *)
    call "printf";
    movq (reg rbp) (reg rsp);
    popq rbp;
    ret;
  ] in
  List.fold_left (++) nop code

let create_int n =
  if !debug then Printf.printf "Compiling int: %Ld\n" n;
  let size = 16 in
  movq (imm size) (reg rdi) ++
  call "my_malloc" ++
  movq (imm 2) (ind ~ofs:0 rax) ++  (* 0 -> int *)
  movq (imm64 n) (ind ~ofs:8 rax)
let create_bool b =
  let size = 16 in
  let value = if b then 1L else 0L in
  movq (imm size) (reg rdi) ++
  call "my_malloc" ++
  movq (imm 1) (ind ~ofs:0 rax) ++  (* 1 -> bool *)
  movq (imm64 value) (ind ~ofs:8 rax)
let create_none =
  let size = 16 in  
  movq (imm size) (reg rdi) ++
  call "my_malloc" ++
  movq (imm 0) (ind ~ofs:0 rax) ++  (* 0 -> none *)
  movq (imm 0) (ind ~ofs:8 rax)   
let label_counter = ref 0
let fresh_unique_label () =
  let label = Printf.sprintf ".LC%d" !label_counter in
  incr label_counter;
  label    
let create_string str =
  let len = String.length str in
  (* 計算需要的總空間：8(tag) + 8(length) + string_length + 1(null terminator) *)
  let size = 16 + len + 1 in
  let aligned_size = (size + 7) land (-8) in
  
  (* 為字串內容創建一個新的標籤 *)
  let str_label = fresh_unique_label () in
  
  (* 將字串和標籤加入到字串表中 *)
  Hashtbl.add string_table str str_label;
  
  (* 分配記憶體 *)
  movq (imm aligned_size) (reg rdi) ++
  call "my_malloc" ++
  
  (* 設置類型標籤(3表示字串) *)
  movq (imm 3) (ind ~ofs:0 rax) ++
  
  (* 設置字串長度 *)
  movq (imm64 (Int64.of_int len)) (ind ~ofs:8 rax) ++
  
  (* 如果是空字串，直接設置結尾的null *)
  (if len = 0 then
    movb (imm 0) (ind ~ofs:16 rax)
  else
    (* 複製字串內容 *)
    pushq (reg rax) ++                  (* 保存指標 *)
    leaq (ind ~ofs:16 rax) (rdi) ++     (* 目標地址 *)
    movq (ilab str_label) (reg rsi) ++  (* 源地址 *)
    call "strcpy" ++
    popq rax
  )

(* 編譯表達式 *)
let rec compile_expr = function
  | TEcst (Cint n) -> create_int n
  | TEcst (Cbool b) -> create_bool b
  | TEcst (Cstring s) -> create_string s
  | TElist expr_list -> 
    let list_size = 16 + (16 * List.length expr_list) in  
      movq (imm list_size) (reg rdi) ++
      call "my_malloc" ++
      
      (* 設置型別標籤和長度 *)
      movq (imm 4) (ind ~ofs:0 rax) ++  (* 型別標籤 4 = list *)
      movq (imm (List.length expr_list)) (ind ~ofs:8 rax) ++
      
      (* 保存list的基址 *)
      pushq (reg rax) ++
      
      (* 編譯並存入每個元素, 現在每個元素佔16 bytes *)
      let store_elements = List.mapi (fun i e ->
        compile_expr e ++                (* 編譯元素，結果在rax *)
        pushq (reg rax) ++              (* 暫存元素值 *)
        movq (ind ~ofs:0 rsp) (reg rcx) ++  (* 取出元素值 *)
        popq rax ++                     (* 恢復rax *)
        popq rdx ++                     (* 取出list基址 *)
        movq (reg rcx) (ind ~ofs:(16 + 16*i) rdx) ++  (* 修改: 偏移量改為16*i *)
        pushq (reg rdx)                 (* 保存list基址 *)
      ) expr_list in
      
      List.fold_left (++) nop store_elements ++
      (* 恢復list的基址到rax *)
      popq rax
  | TEbinop (op, e1, e2) ->
    let compile_and e1 e2 =
      let end_label = fresh_unique_label () in
      let false_label = fresh_unique_label () in
      let error_check_label = fresh_unique_label () in
      
      (* e1 *)
      compile_expr e1 ++
      movq (ind ~ofs:8 rax) (reg rdx) ++  (* 第一個值到rdx *)
      
      (* e1 -> flase，跳到false處理 *)
      cmpq (imm 0) (reg rdx) ++
      je false_label ++
      
      (* e2 *)
      compile_expr e2 ++
      
      (* len特殊處理 *)
      (match e2 with
      | TEcall (fn, _) when fn.fn_name = "len" ->
        movq (ind ~ofs:0 rax) (reg rcx) ++
        cmpq (imm 4) (reg rcx) ++
        je error_check_label ++
        cmpq (imm 3) (reg rcx) ++
        jne "error_label" ++
        label error_check_label ++
        nop
      | _ -> nop) ++
      
      (* 檢查e2 *)
      movq (ind ~ofs:8 rax) (reg rdx) ++
      cmpq (imm 0) (reg rdx) ++
      je false_label ++
      
      (* true *)
      movq (imm 16) (reg rdi) ++
      call "my_malloc" ++
      movq (imm 1) (ind ~ofs:0 rax) ++
      movq (imm 1) (ind ~ofs:8 rax) ++
      jmp end_label ++
      
      (* false *)
      label false_label ++
      movq (imm 16) (reg rdi) ++
      call "my_malloc" ++
      movq (imm 1) (ind ~ofs:0 rax) ++
      movq (imm 0) (ind ~ofs:8 rax) ++
      
      label end_label in
    let compile_or e1 e2 =
      let end_label = fresh_unique_label () in
      let true_label = fresh_unique_label () in
      let error_check_label = fresh_unique_label () in
      
      (* e1 *)
      compile_expr e1 ++
      movq (ind ~ofs:8 rax) (reg rdx) ++  (* 第一個值到rdx *)
      
      (* e1 -> true，跳到true處理 *)
      cmpq (imm 0) (reg rdx) ++
      jne true_label ++
      
      (* e2 *)
      compile_expr e2 ++
      
      (* len特殊處理 *)
      (match e2 with
      | TEcall (fn, _) when fn.fn_name = "len" ->
        movq (ind ~ofs:0 rax) (reg rcx) ++
        cmpq (imm 4) (reg rcx) ++
        je error_check_label ++
        cmpq (imm 3) (reg rcx) ++
        jne "error_label" ++
        label error_check_label ++
        nop
      | _ -> nop) ++
      
      (* 檢查e2 *)
      movq (ind ~ofs:8 rax) (reg rdx) ++
      cmpq (imm 0) (reg rdx) ++
      jne true_label ++
      
      (* false *)
      movq (imm 16) (reg rdi) ++
      call "my_malloc" ++
      movq (imm 1) (ind ~ofs:0 rax) ++
      movq (imm 0) (ind ~ofs:8 rax) ++
      jmp end_label ++
      
      (* true *)
      label true_label ++
      movq (imm 16) (reg rdi) ++
      call "my_malloc" ++
      movq (imm 1) (ind ~ofs:0 rax) ++
      movq (imm 1) (ind ~ofs:8 rax) ++
      
      label end_label in
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
        let lbl_end = fresh_unique_label () in
        cmpq (imm 0) (reg rax) ++
        je lbl_end ++
        andq (reg rcx) (reg rax) ++
        jmp lbl_end ++
        label lbl_end 
      | "or" ->
        let lbl_end = fresh_unique_label () in
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
      | _ -> failwith "error: runtime error") ++
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
            (* 跳轉至錯誤處理 *)
            cqto ++                          (* RAX -> RDX:RAX *)
            idivq (reg rcx)
        | "mod" ->
            cmpq (imm 0) (reg rcx) ++        (* 檢查除數是否為 0 *)
            (* 跳轉至錯誤處理 *)
            cqto ++                          (* RAX -> RDX:RAX *)
            idivq (reg rcx) ++
            movq (reg rdx) (reg rax)
        | _ -> failwith "error: runtime error") ++
      pushq (reg rax) ++                   (* 保存結果 *)
      movq (imm 16) (reg rdi) ++           (* 分配16字節空間 *)
      call "my_malloc" ++
      popq rdx ++                          (* 恢復結果 *)
      movq (imm 2) (ind ~ofs:0 rax) ++     (* 設置類型標籤為整數 *)
      movq (reg rdx) (ind ~ofs:8 rax) in    
    let compile_list_compare op e1 e2 =
      let rec_label = fresh_unique_label () in
      let exit_label = fresh_unique_label () in
      let true_label = fresh_unique_label () in
      let false_label = fresh_unique_label () in
        
      (* 編譯兩個list並保存 *)
      compile_expr e1 ++
      pushq (reg rax) ++
      compile_expr e2 ++
      movq (reg rax) (reg rcx) ++
      popq rax ++
      
      (* 檢查兩者是否為list *)
      movq (ind ~ofs:0 rax) (reg r8) ++
      movq (ind ~ofs:0 rcx) (reg r9) ++
      cmpq (imm 4) (reg r8) ++
      jne "error" ++
      cmpq (imm 4) (reg r9) ++
      jne "error" ++
      
      (* 保存長度 *)
      movq (ind ~ofs:8 rax) (reg r8) ++  (* len1 *)
      movq (ind ~ofs:8 rcx) (reg r9) ++  (* len2 *)
      pushq (reg rax) ++  (* list1 *)
      pushq (reg rcx) ++  (* list2 *)
      
      (* 基於操作符的比較邏輯 *)
      (match op with
      | "eq" ->  (* == *)
        cmpq (reg r8) (reg r9) ++
        jne false_label ++  (* 長度不同，直接返回 false *)
        
        (* 長度相等，比較每個元素 *)
        popq rcx ++        (* 重新載入 list2 *)
        popq rax ++        (* 重新載入 list1 *)
        xorq (reg rdx) (reg rdx) ++  (* 索引 = 0 *)
        
        label rec_label ++
        cmpq (reg r8) (reg rdx) ++
        je true_label ++             (* 都遍歷完了且相等，返回 true *)
        
        (* 比較當前元素 *)
        movq (reg rdx) (reg r10) ++
        imulq (imm 16) (reg r10) ++
        addq (imm 16) (reg r10) ++
        movq (ind ~ofs:0 rax ~index:r10) (reg r11) ++
        movq (ind ~ofs:0 rcx ~index:r10) (reg r12) ++
        movq (ind ~ofs:8 r11) (reg r11) ++  (* 取出值 *)
        movq (ind ~ofs:8 r12) (reg r12) ++  (* 取出值 *)
        cmpq (reg r11) (reg r12) ++
        jne false_label ++           (* 元素不同，返回 false *)
        
        incq (reg rdx) ++
        jmp rec_label
      | "lt" -> (* < *)
        (* 比較每個元素 *)
        popq rcx ++        (* 重新載入 list2 *)
        popq rax ++        (* 重新載入 list1 *)
        xorq (reg rdx) (reg rdx) ++  (* index = 0 *)
        
        label rec_label ++
        (* 如果到達任一list的結尾 *)
        cmpq (reg r8) (reg rdx) ++
        je true_label ++             (* list1更短，返回true *)
        cmpq (reg r9) (reg rdx) ++
        je false_label ++            (* list2更短，返回false *)
        
        (* 比較當前元素 *)
        movq (reg rdx) (reg r10) ++
        imulq (imm 16) (reg r10) ++
        addq (imm 16) (reg r10) ++
        movq (ind ~ofs:0 rax ~index:r10) (reg r11) ++
        movq (ind ~ofs:0 rcx ~index:r10) (reg r12) ++
        movq (ind ~ofs:8 r11) (reg r11) ++  (* 取出值 *)
        movq (ind ~ofs:8 r12) (reg r12) ++
        
        (* 如果元素不相等 *)
        cmpq (reg r12) (reg r11) ++
        jl true_label ++   (* list1[i] < list2[i] *)
        jg false_label ++  (* list1[i] > list2[i] *)
        
        (* 元素相等，繼續比較 *)
        incq (reg rdx) ++
        jmp rec_label
      | "le" -> (* <= *)
        (* 比較每個元素 *)
        popq rcx ++        (* 重新載入 list2 *)
        popq rax ++        (* 重新載入 list1 *)
        xorq (reg rdx) (reg rdx) ++  (* index = 0 *)
        
        label rec_label ++
        (* 如果到達任一list的結尾 *)
        cmpq (reg r8) (reg rdx) ++
        je true_label ++             (* list1更短或等長，返回true *)
        cmpq (reg r9) (reg rdx) ++
        je false_label ++            (* list2更短，返回false *)
        
        (* 比較當前元素 *)
        movq (reg rdx) (reg r10) ++
        imulq (imm 16) (reg r10) ++
        addq (imm 16) (reg r10) ++
        movq (ind ~ofs:0 rax ~index:r10) (reg r11) ++
        movq (ind ~ofs:0 rcx ~index:r10) (reg r12) ++
        movq (ind ~ofs:8 r11) (reg r11) ++
        movq (ind ~ofs:8 r12) (reg r12) ++
        
        (* 如果元素不相等 *)
        cmpq (reg r11) (reg r12) ++
        jl true_label ++   (* list1[i] < list2[i] *)
        jg false_label ++  (* list1[i] > list2[i] *)
        
        (* 元素相等，繼續比較 *)
        incq (reg rdx) ++
        jmp rec_label
      | _ -> nop) ++
      (* 處理比較結果 *)
      label true_label ++
      movq (imm 1) (reg rax) ++
      jmp exit_label ++

      (* 處理比較結果 *)
      label false_label ++
      movq (imm 0) (reg rax) ++

      (* 創建布林值對象 *)
      label exit_label ++
      
      (* 清理堆疊並將結果轉換為bool類型 *)
      pushq (reg rax) ++              (* 保存比較結果 *)
      movq (imm 16) (reg rdi) ++      (* 分配空間給布林值 *)
      call "my_malloc" ++
      popq rdx ++                     (* 恢復比較結果 *)
      movq (imm 1) (ind ~ofs:0 rax) ++ (* 設置類型標籤為布林(1) *)
      movq (reg rdx) (ind ~ofs:8 rax) in  (* 設置布林值 *)
    let is_list = function
      | TElist _ | TEvar _ | TErange _ -> true
      | _ -> false in
    if is_list e1 || is_list e2 then
      match op with
      | Beq -> compile_list_compare "eq" e1 e2
      | Bneq -> 
          compile_list_compare "eq" e1 e2 ++
          movq (ind ~ofs:8 rax) (reg rdx) ++
          xorq (imm 1) (reg rdx) ++
          movq (reg rdx) (ind ~ofs:8 rax)
      | Blt -> compile_list_compare "lt" e1 e2
      | Ble -> compile_list_compare "le" e1 e2
      | Bgt -> compile_list_compare "lt" e2 e1
      | Bge -> compile_list_compare "le" e2 e1
      | _ -> failwith "Unsupported operation for lists"
    else
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
        | Band -> compile_and e1 e2
        | Bor -> compile_or e1 e2
      end
  | TEcall (fn, [arg]) when fn.fn_name = "len" -> compile_expr arg ++         (* 編譯參數，結果在 rax *)
    pushq (reg rax) ++         (* 保存參數指針 *)
    movq (ind ~ofs:0 rax) (reg rdx) ++  (* 檢查類型 *)
    
    (* 檢查類型標籤 *)
    cmpq (imm 3) (reg rdx) ++  (* 是字串? *)
    je "handle_len" ++
    cmpq (imm 4) (reg rdx) ++  (* 是列表? *)
    je "handle_len" ++
    jmp "error_label" ++       (* 其他類型，跳到錯誤處理 *)
    
    (* 處理長度 *)
    label "handle_len" ++
    popq rax ++                (* 取回列表/字串指針 *)
    movq (ind ~ofs:8 rax) (reg rsi) ++  (* 取得長度值 *)
    
    (* 創建新的整數物件 *)
    pushq (reg rsi) ++         (* 保存長度值 *)
    movq (imm 16) (reg rdi) ++
    call "my_malloc" ++
    popq rsi ++                (* 取回長度值 *)
    movq (imm 2) (ind ~ofs:0 rax) ++  (* 設置類型為整數 *)
    movq (reg rsi) (ind ~ofs:8 rax)   (* 存入長度值 *)
  | TEcall (fn, args) ->
    Printf.printf "sfsnbafjhsa\n";
    let v_name_list = Hashtbl.find function_table fn.fn_name in
    let combine_list = List.combine args v_name_list in
    let call_fn_code =
      List.map (fun (arg, vn) -> 
        Printf.printf "%s\n" vn;
        let v_ofs = Hashtbl.find var_table vn in
        compile_expr arg ++
        movq (reg rax) (ind ~ofs:v_ofs rbp)
      ) combine_list
    in
    let combined_code = List.fold_left (++) nop call_fn_code in
    combined_code ++ call fn.fn_name
  | TEvar var ->
    let v_ofs = Hashtbl.find var_table var.v_name in
    movq (ind ~ofs:v_ofs rbp) (!%rdi)
  | TEget (lst, index) ->
    (match lst with
    | TEvar var -> 
      let list_offset = Hashtbl.find var_table var.v_name in
      
      compile_expr index ++
      pushq (reg rax) ++               (* 保存 *)
      
      (* 取得list *)
      movq (ind ~ofs:list_offset rbp) (reg rdx) ++
      
      (* 檢查是否為list *)
      movq (ind ~ofs:0 rdx) (reg rcx) ++
      cmpq (imm 4) (reg rcx) ++
      jne "error_label" ++             (* 不是list，跳到錯誤處理 *)
      
      (* list 長度 *)
      movq (ind ~ofs:8 rdx) (reg rcx) ++
      
      (* get index *)
      popq rax ++
      movq (ind ~ofs:8 rax) (reg r8) ++
      
      (* check index *)
      cmpq (imm 0) (reg r8) ++        (* 檢查是否小於0 *)
      jl "error_label" ++
      cmpq (reg rcx) (reg r8) ++      (* 檢查是否大於等於長度 *)
      jge "error_label" ++
      
      (* get element position *)
      imulq (imm 16) (reg r8) ++      (* 索引 * 16(每個元素大小) *)
      addq (imm 16) (reg r8) ++       (* 加上基本偏移16 *)
      
      (* 取出對應位置的元素 *)
      movq (ind rdx ~index:r8) (reg rax)
    
    | TEget(inner_lst, inner_idx) ->
      compile_expr inner_lst ++     (* 先取得內層列表 *)
      pushq (reg rax) ++            (* 保存內層列表位址 *)
      
      (* 編譯內層索引 *)
      compile_expr inner_idx ++
      movq (ind ~ofs:8 rax) (reg rcx) ++  (* 取出內層索引值 *)
      imulq (imm 16) (reg rcx) ++         (* 計算內層偏移 *)
      addq (imm 16) (reg rcx) ++
      
      (* 取得內層元素 *)
      popq rdx ++                         (* 恢復列表位址 *)
      movq (ind rdx ~index:rcx) (reg rax) ++
      
      (* 現在rax中是內層列表,繼續處理外層索引 *)
      pushq (reg rax) ++                  (* 保存內層元素 *)
      
      (* 編譯外層索引 *)
      compile_expr index ++
      movq (ind ~ofs:8 rax) (reg rcx) ++
      imulq (imm 16) (reg rcx) ++
      addq (imm 16) (reg rcx) ++
      
      (* 取得最終元素 *)
      popq rdx ++
      movq (ind rdx ~index:rcx) (reg rax)
    | _ ->  
        compile_expr lst ++           (* 編譯整個列表 *)
        pushq (reg rax) ++            (* 保存列表位址 *)
        
        compile_expr index ++
        movq (ind ~ofs:8 rax) (reg rcx) ++
        imulq (imm 16) (reg rcx) ++
        addq (imm 16) (reg rcx) ++
        
        popq rdx ++
        movq (ind rdx ~index:rcx) (reg rax))
  | TErange n ->
    (* 編譯範圍大小參數 *)
    compile_expr n ++
    pushq (reg rax) ++                 (* 保存範圍物件 *)
    
    (* 檢查參數類型 *)
    movq (ind ~ofs:0 rax) (reg rcx) ++
    cmpq (imm 2) (reg rcx) ++          (* 確認是否為整數類型 *)
    jne "error_label" ++               (* 若不是整數類型，跳到錯誤處理 *)
    
    (* 取出範圍大小值 *)
    movq (ind ~ofs:8 rax) (reg rbx) ++  
    
    (* 檢查range參數是否為負數或0 *)
    cmpq (imm 0) (reg rbx) ++
    jle "error_label" ++               (* 如果是負數或0，跳到錯誤處理 *)
    
    popq rax ++
    
    (* 生成0到n-1的list *)
    let size = match n with
      | TEcst (Cint v) -> Int64.to_int v
      | _ -> 0  (* 前面已經檢查過了 *)
    in
    let range_list = List.init size (fun i -> 
      TEcst (Cint (Int64.of_int i))
    ) in
    
    compile_expr (TElist range_list)
  | _ -> failwith "Unsupported expression type"


(* 編譯語句 *)
let rec compile_stmt = function
  | TSprint e ->
    let rec print_code e =
      begin match e with
      | TEcst (Cint _) | TEcst (Cbool _) | TEcst (Cstring _) | TElist _ | TEget _ ->
        compile_expr e ++
        movq (reg rax) (reg rdi) ++
        call "print_value" ++
        movq (imm 10) (!%rdi) ++
        call "putchar"
      | TEbinop(op, e1, e2) ->
        begin match op with 
        | Badd when (match e1 with TEcst (Cstring _) | TElist _ -> true | _ -> false) ->
          compile_expr e1 ++
          call "printf" ++
          compile_expr e2 ++  
          call "printf" ++
          movq (imm 10) (!%rdi) ++
          call "putchar"
        | Beq | Bneq | Bgt | Bge | Blt | Ble | Band | Bor ->
          compile_expr e ++
          movq (reg rax) (reg rdi) ++
          call "print_value" ++
          movq (imm 10) (!%rdi) ++
          call "putchar"
        | Badd ->
          test_invalid_operation e1 e2 ++
          compile_expr e ++
          movq (reg rax) (reg rdi) ++
          call "print_value" ++
          movq (imm 10) (!%rdi) ++
          call "putchar"
        | _ -> 
          compile_expr e ++
          movq (reg rax) (reg rdi) ++
          call "print_value" ++
          movq (imm 10) (!%rdi) ++
          call "putchar"
        end
      | TEcall (fn, args) when fn.fn_name = "len" ->
        compile_expr e ++
        movq (reg rax) (reg rdi) ++
        call "print_value" ++
        movq (imm 10) (!%rdi) ++
        call "putchar"
      | TEcall (fn, arg) ->
        Printf.printf "1234564789\n";
        compile_expr e ++
        movq (reg rax) (reg rdi) ++
        call "print_value" ++
        movq (imm 10) (!%rdi) ++
        call "putchar"
      | TErange _ ->
        compile_expr e ++
        movq (reg rax) (reg rdi) ++
        call "print_value" ++
        movq (imm 10) (!%rdi) ++
        call "putchar"
      | TEvar (var) ->
        let old_v_ofs = Hashtbl.find var_table var.v_name in 
        movq (ind ~ofs:old_v_ofs rbp) (!%rax) ++
        movq (!%rax) (!%rdi) ++
        call "print_value" ++
        movq (imm 10) (!%rdi) ++
        call "putchar"
      | _ -> nop
      end
    in
    print_code e
  | TSassign (var, expr) ->
    let cur_var =
      if Hashtbl.mem var_table var.v_name then
        let old_v_ofs = Hashtbl.find var_table var.v_name in 
        old_v_ofs
      else
        let new_v_ofs = !current_offset in
        Hashtbl.add var_table var.v_name new_v_ofs;
        current_offset := !current_offset - 16;
        new_v_ofs
    in
    compile_expr expr ++
    movq (reg rax) (ind ~ofs:cur_var rbp)
  | TSif (cond, then_branch, else_branch) ->
    let else_label = fresh_unique_label () in
    let end_label = fresh_unique_label () in
    (compile_expr cond ++
    cmpq (imm 0) (ind ~ofs:8 rax) ++
    je else_label ++
    compile_stmt then_branch ++
    jmp end_label ++
    label else_label ++
    compile_stmt else_branch ++
    label end_label)
  | TSfor (var, expr, body) ->
    (* 為循環變量分配空間 *)
    let var_offset = 
      if Hashtbl.mem var_table var.v_name then
        Hashtbl.find var_table var.v_name
      else begin
        let offset = !current_offset in
        Hashtbl.add var_table var.v_name offset;
        current_offset := !current_offset - 16;
        offset
      end
    in

    (* 編譯 list 表達式並保存到棧上 *)
    compile_expr expr ++
    pushq (reg rax) ++  

    (* 檢查是否為列表類型 *)
    movq (ind ~ofs:0 rax) (reg rcx) ++
    cmpq (imm 4) (reg rcx) ++          (* 檢查是否為列表類型(tag=4) *)
    jne "error_label" ++               (* 如果不是列表，跳到錯誤處理 *)

    pushq (reg r12) ++  (* 循環計數器 *)
    pushq (reg r13) ++  (* list長度 *)
    xorq (reg r12) (reg r12) ++
    
    (* 取得list的長度 *)
    popq r13 ++                         (* 取回list指針到r13 *)
    movq (ind ~ofs:8 rax) (reg r13) ++  (* 取得長度到r13 *)
    pushq (reg rax) ++                  (* 重新保存list指針 *)

    (* 循環開始標籤 *)
    let loop_start = fresh_unique_label () in
    let loop_end = fresh_unique_label () in
    label loop_start ++

    (* 檢查是否完成循環 *)
    cmpq (reg r13) (reg r12) ++
    je loop_end ++

    (* 取出當前元素 *)
    popq rax ++
    movq (reg r12) (reg rcx) ++             (* 當前索引 *)
    imulq (imm 16) (reg rcx) ++             (* 計算偏移 *)
    addq (imm 16) (reg rcx) ++              (* 加上list頭部偏移 *)
    movq (ind rax ~index:rcx) (reg rdx) ++  (* 取得元素 *)
    pushq (reg rax) ++                      (* 保存list指針 *)

    (* 將當前元素存入循環變量 *)
    movq (reg rdx) (ind ~ofs:var_offset rbp) ++

    (* 編譯循環體 *)
    compile_stmt body ++

    (* 增加計數器並繼續循環 *)
    incq (reg r12) ++
    jmp loop_start ++

    (* 循環結束 *)
    label loop_end ++
    popq rax ++
    popq r13 ++
    popq r12
  | TSeval expr ->
    compile_expr expr
  | TSreturn expr ->
    compile_expr expr ++
    ret
  | TSblock stmts -> 
      List.fold_left (fun code stmt -> code ++ compile_stmt stmt) nop stmts
  | _ -> nop
and test_invalid_operation e1 e2 =
  match e1, e2 with
  | TEcst (Cint _), TEcst (Cint _) -> nop 
  | TEcst (Cstring _), TEcst (Cstring _) -> nop 
  | _, _ -> jmp "error_label"

(* 編譯函數 *)
let compile_fun (fn, body) =
  if !debug then Printf.printf "Compiling function: %s\n" fn.fn_name;
  match fn.fn_name with
  | "main" ->
      globl "main" ++
      label "main" ++
      pushq (reg rbp) ++
      movq (reg rsp) (reg rbp) ++    
      subq (imm 1024) (reg rsp) ++
      compile_stmt body ++       (* 編譯函數體 *)
      movq (imm 0) (reg rax) ++  (* 返回值0 *)
      movq (reg rbp) (reg rsp) ++
      popq rbp ++
      ret
  | _ ->
    let v_name_list = ref [] in
    List.iter (fun var -> 
      v_name_list := var.v_name :: !v_name_list;
      if not (Hashtbl.mem var_table var.v_name) then (
        let new_v_ofs = !current_offset in
        Hashtbl.add var_table var.v_name new_v_ofs;
        current_offset := !current_offset - 16
      )
    ) fn.fn_params;
    let v_name_list = List.rev !v_name_list in
    Hashtbl.add function_table fn.fn_name v_name_list;

    label fn.fn_name ++
    compile_stmt body ++
    create_none ++
    ret

let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  let print_wrap1 = print_int_wrapper in
  let print_wrap2 = print_bool_wrapper in
  let print_wrap3 = print_string_wrapper in
  let print_wrap4 = print_none_wrapper in
  if !debug then Printf.printf "Compiling file with %d definitions\n" (List.length p);
  let code = List.fold_left (fun code def -> code ++ compile_fun def) nop p in
  let string_data =
    label ".LCnone"   ++ string "None"   ++
    label ".LCtrue"   ++ string "True"   ++
    label ".LCfalse"  ++ string "False"  ++
    label ".LCcomma"  ++ string ", "     ++
    label ".LCstart"  ++ string "["      ++
    label ".LCend"    ++ string "]"      ++
    label ".LCs"      ++ string "%s"     ++
    label ".LCd"      ++ string "%d" ++
    label ".LCerror" ++ string "Runtime Error" ++
    label ".LClen"    ++ string "len: %d\n"
  in
  let string_data2 =
    Hashtbl.fold (fun value label_name acc ->
      acc ++ (label label_name) ++ string value
    ) string_table nop
  in
  { 
    text = malloc_wrapper ++         (* malloc *)
            print_wrap1 ++           (* int print *)
            print_wrap2 ++           (* bool print *)
            print_wrap3 ++           (* string print *)
            print_wrap4 ++           (* none print *)
            print_value_wrapper ++   (* general print *)
            code ++
            error_handler;
    data = string_data ++
           string_data2;  (* 格式字串 *)
  }