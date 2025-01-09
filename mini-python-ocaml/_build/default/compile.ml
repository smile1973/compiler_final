
open Format
open X86_64
open Ast

let debug = ref false
let string_table : (string, string) Hashtbl.t = Hashtbl.create 64
let function_table : (string, fn) Hashtbl.t = Hashtbl.create 16
let var_table : (string, int) Hashtbl.t = Hashtbl.create 64
let current_offset = ref (-64)

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
    
    (* 布林 *)
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
    leaq (ind ~ofs:16 rbx) (rdi) ++ (* 字串內容的位址 *)
    call "print_string" ++
    jmp "print_value_end" ++
    
    (* list *)
    label "print_value_not_str" ++
    cmpq (imm 4) (reg rdx) ++   
    jne "print_value_end" ++ 

    pushq (reg r12) ++    (* 保存 r12 用於儲存長度 *)
    pushq (reg r13) ++    (* 保存 r13 用於儲存當前索引 *)

    pushq (reg rbx) ++
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
    movq (reg r13) (reg rcx) ++         (* 把索引放入 rcx *)
    imulq (imm 16) (reg rcx) ++         (* 乘以 16 (每個元素大小) *)
    addq (imm 16) (reg rcx) ++          (* 加上基本偏移 16 *)
    movq (ind rbx ~index:rcx) (reg rdi) ++  (* 取得元素 *)
    call "print_value" ++               (* 遞迴呼叫print_value *)

    (* 增加index並繼續迴圈 *)
    incq (reg r13) ++
    jmp "print_list_loop" ++

    (* 印出 ] 並結束 *)
    label "print_list_end" ++
    leaq (lab ".LCend") (rdi) ++
    call "printf" ++
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
    pushq (reg rax) ++  (* 保存指標 *)
    leaq (ind ~ofs:16 rax) (rdi) ++  (* 目標地址 *)
    movq (ilab str_label) (reg rsi) ++  (* 源地址 *)
    call "strcpy" ++
    popq rax  (* 恢復指標 *)
  )

(* 編譯表達式 *)
let rec compile_expr = function
  | TEcst (Cint n) -> create_int n
  | TEcst (Cbool b) -> create_bool b
  | TEcst (Cstring s) -> create_string s
  | TElist expr_list -> 
    let list_size = 16 + (16 * List.length expr_list) in  (* 修改: 每個元素需要16 bytes *)
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
      compile_expr e1 ++                   (* 計算第一個表達式，結果在 rax *)
      pushq (reg rax) ++                   (* 保存第一個結果到棧上 *)
      compile_expr e2 ++                   (* 計算第二個表達式，結果在 rax *)
      movq (reg rax) (reg rcx) ++          (* 保存第二個結果到 rcx *)
      popq rax ++                          (* 恢復第一個結果到 rax *)
      movq (ind ~ofs:8 rax) (reg rax) ++   (* 取出第一個數的值 *)
      movq (ind ~ofs:8 rcx) (reg rcx) ++   (* 取出第二個數的值 *)
      let lbl_end = fresh_unique_label () in
        cmpq (imm 0) (reg rax) ++
        je lbl_end ++
        andq (reg rcx) (reg rax) ++
        jmp lbl_end ++
      
      label lbl_end ++
      pushq (reg rax) ++                   (* 保存結果 *)
      movq (imm 16) (reg rdi) ++           (* 分配16字節空間 *)
      call "my_malloc" ++
      popq rdx ++                          (* 恢復結果 *)
      movq (imm 1) (ind ~ofs:0 rax) ++     (* 設置類型標籤為布林值 *)
      movq (reg rdx) (ind ~ofs:8 rax) in
    let compile_or e1 e2 =
      compile_expr e1 ++                   (* 計算第一個表達式，結果在 rax *)
      pushq (reg rax) ++                   (* 保存第一個結果到棧上 *)
      compile_expr e2 ++                   (* 計算第二個表達式，結果在 rax *)
      movq (reg rax) (reg rcx) ++          (* 保存第二個結果到 rcx *)
      popq rax ++                          (* 恢復第一個結果到 rax *)
      movq (ind ~ofs:8 rax) (reg rax) ++   (* 取出第一個數的值 *)
      movq (ind ~ofs:8 rcx) (reg rcx) ++   (* 取出第二個數的值 *)
      let lbl_end = fresh_unique_label () in
        cmpq (imm 0) (reg rax) ++
        jne lbl_end ++
        orq (reg rcx) (reg rax) ++
        jmp lbl_end ++
      
      label lbl_end ++
      pushq (reg rax) ++                   (* 保存結果 *)
      movq (imm 16) (reg rdi) ++           (* 分配16字節空間 *)
      call "my_malloc" ++
      popq rdx ++                          (* 恢復結果 *)
      movq (imm 1) (ind ~ofs:0 rax) ++     (* 設置類型標籤為布林值 *)
      movq (reg rdx) (ind ~ofs:8 rax) in
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
            cqto ++                          (* 擴展 RAX -> RDX:RAX *)
            idivq (reg rcx)
        | "mod" ->
            cmpq (imm 0) (reg rcx) ++        (* 檢查除數是否為 0 *)
            (* 跳轉至錯誤處理 *)
            cqto ++                          (* 擴展 RAX -> RDX:RAX *)
            idivq (reg rcx) ++
            movq (reg rdx) (reg rax)
        | _ -> failwith "error: runtime error") ++
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
    | Band -> compile_and e1 e2
    | Bor -> compile_or e1 e2
    end
  | TEcall (fn, [arg]) when fn.fn_name = "len" ->
    compile_expr arg ++
    pushq (reg rax) ++
    movq (ind ~ofs:0 rax) (reg rdx) ++
    
    movq (imm 16) (reg rdi) ++
    call "my_malloc" ++
    movq (imm 2) (ind ~ofs:0 rax) ++  (* int *)
    popq rcx ++
    
    (* 目前先處理字串的情況 *)
    cmpq (imm 3) (reg rdx) ++  (* 是字串? *)
    movq (ind ~ofs:8 rcx) (reg rdx) ++  (* 取得字串長度 *)
    movq (reg rdx) (ind ~ofs:8 rax)     (* 儲存長度 *)
  | TEvar var ->
    movq (ind ~ofs:var.v_ofs rbp) (!%rdi)
  | TEget (lst, index) ->
    (match lst with
    | TEvar var -> 
        let list_offset = Hashtbl.find var_table var.v_name in
        
        (* 編譯索引表達式並獲取值 *)
        compile_expr index ++
        movq (ind ~ofs:8 rax) (reg rcx) ++  (* 取出索引值到rcx *)
        
        (* 計算要獲取的元素位置 *)
        imulq (imm 16) (reg rcx) ++      (* 索引 * 16(每個元素大小) *)
        addq (imm 16) (reg rcx) ++       (* 加上基本偏移16 *)
        
        (* 從list中取出對應位置的元素 *)
        movq (ind ~ofs:list_offset rbp) (reg rdx) ++  (* 取得list起始位置 *)
        movq (ind rdx ~index:rcx) (reg rax)
    | _ -> failwith "Unsupported expression type")
  | TErange n ->
    (* 編譯範圍大小參數 *)
    compile_expr n ++
    pushq (reg rax) ++
    movq (ind ~ofs:8 rax) (reg rbx) ++  (* 取出範圍大小值 *)
    popq rax ++
    
    (* 生成0到n-1的list *)
    let size = match n with
      | TEcst (Cint v) -> Int64.to_int v
      | _ -> failwith "Range size must be constant"
    in
    let range_list = List.init size (fun i -> 
      TEcst (Cint (Int64.of_int i))
    ) in
    
    (* 使用既有的TElist編譯功能 *)
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
        | Badd when (match e1 with TEcst (Cstring _) -> true | _ -> false) ->
          (* 處理字串拼接 *)
          compile_expr e1 ++
          call "printf" ++
          compile_expr e2 ++
          call "printf" ++
          movq (imm 10) (!%rdi) ++
          call "putchar"
        | Beq | Bneq | Bgt | Bge | Blt | Ble | Band | Bor ->
          (* 處理比較運算和布林運算 *)
          compile_expr e ++
          movq (reg rax) (reg rdi) ++
          call "print_value" ++
          movq (imm 10) (!%rdi) ++
          call "putchar"
        | _ ->
          (* 處理其他算術運算 *)
          compile_expr e ++
          movq (reg rax) (reg rdi) ++
          call "print_value" ++
          movq (imm 10) (!%rdi) ++
          call "putchar"
        end
      | TEcall (fn, arg) when fn.fn_name = "len" ->
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
      subq (imm 1024) (reg rsp) ++
      compile_stmt body ++     (* 編譯函數體 *)
      movq (imm 0) (reg rax) ++  (* 返回值0 *)
      movq (reg rbp) (reg rsp) ++
      popq rbp ++
      ret
  | _ -> nop

let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  let print_wrap1 = print_int_wrapper in
  let print_wrap2 = print_bool_wrapper in
  let print_wrap3 = print_string_wrapper in
  if !debug then Printf.printf "Compiling file with %d definitions\n" (List.length p);
  let code = List.fold_left (fun code def -> code ++ compile_fun def) nop p in
  let string_data =
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
    text = malloc_wrapper ++         (* malloc包裝函數 *)
            print_wrap1 ++           (* int print *)
            print_wrap2 ++           (* bool print *)
            print_wrap3 ++           (* string print *)
            print_value_wrapper ++   (* general print *)
            code;
    data = string_data ++
           string_data2;  (* 格式字符串 *)
  }