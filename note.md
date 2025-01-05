
Boolean Condition:
	false -> None, False, 0, empty string, empty list
	true -> false 以外的

	and 運算 -> 如果第一個是 false, 後面不用算
	or 運算 -> 如果第一個是 true, 前面不用算

Iteration: 	for x in e: s
	e 要是 list

Operators:
	減 乘 除 MOD 加負號 -> 只有 int 能做這些
	加 -> two parameters of the same type
			string -> concatenation (in a new string)
			list -> concatenation (in a new list)
 
Comparison Operators: 	<, <=, >, >=, ==, !=
	always return a Boolean value (False being 0 and True being 1)
	對 int bool -> 數學操作
	對 list string -> 字典序比較

Build-in Functions:
	len -> 只能用在 list string

===========================================


open Format
open X86_64
open Ast

let debug = ref false

exception Error of string
let error s = raise (Error s)

type value =
  | Vnone
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vlist of value array

let rec print_value = function
  | Vnone -> printf "None"
  | Vbool true -> printf "True"
  | Vbool false -> printf "False"
  | Vint n -> printf "%d" n
  | Vstring s -> printf "%s" s
  | Vlist a ->
    let n = Array.length a in
    printf "[";
    for i = 0 to n-1 do print_value a.(i); if i < n-1 then printf ", " done;
    printf "]"

let is_false v = match v with
  | Vint n -> n = 0
  | Vnone
  | Vbool false
  | Vstring ""
  | Vlist [||] -> true (*[||] 代表空array *)
  | _ -> false

let is_true v = not (is_false v)

let rec compare_value v1 v2 = match v1, v2 with
  | Vlist a1, Vlist a2 -> 
      let l1 = Array.length a1 in
      let l2 = Array.length a2 in
      let rec aux i =
        if i = l1 && i = l2 then 0
        else if i = l2 then 1
        else if i = l1 then -1
        else 
          let c = compare_value a1.(i) a2.(i) in
          if c <> 0 then c else aux (i + 1)
      in aux 0
  | _ -> compare v1 v2

let functions = (Hashtbl.create 16 : (string, ident list * stmt) Hashtbl.t)

exception Return of value

type ctx = (string, value) Hashtbl.t

let rec expr ctx = function
  | Ecst Cnone ->
      Vnone
  | Ecst (Cstring s) ->
      Vstring s
  (* arithmetic *)
  | Ecst (Cint n) ->
      Vint (Int64.to_int n)
  | Ebinop (Badd | Bsub | Bmul | Bdiv | Bmod |
            Beq | Bneq | Blt | Ble | Bgt | Bge as op, e1, e2) ->
      let v1 = expr ctx e1 in
      let v2 = expr ctx e2 in
      begin match op, v1, v2 with
        | Badd, Vint n1, Vint n2 -> Vint (n1+n2)
        | Bsub, Vint n1, Vint n2 -> Vint (n1-n2)
        | Bmul, Vint n1, Vint n2 -> Vint (n1*n2)
        | (Bdiv | Bmod), Vint _, Vint 0 -> error "cant divide by zero"
        | Bdiv, Vint n1, Vint n2 -> Vint (n1/n2)
        | Bmod, Vint n1, Vint n2 -> Vint (n1 mod n2)
        | Beq, _, _  -> Vbool (compare_value v1 v2 = 0)
        | Bneq, _, _ -> Vbool (compare_value v1 v2 <> 0)
        | Blt, _, _  -> Vbool (compare_value v1 v2 < 0)
        | Ble, _, _  -> Vbool (compare_value v1 v2 <= 0)
        | Bgt, _, _  -> Vbool (compare_value v1 v2 > 0)
        | Bge, _, _  -> Vbool (compare_value v1 v2 >= 0)
        | Badd, Vstring s1, Vstring s2 -> Vstring (s1 ^ s2)
        | Badd, Vlist l1, Vlist l2 -> Vlist (Array.append l1 l2)
        | _ -> error "wrong operand types"
      end
  | Eunop (Uneg, e1) ->
    begin match expr ctx e1 with
      | Vint n -> Vint (-n)
      | _ -> error "wrong operand types" end
  (* Boolean *)
  | Ecst (Cbool b) ->
      Vbool b
  | Ebinop (Band, e1, e2) ->
      let v1 = expr ctx e1 in
      if is_true v1 then expr ctx e2 else v1
  | Ebinop (Bor, e1, e2) ->
      let v1 = expr ctx e1 in
      if is_false v1 then expr ctx e2 else v1
  | Eunop (Unot, e1) ->
      Vbool (is_false (expr ctx e1))
  | Eident {id} ->
      if not (Hashtbl.mem ctx id) then error "variable error";
      Hashtbl.find ctx id
  (* function call *)
  | Ecall ({id="len"}, [e1]) ->
      begin match expr ctx e1 with
        | Vstring s -> Vint (String.length s)
        | Vlist l -> Vint (Array.length l)
        | _ -> error "wrong method 'len'" end
  | Ecall ({id="list"}, [Ecall ({id="range"}, [e1])]) ->
      let n = expr_int_check ctx e1 in
      Vlist (Array.init (max 0 n) (fun i -> Vint i))
  | Ecall ({id=f}, el) ->
      if not (Hashtbl.mem functions f) then error ("unbound function " ^ f);
      let args, body = Hashtbl.find functions f in
      if List.length args <> List.length el then error "bad arity";
      let ctx' = Hashtbl.create 16 in
      List.iter2 (fun {id=x} e -> Hashtbl.add ctx' x (expr ctx e)) args el;
      begin try stmt ctx' body; Vnone with Return v -> v end
  | Elist el ->
      Vlist (Array.of_list (List.map (expr ctx) el))
  | Eget (e1, e2) ->
      begin match expr ctx e1 with
      | Vlist l ->
          let i = expr_int_check ctx e2 in
          (try l.(i) with Invalid_argument _ -> error "index out of bounds")
      | _ -> error "list expected" end

  and expr_int_check ctx e = match expr ctx e with
    | Vbool false -> 0
    | Vbool true -> 1
    | Vint n -> n
    | _ -> error "integer expected"

  and stmt ctx = function
    | Seval e ->
        ignore (expr ctx e)
    | Sprint e ->
        print_value (expr ctx e); printf "@."
    | Sblock bl ->
        block ctx bl
    | Sif (e, s1, s2) ->
        if is_true (expr ctx e) then stmt ctx s1 else stmt ctx s2
    | Sassign ({id}, e1) ->
        Hashtbl.replace ctx id (expr ctx e1)
    | Sreturn e ->
        raise (Return (expr ctx e))
    | Sfor ({id=a}, e, s) ->
        begin match expr ctx e with
        | Vlist l ->
          Array.iter (fun v -> Hashtbl.replace ctx a v; stmt ctx s) l
        | _ -> error "list expected" end
    | Sset (e1, e2, e3) ->
        begin match expr ctx e1 with
        | Vlist l -> l.(expr_int_check ctx e2) <- expr ctx e3
        | _ -> error "list expected" end

  and block ctx = function
    | [] -> ()
    | s :: sl -> stmt ctx s; block ctx sl
  
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

(* 產生字串值 *)
let string_literals = ref ([] : (string * string) list)
let string_counter = ref 0
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


let rec compile_list_elements el offset reg_list =
  match el with
  | [] -> nop
  | e::rest ->
      let next_offset = offset + size_int in
      movq (reg rax) (ind ~ofs:offset reg_list) ++
      compile_list_elements rest next_offset reg_list

let rec compile_expr e =
  begin match e with
  | TEcst (Cnone) -> create_none
  | TEcst (Cbool b) -> create_bool b
  | TEcst (Cint n) -> create_int (Int64.to_int n)
  | TEcst (Cstring s) -> create_string s
  | TElist el ->
    let size = List.length el in
    if size = 0 then
      allocate_memory (2 * size_int) ++   (* tag + length *)
      movq (imm tag_list) (ind rax) ++    (* 設定 tag *)
      movq (imm 0) (ind ~ofs:size_int rax)  (* 設定長度為0 *)
    else
      allocate_memory ((2 + size) * size_int) ++   (* tag + length + elements *)
      movq (imm tag_list) (ind rax) ++           (* 設定 tag *)
      movq (imm size) (ind ~ofs:size_int rax) ++  (* 設定長度 *) 
      pushq (reg rax)  (* 保存列表指標 *) ++
      (* 依序處理每個元素 *)
      movq (reg rax) (reg r11) ++          (* 保存列表指標到 r11 *)
      List.fold_left (fun code e ->
        code ++
        compile_expr e ++                   (* 編譯元素 *)
        pushq (reg rax)                     (* 保存元素值 *)
      ) nop el ++
      (* 反向填入元素 *)
      movq (reg r11) (reg r10) ++          (* 取回列表指標 *)
      List.fold_left (fun code _ ->
        code ++
        popq rax ++                         (* 取出元素值 *)
        movq (reg rax) (ind ~ofs:(2 * size_int) r10) ++  (* 填入元素 *)
        addq (imm size_int) (reg r10)      (* 移動到下一個元素位置 *)
      ) nop el ++
      movq (reg r11) (reg rax)

  (* 編譯二元運算 *)
  | TEbinop (op, e1, e2) ->
      let compile_arithmetic e1 e2 instr =
        compile_expr e1 ++                    (* 編譯左運算元 *)
        pushq (reg rax) ++                    (* 保存左運算元 *)
        compile_expr e2 ++                    (* 編譯右運算元 *)
        movq (reg rax) (reg r11) ++          (* 保存右運算元 *)
        popq rax ++                          (* 恢復左運算元 *)
        (* 取出整數值 *)
        movq (ind ~ofs:size_int rax) (reg rsi) ++  (* 左值 *)
        movq (ind ~ofs:size_int r11) (reg rdi) ++  (* 右值 *)
        instr ++                             (* 執行運算 *)
        (* 建立新的整數值 *)
        pushq (reg rax) ++                   (* 保存運算結果 *)
        create_int 0 ++                      (* 分配新的整數物件 *)
        popq rsi ++                          (* 恢復運算結果 *)
        movq (reg rsi) (ind ~ofs:size_int rax)  (* 存入結果 *)
      in
      begin match op with
      (* 算術運算 *)
      | Badd -> compile_arithmetic e1 e2 (addq (reg rdi) (reg rsi))
      | Bsub -> compile_arithmetic e1 e2 (subq (reg rdi) (reg rsi))
      | Bmul -> compile_arithmetic e1 e2 (imulq (reg rdi) (reg rsi))
      | Bdiv -> 
          compile_arithmetic e1 e2 (
            cmpq (imm 0) (reg rdi) ++
            je "error_div_by_zero" ++
            movq (reg rsi) (reg rax) ++
            cqto ++
            idivq (reg rdi)
          )
      | Bmod ->
          compile_arithmetic e1 e2 (
            cmpq (imm 0) (reg rdi) ++
            je "error_div_by_zero" ++
            movq (reg rsi) (reg rax) ++
            cqto ++
            idivq (reg rdi) ++
            movq (reg rdx) (reg rax)  (* 餘數在 rdx *)
          )
      
      (* 比較運算 *)
      | Beq | Bneq | Blt | Ble | Bgt | Bge as cmp ->
        compile_expr e1 ++
        pushq (reg rax) ++
        compile_expr e2 ++
        movq (reg rax) (reg r11) ++
        popq rax ++
        (* 比較邏輯 *)
        let make_comparison jump_insn =
          let l_true = new_label () in
          let l_end = new_label () in
          movq (ind ~ofs:size_int rax) (reg rsi) ++
          movq (ind ~ofs:size_int r11) (reg rdi) ++
          cmpq (reg rdi) (reg rsi) ++
          jump_insn l_true ++
          create_bool false ++
          jmp l_end ++
          label l_true ++
          create_bool true ++
          label l_end
        in
        (match cmp with
          | Beq  -> make_comparison je
          | Bneq -> make_comparison jne
          | Blt  -> make_comparison jl
          | Ble  -> make_comparison jle
          | Bgt  -> make_comparison jg
          | Bge  -> make_comparison jge
          | _ -> assert false)
      
      (* 邏輯運算 *)
      | Band -> 
        let l_false = new_label () in
        let l_end = new_label () in
        compile_expr e1 ++
        (* 短路求值：若第一個運算元為假，直接返回 *)
        cmpq (imm 0) (ind ~ofs:size_int rax) ++
        je l_false ++
        compile_expr e2 ++
        jmp l_end ++
        label l_false ++
        create_bool false ++
        label l_end
      | Bor ->
        let l_true = new_label () in
        let l_end = new_label () in
        compile_expr e1 ++
        (* 短路求值：若第一個運算元為真，直接返回 *)
        cmpq (imm 0) (ind ~ofs:size_int rax) ++
        jne l_true ++
        compile_expr e2 ++
        jmp l_end ++
        label l_true ++
        create_bool true ++
        label l_end
      end
  | TEunop (op, e) ->
    begin match op with
    | Uneg ->
        compile_expr e ++
        (* 檢查型別並取negation *)
        movq (ind ~ofs:size_int rax) (reg rsi) ++
        negq (reg rsi) ++
        create_int 0 ++
        movq (reg rsi) (ind ~ofs:size_int rax)
    | Unot ->
        compile_expr e ++
        (* 將值轉換為布林值 *)
        test_value_false ++
        setne (reg al) ++
        movzbq (reg al) (rsi) ++
        create_bool false ++
        movq (reg rsi) (ind ~ofs:size_int rax)
    end
  | TEvar v ->
    movq (ind ~ofs:v.v_ofs rbp) (reg rax)
  | TEcall (fn, args) ->
      (* 編譯函數呼叫 *)
      let allocate_params =
        List.fold_left (fun code arg ->
          code ++
          compile_expr arg ++
          pushq (reg rax)
        ) nop args in
      
      allocate_params ++
      call fn.fn_name ++
      (* 清理參數 *)
      addq (imm (8 * List.length args)) (reg rsp)
  | _ -> nop end

  and test_value_false =
    let l_false = new_label () in
    let l_true = new_label () in
    let l_next1 = new_label () in
    let l_next2 = new_label () in
    let l_next3 = new_label () in
    let l_next4 = new_label () in
    let l_end = new_label () in
  
    (* 首先取得型別標籤 *)
    movq (ind rax) (reg rsi) ++
  
    (* 檢查 None *)
    cmpq (imm tag_none) (reg rsi) ++
    jne l_next1 ++
    jmp l_false ++  (* None 是 false *)
  
    (* 檢查布林值 *)
    label l_next1 ++
    cmpq (imm tag_bool) (reg rsi) ++
    jne l_next2 ++
    movq (ind ~ofs:size_int rax) (reg rsi) ++
    cmpq (imm 0) (reg rsi) ++
    je l_false ++  (* False 是 false *)
    jmp l_true ++
  
    (* 檢查整數 *)
    label l_next2 ++
    cmpq (imm tag_int) (reg rsi) ++
    jne l_next3 ++
    movq (ind ~ofs:size_int rax) (reg rsi) ++
    cmpq (imm 0) (reg rsi) ++
    je l_false ++  (* 0 是 false *)
    jmp l_true ++
  
    (* 檢查字串 *)
    label l_next3 ++
    cmpq (imm tag_string) (reg rsi) ++
    jne l_next4 ++
    movq (ind ~ofs:size_int rax) (reg rsi) ++
    cmpq (imm 0) (reg rsi) ++
    je l_false ++  (* 空字串是 false *)
    jmp l_true ++
  
    (* 檢查列表 *)
    label l_next4 ++
    cmpq (imm tag_list) (reg rsi) ++
    jne l_true ++  (* 不認識的型別當作 true *)
    movq (ind ~ofs:size_int rax) (reg rsi) ++
    cmpq (imm 0) (reg rsi) ++
    je l_false ++  (* 空列表是 false *)
    
    (* 設定返回值 *)
    label l_true ++
    movq (imm 1) (reg rax) ++
    jmp l_end ++
  
    label l_false ++
    movq (imm 0) (reg rax) ++
  
    label l_end
  
    and compile_compare op =
      let l_true = new_label () in
      let l_false = new_label () in
      let l_end = new_label () in
      let l_next = new_label () in
    
      (* 先檢查兩個運算元的型別是否相同 *)
      movq (ind rax) (reg rcx) ++    (* 第一個運算元的型別 *)
      movq (ind r11) (reg rdx) ++    (* 第二個運算元的型別 *)
      cmpq (reg rcx) (reg rdx) ++
      jne l_false ++                 (* 型別不同，直接返回 false *)
    
      (* 根據型別進行不同的比較 *)
      cmpq (imm tag_int) (reg rcx) ++
      je "compare_int" ++            (* 整數比較 *)
      cmpq (imm tag_bool) (reg rcx) ++
      je "compare_bool" ++           (* 布林比較 *)
      cmpq (imm tag_string) (reg rcx) ++
      je "compare_string" ++         (* 字串比較 *)
      cmpq (imm tag_list) (reg rcx) ++
      je "compare_list" ++           (* 列表比較 *)
      cmpq (imm tag_none) (reg rcx) ++
      je "compare_none" ++           (* None 比較 *)
      jmp l_false ++                 (* 不支援的型別比較 *)
    
      (* None 的比較 *)
      label "compare_none" ++
      (* None 只與 None 相等 *)
      cmpq (reg rcx) (reg rdx) ++
      je l_true ++
      jmp l_false ++
    
      (* 整數比較 *)
      label "compare_int" ++
      movq (ind ~ofs:size_int rax) (reg rsi) ++   (* 第一個數 *)
      movq (ind ~ofs:size_int r11) (reg rdi) ++   (* 第二個數 *)
      cmpq (reg rdi) (reg rsi) ++
      (match op with
       | Beq  -> je  l_true
       | Bneq -> jne l_true
       | Blt  -> jl  l_true
       | Ble  -> jle l_true
       | Bgt  -> jg  l_true
       | Bge  -> jge l_true
       | _ -> nop) ++
      jmp l_false ++
    
      (* 布林值比較 *)
      label "compare_bool" ++
      movq (ind ~ofs:size_int rax) (reg rsi) ++
      movq (ind ~ofs:size_int r11) (reg rdi) ++
      cmpq (reg rdi) (reg rsi) ++
      (match op with
       | Beq  -> je  l_true
       | Bneq -> jne l_true
       | _ -> jmp l_false) ++
    
      (* 字串比較：需要調用 strcmp *)
      label "compare_string" ++
      leaq (ind ~ofs:(2 * size_int) rax) (rdi) ++  (* 第一個字串 *)
      leaq (ind ~ofs:(2 * size_int) r11) (rsi) ++  (* 第二個字串 *)
      pushq (reg rax) ++
      pushq (reg r11) ++
      call "strcmp" ++                               (* 調用 strcmp *)
      popq r11 ++
      popq rcx ++
      cmpq (imm 0) (reg rax) ++                     (* 比較 strcmp 結果 *)
      (match op with
       | Beq  -> je  l_true
       | Bneq -> jne l_true
       | Blt  -> jl  l_true
       | Ble  -> jle l_true
       | Bgt  -> jg  l_true
       | Bge  -> jge l_true
       | _ -> nop) ++
      jmp l_false ++
    
      (* 列表比較 *)
      label "compare_list" ++
      (* 先比較長度 *)
      movq (ind ~ofs:size_int rax) (reg rsi) ++     (* 第一個列表長度 *)
      movq (ind ~ofs:size_int r11) (reg rdi) ++     (* 第二個列表長度 *)
      cmpq (reg rdi) (reg rsi) ++
      jne (match op with
           | Beq | Bneq -> l_false
           | Blt -> l_true
           | Ble -> l_true
           | Bgt -> l_false
           | Bge -> l_false
           | _ -> l_false) ++
      
      (* 產生結果 *)
      label l_true ++
      pushq (reg rax) ++                    (* 保存 rax *)
      create_bool true ++
      popq r11 ++                           (* 恢復 r11 *)
      jmp l_end ++
    
      label l_false ++
      pushq (reg rax) ++                    (* 保存 rax *)
      create_bool false ++
      popq r11 ++                           (* 恢復 r11 *)
    
      label l_end ++
      nop
  
(* 編譯敘述 *)
let rec compile_stmt = function
  | TSprint e ->
      compile_expr e ++
      pushq (reg rax) ++  (* 保存要印出的值 *)
      movq (ind rax) (reg rsi) ++  (* 取得型別標籤 *)
      
      (* 根據型別選擇適當的印出方式 *)
      cmpq (imm tag_none) (reg rsi) ++
      je "print_none" ++
      cmpq (imm tag_bool) (reg rsi) ++
      je "print_bool" ++
      cmpq (imm tag_int) (reg rsi) ++
      je "print_int" ++
      cmpq (imm tag_string) (reg rsi) ++
      je "print_string" ++
      cmpq (imm tag_list) (reg rsi) ++
      je "print_list" ++
      
      (* 印出 None *)
      label "print_none" ++
      leaq (lab "fmt_string") (rdi) ++
      leaq (lab "str_none") (rsi) ++
      movq (imm 0) (reg rax) ++  (* printf 需要 rax = 0 *)
      call "printf" ++
      jmp "print_end" ++
      
      (* 印出布林值 *)
      label "print_bool" ++
      movq (ind ~ofs:size_int rax) (reg rsi) ++
      leaq (lab "fmt_string") (rdi) ++
      cmpq (imm 0) (reg rsi) ++
      je "print_false" ++
      leaq (lab "str_true") (rsi) ++
      jmp "do_print_bool" ++
      label "print_false" ++
      leaq (lab "str_false") (rsi) ++
      label "do_print_bool" ++
      movq (imm 0) (reg rax) ++
      call "printf" ++
      jmp "print_end" ++
      
      (* 印出整數 *)
      label "print_int" ++
      leaq (lab "fmt_int") (rdi) ++
      movq (ind ~ofs:size_int rax) (reg rsi) ++
      movq (imm 0) (reg rax) ++
      call "printf" ++
      jmp "print_end" ++
      
      (* 印出字串 *)
      label "print_string" ++
      leaq (lab "fmt_string") (rdi) ++
      leaq (ind ~ofs:(2 * size_int) rax) (rsi) ++
      movq (imm 0) (reg rax) ++
      call "printf" ++
      jmp "print_end" ++
      
      (* 印出列表 *)
      label "print_list" ++
      (* TODO: 實作列表的印出 *)
      jmp "print_end" ++
      
      label "print_end" ++
      popq rax  (* 恢復值 *)
  
  | TSassign (v, e) ->
      compile_expr e ++             (* 編譯賦值運算式 *)
      movq (reg rax) (ind ~ofs:v.v_ofs rbp)  (* 存入變數 *)
  
  | TSif (e, s1, s2) ->
      let l_else = new_label () in
      let l_end = new_label () in
      compile_expr e ++             (* 編譯條件 *)
      test_value_false ++          (* 測試結果 *)
      je l_else ++                 (* 若為假則跳到else *)
      compile_stmt s1 ++           (* 編譯then部分 *)
      jmp l_end ++
      label l_else ++
      compile_stmt s2 ++           (* 編譯else部分 *)
      label l_end
  
  | TSblock sl ->
      List.fold_left (fun code s -> code ++ compile_stmt s) nop sl
  
  | TSfor (v, e, s) ->
      let l_start = new_label () in
      let l_cond = new_label () in
      let l_body = new_label () in
      let l_end = new_label () in
      
      compile_expr e ++            (* 編譯列表運算式 *)
      pushq (reg rax) ++          (* 保存列表指標 *)
      movq (imm 0) (reg r12) ++   (* 初始化計數器 *)
      
      label l_start ++
      (* 檢查是否到達列表尾 *)
      movq (ind ~ofs:size_int rax) (reg rcx) ++  (* 取得列表長度 *)
      cmpq (reg r12) (reg rcx) ++
      jge l_end ++
      
      (* 取得當前元素並賦值給迭代變數 *)
      movq (ind ~ofs:(2 * size_int) rax ~index:r12 ~scale:8) (reg rdx) ++
      movq (reg rdx) (ind ~ofs:v.v_ofs rbp) ++
      
      (* 編譯循環體 *)
      compile_stmt s ++
      
      (* 增加計數器並繼續 *)
      incq (reg r12) ++
      jmp l_start ++
      
      label l_end ++
      popq rax  (* 恢復列表指標 *)
  
  | TSset (e1, e2, e3) ->
      compile_expr e1 ++           (* 編譯列表 *)
      pushq (reg rax) ++          (* 保存列表指標 *)
      compile_expr e2 ++           (* 編譯索引 *)
      pushq (reg rax) ++          (* 保存索引值 *)
      compile_expr e3 ++           (* 編譯要設定的值 *)
      movq (reg rax) (reg rcx) ++  (* 保存值 *)
      popq rdx ++                 (* 恢復索引值 *)
      popq rax ++                 (* 恢復列表指標 *)
      
      (* 檢查索引範圍 *)
      movq (ind ~ofs:size_int rax) (reg rdi) ++
      movq (ind ~ofs:size_int rdx) (reg rsi) ++
      cmpq (reg rdi) (reg rsi) ++
      jle "error_index" ++
      
      (* 設定列表元素 *)
      movq (reg rcx) (ind ~ofs:(2 * size_int) rax ~index:rsi ~scale:8)
  | TSreturn e ->
    compile_expr e ++
    movq (reg rbp) (reg rsp) ++
    popq rbp ++
    ret

  | TSeval e ->
      compile_expr e

(* 編譯函數定義 *)
let compile_fun (fn, body) =
  let setup_param i =
    let offset = -(i + 1) * size_int in
    movq (ind ~ofs:((i + 2) * size_int) rbp) (reg rax) ++  (* 從調用者的堆疊取參數 *)
    movq (reg rax) (ind ~ofs:offset rbp)                   (* 存到當前函數的堆疊 *)
  in

  label fn.fn_name ++
  
  (* 函數序言 *)
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  
  (* 計算並分配區域變數所需空間 *)
  let local_size = List.length fn.fn_params * size_int in
  subq (imm local_size) (reg rsp) ++
  
  (* 保存函數參數 - 直接展開代碼 *)
  (match List.length fn.fn_params with
  | 0 -> nop
  | 1 -> setup_param 0
  | 2 -> setup_param 0 ++ setup_param 1
  | 3 -> setup_param 0 ++ setup_param 1 ++ setup_param 2
  | 4 -> setup_param 0 ++ setup_param 1 ++ setup_param 2 ++ setup_param 3
  | n -> 
      (* 如果參數太多，可以加入錯誤處理 *)
      nop) ++
  
  (* 編譯函數體 *)
  compile_stmt body ++
  
  (* 若沒有明確的return，返回None *)
  create_none ++
  
  (* 函數收尾 *)
  movq (reg rbp) (reg rsp) ++
  popq rbp ++
  ret

let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  
  { text =
      (* 外部函數 *)
      inline ".extern malloc\n" ++
      inline ".extern strcpy\n" ++
      inline ".extern strcmp\n" ++
      inline ".extern printf\n" ++
      
      (* 記憶體分配包裝函式 *)
      inline (malloc_wrapper()) ++
    
      (* 運行時錯誤處理函式 *)
      label "error_div_by_zero" ++
      leaq (lab "error_div_msg") (rdi) ++
      call "printf" ++
      movq (imm 1) (reg rdi) ++
      call "exit" ++
      
      label "error_type" ++
      leaq (lab "error_type_msg") (rdi) ++
      call "printf" ++
      movq (imm 1) (reg rdi) ++
      call "exit" ++
      
      (* 主程式 *)
      globl "main" ++
      label "main" ++
      pushq (reg rbp) ++
      movq (reg rsp) (reg rbp) ++
      
      (* 編譯主體程式 *)
      (* List.fold_left (fun code def ->
        code ++
        compile_fun def
      ) nop p ++  *)
            
      (* 清理和返回 *)
      movq (imm 0) (reg rax) ++
      movq (reg rbp) (reg rsp) ++
      popq rbp ++
      ret;
      
    data =
      (* 字串常量 *)
      List.fold_left (fun code (lbl, str) ->
        code ++
        X86_64.label lbl ++
        string str
      ) nop !string_literals ++
      
      (* 錯誤訊息 *)
      label "error_div_msg" ++ string "error: division by zero\n" ++
      label "error_type_msg" ++ string "error: type error\n" ++
      label "fmt_int" ++ string "%d\n" ++
      label "fmt_bool" ++ string "%s\n" ++
      label "fmt_string" ++ string "%s\n" ++
      label "str_true" ++ string "True\n" ++
      label "str_false" ++ string "False\n" ++
      label "str_none" ++ string "None\n"
  }