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
  pushq (imm n) ++            (* 保存要設置的值 *)
  allocate_memory (2 * size_int) ++     (* tag + value *)
  movq (imm tag_int) (ind rax) ++       (* 設置 tag *)
  popq rsi ++                  (* 恢復值 *)
  movq (reg rsi) (ind ~ofs:size_int rax)  (* 設置值 *)

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
          jne l_list ++
          
          (* 整數加法 *)
          movq (ind ~ofs:size_int rax) (reg rsi) ++  (* 取得第一個整數值 *)
          movq (ind ~ofs:size_int r11) (reg rdi) ++  (* 取得第二個整數值 *)
          pushq (reg rsi) ++                         (* 保存第一個值 *)
          pushq (reg rdi) ++                         (* 保存第二個值 *)
          addq (reg rdi) (reg rsi) ++                (* 執行加法 *)
          pushq (reg rsi) ++                         (* 保存結果 *)
          
          (* 分配新空間保存結果 *)
          movq (imm (2 * size_int)) (reg rdi) ++
          call "my_malloc" ++
          
          (* 設置結果對象 *)
          movq (imm tag_int) (ind rax) ++           (* 設置整數標籤 *)
          popq rsi ++                               (* 恢復計算結果 *)
          movq (reg rsi) (ind ~ofs:size_int rax) ++ (* 保存計算結果 *)
          addq (imm 16) (reg rsp) ++                (* 清理堆疊 *)
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
          (* 列表連接 *)
          label l_list ++
          cmpq (imm tag_list) (reg rcx) ++
          jne "error" ++
          cmpq (imm tag_list) (reg rdx) ++
          jne "error" ++

          (* 取得兩個列表的長度 *)
          pushq (reg rax) ++
          pushq (reg r11) ++
          movq (ind ~ofs:size_int rax) (reg r12) ++   (* 第一個列表的長度 *)
          movq (ind ~ofs:size_int r11) (reg r13) ++   (* 第二個列表的長度 *)
          
          (* 計算新列表的大小 *)
          movq (reg r12) (reg rdi) ++
          addq (reg r13) (reg rdi) ++                 (* 總長度 *)
          pushq (reg rdi) ++                          (* 保存總長度 *)
          imulq (imm size_int) (reg rdi) ++           (* 元素所需空間 *)
          addq (imm (2 * size_int)) (reg rdi) ++      (* 加上 tag 和 length *)
          
          (* 分配新列表空間 *)
          call "malloc" ++
          
          (* 設定新列表的標頭 *)
          popq rdi ++                                 (* 恢復總長度 *)
          movq (imm tag_list) (ind rax) ++
          movq (reg rdi) (ind ~ofs:size_int rax) ++
          
          (* 複製第一個列表的元素 *)
          movq (reg rax) (reg r14) ++                 (* r14 = 新列表 *)
          popq r11 ++                                 (* 恢復原列表指針 *)
          popq r15 ++                                 (* r15 = 第一個列表 *)
          
          (* 複製第一個列表 *)
          xorq (reg rcx) (reg rcx) ++                 (* rcx = 0 (計數器) *)
          label "copy_list1_loop" ++
          cmpq (reg r12) (reg rcx) ++                (* 比較計數器和長度 *)
          je "copy_list1_end" ++
          
          movq (reg rcx) (reg rdx) ++
          imulq (imm size_int) (reg rdx) ++
          addq (imm (2 * size_int)) (reg rdx) ++
          movq (ind ~index:rdx r15) (reg rdi) ++     (* 載入源元素 *)
          movq (ind ~index:rdx r14) (reg rdi) ++     (* 存入目標位置 *)
          
          incq (reg rcx) ++
          jmp "copy_list1_loop" ++
          
          label "copy_list1_end" ++
          
          (* 複製第二個列表的元素 *)
          xorq (reg rcx) (reg rcx) ++                (* rcx = 0 (計數器) *)
          label "copy_list2_loop" ++
          cmpq (reg r13) (reg rcx) ++               (* 比較計數器和長度 *)
          je "copy_list2_end" ++
          
          movq (reg rcx) (reg rdx) ++
          imulq (imm size_int) (reg rdx) ++
          addq (imm (2 * size_int)) (reg rdx) ++
          movq (ind ~index:rdx r11) (reg rdi) ++    (* 載入源元素 *)
          
          movq (reg r12) (reg r8) ++                (* r8 = 第一個列表長度 + 當前索引 *)
          addq (reg rcx) (reg r8) ++
          imulq (imm size_int) (reg r8) ++
          addq (imm (2 * size_int)) (reg r8) ++
          movq (reg rdi) (ind ~index:r8 r14) ++     (* 存入目標位置 *)
          
          incq (reg rcx) ++
          jmp "copy_list2_loop" ++
          
          label "copy_list2_end" ++
          
          movq (reg r14) (reg rax) ++               (* 返回新列表 *)
          label l_end

      (* 減法 *)
      | Bsub ->
        (* 計算兩個表達式的值 *)
        compile_expr e1 ++               (* 計算第一個表達式，結果在 rax *)
        pushq (reg rax) ++               (* 保存第一個值 *)
        compile_expr e2 ++               (* 計算第二個表達式，結果在 rax *)
        movq (reg rax) (reg r11) ++      (* 第二個值移到 r11 *)
        popq rax ++                      (* 恢復第一個值到 rax *)
        
        (* 檢查類型 *)
        movq (ind rax) (reg rcx) ++      (* 取得第一個值的類型 *)
        movq (ind r11) (reg rdx) ++      (* 取得第二個值的類型 *)
        
        (* 檢查是否為整數 *)
        cmpq (imm tag_int) (reg rcx) ++
        jne "error" ++
        cmpq (imm tag_int) (reg rdx) ++
        jne "error" ++
        
        (* 整數減法 *)
        movq (ind ~ofs:size_int rax) (reg rsi) ++  (* 取得第一個整數值 *)
        movq (ind ~ofs:size_int r11) (reg rdi) ++  (* 取得第二個整數值 *)
        pushq (reg rsi) ++                         (* 保存第一個值 *)
        pushq (reg rdi) ++                         (* 保存第二個值 *)
        subq (reg rdi) (reg rsi) ++                (* 執行減法 *)
        pushq (reg rsi) ++                         (* 保存結果 *)
        
        (* 分配新空間保存結果 *)
        movq (imm (2 * size_int)) (reg rdi) ++
        call "my_malloc" ++
        
        (* 設置結果對象 *)
        movq (imm tag_int) (ind rax) ++           (* 設置整數標籤 *)
        popq rsi ++                               (* 恢復計算結果 *)
        movq (reg rsi) (ind ~ofs:size_int rax) ++ (* 保存計算結果 *)
        addq (imm 16) (reg rsp)                   (* 清理堆疊 *)

      (* 乘法 *)
      | Bmul ->
        (* 計算兩個表達式的值 *)
        compile_expr e1 ++               (* 計算第一個表達式，結果在 rax *)
        pushq (reg rax) ++               (* 保存第一個值 *)
        compile_expr e2 ++               (* 計算第二個表達式，結果在 rax *)
        movq (reg rax) (reg r11) ++      (* 第二個值移到 r11 *)
        popq rax ++                      (* 恢復第一個值到 rax *)

        (* 檢查類型 *)
        movq (ind rax) (reg rcx) ++      (* 取得第一個值的類型 *)
        movq (ind r11) (reg rdx) ++      (* 取得第二個值的類型 *)

        (* 檢查是否為整數 *)
        cmpq (imm tag_int) (reg rcx) ++
        jne "error" ++
        cmpq (imm tag_int) (reg rdx) ++
        jne "error" ++

        (* 整數乘法 *)
        movq (ind ~ofs:size_int rax) (reg rsi) ++  (* 取得第一個整數值 *)
        movq (ind ~ofs:size_int r11) (reg rdi) ++  (* 取得第二個整數值 *)
        pushq (reg rsi) ++                         (* 保存第一個值 *)
        pushq (reg rdi) ++                         (* 保存第二個值 *)
        imulq (reg rdi) (reg rsi) ++               (* 執行乘法 *)
        pushq (reg rsi) ++                         (* 保存結果 *)

        (* 分配新空間保存結果 *)
        movq (imm (2 * size_int)) (reg rdi) ++
        call "my_malloc" ++

        (* 設置結果對象 *)
        movq (imm tag_int) (ind rax) ++           (* 設置整數標籤 *)
        popq rsi ++                               (* 恢復計算結果 *)
        movq (reg rsi) (ind ~ofs:size_int rax) ++ (* 保存計算結果 *)
        addq (imm 16) (reg rsp)                   (* 清理堆疊 *)

      (* 除法 *)
      | Bdiv ->
        (* 計算兩個表達式的值 *)
        compile_expr e1 ++               (* 計算第一個表達式，結果在 rax *)
        pushq (reg rax) ++               (* 保存第一個值 *)
        compile_expr e2 ++               (* 計算第二個表達式，結果在 rax *)
        movq (reg rax) (reg r11) ++      (* 第二個值移到 r11 *)
        popq rax ++                      (* 恢復第一個值到 rax *)
        
        (* 檢查類型 *)
        movq (ind rax) (reg rcx) ++      (* 取得第一個值的類型 *)
        movq (ind r11) (reg rdx) ++      (* 取得第二個值的類型 *)
        
        (* 檢查是否為整數 *)
        cmpq (imm tag_int) (reg rcx) ++
        jne "error" ++
        cmpq (imm tag_int) (reg rdx) ++
        jne "error" ++
        
        (* 除法前的準備：檢查除數是否為0 *)
        movq (ind ~ofs:size_int r11) (reg rcx) ++
        cmpq (imm 0) (reg rcx) ++
        je "error_div_by_zero" ++
        
        (* 整數除法 *)
        movq (ind ~ofs:size_int rax) (reg rax) ++  (* 被除數到 rax *)
        pushq (reg rdx) ++                         (* 保存 rdx *)
        cqto ++                                    (* 擴展 rax 到 rdx:rax *)
        idivq (reg rcx) ++                         (* 執行除法，商在 rax *)
        pushq (reg rax) ++                         (* 保存商 *)
        
        (* 分配新空間保存結果 *)
        movq (imm (2 * size_int)) (reg rdi) ++
        call "my_malloc" ++
        
        (* 設置結果對象 *)
        movq (imm tag_int) (ind rax) ++           (* 設置整數標籤 *)
        popq rsi ++                               (* 恢復除法結果 *)
        movq (reg rsi) (ind ~ofs:size_int rax) ++ (* 保存結果 *)
        popq rdx                                  (* 恢復 rdx *)

      (* 取模 *)
      | Bmod ->
        (* 計算兩個表達式的值 *)
        compile_expr e1 ++               (* 計算第一個表達式，結果在 rax *)
        pushq (reg rax) ++               (* 保存第一個值 *)
        compile_expr e2 ++               (* 計算第二個表達式，結果在 rax *)
        movq (reg rax) (reg r11) ++      (* 第二個值移到 r11 *)
        popq rax ++                      (* 恢復第一個值到 rax *)
        
        (* 檢查類型 *)
        movq (ind rax) (reg rcx) ++      (* 取得第一個值的類型 *)
        movq (ind r11) (reg rdx) ++      (* 取得第二個值的類型 *)
        
        (* 檢查是否為整數 *)
        cmpq (imm tag_int) (reg rcx) ++
        jne "error" ++
        cmpq (imm tag_int) (reg rdx) ++
        jne "error" ++
        
        (* 取模前檢查除數是否為0 *)
        movq (ind ~ofs:size_int r11) (reg rcx) ++
        cmpq (imm 0) (reg rcx) ++
        je "error_div_by_zero" ++
        
        (* 執行取模運算 *)
        movq (ind ~ofs:size_int rax) (reg rax) ++  (* 被除數到 rax *)
        pushq (reg rdx) ++                         (* 保存 rdx *)
        cqto ++                                    (* 擴展 rax 到 rdx:rax *)
        idivq (reg rcx) ++                         (* 執行除法，餘數在 rdx *)
        movq (reg rdx) (reg rsi) ++                (* 保存餘數 *)
        pushq (reg rsi) ++                         (* 保存結果 *)
        
        (* 分配新空間保存結果 *)
        movq (imm (2 * size_int)) (reg rdi) ++
        call "my_malloc" ++
        
        (* 設置結果對象 *)
        movq (imm tag_int) (ind rax) ++           (* 設置整數標籤 *)
        popq rsi ++                               (* 恢復取模結果 *)
        movq (reg rsi) (ind ~ofs:size_int rax) ++ (* 保存結果 *)
        popq rdx                                  (* 恢復 rdx *)

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
  
  (* 在 compile_expr 中添加列表相關操作 *)
  | TElist elements ->
    (* 計算需要的記憶體大小：tag + length + elements *)
    let size = (2 + List.length elements) * size_int in
    (* 分配記憶體 *)
    allocate_memory size ++
    (* 設定tag和長度 *)
    movq (imm tag_list) (ind rax) ++
    movq (imm (List.length elements)) (ind ~ofs:size_int rax) ++
    (* 保存列表的基址 *)
    pushq (reg rax) ++
    (* 編譯並存入每個元素 *)
    let store_elements = 
      let cnt = ref 0 in
      List.fold_left (fun code e ->
        let i = !cnt in
        incr cnt;
        code ++
        compile_expr e ++
        movq (reg rax) (reg r11) ++  (* 暫存元素值 *)
        popq rax ++                  (* 恢復列表基址 *)
        movq (reg r11) (ind ~ofs:((i + 2) * size_int) rax) ++  (* 存入元素 *)
        pushq (reg rax)              (* 保存列表基址用於下一輪 *)
      ) nop elements in
    store_elements ++
    (* 恢復列表基址到 rax *)
    popq rax

  | TEget (lst, idx) ->
      (* 編譯列表表達式 *)
      compile_expr lst ++
      pushq (reg rax) ++     (* 保存列表指標 *)
      (* 編譯索引表達式 *)
      compile_expr idx ++
      (* 檢查索引型別 *)
      movq (ind rax) (reg rcx) ++
      cmpq (imm tag_int) (reg rcx) ++
      jne "error" ++
      (* 取出索引值 *)
      movq (ind ~ofs:size_int rax) (reg rcx) ++
      popq rax ++            (* 恢復列表指標 *)
      (* 檢查列表型別 *)
      movq (ind rax) (reg rdx) ++
      cmpq (imm tag_list) (reg rdx) ++
      jne "error" ++
      (* 檢查索引範圍 *)
      movq (ind ~ofs:size_int rax) (reg rdx) ++  (* 取得列表長度 *)
      cmpq (reg rdx) (reg rcx) ++
      jge "error" ++         (* 若索引 >= 長度，錯誤 *)
      cmpq (imm 0) (reg rcx) ++
      jl "error" ++          (* 若索引 < 0，錯誤 *)
      (* 計算元素位置並載入 *)
      imulq (imm size_int) (reg rcx) ++
      addq (imm (2 * size_int)) (reg rcx) ++
      movq (ind ~index:rcx rax) (reg rax)
  (* 在 compile_expr 中添加標準函數支援 *)
  
  | TEcall (fn, args) ->
    begin match fn.fn_name, args with
    | "len", [e] ->
        compile_expr e ++
        movq (ind rax) (reg rcx) ++
        (* 檢查參數型別 *)
        cmpq (imm tag_string) (reg rcx) ++
        je "len_string" ++
        cmpq (imm tag_list) (reg rcx) ++
        je "len_list" ++
        jmp "error" ++
        
        label "len_string" ++
        movq (ind ~ofs:size_int rax) (reg rsi) ++
        jmp "len_end" ++
        
        label "len_list" ++
        movq (ind ~ofs:size_int rax) (reg rsi) ++
        
        label "len_end" ++
        (* 建立整數返回值 *)
        create_int 0 ++
        movq (reg rsi) (ind ~ofs:size_int rax)

    | "range", [e] ->
        (* 計算範圍大小 *)
        compile_expr e ++
        movq (ind rax) (reg rcx) ++
        cmpq (imm tag_int) (reg rcx) ++
        jne "error" ++
        movq (ind ~ofs:size_int rax) (reg rcx) ++  (* 取得 n *)
        (* 檢查範圍 >= 0 *)
        cmpq (imm 0) (reg rcx) ++
        jl "error" ++
        (* 分配列表空間 *)
        pushq (reg rcx) ++  (* 保存 n *)
        movq (reg rcx) (reg rdi) ++
        imulq (imm size_int) (reg rdi) ++
        addq (imm (2 * size_int)) (reg rdi) ++
        call "malloc" ++
        popq rcx ++        (* 恢復 n *)
        (* 設定列表標頭 *)
        movq (imm tag_list) (ind rax) ++
        movq (reg rcx) (ind ~ofs:size_int rax) ++
        (* 設定列表元素 *)
        pushq (reg rax) ++  (* 保存列表基址 *)
        xorq (reg rdx) (reg rdx) ++  (* 計數器 = 0 *)
        label "range_loop" ++
        cmpq (reg rcx) (reg rdx) ++
        je "range_end" ++
        (* 建立整數物件 *)
        movq (reg rdx) (reg rdi) ++
        pushq (reg rcx) ++
        pushq (reg rdx) ++
        create_int 0 ++
        movq (reg rdi) (ind ~ofs:size_int rax) ++
        (* 存入列表 *)
        movq (reg rax) (reg rdi) ++  (* 暫存整數物件 *)
        popq rdx ++
        popq rcx ++
        popq rax ++        (* 恢復列表基址 *)
        pushq (reg rax) ++ (* 再次保存列表基址 *)
        (* 計算元素位置並存入 *)
        pushq (reg rcx) ++
        movq (reg rdx) (reg rcx) ++
        imulq (imm size_int) (reg rcx) ++
        addq (imm (2 * size_int)) (reg rcx) ++
        movq (reg rdi) (ind ~index:rcx rax) ++
        popq rcx ++
        incq (reg rdx) ++
        jmp "range_loop" ++
        label "range_end" ++
        popq rax

    | "list", [e] ->
        (* list(range(n)) 前面已經實作過了 *)
        compile_expr e

    | _ -> 
      (* 一般函數呼叫 *)
      let push_args = List.fold_left (fun code e -> 
        code ++
        compile_expr e ++
        pushq (reg rax)
      ) nop (List.rev args) in
      push_args ++
      call fn.fn_name ++
      (* 清理參數 *)
      addq (imm (8 * List.length args)) (reg rsp)
    end
  
  | _ -> failwith "error" end

  and compare_values op =
    let l_true = new_label () in
    let l_false = new_label () in
    let l_end = new_label () in
    
    (* 先檢查兩個值的類型是否相同 *)
    movq (ind rax) (reg rcx) ++  (* 第一個值的類型 *)
    movq (ind r11) (reg rdx) ++ (* 第二個值的類型 *)
    
    (* 對於 == 和 != ，允許不同類型之間比較 *)
    begin match op with
    | Beq | Bneq ->
        cmpq (reg rcx) (reg rdx) ++
        jne l_false  (* 如果類型不同，直接判定不相等 *)
    | _ ->
        cmpq (reg rcx) (reg rdx) ++
        jne "error"  (* 其他比較操作要求類型相同 *)
    end ++
    
    (* 根據類型選擇比較方式 *)
    let l_str = new_label () in
    let l_list = new_label () in
    let l_bool = new_label () in
    
    (* 先檢查是不是布林值 *)
    cmpq (imm tag_bool) (reg rcx) ++
    je l_bool ++
    
    (* 再檢查是不是整數 *)
    cmpq (imm tag_int) (reg rcx) ++
    je "compare_int" ++
    
    (* 再檢查是不是字串 *)
    cmpq (imm tag_string) (reg rcx) ++
    je l_str ++
    
    (* 最後檢查是不是列表 *)
    cmpq (imm tag_list) (reg rcx) ++
    je l_list ++
    
    jmp "error" ++  (* 不支援的類型 *)
    
    (* 布林值比較：轉成整數比較 *)
    label l_bool ++
    movq (ind ~ofs:size_int rax) (reg rsi) ++
    movq (ind ~ofs:size_int r11) (reg rdi) ++
    jmp "compare_int" ++
    
    (* 整數比較 *)
    label "compare_int" ++
    movq (ind ~ofs:size_int rax) (reg rsi) ++
    movq (ind ~ofs:size_int r11) (reg rdi) ++
    cmpq (reg rdi) (reg rsi) ++
    begin match op with
    | Beq -> je l_true
    | Bneq -> jne l_true
    | Blt -> jl l_true
    | Ble -> jle l_true
    | Bgt -> jg l_true
    | Bge -> jge l_true
    | _ -> jmp "error"
    end ++
    jmp l_false ++
    
    (* 字串比較 *)
    label l_str ++
    pushq (reg rax) ++
    pushq (reg r11) ++
    leaq (ind ~ofs:(2 * size_int) rax) (rdi) ++
    leaq (ind ~ofs:(2 * size_int) r11) (rsi) ++
    call "strcmp" ++
    popq r11 ++
    popq rcx ++
    (* strcmp 返回: <0 if s1<s2, 0 if s1=s2, >0 if s1>s2 *)
    begin match op with
    | Beq -> cmpq (imm 0) (reg rax) ++ je l_true
    | Bneq -> cmpq (imm 0) (reg rax) ++ jne l_true
    | Blt -> cmpq (imm 0) (reg rax) ++ jl l_true
    | Ble -> cmpq (imm 0) (reg rax) ++ jle l_true
    | Bgt -> cmpq (imm 0) (reg rax) ++ jg l_true
    | Bge -> cmpq (imm 0) (reg rax) ++ jge l_true
    | _ -> jmp "error"
    end ++
    jmp l_false ++
    
    (* 列表比較 *)
    label l_list ++
    (* 先比較長度 *)
    movq (ind ~ofs:size_int rax) (reg rsi) ++  (* 第一個列表長度 *)
    movq (ind ~ofs:size_int r11) (reg rdi) ++  (* 第二個列表長度 *)
    
    (* 如果長度不同且是等於/不等於比較 *)
    begin match op with
    | Beq | Bneq ->
        cmpq (reg rdi) (reg rsi) ++
        jne l_false
    | Blt | Ble ->
        cmpq (reg rdi) (reg rsi) ++
        jl l_true ++
        jg l_false
    | Bgt | Bge ->
        cmpq (reg rdi) (reg rsi) ++
        jg l_true ++
        jl l_false
    | _ -> jmp "error"
    end ++
    
    (* 到這裡，要麼是長度相同，要麼是需要進一步比較元素 *)
    pushq (reg rax) ++  (* 保存列表指針 *)
    pushq (reg r11) ++
    call "compare_lists" ++  (* 呼叫輔助函數比較列表元素 *)
    popq r11 ++
    popq rcx ++
    
    begin match op with
    | Beq -> cmpq (imm 0) (reg rax) ++ je l_true
    | Bneq -> cmpq (imm 0) (reg rax) ++ jne l_true
    | Blt -> cmpq (imm 0) (reg rax) ++ jl l_true
    | Ble -> cmpq (imm 0) (reg rax) ++ jle l_true
    | Bgt -> cmpq (imm 0) (reg rax) ++ jg l_true
    | Bge -> cmpq (imm 0) (reg rax) ++ jge l_true
    | _ -> jmp "error"
    end ++
    
    (* 產生結果 *)
    label l_false ++
    create_bool false ++
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

(* 需要在程式開頭加入 label compare_lists 的相關代碼 *)
let compare_lists_code = 
  label "compare_lists" ++
  (* 保存 caller 的暫存器 *)
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  pushq (reg r12) ++  (* 保存第一個列表 *)
  pushq (reg r13) ++  (* 保存第二個列表 *)
  pushq (reg r14) ++  (* 當前索引 *)
  pushq (reg r15) ++  (* 列表長度 *)

  (* 取得兩個列表的參數 *)
  movq (reg rdi) (reg r12) ++  (* 第一個列表 *)
  movq (reg rsi) (reg r13) ++  (* 第二個列表 *)

  (* 取得列表長度(兩個列表長度一樣，取任一個) *)
  movq (ind ~ofs:size_int r12) (reg r15) ++
  
  (* 初始化索引 *)
  xorq (reg r14) (reg r14) ++  (* r14 = 0 *)

  (* 比較循環開始 *)
  label "compare_lists_loop" ++
  
  (* 檢查是否已經比較完所有元素 *)
  cmpq (reg r14) (reg r15) ++
  je "compare_lists_equal" ++  (* 如果全部比較完且相等，返回0 *)

  (* 取得兩個列表當前元素的指針 *)
  movq (reg r14) (reg rcx) ++
  imulq (imm size_int) (reg rcx) ++
  addq (imm (2 * size_int)) (reg rcx) ++  (* 跳過 tag 和 length *)

  (* 載入當前元素 *)
  movq (ind ~index:rcx r12) (reg rdi) ++  (* 第一個列表的元素 *)
  movq (ind ~index:rcx r13) (reg rsi) ++  (* 第二個列表的元素 *)

  (* 比較兩個元素 *)
  pushq (reg r14) ++  (* 保存當前狀態 *)
  pushq (reg r15) ++
  pushq (reg r12) ++
  pushq (reg r13) ++

  (* 調用遞迴比較 *)
  movq (reg rdi) (reg rax) ++
  movq (reg rsi) (reg r11) ++
  (* 這裡用到之前實作的 compare_values *)
  movq (imm 0) (reg rax) ++  (* 先假設相等 *)
  
  movq (ind rdi) (reg rcx) ++  (* 取得第一個元素類型 *)
  movq (ind rsi) (reg rdx) ++  (* 取得第二個元素類型 *)
  
  (* 如果類型不同，不相等 *)
  cmpq (reg rcx) (reg rdx) ++
  jne "compare_lists_restore" ++

  (* 根據類型進行比較 *)
  cmpq (imm tag_int) (reg rcx) ++
  je "compare_lists_int" ++
  cmpq (imm tag_string) (reg rcx) ++
  je "compare_lists_string" ++
  cmpq (imm tag_list) (reg rcx) ++
  je "compare_lists_recurse" ++
  (* 其他類型暫不支援 *)
  jmp "compare_lists_restore" ++

  (* 整數比較 *)
  label "compare_lists_int" ++
  movq (ind ~ofs:size_int rdi) (reg rax) ++
  subq (ind ~ofs:size_int rsi) (reg rax) ++
  jmp "compare_lists_restore" ++

  (* 字串比較 *)
  label "compare_lists_string" ++
  pushq (reg rdi) ++
  pushq (reg rsi) ++
  leaq (ind ~ofs:(2 * size_int) rdi) (rdi) ++
  leaq (ind ~ofs:(2 * size_int) rsi) (rsi) ++
  call "strcmp" ++
  popq rsi ++
  popq rdi ++
  jmp "compare_lists_restore" ++

  (* 遞迴比較列表 *)
  label "compare_lists_recurse" ++
  pushq (reg rdi) ++
  pushq (reg rsi) ++
  call "compare_lists" ++
  popq rsi ++
  popq rdi ++

  (* 恢復暫存器並檢查比較結果 *)
  label "compare_lists_restore" ++
  popq r13 ++
  popq r12 ++
  popq r15 ++
  popq r14 ++

  (* 檢查比較結果 *)
  cmpq (imm 0) (reg rax) ++
  jne "compare_lists_end" ++  (* 如果不相等，直接返回結果 *)

  (* 元素相等，繼續比較下一個 *)
  incq (reg r14) ++
  jmp "compare_lists_loop" ++

  (* 所有元素都相等 *)
  label "compare_lists_equal" ++
  xorq (reg rax) (reg rax) ++  (* 返回 0 *)

  (* 結束比較 *)
  label "compare_lists_end" ++
  popq (r15) ++
  popq (r14) ++
  popq (r13) ++
  popq (r12) ++
  movq (reg rbp) (reg rsp) ++
  popq (rbp) ++
  ret

let stack_top n = 
  ind ~ofs:(8 * n) rsp
  
(* 輔助函數：列表元素輸出 *)
let print_element_code =
  label "print_element" ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  
  (* 檢查值的類型並輸出 *)
  movq (ind rdi) (reg rsi) ++  (* 獲取類型標籤 *)
  
  (* 根據類型處理 *)
  cmpq (imm tag_none) (reg rsi) ++
  je "print_element_none" ++
  cmpq (imm tag_bool) (reg rsi) ++
  je "print_element_bool" ++
  cmpq (imm tag_string) (reg rsi) ++
  je "print_element_string" ++
  cmpq (imm tag_list) (reg rsi) ++
  je "print_element_list" ++
  
  (* 默認當作整數處理 *)
  movq (ind ~ofs:size_int rdi) (reg rsi) ++
  movq (ilab "fmt_int") (reg rdi) ++
  movq (imm 0) (reg rax) ++
  call "printf" ++
  jmp "print_element_end" ++
  
  (* None *)
  label "print_element_none" ++
  movq (ilab "str_none") (reg rsi) ++
  movq (ilab "fmt_string") (reg rdi) ++
  movq (imm 0) (reg rax) ++
  call "printf" ++
  jmp "print_element_end" ++
  
  (* Bool *)
  label "print_element_bool" ++
  movq (ind ~ofs:size_int rdi) (reg rdx) ++
  cmpq (imm 0) (reg rdx) ++
  je "print_element_false" ++
  movq (ilab "str_true") (reg rsi) ++
  jmp "print_element_do_bool" ++
  label "print_element_false" ++
  movq (ilab "str_false") (reg rsi) ++
  label "print_element_do_bool" ++
  movq (ilab "fmt_string") (reg rdi) ++
  movq (imm 0) (reg rax) ++
  call "printf" ++
  jmp "print_element_end" ++
  
  (* String *)
  label "print_element_string" ++
  leaq (ind ~ofs:(2 * size_int) rdi) (rsi) ++
  movq (ilab "fmt_string") (reg rdi) ++
  movq (imm 0) (reg rax) ++
  call "printf" ++
  jmp "print_element_end" ++
  
  (* List - 遞迴輸出 *)
  label "print_element_list" ++
  call "print_list" ++
  
  label "print_element_end" ++
  movq (reg rbp) (reg rsp) ++
  popq rbp ++
  ret 

let rec compile_stmt = function
(* print 語句：編譯表達式，然後根據型別進行對應的輸出 *)
  | TSprint e ->
    compile_expr e ++  (* 計算表達式的值，結果在 rax *)
    pushq (reg rax) ++ (* 保存輸出值 *)

    (* 檢查類型並跳轉到共用的打印例程 *)
    movq (ind rax) (reg rsi) ++  (* 獲取標籤類型 *)

    cmpq (imm tag_none) (reg rsi) ++
    je "print_none_value" ++
    cmpq (imm tag_bool) (reg rsi) ++
    je "print_bool_value" ++
    cmpq (imm tag_list) (reg rsi) ++
    je "print_list_value" ++
    cmpq (imm tag_int) (reg rsi) ++
    je "print_int_value" ++
    jmp "print_string_value" ++

    (* Print int *)
    label "print_int_value" ++
    popq rax ++                         (* 恢復要打印的值 *)
    pushq (reg rax) ++                  (* 再次保存，因為後面還要用 *)
    movq (ind ~ofs:size_int rax) (reg rsi) ++
    movq (ilab "fmt_int") (reg rdi) ++
    xorq (reg rax) (reg rax) ++         (* printf needs rax = 0 *)
    call "printf" ++
    jmp "print_newline" ++

    (* Print string *)
    label "print_string_value" ++
    leaq (ind ~ofs:(2 * size_int) rax) (rsi) ++
    movq (ilab "fmt_string") (reg rdi) ++
    xorq (reg rax) (reg rax) ++
    call "printf" ++
    jmp "print_newline" ++

    (* Print None *)
    label "print_none_value" ++
    movq (ilab "str_none") (reg rsi) ++
    movq (ilab "fmt_string") (reg rdi) ++
    xorq (reg rax) (reg rax) ++
    call "printf" ++
    jmp "print_newline" ++

    (* Print bool *)
    label "print_bool_value" ++
    movq (ind ~ofs:size_int rax) (reg r11) ++
    cmpq (imm 0) (reg r11) ++
    je "print_false_value" ++
    movq (ilab "str_true") (reg rsi) ++
    jmp "print_bool_str_value" ++
    label "print_false_value" ++
    movq (ilab "str_false") (reg rsi) ++
    label "print_bool_str_value" ++
    movq (ilab "fmt_string") (reg rdi) ++
    xorq (reg rax) (reg rax) ++
    call "printf" ++
    jmp "print_newline" ++

    (* Print list *)
    label "print_list_value" ++
    movq (reg rax) (reg rdi) ++
    call "print_list" ++
    jmp "print_newline" ++

    (* Print newline and cleanup *)
    label "print_newline" ++
    popq rax ++                         (* 恢復原始值 *)
    pushq (reg rax) ++                  (* 保持堆疊平衡 *)
    movq (imm 10) (reg rdi) ++
    call "putchar" ++
    popq rax
  
    (* 變數賦值 *)
  | TSassign (v, e) ->
      compile_expr e ++
      movq (reg rax) (ind ~ofs:v.v_ofs rbp)

  (* if 條件語句 *)
  | TSif (e, s1, s2) ->
      let l_else = new_label () in
      let l_end = new_label () in
      compile_expr e ++
      test_value_false ++
      jne l_else ++
      compile_stmt s1 ++
      jmp l_end ++
      label l_else ++
      compile_stmt s2 ++
      label l_end

  (* 語句塊 *)
  | TSblock sl ->
      List.fold_left (fun code s -> 
        code ++ compile_stmt s
      ) nop sl

  (* for 迴圈 *)
  | TSfor (v, e, s) ->
      let l_start = new_label () in
      let l_end = new_label () in
      (* 編譯列表表達式 *)
      compile_expr e ++
      (* 檢查是否是列表 *)
      movq (ind rax) (reg rcx) ++
      cmpq (imm tag_list) (reg rcx) ++
      jne "error" ++
      (* 保存列表指針和獲取長度 *)
      pushq (reg rax) ++
      movq (ind ~ofs:size_int rax) (reg r12) ++  (* r12 = 長度 *)
      xorq (reg r13) (reg r13) ++                (* r13 = 當前索引 *)
      (* 迴圈開始 *)
      label l_start ++
      (* 檢查是否結束 *)
      cmpq (reg r13) (reg r12) ++
      je l_end ++
      (* 取得當前元素 *)
      movq (reg r13) (reg rcx) ++
      imulq (imm size_int) (reg rcx) ++
      addq (imm (2 * size_int)) (reg rcx) ++
      movq (stack_top 0) (reg rax) ++
      movq (ind ~index:rcx rax) (reg rdx) ++
      (* 賦值給迭代變數 *)
      movq (reg rdx) (ind ~ofs:v.v_ofs rbp) ++
      (* 編譯循環體 *)
      pushq (reg r12) ++
      pushq (reg r13) ++
      compile_stmt s ++
      popq r13 ++
      popq r12 ++
      (* 增加索引並繼續 *)
      incq (reg r13) ++
      jmp l_start ++
      label l_end ++
      addq (imm 8) (reg rsp)  (* 清理堆疊 *)

  (* return 語句 *)
  | TSreturn e ->
      compile_expr e ++
      movq (reg rbp) (reg rsp) ++
      popq rbp ++
      ret

  (* 表達式求值 *)
  | TSeval e ->
      compile_expr e
      
  | _ -> failwith "f"

(* 獨立的函數定義處理函數 *)
let compile_def (fn, body) =
  let setup_params =
    List.fold_left (fun code (i, param) ->
      let param_offset = i * 8 in
      code ++
      movq (ind ~ofs:(16 + param_offset) rbp) (reg rax) ++  (* 16 = ret addr + old rbp *)
      movq (reg rax) (ind ~ofs:(-(8 + param_offset)) rbp)
    ) nop (List.mapi (fun i p -> (i, p)) fn.fn_params) in

  label fn.fn_name ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  (* 為局部變數預留空間 *)
  subq (imm (8 * List.length fn.fn_params)) (reg rsp) ++
  (* 設置參數 *)
  setup_params ++
  (* 編譯函數體 *)
  compile_stmt body ++
  (* 如果沒有明確的return，返回None *)
  create_none ++
  (* 清理堆疊框架並返回 *)
  movq (reg rbp) (reg rsp) ++
  popq rbp ++
  ret

let error_handlers =
  label "error" ++
  movq (ilab "error_msg") (reg rdi) ++
  call "puts" ++
  movq (imm 1) (reg rdi) ++
  call "exit" ++

  label "error_div_by_zero" ++
  movq (ilab "div_zero_msg") (reg rdi) ++
  call "puts" ++
  movq (imm 1) (reg rdi) ++
  call "exit" ++

  label "error_type" ++
  movq (ilab "type_error_msg") (reg rdi) ++
  call "puts" ++
  movq (imm 1) (reg rdi) ++
  call "exit"

let generate_print_routines =
  label "_print_int" ++
  popq rax ++
  pushq (reg rax) ++
  movq (ind ~ofs:size_int rax) (reg rsi) ++
  movq (ilab "fmt_int") (reg rdi) ++
  xorq (reg rax) (reg rax) ++
  call "printf" ++
  jmp "_print_end" ++

  label "_print_string" ++
  leaq (ind ~ofs:(2 * size_int) rax) (rsi) ++
  movq (ilab "fmt_string") (reg rdi) ++
  xorq (reg rax) (reg rax) ++
  call "printf" ++
  jmp "_print_end" ++

  label "_print_none" ++
  movq (ilab "str_none") (reg rsi) ++
  movq (ilab "fmt_string") (reg rdi) ++
  xorq (reg rax) (reg rax) ++
  call "printf" ++
  jmp "_print_end" ++

  label "_print_bool" ++
  movq (ind ~ofs:size_int rax) (reg r11) ++
  cmpq (imm 0) (reg r11) ++
  je "_print_false" ++
  movq (ilab "str_true") (reg rsi) ++
  jmp "_print_bool_str" ++
  label "_print_false" ++
  movq (ilab "str_false") (reg rsi) ++
  label "_print_bool_str" ++
  movq (ilab "fmt_string") (reg rdi) ++
  xorq (reg rax) (reg rax) ++
  call "printf" ++
  jmp "_print_end" ++

  label "_print_list" ++
  movq (reg rax) (reg rdi) ++
  call "print_list" ++
  
  label "_print_end" ++
  popq rax ++
  pushq (reg rax) ++
  movq (imm 10) (reg rdi) ++
  call "putchar" ++
  popq rax

let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  
  (* 初始化 *)
  string_literals := [];
  string_counter := 0;

  (* 生成數據段：包含錯誤訊息和字串常量 *)
  let data_section =
    label "error_msg" ++ string "runtime error" ++
    label "div_zero_msg" ++ string "division by zero" ++
    label "type_error_msg" ++ string "type error" ++
    label "fmt_int" ++ string "%d" ++
    label "fmt_bool" ++ string "%s" ++
    label "fmt_string" ++ string "%s" ++
    label "str_true" ++ string "True" ++
    label "str_false" ++ string "False" ++
    label "str_none" ++ string "None" ++
    label "str_lbracket" ++ string "[" ++
    label "str_rbracket" ++ string "]" ++
    label "str_comma" ++ string ", " in

  (* 運行時錯誤處理程式碼 *)
  let error_code =
    label "error" ++
    movq (ilab "error_msg") (reg rdi) ++
    call "puts" ++
    movq (imm 1) (reg rdi) ++
    call "exit" ++

    label "error_div_by_zero" ++
    movq (ilab "div_zero_msg") (reg rdi) ++
    call "puts" ++
    movq (imm 1) (reg rdi) ++
    call "exit" ++

    label "error_type" ++
    movq (ilab "type_error_msg") (reg rdi) ++
    call "puts" ++
    movq (imm 1) (reg rdi) ++
    call "exit" in

  (* 列表輸出函數 *)
  let print_list_code =
    label "print_list" ++
    pushq (reg rbp) ++
    movq (reg rsp) (reg rbp) ++
    
    (* 印出左中括號 *)
    movq (ilab "str_lbracket") (reg rdi) ++
    call "puts" ++
    
    (* 取得列表長度 *)
    movq (ind ~ofs:size_int rdi) (reg r12) ++  (* r12 = 長度 *)
    xorq (reg r13) (reg r13) ++                (* r13 = 當前索引 *)

    label "print_list_inner_loop" ++
    cmpq (reg r13) (reg r12) ++
    je "print_list_inner_end" ++
    
    (* 印出元素 *)
    movq (reg r13) (reg rcx) ++
    imulq (imm size_int) (reg rcx) ++
    addq (imm (2 * size_int)) (reg rcx) ++
    pushq (reg rdi) ++
    pushq (reg r12) ++
    pushq (reg r13) ++
    movq (ind ~index:rcx rdi) (reg rdi) ++
    call "print_element" ++
    popq r13 ++
    popq r12 ++
    popq rdi ++

    (* 處理逗號 *)
    incq (reg r13) ++
    cmpq (reg r13) (reg r12) ++
    je "print_list_inner_end" ++
    pushq (reg rdi) ++
    movq (ilab "str_comma") (reg rdi) ++
    call "printf" ++
    popq rdi ++
    jmp "print_list_inner_loop" ++

    label "print_list_inner_end" ++
    movq (ilab "str_rbracket") (reg rdi) ++
    call "puts" ++

    leave ++
    ret in

  (* 分割主程式和函數定義 *)
  let (main_fn, main_stmt) = List.hd p in
  let function_defs = List.tl p in

  (* 編譯函數定義 *)
  let functions_code = List.fold_left (fun acc (fn, body) ->
    acc ++ compile_def (fn, body)
  ) nop function_defs in

  (* 生成所有字串常量 *)
  let const_strings = 
    List.fold_left (fun acc (label_name, str) ->
      acc ++ label label_name ++ string str
    ) nop !string_literals in

  { text = 
      (* 外部函數宣告 *)
      inline ".extern printf\n" ++
      inline ".extern puts\n" ++
      inline ".extern strlen\n" ++
      inline ".extern malloc\n" ++
      inline ".extern strcpy\n" ++
      inline ".extern strcmp\n" ++
      inline ".extern strcat\n" ++
      inline ".extern exit\n" ++
      
      (* malloc包裝函數 *)
      inline (malloc_wrapper()) ++

      (* 主程式 *)
      globl "main" ++
      label "main" ++
      pushq (reg rbp) ++
      movq (reg rsp) (reg rbp) ++
      subq (imm 8192) (reg rsp) ++  (* 為區域變數預留空間 *)
      compile_stmt main_stmt ++
      movq (imm 0) (reg rax) ++
      leave ++
      ret ++

      (* 所有函數定義 *)
      functions_code ++

      (* 列表比較函數 *)
      compare_lists_code ++

      (* print_element 函數 *)
      print_element_code ++

      (* 列表輸出函數 *)
      print_list_code ++

      (* 錯誤處理程式碼 *)
      error_code;

    data = 
      data_section ++  (* 固定的字串常量 *)
      const_strings    (* 程式中的字串常量 *)
  }
  