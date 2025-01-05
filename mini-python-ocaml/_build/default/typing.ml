open Ast
open Format

let debug = ref false

exception Error of Ast.location * string

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

let error ?(loc=dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")

module Env = struct
  type scope = {
    vars: (string, var) Hashtbl.t;
    parent: scope option;
  }

  type t = {
    fns: (string, fn) Hashtbl.t;
    current_fn: fn option;
    scope: scope;
    globals: scope;
  }

  let create_scope ?parent () = {
    vars = Hashtbl.create 17;
    parent = parent;
  }

  let empty = 
    let globals = create_scope () in
    {
      fns = Hashtbl.create 17;
      current_fn = None;
      scope = globals;
      globals = globals;
    }

  let rec find_var_in_scope scope name =
    if Hashtbl.mem scope.vars name then
      Some (Hashtbl.find scope.vars name)
    else
      match scope.parent with
      | Some parent -> find_var_in_scope parent name
      | None -> None

  let add_var env v =
    match env.current_fn with
    | None -> 
        Hashtbl.add env.globals.vars v.v_name v
    | Some _ -> 
        Hashtbl.add env.scope.vars v.v_name v

  let find_var env loc x =
    match find_var_in_scope env.scope x with
    | Some v -> v
    | None -> error ~loc "unbound variable %s" x

  let add_fn env f = Hashtbl.add env.fns f.fn_name f
  
  let find_fn env loc x =
    try Hashtbl.find env.fns x
    with Not_found -> error ~loc "undefined function %s" x

  let set_current_fn env f =
    let new_scope = create_scope ~parent:env.globals () in
    { env with 
      current_fn = Some f;
      scope = new_scope;
    }

  let has_fn env name = Hashtbl.mem env.fns name

  let add_builtin env name =
    let fn = {
      fn_name = name;
      fn_params = [{ v_name = "x"; v_ofs = 0 }];
    } in
    add_fn env fn;
    fn
  
  let init_builtins env =
    let _len_fn = add_builtin env "len" in
    let _range_fn = add_builtin env "range" in
    let _list_fn = add_builtin env "list" in
    env
end

let is_builtin = function
  | "len" | "range" | "list" -> true
  | _ -> false

let rec type_expr env = function
  | Ecst c -> TEcst c
  | Eident id -> TEvar (Env.find_var env id.loc id.id)
  | Ebinop (op, e1, e2) ->
      TEbinop(op, type_expr env e1, type_expr env e2)
  | Eunop (op, e1) -> 
      TEunop(op, type_expr env e1)
  | Ecall ({id="len"; loc}, [e1]) ->
    let fn = Env.find_fn env loc "len" in
    TEcall(fn, [type_expr env e1])
  | Ecall ({id="range"; loc}, [e1]) ->
      TErange(type_expr env e1)
  | Ecall ({id="list"; loc}, [e]) ->
      if not (is_list_range e) then
        error ~loc "list() can only be called with range()";
      type_expr env e
  | Ecall ({id; loc}, el) ->
      let fn = Env.find_fn env loc id in
      if List.length el <> List.length fn.fn_params then
        error ~loc "function %s expects %d arguments but got %d" id
          (List.length fn.fn_params) (List.length el);
      TEcall(fn, List.map (type_expr env) el)
  | Elist el ->
      TElist(List.map (type_expr env) el)
  | Eget (e1, e2) ->
      TEget(type_expr env e1, type_expr env e2)

and is_list_range = function
  | Ecall ({id="range"}, _) -> true
  | _ -> false

let rec type_stmt env = function
  | Sif (e, s1, s2) ->
      TSif(type_expr env e, type_stmt env s1, type_stmt env s2)  
  | Sreturn e ->
      TSreturn(type_expr env e)
  | Sassign (id, e) ->
      let v = {
        v_name = id.id;
        v_ofs = 0
      } in
      Env.add_var env v;
      TSassign(v, type_expr env e)
  | Sprint e ->
      TSprint(type_expr env e)
  | Sblock sl ->
      TSblock(List.map (type_stmt env) sl)
  | Sfor (id, e, s) ->
      let v = {v_name = id.id; v_ofs = 0} in
      Env.add_var env v;
      TSfor(v, type_expr env e, type_stmt env s)
  | Seval e ->
      TSeval(type_expr env e)
  | Sset (e1, e2, e3) ->
      TSset(type_expr env e1, type_expr env e2, type_expr env e3)

let type_def env (id, params, body) =
  if is_builtin id.id then
    error ~loc:id.loc "cannot redefine builtin function %s" id.id;

  let rec check_dup = function
    | [] -> ()
    | id :: rest ->
        if List.exists (fun id' -> id.id = id'.id) rest then
          error ~loc:id.loc "duplicate parameter %s" id.id;
        check_dup rest
  in
  check_dup params;

  let fn = {
    fn_name = id.id;
    fn_params = List.map (fun id -> {v_name = id.id; v_ofs = 0}) params;
  } in

  if Env.has_fn env id.id then  
    error ~loc:id.loc "duplicate function %s" id.id;
  Env.add_fn env fn;

  let env = Env.set_current_fn env fn in
  List.iter (fun p -> 
    let v = {v_name = p.id; v_ofs = 0} in
    Env.add_var env v
  ) params;
  
  fn, type_stmt env body

let file ?debug:(b=false) (p: Ast.file) : Ast.tfile =
  debug := b;
  let env = Env.empty in
  let env = Env.init_builtins env in  
  
  let (defs, main) = p in
  let tdefs = List.map (type_def env) defs in
  
  let main_fn = {
    fn_name = "main";
    fn_params = [];
  } in
  let env = Env.set_current_fn env main_fn in
  let tmain = type_stmt env main in
  
  tdefs @ [main_fn, tmain]