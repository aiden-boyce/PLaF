(* ************************************************************************************************************)
(*                                                   interp.ml                                                *)
(* ************************************************************************************************************)
(* Michael Midthassel & Aiden Boyce --> "We pledge our honor that we have abided by the Stevens Honor System" *)
open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds
    
(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Int(n) ->
    return (NumVal n)
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Let(id,def,body) ->
    eval_expr def >>= 
    extend_env id >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
    return (PairVal(ev1,ev2))
  | Fst(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun (l,_) ->
    return l
  | Snd(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun (_,r) ->
    return r
  | Unpair(id1, id2, e1, e2) -> 
    eval_expr e1 >>= 
    pair_of_pairVal >>= fun (evl, evr) ->
    extend_env id1 evl >>+ 
    extend_env id2 evr >>+
    eval_expr e2 
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"

  (* hw3 part 1 *)
  | IsEmpty(e) -> 
    eval_expr e >>= tree_of_treeVal >>= fun t ->
    (match t with
    | Empty -> return @@ (BoolVal true)
    | _ -> return (BoolVal false))

  | EmptyTree(_t) -> return @@ TreeVal Empty

  | Node(e1, e2, e3) -> 
    eval_expr e1  >>= fun n ->
    eval_expr e2  >>= 
    tree_of_treeVal >>= fun l -> 
    eval_expr e3  >>= 
    tree_of_treeVal >>= fun r-> 
    return (TreeVal (Node(n, l, r)))

  | CaseT(e1, e2, id1, id2, id3, e3) -> 
    eval_expr e1 >>=
    tree_of_treeVal >>= fun t ->
    (match t with
    (* handle empty case *)
    | Empty ->  
      eval_expr e2 >>= fun empty -> return @@  empty
    (* handle node case *)
    | Node(a,b,c) -> 
      (* extend environment by binding node value (a) to id1, left subtree (b) to id2, and the right (c) to id3*)
      extend_env id1 a  >>+ extend_env id2 (TreeVal b)  >>+ extend_env id3 (TreeVal c)  >>+
      eval_expr e3 >>= fun node -> return @@  node)

  (* hw3 part 2 *)
  | Record(fs) -> 
  (* 'fs' is a list of (string, (bool, expr)) tuples *)
  let field_names, field_values = List.split fs in
  let field_exprs = List.map snd field_values in
  (* evaluate all field expressions *)
  eval_exprs field_exprs >>= fun eval_fieldVals ->
  (* check for dupes *)
  if dupes field_names
  then error "Record: duplicate fields"
  else
  (* combine evaluated field values with their corresponding field names *)
  return (RecordVal (List.combine field_names eval_fieldVals))

  | Proj(e, id) -> 
    eval_expr e >>= 
    record_of_recordVal >>= fun record ->
    find record id

  | _ -> failwith "implement"
and
  eval_exprs : expr list -> (exp_val list) ea_result = 
  fun es ->
  match es with 
  | [] -> return []
  | h::t -> eval_expr h >>= fun i -> 
    eval_exprs t >>= fun l ->
    return (i::l)
  
(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e


(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
  


