(* ************************************************************************************************************)
(*                                                    ds.ml                                                   *)
(* ************************************************************************************************************)
(* Michael Midthassel & Aiden Boyce --> "We pledge our honor that we have abided by the Stevens Honor System" *)

(* This file defines expressed values and environments *)

(* expressed values and environments are defined mutually recursively *)

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

type exp_val =
  | NumVal of int
  | BoolVal of bool
  | PairVal of exp_val*exp_val
  | TupleVal of exp_val list
  | ListVal of exp_val list
  | TreeVal of exp_val tree
  | RecordVal of (string*exp_val) list
type env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env


(* Environment Abstracted Result *)

type 'a result = Ok of 'a | Error of string

type 'a ea_result = env -> 'a result
  
let return (v:'a) : 'a ea_result =
  fun _env ->
  Ok v

let error (s:string) : 'a ea_result =
  fun _env ->
  Error s

let (>>=) (c:'a ea_result) (f: 'a -> 'b ea_result) : 'b ea_result =
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok v -> f v env

let (>>+) (c:env ea_result) (d:'a ea_result): 'a ea_result =
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok newenv -> d newenv

let run (c:'a ea_result) : 'a result =
  c EmptyEnv

let lookup : env ea_result = fun env ->
  Ok env

let rec sequence : 'a ea_result list -> 'a list ea_result =
  fun l ->
  match l with
  | [] -> return []
  | h::t ->
    h >>= fun ev ->
    sequence t >>= fun evs ->
    return (ev::evs)

      
(* operations on environments *)

let empty_env : unit -> env ea_result = fun () ->
  return EmptyEnv

let extend_env : string -> exp_val -> env ea_result = fun id v env ->
  Ok (ExtendEnv(id,v,env))

let rec extend_env_list_helper =
  fun ids evs en ->
  match ids,evs with
  | [],[] -> en
  | id::idt,ev::evt ->
    ExtendEnv(id,ev,extend_env_list_helper idt evt en)
  | _,_ -> failwith
             "extend_env_list_helper: ids and evs have different sizes"
  
let extend_env_list =
  fun ids evs ->
  fun en ->
  Ok (extend_env_list_helper ids evs en)
    
let rec apply_env : string -> exp_val ea_result = fun id env ->
  match env with
  | EmptyEnv -> Error (id^" not found!")
  | ExtendEnv(v,ev,tail) ->
    if id=v
    then Ok ev
    else apply_env id tail

(* operations on expressed values *)

let int_of_numVal : exp_val -> int ea_result =  function
  |  NumVal n -> return n
  | _ -> error "Expected a number!"

let bool_of_boolVal : exp_val -> bool ea_result =  function
  |  BoolVal b -> return b
  | _ -> error "Expected a boolean!"

let list_of_tupleVal : exp_val -> (exp_val list)  ea_result =  function
  |  TupleVal l -> return l
  | _ -> error "Expected a tuple!"
           
let pair_of_pairVal : exp_val -> (exp_val*exp_val) ea_result =  function
  |  PairVal(ev1,ev2) -> return (ev1,ev2)
  | _ -> error "Expected a pair!"

let tree_of_treeVal : exp_val -> (exp_val tree) ea_result = function
  | TreeVal t -> return t
  | _ -> error "Expected a tree!"

let record_of_recordVal : exp_val -> ((string*exp_val) list) ea_result = function
  | RecordVal r -> return r
  | _ -> error "Expected a record!"
           
(* other string stuff *)
let rec string_of_expval = function
  | NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
  | PairVal (ev1,ev2) -> "PairVal("^string_of_expval ev1
                         ^","^ string_of_expval ev2^")"
  | TupleVal evs -> "TupleVal("^String.concat "," (List.map string_of_expval evs)^")"
  | ListVal _l -> failwith "TODO IMPLEMENT ME"
  | TreeVal _t -> failwith "TODO IMPLEMENT ME"
  | RecordVal _r -> failwith "TODO IMPLEMENT ME"


let rec string_of_env' ac = function
  | EmptyEnv ->  "["^String.concat ",\n" ac^"]"
  | ExtendEnv(id,v,env) -> string_of_env' ((id^":="^string_of_expval v)::ac) env

let string_of_env : string ea_result =
  fun env ->
  match env with
  | EmptyEnv -> Ok ">>Environment:\nEmpty"
  | _ -> Ok (">>Environment:\n"^ string_of_env' [] env)

(* helper functions for hw3 *)
(* checks for duplicate fields *)
let rec dupes list = 
  match list with
  | [] -> false
  | h::t -> 
    if List.mem h t
    then true
    else dupes t
    
(* this helper function sees if the wanted value is in the list *)
let rec find (x:('a * exp_val) list) id = 
  match x with
  | [] -> error "Proj: field does not exist"
  | (a,b)::t -> 
  if a=id
  then return b
  else find t id
