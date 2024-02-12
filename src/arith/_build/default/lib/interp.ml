open Ds
open Parser_plaf.Ast
open Parser_plaf.Parser
    
(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> int result =
  fun e ->
  match e with
  | Int n      -> return n
  | Add(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n+m)   
  | Sub(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n-m)   
  | Mul(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n*m)   
  | Div(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    if m=0
    then error "Division by zero"
    else return (n/m)
  | Abs(e) ->
    eval_expr e >>= fun n ->
    return (abs n)
  | Min(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (min n m)
  | _ -> failwith "Not implemented yet!"

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : int result =
  e |> parse |> eval_prog

(* Original eval_expr before the other stuff *)
(*
(type 'a result = Error of string | Ok of 'a
let rec old_eval_expr : expr -> int result =
  fun e ->
    match e with 
    | Int n -> Ok n
    | Sub(e1, e2) ->
      (match e1 with
       | Error s -> Error s 
       | Ok m ->
        (match e2 with
         | Error s -> Error s 
         | Ok n -> Ok (m-n)
        )  
      )
)
*)