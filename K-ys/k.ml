(*
 * SNU 4190.310 Programming Languages 2017 Fall
 *  K- Interpreter Skeleton Code
 * DongKwon Lee (dklee@ropas.snu.ac.kr)
 *)

(* Location Signature *)
module type LOC =
sig
  type t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC =
struct
  type t = Location of int
  let base = Location(0)
  let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM = 
sig
  type 'a t
  exception Not_allocated
  exception Not_initialized
  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
  val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
  val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
  exception Not_allocated
  exception Not_initialized
  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list
  let empty = M (Loc.base,[])

  let rec replace_nth = fun l n c -> 
    match l with
    | h::t -> if n = 1 then c :: t else h :: (replace_nth t (n - 1) c)
    | [] -> raise Not_allocated

  let load (M (boundary,storage)) loc =
    match (List.nth storage ((Loc.diff boundary loc) - 1)) with
    | V v -> v 
    | U -> raise Not_initialized

  let store (M (boundary,storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary,storage)) = 
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
  exception Error of string
  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp
    
  type program = exp
  type memory
  type env
  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS =
struct
  exception Error of string

  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp

  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
    
  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with
    | Num n -> n
    | _ -> raise (Error "TypeError : not int / val")

  let value_bool v =
    match v with
    | Bool b -> b
    | _ -> raise (Error "TypeError : not bool ")

  let value_unit v =
    match v with
    | Unit -> ()
    | _ -> raise (Error "TypeError : not unit")

  let value_record v =
    match v with
    | Record r -> r
    | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      (match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")) 
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      (match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc") 
      | Proc (id_list, exp, env) -> (id_list, exp, env))
    with Env.Not_bound -> raise (Error "Unbound")

  (*let record_list : (id * Loc.t) list = []*)
  (*let rl_maker ((lst : (id * Loc.t) list),(nd : (id * Loc.t))) : (id * Loc.t) list = nd::lst
  let x_finder (x : id) : Loc.t =
    List.assoc x record_list*)
  let reference_list : (id * Loc.t) list = []
  
  let rec eval mem env e =
    match e with
    | READ x -> 
      let v = Num (read_int()) in
      let l = lookup_env_loc env x in
      (v, Mem.store mem l v)
    | WRITE e ->
      let (v, mem') = eval mem env e in
      let n = value_int v in
      let _ = print_endline (string_of_int n) in
      (v, mem')
    | LETV (x, e1, e2) ->
      let (v, mem') = eval mem env e1 in
      let (l, mem'') = Mem.alloc mem' in
      eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | ASSIGN (x, e) ->
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)
    
    | NUM n ->
      let v = Num n in
      (v, mem)
    | TRUE ->
      let v = Bool true in
      (v, mem)
    | FALSE ->
      let v = Bool false in
      (v, mem)
    | UNIT ->
      (Unit, mem)
    | VAR x ->
      ((Mem.load mem (lookup_env_loc env x)), mem)
    | ADD (e1, e2) ->
      let (n1, mem') = eval mem env e1 in
      let (n2, mem'') = eval mem' env e2 in
      let v = Num ((value_int n1)+(value_int n2)) in
      (v, mem'')
    | SUB (e1, e2) ->
      let (n1, mem') = eval mem env e1 in
      let (n2, mem'') = eval mem' env e2 in
      let v = Num ((value_int n1)-(value_int n2)) in
      (v, mem'')
    | MUL (e1, e2) ->
      let (n1, mem') = eval mem env e1 in
      let (n2, mem'') = eval mem' env e2 in
      let v = Num ((value_int n1)*(value_int n2)) in
      (v, mem'')
    | DIV (e1, e2) ->
      let (n1, mem') = eval mem env e1 in
      let (n2, mem'') = eval mem' env e2 in
      let v = Num ((value_int n1)/(value_int n2)) in
      (v, mem'')
    | RECORD id_exp_list ->
    (
      match id_exp_list with
      | [] -> (Unit, mem)
      | _ ->
      (
        let rec x_v_listmaker (mem, id_exp_list, x_v_list) =
        (
          match id_exp_list with
          | [] -> (mem, x_v_list)
          | (id1,exp1)::ie_list_tail ->
          (
            let (v1, mem1) = eval mem env exp1 in
            x_v_listmaker (mem1, ie_list_tail, (id1,v1)::x_v_list)
          )
        ) in
        let (memfex, x_v_listfex) = x_v_listmaker (mem, id_exp_list, []) in
        
        let rec loc_maker (mem, x_v_list, record_list) =
        (
          match x_v_list with
          | (xhd,vhd)::listtl ->
          (
            let (l1, mem1) = Mem.alloc mem in
            let env1 = Env.bind env xhd (Addr l1) in (* xhd -> l1 *)
            let mem2 = Mem.store mem1 l1 vhd in (* l1 -> v1 *)
            loc_maker (mem2, listtl, (xhd,l1)::record_list)
          )
          | [] -> (mem, record_list)
        ) in
        let (memfl, r_listfl) = loc_maker (memfex, x_v_listfex, []) in
        ((Record (fun id -> List.assoc id r_listfl)), memfl)
      )
    )
    (*
      match exl with
      | [] -> (Unit, mem)
      | exlhd::exltl ->
      (
        match exlhd with
        | (hdid, hdexp) ->
        (
          let (hdv, memhd) = eval mem env hdexp in
          let (hdl, memhd2) = Mem.alloc memhd in
          let envhd = Env.bind env hdid (Addr hdl) in
          let memhd3 = Mem.store memhd2 hdl hdv in
          let newnd = (hdid, hdl) in
          let (v2, mem'') = eval memhd3 envhd (RECORD exltl) in
          ((Record (fun id -> List.assoc id (newnd::record_list))),mem'')
        )
      )
    *)
    | EQUAL (e1, e2) ->
    (
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
        if v1=v2 then (Bool true, mem'') else (Bool false, mem'')
    )
    | LESS (e1, e2) ->
    (
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      match v1 with
      | Num a1 ->
        (
        match v2 with
        | Num a2 ->
          (
            if a1 < a2 then (Bool true, mem'')
            else (Bool false, mem'') 
          )
        | _ -> raise (Error "TypeError : not int / v2")
        )
      | _ -> raise (Error "TypeError : not int / v1")
    )
    (*(
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      match v1 with
      | Num a1 ->
        (
        match v2 with
        | Num a2 ->
          (
            if a1 < a2 then (v1, mem'')
            else if a1 > a2 then (v2, mem'')
            else raise (Error "TypeError : equal") 
          )
        | _ -> raise (Error "TypeError : not int / v2")
        )
      | _ -> raise (Error "TypeError : not int / v1")
    )*)
    | NOT e ->
      (
      let (v, mem') = eval mem env e in
      match v with
      | Bool b -> if value_bool v then ((Bool false), mem') else ((Bool true), mem')
      | _ -> raise (Error "TypeError : not bool / not")
      )
    | SEQ (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      (v2, mem'')
    | IF (e, e1, e2) ->
      let (torf, mem') = eval mem env e in
      (
        match torf with
        (*| Bool true -> (*if (value_bool torf) then (eval mem' env e1) else (eval mem' env e2)*)*)
        | Bool true -> eval mem' env e1
        | Bool false -> eval mem' env e2
        | Num n -> raise (Error "TypeError : not bool / if num")
        | Unit -> raise (Error "TypeError : not bool / if unit")
        | Record r -> raise (Error "TypeError : not bool / if record")
      )
    | WHILE (e1, e2) ->
      let (torf, mem') = eval mem env e1 in
      let (v1, mem1) = eval mem' env e2 in
      (
        match torf with
        | Bool b -> if value_bool torf then (eval mem1 env (WHILE(e1,e2))) else (Unit , mem')
        | _ -> raise (Error "TypeError : not bool / while")
      )
    | LETF (f, xl, e1, e2) ->
      let env' = Env.bind env f (Proc (xl, e1, env)) in
      eval mem env' e2
    | CALLV (f,exp_list) ->
    (
      let rec vlist_maker mem exp_list v_list =
      (
        match exp_list with
        | [] -> (mem, v_list)
        | exp1::exptl ->
        (
          let (v1, mem1) = eval mem env exp1 in
          vlist_maker mem1 exptl (v1::v_list) 
        )
      ) in
      let (memfex, v_listfex) = vlist_maker mem exp_list [] in
      let (x_list, expf, envf) = lookup_env_proc env f in (* find id(x) list *)

      let rec insert_value mem env v_list x_list =
      (
        match (v_list , x_list) with
        |(v1::vltl, x1::xltl) ->
        (
          let (l1, mem1) = Mem.alloc mem in
          let mem2 = Mem.store mem1 l1 v1 in (* l1 -> v1 *)
          let env1 = Env.bind env x1 (Addr l1) in (* x1 -> l1 *)
          insert_value mem2 env1 vltl xltl
        )
        | _ -> (mem, env)
      ) in

      let (memfi, envfi) = insert_value memfex envf (List.rev v_listfex) x_list in
      let env' = Env.bind envfi f (Proc (x_list, expf, envfi)) in
      eval memfi env' expf
    )
    | CALLR (f, y_list) ->
    (
      let (x_list, exp1, env1) = lookup_env_proc env f in
      let rec insert_loc x_list y_list env env1= 
      (
        match (x_list, y_list) with
        | (x1::xltl, y1::yltl) -> insert_loc xltl yltl env (Env.bind env1 x1 (Addr (lookup_env_loc env y1)))
        | _ -> env1
      ) in
      let envf = insert_loc x_list y_list env env1 in
      
      eval mem (Env.bind envf f (Proc(x_list, exp1, env1))) exp1
    )
    | ASSIGNF (e1, x, e2) -> 
    (
      let (r,mem1) = eval mem env e1 in
      let (v,mem2) = eval mem1 env e2 in
      let record = value_record r in
      (v, (Mem.store mem2 (record x) v))
    )
    | FIELD (exp, x) -> (* RECLOOKUP *)
    (
      let (r, mem1) = eval mem env exp in
      let record = value_record r in
      let v = Mem.load mem1 (record x) in
      (v, mem1)
    )



  let run (mem, env, pgm) = 
    let (v, _ ) = eval mem env pgm in
    v
end
