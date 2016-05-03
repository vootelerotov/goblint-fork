(** How to generate constraints for a solver using specifications described in [Analyses]. *)

open Prelude
open Cil
open MyCFG
open Pretty
open Analyses
open GobConfig

module M = Messages

(** Lifts a [Spec] so that the domain and the context are [Hashcons]d. *)
module HashconsLifter (S:Spec)
  : Spec with module D = Lattice.HConsed (S.D)
          and module G = S.G
          and module C = Printable.HConsed (S.C)
=
struct
  module D = Lattice.HConsed (S.D)
  module G = S.G
  module C = Printable.HConsed (S.C)

  let name = S.name^" hashconsed"

  let init = S.init
  let finalize = S.finalize

  let should_join x y = S.should_join (D.unlift x) (D.unlift y)

  let startstate v = D.lift (S.startstate v)
  let exitstate  v = D.lift (S.exitstate  v)
  let otherstate v = D.lift (S.otherstate v)
  let morphstate v d = D.lift (S.morphstate v (D.unlift d))

  let val_of = D.lift % S.val_of % C.unlift
  let context = C.lift % S.context % D.unlift
  let call_descr f = S.call_descr f % D.unlift

  let conv ctx =
    { ctx with local = D.unlift ctx.local
             ; spawn = (fun v -> ctx.spawn v % D.lift )
             ; split = (fun d e tv -> ctx.split (D.lift d) e tv )
    }

  let sync ctx =
    let d, diff = S.sync (conv ctx) in
    D.lift d, diff

  let query ctx q =
    S.query (conv ctx) q

  let assign ctx lv e =
    D.lift @@ S.assign (conv ctx) lv e

  let branch ctx e tv =
    D.lift @@ S.branch (conv ctx) e tv

  let body ctx f =
    D.lift @@ S.body (conv ctx) f

  let return ctx r f =
    D.lift @@ S.return (conv ctx) r f

  let intrpt ctx =
    D.lift @@ S.intrpt (conv ctx)

  let enter ctx r f args =
    List.map (fun (x,y) -> D.lift x, D.lift y) @@ S.enter (conv ctx) r f args

  let special ctx r f args =
    D.lift @@ S.special (conv ctx) r f args

  let combine ctx r fe f args es =
    D.lift @@ S.combine (conv ctx) r fe f args (D.unlift es)

  let part_access _ _ _ _ = 
    (Access.LSSSet.singleton (Access.LSSet.empty ()), Access.LSSet.empty ())
end


(** Lifts a [Spec] with a special bottom element that represent unreachable code. *)
module LevelSliceLifter (S:Spec)
  : Spec with module D = Lattice.Prod (S.D) (Lattice.Reverse (IntDomain.Lifted))
          and module G = S.G
          and module C = S.C
=
struct
  module D = Lattice.Prod (S.D) (Lattice.Reverse (IntDomain.Lifted))
  module G = S.G
  module C = S.C

  let name = S.name^" level sliced"

  let start_level = ref (`Top)
  let error_level = ref (`Lifted  0L)

  let init () = 
    if get_bool "dbg.slice.on" then
      start_level := `Lifted (Int64.of_int (get_int "dbg.slice.n"));
    S.init ()

  let finalize = S.finalize

  let should_join (x,_) (y,_) = S.should_join x y

  let startstate v = (S.startstate v, !start_level)
  let exitstate  v = (S.exitstate  v, !start_level)
  let otherstate v = (S.otherstate v, !start_level)
  let morphstate v (d,l) = (S.morphstate v d, l)

  let val_of d = (S.val_of d, !error_level)
  let context (d,_) = S.context d
  let call_descr f = S.call_descr f

  let conv ctx =
    { ctx with local = fst ctx.local
             ; spawn = (fun v d -> ctx.spawn v (d, snd ctx.local) )
             ; split = (fun d e tv -> ctx.split (d, snd ctx.local) e tv )
    }

  let lift_fun ctx f g h =
    f @@ h (g (conv ctx))

  let sync ctx =
    let liftpair (x, y) = (x, snd ctx.local), y in
    lift_fun ctx liftpair S.sync identity 

  let enter' ctx r f args =
    let liftmap = List.map (fun (x,y) -> (x, snd ctx.local), (y, snd ctx.local)) in
    lift_fun ctx liftmap S.enter ((|>) args % (|>) f % (|>) r) 

  let lift ctx d = (d, snd ctx.local)

  let query' ctx q    = lift_fun ctx identity   S.query  ((|>) q)            
  let assign ctx lv e = lift_fun ctx (lift ctx) S.assign ((|>) e % (|>) lv)  
  let branch ctx e tv = lift_fun ctx (lift ctx) S.branch ((|>) tv % (|>) e)  
  let body ctx f      = lift_fun ctx (lift ctx) S.body   ((|>) f)            
  let return ctx r f  = lift_fun ctx (lift ctx) S.return ((|>) f % (|>) r)   
  let intrpt ctx      = lift_fun ctx (lift ctx) S.intrpt identity            
  let special ctx r f args        = lift_fun ctx (lift ctx) S.special ((|>) args % (|>) f % (|>) r)       
  let combine' ctx r fe f args es = lift_fun ctx (lift ctx) S.combine (fun p -> p r fe f args (fst es))

  let leq0 = function
    | `Top -> false
    | `Lifted x -> x <= 0L
    | `Bot -> true

  let sub1 = function 
    | `Lifted x -> `Lifted (Int64.sub x 1L)
    | x -> x

  let add1 = function 
    | `Lifted x -> `Lifted (Int64.add x 1L)
    | x -> x

  let enter ctx r f args = 
    let (d,l) = ctx.local in
    if leq0 l then
      [ctx.local, D.bot ()]
    else
      enter' {ctx with local=(d, sub1 l)} r f args

  let combine ctx r fe f args es = 
    let (d,l) = ctx.local in
    let l = add1 l in
    if leq0 l then
      (d, l)
    else
      let d',_ = combine' ctx r fe f args es in
      (d', l)

  let query ctx = function 
    | Queries.EvalFunvar e ->
      let (d,l) = ctx.local in
      if leq0 l then 
        `LvalSet (Queries.LS.empty ())
      else 
        query' ctx (Queries.EvalFunvar e)
    | q -> query' ctx q

  let part_access _ _ _ _ = 
    (Access.LSSSet.singleton (Access.LSSet.empty ()), Access.LSSet.empty ())
end


(** Lifts a [Spec] with a special bottom element that represent unreachable code. *)
module DeadCodeLifter (S:Spec)
  : Spec with module D = Dom (S.D)
          and module G = S.G
          and module C = S.C
=
struct
  module D = Dom (S.D)
  module G = S.G
  module C = S.C

  let name = S.name^" lifted"

  let init = S.init
  let finalize = S.finalize

  let should_join x y =
    match x, y with
    | `Lifted a, `Lifted b -> S.should_join a b
    | _ -> true

  let startstate v = `Lifted (S.startstate v)
  let exitstate  v = `Lifted (S.exitstate  v)
  let otherstate v = `Lifted (S.otherstate v)
  let morphstate v d = try `Lifted (S.morphstate v (D.unlift d)) with Deadcode -> d

  let val_of = D.lift % S.val_of
  let context = S.context % D.unlift
  let call_descr f = S.call_descr f

  let conv ctx =
    { ctx with local = D.unlift ctx.local
             ; spawn = (fun v -> ctx.spawn v % D.lift )
             ; split = (fun d e tv -> ctx.split (D.lift d) e tv )
    }

  let lift_fun ctx f g h b =
    try f @@ h (g (conv ctx))
    with Deadcode -> b

  let sync ctx =
    let liftpair (x,y) = D.lift x, y in
    lift_fun ctx liftpair S.sync identity (`Bot, [])

  let enter ctx r f args =
    let liftmap = List.map (fun (x,y) -> D.lift x, D.lift y) in
    lift_fun ctx liftmap S.enter ((|>) args % (|>) f % (|>) r) []

  let query ctx q     = lift_fun ctx identity S.query  ((|>) q)            `Bot
  let assign ctx lv e = lift_fun ctx D.lift   S.assign ((|>) e % (|>) lv) `Bot
  let branch ctx e tv = lift_fun ctx D.lift   S.branch ((|>) tv % (|>) e) `Bot
  let body ctx f      = lift_fun ctx D.lift   S.body   ((|>) f)            `Bot
  let return ctx r f  = lift_fun ctx D.lift   S.return ((|>) f % (|>) r)  `Bot
  let intrpt ctx      = lift_fun ctx D.lift   S.intrpt identity            `Bot
  let special ctx r f args       = lift_fun ctx D.lift S.special ((|>) args % (|>) f % (|>) r)        `Bot
  let combine ctx r fe f args es = lift_fun ctx D.lift S.combine (fun p -> p r fe f args (D.unlift es)) `Bot

  let part_access _ _ _ _ = 
    (Access.LSSSet.singleton (Access.LSSet.empty ()), Access.LSSet.empty ())
end

module FromBackwardSpec (S:BackwardSpec) (Cfg:CfgForward)
  : GlobConstrSys with module LVar = Analyses.Var
                   and module GVar = Basetype.Variables
                   and module G = S.G
                   and module D = S.D
=
struct
  type lv = MyCFG.node
  type gv = varinfo
  type ld = S.D.t
  type gd = S.G.t
  module LVar = Analyses.Var
  module GVar = Basetype.Variables
  module D = S.D
  module G = S.G

  let tf (u:lv) ((e:(Cil.location * MyCFG.edge) list),(v:MyCFG.node)) get set gget gset = get v

  let system v = List.map (tf v) (Cfg.next v)
end

(** The main point of this file---generating a [GlobConstrSys] from a [Spec]. *)
module FlatFromSpec (S:Spec) (Cfg:CfgBackward)
  : sig
    include GlobConstrSys with module LVar = VarF (S.C)
                           and module GVar = Basetype.Variables
                           and module D = S.D
                           and module G = S.G
    val tf : MyCFG.node * S.C.t -> (Cil.location * MyCFG.edge) list * MyCFG.node -> ((MyCFG.node * S.C.t) -> S.D.t) -> (MyCFG.node * S.C.t -> S.D.t -> unit) -> (Cil.varinfo -> G.t) -> (Cil.varinfo -> G.t -> unit) -> D.t
  end
=
struct
  type lv = MyCFG.node * S.C.t
  type gv = varinfo
  type ld = S.D.t
  type gd = S.G.t
  module LVar = VarF (S.C)
  module GVar = Basetype.Variables
  module D = S.D
  module G = S.G

  let common_ctx pval (getl:lv -> ld) sidel getg sideg : (D.t, G.t) ctx * D.t list ref =
    let r = ref [] in
    if !Messages.worldStopped then raise M.StopTheWorld;
    (* now watch this ... *)
    let rec ctx =
      { ask     = query
      ; local   = pval
      ; global  = getg
      ; presub  = []
      ; postsub = []
      ; spawn   = (fun f d -> let c = S.context d in
                    sidel (FunctionEntry f, c) d;
                    ignore (getl (Function f, c)))
      ; split   = (fun (d:D.t) _ _ -> r := d::!r)
      ; sideg   = sideg
      ; assign = (fun ?name _    -> failwith "Cannot \"assign\" in common context.")
      }
    and query x = S.query ctx x in
    (* ... nice, right! *)
    let pval, diff = S.sync ctx in
    let _ = List.iter (uncurry sideg) diff in
    { ctx with local = pval }, r

  let rec bigsqcup = function
    | []    -> D.bot ()
    | [x]   -> x
    | x::xs -> D.join x (bigsqcup xs)

  let tf_loop getl sidel getg sideg d =
    let ctx, r = common_ctx d getl sidel getg sideg in
    bigsqcup ((S.intrpt ctx)::!r)

  let tf_assign lv e getl sidel getg sideg d =
    let ctx, r = common_ctx d getl sidel getg sideg in
    bigsqcup ((S.assign ctx lv e)::!r)

  let normal_return r fd ctx sideg =
    let spawning_return = S.return ctx r fd in
    let nval, ndiff = S.sync { ctx with local = spawning_return } in
    List.iter (fun (x,y) -> sideg x y) ndiff;
    nval

  let toplevel_kernel_return r fd ctx sideg =
    let st = if fd.svar.vname = MyCFG.dummy_func.svar.vname then ctx.local else S.return ctx r fd in
    let spawning_return = S.return {ctx with local = st} None MyCFG.dummy_func in
    let nval, ndiff = S.sync { ctx with local = spawning_return } in
    List.iter (fun (x,y) -> sideg x y) ndiff;
    nval

  let tf_ret ret fd getl sidel getg sideg d =
    let ctx, r = common_ctx d getl sidel getg sideg in
    let d =
      if (fd.svar.vid = MyCFG.dummy_func.svar.vid ||
          List.mem fd.svar.vname (List.map Json.string (get_list "mainfun"))) &&
         (get_bool "kernel" || get_string "ana.osek.oil" <> "")
      then toplevel_kernel_return ret fd ctx sideg
      else normal_return ret fd ctx sideg
    in
    bigsqcup (d::!r)

  let tf_entry fd getl sidel getg sideg d =
    let ctx, r = common_ctx d getl sidel getg sideg in
    bigsqcup ((S.body ctx fd)::!r)

  let tf_test e tv getl sidel getg sideg d =
    let ctx, r = common_ctx d getl sidel getg sideg in
    bigsqcup ((S.branch ctx e tv)::!r)

  let tf_normal_call ctx lv e f args  getl sidel getg sideg =
    let combine (cd, fd) = S.combine {ctx with local = cd} lv e f args fd in
    let paths = S.enter ctx lv f args in
    let _     = if not (get_bool "exp.full-context") then List.iter (fun (c,v) -> sidel (FunctionEntry f, S.context v) v) paths in
    let paths = List.map (fun (c,v) -> (c, getl (Function f, S.context v))) paths in
    let paths = List.filter (fun (c,v) -> D.is_bot v = false) paths in
    let paths = List.map combine paths in
    List.fold_left D.join (D.bot ()) paths

  let tf_special_call ctx lv f args = S.special ctx lv f args

  let tf_proc lv e args getl sidel getg sideg d =
    let ctx, r = common_ctx d getl sidel getg sideg in
    let functions =
      match ctx.ask (Queries.EvalFunvar e) with
      | `LvalSet ls -> Queries.LS.fold (fun ((x,_)) xs -> x::xs) ls []
      | `Bot -> []
      | _ -> Messages.bailwith ("ProcCall: Failed to evaluate function expression "^(sprint 80 (d_exp () e)))
    in
    let one_function f =
      let has_dec = try ignore (Cilfacade.getdec f); true with Not_found -> false in
      if has_dec && not (LibraryFunctions.use_special f.vname)
      then tf_normal_call ctx lv e f args getl sidel getg sideg
      else tf_special_call ctx lv f args
    in
    if [] = functions then
      d (* because LevelSliceLifter *)
    else
      let funs = List.map one_function functions in
      bigsqcup (funs @ !r)

  let tf getl sidel getg sideg edge d =
    begin match edge with
      | Assign (lv,rv) -> tf_assign lv rv
      | Proc (r,f,ars) -> tf_proc r f ars
      | Entry f        -> tf_entry f
      | Ret (r,fd)     -> tf_ret r fd
      | Test (p,b)     -> tf_test p b
      | ASM _          -> fun _ _ _ _ d -> ignore (M.warn "ASM statement ignored."); d
      | Skip           -> fun _ _ _ _ d -> d
      | SelfLoop       -> tf_loop
    end getl sidel getg sideg d

  let tf getl sidel getg sideg (_,edge) d (f,t) =
    let old_loc  = !Tracing.current_loc in
    let old_loc2 = !Tracing.next_loc in
    let _       = Tracing.current_loc := f in
    let _       = Tracing.next_loc := t in
    let d       = tf getl sidel getg sideg edge d in
    let _       = Tracing.current_loc := old_loc in
    let _       = Tracing.next_loc := old_loc2 in
    d

  let tf (v,c) (edges, u) getl sidel getg sideg =
    let pval = getl (u,c) in
    let _, locs = List.fold_right (fun (f,e) (t,xs) -> f, (f,t)::xs) edges (getLoc v,[]) in
    List.fold_left2 (|>) pval (List.map (tf getl sidel getg sideg) edges) locs

  let tf (v,c) (e,u) getl sidel getg sideg =
    let old_node = !current_node in
    let _       = current_node := Some u in
    let d       = try tf (v,c) (e,u) getl sidel getg sideg
      with M.StopTheWorld -> D.bot ()
         | M.Bailure s -> Messages.warn_each s; (getl (u,c))  in
    let _       = current_node := old_node in
    d

  let system (v,c) =
    match v with
    | FunctionEntry _ when get_bool "exp.full-context" ->
      [fun _ _ _ _ -> S.val_of c]
    | _ -> List.map (tf (v,c)) (Cfg.prev v)
end



(** Generating a [GlobConstrSys] from a [Spec]. *)
module NestedFromSpec (Solver:GenericIneqBoxSolver) (S:Spec) (Cfg:CfgBackward)
  : sig
    include GlobConstrSys with module LVar = VarF (S.C)
                           and module GVar = Basetype.Variables
                           and module D = S.D
                           and module G = S.G
  end
=
struct
  module LVar = VarF (S.C)
  module GVar = Basetype.Variables
  module D = S.D
  module G = S.G

  type lv = LVar.t
  type gv = GVar.t
  type ld = S.D.t
  type gd = S.G.t

  module InnerSystemFromSpec
  =
  struct
    let get_l : (LVar.t -> S.D.t         ) ref = ref (fun _   -> failwith "get_l")
    let set_l : (LVar.t -> S.D.t -> unit ) ref = ref (fun _ _ -> failwith "set_l")
    let get_g : (varinfo -> S.G.t        ) ref = ref (fun _   -> failwith "get_g")
    let set_g : (varinfo -> S.G.t -> unit) ref = ref (fun _ _ -> failwith "set_g")

    module Var = Var
    module Dom = S.D

    type v = Var.t
    type d = Dom.t

    let common_ctx pval (get:v -> d) (side:v -> d -> unit) : (Dom.t, S.G.t) ctx * Dom.t list ref =
      if !Messages.worldStopped then raise M.StopTheWorld;
      let r = ref [] in
      let rec ctx =
        { ask     = query
        ; local   = pval
        ; global  = !get_g
        ; presub  = []
        ; postsub = []
        ; spawn   = (fun f d -> let c = S.context d in
                      !set_l (FunctionEntry f, c) d;
                      ignore (!get_l (Function f, c)))
        ; split   = (fun (d:Dom.t) _ _ -> r := d::!r)
        ; sideg   = !set_g
        ; assign = (fun ?name _    -> failwith "Cannot \"assign\" in common context.")
        }
      and query x = S.query ctx x in
      let pval, diff = S.sync ctx in
      let _ = List.iter (fun (x,y) -> !set_g x y) diff in
      { ctx with local = pval }, r

    let rec bigsqcup = function
      | []    -> D.bot ()
      | [x]   -> x
      | x::xs -> D.join x (bigsqcup xs)

    let tf_loop d (get:v -> d) (side:v -> d -> unit) =
      let ctx, r = common_ctx d get side in
      bigsqcup ((S.intrpt ctx)::!r)

    let tf_assign lv e d (get:v -> d) (side:v -> d -> unit) =
      let ctx, r = common_ctx d get side in
      bigsqcup ((S.assign ctx lv e)::!r)

    let normal_return r fd ctx =
      let spawning_return = S.return ctx r fd in
      let nval, ndiff = S.sync { ctx with local = spawning_return } in
      List.iter (fun (x,y) -> !set_g x y) ndiff;
      nval

    let toplevel_kernel_return r fd ctx =
      let st = if fd.svar.vname = MyCFG.dummy_func.svar.vname then ctx.local else S.return ctx r fd in
      let spawning_return = S.return {ctx with local = st} None MyCFG.dummy_func in
      let nval, ndiff = S.sync { ctx with local = spawning_return } in
      List.iter (fun (x,y) -> !set_g x y) ndiff;
      nval

    let tf_ret ret fd d (get:v -> d) (side:v -> d -> unit) =
      let ctx, r = common_ctx d get side in
      let d =
        if (fd.svar.vid = MyCFG.dummy_func.svar.vid ||
            List.mem fd.svar.vname (List.map Json.string (get_list "mainfun"))) &&
           (get_bool "kernel" || get_string "ana.osek.oil" <> "")
        then toplevel_kernel_return ret fd ctx
        else normal_return ret fd ctx
      in
      bigsqcup (d::!r)

    let tf_entry fd d (get:v -> d) (side:v -> d -> unit) =
      let ctx, r = common_ctx d get side in
      bigsqcup ((S.body ctx fd)::!r)

    let tf_test e tv d (get:v -> d) (side:v -> d -> unit) =
      let ctx, r = common_ctx d get side in
      bigsqcup ((S.branch ctx e tv)::!r)

    let tf_normal_call ctx lv e f args (get:v -> d) (side:v -> d -> unit) =
      let combine (cd, fd) = S.combine {ctx with local = cd} lv e f args fd in
      let paths = S.enter ctx lv f args in
      let _     = if not (get_bool "exp.full-context") then List.iter (fun (c,v) -> !set_l (FunctionEntry f, S.context v) v) paths in
      let paths = List.map (fun (c,v) -> (c, !get_l (Function f, S.context v))) paths in
      let paths = List.filter (fun (c,v) -> Dom.is_bot v = false) paths in
      let paths = List.map combine paths in
      List.fold_left Dom.join (Dom.bot ()) paths

    let tf_special_call ctx lv f args = S.special ctx lv f args

    let tf_proc lv e args d (get:v -> d) (side:v -> d -> unit) =
      let ctx, r = common_ctx d get side in
      let functions =
        match ctx.ask (Queries.EvalFunvar e) with
        | `LvalSet ls -> Queries.LS.fold (fun ((x,_)) xs -> x::xs) ls []
        | `Bot -> []
        | _ -> Messages.bailwith ("ProcCall: Failed to evaluate function expression "^(sprint 80 (d_exp () e)))
      in
      let one_function f =
        let has_dec = try ignore (Cilfacade.getdec f); true with Not_found -> false in
        if has_dec && not (LibraryFunctions.use_special f.vname)
        then tf_normal_call ctx lv e f args get side
        else tf_special_call ctx lv f args
      in
      let funs = List.map one_function functions in
      bigsqcup (funs @ !r)

    let tf get side edge d =
      begin match edge with
        | Assign (lv,rv) -> tf_assign lv rv
        | Proc (r,f,ars) -> tf_proc r f ars
        | Entry f        -> tf_entry f
        | Ret (r,fd)     -> tf_ret r fd
        | Test (p,b)     -> tf_test p b
        | ASM _          -> fun d _ _ -> ignore (warn "ASM statement ignored."); d
        | Skip           -> fun d _ _ -> d
        | SelfLoop       -> tf_loop
      end d get side

    let tf get side (_,edge) d (f,t) =
      let old_loc  = !Tracing.current_loc in
      let old_loc2 = !Tracing.next_loc in
      let _       = Tracing.current_loc := f in
      let _       = Tracing.next_loc := t in
      let d       = tf get side edge d in
      let _       = Tracing.current_loc := old_loc in
      let _       = Tracing.next_loc := old_loc2 in
      d

    let tf v (edges, u) get side =
      let pval = get u in
      let _, locs = List.fold_right (fun (f,e) (t,xs) -> f, (f,t)::xs) edges (getLoc v,[]) in
      List.fold_left2 (|>) pval (List.map (tf get side) edges) locs

    let tf (v:v) (e,u) get side =
      let old_node = !current_node in
      let _       = current_node := Some u in
      let d       = try tf v (e,u) get side
        with M.StopTheWorld -> Dom.bot ()
           | M.Bailure s -> Messages.warn_each s; (get u)  in
      let _       = current_node := old_node in
      d

    let system (v:v) = List.map (tf v) (Cfg.prev v)

    let box _ = Dom.join
  end

  module VarHash = BatHashtbl.Make (Var)
  module Solver' = Solver (InnerSystemFromSpec) (VarHash)


  let tf (v, c) getl sidel getg sideg =
    let d = getl (FunctionEntry v,c) in
    let t2 = !InnerSystemFromSpec.get_l in
    let t3 = !InnerSystemFromSpec.set_l in
    let t4 = !InnerSystemFromSpec.get_g in
    let t5 = !InnerSystemFromSpec.set_g in
    InnerSystemFromSpec.get_l         := getl;
    InnerSystemFromSpec.set_l         := sidel;
    InnerSystemFromSpec.get_g         := getg;
    InnerSystemFromSpec.set_g         := sideg;
    let ret_p = MyCFG.Function v in
    let sta_p = MyCFG.FunctionEntry v in
    (* let _ = Pretty.printf "solving function '%s' with starting state:\n%a\n" v.vname S.D.pretty d in  *)
    let ht = Solver'.solve InnerSystemFromSpec.box [sta_p,d] [ret_p] in
    InnerSystemFromSpec.get_l         := t2;
    InnerSystemFromSpec.set_l         := t3;
    InnerSystemFromSpec.get_g         := t4;
    InnerSystemFromSpec.set_g         := t5;
    (* let _ = Pretty.printf "solving function '%s' done\n\n" v.vname in  *)
    try VarHash.find ht ret_p
    with Not_found ->
      (* ignore (Pretty.printf "Searching for '%a':\n\n" Var.pretty ret_p);
         let f k v = ignore (Pretty.printf "%a = ...\n" Var.pretty k) in
         VarHash.iter f ht; *)
      S.D.bot ()


  let system = function
    | (FunctionEntry v,c) when get_bool "exp.full-context" ->
      [fun _ _ _ _ -> S.val_of c]
    | (Function v,c) -> [tf (v, c)]
    | _ -> []

end


(** [ForwardFromSpec] generates a forward-propagating
    constraint system or a normal constriant system (using [FromSpec]) *)
module ForwardFromSpec (S:Spec) (Cfg:CfgBidir) =
struct
  include FlatFromSpec (S) (Cfg)

  let system (u,c) =
    let tf' (u,c) (e,v) getl sidel getg sideg =
      let d = tf (v,c) (e,u) getl sidel getg sideg in
      sidel (v,c) d; S.D.bot ()
    in
    List.map (tf' (u,c)) (Cfg.next u)
end

(** Depending on "exp.forward", [FromSpec] generates a forward-propagating
    constraint system or a normal constraint system *)
module FromSpec (Solver:GenericIneqBoxSolver) (S:Spec) (Cfg:CfgBidir)
  : sig
    include GlobConstrSys with module LVar = VarF (S.C)
                           and module GVar = Basetype.Variables
                           and module D = S.D
                           and module G = S.G
  end
=
struct
  module LVar = VarF (S.C)
  module GVar = Basetype.Variables
  module D = S.D
  module G = S.G

  module Flat = FlatFromSpec             (S) (Cfg)
  module Forw = ForwardFromSpec          (S) (Cfg)
  module Nest = NestedFromSpec  (Solver) (S) (Cfg)

  let system x =
    if get_bool "exp.nested" then
      Nest.system x
    else if get_bool "exp.forward" then
      Forw.system x
    else
      Flat.system x
end


(** Combined variables so that we can also use the more common [IneqConstrSys], and [EqConstrSys]
    that use only one kind of a variable. *)
module Var2 (LV:VarType) (GV:VarType)
  : VarType
    with type t = [ `L of LV.t  | `G of GV.t ]
=
struct
  type t = [ `L of LV.t  | `G of GV.t ]

  let equal x y =
    match x, y with
    | `L a, `L b -> LV.equal a b
    | `G a, `G b -> GV.equal a b
    | _ -> false

  let hash = function
    | `L a -> LV.hash a
    | `G a -> 113 * GV.hash a

  let compare x y =
    match x, y with
    | `L a, `L b -> LV.compare a b
    | `G a, `G b -> GV.compare a b
    | `L a, _ -> -1 | _ -> 1

  let category = function
    | `L a -> LV.category a
    | `G _ -> -1

  let pretty_trace () = function
    | `L a -> LV.pretty_trace () a
    | `G a -> GV.pretty_trace () a

  let printXml f = function
    | `L a -> LV.printXml f a
    | `G a -> GV.printXml f a

  let var_id = function
    | `L a -> LV.var_id a
    | `G a -> GV.var_id a

  let line_nr = function
    | `L a -> LV.line_nr a
    | `G a -> GV.line_nr a

  let file_name = function
    | `L a -> LV.file_name a
    | `G a -> GV.file_name a

  let node = function
    | `L a -> LV.node a
    | `G a -> GV.node a
end

(** Translate a [GlobConstrSys] into a [IneqConstrSys] *)
module IneqConstrSysFromGlobConstrSys (S:GlobConstrSys)
  : IneqConstrSys with type v = Var2(S.LVar)(S.GVar).t
                   and type d = Lattice.Either(S.G)(S.D).t
                   and module Var = Var2(S.LVar)(S.GVar)
                   and module Dom = Lattice.Either(S.G)(S.D)
=
struct
  module Var = Var2(S.LVar)(S.GVar)
  module Dom =
  struct
    include Lattice.Either(S.G)(S.D)
    let printXml f = function
      | `Left  a -> S.G.printXml f a
      | `Right a -> S.D.printXml f a
  end

  type v = Var.t
  type d = Dom.t

  let box f x y = if Dom.leq y x then Dom.narrow x y else Dom.widen x (Dom.join x y)

  let getR = function
    | `Left x -> x
    | `Right _ -> S.G.bot ()
    | _ -> failwith "IneqConstrSysFromGlobConstrSys broken: Right!"

  let getL = function
    | `Right x -> x
    | `Left _ -> S.D.top ()
    | _ -> failwith "IneqConstrSysFromGlobConstrSys broken: Left!"

  let l, g = (fun x -> `L x), (fun x -> `G x)
  let le, ri = (fun x -> `Right x), (fun x -> `Left x)

  let conv f get set =
    f (getL % get % l) (fun x v -> set (l x) (le v))
      (getR % get % g) (fun x v -> set (g x) (ri v))
    |> le

  let system = function
    | `G _ -> []
    | `L x -> List.map conv (S.system x)
end


(*module GlobSolverFromEqSolverWhat (Sol:GenericEqBoxSolver)
  : GenericGlobSolver
  = functor (S:GlobConstrSys) ->
    functor (LH:Hash.H with type key=S.lv) ->
    functor (GH:Hash.H with type key=S.gv) ->
  struct
  module IneqSys = IneqConstrSysFromGlobConstrSys (S)
  module EqSys = Generic.SimpleSysConverter (IneqSys)

  module VH : Hash.H with type key=IneqSys.v and type 'a t = 'a LH.t * 'a GH.t =
  struct
    type key = IneqSys.Var.t
    type 'a t = ('a LH.t) * ('a GH.t)
    let create n = (LH.create n, GH.create n)
    let clear (l,g) = LH.clear l; GH.clear g
    let copy (l,g) = (LH.copy l, GH.copy g)

    let lift (f:'a LH.t -> S.lv -> 'b) (h:'a GH.t -> S.gv -> 'b)
             (l,g:'a t) : [`L of S.lv | `G of S.gv] -> 'b  = function
      | `L x -> f l x
      | `G x -> h g x

    let add          x = lift LH.add          GH.add          x
    let remove       x = lift LH.remove       GH.remove       x
    let find         x = lift LH.find         GH.find         x
    let find_default x = lift LH.find_default GH.find_default x
    let find_all     x = lift LH.find_all     GH.find_all     x
    let replace      x = lift LH.replace      GH.replace      x
    let mem          x = lift LH.mem          GH.mem          x
    let find_all     x = lift LH.find_all     GH.find_all     x
    let find_all     x = lift LH.find_all     GH.find_all     x
    let find_all     x = lift LH.find_all     GH.find_all     x

    let iter f (l,g) =
      LH.iter (fun k v -> f (`L k) v) l;
      GH.iter (fun k v -> f (`G k) v) g

    let fold f (l,g) x =
      let gx = LH.fold (fun k v a -> f (`L k) v a) l x in
      let rx = GH.fold (fun k v a -> f (`G k) v a) g gx in
      rx

    let length (l,g) = LH.length l + GH.length g
  end

  module Sol' = Sol (EqSys) (VH)

  let getL = function
    | `Left x -> x
    | _ -> undefined ()

  let getR = function
    | `Right x -> x
    | _ -> undefined ()

  let solve ls gs l =
    let vs = List.map (fun (x,v) -> `L x, `Left v) ls @ List.map (fun (x,v) -> `G x, `Right v) gs in
    let l, g = Sol'.solve IneqSys.box vs [] in
    (* one could 'magic' it so no copying would be necessary *)
    let l' = LH.create (LH.length l) in
    let g' = GH.create (GH.length g) in
    LH.iter (fun k v -> LH.add l' k (getL v)) l;
    GH.iter (fun k v -> GH.add g' k (getR v)) g;
    (l', g')
  end*)

(** Transforms a [GenericEqBoxSolver] into a [GenericGlobSolver]. *)
module GlobSolverFromEqSolver (Sol:GenericEqBoxSolver)
  : GenericGlobSolver
  = functor (S:GlobConstrSys) ->
    functor (LH:Hash.H with type key=S.LVar.t) ->
    functor (GH:Hash.H with type key=S.GVar.t) ->
    struct
      let lh_find_default h k d = try LH.find h k with Not_found -> d
      let gh_find_default h k d = try GH.find h k with Not_found -> d

      module IneqSys = IneqConstrSysFromGlobConstrSys (S)
      module EqSys = Generic.NormalSysConverter (IneqSys)

      module VH : Hash.H with type key=EqSys.v = Hashtbl.Make(EqSys.Var)
      module Sol' = Sol (EqSys) (VH)

      let getR = function
        | `Left x -> x
        | `Right _ -> S.G.bot ()
        | _ -> undefined ()

      let getL = function
        | `Right x -> x
        | `Left _ -> S.D.top ()
        | _ -> undefined ()

      let solve ls gs l =
        let vs = List.map (fun (x,v) -> EqSys.conv (`L x), `Right v) ls
                 @ List.map (fun (x,v) -> EqSys.conv (`G x), `Left  v) gs in
        let sv = List.map (fun x -> EqSys.conv (`L x)) l in
        let hm = Sol'.solve EqSys.box vs sv in
        let l' = LH.create 113 in
        let g' = GH.create 113 in
        let split_vars = function
          | (`L x,_) -> fun y -> LH.replace l' x (S.D.join (getL y) (lh_find_default l' x (S.D.bot ())))
          | (`G x,_) -> fun y -> GH.replace g' x (getR y)
        in
        VH.iter split_vars hm;
        (l', g')
    end

(** Transforms a [GenericIneqBoxSolver] into a [GenericGlobSolver]. *)
module GlobSolverFromIneqSolver (Sol:GenericIneqBoxSolver)
  : GenericGlobSolver
  = functor (S:GlobConstrSys) ->
    functor (LH:Hash.H with type key=S.LVar.t) ->
    functor (GH:Hash.H with type key=S.GVar.t) ->
    struct
      let lh_find_default h k d = try LH.find h k with Not_found -> d
      let gh_find_default h k d = try GH.find h k with Not_found -> d

      module IneqSys = IneqConstrSysFromGlobConstrSys (S)

      module VH : Hash.H with type key=IneqSys.v = Hashtbl.Make(IneqSys.Var)
      module Sol' = Sol (IneqSys) (VH)

      let getR = function
        | `Left x -> x
        | `Right _ -> S.G.bot ()
        | _ -> undefined ()

      let getL = function
        | `Right x -> x
        | `Left _ -> S.D.top ()
        | _ -> undefined ()

      let solve ls gs l =
        let vs = List.map (fun (x,v) -> `L x, `Right v) ls
                 @ List.map (fun (x,v) -> `G x, `Left  v) gs in
        let sv = List.map (fun x -> `L x) l in
        let hm = Sol'.solve IneqSys.box vs sv in
        let l' = LH.create 113 in
        let g' = GH.create 113 in
        let split_vars = function
          | `L x -> fun y -> LH.replace l' x (S.D.join (getL y) (lh_find_default l' x (S.D.bot ())))
          | `G x -> fun y -> GH.replace g' x (getR y)
        in
        VH.iter split_vars hm;
        (l', g')
    end

module N = struct let topname = "Top" end
(** Add path sensitivity to a analysis *)
module PathSensitive2 (Spec:Spec)
  : Spec
    with type D.t = SetDomain.Hoare(Spec.D)(N).t
     and module G = Spec.G
     and module C = Spec.C
=
struct
  module D =
  struct
    include SetDomain.Hoare (Spec.D) (N)
    let name () = "PathSensitive (" ^ name () ^ ")"

    let pretty_diff () ((s1:t),(s2:t)): Pretty.doc =
      if leq s1 s2 then dprintf "%s: These are fine!" (name ()) else begin
        try
          let p t = not (mem t s2) in
          let evil = choose (filter p s1) in
          let other = choose s2 in
          (* dprintf "%s has a problem with %a not leq %a because %a" (name ())
             Spec.D.pretty evil Spec.D.pretty other
             Spec.D.pretty_diff (evil,other) *)
          Spec.D.pretty_diff () (evil,other)
        with _ -> failwith @@
          "PathSensitive2: choose failed b/c of empty set!"
          ^", s1: "^string_of_int (cardinal s1)
          ^", s2: "^string_of_int (cardinal s2)
      end

    let printXml f x =
      let print_one x =
        BatPrintf.fprintf f "\n<path>%a</path>" Spec.D.printXml x
      in
      iter print_one x

    (* join elements in the same partition (specified by should_join) *)
    let join_reduce a =
      let rec loop js = function
        | [] -> js
        | x::xs -> let (j,r) = List.fold_left (fun (j,r) x ->
            if Spec.should_join x j then Spec.D.join x j, r else j, x::r
          ) (x,[]) xs in
          loop (j::js) r
      in
      apply_list (loop []) a

    let binop op a b = op a b |> join_reduce

    let join = binop join
    let meet = binop meet
    let widen = binop widen
    let narrow = binop narrow
  end

  module G = Spec.G
  module C = Spec.C

  let name = "PathSensitive2("^Spec.name^")"

  let init = Spec.init
  let finalize = Spec.finalize

  let should_join x y = true

  let otherstate v = D.singleton (Spec.otherstate v)
  let exitstate  v = D.singleton (Spec.exitstate  v)
  let startstate v = D.singleton (Spec.startstate v)
  let morphstate v d = D.map (Spec.morphstate v) d

  let call_descr = Spec.call_descr

  let val_of = D.singleton % Spec.val_of
  let context l =
    if D.cardinal l <> 1 then
      failwith "PathSensitive2.context must be called with a singleton set."
    else
      Spec.context @@ D.choose l

  let conv ctx x =
    let rec ctx' = { ctx with ask   = query
                            ; local = x
                            ; spawn = (fun v -> ctx.spawn v % D.singleton )
                            ; split = (ctx.split % D.singleton) }
    and query x = Spec.query ctx' x in
    ctx'

  let map ctx f g =
    let h x xs =
      try D.add (g (f (conv ctx x))) xs
      with Deadcode -> xs
    in
    let d = D.fold h ctx.local (D.empty ()) in
    if D.is_bot d then raise Deadcode else d

  let assign ctx l e    = map ctx Spec.assign  (fun h -> h l e )
  let body   ctx f      = map ctx Spec.body    (fun h -> h f   )
  let return ctx e f    = map ctx Spec.return  (fun h -> h e f )
  let branch ctx e tv   = map ctx Spec.branch  (fun h -> h e tv)
  let intrpt ctx        = map ctx Spec.intrpt  identity
  let special ctx l f a = map ctx Spec.special (fun h -> h l f a)

  let fold ctx f g h a =
    let k x a =
      try h a @@ g @@ f @@ conv ctx x
      with Deadcode -> a
    in
    let d = D.fold k ctx.local a in
    if D.is_bot d then raise Deadcode else d

  let fold' ctx f g h a =
    let k x a =
      try h a @@ g @@ f @@ conv ctx x
      with Deadcode -> a
    in
    D.fold k ctx.local a

  let sync ctx =
    fold' ctx Spec.sync identity (fun (a,b) (a',b') -> D.add a' a, b'@b) (D.empty (), [])

  let query ctx q =
    fold' ctx Spec.query identity (fun x f -> Queries.Result.meet x (f q)) `Top

  let enter ctx l f a =
    let g xs ys = (List.map (fun (x,y) -> D.singleton x, D.singleton y) ys) @ xs in
    fold' ctx Spec.enter (fun h -> h l f a) g []

  let combine ctx l fe f a d =
    assert (D.cardinal ctx.local = 1);
    let cd = D.choose ctx.local in
    let k x y =
      try D.add (Spec.combine (conv ctx cd) l fe f a x) y
      with Deadcode -> y
    in
    let d = D.fold k d (D.bot ()) in
    if D.is_bot d then raise Deadcode else d

  let part_access _ _ _ _ = 
    (Access.LSSSet.singleton (Access.LSSet.empty ()), Access.LSSet.empty ())
end

module Compare
    (S:Spec)
    (Sys:GlobConstrSys with module LVar = VarF (S.C)
                        and module GVar = Basetype.Variables
                        and module D = S.D
                        and module G = S.G)
    (LH:Hash.H with type key=Sys.LVar.t)
    (GH:Hash.H with type key=Sys.GVar.t)
=
struct
  open S

  module PP = Hashtbl.Make (MyCFG.Node)

  let compare_locals h1 h2 =
    let eq, le, gr, uk = ref 0, ref 0, ref 0, ref 0 in
    let f_eq () = incr eq in
    let f_le () = incr le in
    let f_gr () = incr gr in
    let f_uk () = incr uk in
    let f k v1 =
      if not (PP.mem h2 k) then () else
      let v2 = PP.find h2 k in
      let b1 = D.leq v1 v2 in
      let b2 = D.leq v2 v1 in
      if b1 && b2 then
        f_eq ()
      else if b1 then begin
        if get_bool "solverdiffs" then
          ignore (Pretty.printf "%a @@ %a is more precise using %s:\n%a\n" pretty_node k d_loc (getLoc k) (get_string "solver") D.pretty_diff (v1,v2));
        f_le ()
      end else if b2 then begin
        if get_bool "solverdiffs" then
          ignore (Pretty.printf "%a @@ %a is more precise using %s:\n%a\n" pretty_node k d_loc (getLoc k) (get_string "comparesolver") D.pretty_diff (v1,v2));
        f_gr ()
      end else
        f_uk ()
    in
    PP.iter f h1;
    let k1 = Set.of_enum @@ PP.keys h1 in
    let k2 = Set.of_enum @@ PP.keys h2 in
    let o1 = Set.cardinal @@ Set.diff k1 k2 in
    let o2 = Set.cardinal @@ Set.diff k2 k1 in
    Printf.printf "locals:  eq=%d\t%s=%d[%d]\t%s=%d[%d]\tuk=%d\n" !eq (get_string "solver") !le o1 (get_string "comparesolver") !gr o2 !uk

  let compare_globals g1 g2 =
    let eq, le, gr, uk = ref 0, ref 0, ref 0, ref 0 in
    let f_eq () = incr eq in
    let f_le () = incr le in
    let f_gr () = incr gr in
    let f_uk () = incr uk in
    let f k v1 =
      let v2 = try GH.find g2 k with Not_found -> G.bot () in
      let b1 = G.leq v1 v2 in
      let b2 = G.leq v2 v1 in
      if b1 && b2 then
        f_eq ()
      else if b1 then begin
        if get_bool "solverdiffs" then
          ignore (Pretty.printf "Global %a is more precise using %s:\n%a\n" Sys.GVar.pretty_trace k (get_string "solver") G.pretty_diff (v1,v2));
        f_le ()
      end else if b2 then begin
        if get_bool "solverdiffs" then
          ignore (Pretty.printf "Global %a is more precise using %s:\n%a\n" Sys.GVar.pretty_trace k (get_string "comparesolver") G.pretty_diff (v1,v2));
        f_gr ()
      end else
        f_uk ()
    in
    GH.iter f g1;
    Printf.printf "globals: eq=%d\t%s=%d\t%s=%d\tuk=%d\n" !eq (get_string "solver") !le (get_string "comparesolver") !gr !uk

  let compare (l1,g1) (l2,g2) =
    let one_ctx (n,_) v h =
      PP.replace h n (try D.join v (PP.find h n) with Not_found -> v);
      h
    in
    let h1 = PP.create 113 in
    let h2 = PP.create 113 in
    let _  = LH.fold one_ctx l1 h1 in
    let _  = LH.fold one_ctx l2 h2 in
    compare_locals h1 h2;
    compare_globals g1 g2

end

(** Verify if the hashmap pair is really a (partial) solution. *)
module Verify2
    (S:GlobConstrSys)
    (LH:Hash.H with type key=S.LVar.t)
    (GH:Hash.H with type key=S.GVar.t)
=
struct
  open S

  let verify (sigma:D.t LH.t) (theta:G.t GH.t) =
    Goblintutil.in_verifying_stage := true;
    Goblintutil.verified := Some true;
    let complain_l (v:LVar.t) lhs rhs =
      Goblintutil.verified := Some false;
      ignore (Pretty.printf "Fixpoint not reached at %a (%s:%d)\n  @[Variable:\n%a\nRight-Hand-Side:\n%a\nCalculating one more step changes: %a\n@]"
                LVar.pretty_trace v (LVar.file_name v) (LVar.line_nr v) D.pretty lhs D.pretty rhs D.pretty_diff (rhs,lhs))
    in
    let complain_g v (g:GVar.t) lhs rhs =
      Goblintutil.verified := Some false;
      ignore (Pretty.printf "Unsatisfied constraint for global %a at variable %a\n  @[Variable:\n%a\nRight-Hand-Side:\n%a\n@]"
                GVar.pretty_trace g LVar.pretty_trace v G.pretty lhs G.pretty rhs)
    in
    (* For each variable v which has been assigned value d', would like to check
     * that d' satisfied all constraints. *)
    let verify_var v d' =
      let verify_constraint rhs =
        let sigma' x = try LH.find sigma x with Not_found -> D.bot () in
        let theta' x = try GH.find theta x with Not_found -> G.bot () in
        (* First check that each (global) delta is included in the (global)
         * invariant. *)
        let check_local l lv =
          let lv' = sigma' l in
          if not (D.leq lv lv') then
            complain_l l lv' lv
        in
        let check_glob g gv =
          let gv' = theta' g in
          if not (G.leq gv gv') then
            complain_g v g gv' gv
        in
        let d = rhs sigma' check_local theta' check_glob in
        (* Then we check that the local state satisfies this constraint. *)
        if not (D.leq d d') then
          complain_l v d' d
      in
      let rhs = system v in
      List.iter verify_constraint rhs
    in
    LH.iter verify_var sigma;
    Goblintutil.in_verifying_stage := false
end
(*
(** Use Astree-like abstract interpretation *)
module IterateLikeAstree
    (S:Spec)
    (Cfg:CfgBidir)
    (VH:Hash.H with type key=varinfo)
  =
struct
  open MyCFG
  open S

  type lv = MyCFG.node * S.C.t
  type gv = varinfo
  type ld = S.D.t
  type gd = S.G.t
  module LVar = VarF (S.C)
  module GVar = Basetype.Variables
  module D = S.D
  module G = S.G

  let add_update uh v d =
    let xs = try VH.find uh v with Not_found -> [] in
    VH.replace uh v (d::xs)

  let ginv          : G.t      VH.t = VH.create 100
  let ginv_updates  : G.t list VH.t = VH.create 100
  let spawn         : D.t      VH.t = VH.create 100
  let spawn_updates : D.t list VH.t = VH.create 100

  let print_globs () =
    let print_var v x =
      ignore (Pretty.printf "\t%s = %a\n" v.vname G.pretty x)
    in
    ignore (Pretty.printf "Partial Global invariant:\n");
    VH.iter print_var ginv

  let process_spawn_updates () =
    let dirty = ref false in
    let one_var k vs =
      let od = try VH.find spawn k with Not_found -> D.bot () in
      let nw = List.fold_left D.join od vs in
      if not (D.equal od nw) then begin
        dirty := true;
        VH.replace spawn k nw
      end
    in
    VH.iter one_var spawn_updates;
    VH.clear spawn_updates;
    !dirty

  let process_ginv_updates () =
    let dirty = ref false in
    let one_var k vs =
      let od = try VH.find ginv k with Not_found -> G.bot () in
      let nw = List.fold_left G.join od vs in
      if not (G.equal od nw) then begin
        dirty := true;
        VH.replace ginv k nw
      end
    in
    VH.iter one_var ginv_updates;
    VH.clear ginv_updates;
    !dirty

  let common_ctx pval : (D.t, G.t) ctx =
    if !Messages.worldStopped then raise M.StopTheWorld;
    (* now watch this ... *)
    let rec ctx =
      { ask     = query
      ; local   = pval
      ; global  = (fun x -> try VH.find ginv x with Not_found -> G.bot ())
      ; presub  = []
      ; postsub = []
      ; spawn   = (fun x y ->  if Messages.tracing then ignore (Pretty.printf "spawn '%s' with %a\n" x.vname D.pretty y);
                                add_update spawn_updates x y)
      ; split   = (fun (d:D.t) _ _ -> failwith "split")
      ; sideg   = (fun x y -> if Messages.tracing then ignore (Pretty.printf "side-effect '%s' with %a\n" x.vname G.pretty y);
                               add_update ginv_updates x y)
      ; assign = (fun ?name _    -> failwith "Cannot \"assign\" in common context.")
      }
    and query x = S.query ctx x in
    (* ... nice, right! *)
    let pval, diff = S.sync ctx in
    let _ = List.iter (uncurry (add_update ginv_updates)) diff in
    { ctx with local = pval }


  let tf_loop d =
    let ctx = common_ctx d
    in S.intrpt ctx

  let tf_assign lv e d =
    let ctx = common_ctx d
    in S.assign ctx lv e

  let normal_return r fd ctx sideg =
    let spawning_return = S.return ctx r fd in
    let nval, ndiff = S.sync { ctx with local = spawning_return } in
    List.iter (fun (x,y) -> sideg x y) ndiff;
    nval

  let toplevel_kernel_return r fd ctx sideg =
    let spawning_return = S.return ctx None MyCFG.dummy_func in
    let nval, ndiff = S.sync { ctx with local = spawning_return } in
    List.iter (fun (x,y) -> sideg x y) ndiff;
    nval

  let tf_ret ret fd d =
    let sideg _ = failwith "sideg" in
    let ctx = common_ctx d in
    if (fd.svar.vid = MyCFG.dummy_func.svar.vid ||
        List.mem fd.svar.vname (List.map Json.string (get_list "mainfun"))) &&
       (get_bool "kernel" || get_string "ana.osek.oil" <> "")
    then toplevel_kernel_return ret fd ctx sideg
    else normal_return ret fd ctx sideg

  let tf_body fd d =
    let ctx = common_ctx d
    in S.body ctx fd

  let tf_test e tv d =
    let ctx = common_ctx d
    in S.branch ctx e tv

  let tf' edge d =
    begin match edge with
      | Assign (lv,rv) -> tf_assign lv rv d
      | Proc (r,f,ars) -> failwith "tf'"
      | Entry f        -> tf_body f d
      | Ret (r,fd)     -> tf_ret r fd d
      | Test (p,b)     -> tf_test p b d
      | ASM _          -> ignore (warn "ASM statement ignored."); d
      | Skip           -> d
      | SelfLoop       -> tf_loop d
    end

  let tf' u e d =
    let old_loc = !Tracing.current_loc in
    let _       = Tracing.current_loc := getLoc u in
    let d       = try tf' e d
                  with M.StopTheWorld -> D.bot ()
                     | M.Bailure s -> Messages.warn_each s; d  in
    let _       = Tracing.current_loc := old_loc in
      d

  let get_functions d (lv,exp,args) =
    match S.query (common_ctx d) (Queries.EvalFunvar exp) with
      | `Bot -> []
      | `LvalSet ls -> Queries.LS.fold (fun ((x,_)) xs -> x::xs) ls []
      | _ -> Messages.bailwith ("ProcCall: Failed to evaluate function expression "^(sprint 80 (d_exp () exp)))


  module Nodes = Set.Make (Node)
  module NodeDoms =
  struct
    module M = Map.Make (Node)
    type t = D.t M.t
    let empty = M.empty
    let elements = List.of_enum % M.enum
    let filter = M.filter
    let equal = M.equal D.equal
    let compare = M.compare D.compare
    let iter f = M.iter (curry f)
    let add k v d =
      try
        M.add k (D.join v @@ M.find k d) d
      with Not_found ->
        M.add k v d
    let of_enum = Enum.fold (fun d (k,v) -> add k v d) empty
  end
  module NodeTable = Hashtbl.Make (Node)

  type stack = (varinfo list) * bool * D.t * (D.t list ref)

  let bsf_order : int NodeTable.t = NodeTable.create 111
  let init_bsf v d =
    let q = Queue.create () in
    let rec loop d =
      try
        let v = Queue.take q in
        if not (NodeTable.mem bsf_order v) then begin
          NodeTable.add bsf_order v d;
          List.iter (fun (_,x) -> Queue.add x q) (Cfg.next v)
        end;
        loop (d+1)
      with Queue.Empty -> ()
    in
    Queue.add v q;
    loop d

  let pretty_short_node () = function
    | FunctionEntry f -> Pretty.dprintf "fun%d" f.vid
    | Function f -> Pretty.dprintf "ret%d" f.vid
    | Statement s -> Pretty.dprintf "%d (%d)" s.sid (NodeTable.find bsf_order (Statement s))

  module CM = Hashtbl.Make
    (struct
      type t = Node.t * int
      let equal (n1,p1) (n2,p2) = Node.equal n1 n2 && p1=p2
      let hash (n,p) = Node.hash n * p
    end)

  let reachable_node_cache = CM.create 10

  let reachable_node : int -> Node.t -> Nodes.t = fun p n ->
    let reachability : Nodes.t NodeTable.t = NodeTable.create 111 in
    let rec init_reachability v d =
      let propagate d =
        let d = Nodes.add v d in
        match Cfg.prev v with
          | [(_,n)] -> init_reachability n d
          | xs      -> List.iter (fun (_,n) -> init_reachability n d) xs
      in
        if NodeTable.find bsf_order v <= p then begin
          if not (NodeTable.mem reachability v) then begin
            NodeTable.replace reachability v Nodes.empty;
            propagate Nodes.empty
          end
        end else if NodeTable.mem reachability v then begin
          let old = NodeTable.find reachability v in
          if not (Nodes.equal d old) then begin
            let new_d = Nodes.union old d in
            let _ = NodeTable.replace reachability v new_d in
              propagate new_d
          end
        end else begin
          let _ = NodeTable.add reachability v d in
            propagate d
        end
    in
    try
      CM.find reachable_node_cache (n,p)
    with Not_found ->
      let f = MyCFG.getFun n in
      init_reachability (Function f.svar) Nodes.empty;
      if Messages.tracing then ignore (Pretty.printf "reachable_node %d '%a' =\n" p pretty_short_node n);
      if Messages.tracing then Nodes.iter (fun x -> ignore (Pretty.printf "\t%a\n" pretty_short_node x)) (NodeTable.find reachability n);
      let r = NodeTable.find reachability n in
      CM.add reachable_node_cache (n,p) r;
      r

  let reachability_no_rec : Nodes.t NodeTable.t = NodeTable.create 111
  let rec init_reachability_no_rec v d =
    let propagate d =
      let d = Nodes.add v d in
      match Cfg.prev v with
        | [(_,n)] -> init_reachability_no_rec n d
        | xs      -> List.iter (fun (_,n) -> init_reachability_no_rec n d) xs
    in
    if not (Nodes.mem v d) then begin
      if NodeTable.mem reachability_no_rec v then begin
        let old = NodeTable.find reachability_no_rec v in
        if not (Nodes.equal d old) then begin
          let new_d = Nodes.union old d in
          let _ = NodeTable.replace reachability_no_rec v new_d in
            propagate new_d
        end
      end else begin
        let _ = NodeTable.add reachability_no_rec v d in
          propagate d
      end
    end

  let reachable_none_loop : Node.t -> Nodes.t = fun n ->
    NodeTable.find reachability_no_rec n


  let init_fun f =
    (*init_reachability        (Function f)      Nodes.empty;*)
    init_reachability_no_rec (Function f)      Nodes.empty;
    init_bsf                 (FunctionEntry f) 0

  let compare_bfs_order (x:Node.t) (y:Node.t) = Pervasives.compare (NodeTable.find bsf_order x) (NodeTable.find bsf_order y)

  let rec map_pw_un f = function
    | []    -> []
    | x::zs -> List.map (f x) zs @ map_pw_un f zs

  let min1 cmp xs =
    let rec min x = function
      | [] -> x
      | y::ys -> if cmp x y >= 0 then min y ys else min x ys
    in match xs with [] -> raise Not_found | x::xs -> min x xs

  let min_Nodes (x:Nodes.t) : Node.t =
    min1 compare_bfs_order (Nodes.elements x)

  let choose_next : NodeDoms.t -> Node.t -> ((Node.t * D.t) * (Node.t * D.t) * Node.t * NodeDoms.t)  = fun xs t ->
    let xs = NodeDoms.filter (fun x _ -> Nodes.mem t (reachable_node 0 x)) xs in
    let f x y =
      let xx = Nodes.add (fst x) @@ reachable_none_loop (fst x) in
      let yy = Nodes.add (fst y) @@ reachable_none_loop (fst y) in
      x, y, Nodes.inter xx yy
    in
    let pw = map_pw_un f (NodeDoms.elements xs) in
    let pw = List.map (fun (x,y,z) -> x, y, min_Nodes z) pw in
    let cmp (_,_,x) (_,_,y) = compare_bfs_order x y in
    let x,y,z = min1 cmp pw in
    x, y, z, NodeDoms.filter (fun z _ -> not (Node.equal z (fst x) || Node.equal z (fst y))) xs


  (*let rec solve_loop : stack -> Node.t -> (Node.t * D.t) -> D.t = fun st m (n,d) ->
    ignore (Pretty.printf "solve_loop from '%a' back to '%a'.\n" pretty_short_node n pretty_short_node m);
    let d' = solve_path st n m d in
    if D.equal d d' then d' else solve_loop st m (n,d')*)

  let find_next_split : Node.t -> (Node.t * Nodes.t) option = fun n ->
    let r = ref None in
    let rec look_next n s =
      match Cfg.next n with
        | [_,m] -> r := Some (m,s); look_next m (Nodes.add n s)
        | _ -> ()
    in
    look_next n Nodes.empty; !r

  let rec solve_split : stack -> NodeDoms.t -> Node.t -> D.t = fun st xs m ->
    if Messages.tracing then ignore (Pretty.printf "solve_split:\n");
    NodeDoms.iter (fun (n,_) -> ignore (Pretty.printf "\t%a\n" pretty_short_node n)) xs;

    match NodeDoms.elements xs with
      | [] -> raise Not_found
      | [n,d] -> solve_path st n m d
      | _ ->
        let x,y,m',xs' = choose_next xs m in
        let d1 = solve_path st (fst x) m' (snd x) in
        let d2 = solve_path st (fst y) m' (snd y) in
        solve_split st (NodeDoms.add m' (D.join d1 d2) xs') m

  and solve_path st n m d =
    if Messages.tracing then ignore (Pretty.printf "solve_path from '%a' to '%a'.\n" pretty_short_node n pretty_short_node m);
    (*Printf.printf "%!";
    ignore (input_line stdin);*)
    if Node.equal n m then begin
      if Messages.tracing then ignore (Pretty.printf "solve_path done.\n");
      d
    end else
    match Cfg.next n with
      | [] -> raise Not_found
      | [(e,x)] -> solve_path st x m (tf st x e d)
      | xs ->
        let p = NodeTable.find bsf_order n in
        let loop_head_prop (_,x) =
          let xr = reachable_node p x in
          try Node.equal n (min_Nodes xr)
          with Not_found -> false
        in
        let loops = List.filter loop_head_prop xs in
        if Messages.tracing then begin
          if List.length loops <> 0 then begin
            ignore (Pretty.printf "solve_path found cycles:\n");
            List.iter (fun (_,n) -> ignore (Pretty.printf "\t%a\n" pretty_short_node n)) loops
            end else
            ignore (Pretty.printf "no cycles\n")
        end;
        let rec do_loop d =
          if List.length loops = 0 then d else
          let _ = if Messages.tracing then ignore (Pretty.printf "starting loop with state:%a\n" D.pretty d) in
          let d' = List.fold_left (fun d (e,n') -> D.join d (solve_path st n' n (tf st n' e d))) d loops in
          let _ = if Messages.tracing then ignore (Pretty.printf "new value:%a\n" D.pretty d') in
          if D.equal d d' then d else do_loop (D.widen d d')
        in
        let d' = do_loop d in
        let reaches_wo_loops x = Node.equal x m || Nodes.mem m (reachable_node p x) in
        let cont = List.filter (reaches_wo_loops % snd) xs in
        if Messages.tracing then ignore (Pretty.printf "solve_path done with the loop, continuing:\n");
        List.iter (fun (_,n) -> ignore (Pretty.printf "\t%a\n" pretty_short_node n)) cont;
        if List.length cont = 0 then
          d'
        else
          solve_split st (NodeDoms.of_enum @@ List.enum @@ List.map (fun (e,n) -> n, tf st n e d') cont) m


  and tf st n e d =
    if Messages.tracing then ignore (Pretty.printf "tf '%a'\n" pretty_edge e);
    let one_normal_call st d f r fexp ars =
      let ds = S.enter (common_ctx d) r f ars in
      List.fold_left (fun d (call,entry) -> D.join d @@ S.combine (common_ctx call) r fexp f ars @@ solve_fun st f entry) (D.bot ()) ds
    in
    let one_special_call d lv f args =
      S.special (common_ctx d) lv f args
    in
    match e with
      | Proc (r,Lval (Var f,NoOffset),ars) when f.vname = "print_state" ->
          if Messages.tracing then ignore (Pretty.printf "state at '%a':\n%a\n\n" pretty_short_node n D.pretty d);
          d
      | Proc (r,fexp,ars) ->
          let fs = get_functions d (r,fexp,ars) in
          let one_fun d' f =
            D.join d' @@
            let has_dec = try ignore (Cilfacade.getdec f); true with Not_found -> false in
            if has_dec && not (LibraryFunctions.use_special f.vname)
            then one_normal_call st d f r fexp ars
            else one_special_call d r f ars
          in
            List.fold_left one_fun (D.bot ()) fs
      | _ -> tf' n e d

  and solve_fun : stack -> varinfo -> D.t -> D.t = fun (st,re,ua,cs) f d ->
    if Messages.tracing then ignore (Pretty.printf "solve_fun '%s'.\n" f.vname);
    if not (NodeTable.mem reachability_no_rec (FunctionEntry f)) then begin
      if Messages.tracing then ignore (Pretty.printf "init of '%s'.\n" f.vname);
      init_fun f
    end;
    if re && (f.vid = (List.last st).vid) then begin
      if Messages.tracing then ignore (Pretty.printf "recursive call to '%s' detected:\n%a\n\n" f.vname D.pretty d);
      cs := d :: !cs;
      ua
    end else if List.mem f st then begin
      let rec loop d r =
        let cs = ref [] in
        let r' = solve_path ([f],true,r, cs) (FunctionEntry f) (Function f) d in
        let d' = List.fold_left D.join d !cs in
        if D.equal d d' then begin
          if Messages.tracing then ignore (Pretty.printf "recursive function '%s' stabile:\n%a\n\n" f.vname D.pretty d');
          r'
        end else loop (D.widen d d') r'
      in
        if Messages.tracing then ignore (Pretty.printf "recursive function '%s' detected!\n" f.vname);
        loop d (D.bot ())
    end else
      solve_path (f::st,re,ua,cs) (FunctionEntry f) (Function f) d

  let iterate file sv =
    let iter_one_var (f, d) =
      ignore (solve_fun ([],false,D.bot (),ref []) f d)
    in
    let iter_one_func = function
      | Function f, c -> iter_one_var (f, val_of c)
      | _ -> ()
    in
    let rec loop () =
      List.iter iter_one_func sv;
      ignore (process_spawn_updates ());
      let spanws = VH.fold (fun k v d -> (k,v)::d) spawn [] in
      List.iter iter_one_var spanws ;
      if process_ginv_updates () || process_spawn_updates () then
        loop ()
    in loop (); print_globs ()

end
*)
