open Cil
open Pretty

module GU     = Goblintutil
module CF     = Cilfacade
module ME     = Messages
module VD     = ValueDomain.Compound
module CPA    = 
struct
  include Lattice.StdCousot
  module M = BatMap.IntMap
  module Domain = Basetype.Variables

  type t = [  `Map of VD.t M.t | `Unknown ] 
  
  let name () = "Constant Propagation"
  let top () = `Unknown
  let is_top = function `Unknown -> true | _ -> false
  
  let bot () = `Map M.empty
  let is_bot = function `Map m -> M.is_empty m | _ -> false
  
  let hash = function `Map m -> Hashtbl.hash m | `Unknown -> 100
  let equal x y =
    match x, y with
      | `Unknown, `Unknown -> true
      | `Map x  , `Map y   -> M.equal VD.equal x y
      | _ -> false
  let compare x y =
    match x, y with
      | `Unknown, `Unknown -> 0
      | `Map x  , `Map y   -> M.compare VD.compare x y
      | `Unknown, _        -> 1
      | _                  -> -1
  
  let isSimple = function `Unknown -> true | `Map x -> M.is_empty x
  let short _ _ = "value mapping"
  
  let trace_enabled = Domain.trace_enabled
  
  let fold f m d =
    match m with
      | `Unknown -> raise (MapDomain.Fn_over_All "fold")
      | `Map m -> 
        let f' id = f (GU.IH.find GU.idVar id) in M.fold f' m d
  
  let find x = function
    | `Unknown -> VD.top ()
    | `Map m   -> try M.find x.vid m with Not_found -> VD.bot ()
  
  let remove k = function `Unknown -> `Unknown | `Map m -> `Map (M.remove k.vid m)
  let add k v = function `Unknown -> `Unknown | `Map m -> 
      `Map (try M.modify k.vid (fun _ -> v) m with Not_found -> M.add k.vid v m)
  
  let mem k = function `Unknown -> true | `Map m -> M.mem k.vid m
  
  let filter_class i = function
    | `Unknown -> `Unknown
    | `Map m -> 
        let f k _ = Domain.classify (GU.IH.find GU.idVar k) = i in
        `Map (M.filteri f m)
  
  let add_list xs = function
    | `Unknown -> `Unknown
    | `Map m -> `Map (List.fold_left (fun m (x,y) -> M.add x.vid y m) m xs) 

  let add_list_fun xs f = function
    | `Unknown -> `Unknown
    | `Map m -> `Map (List.fold_left (fun m x -> M.add x.vid (f x) m) m xs) 
  
  let join x y = 
    let joiner _ x y =
      match x, y with
        | Some x, Some y -> Some (VD.join x y)
        | None, x | x, None -> x
    in 
    match x, y with
      | `Unknown, _ | _, `Unknown -> `Unknown
      | `Map x, `Map y -> `Map (M.merge joiner x y)

  let meet x y = 
    let meeter _ x y =
      match x, y with
        | Some x, Some y -> 
            let v = VD.meet x y in
            if VD.is_bot v then None else Some v
        | None, _ | _, None -> None
    in 
    match x, y with
      | `Unknown, x | x, `Unknown -> x
      | `Map x, `Map y -> `Map (M.merge meeter x y)

  let leq x y =
    match x, y with 
      | _, `Unknown -> true
      | `Unknown, _ -> false
      | `Map x, `Map y ->
        let p k y = try VD.leq (M.find k x) y with Not_found -> true in
        M.for_all p y
  
  let pretty_f short () = function
    | `Unknown -> text "CPA.top"
    | `Map mapping ->
      let groups =
        let group_fold key itm gps = 
          let cl = Domain.classify key in
            match gps with
              | (a,n) when cl <>  n -> ((cl,(M.add key.vid itm M.empty))::a, cl)
              | (a,_) -> ((fst (List.hd a),(M.add key.vid itm (snd (List.hd a))))::(List.tl a),cl) in	
          List.rev (fst (fold group_fold (`Map mapping) ([],min_int))) 
      in      
      let f key st dok = 
        if ME.tracing && trace_enabled && !ME.tracevars <> [] && 
          not (List.mem (Domain.short 80 key) !ME.tracevars) then 
            dok
        else
          dok ++ (if VD.isSimple st then dprintf "%a -> %a\n" else 
                    dprintf "%a -> \n  @[%a@]\n") Domain.pretty key VD.pretty st
      in
      let group_name a () = text (Domain.class_name a) in
      let pretty_group  map () = fold f (`Map map) nil in
      let pretty_groups rest map = 
        match (fst map) with
          | 0 ->  rest ++ pretty_group (snd map) ()
          | a -> rest ++ dprintf "@[%t {\n  @[%t@]}@]\n" (group_name a) (pretty_group (snd map)) in 
      let content () = List.fold_left pretty_groups nil groups in
        dprintf "@[%s {\n  @[%t@]}@]" (short 60 (`Map mapping)) content
  
  let toXML_f _ = function
    | `Unknown -> Xml.Element ("Leaf", ["text", "CPA.top"], [])
    | `Map mapping ->
      let esc = Goblintutil.escape in
      let f (key,st) = 
        match Domain.toXML key with
          | Xml.Element ("Loc",attr,[]) ->
              Xml.Element ("Loc", attr, [VD.toXML st])
          | Xml.Element ("Leaf",attr,[]) ->
              let w = Goblintutil.summary_length - 4 in
              let key_str = Domain.short w key in
              let summary = 
                let st_str = VD.short (w - String.length key_str) st in
            esc key_str ^ " -> " ^ esc st_str in

              let attr = [("text", summary);("id",esc key_str)] in begin
                match VD.toXML st with
                  | Xml.Element (_, chattr, children) -> 
                      if VD.isSimple st then Xml.Element ("Leaf", attr, [])
                      else Xml.Element ("Node", attr, children)
                  | x -> x
              end
          | _ -> Xml.Element ("Node", [("text",esc (Domain.short 40 key^" -> "^VD.short 40 st))], [Domain.toXML key; VD.toXML st])
      in
      let module IMap = Map.Make (struct type t = int let compare (x:int) (y:int) = Pervasives.compare x y end) in
      let groups = 
        let add_grpd k v m = 
          let group = Domain.classify k in
          IMap.add group ((k,v) :: try IMap.find group m with Not_found -> []) m
        in
        fold add_grpd (`Map mapping) IMap.empty
      in
      let children = 
          let h g (kvs:(Domain.t * VD.t) list) xs = 
            match g with 
              | -1 when not !Goblintutil.show_temps ->  xs
              | 0 -> List.map f kvs @ xs
              | _ -> (Xml.Element ("Node", [("text", Domain.class_name g);("id",Domain.class_name g)], List.map f kvs))::xs
          in
          IMap.fold h groups []
      in
      let node_attrs = [("text", esc (short Goblintutil.summary_length mapping));("id","map")] in
        Xml.Element ("Node", node_attrs, children)
          
  let pretty = pretty_f short 
  let toXML = toXML_f short 
  
  let pretty_diff () ((m1:t),(m2:t)): Pretty.doc = 
    match m1, m2 with
      | `Unknown, `Unknown -> Pretty.dprintf "Both maps are top."
      | `Map m, `Unknown 
      | `Unknown, `Map m -> Pretty.dprintf "%a instead of top" pretty (`Map m)
      | `Map m1, `Map m2 ->
        let p key value = 
          not (try VD.leq value (find key (`Map m2)) with Not_found -> true)
        in
        let report key v1 v2 =
          Pretty.dprintf "Map: %a =@?@[%a@]"
             Domain.pretty key VD.pretty_diff (v1,v2)
        in
        let diff_key k v = function
          | None   when p k v -> Some (report k v (find k (`Map m2)))
          | Some w when p k v -> Some (w++Pretty.line++report k v (find k (`Map m2)))
          | x -> x
        in 
        match fold diff_key (`Map m1) None with
          | Some w -> w
          | None -> Pretty.dprintf "No binding grew."

  (*include MapDomain.MapBot_LiftTop (Basetype.Variables) (VD)*)
end

let heap_hash = Hashtbl.create 113 

let get_heap_var loc = 
  try Hashtbl.find heap_hash loc
  with Not_found ->
    let name = "(alloc@" ^ loc.Cil.file ^ ":" ^ string_of_int loc.Cil.line ^ ")" in
    let newvar = GU.makeGlobal name Cil.voidType in
      Hashtbl.add heap_hash loc newvar;
      newvar
      
module Glob = 
struct
  module Var = Basetype.Variables
  module Val = VD
end

module Dom (Flag: ConcDomain.S) = Lattice.Prod(CPA)(Flag)
