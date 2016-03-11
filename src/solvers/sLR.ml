(** The 'slr*' solvers. *)

open Prelude
open Analyses
open Constraints
open Messages

let narrow f = if GobConfig.get_bool "exp.no-narrow" then (fun a b -> a) else f

(** the SLR3 box solver *)
module SLR3 =
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
  struct

    include Generic.SolverStats (S)
    module VS = Set.Make (S.Var)

    module P =
    struct
      type t = S.Var.t * S.Var.t
      let equal (x1,x2) (y1,y2) = S.Var.equal x1 y1 && S.Var.equal x2 y2
      let hash  (x1,x2)         = (S.Var.hash x1 * 13) + S.Var.hash x2
    end

    module HPM = Hashtbl.Make (P)

    let solve box st vs =
      let key    = HM.create 10 in
      let count  = ref 0 in
      let get_key x = try HM.find key x with Not_found -> (let c = !count in HM.replace key  x c; decr count; print_endline ("Variable " ^ S.Var.var_id x ^ " to " ^ string_of_int c); c) in
      let module H = Heap.Make (struct
          type t = S.Var.t
          let compare x y = compare (get_key x) (get_key y)
        end)
      in
      let extract_min q =
        let x = H.find_min !q in
        q := H.del_min !q; x
      in
      let min_key q =
        let x = H.find_min !q in
        get_key x
      in
      let wpoint = HM.create  10 in
      let stable = HM.create  10 in
      let infl   = HM.create  10 in
      let set    = HM.create  10 in
      let rho    = HM.create  10 in
      let rho'   = HPM.create 10 in
      let q      = ref H.empty in

      let add_infl y x = HM.replace infl y (VS.add x (try HM.find infl y with Not_found -> VS.empty)) in
      let add_set x y d = HM.replace set y (VS.add x (try HM.find set y with Not_found -> VS.empty)); HPM.add rho' (x,y) d in
      let make_wpoint x = HM.replace wpoint x () in
      let rec solve x =
        if not (HM.mem stable x) then begin
          let wpx = HM.mem wpoint x in
          HM.remove wpoint x;
          HM.replace stable x ();
          let old = HM.find rho x in
          let tmp = eq x (eval x) (side x) in
          let tmp = S.Dom.join tmp (sides x) in
          if tracing then trace "sol" "Var: %a\n" S.Var.pretty_trace x ;
          if tracing then trace "sol" "Contrib:%a\n" S.Dom.pretty tmp;
          let tmp = if wpx then box x old tmp else tmp in
          if not (S.Dom.equal old tmp) then begin
            update_var_event x old tmp;
            if tracing then trace "sol" "New Value:%a\n\n" S.Dom.pretty tmp;
            HM.replace rho x tmp;
            let w = try HM.find infl x with Not_found -> VS.empty in
            let w = if wpx then VS.add x w else w in
            q := Enum.fold (fun x y -> H.add y x) !q (VS.enum w);
            HM.replace infl x VS.empty;
            Enum.iter (HM.remove stable) (VS.enum w)
          end;
          while (H.size !q <> 0) && (min_key q <= get_key x) do
            solve (extract_min q)
          done;
        end;
        assert (HM.mem stable x)
      and eq x get set =
        eval_rhs_event x;
        match S.system x with
        | None -> S.Dom.bot ()
        | Some f ->
          let effects = ref Set.empty in
          let sidef y d =
            if not (Set.mem y !effects) then (
              HPM.replace rho' (x,y) (S.Dom.bot ());
              effects := Set.add y !effects
            );
            set y d
          in
          f get sidef
      and eval x y =
        get_var_event y;
        if not (HM.mem rho y) then init y;
        if get_key x <= get_key y then make_wpoint y else solve y;
        add_infl y x;
        HM.find rho y
      and sides x =
        let w = try HM.find set x with Not_found -> VS.empty in
        Enum.fold (fun d z -> try S.Dom.join d (HPM.find rho' (z,x)) with Not_found -> d) (S.Dom.bot ()) (VS.enum w)
      and side x y d =
        if not (HM.mem rho y) then begin
          init y;
          add_set x y d;
          solve y
        end else begin
          let old = HPM.find rho' (x,y) in
          if not (S.Dom.equal old d) then begin
            add_set x y (S.Dom.join old d);
            HM.remove stable y;
            make_wpoint y;
            q := H.add y !q
          end
        end
      and init x =
        if not (HM.mem rho x) then begin
          new_var_event x;
          let _ = get_key x in
          HM.replace rho  x (S.Dom.bot ());
          HM.replace infl x (VS.add x VS.empty)
        end
      in

      let set_start (x,d) =
        init x;
        (* HM.replace rho x d; *)
        (* HM.replace set x (VS.add x VS.empty); *)
        (* HPM.add rho' (x,x) d *)
        add_set x x d;
        solve x
      in

      start_event ();
      List.iter set_start st;
      List.iter init vs;
      q := List.fold_left (fun q v -> H.add v q) H.empty vs;

      (* List.iter solve vs; *)
      while (H.size !q <> 0) do
        solve (extract_min q)
      done;

      let reachability xs =
        let reachable = HM.create (HM.length rho) in
        let rec one_var x =
          if not (HM.mem reachable x) then begin
            HM.replace reachable x ();
            match S.system x with
            | None -> ()
            | Some x -> one_constaint x
          end
        and one_constaint f =
          ignore (f (fun x -> one_var x; try HM.find rho x with Not_found -> S.Dom.bot ()) (fun x _ -> one_var x))
        in
        List.iter one_var xs;
        HM.iter (fun x _ -> if not (HM.mem reachable x) then HM.remove rho x) rho
      in
      reachability vs;
      stop_event ();

      HM.clear key   ;
      HM.clear wpoint;
      HM.clear stable;
      HM.clear infl  ;
      HM.clear set   ;
      HPM.clear rho'  ;

      rho

  end

module type Version = sig val ver : int end

(** the box solver *)
module Make =
  functor (V:Version) ->
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
  struct

    let h_find_option h x =
      try Some (HM.find h x)
      with Not_found -> None

    let h_find_default h x d =
      try HM.find h x
      with Not_found -> d

    (** Helper module for values and priorities. *)
    module X =
    struct
      let keys = HM.create 1024
      let vals = HM.create 1024
      let val0 = HM.create 1024
      let last_key = ref 0

      let get_value x = h_find_default vals x (S.Dom.bot ())
      let set_value x d =
        if (V.ver >= 5) && not (HM.mem val0 x) then HM.replace val0 x d;
        HM.replace vals x d

      let get_key x =
        try
          HM.find keys x
        with Not_found ->
          incr Goblintutil.vars;
          last_key := !last_key - 1;
          HM.add keys x !last_key;
          !last_key

      let get_index c =
        try (HM.find keys c, true)
        with Not_found ->
          incr Goblintutil.vars;
          last_key := !last_key - 1;
          HM.add keys c !last_key;
          (!last_key, false)

      let to_list () = vals
    end

    (** Helper module for values of global contributions. *)
    module XY =
    struct
      module P =
      struct
        type t = S.Var.t * S.Var.t
        let equal (x1,x2) (y1,y2) = S.Var.equal x1 y1 && S.Var.equal x2 y2
        let hash (x1,x2) = (S.Var.hash x1 - 800) * S.Var.hash x2
      end
      module HPM = Hashtbl.Make (P)
      let hpm_find_default h x d =
        try HPM.find h x
        with Not_found -> d

      let xy = HPM.create 1024

      let get_value x = hpm_find_default xy x (S.Dom.bot ())
      let set_value = HPM.replace xy
    end

    (** Helper module for priority queues. *)
    module H =
    struct
      module HeapCompare =
      struct
        type t = S.Var.t
        let compare x y = Int.compare (X.get_key x) (X.get_key y)
      end

      include Heap.Make (HeapCompare)
      let from_list xs = List.enum xs |> of_enum
      let is_empty x = size x = 0
      let get_root_key x = find_min x |> X.get_key
      let extract_min h = (find_min h, del_min h)
      let insert h k =
        (* ignore @@ Printf.printf "add %d\n" (X.get_key k); *)
        insert h k
      let extract_min h =
        let (k,h) = extract_min h in
        (* ignore @@ Printf.printf "removing %d\n" (X.get_key k); *)
        (k,h)
    end

    (** Helper module for influence lists. *)
    module L =
    struct
      let add h k v = HM.replace h k (v::h_find_default h k [])
      let sub h k = h_find_default h k []
      let rem_item = HM.remove
    end

    (** Helper module for the stable set and global variable setting deps. *)
    module P =
    struct
      let single x = tap (fun s -> HM.add s x ()) (HM.create 10)
      let rem_item = HM.remove
      let to_list s = HM.fold (fun x y z -> x :: z ) s []
      let has_item = HM.mem
      let rem_item = HM.remove
      let insert m = flip (HM.replace m) ()
    end

    (** Helper module for variable setting deps. *)
    module T =
    struct
      let sub = h_find_option
      let update = HM.replace
      let set    = HM.create 1024
    end

    (** Helper module for the domain. *)
    module D =
    struct
      include S.Dom
      let eq = equal
      let cup = join
      let cap = meet
    end

    let infl   = HM.create 1024
    let wpoint = HM.create 1024
    let restart_mode = HM.create 1024

    let solve box st list =
      let stable = HM.create 1024 in
      let work   = ref H.empty    in

      let _ = List.iter (fun (x,v) -> XY.set_value (x,x) v; T.update T.set x (P.single x)) st in
      let _ = work := H.merge (H.from_list list) !work in

      let eq x get set =
        match S.system x with
        | None -> S.Dom.bot ()
        | Some f ->
          let sides = HM.create 10 in
          let collect_set x v =
            let _ = X.get_key x in (* set priority immediately! *)
            h_find_default sides x (S.Dom.bot ()) |> S.Dom.join v |> HM.replace sides x
          in
          let d = f get collect_set in
          HM.iter set sides;
          d
      in

      let restart x =
        let sk = X.get_key x in
        let rec handle_one x =
          let k = X.get_key x in
          let _ = work := H.insert !work x in
          let _ = P.rem_item stable x in
          if k >= sk then () else
            let _ = X.set_value x (D.bot ()) in
            (* ignore @@ Pretty.printf " also restarting %d: %a\n" k S.Var.pretty_trace x; *)
            (* flush_all (); *)
            let w = L.sub infl x in
            let _ = L.rem_item infl x in
            List.iter handle_one w
        in
        (* ignore @@ Pretty.printf "restarting %d: %a\n" sk S.Var.pretty_trace x; *)
        (* flush_all (); *)
        let w = L.sub infl x in
        let _ = L.rem_item infl x in
        List.iter handle_one w
      in

      let rec eval x =
        let (xi,_) = X.get_index x in
        fun y ->
          let (i,nonfresh) = X.get_index y in
          let _ = if xi <= i then HM.replace wpoint y () in
          let _ = if (V.ver>2) && xi <= i then work := H.insert (!work) y in
          let _ = if nonfresh then () else solve y in
          let _ = L.add infl y x in
          X.get_value y

      and side x y d =
        let yk, ynonfresh = X.get_index y in
        if X.get_key x > yk then begin
          (* ignore @@ Pretty.printf "wrong order: %d > %d\n\n"  (X.get_key x) yk; *)
          ()
        end;

        if (V.ver>1) then HM.replace wpoint y ();

        let _ =
          match T.sub T.set y with
          | None -> T.update T.set y (P.single x)
          | Some p -> P.insert p x
        in

        let old = XY.get_value (x,y) in
        (* ignore @@ Pretty.printf "key: %a -> %a\nold: %a\n\nd: %a\n\n" S.Var.pretty_trace x S.Var.pretty_trace y S.Dom.pretty old S.Dom.pretty d; *)
        let tmp = d in
        (* ignore @@ Pretty.printf "tmp: %a\n\n"  S.Dom.pretty tmp; *)

        if not (D.eq tmp old) then begin
          let _ = XY.set_value (x,y) tmp in

          if ynonfresh then
            let _ = P.rem_item stable y in
            work := H.insert (!work) y
          else
            solve y
        end

      and do_side x a =
        match T.sub T.set x with
        | None -> a
        | Some p ->
          let xs = P.to_list p in
          (* ignore (Pretty.printf "%d var %a\n\n" (List.length list) S.Var.pretty_trace x); *)
          List.fold_left (fun a z -> D.cup a (XY.get_value (z,x))) a xs

      and solve x =
        if not (P.has_item stable x) then begin
          incr Goblintutil.evals;
          let _ = P.insert stable x in
          let old = X.get_value x in

          let tmp = do_side x (eq x (eval x) (side x)) in
          let use_box = (not (V.ver>1)) || HM.mem wpoint x in
          let restart_mode_x = h_find_default restart_mode x (2*GobConfig.get_int "ana.restart_count") in
          let rstrt = use_box && (V.ver>3) && D.leq tmp old && restart_mode_x <> 0 in
          if tracing then trace "sol" "Var: %a\n" S.Var.pretty_trace x ;
          if tracing then trace "sol" "Contrib:%a\n" S.Dom.pretty tmp;
          let tmp = if use_box then box x old tmp else tmp in
          if not (D.eq tmp old) then begin
            if tracing then trace "sol" "New Value:%a\n\n" S.Dom.pretty tmp;
            let _ = X.set_value x tmp in
            if V.ver>3 && restart_mode_x mod 2 = 1 && not (D.leq tmp old) then
              HM.replace restart_mode x (restart_mode_x - 1);

            if rstrt then begin
              if restart_mode_x mod 2 = 0 then
                HM.replace restart_mode x (restart_mode_x - 1);
              restart x
            end else
              let w = L.sub infl x in
              let w = if use_box then x::w else w in
              let _ = L.rem_item infl x in
              let _ = if (V.ver>2) then HM.remove wpoint x in
              let _ = work := List.fold_left H.insert (!work) w in
              List.iter (P.rem_item stable) w;
              loop (X.get_key x)
          end
        end;
        if (V.ver>2) then HM.remove wpoint x

      and loop a =
        if not (H.is_empty (!work)) then begin
          if H.get_root_key (!work) <= a
          then let (x,h) = H.extract_min (!work) in
            let _ = work := h in
            let _ = solve x in
            loop a
        end
      in

      let rec loop () =
        if not (H.is_empty (!work)) then begin
          let (x,h) = H.extract_min (!work) in
          let _ = work := h in
          let _ = solve x in
          loop ()
        end
      in

      let _ = loop () in

      let reachability xs =
        let reachable = HM.create (HM.length X.vals) in
        let rec one_var x =
          if not (HM.mem reachable x) then begin
            HM.replace reachable x ();
            match S.system x with
            | None -> ()
            | Some x -> one_constaint x
          end
        and one_constaint f =
          ignore (f (fun x -> one_var x; h_find_default X.vals x (D.bot ())) (fun x _ -> one_var x))
        in
        List.iter one_var xs;
        HM.iter (fun x _ -> if not (HM.mem reachable x) then HM.remove X.vals x) X.vals
      in
      reachability list;

      X.to_list ()

  end


module type MyGenericEqBoxSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hash.H with type key = S.v) ->
  sig
    val solve : (S.v -> S.d -> S.d -> S.d) -> (S.v*S.d) list -> S.v list -> S.d H.t
    val wpoint : unit H.t
    val infl :  S.v list H.t
    val h_find_default : 'a H.t -> S.v -> 'a -> 'a
    module X :
    sig
      val keys : int H.t
    end
  end

module PrintInfluence =
  functor (Sol:MyGenericEqBoxSolver) ->
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
  struct
    module S1 = Sol (S) (HM)
    let solve box x y =
      let ch = Legacy.open_out "test.dot" in
      let r = S1.solve box x y in
      let f k _ =
        let q = if HM.mem S1.wpoint k then " shape=box style=rounded" else "" in
        let s = Pretty.sprint 80 (S.Var.pretty_trace () k) ^ " " ^ string_of_int (try HM.find S1.X.keys k with Not_found -> 0) in
        ignore (Pretty.fprintf ch "%d [label=\"%s\"%s];\n" (S.Var.hash k) (Goblintutil.escape s) q);
        let f y =
          if try HM.find S1.X.keys k > HM.find S1.X.keys y with Not_found -> false then
            ignore (Pretty.fprintf ch "%d -> %d [arrowhead=box style=dashed];\n" (S.Var.hash k) (S.Var.hash y))
          else
            ignore (Pretty.fprintf ch "%d -> %d ;\n" (S.Var.hash k) (S.Var.hash y))
        in
        List.iter f (try HM.find S1.infl k with Not_found -> [])
      in
      ignore (Pretty.fprintf ch "digraph G {\nedge [arrowhead=vee];\n");
      HM.iter f r;
      ignore (Pretty.fprintf ch "}\n");
      Legacy.close_out_noerr ch;
      r
  end

module JoinContr (Sol: GenericEqBoxSolver) =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
  struct
    include Sol (Generic.SimpleSysConverter (S)) (HM)
  end

module MoreVars (Sol: GenericEqBoxSolver) =
  functor (S:IneqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
  struct
    module HM1   = Hashtbl.Make (Generic.ExtendInt (S.Var))
    module EqSys = Generic.NormalSysConverter (S)
    include Sol (EqSys) (HM1)
    let solve box is iv =
      let box' (k,_) x y = box k x y in
      let is' = List.map (fun (k,v) -> EqSys.conv k, v) is in
      let iv' = List.map (fun k -> EqSys.conv k) iv in
      let r = solve box' is' iv' in
      let r' = HM.create (HM1.length r) in
      HM1.iter (fun (k,_) v -> HM.replace r' k (try S.Dom.join v (HM.find r' k) with Not_found -> v)) r;
      r'
  end

module TwoPhased =
  functor (V:Version) ->
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
  struct
    include Make (V) (S) (HM)
    let narrow = narrow S.Dom.narrow
    let solve box is iv =
      let sd = solve (fun _ x y -> S.Dom.widen x (S.Dom.join x y)) is iv in
      let iv' = HM.fold (fun k _ b -> k::b) sd [] in
      let f v x y =
        (* ignore (Pretty.printf "changed %a\nold:%a\nnew:%a\n\n" S.Var.pretty_trace v S.Dom.pretty x S.Dom.pretty y); *)
        narrow x y
      in
      solve f [] iv'
  end

module JustWiden =
  functor (V:Version) ->
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
  struct
    include Make (V) (S) (HM)
    let solve box is iv =
      solve (fun _ x y -> S.Dom.widen x (S.Dom.join x y)) is iv
  end

let _ =
  let module W1 = GlobSolverFromIneqSolver (JoinContr (JustWiden (struct let ver = 1 end))) in
  let module W2 = GlobSolverFromIneqSolver (JoinContr (JustWiden (struct let ver = 2 end))) in
  let module W3 = GlobSolverFromIneqSolver (JoinContr (JustWiden (struct let ver = 3 end))) in
  Selector.add_solver ("widen1",  (module W1 : GenericGlobSolver));
  Selector.add_solver ("widen2",  (module W2 : GenericGlobSolver));
  Selector.add_solver ("widen3",  (module W3 : GenericGlobSolver));
  let module S2 = GlobSolverFromIneqSolver (JoinContr (TwoPhased (struct let ver = 1 end))) in
  Selector.add_solver ("two",  (module S2 : GenericGlobSolver));
  let module S1 = GlobSolverFromIneqSolver (JoinContr (Make (struct let ver = 1 end))) in
  Selector.add_solver ("new",  (module S1 : GenericGlobSolver));
  Selector.add_solver ("slr+", (module S1 : GenericGlobSolver))

let _ =
  let module S1 = GlobSolverFromIneqSolver (JoinContr (Make (struct let ver = 1 end))) in
  let module S2 = GlobSolverFromIneqSolver (JoinContr (Make (struct let ver = 2 end))) in
  let module S3 = GlobSolverFromIneqSolver (JoinContr (SLR3)) in
  let module S4 = GlobSolverFromIneqSolver (JoinContr (Make (struct let ver = 4 end))) in
  Selector.add_solver ("slr1", (module S1 : GenericGlobSolver)); (* W&N at every program point *)
  Selector.add_solver ("slr2", (module S2 : GenericGlobSolver)); (* W&N dynamic at certain points, growing number of W-points *)
  Selector.add_solver ("slr3", (module S3 : GenericGlobSolver)); (* same as S2 but number of W-points may also shrink *)
  Selector.add_solver ("slr4", (module S4 : GenericGlobSolver)); (* restarting: set influenced variables to bot and start up-iteration instead of narrowing *)
  let module S1p = GlobSolverFromIneqSolver (JoinContr (PrintInfluence (Make (struct let ver = 1 end)))) in
  let module S2p = GlobSolverFromIneqSolver (JoinContr (PrintInfluence (Make (struct let ver = 2 end)))) in
  let module S3p = GlobSolverFromIneqSolver (JoinContr (PrintInfluence (Make (struct let ver = 3 end)))) in
  let module S4p = GlobSolverFromIneqSolver (JoinContr (PrintInfluence (Make (struct let ver = 4 end)))) in
  Selector.add_solver ("slr1p", (module S1p : GenericGlobSolver)); (* same as S1-4 above but with side-effects *)
  Selector.add_solver ("slr2p", (module S2p : GenericGlobSolver));
  Selector.add_solver ("slr3p", (module S3p : GenericGlobSolver));
  Selector.add_solver ("slr4p", (module S4p : GenericGlobSolver));
  let module S1 = GlobSolverFromIneqSolver (MoreVars (Make (struct let ver = 1 end))) in
  let module S2 = GlobSolverFromIneqSolver (MoreVars (Make (struct let ver = 2 end))) in
  let module S3 = GlobSolverFromIneqSolver (MoreVars (Make (struct let ver = 3 end))) in
  let module S4 = GlobSolverFromIneqSolver (MoreVars (Make (struct let ver = 4 end))) in
  Selector.add_solver ("slr1x", (module S1 : GenericGlobSolver)); (* alias for S1-4 *)
  Selector.add_solver ("slr2x", (module S2 : GenericGlobSolver));
  Selector.add_solver ("slr3x", (module S3 : GenericGlobSolver));
  Selector.add_solver ("slr4x", (module S4 : GenericGlobSolver));
  let module S1p = GlobSolverFromIneqSolver (MoreVars (PrintInfluence (Make (struct let ver = 1 end)))) in
  let module S2p = GlobSolverFromIneqSolver (MoreVars (PrintInfluence (Make (struct let ver = 2 end)))) in
  let module S3p = GlobSolverFromIneqSolver (MoreVars (PrintInfluence (Make (struct let ver = 3 end)))) in
  let module S4p = GlobSolverFromIneqSolver (MoreVars (PrintInfluence (Make (struct let ver = 4 end)))) in
  Selector.add_solver ("slr1xp", (module S1p : GenericGlobSolver)); (* alias for S1p-4p *)
  Selector.add_solver ("slr2xp", (module S2p : GenericGlobSolver));
  Selector.add_solver ("slr3xp", (module S3p : GenericGlobSolver));
  Selector.add_solver ("slr4xp", (module S4p : GenericGlobSolver));
