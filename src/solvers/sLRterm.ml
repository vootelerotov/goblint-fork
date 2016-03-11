open Prelude
open Analyses
open Constraints
open Messages
open SLR

(** the terminating SLR3 box solver *)
module SLR3term =
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
  struct

    include Generic.SolverStats (S)
    module VS = Set.Make (S.Var)

    module P =
    struct
      type t = S.Var.t * S.Var.t
      let equal (x1,x2) (y1,y2) = S.Var.equal x1 y1 && S.Var.equal x2 y2
      let hash  (x1,x2)         = (S.Var.hash x1 - 800) * S.Var.hash x2
    end

    module HPM = Hashtbl.Make (P)

    let narrow = narrow S.Dom.narrow

    let solve box st vs =
      let key    = HM.create 10 in
      let module H = Heap.Make (struct
          type t = S.Var.t
          let compare x y = compare (HM.find key x) (HM.find key y)
        end)
      in
      let extract_min q =
        let x = H.find_min !q in
        q := H.del_min !q; x
      in
      let min_key q =
        let x = H.find_min !q in
        HM.find key x
      in
      let wpoint = HM.create  10 in
      let infl   = HM.create  10 in
      let set    = HM.create  10 in
      let rho    = HM.create  10 in
      let rho'   = HPM.create 10 in
      let q      = ref H.empty in
      let count  = ref 0 in
      let count_side  = ref (max_int - 1) in

      let init ?(side=false) x =
        if not (HM.mem rho x) then begin
          new_var_event x;
          HM.replace rho  x (S.Dom.bot ());
          HM.replace infl x (VS.add x VS.empty);
          if side then (
            print_endline @@ "Variable by side-effect " ^ S.Var.var_id x ^ " ("^ string_of_int (S.Var.line_nr x) ^") to " ^ string_of_int !count_side;
            HM.replace key  x !count_side; decr count_side
            (* HM.replace key  x !count; decr count *)
          ) else (
            print_endline @@ "Variable " ^ S.Var.var_id x ^ " ("^ string_of_int (S.Var.line_nr x) ^") to " ^ string_of_int !count;
            HM.replace key  x !count; decr count
          )
        end
      in
      let sides x =
        let w = try HM.find set x with Not_found -> VS.empty in
        let v = Enum.fold (fun d z -> try S.Dom.join d (HPM.find rho' (z,x)) with Not_found -> d) (S.Dom.bot ()) (VS.enum w)
        in trace "sol" "SIDES: Var: %a\nVal: %a\n" S.Var.pretty_trace x S.Dom.pretty v; v
      in
      let rec iterate b_old prio =
        if H.size !q = 0 || min_key q > prio then ()
        else
          let n = min_key q in
          let x = extract_min q in
          let b_new = do_var b_old x in
          if b_old <> b_new && n < prio then (
            iterate b_new n;
            iterate b_old prio
          ) else
            iterate b_new prio
      and solve ?(side=false) x =
        if not (HM.mem rho x) then begin
          init ~side:side x;
          let b_new = do_var false x in
          iterate b_new (HM.find key x)
        end
      and do_var b_old x =
        let wpx = HM.mem wpoint x in
        (* let wpx = true in *)
        HM.remove wpoint x;
        let old = HM.find rho x in
        let eval y =
          get_var_event y;
          solve y;
          if HM.find key x <= HM.find key y then begin
            HM.replace wpoint y ()
          end;
          HM.replace infl y (VS.add x (try HM.find infl y with Not_found -> VS.empty));
          HM.find rho y
        in
        let effects = ref Set.empty in
        let side y d =
          assert (not (S.Dom.is_bot d));
          (*
          init ~side:true y;
          HM.replace set y (VS.add x (try HM.find set y with Not_found -> VS.empty));
          let old = try HPM.find rho' (x,y) with _ -> S.Dom.bot () in
          let newd = S.Dom.join old d in
          HPM.replace rho' (x,y) newd;
          if not (HM.mem rho y) then
            solve ~side:true y
          else
          if not (S.Dom.equal old newd) then (
            HM.replace wpoint y ();
            q := H.add y !q
          )
          *)
          (* if S.Dom.is_bot d then print_endline "BOT" else *)
          trace "sol" "SIDE: Var: %a\nVal: %a\n" S.Var.pretty_trace y S.Dom.pretty d;
          let first = not (Set.mem y !effects) in
          effects := Set.add y !effects;
          if first then (
            HPM.replace rho' (x,y) d;
            HM.replace set y (VS.add x (try HM.find set y with Not_found -> VS.empty));
            if not (HM.mem rho y) then (
              init ~side:true y;
              ignore @@ do_var false y
              (* solve ~side:true y *)
            ) else (
              (* trace "sol" "SIDE: Var: %a already exists with Prio: %i and Val: %a\n" S.Var.pretty_trace y (HM.find key y) S.Dom.pretty d; *)
              if HM.find key y < 0 then HM.replace key y (Ref.post_decr count_side)
            );
            q := H.add y !q
          ) else (
            let old = HPM.find rho' (x,y) in
            let newd = S.Dom.join old d in
            HPM.replace rho' (x,y) newd;
            if not (S.Dom.equal old newd) then (
              q := H.add y !q
            )
          );
          HM.replace wpoint y ()
        in
        let tmp = eq x eval side in
        let tmp = S.Dom.join tmp (sides x) in
        let val_new, b_new =
          if wpx then
            if S.Dom.leq tmp old then (
              let nar = narrow old tmp in
              trace "sol" "NARROW1: Var: %a\nOld: %a\nNew: %a\nNarrow: %a" S.Var.pretty_trace x S.Dom.pretty old S.Dom.pretty tmp S.Dom.pretty nar;
              nar, true
            ) else
            if b_old then (
              let nar = narrow old tmp in
              trace "sol" "NARROW2: Var: %a\nOld: %a\nNew: %a\nNarrow: %a" S.Var.pretty_trace x S.Dom.pretty old S.Dom.pretty tmp S.Dom.pretty nar;
              nar, true
            )
            else (
              let wid = S.Dom.widen old (S.Dom.join old tmp) in
              trace "sol" "WIDEN: Var: %a\nOld: %a\nNew: %a\nWiden: %a" S.Var.pretty_trace x S.Dom.pretty old S.Dom.pretty tmp S.Dom.pretty wid;
              wid, false
            )
          else
            tmp, b_old
        in
        if tracing then trace "sol" "Var: %a\n" S.Var.pretty_trace x ;
        if tracing then trace "sol" "Contrib:%a\n" S.Dom.pretty val_new;
        if S.Dom.equal old val_new then ()
        else begin
          update_var_event x old val_new;
          if tracing then trace "sol" "New Value:%a\n\n" S.Dom.pretty val_new;
          HM.replace rho x val_new;
          let w = try HM.find infl x with Not_found -> VS.empty in
          (* let w = if wpx then VS.add x w else w in *)
          q := Enum.fold (fun x y -> H.add y x) !q (VS.enum w);
          HM.replace infl x VS.empty
        end;
        b_new
      and eq x get set =
        eval_rhs_event x;
        match S.system x with
        | None -> S.Dom.bot ()
        | Some f -> f get set
      in

      let set_start (x,d) =
        init ~side:true x;
        HM.replace rho x d;
        HM.replace wpoint x ();
        q := H.add x !q;
        HM.replace set x (VS.add x VS.empty);
        HPM.replace rho' (x,x) d
      in

      start_event ();
      List.iter set_start st;

      List.iter solve vs;
      iterate false max_int;

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
      HM.clear infl  ;
      HM.clear set   ;
      HPM.clear rho' ;

      rho
  end

let _ =
  let module S3t = GlobSolverFromIneqSolver (JoinContr (SLR3term)) in
  Selector.add_solver ("slr3t", (module S3t : GenericGlobSolver)); (* same as S2 but number of W-points may also shrink + terminating? *)
