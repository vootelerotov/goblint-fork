open Solver

module Algorithm
  (N: NodeType) 
  (L: Lattice.S) =
struct
  module CAL = Hashtbl.Make (Basetype.Variables)
  module VAR = Prod (N) (L)
  module SOL = Hashtbl.Make (Prod (N) (L))
  module WS  = Set.Make (VAR)
  
  type solution = L.t SOL.t (* (N,L) -> L *)
  
  let dbg f = () (*ignore (f ())*)
  
  exception Finished

  (* r -- root node of a proc 
     e -- exit node of a proc 
     succ --- successor node of a node
     start --- start proc, context, and value list
     p1 --- main proc root node
     c1 --- main context
     l0 --- start value of a proc
     f --- abstract effect of an edge
     enter -- node and start state pairs of node and prev state
     comb -- state of proc, pre call state, and fun return state
  *)
  let solve r e succ start f enter comb is_special =    
    (* 1. Initialize WORK := {(r_1,0)}, --- extended with a list of interesting values *)
    let work_N = ref WS.empty in
    let work_C = ref (List.fold_left (fun w (p,c,_) -> WS.add (r p,c) w) WS.empty start) in
    let work_E = ref WS.empty in
    (* PHI(r_1,0) := 0 --- extended with a list of interesting values *)
    let phi  = SOL.create 255 in
    List.iter (fun (p,c,l) -> SOL.add phi (r p, c) l) start;
    (* *** *)
    let calls = CAL.create 255 in

    (*  -- if it is not in the map, then it is bot *)
    let find_bot v =  if SOL.mem phi v then SOL.find phi v else L.bot () in
    
    let add_work (n,x) =
      match N.kind n with 
        | `ProcCall ->
            work_C := WS.add (n,x) !work_C
        | `ExitOfProc _ ->  
            work_E := WS.add (n,x) !work_E
        | _ ->  
            work_N := WS.add (n,x) !work_N       
    in
    let choose_from work =
      let (n,x) = WS.choose !work in
      work := WS.remove (n,x) !work;
      (n,x)
    in    
    let choose () =
       if not (WS.is_empty !work_N) 
       then choose_from work_N
       else if not (WS.is_empty !work_E)  
       then choose_from work_E
       else if not (WS.is_empty !work_C) 
       then choose_from work_C
       else raise Finished
    in
    
    (* helper function propagate*)
    let propagate x z m = 
      dbg (fun () -> Pretty.printf "PROPAGATE (\n%a\n,\n\n%a) %a\n" L.pretty x L.pretty z N.pretty_trace m);
      (* propagate (x,z) to m [ By this we mean: assign PHI(m,x) := PHI(m,x) /\ z where undefined PHI(m,x) is 
        interpreted as \Omega; if the value has changed, then add (m,x) to WORK]*)
      let phi_m_x = find_bot (m,x) in
      if not (L.leq z phi_m_x) then begin
        add_work (m,x); 
        SOL.replace phi (m,x) (L.join phi_m_x z)
      end
    in
    
    let add_var old n x = 
      let st = L.join x old in
(*      ignore (Pretty.printf "spawning %a\nwith state %a\n" N.pretty_trace n L.pretty st); *)
      propagate st st n   
    in

    (* 2. While WORK != {} *)
    try while true do
      (* remove an element (n,x) from WORK, *)
      let (n,x) = choose () in
      dbg (fun () -> Pretty.printf "PICK (%a,?)\n\n" N.pretty_trace n (*L.pretty x*));
      
      (* and let y = PHI(n,x) *)
      let y = SOL.find phi (n,x) in  
      (* (a) If n is a call block in a procedure q, calling procedure p then *)
        match N.kind n with
          | `ProcCall when not (is_special n y) ->
            let ps = enter (add_var y) n y in (* -- for dynamic calls & entry *)
            let one_call (p, y') =
              dbg (fun () -> Pretty.printf "CALL TO (%s,?)\n" p.Cil.vname (*L.pretty y'x*));
              CAL.replace calls p (WS.add (n,x) (try CAL.find calls p with Not_found -> WS.empty));
              (* If z = PHI(e_p,y) is defined, *)
              if SOL.mem phi (e p,y') then
                let z = SOL.find phi (e p, y') in
                (* let m be the unique block such that (n,m)\in E^1_q (m=succ n), and propagate (x,z) to m 
                  [ By this we mean: assign PHI(m,x) := PHI(m,x) /\ z where undefined PHI(m,x) is 
                  interpreted as \Omega; if the value has changed, then add (m,x) to WORK]
                  Kalmer: Why must m be unique? *)
                (*let m = match succ n with [x] -> x | _ -> failwith "Call edge not unique‽" in*)
                let nextNode m =
                  let z' = comb (add_var y) n p y z in
                  propagate x z' m
                in
                List.iter nextNode (succ n)
              else
                (* Otherwise, propagate (y,y) to r_p. This will propagation through p, which will later
                   trigger propagation to the block following n in q.*)
                 propagate y' y' (r p)
            in
            List.iter one_call ps
        (* (b) If n is the exit block of some procedure p, i.e., n = e_p, find all pairs (m,u) such that
               m is a block following some call c to p, and PHI(c,u) = x, and for each such pair propagate
               (u,y) to m *)
          | `ExitOfProc p ->
              dbg (fun () -> Pretty.printf "EXIT OF %s\n" p.Cil.vname);
              let one_callsite (c,u) =
                dbg (fun () -> Pretty.printf "callsite of %s:\n %a\n%a\n\n" p.Cil.vname N.pretty_trace c L.pretty u);                
                let pval = find_bot (c,u) in
                let ps = enter (add_var y) c pval in
                if List.exists (fun (v,y) -> v.Cil.vid=p.Cil.vid && L.equal y x) ps then
                  let callsite_succ m =
                    let oldval = find_bot (m,u) in
                    let y' = comb (add_var y) c p pval y  in
                    dbg (fun () -> Pretty.printf "OLD:%a\n\nCALL:%a\n\nNEW:%a\n\n\n" L.pretty oldval L.pretty x L.pretty y');
                    if not (L.equal oldval y) then propagate u y' m
                  in
                  List.iter callsite_succ (succ c)
              in
              WS.iter one_callsite (try CAL.find calls p with Not_found -> WS.empty)
        (* (c) If n is any other block in some procedure p, then, for each m \in E^0_p - {n}, 
               propagate (x,f_(n,m)(y)) to m *)
          | _ (*`Other*) ->
              let ms = succ n in
              dbg (fun () -> Pretty.printf "OTHER EDGE (%d successors)\n" (List.length ms));
              List.iter 
                (fun m -> let nextval = f (add_var y) (n,m) y in propagate x nextval m) 
                ms
    done; phi with Finished ->
    (* 3. Repeat step (2) till WORK = {}. When this happens, PHI represents
          the desired \phi functions, computed only for "relevant" data values,
          from which the x solution can computed ... *)
    phi
     
end