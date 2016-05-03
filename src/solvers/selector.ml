open Prelude
open Analyses
open GobConfig

(* Registered solvers. *)
let solvers =
  ref ["effectWCon", (module EffectWCon.Make2 : GenericGlobSolver)]

(** Register your solvers here!!! *)
let add_solver x = solvers := x::!solvers

(** The solver that actually uses the implementation based of [GobConfig.get_string "solver"]. *)
module Make =
  functor (S:GlobConstrSys) ->
  functor (LH:Hash.H with type key=S.LVar.t) ->
  functor (GH:Hash.H with type key=S.GVar.t) ->
  struct

    (** Dynamically choose the solver. *)
    let choose_solver solver =
      try List.assoc solver !solvers
      with Not_found ->
        raise @@ ConfigError ("Solver '"^solver^"' not found. Abort!")

    (** You wont belive this! It really works! *)
    let solve =
      (* Watch and learn! *)
      let dark_magic (module SOL : GenericGlobSolver) =
        let module F = SOL (S) (LH) (GH) in F.solve
      in
      (* Did you see! *)
      dark_magic (choose_solver (get_string "solver"))

  end

let _ =
  let module T1 : GenericGlobSolver = Make in
  ()
