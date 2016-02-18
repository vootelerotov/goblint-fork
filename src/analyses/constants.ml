(** Simple integer constants finder. *)

open Prelude.Ana
open Analyses

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name = "constants"

  module ID = IntDomain.Integers
  module ISD = SetDomain.ToppedSet (ID) (struct let topname = "bla" end)

  module D = ISD
  module G = Lattice.Unit
  module C = ISD

  class constVisitorClass (ctx : ISD.t ref) = object(self)
    inherit nopCilVisitor

    method vexpr (e:exp) =
      (match e with
      | Const (CInt64 (i, _, _)) ->
        ctx := ISD.add (ID.of_int (i)) (!ctx)
      | _ ->
        ());
      DoChildren
  end

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let ct = ref ctx.local in
      ignore (visitCilLval (new constVisitorClass ct) lval);
      ignore (visitCilExpr (new constVisitorClass ct) rval);
      !ct

  let branch ctx (exp:exp) (tv:bool) : D.t =
    let ct = ref ctx.local in
      ignore (visitCilExpr (new constVisitorClass ct) exp);
      !ct

  let body ctx (f:fundec) : D.t =
    let ct = ref ctx.local in
      ignore (visitCilFunction (new constVisitorClass ct) f);
      !ct

  let return ctx (exp:exp option) (f:fundec) : D.t =
    match exp with
    | Some e ->
      let ct = ref ctx.local in
        ignore (visitCilFunction (new constVisitorClass ct) f);
        ignore (visitCilExpr (new constVisitorClass ct) e);
        !ct
    | _ ->
      ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [
      let ct = ref ctx.local in
        (match lval with | Some l -> ignore (visitCilLval (new constVisitorClass ct) l) | _ -> ());
        ignore (visitCilVarDecl (new constVisitorClass ct) f);
        List.iter (fun e -> ignore (visitCilExpr (new constVisitorClass ct) e)) args;
        !ct
    ,
      ctx.local
    ]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    let ct = ref (D.join au ctx.local) in
      (match lval with | Some l -> ignore (visitCilLval (new constVisitorClass ct) l) | _ -> ());
      ignore (visitCilExpr (new constVisitorClass ct) fexp);
      ignore (visitCilVarDecl (new constVisitorClass ct) f);
      List.iter (fun e -> ignore (visitCilExpr (new constVisitorClass ct) e)) args;
      !ct

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let ct = ref ctx.local in
      (match lval with | Some l -> ignore (visitCilLval (new constVisitorClass ct) l) | _ -> ());
      ignore (visitCilVarDecl (new constVisitorClass ct) f);
      List.iter (fun e -> ignore (visitCilExpr (new constVisitorClass ct) e)) arglist;
      !ct

  let startstate v = D.bot ()
  let otherstate v = D.top ()
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : Spec)
