(** Helpful functions for dealing with [Cil]. *)

open GobConfig
open Json
open Gil
module E = Errormsg
module GU = Goblintutil

let currentStatement = ref dummyStmt
let ugglyImperativeHack = ref dummyFile
let showtemps = ref false
    
let print (fileAST: file) = 
  dumpFile defaultCilPrinter stdout "stdout" fileAST
    
let printDebug fileAST = 
  dumpFile GPrinter.debugCilPrinter stdout "stdout" fileAST

exception Found of fundec
let getFun fun_name = 
  try 
    iterGlobals !ugglyImperativeHack (fun glob ->
      match glob with 
        | GFun({svar={vname=vn}} as def,_) when vn = fun_name -> raise (Found def)
        | _ -> ()
    );
    failwith ("Function "^ fun_name ^ " not found!")
  with
    | Found def -> def

let in_section check attr_list = 
  let f attr = match attr with
    | Attr ("section", [AStr str]) -> check str
    | _ -> false
  in List.exists f attr_list

let is_init = in_section (fun s -> s = ".init.text")
let is_initptr = in_section (fun s -> s = ".initcall6.init")
let is_exit = in_section (fun s -> s = ".exit.text")

let rec get_varinfo exp: varinfo = 
  (* ignore (Pretty.printf "expression: %a\n" (printExp plainCilPrinter) exp); *)
  match exp with
    | AddrOf (Var v, _) -> v
    | CastE (_,e) -> get_varinfo e
    | _ -> failwith "Unimplemented: searching for variable in more complicated expression"

exception MyException of varinfo
let find_module_init funs fileAST = 
  try iterGlobals fileAST (
    function 
      | GVar ({vattr=attr}, {init=Some (SingleInit exp) }, _) when is_initptr attr -> 
          raise (MyException (get_varinfo exp))
      | _ -> ()
    ); 
    (funs, [])
  with MyException var -> 
    let f (s:fundec) = s.svar.vname = var.vname in
      List.partition f funs

type startfuns = fundec list * fundec list * fundec list

let getFuns fileAST : startfuns =
  let add_main f (m,e,o) = (f::m,e,o) in
  let add_exit f (m,e,o) = (m,f::e,o) in
  let add_other f (m,e,o) = (m,e,f::o) in
  let f acc glob =
    match glob with 
      | GFun({svar={vname=mn}} as def,_) when List.mem mn (List.map string (get_list "mainfun")) -> add_main def acc
      | GFun({svar={vname=mn}} as def,_) when List.mem mn (List.map string (get_list "exitfun")) -> add_exit def acc
      | GFun({svar={vname=mn}} as def,_) when List.mem mn (List.map string (get_list "otherfun")) -> add_other def acc
      | GFun({svar={vname=mn; vattr=attr}} as def, _) when get_bool "kernel" && is_init attr -> 
          Printf.printf "Start function: %s\n" mn; set_string "mainfun[+]" mn; add_main def acc
      | GFun({svar={vname=mn; vattr=attr}} as def, _) when get_bool "kernel" && is_exit attr -> 
          Printf.printf "Cleanup function: %s\n" mn; set_string "exitfun[+]" mn; add_exit def acc
      | GFun ({svar={vstorage=NoStorage}} as def, _) when (get_bool "nonstatic") -> add_other def acc
      | GFun (def, _) when ((get_bool "allfuns")) ->  add_other def  acc
      | GFun (def, _) when get_string "ana.osek.oil" <> "" && OilUtil.is_starting def.svar.vname -> add_other def acc
      | _ -> acc
  in
  foldGlobals fileAST f ([],[],[])

let dec_table_ok = ref false
let dec_table = Hashtbl.create 111
let dec_make () : unit =
  dec_table_ok := true ;
  Hashtbl.clear dec_table;
  iterGlobals !ugglyImperativeHack (fun glob ->
    match glob with 
      | GFun({svar={vid=vid}} as def,_) -> Hashtbl.add dec_table vid def
      | _ -> ()
  )
  
let rec getdec fv = 
  if !dec_table_ok then
    Hashtbl.find dec_table fv.vid
  else begin
    dec_make ();
    getdec fv
  end

let getFirstStmt fd = List.hd fd.sbody.bstmts

let pstmt stmt = dumpStmt defaultCilPrinter stdout 0 stmt; print_newline ()

let p_expr exp = Pretty.printf "%a\n" (printExp defaultCilPrinter) exp
let d_expr exp = Pretty.printf "%a\n" (printExp plainCilPrinter) exp

let rec typeOf (e: exp) : typ = 
  match e with
  | Const(CInt64 (_, ik, _)) -> TInt(ik, [])

    (* Character constants have type int.  ISO/IEC 9899:1999 (E),
     * section 6.4.4.4 [Character constants], paragraph 10, if you
     * don't believe me. *)
  | Const(CChr _) -> intType

    (* The type of a string is a pointer to characters ! The only case when 
     * you would want it to be an array is as an argument to sizeof, but we 
     * have SizeOfStr for that *)
  | Const(CStr s) -> charPtrType

  | Const(CWStr s) -> TPtr(!wcharType,[])

  | Const(CReal (_, fk, _)) -> TFloat(fk, [])

  | Const(CEnum(tag, _, ei)) -> typeOf tag

  | Lval(lv) -> typeOfLval lv
  | SizeOf _ | SizeOfE _ | SizeOfStr _ -> !typeOfSizeOf
  | AlignOf _ | AlignOfE _ -> !typeOfSizeOf
  | UnOp (_, _, t) -> t
  | BinOp (_, _, _, t) -> t
  | CastE (t, _) -> t
  | AddrOf (lv) -> TPtr(typeOfLval lv, [])
  | StartOf (lv) -> begin
      match unrollType (typeOfLval lv) with
        TArray (t,_, a) -> TPtr(t, a)
     | _ -> raise Not_found
    end
  | Question _ -> failwith "Logical operations should be compiled away by CIL."
  | _ -> failwith "Unmatched pattern."

and typeOfInit (i: init) : typ = 
  match i with 
    SingleInit e -> typeOf e
  | CompoundInit (t, _) -> t

and typeOfLval = function
    Var vi, off -> typeOffset vi.vtype off
  | Mem addr, off -> begin
      match unrollType (typeOf addr) with
        TPtr (t, _) -> typeOffset t off
      | _ -> raise Not_found
  end

and typeOffset basetyp =
  let blendAttributes baseAttrs =
    let (_, _, contageous) = 
      partitionAttributes ~default:(AttrName false) baseAttrs in
    typeAddAttributes contageous
  in
  function
    NoOffset -> basetyp
  | Index (_, o) -> begin
      match unrollType basetyp with
        TArray (t, _, baseAttrs) ->
	  let elementType = typeOffset t o in
	  blendAttributes baseAttrs elementType
      | t -> raise Not_found
  end 
  | Field (fi, o) ->
      match unrollType basetyp with
        TComp (_, baseAttrs) ->
	  let fieldType = typeOffset fi.ftype o in
	  blendAttributes baseAttrs fieldType
      | _ -> raise Not_found
