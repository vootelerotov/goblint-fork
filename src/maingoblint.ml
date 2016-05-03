(** This is the main program! *)

open Prelude
open GobConfig
open Defaults
open Printf
open Json
open Goblintutil
open Questions

let writeconf = ref false
let writeconffile = ref ""

(** Print version and bail. *)
let print_version ch =
  let open Version in let open Config in
  let f ch b = if b then fprintf ch "enabled" else fprintf ch "disabled" in
  printf "Goblint version: %s\n" goblint;
  printf "Cil version:     %s (%s)\n" Cil.cilVersion cil;
  printf "Configuration:   tracing %a, tracking %a\n" f tracing f tracking ;
  raise BailFromMain

(** Print helpful messages. *)
let print_help ch =
  fprintf ch "Usage: goblint [options] source-files\nOptions\n";
  fprintf ch "    -v                        Prints more status information.                 \n";
  fprintf ch "    -o <file>                 Prints the output to file.                      \n";
  fprintf ch "    -I <dir>                  Add include directory.                          \n";
  fprintf ch "    -IK <dir>                 Add kernel include directory.                   \n\n";
  fprintf ch "    --help                    Prints this text                                \n";
  fprintf ch "    --version                 Print out current version information.          \n\n";
  fprintf ch "    --conf <file>             Merge the configuration from the <file>.        \n";
  fprintf ch "    --writeconf <file>        Write the effective configuration to <file>     \n";
  fprintf ch "    --set <jpath> <jvalue>    Set a configuration variable <jpath> to the specified <jvalue>.\n";
  fprintf ch "    --sets <jpath> <string>   Set a configuration variable <jpath> to the string.\n";
  fprintf ch "    --enable  <jpath>         Set a configuration variable <jpath> to true.   \n";
  fprintf ch "    --disable <jpath>         Set a configuration variable <jpath> to false.  \n\n";
  fprintf ch "    --print_options           Print out commonly used configuration variables.\n";
  fprintf ch "    --print_all_options       Print out all configuration variables.          \n";
  fprintf ch "\n";
  fprintf ch "A <jvalue> is a string from the JSON language where single-quotes (')";
  fprintf ch " are used instead of double-quotes (\").\n\n";
  fprintf ch "A <jpath> is a path in a json structure. E.g. 'field.another_field[42]';\n";
  fprintf ch "in addition to the normal syntax you can use 'field[+]' append to an array.\n\n"

(** [Arg] option specification *)
let option_spec_list =
  let add_string l = let f str = l := str :: !l in Arg.String f in
  let add_int    l = let f str = l := str :: !l in Arg.Int f in
  let set_trace sys =
    let msg = "Goblin has been compiled without tracing, run ./scripts/trace_on.sh to recompile." in
    if Config.tracing then Tracing.addsystem sys
    else (prerr_endline msg; raise BailFromMain)
  in
  let oil file =
    set_string "ana.osek.oil" file;
    set_auto "ana.activated" "['base','escape','OSEK','OSEK2','stack_trace_set','fmode','flag']";
    set_auto "mainfun" "[]"
  in
  let configure_html () =
    if (get_string "outfile" = "") then
      set_string "outfile" "result";
    if get_string "exp.g2html_path" = "" then
      set_string "exp.g2html_path" get_goblint_path;
    set_bool "dbg.print_dead_code" true;
    set_bool "exp.cfgdot" true;
    set_bool "g2html" true;
    set_string "result" "fast_xml"
  in
  let tmp_arg = ref "" in
  [ "-o"                   , Arg.String (set_string "outfile"), ""
  ; "-v"                   , Arg.Unit (fun () -> set_bool "dbg.verbose" true), ""
  ; "-I"                   , Arg.String (set_string "includes[+]"), ""
  ; "-IK"                  , Arg.String (set_string "kernel_includes[+]"), ""
  ; "--set"                , Arg.Tuple [Arg.Set_string tmp_arg; Arg.String (fun x -> set_auto !tmp_arg x)], ""
  ; "--sets"               , Arg.Tuple [Arg.Set_string tmp_arg; Arg.String (fun x -> set_string !tmp_arg x)], ""
  ; "--enable"             , Arg.String (fun x -> set_bool x true), ""
  ; "--disable"            , Arg.String (fun x -> set_bool x false), ""
  ; "--conf"               , Arg.String merge_file, ""
  ; "--writeconf"          , Arg.String (fun fn -> writeconf:=true;writeconffile:=fn), ""
  ; "--version"            , Arg.Unit print_version, ""
  ; "--print_options"      , Arg.Unit (fun _ -> printCategory stdout Std; raise BailFromMain), ""
  ; "--print_all_options"  , Arg.Unit (fun _ -> printAllCategories stdout; raise BailFromMain), ""
  ; "--trace"              , Arg.String set_trace, ""
  ; "--tracevars"          , add_string Tracing.tracevars, ""
  ; "--tracelocs"          , add_int Tracing.tracelocs, ""
  ; "--help"               , Arg.Unit (fun _ -> print_help stdout),""
  ; "--halp"               , Arg.Unit (fun _ -> print_help stdout),""
  ; "-help"                , Arg.Unit (fun _ -> print_help stdout),""
  ; "--html"               , Arg.Unit (fun _ -> configure_html ()),""
  ; "--oil"                , Arg.String oil, ""
  (*     ; "--tramp"              , Arg.String (set_string "ana.osek.tramp"), ""  *)
  ; "--osekdefaults"       , Arg.Unit (fun () -> set_bool "ana.osek.defaults" false), ""
  ; "--osektaskprefix"     , Arg.String (set_string "ana.osek.taskprefix"), ""
  ; "--osekisrprefix"      , Arg.String (set_string "ana.osek.isrprefix"), ""
  ; "--osektasksuffix"     , Arg.String (set_string "ana.osek.tasksuffix"), ""
  ; "--osekisrsuffix"      , Arg.String (set_string "ana.osek.isrsuffix"), ""
  ; "--osekcheck"          , Arg.Unit (fun () -> set_bool "ana.osek.check" true), ""
  ; "--oseknames"          , Arg.Set_string OilUtil.osek_renames, ""
  ; "--osekids"            , Arg.Set_string OilUtil.osek_ids, ""
  ]

(** List of C files to consider. *)
let cFileNames = ref []

(** Parse arguments and fill [cFileNames] and [jsonFiles]. Print help if needed. *)
let parse_arguments () =
  let jsonRegex = Str.regexp ".+\\.json$" in
  let recordFile fname =
    if Str.string_match jsonRegex fname 0
    then Goblintutil.jsonFiles := fname :: !Goblintutil.jsonFiles
    else cFileNames := fname :: !cFileNames
  in
  Arg.parse option_spec_list recordFile "Look up options using 'goblint --help'.";
  if !writeconf then begin File.with_file_out !writeconffile print; raise BailFromMain end

(** Initialize some globals in other modules. *)
let handle_flags () =
  let has_oil = get_string "ana.osek.oil" <> "" in
  if has_oil then Osek.Spec.parse_oil ();

  if get_bool "dbg.debug" then Messages.warnings := true;
  if get_bool "dbg.verbose" then begin
    Printexc.record_backtrace true;
    Errormsg.debugFlag := true;
    Errormsg.verboseFlag := true
  end;

  match get_string "dbg.dump" with
  | "" -> ()
  | path -> begin
      Messages.warn_out := Legacy.open_out (Legacy.Filename.concat path "warnings.out");
      set_string "outfile" ""
    end

(** Use gcc to preprocess a file. Returns the path to the preprocessed file. *)
let preprocess_one_file cppflags includes dirName fname =
  (* The actual filename of the preprocessed sourcefile *)
  let nname =  Filename.concat dirName (Filename.basename fname) in

  (* Preprocess using cpp. *)
  (* ?? what is __BLOCKS__? is it ok to just undef? this? http://en.wikipedia.org/wiki/Blocks_(C_language_extension) *)
  let command = Config.cpp ^ " --undef __BLOCKS__ " ^ cppflags ^ " " ^ includes ^ " \"" ^ fname ^ "\" -o \"" ^ nname ^ "\"" in
  if get_bool "dbg.verbose" then print_endline command;

  (* if something goes wrong, we need to clean up and exit *)
  let rm_and_exit () =
    if not (get_bool "keepcpp") then ignore (Goblintutil.rm_rf dirName); raise BailFromMain
  in
  try match Unix.system command with
    | Unix.WEXITED 0 -> nname
    | _ -> eprintf "Goblint: Preprocessing failed."; rm_and_exit ()
  with Unix.Unix_error (e, f, a) ->
    eprintf "%s at syscall %s with argument \"%s\".\n" (Unix.error_message e) f a; rm_and_exit ()

(** Preprocess all files. Return list of preprocessed files and the temp directory name. *)
let preprocess_files () =
  (* Handy (almost) constants. *)
  let myname = Filename.dirname Sys.executable_name in
  let kernel_root = Filename.concat myname "linux-headers" in
  let kernel_dir = kernel_root ^ "/include" in
  let arch_dir = kernel_root ^ "/arch/x86/include" in

  (* Preprocessor flags *)
  let cppflags = ref (get_string "cppflags") in

  (* the base include directory *)
  let include_dir =
    let incl1 = Filename.concat myname "includes" in
    let incl2 = "/usr/share/goblint/includes" in
    if get_string "custom_incl" <> "" then (get_string "custom_incl")
    else if Sys.file_exists incl1 then incl1
    else if Sys.file_exists incl2 then incl2
    else "/usr/local/share/goblint/includes"
  in

  (* include flags*)
  let includes = ref "" in

  (* fill include flags *)
  let one_include_f f x = includes := "-I " ^ f (string x) ^ " " ^ !includes in
  if get_string "ana.osek.oil" <> "" then includes := "-include " ^ (!OilUtil.header_path ^ !OilUtil.header) ^" "^ !includes;
  (*   if get_string "ana.osek.tramp" <> "" then includes := "-include " ^ get_string "ana.osek.tramp" ^" "^ !includes; *)
  get_list "includes" |> List.iter (one_include_f identity);
  get_list "kernel_includes" |> List.iter (Filename.concat kernel_root |> one_include_f);

  if Sys.file_exists include_dir
  then includes := "-I" ^ include_dir ^ " " ^ !includes
  else print_endline "Warning, cannot find goblint's custom include files.";

  (* reverse the files again *)
  cFileNames := List.rev !cFileNames;

  (* possibly add our lib.c to the files *)
  if get_bool "custom_libc" then
    cFileNames := (Filename.concat include_dir "lib.c") :: !cFileNames;

  (* If we analyze a kernel module, some special includes are needed. *)
  if get_bool "kernel" then begin
    let preconf = Filename.concat include_dir "linux/goblint_preconf.h" in
    let autoconf = Filename.concat kernel_dir "linux/kconfig.h" in
    cppflags := "-D__KERNEL__ -U__i386__ -include " ^ preconf ^ " -include " ^ autoconf ^ " " ^ !cppflags;
    (* These are not just random permutations of directories, but based on USERINCLUDE from the
     * Linux kernel Makefile (in the root directory of the kernel distribution). *)
    includes := !includes ^ " -I" ^ String.concat " -I" [
        kernel_dir; kernel_dir ^ "/uapi"; kernel_dir ^ "include/generated/uapi";
        arch_dir; arch_dir ^ "/generated"; arch_dir ^ "/uapi"; arch_dir ^ "/generated/uapi";
      ]
  end;

  (* The temp directory for preprocessing the input files *)
  let dirName = Goblintutil.create_dir "goblint_temp" in

  (* preprocess all the files *)
  if get_bool "dbg.verbose" then print_endline "Preprocessing files.";
  List.rev_map (preprocess_one_file !cppflags !includes dirName) !cFileNames, dirName


(** Possibly merge all postprocessed files *)
let merge_preprocessed (cpp_file_names, dirName) =
  (* get the AST *)
  if get_bool "dbg.verbose" then print_endline "Parsing files.";
  let files_AST = List.rev_map Cilfacade.getAST cpp_file_names in

  (* remove the files *)
  if not (get_bool "keepcpp") then ignore (Goblintutil.rm_rf dirName);

  let cilout =
    if get_string "dbg.cilout" = "" then Legacy.stderr else Legacy.open_out (get_string "dbg.cilout")
  in

  (* direct the output to file if requested  *)
  if not (get_bool "g2html" || get_string "outfile" = "") then Goblintutil.out := Legacy.open_out (get_string "outfile");
  Errormsg.logChannel := Messages.get_out "cil" cilout;

  (* we use CIL to merge all inputs to ONE file *)
  let merged_AST =
    match files_AST with
    | [one] -> Cilfacade.callConstructors one
    | [] -> prerr_endline "No arguments for Goblint?";
      prerr_endline "Try `goblint --help' for more information.";
      raise BailFromMain
    | xs -> Cilfacade.getMergedAST xs |> Cilfacade.callConstructors
  in

  (* using CIL's partial evaluation and constant folding! *)
  if get_bool "dopartial" then Cilfacade.partial merged_AST;
  Cilfacade.rmTemps merged_AST;

  (* create the Control Flow Graph from CIL's AST *)
  Cilfacade.createCFG merged_AST;
  Cilfacade.ugglyImperativeHack := merged_AST;
  merged_AST

(** Perform the analysis over the merged AST.  *)
let do_analyze merged_AST =
  let module L = Printable.Liszt (Basetype.CilFundec) in
  (* we let the "--eclipse" flag override result style: *)
  if get_bool "exp.eclipse" then set_string "result_style" "compact";

  if get_bool "justcil" then
    (* if we only want to print the output created by CIL: *)
    Cilfacade.print merged_AST
  else begin
    (* we first find the functions to analyze: *)
    if get_bool "dbg.verbose" then print_endline "And now...  the Goblin!";
    let (stf,exf,otf as funs) = Cilfacade.getFuns merged_AST in
    if stf@exf@otf = [] then failwith "No suitable function to start from.";
    if get_bool "dbg.verbose" then ignore (Pretty.printf "Startfuns: %a\nExitfuns: %a\nOtherfuns: %a\n"
                                             L.pretty stf L.pretty exf L.pretty otf);
    Goblintutil.has_otherfuns := otf <> [];
    (* and here we run the analysis! *)
    if get_string "result" = "html" then Report.prepare_html_report ();

    let do_all_phases ast funs =
      let do_one_phase ast p =
        phase := p;
        if get_bool "dbg.verbose" then (
          let aa = String.concat ", " @@ List.map Json.jsonString (get_list "ana.activated") in
          let at = String.concat ", " @@ List.map Json.jsonString (get_list "trans.activated") in
          print_endline @@ "Activated analyses for phase " ^ string_of_int p ^ ": " ^ aa;
          print_endline @@ "Activated transformations for phase " ^ string_of_int p ^ ": " ^ at
        );
        try Control.analyze ast funs
        with x ->
          let loc = !Tracing.current_loc in
          Printf.printf "About to crash on %s:%d\n" loc.Cil.file loc.Cil.line;
          raise x
          (* Cilfacade.ugglyImperativeHack := ast'; *)
      in
      (* old style is ana.activated = [phase_1, ...] with phase_i = [ana_1, ...]
         new style (Goblintutil.phase_config = true) is phases[i].ana.activated = [ana_1, ...]
         phases[i].ana.x overwrites setting ana.x *)
      let num_phases =
        let np,na,nt = Tuple3.mapn (List.length % get_list) ("phases", "ana.activated", "trans.activated") in
        phase_config := np > 0; (* TODO what about wrong usage like { phases = [...], ana.activated = [...] }? should child-lists add to parent-lists? *)
        if get_bool "dbg.verbose" then print_endline @@ "Using " ^ if !phase_config then "new" else "old" ^ " format for phases!";
        if np = 0 && na = 0 && nt = 0 then failwith "No phases and no activated analyses or transformations!";
        max np 1
      in
      ignore @@ Enum.iter (do_one_phase ast) (0 -- (num_phases - 1))
    in

    (* Analyze with the new experimental framework. *)
    Stats.time "analysis" (do_all_phases merged_AST) funs
  end

let do_html_output () =
  (* if we are in Cygwin, we use the host's Java and GraphViz -> paths need to be converted from Cygwin to Windows style *)
  let get_path path = if Sys.os_type = "Cygwin" then "$(cygpath -wa "^path^")" else path in
  let jar = Filename.concat (get_string "exp.g2html_path") "g2html.jar" in
  if get_bool "g2html" then begin
    if Sys.file_exists jar then begin
      let command = "java -jar "^get_path jar^" --result-dir "^get_path (get_string "outfile")^" "^get_path !Messages.xml_file_name in
      try match Unix.system command with
        | Unix.WEXITED 0 -> ()
        | _ -> eprintf "HTML generation failed!\n"
      with Unix.Unix_error (e, f, a) ->
        eprintf "%s at syscall %s with argument \"%s\".\n" (Unix.error_message e) f a
    end else
      eprintf "Warning: jar file %s not found.\n" jar
  end

let handle_extraspecials () = 
  let f xs = function
    | String x -> x::xs
    | _ -> xs
  in
  let funs = List.fold_left f [] (get_list "exp.extraspecials") in
  LibraryFunctions.add_lib_funs funs

(** the main function *)
let main =
  let main_running = ref false in fun () ->
    if !main_running then () else
      let _ = main_running := true in
      try
        Stats.reset Stats.SoftwareTimer;
        Cilfacade.init ();
        parse_arguments ();
        handle_extraspecials ();
        handle_flags ();
        if String.length (get_string "questions.file") > 0 then question_load_db (get_string "questions.file");
        preprocess_files () |> merge_preprocessed |> do_analyze;
        if String.length (get_string "questions.file") > 0 then question_save_db (get_string "questions.file");
        Report.do_stats !cFileNames;
        do_html_output ();
        if !verified = Some false then exit 3 (* verifier failed! *)
      with BailFromMain -> ()

let _ =
  at_exit main
