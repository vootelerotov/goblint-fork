open Cil

module type S =
sig
  include Lattice.S
  val is_multi: t -> bool
  val is_bad: t -> bool
  val get_single: unit -> t
  val get_multi: unit -> t
  val get_main:  unit -> t
  val switch: t -> t -> bool
end

module Trivial = struct
  module TrivialNames = struct
    let truename = "Multithreaded"
    let falsename = "Singlethreaded"
  end
  include IntDomain.MakeBooleans (TrivialNames)

  let is_multi x = x
  let is_bad   x = x
  let get_single () = false
  let get_multi () = true
  let get_main  () = true
  let switch x y = x <> y
end

module Simple = struct
  module SimpleNames = struct
    let n = 3
    let names = function
      | 0 -> "Singlethreaded"
      | 1 -> "Main Thread"
      | 2 -> "Some Threads"
      | _ -> "WHAT??"
  end
  include Lattice.Chain (SimpleNames)
  let is_multi x = x > 0
  let is_bad   x = x > 1
  let get_multi () = 2
  let get_main  () = 1
  let get_single () = 0
  let switch x y = match x,y with
    | 0,0 -> false
    | 0,_ -> true
    | _,0 -> true
    | _   -> false
  let name () = "MT mode"
end


(** Type to represent an abstract thread ID. *)
module Thread = struct
  include Basetype.Variables

  let thread_hash = Hashtbl.create 113

  let get_thread_var (f: varinfo) loc =
    try Hashtbl.find thread_hash (f,loc)
    with Not_found ->
      let name =
        match loc with
        | None -> f.vname
        | Some l -> f.vname ^ "@" ^ Basetype.ProgLines.short 80 l
      in
      let newvar = makeGlobalVar name voidType in
      Hashtbl.add thread_hash (f,loc) newvar;
      newvar

  let start_thread v: t = get_thread_var v None
  let spawn_thread l v: t = get_thread_var v (Some l)
end

(** The basic thread domain that distinguishes singlthreaded mode, a single
  * thread ID, and otherwise goes to top. *)
module SimpleThreadDomain = struct
  module ThreadLiftNames = struct
    let bot_name = "Bot Threads"
    let top_name = "Top Threads"
  end
  module Lifted =
  struct
    include Lattice.Flat (Thread) (ThreadLiftNames)
    let name () = "Thread"
  end
  include Lattice.ProdSimple (Simple) (Lifted)
  let is_multi (x,_) = x > 0
  let is_bad   (x,_) = x > 1
  let get_multi () = (2, Lifted.top ())
  let make_main (x,y) = (Simple.join 1 x, y)
  let spawn_thread l v = (2, `Lifted (Thread.spawn_thread l v))
  let start_single v : t = (0, `Lifted (Thread.start_thread v))
  let start_main   v : t = (2, `Lifted (Thread.start_thread v))
  let start_multi  v : t = (2, `Lifted (Thread.start_thread v))
  let switch (x,z) (y,_) = (Simple.switch x y, z)


  let short w (x,y) =
    let tid = Lifted.short w y in
    if x > 1 then tid else tid ^ "!"
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let same_tid x y =
    match x,y with
    | (_, `Lifted x), (_, `Lifted y) -> Thread.equal x y
    | _ -> false
end

module NotSimple = struct 
  module MultiThreaded = IntDomain.MakeBooleans (struct let truename = "multithreaded"
							let falsename = "singlethreaded" end)

  module PhaseSet = struct
    include SetDomain.ToppedSet(Basetype.Phase)( struct let topname = "All" end)
    let is_init_exit xs =
      let is_single x = match x with
	| Basetype.Init -> true
	| Basetype.Exit -> true
	| _ -> false
      in
      match xs with 
      | All -> false
      | _ -> for_all is_single xs
    let init_phase ()  =  singleton Basetype.Init
    let exit_phase ()  =  singleton Basetype.Exit
    let init_spawn_phase () = singleton Basetype.InitSpawn
    let exit_spawn_phase () = singleton Basetype.ExitSpawn
    let transform_phase phase = match phase with
      | Basetype.Init -> Basetype.InitSpawn
      | Basetype.Exit -> Basetype.ExitSpawn
      | _ -> phase
    let transform phases = map transform_phase phases
    let left_side phases =
      let add_to_it x s = Access.LSSSet.add (Access.LSSet.singleton ("phase",Basetype.Phase.short 80 x)) s
      in
      match phases with
      | All -> Access.LSSSet.singleton (Access.LSSet.empty ())
      | Set s -> S.fold add_to_it s (Access.LSSSet.empty ())
    
    (*let short w x : string = 
      match x with
      | All -> "All"
      | Set t -> Printexc.print_raw_backtrace stdout (Printexc.get_callstack 5);
	print_int w;
	S.short max_int t*)
  end

  module Unique = IntDomain.MakeBooleans (struct let truename = "not unique" 
						 let falsename = "unique" end)

 
  module LiftedThreads  = 
  struct 
    include Lattice.Flat (Thread) (struct let bot_name = "Bot threads"
					  let top_name = "Top threads" end)
    let name () = "Thread"
  end
  include Lattice.ProdSimple (Lattice.ProdSimple (PhaseSet) (Unique)) (LiftedThreads)
  let is_multi ((xs,_),_) = not (PhaseSet.is_init_exit xs)
  let is_bad ((_,y),_) = y
  let get_multi () = ((PhaseSet.top (),Unique.top ()), LiftedThreads.top())
  let make_main ((phases,y),z) =  ((PhaseSet.transform phases, y), z)
  let spawn_thread l v ((phases,_),_) = ((PhaseSet.transform phases , Unique.top ()), `Lifted (Thread.spawn_thread l v ))
  let start_single v : t = ((PhaseSet.init_phase () , Unique.bot ()), `Lifted (Thread.start_thread v))
  let start_main v : t = ((PhaseSet.exit_phase (), Unique.bot()), `Lifted (Thread.start_thread v))
  let start_multi v : t = ((PhaseSet.top(), Unique.top()), `Lifted (Thread.start_thread v))
  let switch (x,y,z) (x1,y1,_) = (Simple.switch x x1, Simple.switch y y1, z)
  let short_thread_id length thread_id = LiftedThreads.short length thread_id
  let left_side (phases: PhaseSet.t) = PhaseSet.left_side phases

  let short w elem =
  let ((x,y),z) = elem in 
  let tid = LiftedThreads.short w z in
  if is_bad elem then tid else tid ^ "!"
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let same_tid x y =
    match x,y with
    | (_, `Lifted x), (_, `Lifted y) -> Thread.equal x y
    | _ -> false
    
end

(** Thread domain that separtes between uniqe single thread,
  * multithread enviornment with the thread being unique and 
  * multihreaded enviornment with the thread not being unique. 
**)
module ThreadSet = SetDomain.ToppedSet (Thread) (struct let topname = "All Threads" end)

module CreatedThreadSet =
struct
  include ThreadSet

  let toXML_f sf x =
    match toXML x with
    | Xml.Element (node, [text, _], elems) ->
      let summary = "Created Threads: " ^ sf Goblintutil.summary_length x in
      Xml.Element (node, [text, summary], elems)
    | x -> x

  let toXML s  = toXML_f short s
end

module ThreadCreation =
struct
  module UNames = struct
    let truename  = "repeated"
    let falsename = "unique"
  end
  module Uniqueness = IntDomain.MakeBooleans (UNames)
  include Lattice.ProdSimple (Uniqueness) (ThreadSet)
end


module ThreadStringSet =
struct
  include SetDomain.ToppedSet (Printable.Strings) (struct let topname = "All Threads" end)

  let toXML_f sf x =
    match toXML x with
    | Xml.Element (node, [text, _], elems) ->
      let summary = "Thread: " ^ sf Goblintutil.summary_length x in
      Xml.Element (node, [text, summary], elems)
    | x -> x

  let toXML s  = toXML_f short s
end
