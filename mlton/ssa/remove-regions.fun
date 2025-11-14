functor RemoveRegions(S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct
  open S

  structure Allocates =
  struct
    structure L = TwoPointLattice (val bottom = "pureHeap" val top = "stackAlloc")
    open L

    val isStackAlloc = isTop
    val makeStackAlloc = makeTop
    val whenStackAlloc = addHandler
  end

  structure AllocatesInCaller =
  struct
    structure L = TwoPointLattice (val bottom = "doesNotAllocInCaller" val top = "allocInCaller")
    open L

    val allocatesInCaller = isTop
    val makeAllocatesInCaller = makeTop
    val whenAllocatesInCaller = addHandler
  end

  type regionId = Label.t * int (* (block label, statement index) *)

  type regionInfo =
    { pushLoc: regionId
    , hasStackAlloc: bool ref
    , popLocs: regionId list ref (* Multiple pops can correspond to one push *)
    }

  fun transformFunction
    (func: Function.t, getFuncAlloc: Func.t -> Allocates.t, getFuncAllocInCaller: Func.t -> AllocatesInCaller.t) :
    Function.t * bool =
    let
      val {args, blocks, mayInline, name, raises, returns, start} = Function.dest func

      val regions: regionInfo list ref = ref []

      val allocatesInCaller: bool ref = ref false

      fun findRegion (pushLoc as (label, idx): regionId) : regionInfo option =
        List.peek (!regions, fn r =>
          let val (rLabel, rIdx) = #pushLoc r
          in Label.equals (label, rLabel) andalso idx = rIdx
          end)

      fun getOrCreateRegion (pushLoc: regionId) : regionInfo =
        case findRegion pushLoc of
          SOME r => r
        | NONE =>
            let
              val r = {pushLoc = pushLoc, hasStackAlloc = ref false, popLocs = ref []}
              val _ = List.push (regions, r)
            in
              r
            end

      val {get = getBlockDepth, set = setBlockDepth, ...} = Property.getSet
        (Label.plist, Property.initConst (NONE : regionId list option))

      fun isStackMode (mode: Mode.t) : bool =
        case mode of
          Mode.Stack => true
        | _ => false

      fun markRegionsAllocate (regionStack: regionId list) : unit =
        case regionStack of
          [] => (* empty stack means we're allocating in caller's region *) allocatesInCaller := true
        | _ => List.foreach (regionStack, fn pushLoc => #hasStackAlloc (getOrCreateRegion pushLoc) := true)

      fun processBlock (block: Block.t, depthStack: regionId list) : regionId list =
        let
          val Block.T {label, statements, transfer, ...} = block
          val currentDepth: regionId list ref = ref depthStack

          fun processStatement (stmtIdx: int, stmt: Statement.t) : unit =
            let
              val Statement.T {exp, ...} = stmt
              val stmtLoc = (label, stmtIdx)
            in
              case exp of
                Exp.PrimApp {prim, mode, ...} =>
                  (case prim of
                     Prim.Region_push =>
                       let
                         val _ = getOrCreateRegion stmtLoc
                         val _ = currentDepth := stmtLoc :: (!currentDepth)
                       in
                         ()
                       end
                   | Prim.Region_pop =>
                       (case !currentDepth of
                          [] => ()
                        | pushLoc :: rest =>
                            let
                              val region = getOrCreateRegion pushLoc
                              val _ = List.push (#popLocs region, stmtLoc)
                              val _ = currentDepth := rest
                            in
                              ()
                            end)
                   | _ =>
                       (case mode of
                          SOME m => if isStackMode m then markRegionsAllocate (!currentDepth) else ()
                        | NONE => ()))
              | Exp.ConApp {mode, ...} => if isStackMode mode then markRegionsAllocate (!currentDepth) else ()
              | Exp.Tuple {mode, ...} => if isStackMode mode then markRegionsAllocate (!currentDepth) else ()
              | _ => ()
            end

          val _ = Vector.foreachi (statements, processStatement)

            val _ =
              case transfer of
                Transfer.Call {func, ...} =>
                  if Allocates.isStackAlloc (getFuncAlloc func) orelse
                     AllocatesInCaller.allocatesInCaller (getFuncAllocInCaller func)
                  then markRegionsAllocate (!currentDepth) else ()
              | _ => ()
        in
          !currentDepth
        end

      val _ = Function.dfs (func, fn block as Block.T {label, ...} =>
        let
          val entryDepth =
            case getBlockDepth label of
              SOME d => d
            | NONE => []

          val exitDepth = processBlock (block, entryDepth)

          val Block.T {transfer, ...} = block
          val _ = Transfer.foreachLabel (transfer, fn succLabel =>
            case getBlockDepth succLabel of
              NONE => setBlockDepth (succLabel, SOME exitDepth)
            | SOME existingDepth =>
                if List.length exitDepth < List.length existingDepth then setBlockDepth (succLabel, SOME exitDepth)
                else ())
        in
          fn () => ()
        end)

      val stmtsToRemove: regionId list ref = ref []
      val numRegionsRemoved = ref 0
      val _ = List.foreach (!regions, fn region =>
        if !(#hasStackAlloc region) then
          ()
        else
          let
            val _ = numRegionsRemoved := !numRegionsRemoved + 1
            val _ = List.push (stmtsToRemove, #pushLoc region)
            val _ = List.foreach (!(#popLocs region), fn popLoc => List.push (stmtsToRemove, popLoc))
          in
            ()
          end)

      val _ =
        if !numRegionsRemoved > 0 then
          Control.diagnostic (fn () =>
            Layout.str (concat
              ["RemoveRegions: ", Func.toString name, " removed ", Int.toString (!numRegionsRemoved), " empty regions"]))
        else
          ()

      val stmtsToRemoveSet = !stmtsToRemove
      fun shouldRemove (blockLabel: Label.t, stmtIdx: int) : bool =
        List.exists (stmtsToRemoveSet, fn (l, i) => Label.equals (l, blockLabel) andalso i = stmtIdx)

      val newBlocks = Vector.map (blocks, fn Block.T {args, label, statements, transfer} =>
        let
          val filteredStatements = Vector.keepAllMapi (statements, fn (i, s) =>
            if shouldRemove (label, i) then NONE else SOME s)
        in
          Block.T {args = args, label = label, statements = filteredStatements, transfer = transfer}
        end)
    in
      ( Function.new
          { args = args
          , blocks = newBlocks
          , mayInline = mayInline
          , name = name
          , raises = raises
          , returns = returns
          , start = start
          }
      , !allocatesInCaller
      )
    end

  fun transform (Program.T {datatypes, globals, functions, main}) =
    let
      val {get = getFuncAlloc: Func.t -> Allocates.t, ...} = Property.get (Func.plist, Property.initFun (fn _ =>
        Allocates.new ()))

      val {get = getFuncAllocInCaller: Func.t -> AllocatesInCaller.t, ...} =
        Property.get (Func.plist, Property.initFun (fn _ => AllocatesInCaller.new ()))

      (* First pass: set up handlers so allocation info propagates *)
      val _ = List.foreach (functions, fn f =>
        let
          val {name, blocks, ...} = Function.dest f
          val myAlloc = getFuncAlloc name
          val myAllocInCaller = getFuncAllocInCaller name

          val _ = Vector.foreach (blocks, fn Block.T {transfer, ...} =>
            case transfer of
              Transfer.Call {func, ...} =>
                ( Allocates.whenStackAlloc (getFuncAlloc func, fn () => Allocates.makeStackAlloc myAlloc)
                ; AllocatesInCaller.whenAllocatesInCaller (getFuncAllocInCaller func, fn () =>
                    Allocates.makeStackAlloc myAlloc)
                )
            | _ => ())
        in
          ()
        end)

      (* Second pass: discover direct stack allocations *)
      val _ = List.foreach (functions, fn f =>
        let
          val {name, blocks, ...} = Function.dest f
          val myAlloc = getFuncAlloc name

          fun checkBlock (Block.T {statements, ...}) =
            Vector.foreach (statements, fn Statement.T {exp, ...} =>
              case exp of
                Exp.PrimApp {mode = SOME Mode.Stack, ...} => Allocates.makeStackAlloc myAlloc
              | Exp.ConApp {mode = Mode.Stack, ...} => Allocates.makeStackAlloc myAlloc
              | Exp.Tuple {mode = Mode.Stack, ...} => Allocates.makeStackAlloc myAlloc
              | _ => ())
        in
          Vector.foreach (blocks, checkBlock)
        end)

      (* Phase 2: Transform each function to remove empty regions *)
      (* This also discovers which functions allocate in caller's region *)
      (* We iterate until no new exclave information is discovered *)
      fun iterateUntilFixed (currentFunctions: Function.t list) : Function.t list =
        let
          val newFunctionsAndFlags = List.map (currentFunctions, fn f =>
            transformFunction (f, getFuncAlloc, getFuncAllocInCaller))

          val newFunctions = List.map (newFunctionsAndFlags, #1)

          val anyNewExclave = ref false
          val _ = List.foreach (List.zip (currentFunctions, newFunctionsAndFlags), fn (origFunc, (_, allocInCaller)) =>
            if allocInCaller then
              let
                val {name, ...} = Function.dest origFunc
                val allocInCallerLattice = getFuncAllocInCaller name
              in
                if not (AllocatesInCaller.allocatesInCaller allocInCallerLattice) then
                  (AllocatesInCaller.makeAllocatesInCaller allocInCallerLattice; anyNewExclave := true)
                else
                  ()
              end
            else
              ())
        in
          if !anyNewExclave then iterateUntilFixed newFunctions else newFunctions
        end

      val newFunctions = iterateUntilFixed functions
    in
      Program.T {datatypes = datatypes, globals = globals, functions = newFunctions, main = main}
    end

end
