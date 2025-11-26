functor Regions(S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct
  open S

  val regionPushStmt = Statement.T
    { exp = Exp.PrimApp {args = Vector.new0 (), prim = Prim.Region_push, mode = NONE, targs = Vector.new0 ()}
    , ty = Type.unit
    , var = NONE
    }

  val regionPopStmt = Statement.T
    { exp = Exp.PrimApp {args = Vector.new0 (), prim = Prim.Region_pop, mode = NONE, targs = Vector.new0 ()}
    , ty = Type.unit
    , var = NONE
    }

  val {get = doesFunctionAllocateInCaller, set = setFunctionAllocatesInCaller, ...} =
    Property.getSetOnce (Func.plist, Property.initConst false)

  exception AllocFound

  fun isStackMode (mode: Mode.t) : bool =
    case mode of
      Mode.Stack => true
    | _ => false

  fun hasStackAlloc (Statement.T {exp, ...}) : bool =
    case exp of
      Exp.ConApp {mode, ...} => isStackMode mode
    | Exp.Tuple {mode, ...} => isStackMode mode
    | Exp.PrimApp {mode = mode_opt, ...} => Option.isSome mode_opt andalso isStackMode (Option.valOf mode_opt)
    | _ => false

  fun statementIsPrim prim (Statement.T {exp, ...}) =
    case exp of
      Exp.PrimApp {prim = prim', ...} => Prim.equals (prim', prim)
    | _ => false

  fun blockHasPrim prim (block: Block.t) =
    Vector.exists (Block.statements block, statementIsPrim prim)

  fun transformFunction (func: Function.t) : Function.t =
    let
      val {args, blocks, mayInline, name, raises, returns, start} = Function.dest func
      val startBlockIdx = ref NONE
      val _ = Vector.foreachi (blocks, fn (i, block) =>
        if Label.equals (Block.label block, start) then startBlockIdx := SOME i else ())
      val startIdx =
        case !startBlockIdx of
          SOME i => i
        | NONE => Error.bug "Regions.transformFunction: missing start block"

      val foundStackAlloc = ref false
      fun allocSearch (block: Block.t) : unit -> unit =
        let
          val stmts = Block.statements block
          val transfer = Block.transfer block
          val _ = Vector.foreach (stmts, fn stmt =>
            if hasStackAlloc stmt then (foundStackAlloc := true; raise AllocFound) else ())
          val _ =
            case transfer of
              Transfer.Call {func, ...} =>
                if doesFunctionAllocateInCaller func then (foundStackAlloc := true; raise AllocFound) else ()
            | _ => ()
        in
          fn () => ()
        end

      val _ = Function.dfs (func, allocSearch) handle AllocFound => ()
      val hasStackAlloc = !foundStackAlloc
    in
      if not hasStackAlloc then
        func
      else
        let
          val numBlocks = Vector.length blocks
          val blockHasPop = Array.tabulate (numBlocks, fn i => blockHasPrim Prim.Region_pop (Vector.sub (blocks, i)))
          val blockHasPush = Array.tabulate (numBlocks, fn i => blockHasPrim Prim.Region_push (Vector.sub (blocks, i)))
          val handlerNeedsPop = Array.array (numBlocks, false)
          val {destroy = destroyLabelIndex, get = labelIndex, set = setLabelIndex} = Property.destGetSetOnce
            (Label.plist, Property.initRaise ("Regions.labelIndex", Label.layout))

          val _ = Vector.foreachi (blocks, fn (i, Block.T {label, ...}) => setLabelIndex (label, i))
          val preds = Array.tabulate (numBlocks, fn _ => ref ([] : int list))
          val _ = Vector.foreachi (blocks, fn (i, Block.T {transfer, ...}) =>
            Transfer.foreachLabel (transfer, fn dst =>
              let
                val j = labelIndex dst
                val r = Array.sub (preds, j)
              in
                r := i :: !r
              end))
          val entryHasRegion = Array.array (numBlocks, false)
          val exitHasRegion = Array.array (numBlocks, false)
          val _ = Array.update (entryHasRegion, startIdx, true)

          fun propagate () =
            let
              val changed = ref false
              val _ = Vector.foreachi (blocks, fn (i, _) =>
                let
                  val predsList = !(Array.sub (preds, i))
                  val newEntry = i = startIdx orelse List.exists (predsList, fn pred => Array.sub (exitHasRegion, pred))
                  val newExit = newEntry andalso not (Array.sub (blockHasPop, i))
                  val () =
                    if Array.sub (entryHasRegion, i) <> newEntry then
                      (Array.update (entryHasRegion, i, newEntry); changed := true)
                    else
                      ()
                  val () =
                    if Array.sub (exitHasRegion, i) <> newExit then
                      (Array.update (exitHasRegion, i, newExit); changed := true)
                    else
                      ()
                in
                  ()
                end)
            in
              if !changed then propagate () else ()
            end
          val _ = propagate ()

          val _ = Vector.foreachi (blocks, fn (i, Block.T {transfer, ...}) =>
            case transfer of
              Transfer.Call {return = Return.NonTail {handler, ...}, ...} =>
                (case handler of
                   Handler.Handle handlerLabel =>
                     if Array.sub (exitHasRegion, i) then
                       let val j = labelIndex handlerLabel
                       in if not (Array.sub (blockHasPop, j)) then Array.update (handlerNeedsPop, j, true) else ()
                       end
                     else
                       ()
                 | _ => ())
            | _ => ())

          fun transferCanEnterStart transfer =
            let
              val hitsStart = ref false
              val _ = Transfer.foreachLabel (transfer, fn dst =>
                if Label.equals (dst, start) then hitsStart := true else ())
            in
              !hitsStart
            end

          fun needsPopAtExit i transfer =
            let
              val enterStart = transferCanEnterStart transfer
              val exitNeedsPop =
                Array.sub (exitHasRegion, i)
                andalso
                (case transfer of
                   Transfer.Raise _ => true
                 | Transfer.Return _ => true
                 | Transfer.Call {return = Return.Tail, ...} => true
                 | _ => enterStart)
              val tailRecNeedsPop = enterStart andalso not (Array.sub (blockHasPop, i))
            in
              exitNeedsPop orelse tailRecNeedsPop
            end
          fun maybeAddPush (stmts, label, hasPush) =
            if Label.equals (label, start) andalso not hasPush then regionPushStmt :: stmts else stmts
          val newBlocks = Vector.mapi (blocks, fn (i, Block.T {args = blockArgs, label, statements, transfer}) =>
            let
              val stmtsList0 = Vector.toList statements
              val stmtsList0 = if Array.sub (handlerNeedsPop, i) then regionPopStmt :: stmtsList0 else stmtsList0
              val stmtsList1 = maybeAddPush (stmtsList0, label, Array.sub (blockHasPush, i))
              val stmtsList2 = if needsPopAtExit i transfer then stmtsList1 @ [regionPopStmt] else stmtsList1
            in
              Block.T {args = blockArgs, label = label, statements = Vector.fromList stmtsList2, transfer = transfer}
            end)
          val _ = destroyLabelIndex ()
        in
          Function.new
            { args = args
            , blocks = newBlocks
            , mayInline = mayInline
            , name = name
            , raises = raises
            , returns = returns
            , start = start
            }
        end
    end

  fun functionAllocatesInCaller (func: Function.t) : unit =
    let
      val {get = getVarMode, set = setVarMode, ...} = Property.getSetOnce (Var.plist, Property.initConst NONE)

      val allocatesInCaller = ref false
      val _ = Function.dfs (func, fn block =>
        ( Vector.foreach (Block.statements block, fn stmt =>
            let
              val var = Statement.var stmt
            in
              if Option.isSome var then
                let
                  val var = Option.valOf var
                in
                  case Statement.exp stmt of
                    Exp.PrimApp {mode, ...} => (setVarMode (var, mode))
                  | Exp.ConApp {mode, ...} => (setVarMode (var, SOME mode))
                  | Exp.Tuple {mode, ...} => (setVarMode (var, SOME mode))
                  | _ => ()
                end
              else
                ()
            end)
        ; fn () =>
            case Block.transfer block of
              Transfer.Return vars =>
                Vector.foreach (vars, fn var =>
                  let
                    val mode = getVarMode var
                  in
                    if Option.isSome mode andalso Mode.equals (Option.valOf mode, Mode.Stack) then
                      allocatesInCaller := true
                    else
                      ()
                  end)
            | _ => ()
        ))
      val _ = Function.clear func
    in
      setFunctionAllocatesInCaller (Function.name func, !allocatesInCaller)
    end


  fun transform (Program.T {datatypes, globals, functions, main}) =
    let
      val _ = List.foreach (functions, functionAllocatesInCaller)
    in
      Program.T
        {datatypes = datatypes, globals = globals, functions = List.map (functions, transformFunction), main = main}
    end
end
