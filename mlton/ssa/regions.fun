functor Regions(S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct
  open S

  val regionPushStmt = Statement.T
    { exp = Exp.PrimApp
        { args = Vector.new0 ()
        , prim = Prim.Region_push
        , mode = NONE
        , targs = Vector.new0 ()
        }
    , ty = Type.unit
    , var = NONE
    }

  val regionPopStmt = Statement.T
    { exp = Exp.PrimApp
        { args = Vector.new0 ()
        , prim = Prim.Region_pop
        , mode = NONE
        , targs = Vector.new0 ()
        }
    , ty = Type.unit
    , var = NONE
    }

  val
    { get = doesFunctionAllocateInCaller
    , set = setFunctionAllocatesInCaller
    , ...
    } = Property.getSetOnce (Func.plist, Property.initConst false)

  exception AllocFound

  fun isStackMode (mode: Mode.t) : bool =
    case mode of
      Mode.Stack => true
    | _ => false

  fun hasStackAlloc (Statement.T {exp, ...}) : bool =
    case exp of
      Exp.ConApp {mode, ...} => isStackMode mode
    | Exp.Tuple {mode, ...} => isStackMode mode
    | Exp.PrimApp {mode = mode_opt, ...} =>
        Option.isSome mode_opt andalso isStackMode (Option.valOf mode_opt)
    | _ => false

  fun transformFunction (func: Function.t) : Function.t =
    let
      val {args, blocks, mayInline, name, raises, returns, start} =
        Function.dest func
      val startBlockIdx = ref 0
      val _ = Vector.foreachi (blocks, fn (i, block) =>
        if Label.equals (Block.label block, start) then (startBlockIdx := i; ())
        else ())

      val foundStackAlloc = ref false
      fun allocSearch (block: Block.t) : unit -> unit =
        let
          val stmts = Block.statements block
          val transfer = Block.transfer block
          val _ = Vector.foreach (stmts, fn stmt =>
            if hasStackAlloc stmt then
              (foundStackAlloc := true; raise AllocFound)
            else
              ())
          val _ =
            case transfer of
              Transfer.Call {func, ...} =>
                if doesFunctionAllocateInCaller func then
                  (foundStackAlloc := true; raise AllocFound)
                else
                  ()
            | _ => ()
        in
          fn () => ()
        end

      fun insertPushesAndPops (block: Block.t) : Block.t =
        let
          val transfer = Block.transfer block
          val stmts = Block.statements block
          val hasRegionPop = Vector.exists (stmts, fn stmt =>
            case Statement.exp stmt of
              Exp.PrimApp {prim, ...} => Prim.equals (prim, Prim.Region_pop)
            | _ => false)
          val stmts = Vector.toList (stmts)
          val label = Block.label block
          val args = Block.args block
          val stmts =
            if Label.equals (label, start) then
              regionPushStmt :: stmts
            else
              stmts

          val regionPopBlock = Block.T
            { args = args
            , label = label
            , statements = Vector.fromList (stmts @ [regionPopStmt])
            , transfer = transfer
            }
          val defaultBlock = Block.T
            { args = args
            , label = label
            , statements = Vector.fromList stmts
            , transfer = transfer
            }
        in
          if hasRegionPop then
            defaultBlock
          else
            case transfer of
              Transfer.Bug => defaultBlock
            | Transfer.Call _ => defaultBlock
            | Transfer.Case _ => defaultBlock
            | Transfer.Goto _ => defaultBlock
            | Transfer.Runtime _ => defaultBlock
            | Transfer.Raise _ => regionPopBlock
            | Transfer.Return _ => regionPopBlock
        end

      val _ = Function.dfs (func, allocSearch) handle AllocFound => ()
    in
      if !foundStackAlloc then
        let
          val blocks = Vector.map (blocks, insertPushesAndPops)
        in
          Function.new
            { args = args
            , blocks = blocks
            , mayInline = mayInline
            , name = name
            , raises = raises
            , returns = returns
            , start = start
            }
        end
      else
        func
    end

  fun functionAllocatesInCaller (func: Function.t) : unit =
    let
      val {get = getVarMode, set = setVarMode, ...} =
        Property.getSetOnce (Var.plist, Property.initConst NONE)

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
                    if
                      Option.isSome mode
                      andalso Mode.equals (Option.valOf mode, Mode.Stack)
                    then allocatesInCaller := true
                    else ()
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
        { datatypes = datatypes
        , globals = globals
        , functions = List.map (functions, transformFunction)
        , main = main
        }
    end
end
