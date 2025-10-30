functor RemoveRegions(S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct
  open S

  structure Allocates =
  struct
    (* this links functions together that allocate on the stack, thus
     * preventing regions from being removed *)
    structure Lattice = TwoPointLattice (val bottom = "pureHeap" val top = "stackAlloc")

    open Lattice

    fun isStackAlloc (alloc: t) : bool = isTop alloc
    fun makeStackAlloc (alloc: t) = makeTop alloc
    val whenStackAlloc = addHandler
  end

  type region = {block_idx: int, inst_offset: int, has_stack_alloc: bool}
  type toFilter = {block_idx_start: int, inst_start: int, block_idx_end: int, inst_end: int}

  fun loopBlocks (blocks: Block.t vector) : Block.t vector =
    let
      val queue = ref []
      val filter_list = ref []

      fun loop (idx: int) : unit =
        let
          val block = Vector.sub (blocks, idx)
          val () = Vector.foreachi (Block.statements block, fn (i, s) =>
            let
              val Statement.T {exp, ...} = s

              fun checkMode (mode: Mode.t) : unit =
                case mode of
                  Mode.Stack =>
                    let val {block_int, inst_offset, has_stack_alloc} = List.pop (queue)
                    in List.push (queue, {block_int = block_int, inst_offset = inst_offset, has_stack_alloc = true})
                    end
                | _ => ()
            in
              case exp of
                Exp.PrimApp {prim, mode, ...} =>
                  (case prim of
                     Prim.Region_push => List.push (queue, {block_int = idx, inst_offset = i, has_stack_alloc = false})
                   | Prim.Region_pop =>
                       let
                         val {block_int, inst_offset, has_stack_alloc} = List.pop (queue)
                       in
                         if has_stack_alloc then
                           List.push
                             ( filter_list
                             , { block_idx_start = block_int
                               , inst_start = inst_offset
                               , block_idx_end = idx
                               , inst_end = i
                               }
                             )
                         else
                           ()
                       end

                   | _ =>
                       case mode of
                         SOME m => checkMode m
                       | _ => ())
              | Exp.ConApp {mode, ...} => checkMode mode
              | Exp.Tuple {mode, ...} => checkMode mode
              | _ => ()
            end)
        in
          ()
        end

      val () = Error.warning ("RemoveRegions.loopBlocks: length of queue is " ^ Int.toString (List.length (!queue)))
      val () = Error.warning
        ("RemoveRegions.loopBlocks: length of filter_list is " ^ Int.toString (List.length (!filter_list)))
      val () = Vector.foreachi (blocks, fn (idx, _) => loop idx)

      fun shouldFilter (block_idx: int, inst_idx: int) : bool =
        List.exists (!filter_list, fn {block_idx_start, inst_start, block_idx_end, inst_end} =>
          (block_idx > block_idx_start andalso block_idx < block_idx_end)
          orelse (block_idx = block_idx_start andalso inst_idx >= inst_start)
          orelse (block_idx = block_idx_end andalso inst_idx <= inst_end))

      fun filterBlock (block_idx: int, block: Block.t) : Block.t =
        let
          val Block.T {args, label, statements, transfer} = block
          val filtered_statements = Vector.keepAllMapi (statements, fn (i, s) =>
            if shouldFilter (block_idx, i) then NONE else SOME s)
        in
          Block.T {args = args, label = label, statements = filtered_statements, transfer = transfer}
        end
    in
      Vector.mapi (blocks, filterBlock)
    end


  fun transformFunction (func: Function.t) : Function.t =
    let
      val {args, blocks, mayInline, name, raises, returns, start} = Function.dest func
      val {get = labelAlloc: Label.t -> Allocates.t, set = setLabelAlloc: Label.t * Allocates.t -> unit, ...} =
        Property.getSetOnce (Label.plist, Property.initRaise ("Flatten.labelInfo", Label.layout))

      val _ = Function.dfs (func, fn (Block.T {label, transfer, statements, ...}) =>
        let
          val alloc_t = Allocates.new ()
          val _ = setLabelAlloc (label, alloc_t)
          val _ = Allocates.whenStackAlloc (alloc_t, fn () => ())
        in
          fn () => let (* this function executes after looping through the transfer blocks *) val () = () in () end
        end)
    in
      Function.new
        { args = args
        , blocks = loopBlocks blocks
        , mayInline = mayInline
        , name = name
        , raises = raises
        , returns = returns
        , start = start
        }
    end


  fun transform (Program.T {datatypes, globals, functions, main}) =
    Program.T
      {datatypes = datatypes, globals = globals, functions = List.map (functions, transformFunction), main = main}

end
