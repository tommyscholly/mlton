functor RemoveRegions(S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct
  open S

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
    in
      blocks
    end


  fun transformFunction (func: Function.t) : Function.t =
    let
      val {args, blocks, mayInline, name, raises, returns, start} = Function.dest func
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
      {datatypes = datatypes, globals = globals, functions = Vector.map (functions, transformFunction), main = main}

end
