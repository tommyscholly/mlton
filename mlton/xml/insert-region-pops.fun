functor RegionPops(S: REGION_POPS_STRUCTS): REGION_POPS =
struct

  open S
  open X

  (* in this pass, we are looking to insert regionPops in every function body,
   * before they return. we need to check that we have not inserted a 
   * regionPop already, in cases like exclave *)

  fun insertRegionPops (Program.T {body, datatypes}) =
    let
      val regionPush = DirectExp.primApp ({args = Vector.new0 (), prim = Prim.Region_push, targs = Vector.new0 (), ty = Type.unit}, Mode.Heap)
      val regionVar = Var.newNoname ()
      val vall = DirectExp.vall {var = regionVar, exp = regionPush, mode = Mode.Heap}
      val {decs, result} = Exp.dest body
    (* val body = Exp.make {decs = vall @ decs, result = result} *)
    in
      Program.T {body = body, datatypes = datatypes}
    end

  fun loopExp (e: Exp.t) : Exp.t =
    let
      val {decs, result} = Exp.dest e

      val shouldInsert = ref true
      val _ = List.foreach (decs, fn d =>
        case d of
          Dec.MonoVal {exp, ...} => if checkPrimExpForPop exp then (shouldInsert := false; ()) else ()
        | _ => ())
      val decs =
        if !shouldInsert then
          let
            val regionPop = DirectExp.primApp ({args = Vector.new0 (), prim = Prim.Region_pop, targs = Vector.new0 (), ty = Type.unit}, Mode.Heap)
            val regionVar = Var.newNoname ()
            val vall = DirectExp.vall {var = regionVar, exp = regionPop, mode = Mode.Heap}
          in
            decs @ vall
          end
        else
          decs
    in
      Exp.make {decs = decs, result = result}
    end

  and checkPrimExpForPop (prim: PrimExp.t) : bool =
    case prim of
      PrimExp.PrimApp {prim, ...} =>
        (case prim of
           Prim.Region_pop => true
         | _ => false)
    | _ => false
end
