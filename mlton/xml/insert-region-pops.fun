functor RegionPops (S : REGION_POPS_STRUCTS) : REGION_POPS =
  struct

    open S
    open X

    (* in this pass, we are looking to insert regionPops in every function body,
     * before they return. we need to check that we have not inserted a 
     * regionPop already, in cases like exclave *)

    fun insertRegionPops (Program.T {body, datatypes}) =
      let

        val regionPush =
          DirectExp.primApp
            ( { args = Vector.new0 ()
              , prim = Prim.Region_push
              , targs = Vector.new0 ()
              , ty = Type.unit
              }
            , Mode.Heap
            )
        val regionVar = Var.newNoname ()
        val vall =
          DirectExp.vall {var = regionVar, exp = regionPush, mode = Mode.Heap}
        val {decs, result} = Exp.dest body
        (* val body = Exp.make {decs = vall @ decs, result = result} *)
      in
        Program.T {body = body, datatypes = datatypes}
      end

  (* fun loopExp e: Exp.t = *)
  (*    let *)
  (*       val {decs, result} = Exp.dest e *)
  (*    in *)
  (*       Exp.make {decs = List.map (decs, loopDec), result = result} *)
  (*    end *)

  end
