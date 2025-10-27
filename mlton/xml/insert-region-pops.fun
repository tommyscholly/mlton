functor RegionPops(S: REGION_POPS_STRUCTS): REGION_POPS =
struct

  open S
  open X

  (* in this pass, we are looking to insert regionPops in every function body,
   * before they return. we need to check that we have not inserted a 
   * regionPop already, in cases like exclave *)

  fun insertRegionPops (Program.T {body, datatypes}) =
    let val body = loopExp (body, false)
    in Program.T {body = body, datatypes = datatypes}
    end

  and loopDec (d: Dec.t) : Dec.t * bool =
    case d of
      Dec.MonoVal {exp, ty, mode, var} =>
        let val (p, hasPop) = checkPrimExpForPop exp
        in (Dec.MonoVal {exp = p, ty = ty, mode = mode, var = var}, hasPop)
        end
    | Dec.PolyVal {exp, ty, tyvars, mode, var} =>
        let val exp = loopExp (exp, false)
        in (Dec.PolyVal {exp = exp, ty = ty, tyvars = tyvars, mode = mode, var = var}, false)
        end
    | Dec.Fun {decs, tyvars} =>
        let val decs = Vector.map (decs, fn {lambda, ty, var} => {lambda = loopLambda lambda, ty = ty, var = var})
        in (Dec.Fun {decs = decs, tyvars = tyvars}, false)
        end
    | _ => (d, false)

  and loopExp (e: Exp.t, insertOverride: bool) : Exp.t =
    let
      val {decs, result} = Exp.dest e

      val shouldInsert = ref (if insertOverride then false else true)
      val decs = List.map (decs, fn d =>
        let
          val (d, hasPop) = loopDec d
          (* hasPop is true if there is a regionPop. that means we should not insert another one *)
          val _ = if hasPop then (shouldInsert := false; ()) else ()
        in
          d
        end)

      val decs =
        if !shouldInsert then
          let
            val prim = PrimExp.PrimApp {prim = Prim.Region_pop, args = Vector.new0 (), targs = Vector.new0 ()}
            val pop = Dec.MonoVal {exp = prim, ty = Type.unit, mode = Mode.Heap, var = Var.newString "pop"}
          in
            decs @ [pop]
          end
        else
          decs
    in
      Exp.make {decs = decs, result = result}
    end

  and checkPrimExpForPop (prim: PrimExp.t) : (PrimExp.t * bool) =
    case prim of
      PrimExp.PrimApp {prim = prim', ...} =>
        (case prim' of
           Prim.Region_pop => (prim, true)
         | _ => (prim, false))
    | PrimExp.Case {cases, default, test} =>
        let
          val cases = Cases.map (cases, fn caseExp => loopExp (caseExp, true))
          val default = Option.map (default, fn defaultExp => loopExp (defaultExp, true))
        in
          (PrimExp.Case {cases = cases, default = default, test = test}, false)
        end
    | _ => (prim, false)

  and loopLambda (l: Lambda.t) : Lambda.t =
    let
      val {arg, argType, argMode, lambdaMode, resultMode, body, mayInline} = Lambda.dest l
    in
      Lambda.make
        { arg = arg
        , argType = argType
        , argMode = argMode
        , lambdaMode = lambdaMode
        , resultMode = resultMode
        , body = loopExp (body, false)
        , mayInline = mayInline
        }
    end

end
