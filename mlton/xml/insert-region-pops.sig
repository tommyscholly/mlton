(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature REGION_POPS_STRUCTS = 
   sig
      structure X: XML_TREE
   end

signature REGION_POPS = 
   sig
      include REGION_POPS_STRUCTS

      val insertRegionPops: X.Program.t -> X.Program.t
   end
