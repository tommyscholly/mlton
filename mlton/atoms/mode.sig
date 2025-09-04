signature MODE_STRUCTS =
   sig
   end

signature MODE =
   sig
      datatype t = Stack | Heap | Undetermined
      val layout: t -> Layout.t
      val equals: t * t -> bool
      val join: t * t -> t
      val subsumes: t * t -> bool
   end
