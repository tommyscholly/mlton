MLton Objects
- Normal Objects: [Header : Field0 | Field1 | Field2]
- Sequence Objects: [length : Header : [field0 | field1] | [field0 | field1]]
                                               ^                   ^
                                            element1            element2


Headers are 64 bits, the low bit is always 1
- Header is defined in `gc/object.h`

Heap
- 1 contiguous heap
- see gc/heap.h
- the card map loosely partitions the old generation
- the cross map locates elements within a marked card map

Allocating a buffer in the runtime
Look at init.c, then initWorld, then GC\_mmapAnon 
