void GC_regionPush(GC_state s) {
  pointer oldBase = s->regionBase;
  pointer oldTop  = s->regionTop;

  // store previous base ptr right below the new base
  *(pointer *)oldTop = oldBase;

  // new region's base begins just after return ptr
  pointer newBase = oldTop + sizeof(pointer);

  s->regionBase = newBase;
  s->regionTop  = newBase;
}

void GC_regionPop(GC_state s) {
  if (s->regionTop == s->regionBuffer) {
    // underflow
    printf("underflow");
    return;
  }

  if (s->regionBase == s->regionBuffer) {
    s->regionTop = s->regionBuffer;
    return;
  }

  pointer currTop  = s->regionTop;
  pointer currBase = s->regionBase;

  // stored return ptr/base is one pointer *before* regionBase
  pointer newTop  = currBase - sizeof(pointer);
  pointer oldBase = *(pointer *)newTop;

  if (currTop > newTop) {
    size_t poppedBytes = (size_t)(currTop - newTop);
    s->cumulativeStatistics.bytesStackAllocated += poppedBytes;
  }

  {
    size_t liveBytes = (size_t)(currTop - s->regionBuffer);
    if (liveBytes > s->cumulativeStatistics.maxRegionBytesLive)
      s->cumulativeStatistics.maxRegionBytesLive = liveBytes;
  }

  s->regionTop  = newTop;
  s->regionBase = oldBase;
}
