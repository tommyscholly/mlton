void GC_regionPush (GC_state s) {
  struct RegionStackNode* node = (struct RegionStackNode*) malloc(sizeof(struct RegionStackNode));
  if (node == NULL)
    die ("Cannot allocate region stack node");


  // printf("Current regionStack %p ", s->regionStack);
  // printf("Current regionTop %p\n", s->regionTop);
  node->partitionStart = s->regionTop;
  node->next = s->regionStack;
  s->regionStack = node;
}

void GC_regionPop (GC_state s) {
  if (s->regionStack == NULL) {
    // printf ("Region stack underflow");
    return;
  }

  pointer oldTop = s->regionTop;
  pointer newTop = s->regionStack->partitionStart;

  // if (oldTop > newTop)
  //   printf("RegionPop reclaiming %zu bytes (from %p down to %p)\n",
  //          (size_t)(oldTop - newTop), oldTop, newTop);
  // else
  //   printf("RegionPop with no new allocations (top at %p)\n", oldTop);

  s->regionTop = newTop;
  // printf("RegionTop after pop %p\n", s->regionTop);

  struct RegionStackNode* temp = s->regionStack;
  s->regionStack = s->regionStack->next;
  free(temp);
}
