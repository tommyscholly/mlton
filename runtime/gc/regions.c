void GC_regionPush (GC_state s) {
  // printf("Region pushing\n");

  struct RegionStackNode* node = (struct RegionStackNode*) malloc(sizeof(struct RegionStackNode));
  if (node == NULL)
    die ("Cannot allocate region stack node");
  node->partitionStart = s->regionTop;
  node->next = s->regionStack;
  s->regionStack = node;
}

void GC_regionPop (GC_state s) {
  // printf("Region popping\n");

  if (s->regionStack == NULL) {
    printf ("Region stack underflow");
    return;
  }

  if (s->regionTop > s->regionStack->partitionStart) {
    printf("We allocated something\n");
  }

  s->regionTop = s->regionStack->partitionStart;
  struct RegionStackNode* temp = s->regionStack;
  s->regionStack = s->regionStack->next;
  free(temp);
}
