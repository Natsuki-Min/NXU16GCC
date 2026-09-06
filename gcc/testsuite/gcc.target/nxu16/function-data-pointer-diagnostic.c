/* { dg-do compile } */

void function (void) {}

/* Function pointers are far while ordinary data pointers are near.  This
   invalid implicit conversion must produce a normal diagnostic, not an ICE.  */
void *data_pointer = function; /* { dg-error "initialization from pointer to non-enclosed address space" } */
