/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-final { scan-assembler "\\tpush[ \\t]+xr12" } } */
/* { dg-final { scan-assembler "\\tmov[ \\t]+er14,sp" } } */
/* { dg-final { scan-assembler "\\tl[ \\t]+er14,4\\[er14\\]" } } */
/* { dg-final { scan-assembler-not "\\tadd[ \\t]+er14,2" } } */

/* The third word argument is stacked.  SP is not an ISA memory base, so LRA
   copies it to ER14.  Its save-induced correction and the argument offset
   must be folded into one compact displaced load rather than an ADD/load
   pair.  */
void *
fill_bytes (void *destination, unsigned int value, unsigned int count)
{
  unsigned char *next = destination;

  while (count-- != 0)
    *next++ = value;
  return destination;
}
