/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "mov[ \\t]+r2,r1.*sra[ \\t]+r2,7.*mov[ \\t]+r3,r2" } } */

/* The high byte must be copied outwards.  Reversing the final MOV leaves
   byte 3 stale and corrupts far-pointer arithmetic using a signed index.  */
long
extend_word (int value)
{
  return value;
}
