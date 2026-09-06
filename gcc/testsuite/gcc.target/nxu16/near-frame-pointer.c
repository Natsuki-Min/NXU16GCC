/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-final { scan-assembler "\\tpush[ \\t]+er14" } } */
/* { dg-final { scan-assembler "\\tmov[ \\t]+er14,sp" } } */
/* { dg-final { scan-assembler-not "\\tmov[ \\t]+er12,sp" } } */

unsigned int
local_word (unsigned int value)
{
  volatile unsigned int slot = value;
  return slot + 1U;
}
