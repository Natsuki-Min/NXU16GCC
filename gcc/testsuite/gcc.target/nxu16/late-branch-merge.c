/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-final { scan-assembler-times "\\tcmp[ \\t]" 3 } } */

extern unsigned char heap_start[];
extern unsigned char heap_end[];

/* Keep both range checks distinct.  A fused compare-and-branch must describe
   the comparison directly in its PC expression; otherwise jump2 can mistake
   these branches for equivalent uses of an old condition-code value.  */
unsigned char *
allocate (int increment, unsigned char *current)
{
  if (increment < 0)
    {
      if ((unsigned int) (current - heap_start)
	  < (unsigned int) -increment)
	return 0;
    }
  else if ((unsigned int) (heap_end - current)
	   < (unsigned int) increment)
    return 0;

  return current + increment;
}
