/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-final { scan-assembler "\\tl[ \\t]+er[0-9]+,2\\[er14\\]" } } */
/* { dg-final { scan-assembler-not "\\tadd[ \\t]+er14,2" } } */

struct pair
{
  unsigned int first;
  unsigned int second;
};

extern void mutate (struct pair *);

unsigned int
read_second (struct pair *pointer)
{
  mutate (pointer);
  return pointer->second;
}
