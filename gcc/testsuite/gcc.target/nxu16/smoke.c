/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "\\tmul[ \\t]" } } */
/* { dg-final { scan-assembler "\\t[bB][ \\t]+tail_target" } } */
/* { dg-final { scan-assembler "\\trti" } } */

/* Basic freestanding compiler smoke test for the nX-U16 port.  */

unsigned int
sum_words (const unsigned char *data, unsigned int length)
{
  unsigned int sum = 0;
  while (length-- != 0)
    sum += *data++;
  return sum;
}

unsigned int
mul_div (unsigned int a, unsigned int b)
{
  return (a * b) / (b | 1);
}

unsigned int
widening_multiply (unsigned char a, unsigned char b)
{
  return (unsigned int) a * (unsigned int) b;
}

__attribute__ ((noinline)) int
tail_target (int value)
{
  return value + 1;
}

int
tail_caller (int value)
{
  return tail_target (value);
}

unsigned int
invert (unsigned int value)
{
  return ~value;
}

int
variadic_sum (unsigned int count, ...)
{
  __builtin_va_list values;
  int sum = 0;
  __builtin_va_start (values, count);
  while (count-- != 0)
    sum += __builtin_va_arg (values, int);
  __builtin_va_end (values);
  return sum;
}

void __attribute__ ((interrupt))
timer_interrupt (void)
{
}
