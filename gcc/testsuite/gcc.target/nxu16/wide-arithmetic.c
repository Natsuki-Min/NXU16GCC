/* { dg-do assemble } */
/* { dg-options "-O2 -ffreestanding -fno-asynchronous-unwind-tables -fno-unwind-tables" } */

unsigned long
mul32 (unsigned long left, unsigned long right)
{
  return left * right;
}

unsigned long
div32 (unsigned long left, unsigned long right)
{
  return left / right;
}

unsigned long
mod32 (unsigned long left, unsigned long right)
{
  return left % right;
}

long
sdiv32 (long left, long right)
{
  return left / right;
}

long
smod32 (long left, long right)
{
  return left % right;
}

unsigned long
shl32 (unsigned long value, unsigned int count)
{
  return value << count;
}

unsigned long
shr32 (unsigned long value, unsigned int count)
{
  return value >> count;
}

long
sar32 (long value, unsigned int count)
{
  return value >> count;
}
