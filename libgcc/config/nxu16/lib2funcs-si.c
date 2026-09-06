/* Freestanding 32-bit integer helpers for the nX-U16 ABI.  Keep these in a
   separate archive member so 16-bit-only programs do not pull them in.  */

typedef int HItype __attribute__ ((mode (HI)));
typedef unsigned int UHItype __attribute__ ((mode (HI)));
typedef int SItype __attribute__ ((mode (SI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef unsigned int UQItype __attribute__ ((mode (QI)));

USItype __mulsi3 (USItype, USItype);
USItype __udivsi3 (USItype, USItype);
USItype __umodsi3 (USItype, USItype);
SItype __divsi3 (SItype, SItype);
SItype __modsi3 (SItype, SItype);
HItype __clzsi2 (USItype);

typedef union
{
  USItype value;
  UQItype byte[4];
} SIunion;

static USItype
lshrsi1 (USItype value)
{
  SIunion in;
  SIunion out;

  in.value = value;
  out.byte[0] = (in.byte[0] >> 1) | (in.byte[1] << 7);
  out.byte[1] = (in.byte[1] >> 1) | (in.byte[2] << 7);
  out.byte[2] = (in.byte[2] >> 1) | (in.byte[3] << 7);
  out.byte[3] = in.byte[3] >> 1;
  return out.value;
}

USItype
__mulsi3 (USItype left, USItype right)
{
  SIunion a;
  SIunion b;
  SIunion result;
  UHItype carry;

  a.value = left;
  b.value = right;

  /* Byte convolution maps every product to the native 8x8 -> 16 MUL.
     Terms above byte three cannot affect the low 32-bit C result.  */
  carry = (UHItype) a.byte[0] * b.byte[0];
  result.byte[0] = carry;
  carry = (carry >> 8)
          + (UHItype) a.byte[0] * b.byte[1]
          + (UHItype) a.byte[1] * b.byte[0];
  result.byte[1] = carry;
  carry = (carry >> 8)
          + (UHItype) a.byte[0] * b.byte[2]
          + (UHItype) a.byte[1] * b.byte[1]
          + (UHItype) a.byte[2] * b.byte[0];
  result.byte[2] = carry;
  carry = (carry >> 8)
          + (UHItype) a.byte[0] * b.byte[3]
          + (UHItype) a.byte[1] * b.byte[2]
          + (UHItype) a.byte[2] * b.byte[1]
          + (UHItype) a.byte[3] * b.byte[0];
  result.byte[3] = carry;
  return result.value;
}

HItype
__clzsi2 (USItype value)
{
  SIunion in;
  UQItype byte;
  HItype count;

  in.value = value;
  if (in.byte[3] != 0)
    byte = in.byte[3], count = 0;
  else if (in.byte[2] != 0)
    byte = in.byte[2], count = 8;
  else if (in.byte[1] != 0)
    byte = in.byte[1], count = 16;
  else
    byte = in.byte[0], count = 24;

  if (byte == 0)
    return 32;
  while ((byte & 0x80u) == 0)
    byte <<= 1, ++count;
  return count;
}

static USItype
udivmodsi4 (USItype dividend, USItype divisor, USItype *remainder)
{
  USItype quotient = 0;
  USItype bit = 1;

  if (divisor == 0)
    {
      if (remainder != 0)
	*remainder = dividend;
      return 0;
    }

  while (divisor < dividend && (divisor & 0x80000000UL) == 0)
    {
      divisor += divisor;
      bit += bit;
    }

  while (bit != 0)
    {
      if (dividend >= divisor)
	{
	  dividend -= divisor;
	  quotient |= bit;
	}
      divisor = lshrsi1 (divisor);
      bit = lshrsi1 (bit);
    }

  if (remainder != 0)
    *remainder = dividend;
  return quotient;
}

USItype
__udivsi3 (USItype dividend, USItype divisor)
{
  return udivmodsi4 (dividend, divisor, 0);
}

USItype
__umodsi3 (USItype dividend, USItype divisor)
{
  USItype remainder;
  udivmodsi4 (dividend, divisor, &remainder);
  return remainder;
}

SItype
__divsi3 (SItype dividend, SItype divisor)
{
  int negative = (dividend < 0) != (divisor < 0);
  USItype udividend = (dividend < 0
		       ? (USItype) 0 - (USItype) dividend
		       : (USItype) dividend);
  USItype udivisor = (divisor < 0
		      ? (USItype) 0 - (USItype) divisor
		      : (USItype) divisor);
  USItype quotient = udivmodsi4 (udividend, udivisor, 0);

  return negative ? (SItype) ((USItype) 0 - quotient) : (SItype) quotient;
}

SItype
__modsi3 (SItype dividend, SItype divisor)
{
  int negative = dividend < 0;
  USItype udividend = (dividend < 0
		       ? (USItype) 0 - (USItype) dividend
		       : (USItype) dividend);
  USItype udivisor = (divisor < 0
		      ? (USItype) 0 - (USItype) divisor
		      : (USItype) divisor);
  USItype remainder;

  udivmodsi4 (udividend, udivisor, &remainder);
  return negative ? (SItype) ((USItype) 0 - remainder) : (SItype) remainder;
}
