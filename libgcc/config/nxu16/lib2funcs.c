/* Freestanding integer helpers for the nX-U16 ABI.  Keep these algorithms
   expressed in addition, subtraction and shifts so their own definitions do
   not recursively call the multiply and divide libcalls they implement.  */

typedef int HItype __attribute__ ((mode (HI)));
typedef unsigned int UHItype __attribute__ ((mode (HI)));

UHItype __mulhi3 (UHItype, UHItype);
UHItype __udivhi3 (UHItype, UHItype);
UHItype __umodhi3 (UHItype, UHItype);
HItype __divhi3 (HItype, HItype);
HItype __modhi3 (HItype, HItype);

UHItype
__mulhi3 (UHItype left, UHItype right)
{
  UHItype result = 0;

  while (right != 0)
    {
      if (right & 1)
	result += left;
      left <<= 1;
      right >>= 1;
    }
  return result;
}

static UHItype
udivmodhi4 (UHItype dividend, UHItype divisor, UHItype *remainder)
{
  UHItype quotient = 0;
  UHItype bit = 1;

  if (divisor == 0)
    {
      if (remainder != 0)
	*remainder = dividend;
      return 0;
    }

  while (divisor < dividend && (divisor & 0x8000u) == 0)
    {
      divisor <<= 1;
      bit <<= 1;
    }

  while (bit != 0)
    {
      if (dividend >= divisor)
	{
	  dividend -= divisor;
	  quotient |= bit;
	}
      divisor >>= 1;
      bit >>= 1;
    }

  if (remainder != 0)
    *remainder = dividend;
  return quotient;
}

UHItype
__udivhi3 (UHItype dividend, UHItype divisor)
{
  return udivmodhi4 (dividend, divisor, 0);
}

UHItype
__umodhi3 (UHItype dividend, UHItype divisor)
{
  UHItype remainder;
  udivmodhi4 (dividend, divisor, &remainder);
  return remainder;
}

HItype
__divhi3 (HItype dividend, HItype divisor)
{
  int negative = (dividend < 0) != (divisor < 0);
  UHItype udividend = (dividend < 0
		       ? (UHItype) 0 - (UHItype) dividend
		       : (UHItype) dividend);
  UHItype udivisor = (divisor < 0
		      ? (UHItype) 0 - (UHItype) divisor
		      : (UHItype) divisor);
  UHItype quotient = udivmodhi4 (udividend, udivisor, 0);

  return negative ? (HItype) ((UHItype) 0 - quotient) : (HItype) quotient;
}

HItype
__modhi3 (HItype dividend, HItype divisor)
{
  int negative = dividend < 0;
  UHItype udividend = (dividend < 0
		       ? (UHItype) 0 - (UHItype) dividend
		       : (UHItype) dividend);
  UHItype udivisor = (divisor < 0
		      ? (UHItype) 0 - (UHItype) divisor
		      : (UHItype) divisor);
  UHItype remainder;

  udivmodhi4 (udividend, udivisor, &remainder);
  return negative ? (HItype) ((UHItype) 0 - remainder) : (HItype) remainder;
}
