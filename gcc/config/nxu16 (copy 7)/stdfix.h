/* Copyright (C) 2007-2024 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* ISO/IEC JTC1 SC22 WG14 N1169
 * Date: 2006-04-04
 * ISO/IEC TR 18037
 * Programming languages - C - Extensions to support embedded processors
 */

#ifndef _nxu16GCC_STDFIX_H
#define _nxu16GCC_STDFIX_H

/* 7.18a.1 Introduction.  */
/* 7.18a.3 Precision macros.  */

#include <stdfix-gcc.h>


#if __SIZEOF_INT__ == 2

typedef signed char int_hr_t;
typedef unsigned char uint_uhr_t;

typedef short int int_r_t;
typedef short unsigned int uint_ur_t;

typedef short int int_hk_t;
typedef short unsigned int uint_uhk_t;

typedef long int int_lr_t;
typedef long unsigned int uint_ulr_t;

typedef long int int_k_t;
typedef long unsigned int uint_uk_t;

typedef long long int int_llr_t;
typedef long long unsigned int uint_ullr_t;

typedef long long int int_lk_t;
typedef long long unsigned int uint_ulk_t;

typedef long long int int_llk_t;
typedef long long unsigned int uint_ullk_t;

#elif __SIZEOF_INT__ == 1  /*  -mint8  */

typedef signed char int_hr_t;
typedef unsigned char uint_uhr_t;

typedef long int int_r_t;
typedef long unsigned int uint_ur_t;

typedef long int int_hk_t;
typedef long unsigned int uint_uhk_t;

typedef long long int int_lr_t;
typedef long long unsigned int uint_ulr_t;

typedef long long int int_k_t;
typedef long long unsigned int uint_uk_t;

#endif /* __SIZEOF_INT__ == 1, 2 */


/* 7.18a.6  The fixed-point intrinsic functions.  */


/* 7.18a.6.2  The fixed-point absolute value functions.  */

#define abshr __builtin_nxu16_abshr
#define absr  __builtin_nxu16_absr
#define abslr __builtin_nxu16_abslr

#define abshk __builtin_nxu16_abshk
#define absk  __builtin_nxu16_absk

#if __SIZEOF_INT__ == 2

#define abslk  __builtin_nxu16_abslk
#define absllr __builtin_nxu16_absllr  /* GCC Extension */
#define absllk __builtin_nxu16_absllk  /* GCC Extension */

#endif /* sizeof (int) == 2 */


/* 7.18a.6.3  The fixed-point round functions.  */

/* The Embedded-C paper specifies results only for rounding points

       0 < RP < FBIT
  
   As an extension, the following functions work as expected
   with rounding points

       -IBIT < RP < FBIT
 
   For example, rounding an accum with a rounding point of -1 will
   result in an even integer value.  */

#define roundhr  __builtin_nxu16_roundhr
#define roundr   __builtin_nxu16_roundr
#define roundlr  __builtin_nxu16_roundlr

#define rounduhr __builtin_nxu16_rounduhr
#define roundur  __builtin_nxu16_roundur
#define roundulr __builtin_nxu16_roundulr

#define roundhk  __builtin_nxu16_roundhk
#define roundk   __builtin_nxu16_roundk

#define rounduhk __builtin_nxu16_rounduhk
#define rounduk  __builtin_nxu16_rounduk

#if __SIZEOF_INT__ == 2

#define roundlk   __builtin_nxu16_roundlk
#define roundulk  __builtin_nxu16_roundulk
#define roundllr  __builtin_nxu16_roundllr  /* GCC Extension */
#define roundullr __builtin_nxu16_roundullr /* GCC Extension */
#define roundllk  __builtin_nxu16_roundllk  /* GCC Extension */
#define roundullk __builtin_nxu16_roundullk /* GCC Extension */

#endif /* sizeof (int) == 2 */


/* 7.18a.6.4  The fixed-point bit countls functions.  */

#define countlshr  __builtin_nxu16_countlshr
#define countlsr   __builtin_nxu16_countlsr
#define countlslr  __builtin_nxu16_countlslr

#define countlsuhr __builtin_nxu16_countlsuhr
#define countlsur  __builtin_nxu16_countlsur
#define countlsulr __builtin_nxu16_countlsulr

#define countlshk  __builtin_nxu16_countlshk
#define countlsk   __builtin_nxu16_countlsk

#define countlsuhk __builtin_nxu16_countlsuhk
#define countlsuk  __builtin_nxu16_countlsuk

#if __SIZEOF_INT__ == 2

#define countlslk   __builtin_nxu16_countlslk
#define countlsulk  __builtin_nxu16_countlsulk
#define countlsllr  __builtin_nxu16_countlsllr  /* GCC Extension */
#define countlsullr __builtin_nxu16_countlsullr /* GCC Extension */
#define countlsllk  __builtin_nxu16_countlsllk  /* GCC Extension */
#define countlsullk __builtin_nxu16_countlsullk /* GCC Extension */

#endif /* sizeof (int) == 2 */


/* 7.18a.6.5  The bitwise fixed-point to integer conversion functions. */

#define bitshr  __builtin_nxu16_bitshr
#define bitsr   __builtin_nxu16_bitsr
#define bitslr  __builtin_nxu16_bitslr

#define bitsuhr __builtin_nxu16_bitsuhr
#define bitsur  __builtin_nxu16_bitsur
#define bitsulr __builtin_nxu16_bitsulr

#define bitshk  __builtin_nxu16_bitshk
#define bitsk   __builtin_nxu16_bitsk

#define bitsuhk __builtin_nxu16_bitsuhk
#define bitsuk  __builtin_nxu16_bitsuk

#if __SIZEOF_INT__ == 2

#define bitslk   __builtin_nxu16_bitslk
#define bitsulk  __builtin_nxu16_bitsulk
#define bitsllr  __builtin_nxu16_bitsllr  /* GCC Extension */
#define bitsullr __builtin_nxu16_bitsullr /* GCC Extension */
#define bitsllk  __builtin_nxu16_bitsllk  /* GCC Extension */
#define bitsullk __builtin_nxu16_bitsullk /* GCC Extension */

#endif /* sizeof (int) == 2 */


/* 7.18a.6.6  The bitwise integer to fixed-point conversion functions. */

#define hrbits  __builtin_nxu16_hrbits
#define rbits   __builtin_nxu16_rbits
#define lrbits  __builtin_nxu16_lrbits

#define uhrbits __builtin_nxu16_uhrbits
#define urbits  __builtin_nxu16_urbits
#define ulrbits __builtin_nxu16_ulrbits

#define hkbits  __builtin_nxu16_hkbits
#define kbits   __builtin_nxu16_kbits

#define uhkbits __builtin_nxu16_uhkbits
#define ukbits  __builtin_nxu16_ukbits

#if __SIZEOF_INT__ == 2

#define lkbits   __builtin_nxu16_lkbits
#define ulkbits  __builtin_nxu16_ulkbits
#define llrbits  __builtin_nxu16_llrbits  /* GCC Extension */
#define ullrbits __builtin_nxu16_ullrbits /* GCC Extension */
#define llkbits  __builtin_nxu16_llkbits  /* GCC Extension */
#define ullkbits __builtin_nxu16_ullkbits /* GCC Extension */

#endif /* sizeof (int) == 2 */


/* 7.18a.6.7  Type-generic fixed-point functions.  */

#define absfx     __builtin_nxu16_absfx
#define roundfx   __builtin_nxu16_roundfx
#define countlsfx __builtin_nxu16_countlsfx


/* Hook in stuff from nxu16-Libc.  */

#if (defined (__WITH_nxu16LIBC__)                 \
     && defined (__has_include)                 \
     && __has_include (<stdfix-nxu16libc.h>))
#include <stdfix-nxu16libc.h>
#endif

#endif /* _nxu16GCC_STDFIX_H */
