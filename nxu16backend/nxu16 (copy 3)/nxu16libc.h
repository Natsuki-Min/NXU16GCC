/* Definitions for nxu16 8-bit microcontrollers if configured for nxu16-LibC.
   Copyright (C) 2012-2024 Free Software Foundation, Inc.
   Contributed by Georg-Johann Lay (nxu16@gjlay.de)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#undef  LIB_SPEC
#define LIB_SPEC                                \
  "%{!mmcu=nxu161:-lc} %(nxu16libc_devicelib)"

// nxu16-Libc implements functions from libgcc.a in libm.a, see PR54461.
// For a list of functions which are provided by libm.a and are
// omitted from libgcc.a see libgcc's t-nxu16libc.

#undef  LIBGCC_SPEC
#define LIBGCC_SPEC                             \
  "%{!mmcu=nxu161:-lgcc -lm}"

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC                          \
  " %(nxu16libc_startfile) "

#undef  LINK_GCC_C_SEQUENCE_SPEC
#define LINK_GCC_C_SEQUENCE_SPEC \
  "--start-group %G %{!nolibc:%L} --end-group"
