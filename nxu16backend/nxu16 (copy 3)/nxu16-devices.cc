/* Copyright (C) 2009-2024 Free Software Foundation, Inc.
   Contributed by Anatoly Sokolov (aesok@post.ru)

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

#ifndef IN_GEN_nxu16_MMCU_TEXI
#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "diagnostic.h"
#endif /* IN_GEN_nxu16_MMCU_TEXI */

#include "nxu16-arch.h"

/* List of all known nxu16 MCU architectures.
   Order as of enum nxu16_arch from nxu16.h.  */

const nxu16_arch_t
nxu16_arch_types[] =
{
  /* unknown device specified */
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x0060, 0,      32, NULL, nxu16_MMCU_DEFAULT },
  /*
    A  M  J  LM E  E  E  X  R  T  d S     FPO     S O   A
    S  U  M  PO L  L  I  M  A  I  a t     lMff    F ff  r
    M  L  P  MV P  P  J  E  M  N  t a     a s     R s   c
             XW M  M  M  G  P  Y  a r     s e       e   h
                   X  P  A  D       t     h t       t   ID   */
  { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x0060, 0,      32, "1",   "nxu161"  },
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x0060, 0,      32, "2",   "nxu162"  },
  { 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0x0060, 0,      32, "25",  "nxu1625" },
  { 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0x0060, 0,      32, "3",   "nxu163"  },
  { 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0x0060, 0,      32, "31",  "nxu1631" },
  { 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0x0060, 0,      32, "35",  "nxu1635" },
  { 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0x0060, 0,      32, "4",   "nxu164"  },
  { 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0x0060, 0,      32, "5",   "nxu165"  },
  { 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0x0060, 0,      32, "51",  "nxu1651" },
  { 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0x0060, 0,      32, "6",   "nxu166"  },

  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0x0040, 0x4000, 0, "100", "nxu16tiny" },
  { 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0x2000, 0,      0, "102", "nxu16xmega2" },
  { 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0x2000, 0x8000, 0, "103", "nxu16xmega3" },
  { 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0x2000, 0,      0, "104", "nxu16xmega4" },
  { 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0x2000, 0,      0, "105", "nxu16xmega5" },
  { 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0x2000, 0,      0, "106", "nxu16xmega6" },
  { 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0x2000, 0,      0, "107", "nxu16xmega7" }
};

const nxu16_arch_info_t
nxu16_texinfo[] =
{
  { ARCH_nxu161,
    "This ISA is implemented by the minimal nxu16 core and supported "
    "for assembler only." },
  { ARCH_nxu162,
    "``Classic'' devices with up to 8@tie{}KiB of program memory." },
  { ARCH_nxu1625,
    "``Classic'' devices with up to 8@tie{}KiB of program memory and with "
    "the @code{MOVW} instruction." },
  { ARCH_nxu163,
    "``Classic'' devices with 16@tie{}KiB up to 64@tie{}KiB of "
    "program memory." },
  { ARCH_nxu1631,
    "``Classic'' devices with 128@tie{}KiB of program memory." },
  { ARCH_nxu1635,
    "``Classic'' devices with 16@tie{}KiB up to 64@tie{}KiB of "
    "program memory and with the @code{MOVW} instruction." },
  { ARCH_nxu164,
    "``Enhanced'' devices with up to 8@tie{}KiB of program memory." },
  { ARCH_nxu165,
    "``Enhanced'' devices with 16@tie{}KiB up to 64@tie{}KiB of "
    "program memory." },
  { ARCH_nxu1651,
    "``Enhanced'' devices with 128@tie{}KiB of program memory." },
  { ARCH_nxu166,
    "``Enhanced'' devices with 3-byte PC, i.e.@: with more than 128@tie{}KiB "
    "of program memory." },
  { ARCH_nxu16TINY,
    "``Reduced Tiny'' Tiny core devices with only 16 general purpose "
    "registers and 512@tie{}B up to 4@tie{}KiB of program memory." },
  { ARCH_nxu16XMEGA2,
    "``XMEGA'' devices with more than 8@tie{}KiB and up to 64@tie{}KiB "
    "of program memory." },
  { ARCH_nxu16XMEGA3,
    "``XMEGA'' devices with up to 64@tie{}KiB of combined program memory "
    "and RAM, and with program memory visible in the RAM address space." },
  { ARCH_nxu16XMEGA4,
    "``XMEGA'' devices with more than 64@tie{}KiB and up to 128@tie{}KiB "
    "of program memory." },
  { ARCH_nxu16XMEGA5,
    "``XMEGA'' devices with more than 64@tie{}KiB and up to 128@tie{}KiB "
    "of program memory and more than 64@tie{}KiB of RAM." },
  { ARCH_nxu16XMEGA6,
    "``XMEGA'' devices with more than 128@tie{}KiB of program memory." },
  { ARCH_nxu16XMEGA7,
    "``XMEGA'' devices with more than 128@tie{}KiB of program memory "
    "and more than 64@tie{}KiB of RAM." }
};

const nxu16_mcu_t
nxu16_mcu_types[] =
{
#define nxu16_MCU(NAME, ARCH, DEV_ATTRIBUTE, MACRO, DATA_SEC, TEXT_SEC, FLASH_SIZE, PMOFF) \
  { NAME, ARCH, DEV_ATTRIBUTE, MACRO, DATA_SEC, TEXT_SEC, FLASH_SIZE, PMOFF },
#include "nxu16-mcus.def"
#undef nxu16_MCU
    /* End of list.  */
  { NULL, ARCH_UNKNOWN, nxu16_ISA_NONE, NULL, 0, 0, 0, 0 }
};




#ifndef IN_GEN_nxu16_MMCU_TEXI

static char *
nxu16_archs_str (void)
{
  char *archs = concat ("", NULL);

  // Build of core architectures' names.

  for (const nxu16_mcu_t *mcu = nxu16_mcu_types; mcu->name; mcu++)
    if (!mcu->macro)
      archs = concat (archs, " ", nxu16_arch_types[mcu->arch_id].name, NULL);

  return archs;
}


void
nxu16_inform_core_architectures (void)
{
  char *archs = nxu16_archs_str ();
  inform (input_location, "supported core architectures:%s", archs);
  free (archs);
}


/* When MCU names a core arch like "nxu165", then return a pointer to the
   respective entry in nxu16_arch_types[].  Otherwise, return NULL.  */

const nxu16_arch_t *
nxu16_get_parch (const char *mcu)
{
  for (size_t i = 0; i < ARRAY_SIZE (nxu16_arch_types); ++i)
    {
      if (strcmp (mcu, nxu16_arch_types[i].name) == 0)
	return & nxu16_arch_types[i];
    }

  return NULL;
}

#endif // IN_GEN_nxu16_MMCU_TEXI
