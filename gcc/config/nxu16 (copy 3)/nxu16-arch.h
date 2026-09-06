/* Device information for nxu16 8-bit microcontrollers.
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

#ifndef nxu16_ARCH_H
#define nxu16_ARCH_H

#define nxu16_MMCU_DEFAULT "nxu162"

/* This enum supplies indices into the nxu16_arch_types[] table below. */

enum nxu16_arch_id
{
  ARCH_UNKNOWN,
  ARCH_nxu161,
  ARCH_nxu162,
  ARCH_nxu1625,
  ARCH_nxu163,
  ARCH_nxu1631,
  ARCH_nxu1635,
  ARCH_nxu164,
  ARCH_nxu165,
  ARCH_nxu1651,
  ARCH_nxu166,
  ARCH_nxu16TINY,
  ARCH_nxu16XMEGA2,
  ARCH_nxu16XMEGA3,
  ARCH_nxu16XMEGA4,
  ARCH_nxu16XMEGA5,
  ARCH_nxu16XMEGA6,
  ARCH_nxu16XMEGA7
};


/* Architecture-specific properties.  */

typedef struct
{
  /* Assembler only.  */
  int asm_only;

  /* Core have 'MUL*' instructions.  */
  int have_mul;

  /* Core have 'CALL' and 'JMP' instructions.  */
  int have_jmp_call;

  /* Core have 'MOVW' and 'LPM Rx,Z' instructions.  */
  int have_movw_lpmx;

  /* Core have 'ELPM' instructions.  */
  int have_elpm;

  /* Core have 'ELPM Rx,Z' instructions.  */
  int have_elpmx;

  /* Core have 'EICALL' and 'EIJMP' instructions.  */
  int have_eijmp_eicall;

  /* This is an XMEGA core.  */
  int xmega_p;

  /* This core has the RAMPD special function register
     and thus also the RAMPX, RAMPY and RAMPZ registers.  */
  int have_rampd;

  /* This is a TINY core. */
  int tiny_p;

  /* Default start of data section address for architecture.  */
  int default_data_section_start;

  /* Offset where flash memory is seen in RAM address range or 0.  */
  int flash_pm_offset;

  /* Offset between SFR address and RAM address:
     SFR-address = RAM-address - sfr_offset  */
  int sfr_offset;

  /* Architecture id to built-in define __nxu16_ARCH__ (NULL -> no macro) */
  const char *const macro;

  /* Architecture name.  */
  const char *const name;
} nxu16_arch_t;


/* Device-specific properties.  */

typedef struct
{
  /* Device name.  */
  const char *const name;

  /* Index in nxu16_arch_types[].  */
  enum nxu16_arch_id arch_id;

  /* device specific feature */
  int dev_attribute;

  /* Must lie outside user's namespace.  NULL == no macro.  */
  const char *const macro;

  /* Start of data section.  */
  int data_section_start;

  /* Start of text section. */
  int text_section_start;

  /* Flash size in bytes.  */
  int flash_size;

  /* Offset where flash is seen in the RAM address space.  */
  int flash_pm_offset;
} nxu16_mcu_t;

/* nxu16 device specific features.

nxu16_ISA_RMW
  Only few nxu16 devices have Read-Modify-Write (RMW) instructions
  (XCH, LAC, LAS and LAT)

nxu16_SHORT_SP
  Stack Pointer has only 8 bit width.
  The device / multilib has an 8-bit stack pointer (no SPH).

nxu16_ERRATA_SKIP
  Some nxu16 devices have a core erratum when skipping a 2-word instruction.
  Skip instructions are:  SBRC, SBRS, SBIC, SBIS, CPSE.
  Problems will occur with return address is IRQ executes during the
  skip sequence.

  A support ticket from Atmel returned the following information:

     Subject: (ATTicket:644469) On nxu16 skip-bug core Erratum
     From: nxu16@atmel.com                    Date: 2011-07-27
     (Please keep the subject when replying to this mail)

     This errata exists only in AT90S8515 and ATmega103 devices.

     For information please refer the following respective errata links
       http://www.atmel.com/dyn/resources/prod_documents/doc2494.pdf
       http://www.atmel.com/dyn/resources/prod_documents/doc1436.pdf

nxu16_ISA_RCALL
  Always use RJMP / RCALL and assume JMP / CALL are not available.
  This affects multilib selection via specs generation and -mshort-calls.
  Even if a device like ATtiny417 from nxu16xmega3 supports JMP / CALL, we
  assume these instructions are not available and we set the built-in
  macro __nxu16_HAVE_JMP_CALL__ accordingly.  This macro is used to
  determine a rough estimate of flash size in libgcc, and nxu16-LibC uses
  this macro to determine vector sizes.

nxu16_ISA_FLMAP
  The device has the NVMCTRL_CTRLB.FLMAP bitfield.  The value of FLMAP
  determines which 32 KiB segment of the program memory (flash) is visible
  in the RAM address space at 0x8000.

  If Binutils support emulations nxu16xmega2_flmap resp. nxu16xmega4_flmap
  (PR31124), then  the location of the .rodata section can be determined
  by means of option -m[no-]rodata-in-ram.  If .rodata is located in flash,
  the user can chose which 32 KiB flash block is visible in RAM space by
  means of defining symbol __flmap.

  The startup code from nxu16-LibC initializes FLMAP according to __flmap
  provided one of the nxu16xmega*_flmap emulations is used. If nxu16xmega2/4
  is used, then the startup code does not initialize FLMAP.

  __nxu16_HAVE_FLMAP__ is a macro defined in device-specs and supposed to be
  consumed by code that sets FLMAP, like the startup code for example.
  The macro is defined when all of the following conditions are met:
    * The device is nxu16_ISA_FLMAP.
    * It's not known at compile time / assembler time whether or not .rodata
      will be located in flash or in RAM. This implies Binutils PR31124.
    * The definition of the macro is independent of -m[no-]rodata-in-ram.

  nxu16_ISA_FLMAP does not affect multilib layout or selection in any way.

  For details on which symbols are defined in which way depending on the
  emulation, see <Binutils>/ld/scripttempl/nxu16.sc.  */

enum nxu16_device_specific_features
{
  nxu16_ISA_NONE,
  nxu16_ISA_RMW     = 0x1, /* device has RMW instructions. */
  nxu16_SHORT_SP    = 0x2, /* Stack Pointer has 8 bits width. */
  nxu16_ERRATA_SKIP = 0x4, /* device has a core erratum. */
  nxu16_ISA_LDS     = 0x8, /* whether LDS / STS is valid for all data in static
			    storage.  Only useful for reduced Tiny.	 */
  nxu16_ISA_RCALL	  = 0x10, /* Use RJMP / RCALL even though JMP / CALL
			     are available (-mshort-calls).	 */
  nxu16_ISA_FLMAP	  = 0x20  /* Has NVMCTRL_CTRLB.FLMAP to select which 32 KiB
			     block of program memory is visible in the RAM
			     address space.	 */
};

/* Map architecture to its texinfo string.  */

typedef struct
{
  /* Architecture ID.  */
  enum nxu16_arch_id arch_id;

  /* textinfo source to describe the architecture.  */
  const char *texinfo;
} nxu16_arch_info_t;

/* Preprocessor macros to define depending on MCU type.  */

extern const nxu16_arch_t nxu16_arch_types[];
extern const nxu16_arch_t *nxu16_arch;
extern const nxu16_arch_t *nxu16_get_parch (const char *mcu);

extern const nxu16_mcu_t nxu16_mcu_types[];

extern void nxu16_inform_core_architectures (void);

#endif /* nxu16_ARCH_H */
