OUTPUT_FORMAT("elf32-nxu16", "elf32-nxu16", "elf32-nxu16")
OUTPUT_ARCH(nxu16)
ENTRY(_start)

MEMORY
{
  ROM (rx)  : ORIGIN = 0x0000, LENGTH = 0x40000
  RAM (rwx) : ORIGIN = 0xd000, LENGTH = 0x2000
}

SECTIONS
{
  .vectors 0x0000 :
  {
    KEEP(*(.vectors))
  } > ROM

  /* Reset entry remains fixed.  Near read-only data is placed immediately
     afterwards so it has priority over ordinary code in the ClassWiz
     segment-zero ROM window.  */
  .text.startup 0x0100 :
  {
    *(.text.startup)
    *(.text.startup.*)
  } > ROM

  .rodata :
  {
    *(.rodata)
    *(.rodata.*)
    . = ALIGN(2);
    __preinit_array_start = .;
    KEEP(*(.preinit_array))
    __preinit_array_end = .;
    __init_array_start = .;
    KEEP(*(SORT_BY_INIT_PRIORITY(.init_array.*)))
    KEEP(*(.init_array))
    __init_array_end = .;
    __fini_array_start = .;
    KEEP(*(SORT_BY_INIT_PRIORITY(.fini_array.*)))
    KEEP(*(.fini_array))
    __fini_array_end = .;
  } > ROM

  .text :
  {
    *(.text)
    *(.text.*)
  } > ROM

  /* Explicit high-ROM regions used by firmware that needs deterministic
     bank placement.  Ordinary -ffunction-sections code remains bank-packed
     by the NXU16 linker emulation.  */
  .bank1 0x10000 :
  {
    KEEP(*(.bank1))
    KEEP(*(.bank1.*))
  } > ROM

  .far_rodata 0x11000 :
  {
    KEEP(*(.far_rodata))
    KEEP(*(.far_rodata.*))
  } > ROM

  .rom_data :
  {
    . = ALIGN(2);
    __rom_data_start = .;
  } > ROM

  .data 0xd000 : AT(__rom_data_start)
  {
    __data_start = .;
    *(.data)
    *(.data.*)
    . = ALIGN(2);
    __data_end = .;
  } > RAM
  __data_load_start = LOADADDR(.data);

  .bss (NOLOAD) :
  {
    __bss_start = .;
    *(.bss)
    *(.bss.*)
    *(COMMON)
    . = ALIGN(2);
    __bss_end = .;
  } > RAM

  .noinit (NOLOAD) :
  {
    __noinit_start = .;
    *(.noinit)
    *(.noinit.*)
    . = ALIGN(2);
    __noinit_end = .;
  } > RAM

  __heap_start = ALIGN(__noinit_end, 2);
  __heap_end = 0xe000;
  __stack_bottom = 0xe000;
  __stack_top = ORIGIN(RAM) + LENGTH(RAM);

  ASSERT(__rom_data_start + SIZEOF(.data) <= ORIGIN(ROM) + LENGTH(ROM),
         "ROM image exceeds flash")
  ASSERT(ADDR(.rodata) + SIZEOF(.rodata) <= ORIGIN(RAM),
         "near read-only data exceeds the ClassWiz ROM window")
  ASSERT(ADDR(.text) + SIZEOF(.text) <= 0x10000,
         "low ROM text overlaps bank 1")
  ASSERT(__heap_start <= __heap_end, "static data leaves no heap")

  /DISCARD/ :
  {
    *(.comment)
    *(.note*)
    *(.eh_frame*)
  }
}
