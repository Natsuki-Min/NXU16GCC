# NXU16 Picolibc runtime

This directory integrates Picolibc with the experimental NXU16 GCC and
Binutils ports. The former handwritten test libc has been removed; standard
headers and `libc.a` now come from the workspace's `picolibc/` tree.

The target support kept here is deliberately board-specific:

- reset vectors and startup code;
- the ClassWiz ROM/RAM linker layout;
- Picolibc `stdin`, `stdout`, and `stderr` backed by `__nxu16_putchar`;
- `_exit`, the debugger-visible exit status, and a stable exit loop;
- the compiler wrapper and optional CasioEmu MCP console adapter.

## Prerequisites

Build the in-tree NXU16 Binutils, GCC, and libgcc first. The current port is
based on Picolibc 1.8.12 commit
`036919b76d7124f25f1d82554a06c921708cd4b1` and requires Meson and Ninja.
The Makefile uses `../../.tools/meson/bin/meson` when present, otherwise it
uses `meson` from `PATH`.

## Build and use

```sh
make
bin/nxu16-gcc examples/hello.c -Os -o build/hello.elf
bin/nxu16-objcopy -O binary build/hello.elf build/hello.bin
```

The generated development sysroot is `../build-codex/sysroot`, beside the GCC
build that uses it. `bin/nxu16-gcc` selects that sysroot, `crt0.o`,
`nxu16-classwiz.ld`, `libc.a`, `libnxu16.a`, and libgcc. Set `NXU16_SYSROOT`
to use a packaged or alternate sysroot instead.

`bin/nxu16-picolibc-gcc` is only the bootstrap compiler used while constructing
Picolibc itself. Applications should always use `bin/nxu16-gcc`.

The default image starts at address zero. Its first three words hold the
initial stack pointer and reset entries; code starts at `0x0100`. ROM occupies
the full `0x00000..0x3ffff` flash space and RAM occupies `0xd000..0xefff`; the
heap ends at `0xe000`, and the stack begins at `0xf000` and grows down. The
script provides `.bank1` at `0x10000` and `.far_rodata` at `0x11000` for
deterministic cross-bank tests and firmware placement.

Near `.rodata`, including compiler-generated switch tables and constructor
arrays, is placed immediately after reset/startup code so it has priority in
the segment-zero ROM window. The link fails if that section reaches RAM at
`0xd000`. Ordinary code follows `.rodata`; explicitly far constants belong in
`.far_rodata` and must be accessed through `__far` pointers.

Ordinary C data pointers are 16-bit near pointers for segment-zero RAM and the
ROM window. Function pointers are far by default. An explicit `__far` data
pointer is a 32-bit container: the low word is the offset, the next byte is the
segment, and the top byte is padding. Dereferencing it emits the required
DSR-prefixed instruction. `__near` also explicitly names the 16-bit space.

GCC emits each function in its own section. The NXU16 linker packs function
sections into holes at the ends of lower banks before opening a later bank,
pads when necessary, rejects functions larger than 64 KiB, and diagnoses any
function that still crosses a bank boundary. A jump table stores the low
16-bit addresses of labels in that unsplit function. The table itself is in
segment-zero `.rodata`, so a high-bank function reads it with an ordinary near
load and `B ERn` supplies the function's current CSR.

## I/O and termination

The weak default `__nxu16_putchar` discards output. Firmware can override it:

```c
int __nxu16_putchar(int character)
{
    board_uart_write((unsigned char) character);
    return (unsigned char) character;
}
```

The MCP test links `src/mcp-console.c` and `src/debug-console.s`. This adapter
captures output in RAM and sends it to CasioEmu's debug port using `DSR 6`,
offset `0x0721`; it is not part of the default platform library.

Picolibc's fallback `sbrk` uses the linker's `__heap_start` and `__heap_end`.
Picolibc's `exit` eventually calls the NXU16 `_exit`, which stores the status
in `nxu16_exit_code` and stops at `__nxu16_exit_loop`.

## Profile and tests

This profile is single-threaded and freestanding. It builds Picolibc's library
families and libm, including POSIX and search code, with XDR as the one
target-specific omission. APIs that require an operating-system service still
need platform hooks before they can be used. The default printf/scanf variant
is Picolibc's upstream `minimal` formatter, which omits width, precision,
floating formatting, and `long long` I/O. The emulator test currently validates
basic integer `snprintf`, not the broader formatted-I/O surface or filesystem
streams.

```sh
make check
make check-mcp
```

`check` compiles and links tests at `-Os` and `-O0`. `check-mcp` executes both
images in CasioEmu and verifies startup, Picolibc headers, ctype, strings,
integer formatting, malloc/free, setjmp/longjmp, far flash reads, far function
pointers (aligned calls, unaligned calls, and an aligned tail jump), near
jump-table dispatch from both bank zero and bank one, the `.rodata` overflow
diagnostic, output, and termination.

See `../../doc/picolibc-port.md` for implementation details, current progress,
and known limitations.
