# nX-U16 GCC Backend Status

The backend is experimental.  It can build an `xgcc` cross compiler and emit
freestanding C, but it is not yet a complete ABI or C++ runtime port.

## Known working paths

- Basic integer C compilation.
- Loads, stores, comparisons, loops, calls, and unsigned byte-to-word
  hardware multiply.
- Direct sibling calls and zero-argument indirect sibling calls; indirect
  targets are copied to the call-clobbered `XR0` slot before the epilogue,
  while interrupt handlers retain their `RTI` epilogue.
- The `interrupt` function-type attribute, including diagnostics requiring a
  `void (void)` function and an `RTI` epilogue.
- Basic variadic expansion in compiler smoke tests.
- Register and stack arguments, including `stdarg`, exercised end to end by
  the `nxu16-runtime` MCP test at `-O0` and `-Os`.
- A split pointer ABI: ordinary data pointers are 16-bit near pointers for
  segment-zero RAM and the ROM window, while function pointers and explicit
  `__far` data pointers use a 32-bit container for a full 24-bit address.
- Far flash loads and stores using the pointer segment as the DSR selector.
- Direct and indirect far calls/jumps.  An aligned pointer is transferred with
  `PUSH XRn`; an unaligned pointer uses `PUSH Rsegment` followed by `PUSH ERn`.
  Indirect calls branch through `__nxu16_indirect_call`, whose only instruction
  is `POP PC`.
- 16-bit, same-segment jump-table entries and `B ERn` dispatch.  Tables live
  in near `.rodata` inside the segment-zero ROM window; their labels remain
  in the function's code bank, which the NXU16 linker prevents from crossing
  a 64 KiB boundary.
- Compile-time C++ RTTI lowering (runtime linking still needs a target C++
  ABI library).
- A minimal freestanding `libgcc.a` containing 16- and 32-bit multiply,
  signed and unsigned divide/modulo, and shift helpers.  The C smoke tests
  link against this archive without unresolved arithmetic symbols.
- `libgcov` is disabled by default.  Its runtime requires wider integer and
  hosted-I/O support that this freestanding target does not yet provide.
- Assembly syntax compatible with this workspace's rebuilt nX-U16 GAS.

`gcc/testsuite/gcc.target/nxu16/smoke.c` is the current freestanding smoke
input.  Compiler output should always be assembled as part of testing; a
successful `-S` compilation alone does not validate machine-description
templates.

## Important ISA constraints

The hardware `MUL ERn,Rm` operation multiplies two byte registers and returns
a word.  It is exposed only as an unsigned widening multiply; general 16-bit
multiplication uses a runtime helper rather than being silently truncated.

The hardware `DIV ERn,Rm` operation is not a general same-width division: it
divides a 16-bit dividend by an 8-bit divisor and writes the remainder back to
`Rm`.  A general C division may only use it when the value range and register
overlap constraints are satisfied.  Other cases must use a correct runtime
helper.

`SLL`, `SRL`, and `SRA` are byte operations.  Word and 32-bit shifts use the
libgcc helpers, which combine adjacent byte registers with `SLLC`/`SRLC`.

The 32-bit far-pointer container is an ABI choice, not an ISA requirement:
only 24 address bits are meaningful.  It avoids GCC's awkward three-byte
`PSImode`, naturally fits the four-byte control-address record consumed by
large-mode `POP PC`, and enables a single aligned `PUSH XRn`.  Ordinary data
pointers do not pay this cost; they remain native 16-bit `HImode` values.
The stack and hard frame pointer are ordinary near addresses as well.  ER14
is the hard frame pointer only in functions that require a stable frame;
frameless functions may allocate ER14 normally.

Likewise, `[EA+]` addressing is tied to the dedicated, fixed `EA` register.
Enabling generic GCC post-increment addressing without modeling how values
are transferred to and from `EA` would generate invalid code.

## Remaining backend work

- Expand executable ABI coverage to aggregate arguments/returns, every scalar
  width, recursion, and mixed caller optimization levels.
- Implement and test nested-function trampolines; the current zero-sized
  trampoline definitions are placeholders.
- Model the coprocessor/control registers with appropriate register classes,
  constraints, moves, and call-clobber rules.
- Implement constrained hardware division and dedicated `EA` post-increment
  patterns.
- Audit the machine description against every instruction in the ISA manual.
- Add and validate 64-bit integer and floating-point libgcc routines.  Generic
  DWARF/SJLJ unwinding is deliberately disabled until frame-related RTL and
  CFI are correct.
- Extend sibling-call runtime coverage to argument-bearing indirect calls;
  validate C++ exceptions, RTTI, and more of the minimal libgcc helpers.
- Compile and run CoreMark on the supplied simulator.

Do not mark these items complete from assembly inspection alone.  They need
ABI tests running on the architecture simulator.
