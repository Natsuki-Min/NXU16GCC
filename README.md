# GCC for the Casio nX-U16/100

This repository is an experimental GCC fork for the nX-U16/100 processor used
in Casio ClassWiz calculators.  Its `master` branch follows GCC development and
adds the `nxu16` target.  It is a research and calculator-development toolchain,
not an official GCC or Casio product.

## What is different from upstream GCC

The fork adds a complete early-stage target description under
`gcc/config/nxu16`, target selection in `gcc/config.gcc`, a small freestanding
`libgcc`, and NXU16-specific compiler tests.  One generic call-lowering change
preserves targets whose function pointers are wider than ordinary data
pointers; this is required by the NXU16 split-pointer ABI.

The important target choices are:

- 8-bit registers, 16-bit `int`, and 32-bit `long`;
- 16-bit near pointers for ordinary data in segment zero;
- 32-bit containers for 24-bit function pointers and explicit `__far` data
  pointers;
- direct and indirect calls across 64 KiB code banks, including sibling calls;
- NXU16 interrupt functions, far-memory accesses, near jump tables, and the
  target's unusual byte-oriented multiply, divide, and shift constraints;
- a minimal `libgcc.a` with the integer arithmetic and indirect-call helpers
  needed by the currently supported C runtime.

The repository also contains historical 6502 work used as a reference.  It is
not part of the NXU16 release and is not installed by the release packager.

## Runtime and companion projects

A usable toolchain combines this compiler with
[NXU16GNUbinutils](https://github.com/Natsuki-Min/NXU16GNUbinutils) and the
[NXU16 Picolibc fork](https://github.com/Natsuki-Min/picolibc).  The packaged
Linux release includes the assembler, linker, binary utilities, Picolibc
headers and libraries, the ClassWiz linker script, startup code, and the
NXU16 platform library.

The default runtime profile is freestanding.  It has no operating-system ABI,
and the bundled Picolibc configuration intentionally uses its minimal formatted
I/O implementation.  C++ parsing and code generation are available, but a
complete target `libstdc++`, exception runtime, and C++ ABI library are not.

Detailed backend status and remaining work are documented in
[`gcc/config/nxu16/README.md`](gcc/config/nxu16/README.md).

## Using the Linux x86_64 release

Extract the archive anywhere and prepend its `bin` directory to `PATH`:

```sh
tar -xJf nxu16-toolchain-linux-x86_64-*.tar.xz
export PATH="$PWD/nxu16-toolchain-linux-x86_64-*/bin:$PATH"
nxu16-gcc --version
nxu16-gcc hello.c -o hello.elf
nxu16-objcopy -O binary hello.elf hello.bin
```

The compiler wrappers locate the toolchain relative to themselves, so the
extracted directory can be moved.  The release targets x86_64 Linux with glibc;
the non-glibc shared libraries used by the compiled frontend and Binutils are
included in `lib/host`.

## Creating a release without rebuilding

After GCC, Binutils, and the Picolibc sysroot have already been built in their
workspace `build-codex` locations, run:

```sh
./contrib/nxu16/make-linux-release.sh
```

The script never invokes either source tree's build system.  It verifies the
existing x86_64 artifacts, copies and strips them in a temporary staging tree,
adds the sysroot, tests the relocatable package with a small compile/link smoke
case, and writes a `.tar.xz` archive plus a `.sha256` file.  Override
`BINUTILS_BUILD`, `GCC_BUILD`, `NXU16_SYSROOT`, or `OUTPUT_DIR` when the workspace
uses different locations.

## Current limitations

This backend remains experimental.  Aggregate ABI coverage, nested-function
trampolines, coprocessor registers, general hardware division, 64-bit and
floating-point runtime helpers, unwinding, and a full target C++ runtime still
need implementation or broader emulator validation.  Consult the backend
status document before relying on generated code outside the tested surface.

