RUNTIME_DIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
WORKSPACE := $(abspath $(RUNTIME_DIR)/../..)

NXU16_CC ?= $(WORKSPACE)/gcc/build-codex/gcc/xgcc
NXU16_GCC_BDIR ?= $(WORKSPACE)/gcc/build-codex/gcc
NXU16_AR ?= $(WORKSPACE)/NXU16GNUbinutils/build-codex/binutils/ar
NXU16_OBJCOPY ?= $(WORKSPACE)/NXU16GNUbinutils/build-codex/binutils/objcopy
NXU16_OBJDUMP ?= $(WORKSPACE)/NXU16GNUbinutils/build-codex/binutils/objdump
PICOLIBC_SRC ?= $(WORKSPACE)/picolibc
MESON ?= $(if $(wildcard $(WORKSPACE)/.tools/meson/bin/meson),$(WORKSPACE)/.tools/meson/bin/meson,meson)
NINJA ?= ninja

BUILD := $(RUNTIME_DIR)/build
OBJ := $(BUILD)/obj
# Keep the target C library with the compiler build.  This is the development
# toolchain's sysroot; BUILD only contains disposable integration artifacts.
SYSROOT ?= $(abspath $(NXU16_GCC_BDIR)/../sysroot)
SYSINC := $(SYSROOT)/include
SYSLIB := $(SYSROOT)/lib
PICOLIBC_BUILD := $(BUILD)/picolibc

CFLAGS := -Os -std=gnu18 -ffreestanding -fno-builtin \
	-fno-asynchronous-unwind-tables -fno-unwind-tables \
	-ffunction-sections -fdata-sections -Wall -Wextra
CC_TARGET := $(NXU16_CC) -B$(NXU16_GCC_BDIR)/ --sysroot=$(SYSROOT) \
	-isystem $(SYSINC)

PLATFORM_SOURCES := src/startup.c src/picolibc-platform.c
PLATFORM_OBJECTS := $(PLATFORM_SOURCES:src/%.c=$(OBJ)/%.o)
MCP_CONSOLE_OBJECTS := $(OBJ)/mcp-console.o $(OBJ)/debug-console.o

PICOLIBC_OPTIONS := \
	--cross-file $(PICOLIBC_SRC)/scripts/cross-nxu16.txt \
	--prefix $(SYSROOT) -Dincludedir=include -Dlibdir=lib -Dspecsdir=lib \
	-Dmultilib=false -Dtests=false -Dpicocrt=false -Dpicocrt-lib=false \
	-Dsemihost=false -Dthread-local-storage=false \
	-Dnewlib-global-errno=true -Datomic-ungetc=false -Dsingle-thread=true \
	-Dformat-default=minimal -Dio-long-long=false -Dio-long-double=false \
	-Dmb-capable=false -Dnewlib-obsolete-math=true -Dfast-strcmp=false \
	-Ddebug=false

.PHONY: all clean check check-mcp picolibc sysroot

all: sysroot

sysroot: picolibc $(SYSLIB)/libnxu16.a $(SYSLIB)/crt0.o \
	$(SYSLIB)/nxu16-classwiz.ld $(SYSLIB)/nxu16.specs $(SYSINC)/nxu16/.stamp

picolibc: | $(BUILD) $(SYSROOT)
	@test -f $(PICOLIBC_SRC)/meson.build || \
		(echo "Picolibc source not found at $(PICOLIBC_SRC)" >&2; exit 1)
	@if test -f $(PICOLIBC_BUILD)/build.ninja; then \
		PATH="$(RUNTIME_DIR)/bin:$$PATH" $(MESON) setup --reconfigure \
			$(PICOLIBC_BUILD) $(PICOLIBC_SRC) $(PICOLIBC_OPTIONS); \
	else \
		PATH="$(RUNTIME_DIR)/bin:$$PATH" $(MESON) setup \
			$(PICOLIBC_BUILD) $(PICOLIBC_SRC) $(PICOLIBC_OPTIONS); \
	fi
	PATH="$(RUNTIME_DIR)/bin:$$PATH" $(NINJA) -C $(PICOLIBC_BUILD)
	PATH="$(RUNTIME_DIR)/bin:$$PATH" $(NINJA) -C $(PICOLIBC_BUILD) install

$(BUILD) $(OBJ) $(SYSROOT) $(SYSLIB) $(SYSINC)/nxu16:
	mkdir -p $@

$(OBJ)/%.o: src/%.c picolibc | $(OBJ)
	$(CC_TARGET) $(CFLAGS) -c $< -o $@

$(OBJ)/debug-console.o: src/debug-console.s picolibc | $(OBJ)
	$(CC_TARGET) $(CFLAGS) -c $< -o $@

$(OBJ)/rodata-overflow.o: tests/rodata-overflow.s picolibc | $(OBJ)
	$(CC_TARGET) $(CFLAGS) -c $< -o $@

$(SYSLIB)/libnxu16.a: $(PLATFORM_OBJECTS) | $(SYSLIB)
	$(RM) $@
	$(NXU16_AR) rcs $@ $(PLATFORM_OBJECTS)

$(SYSLIB)/crt0.o: crt/crt0.s picolibc | $(SYSLIB)
	$(CC_TARGET) $(CFLAGS) -c $< -o $@

$(SYSLIB)/nxu16-classwiz.ld: linker/nxu16-classwiz.ld | $(SYSLIB)
	cp $< $@

$(SYSLIB)/nxu16.specs: specs/nxu16.specs | $(SYSLIB)
	cp $< $@

$(SYSINC)/nxu16/.stamp: $(shell find include/nxu16 -type f | sort) | $(SYSINC)/nxu16
	cp -R include/nxu16/. $(SYSINC)/nxu16/
	touch $@

$(BUILD)/runtime-test.elf: tests/runtime-test.c sysroot $(MCP_CONSOLE_OBJECTS)
	bin/nxu16-gcc -Os $< $(MCP_CONSOLE_OBJECTS) -o $@

$(BUILD)/runtime-test.bin: $(BUILD)/runtime-test.elf
	$(NXU16_OBJCOPY) -O binary $< $@

$(BUILD)/runtime-test-o0.elf: tests/runtime-test.c sysroot $(MCP_CONSOLE_OBJECTS)
	bin/nxu16-gcc -O0 $< $(MCP_CONSOLE_OBJECTS) -o $@

$(BUILD)/runtime-test-o0.bin: $(BUILD)/runtime-test-o0.elf
	$(NXU16_OBJCOPY) -O binary $< $@

check: $(BUILD)/runtime-test.bin $(BUILD)/runtime-test-o0.bin \
	$(OBJ)/rodata-overflow.o
	$(NXU16_OBJDUMP) -h -t $(BUILD)/runtime-test.elf > $(BUILD)/runtime-test.dump
	grep -q '00000000.*\.vectors' $(BUILD)/runtime-test.dump
	grep -q '__nxu16_exit_loop' $(BUILD)/runtime-test.dump
	test "$$(wc -c < $(BUILD)/runtime-test.bin)" -gt 69632
	test "$$(wc -c < $(BUILD)/runtime-test.bin)" -lt 131072
	$(NXU16_OBJDUMP) -h -t $(BUILD)/runtime-test-o0.elf > $(BUILD)/runtime-test-o0.dump
	grep -q '00000000.*\.vectors' $(BUILD)/runtime-test-o0.dump
	grep -q '__nxu16_exit_loop' $(BUILD)/runtime-test-o0.dump
	test "$$(wc -c < $(BUILD)/runtime-test-o0.bin)" -gt 69632
	test "$$(wc -c < $(BUILD)/runtime-test-o0.bin)" -lt 131072
	@if $(CC_TARGET) -nostdlib \
		-Wl,-T,$(RUNTIME_DIR)/linker/nxu16-classwiz.ld \
		$(OBJ)/rodata-overflow.o -o $(BUILD)/rodata-overflow.elf \
		>$(BUILD)/rodata-overflow.log 2>&1; then \
		echo "oversized near .rodata unexpectedly linked" >&2; exit 1; \
	fi
	grep -q 'near read-only data exceeds the ClassWiz ROM window' \
		$(BUILD)/rodata-overflow.log
	@echo "NXU16 Picolibc compile/link/layout checks passed"

check-mcp: check
	python3 tests/run-mcp.py
	python3 tests/run-mcp.py --elf $(BUILD)/runtime-test-o0.elf \
		--binary $(BUILD)/runtime-test-o0.bin

clean:
	rm -rf $(BUILD)
