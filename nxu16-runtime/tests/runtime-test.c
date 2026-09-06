#include <nxu16/mcp-console.h>
#include <nxu16/runtime.h>
#include <ctype.h>
#include <picolibc.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static unsigned int failures;
static volatile unsigned int initialized_data = 0x1234U;
static volatile unsigned int jump_selector = 6U;
static volatile unsigned int cleared_bss;
static volatile unsigned long math_si_a = 0x12345678UL;
static volatile unsigned long math_si_b = 0x10203UL;
static volatile unsigned long long math_di_a = 0x123456789abcdef0ULL;
static volatile unsigned long long math_di_b = 0x12345ULL;
static volatile float math_sf_a = 7.5F;
static volatile float math_sf_b = 2.5F;
static jmp_buf jump_environment;

static const unsigned char __far far_flash_data[]
    __attribute__((section(".far_rodata"), used)) = {
        0x31U, 0x62U, 0x93U, 0xc4U, 0xf5U,
};
static const unsigned char __far *volatile far_flash_pointer = far_flash_data;

__attribute__((noinline, section(".bank1"))) static unsigned int
far_function(unsigned int value) {
  return value ^ 0x55aaU;
}

static unsigned int (*volatile far_function_pointer)(unsigned int) =
    far_function;

__attribute__((noinline, section(".bank1.unaligned"))) static unsigned int
far_function_no_arguments(void) {
  return 0xa55aU;
}

static unsigned int (*volatile far_unaligned_function_pointer)(void) =
    far_function_no_arguments;

__attribute__((noinline)) static unsigned int
tail_call_far_no_arguments(unsigned int (*pointer)(void)) {
  return pointer();
}

#ifndef __PICOLIBC__
#error "The NXU16 sysroot must use Picolibc headers"
#endif

static void check(int condition, unsigned int bit) {
  if (!condition)
    failures |= bit;
}

static void jump_back(void) { longjmp(jump_environment, 7); }

__attribute__((noinline)) static unsigned int
read_far_flash(const unsigned char __far *pointer, unsigned int index) {
  return pointer[index];
}

__attribute__((noinline)) static unsigned int
call_unaligned_far(unsigned int (*pointer)(void)) {
  register unsigned int (*target)(void) asm("r2") = pointer;
  asm volatile("" : "+r"(target));
  unsigned int result = target();
  asm volatile("" ::: "memory");
  return result;
}

#define JUMP_CASE(number, value)                                               \
  __attribute__((noipa, used)) unsigned int jump_case_##number(void) {         \
    return value;                                                              \
  }

JUMP_CASE(0, 3U)
JUMP_CASE(1, 5U)
JUMP_CASE(2, 8U)
JUMP_CASE(3, 13U)
JUMP_CASE(4, 21U)
JUMP_CASE(5, 34U)
JUMP_CASE(6, 55U)
JUMP_CASE(7, 89U)

__attribute__((noinline)) static unsigned int jump_table_switch(unsigned int n) {
  switch (n) {
  case 0: return jump_case_0();
  case 1: return jump_case_1();
  case 2: return jump_case_2();
  case 3: return jump_case_3();
  case 4: return jump_case_4();
  case 5: return jump_case_5();
  case 6: return jump_case_6();
  case 7: return jump_case_7();
  default: return 0U;
  }
}

/* The function and its labels live in CSR 1, while its HImode table remains
   in segment-zero .rodata so the ordinary near load can read it.  B ERn then
   combines the selected 16-bit label with the function's current CSR.  */
__attribute__((noinline, section(".bank1.switch"))) static unsigned int
high_bank_jump_table_switch(unsigned int n) {
  switch (n) {
  case 0: return jump_case_0();
  case 1: return jump_case_1();
  case 2: return jump_case_2();
  case 3: return jump_case_3();
  case 4: return jump_case_4();
  case 5: return jump_case_5();
  case 6: return jump_case_6();
  case 7: return jump_case_7();
  default: return 0U;
  }
}

int main(void) {
  char buffer[32];
  volatile char *memory;
  int jump_result;
  check(initialized_data == 0x1234U, 0x001U);
  check(cleared_bss == 0U, 0x002U);
  check(strlen("nxu16") == 5U, 0x004U);
  check(isalpha('N') && isdigit('6') && !isspace('x'), 0x200U);
  strcpy(buffer, "tool");
  strcat(buffer, "chain");
  check(strcmp(buffer, "toolchain") == 0, 0x008U);
  check(snprintf(buffer, sizeof(buffer), "%s:%u", "ok", 42U) == 5,
        0x010U);
  if (strcmp(buffer, "ok:42") != 0)
    puts(buffer);
  check(strcmp(buffer, "ok:42") == 0, 0x020U);
  memory = (volatile char *)malloc(12U);
  check(memory != 0, 0x040U);
  if (memory) {
    memset((void *)memory, 0x5a, 12U);
    check((unsigned char)memory[11] == 0x5aU, 0x080U);
    free((void *)memory);
  }
  jump_result = setjmp(jump_environment);
  if (jump_result == 0)
    jump_back();
  check(jump_result == 7, 0x800U);
  check(read_far_flash(far_flash_pointer, 3U) == 0xc4U, 0x1000U);
  check(far_function_pointer(0x1234U) == (0x1234U ^ 0x55aaU), 0x2000U);
  check(tail_call_far_no_arguments(far_unaligned_function_pointer) == 0xa55aU,
        0x2000U);
  check(jump_table_switch(jump_selector) == 55U, 0x4000U);
  check(high_bank_jump_table_switch(jump_selector) == 55U, 0x400U);
  check(call_unaligned_far(far_unaligned_function_pointer) == 0xa55aU,
        0x8000U);
  check(math_si_a * math_si_b == 0xf5c1f368UL, 0x001U);
  check(math_si_a / math_si_b == 0x1210UL, 0x001U);
  check(math_si_a % math_si_b == 0x48UL, 0x001U);
  check(math_di_a * math_di_b == 0x666666666652e6b0ULL, 0x001U);
  check(math_di_a / math_di_b == 0x100005b00205ULL, 0x001U);
  check(math_di_a % math_di_b == 0xa497ULL, 0x001U);
  check(math_sf_a + math_sf_b == 10.0F, 0x001U);
  check(math_sf_a - math_sf_b == 5.0F, 0x001U);
  check(math_sf_a * math_sf_b == 18.75F, 0x001U);
  check(math_sf_a / math_sf_b == 3.0F, 0x001U);
  puts(failures ? "runtime:FAIL" : "runtime:PASS");
  check(nxu16_console_length != 0U, 0x100U);
  return failures;
}
