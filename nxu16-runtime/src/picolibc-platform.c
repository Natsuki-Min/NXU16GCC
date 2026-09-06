#include <nxu16/runtime.h>
#include <stdio.h>
#include <unistd.h>

volatile int nxu16_exit_code __attribute__((section(".noinit")));

int __attribute__((weak))
__nxu16_putchar(int character)
{
  return (unsigned char) character;
}

static int
console_put(char character, FILE *stream)
{
  (void) stream;
  return __nxu16_putchar((unsigned char) character);
}

static int
console_get(FILE *stream)
{
  (void) stream;
  return _FDEV_EOF;
}

static int
console_flush(FILE *stream)
{
  (void) stream;
  return 0;
}

static FILE console = FDEV_SETUP_STREAM(console_put, console_get,
                                        console_flush, _FDEV_SETUP_RW);

FILE *const stdin = &console;
FILE *const stdout = &console;
FILE *const stderr = &console;

void
_exit(int status)
{
  nxu16_exit_code = status;
  __asm__ volatile(
      ".global __nxu16_exit_loop\n"
      "__nxu16_exit_loop:\n"
      "nop\n"
      "b __nxu16_exit_loop");
  __builtin_unreachable();
}
