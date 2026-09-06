#include <nxu16/mcp-console.h>

volatile char nxu16_console_buffer[NXU16_CONSOLE_CAPACITY];
volatile size_t nxu16_console_length;

extern void __nxu16_debug_putchar(int character);

int __nxu16_putchar(int character) {
  size_t position = nxu16_console_length;
  __nxu16_debug_putchar(character);
  if (position + 1U < NXU16_CONSOLE_CAPACITY) {
    nxu16_console_buffer[position] = (char)character;
    nxu16_console_buffer[position + 1U] = '\0';
    nxu16_console_length = position + 1U;
  }
  return (unsigned char)character;
}
