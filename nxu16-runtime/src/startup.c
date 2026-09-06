#include <nxu16/runtime.h>
#include <stdlib.h>

typedef void (*init_function)(void);

extern unsigned char __far __data_load_start[];
extern unsigned char __data_start[];
extern unsigned char __data_end[];
extern unsigned char __bss_start[];
extern unsigned char __bss_end[];
extern init_function __preinit_array_start[];
extern init_function __preinit_array_end[];
extern init_function __init_array_start[];
extern init_function __init_array_end[];
extern int main(void);

static void run_forward(init_function *begin, init_function *end) {
  while (begin != end)
    (*begin++)();
}

void __nxu16_start(void) __attribute__((noreturn));
void __nxu16_start(void) {
  unsigned char *destination = __data_start;
  const unsigned char __far *source = __data_load_start;
  while (destination != __data_end)
    *destination++ = *source++;
  destination = __bss_start;
  while (destination != __bss_end)
    *destination++ = 0;
  run_forward(__preinit_array_start, __preinit_array_end);
  run_forward(__init_array_start, __init_array_end);
  exit(main());
}
