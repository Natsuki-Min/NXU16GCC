#ifndef _NXU16_RUNTIME_H
#define _NXU16_RUNTIME_H

#ifdef __cplusplus
extern "C" {
#endif

extern volatile int nxu16_exit_code;

int __nxu16_putchar(int character);
void __nxu16_exit_loop(void) __attribute__((noreturn));

#ifdef __cplusplus
}
#endif

#endif
