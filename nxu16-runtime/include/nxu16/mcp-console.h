#ifndef _NXU16_MCP_CONSOLE_H
#define _NXU16_MCP_CONSOLE_H

#include <stddef.h>

#define NXU16_CONSOLE_CAPACITY 256U

#ifdef __cplusplus
extern "C" {
#endif

extern volatile char nxu16_console_buffer[NXU16_CONSOLE_CAPACITY];
extern volatile size_t nxu16_console_length;

#ifdef __cplusplus
}
#endif

#endif
