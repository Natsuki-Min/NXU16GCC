	.text
	.global __nxu16_debug_putchar
	.type __nxu16_debug_putchar,@function
__nxu16_debug_putchar:
	dsr 6
	st r0,0x0721
	rt
	.size __nxu16_debug_putchar,.-__nxu16_debug_putchar
