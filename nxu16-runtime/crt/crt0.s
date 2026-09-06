	.section .vectors,"a",@progbits
	.global __nxu16_vectors
__nxu16_vectors:
	.short __stack_top
	.short _start
	.short _start

	.section .text.startup,"ax",@progbits
	.global _start
	.type _start,@function
_start:
	bl __nxu16_start
__nxu16_start_hang:
	b __nxu16_start_hang
	.size _start,.-_start

