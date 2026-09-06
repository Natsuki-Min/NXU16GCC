	.section .rodata.overflow,"a",@progbits
	.global oversized_near_rodata
oversized_near_rodata:
	.space 0xd002
