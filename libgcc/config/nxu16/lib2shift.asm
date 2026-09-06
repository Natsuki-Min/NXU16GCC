	.text

	.global __ashlqi3
	.type __ashlqi3, @function
__ashlqi3:
	cmp r1,0
	beq .Lashlqi_done
.Lashlqi_loop:
	sll r0,1
	add r1,255
	cmp r1,0
	bne .Lashlqi_loop
.Lashlqi_done:
	rt
	.size __ashlqi3, .-__ashlqi3

	.global __lshrqi3
	.type __lshrqi3, @function
__lshrqi3:
	cmp r1,0
	beq .Llshrqi_done
.Llshrqi_loop:
	srl r0,1
	add r1,255
	cmp r1,0
	bne .Llshrqi_loop
.Llshrqi_done:
	rt
	.size __lshrqi3, .-__lshrqi3

	.global __ashrqi3
	.type __ashrqi3, @function
__ashrqi3:
	cmp r1,0
	beq .Lashrqi_done
.Lashrqi_loop:
	sra r0,1
	add r1,255
	cmp r1,0
	bne .Lashrqi_loop
.Lashrqi_done:
	rt
	.size __ashrqi3, .-__ashrqi3

	.global __ashlhi3
	.type __ashlhi3, @function
__ashlhi3:
	cmp r2,0
	beq .Lashl_done
.Lashl_loop:
	sllc r1,1
	sll r0,1
	add r2,255
	cmp r2,0
	bne .Lashl_loop
.Lashl_done:
	rt
	.size __ashlhi3, .-__ashlhi3

	.global __lshrhi3
	.type __lshrhi3, @function
__lshrhi3:
	cmp r2,0
	beq .Llshr_done
.Llshr_loop:
	srlc r0,1
	srl r1,1
	add r2,255
	cmp r2,0
	bne .Llshr_loop
.Llshr_done:
	rt
	.size __lshrhi3, .-__lshrhi3

	.global __ashrhi3
	.type __ashrhi3, @function
__ashrhi3:
	cmp r2,0
	beq .Lashr_done
.Lashr_loop:
	srlc r0,1
	sra r1,1
	add r2,255
	cmp r2,0
	bne .Lashr_loop
.Lashr_done:
	rt
	.size __ashrhi3, .-__ashrhi3

	/* A 32-bit value occupies R0:R3.  With all four argument registers in
	   use, the 16-bit shift count is passed at the caller's SP.  Preserve
	   XR4, then fetch that count four bytes above the adjusted SP. */
	.global __ashlsi3
	.type __ashlsi3, @function
__ashlsi3:
	push xr4
	mov er4,sp
	l r4,4[er4]
	mov r5,0
	mov er6,0
	cmp er4,er6
	beq .Lashlsi_done
.Lashlsi_loop:
	sllc r3,1
	sllc r2,1
	sllc r1,1
	sll r0,1
	add er4,-1
	cmp er4,er6
	bne .Lashlsi_loop
.Lashlsi_done:
	pop xr4
	rt
	.size __ashlsi3, .-__ashlsi3

	.global __lshrsi3
	.type __lshrsi3, @function
__lshrsi3:
	push xr4
	mov er4,sp
	l r4,4[er4]
	mov r5,0
	mov er6,0
	cmp er4,er6
	beq .Llshrsi_done
.Llshrsi_loop:
	srlc r0,1
	srlc r1,1
	srlc r2,1
	srl r3,1
	add er4,-1
	cmp er4,er6
	bne .Llshrsi_loop
.Llshrsi_done:
	pop xr4
	rt
	.size __lshrsi3, .-__lshrsi3

	.global __ashrsi3
	.type __ashrsi3, @function
__ashrsi3:
	push xr4
	mov er4,sp
	l r4,4[er4]
	mov r5,0
	mov er6,0
	cmp er4,er6
	beq .Lashrsi_done
.Lashrsi_loop:
	srlc r0,1
	srlc r1,1
	srlc r2,1
	sra r3,1
	add er4,-1
	cmp er4,er6
	bne .Lashrsi_loop
.Lashrsi_done:
	pop xr4
	rt
	.size __ashrsi3, .-__ashrsi3
