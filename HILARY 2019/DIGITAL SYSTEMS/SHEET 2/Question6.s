        .syntax unified
        .text

        .global foo
        .thumb_func

	@ r0 = n
	@ r1 = k
	@ r2 = local use addresses
	@ r3 = the jth entry in the current line 	
	@ r4 = base address of the array row
	@ r5 = i 
	@ r6 = j 
	@ r7 = local use values from the Pascal triangle
	@ row[i][j] = the jth entry on the ith line of the Pascal triagle
	

foo:
	push {r4-r7, lr} 	@ Save all registers
	ldr r4, =row 		@ Set r4 to base of array
	movs r5, #0			@ i = 0
@ Invariant I : the ith row of the Pascal triangle is stored in the stack frame
outer: 
	movs r6, #0   		@ j = 0
	movs r3, #1			@ row[i][0] = 1
	str r3, [r4, #0] 	@ store row[i][0] at offset 0
	adds r5, #1			@ i = i + 1 && r3 = row[i-1][j]
	cmp r5, r0			@ if i>n, then the nth row is complete
	bhi done			@ so we are done
@ Invariant J : row[0..j] contains the first (j+1) numbers on the ith line of the Pascal triangle && r3 = row[i-1][j]
inner:
	adds r6, #1			@ j = j + 1
	cmp r5, r6			@ when i=j, we are done with inner,
	beq inner_done		@ as we calculated the ith row
	lsls r2, r6, #2		@ r2 = address of row[i-1][j]
	ldr r7, [r4, r2]		
@ r7 = row[i-1][j] && r3 = row[i-1][j-1]
	adds r7, r7, r3		@ r7 = row[i][j]	
	subs r3, r7, r3		@ r3 = row[i-1][j]
	str r7, [r4, r2]		@ store row[i][j] at the jth entry &&J
	b inner

inner_done:
	movs r3, #1			@ row[i][j] = row[i][i] = 1
	lsls r2, r5, #2		@ calculate the address of row[i][j]
	str r3, [r4, r2]		@ store row[i][j] && I
	b outer			@ back to outer

done:
	lsls r2, r1, #2		@ return row[n][k]
	ldr r0, [r4, r2]		
	pop {r4-r7, pc} 		@ Restore regs and return

@ Statically allocate 256 words for the array
	.bss
	.align 2
row:
	.space 1024