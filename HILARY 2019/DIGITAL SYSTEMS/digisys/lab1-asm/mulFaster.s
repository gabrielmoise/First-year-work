        .syntax unified
        .global foo

        .text
        .thumb_func
foo : 
	movs r2, #0
	subs r2, r2, r0
	adds r1, r1, #1
loop : 
	adds r2, r2, r0
	subs r1, r1, #1
	bne loop
done : 
	movs r0, r2
	bx lr