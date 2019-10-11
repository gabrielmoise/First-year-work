        .syntax unified
        .global foo

        .text
        .thumb_func
foo: 
	movs r2, #0
loop:
	cmp r0, r1
	blo done
	subs r0, r0, r1 
	adds r2, #1
	b loop
done:
	movs r0, r2
	bx lr