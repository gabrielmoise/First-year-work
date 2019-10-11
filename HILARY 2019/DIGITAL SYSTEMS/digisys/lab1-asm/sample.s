        .syntax unified
        .global foo

        .text
        .thumb_func
foo : 
	movs r3, r0
	lsrs r3, #1
	bcc skip
not : 
	movs r0, #1
	b done
skip : 
	movs r0, #0
done : 
	bx lr
