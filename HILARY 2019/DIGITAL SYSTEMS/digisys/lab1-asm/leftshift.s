        .syntax unified
        .global foo

        .text
        .thumb_func
foo : 
	lsls r0,r0,r1 	@ r0 * 2^r1
	bx lr