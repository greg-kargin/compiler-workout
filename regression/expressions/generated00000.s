	.data

global_x0:	.int	0

global_x1:	.int	0

global_y:	.int	0

	.text

	.globl	main

main:

	pushl	%ebp
	movl	%esp,	%ebp
	subl	$Lmain_SIZE,	%esp
	call	Lread
	movl	%eax,	%ebx
	movl	%ebx,	%eax
	movl	%eax,	global_x0
	call	Lread
	movl	%eax,	%ebx
	movl	%ebx,	%eax
	movl	%eax,	global_x1
	movl	$12,	%ebx
	movl	%ebx,	%eax
	movl	%eax,	global_y
	movl	global_y,	%eax
	movl	%eax,	%ebx
	pushl	%ebx
	call	Lwrite
	popl	%eax
	movl	$0,	%ebx
	movl	%ebx,	%eax
	jmp	Lmain_epilogue
Lmain_epilogue:

	movl	%ebp,	%esp
	popl	%ebp
	ret
	.set Lmain_SIZE, 0

