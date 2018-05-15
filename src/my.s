	.data
global_x:	.int	0
	.text
	.globl	main
main:
	pushl	%ebp
	movl	%esp,	%ebp
	subl	$0,	%esp
	movl	$8,	%ebx
	movl	%ebx,	global_x
	movl	%ebp,	%esp
	popl	%ebp
	xorl	%eax,	%eax
	ret
