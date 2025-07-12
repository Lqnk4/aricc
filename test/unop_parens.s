.globl main
main:
	movl $1, %eax
	push %rax
	movl $1, %eax
	pop %rcx
	addl %ecx, %eax
	not %eax
	ret
