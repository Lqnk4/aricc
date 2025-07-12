.globl main
main:
	movl $2, %eax
	push %rax
	movl $3, %eax
	push %rax
	movl $4, %eax
	pop %rcx
	imul %ecx, %eax
	pop %rcx
	addl %ecx, %eax
	ret
