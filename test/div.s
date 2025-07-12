.globl main
main:
	movl $12, %eax
	neg %eax
	push %rax
	movl $5, %eax
	movl %eax, %ecx
	pop %rax
	cdq
	idivl %ecx
	ret
