.globl main
main:
	movl $2, %eax
	push %rax
	movl $1, %eax
	neg %eax
	movl %eax, %ecx
	pop %rax
	subl %ecx, %eax
	ret
