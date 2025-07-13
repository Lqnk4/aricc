.globl main
main:
	movl $6, %eax
	push %rax
	movl $3, %eax
	movl %eax, %ecx
	pop %rax
	cdq
	idivl %ecx
	push %rax
	movl $2, %eax
	movl %eax, %ecx
	pop %rax
	cdq
	idivl %ecx
	push %rax
	movl $6, %eax
	pop %rcx
	addl %ecx, %eax
	push %rax
	movl $3, %eax
	movl %eax, %ecx
	pop %rax
	subl %ecx, %eax
	push %rax
	movl $2, %eax
	movl %eax, %ecx
	pop %rax
	subl %ecx, %eax
	ret
