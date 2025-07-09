.globl main
main:
	movq $1, %rax
	cmp $0, %rax
	movq $0, %rax
	sete %al
	not %rax
	neg %rax
	ret
