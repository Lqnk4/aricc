.globl main
main:
	movl $1, %eax
	cmp $0, %eax
	movl $0, %eax
	sete %al
	not %eax
	neg %eax
	ret
