.data
x:
	/* x */
	.quad 0
.text
	.globl main
.p2align 3, 144
main:
	/* Program entry point. */
	subq $8, %rsp
	call .I_154575915
	movq $0, %rdi
	call exit
.p2align 3, 144
double_int:
	/* Retrolix function double_int. */
	pushq %rbp
	movq %rsp, %rbp
	xorq %rax, %rax
	addq %rdi, %rax
	addq %rdi, %rax
	ret
.p2align 3, 144
.I_154575915:
	/* Initializer for x. */
	pushq %rbp
	movq %rsp, %rbp
	movq $1, x(%rbp)
	movq $4, %rbx
l02:
	subq $1, %rbx
	movq x(%rbp), %rdi
	xorq %rbx, %rbx
	subq $1, %rbx
	addq $1, %rbx
	call double_int
	movq %rax, x(%rbp)
	jmp l02
	movq x(%rbp), %rdi
	call observe_int
	movq $0, %rdi
	call exit
