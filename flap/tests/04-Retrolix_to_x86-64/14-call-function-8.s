.data
.S_0:
	.string "%d %d %d %d %d %d %d\n"
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
	.extern printf
.p2align 3, 144
print_seven_int_for_real:
	/* Retrolix function print_seven_int_for_real. */
	pushq %rbp
	movq %rsp, %rbp
	subq $48, %rsp
	movq %rdi, 0(%rbp)
	movq %rsi, -8(%rbp)
	movq %rdx, -16(%rbp)
	movq %rcx, -24(%rbp)
	movq %r8, -32(%rbp)
	movq %r9, -40(%rbp)
	movq $.S_0, %rdi
	movq 0(%rbp), %rsi
	movq -8(%rbp), %rdx
	movq -16(%rbp), %rcx
	movq -24(%rbp), %r8
	movq -32(%rbp), %r9
	movq $0, %rax
	pushq 0(%rbp)
	pushq -40(%rbp)
	call printf
	popq %r15
	popq %r15
	addq $48, %rsp
	popq %rbp
	ret
.p2align 3, 144
.I_154575915:
	/* Initializer for x. */
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	movq $37, %rdi
	movq $73, %rsi
	movq $31, %rdx
	movq $13, %rcx
	movq $0, %r8
	movq $99, %r9
	pushq $42
	call print_seven_int_for_real
	popq %r15
	movq $0, %rdi
	call exit
