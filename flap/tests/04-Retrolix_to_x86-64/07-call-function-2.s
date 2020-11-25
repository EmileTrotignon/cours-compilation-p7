.data
.S_0:
	.string "%d %d\n"
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
print_two_int:
	/* Retrolix function print_two_int. */
	pushq %rbp
	movq %rsp, %rbp
	addq $0, %rsp
	movq %rdi, %rax
	movq %rsi, %rbx
	movq $.S_0, %rdi
	movq %rax, %rsi
	movq %rbx, %rdx
	movq $0, %rax
	call printf
	addq $0, %rsp
	popq %rbp
	ret
.p2align 3, 144
.I_154575915:
	/* Initializer for x. */
	pushq %rbp
	movq %rsp, %rbp
	addq $0, %rsp
	movq $37, %rdi
	movq $73, %rsi
	call print_two_int
	movq $0, %rdi
	call exit
