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
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	subq $48, %rsp
	/* end prolog */
	/* mov ~dst:"-8(%rbp)" ~src:"%rdi" */
	movq %rdi, -8(%rbp)
	/* end mov */
	/* mov ~dst:"-16(%rbp)" ~src:"%rsi" */
	movq %rsi, -16(%rbp)
	/* end mov */
	/* mov ~dst:"-24(%rbp)" ~src:"%rdx" */
	movq %rdx, -24(%rbp)
	/* end mov */
	/* mov ~dst:"-32(%rbp)" ~src:"%rcx" */
	movq %rcx, -32(%rbp)
	/* end mov */
	/* mov ~dst:"-40(%rbp)" ~src:"%r8" */
	movq %r8, -40(%rbp)
	/* end mov */
	/* mov ~dst:"-48(%rbp)" ~src:"%r9" */
	movq %r9, -48(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"$.S_0" */
	movq $.S_0, %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-8(%rbp)" */
	movq -8(%rbp), %rsi
	/* end mov */
	/* mov ~dst:"%rdx" ~src:"-16(%rbp)" */
	movq -16(%rbp), %rdx
	/* end mov */
	/* mov ~dst:"%rcx" ~src:"-24(%rbp)" */
	movq -24(%rbp), %rcx
	/* end mov */
	/* mov ~dst:"%r8" ~src:"-32(%rbp)" */
	movq -32(%rbp), %r8
	/* end mov */
	/* mov ~dst:"%r9" ~src:"-40(%rbp)" */
	movq -40(%rbp), %r9
	/* end mov */
	/* mov ~dst:"%rax" ~src:"$0" */
	movq $0, %rax
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$printf" ~args:"-48(%rbp), 8(%rbp)" */
	pushq 8(%rbp)
	pushq -48(%rbp)
	subq $8, %rsp
	call printf
	addq $8, %rsp
	popq %r15
	popq %r15
	/* end call */
	/* start epilog */
	addq $48, %rsp
	popq %rbp
	/* end epilog */
	ret
.p2align 3, 144
.I_154575915:
	/* Initializer for x. */
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	/* end prolog */
	/* mov ~dst:"%rdi" ~src:"$37" */
	movq $37, %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"$73" */
	movq $73, %rsi
	/* end mov */
	/* mov ~dst:"%rdx" ~src:"$31" */
	movq $31, %rdx
	/* end mov */
	/* mov ~dst:"%rcx" ~src:"$13" */
	movq $13, %rcx
	/* end mov */
	/* mov ~dst:"%r8" ~src:"$0" */
	movq $0, %r8
	/* end mov */
	/* mov ~dst:"%r9" ~src:"$99" */
	movq $99, %r9
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$print_seven_int_for_real" ~args:"$42" */
	pushq $42
	call print_seven_int_for_real
	popq %r15
	/* end call */
	/* mov ~dst:"%rdi" ~src:"$0" */
	movq $0, %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$exit" ~args:"" */
	call exit
	/* end call */
