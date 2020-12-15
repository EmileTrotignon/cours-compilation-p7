.data
.S_4:
	.string "%s(%d) = %d\n"
.S_3:
	.string "fact_tailrec"
.S_2:
	.string "fact_adapt"
.S_1:
	.string "fact_iter"
.S_0:
	.string "fact"
.text
	.globl main
.p2align 3, 144
main:
	/* Program entry point. */
	subq $8, %rsp
	call .I_129913994
	movq $0, %rdi
	call exit
	.extern printf
.p2align 3, 144
fact:
	/* Retrolix function fact. */
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	/* end prolog */
	/* conditional_jump ~cc:"le" ~srcl:"%rdi" ~srcr:"$1" ~ll:"f2" ~lr:"f4" */
	movq %rdi, %r15
	cmpq $1, %r15
	jle f2
	jmp f4
	/* end conditional_jump */
f2:
	/* mov ~dst:"%rax" ~src:"$1" */
	movq $1, %rax
	/* end mov */
	/* start epilog */
	addq $8, %rsp
	popq %rbp
	/* end epilog */
	ret
f4:
	/* mov ~dst:"0(%rbp)" ~src:"%rdi" */
	movq %rdi, 0(%rbp)
	/* end mov */
	/* sub ~dst:"%rdi" ~srcl:"%rdi" ~srcr:"$1" */
	xorq %r15, %r15
	subq $1, %r15
	addq %rdi, %r15
	movq %r15, %rdi
	/* end sub */
	/* call ~kind:"`Normal" ~f:"$fact" ~args:"" */
	subq $8, %rsp
	call fact
	addq $8, %rsp
	/* end call */
	/* mul ~dst:"%rax" ~srcl:"%rax" ~srcr:"0(%rbp)" */
	xorq %r15, %r15
	incq %r15
	imulq %rax, %r15
	imulq 0(%rbp), %r15
	movq %r15, %rax
	/* end mul */
	/* start epilog */
	addq $8, %rsp
	popq %rbp
	/* end epilog */
	ret
.p2align 3, 144
fact_tailrec_body:
	/* Retrolix function fact_tailrec_body. */
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	/* end prolog */
	/* conditional_jump ~cc:"le" ~srcl:"%rdi" ~srcr:"$1" ~ll:"ft2" ~lr:"ft4" */
	movq %rdi, %r15
	cmpq $1, %r15
	jle ft2
	jmp ft4
	/* end conditional_jump */
ft2:
	/* mov ~dst:"%rax" ~src:"%rsi" */
	movq %rsi, %rax
	/* end mov */
	/* start epilog */
	addq $0, %rsp
	popq %rbp
	/* end epilog */
	ret
ft4:
	/* mul ~dst:"%rsi" ~srcl:"%rsi" ~srcr:"%rdi" */
	xorq %r15, %r15
	incq %r15
	imulq %rsi, %r15
	imulq %rdi, %r15
	movq %r15, %rsi
	/* end mul */
	/* sub ~dst:"%rdi" ~srcl:"%rdi" ~srcr:"$1" */
	xorq %r15, %r15
	subq $1, %r15
	addq %rdi, %r15
	movq %r15, %rdi
	/* end sub */
	/* call ~kind:"`Tail" ~f:"$fact_tailrec_body" ~args:"" */
	/* start epilog */
	addq $0, %rsp
	popq %rbp
	/* end epilog */
	subq $0, %rsp
	jmp fact_tailrec_body
	addq $0, %rsp
	ret
	/* end call */
.p2align 3, 144
fact_tailrec:
	/* Retrolix function fact_tailrec. */
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	/* end prolog */
	/* mov ~dst:"%rsi" ~src:"$1" */
	movq $1, %rsi
	/* end mov */
	/* call ~kind:"`Tail" ~f:"$fact_tailrec_body" ~args:"" */
	/* start epilog */
	addq $0, %rsp
	popq %rbp
	/* end epilog */
	subq $0, %rsp
	jmp fact_tailrec_body
	addq $0, %rsp
	ret
	/* end call */
.p2align 3, 144
fact_iter:
	/* Retrolix function fact_iter. */
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	/* end prolog */
	/* mov ~dst:"%rax" ~src:"$1" */
	movq $1, %rax
	/* end mov */
y2:
	/* conditional_jump ~cc:"le" ~srcl:"%rdi" ~srcr:"$1" ~ll:"y6" ~lr:"y3" */
	movq %rdi, %r15
	cmpq $1, %r15
	jle y6
	jmp y3
	/* end conditional_jump */
y3:
	/* mul ~dst:"%rax" ~srcl:"%rax" ~srcr:"%rdi" */
	xorq %r15, %r15
	incq %r15
	imulq %rax, %r15
	imulq %rdi, %r15
	movq %r15, %rax
	/* end mul */
	/* sub ~dst:"%rdi" ~srcl:"%rdi" ~srcr:"$1" */
	xorq %r15, %r15
	subq $1, %r15
	addq %rdi, %r15
	movq %r15, %rdi
	/* end sub */
	jmp y2
y6:
	/* start epilog */
	addq $0, %rsp
	popq %rbp
	/* end epilog */
	ret
.p2align 3, 144
fact_adapt:
	/* Retrolix function fact_adapt. */
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	/* end prolog */
	cmpq $0, %rdi
	je o11
	cmpq $1, %rdi
	je o11
	cmpq $2, %rdi
	je o13
	cmpq $3, %rdi
	je o15
	cmpq $4, %rdi
	je o17
	cmpq $5, %rdi
	je o19
	jmp oE
o11:
	/* mov ~dst:"%rax" ~src:"$1" */
	movq $1, %rax
	/* end mov */
	/* start epilog */
	addq $0, %rsp
	popq %rbp
	/* end epilog */
	ret
o13:
	/* mov ~dst:"%rax" ~src:"$2" */
	movq $2, %rax
	/* end mov */
	/* start epilog */
	addq $0, %rsp
	popq %rbp
	/* end epilog */
	ret
o15:
	/* mov ~dst:"%rax" ~src:"$6" */
	movq $6, %rax
	/* end mov */
	/* start epilog */
	addq $0, %rsp
	popq %rbp
	/* end epilog */
	ret
o17:
	/* mov ~dst:"%rax" ~src:"$24" */
	movq $24, %rax
	/* end mov */
	/* start epilog */
	addq $0, %rsp
	popq %rbp
	/* end epilog */
	ret
o19:
	/* call ~kind:"`Tail" ~f:"$fact" ~args:"" */
	/* start epilog */
	addq $0, %rsp
	popq %rbp
	/* end epilog */
	subq $0, %rsp
	jmp fact
	addq $0, %rsp
	ret
	/* end call */
oE:
	/* call ~kind:"`Tail" ~f:"$fact_iter" ~args:"" */
	/* start epilog */
	addq $0, %rsp
	popq %rbp
	/* end epilog */
	subq $0, %rsp
	jmp fact_iter
	addq $0, %rsp
	ret
	/* end call */
.p2align 3, 144
test_fact_impl:
	/* Retrolix function test_fact_impl. */
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	/* end prolog */
	/* mov ~dst:"0(%rbp)" ~src:"%r12" */
	movq %r12, 0(%rbp)
	/* end mov */
	/* mov ~dst:"%rax" ~src:"%rdi" */
	movq %rdi, %rax
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"%rsi" */
	movq %rsi, %rdi
	/* end mov */
	/* mov ~dst:"%rbx" ~src:"%rdx" */
	movq %rdx, %rbx
	/* end mov */
	/* mov ~dst:"%r12" ~src:"%rsi" */
	movq %rsi, %r12
	/* end mov */
	/* call ~kind:"`Normal" ~f:"%rax" ~args:"" */
	subq $8, %rsp
	call *%rax
	addq $8, %rsp
	/* end call */
	/* mov ~dst:"%rdi" ~src:"$.S_4" */
	movq $.S_4, %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"%rbx" */
	movq %rbx, %rsi
	/* end mov */
	/* mov ~dst:"%rdx" ~src:"%r12" */
	movq %r12, %rdx
	/* end mov */
	/* mov ~dst:"%rcx" ~src:"%rax" */
	movq %rax, %rcx
	/* end mov */
	/* mov ~dst:"%rax" ~src:"$0" */
	movq $0, %rax
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$printf" ~args:"" */
	subq $8, %rsp
	call printf
	addq $8, %rsp
	/* end call */
	/* mov ~dst:"%r12" ~src:"0(%rbp)" */
	movq 0(%rbp), %r12
	/* end mov */
	/* start epilog */
	addq $8, %rsp
	popq %rbp
	/* end epilog */
	ret
.p2align 3, 144
.I_129913994:
	/* Initializer for . */
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	/* end prolog */
	/* mov ~dst:"0(%rbp)" ~src:"%r12" */
	movq %r12, 0(%rbp)
	/* end mov */
	/* mov ~dst:"-8(%rbp)" ~src:"%r13" */
	movq %r13, -8(%rbp)
	/* end mov */
	/* mov ~dst:"%r12" ~src:"$30" */
	movq $30, %r12
	/* end mov */
	/* mov ~dst:"%r13" ~src:"$1" */
	movq $1, %r13
	/* end mov */
x2:
	/* conditional_jump ~cc:"le" ~srcl:"%r13" ~srcr:"%r12" ~ll:"x3" ~lr:"xEND" */
	movq %r13, %r15
	cmpq %r12, %r15
	jle x3
	jmp xEND
	/* end conditional_jump */
x3:
	/* mov ~dst:"%rdi" ~src:"$fact" */
	movq $fact, %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"%r13" */
	movq %r13, %rsi
	/* end mov */
	/* mov ~dst:"%rdx" ~src:"$.S_0" */
	movq $.S_0, %rdx
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$test_fact_impl" ~args:"" */
	subq $0, %rsp
	call test_fact_impl
	addq $0, %rsp
	/* end call */
	/* mov ~dst:"%rdi" ~src:"$fact_iter" */
	movq $fact_iter, %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"%r13" */
	movq %r13, %rsi
	/* end mov */
	/* mov ~dst:"%rdx" ~src:"$.S_1" */
	movq $.S_1, %rdx
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$test_fact_impl" ~args:"" */
	subq $0, %rsp
	call test_fact_impl
	addq $0, %rsp
	/* end call */
	/* mov ~dst:"%rdi" ~src:"$fact_adapt" */
	movq $fact_adapt, %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"%r13" */
	movq %r13, %rsi
	/* end mov */
	/* mov ~dst:"%rdx" ~src:"$.S_2" */
	movq $.S_2, %rdx
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$test_fact_impl" ~args:"" */
	subq $0, %rsp
	call test_fact_impl
	addq $0, %rsp
	/* end call */
	/* mov ~dst:"%rdi" ~src:"$fact_tailrec" */
	movq $fact_tailrec, %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"%r13" */
	movq %r13, %rsi
	/* end mov */
	/* mov ~dst:"%rdx" ~src:"$.S_3" */
	movq $.S_3, %rdx
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$test_fact_impl" ~args:"" */
	subq $0, %rsp
	call test_fact_impl
	addq $0, %rsp
	/* end call */
	/* add ~dst:"%r13" ~srcl:"%r13" ~srcr:"$1" */
	xorq %r15, %r15
	addq $1, %r15
	addq %r13, %r15
	movq %r15, %r13
	/* end add */
	jmp x2
xEND:
	/* mov ~dst:"%r12" ~src:"0(%rbp)" */
	movq 0(%rbp), %r12
	/* end mov */
	/* mov ~dst:"%r13" ~src:"-8(%rbp)" */
	movq -8(%rbp), %r13
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"$0" */
	movq $0, %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$exit" ~args:"" */
	subq $0, %rsp
	call exit
	addq $0, %rsp
	/* end call */
