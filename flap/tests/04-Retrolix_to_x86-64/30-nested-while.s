.data
n2:
	/* n2 */
	.quad 0
main37:
	/* main37 */
	.quad 0
m1:
	/* m1 */
	.quad 0
.text
	.globl main
.p2align 3, 144
main:
	/* Program entry point. */
	subq $8, %rsp
	call .I_833836167
	call .I_600030479
	call .I_325745432
	movq $0, %rdi
	call exit
.p2align 3, 144
.I_833836167:
	/* Initializer for m1. */
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	/* end prolog */
	/* mov ~dst:"m1(%rip)" ~src:"$10" */
	movq $10, m1(%rip)
	/* end mov */
	/* start epilog */
	addq $0, %rsp
	popq %rbp
	/* end epilog */
	ret
.p2align 3, 144
.I_600030479:
	/* Initializer for n2. */
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	/* end prolog */
	/* mov ~dst:"n2(%rip)" ~src:"$10" */
	movq $10, n2(%rip)
	/* end mov */
	/* start epilog */
	addq $0, %rsp
	popq %rbp
	/* end epilog */
	ret
.p2align 3, 144
ref:
	/* Retrolix function ref. */
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	subq $64, %rsp
	/* end prolog */
	/* mov ~dst:"-56(%rbp)" ~src:"%rdi" */
	movq %rdi, -56(%rbp)
	/* end mov */
	/* mov ~dst:"-32(%rbp)" ~src:"$1" */
	movq $1, -32(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-32(%rbp)" */
	movq -32(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$allocate_block" ~args:"" */
	call allocate_block
	/* end call */
	/* mov ~dst:"-48(%rbp)" ~src:"%rax" */
	movq %rax, -48(%rbp)
	/* end mov */
	/* mov ~dst:"-8(%rbp)" ~src:"-48(%rbp)" */
	movq -48(%rbp), %r15
	movq %r15, -8(%rbp)
	/* end mov */
	/* mov ~dst:"-16(%rbp)" ~src:"$0" */
	movq $0, -16(%rbp)
	/* end mov */
	/* mov ~dst:"-24(%rbp)" ~src:"-56(%rbp)" */
	movq -56(%rbp), %r15
	movq %r15, -24(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-8(%rbp)" */
	movq -8(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-16(%rbp)" */
	movq -16(%rbp), %rsi
	/* end mov */
	/* mov ~dst:"%rdx" ~src:"-24(%rbp)" */
	movq -24(%rbp), %rdx
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$write_block" ~args:"" */
	call write_block
	/* end call */
	/* mov ~dst:"-40(%rbp)" ~src:"%rax" */
	movq %rax, -40(%rbp)
	/* end mov */
	/* mov ~dst:"0(%rbp)" ~src:"-48(%rbp)" */
	movq -48(%rbp), %r15
	movq %r15, 0(%rbp)
	/* end mov */
	/* mov ~dst:"%rax" ~src:"0(%rbp)" */
	movq 0(%rbp), %rax
	/* end mov */
	/* start epilog */
	addq $64, %rsp
	popq %rbp
	/* end epilog */
	ret
.p2align 3, 144
read:
	/* Retrolix function read. */
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	subq $32, %rsp
	/* end prolog */
	/* mov ~dst:"-24(%rbp)" ~src:"%rdi" */
	movq %rdi, -24(%rbp)
	/* end mov */
	/* mov ~dst:"-8(%rbp)" ~src:"-24(%rbp)" */
	movq -24(%rbp), %r15
	movq %r15, -8(%rbp)
	/* end mov */
	/* mov ~dst:"-16(%rbp)" ~src:"$0" */
	movq $0, -16(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-8(%rbp)" */
	movq -8(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-16(%rbp)" */
	movq -16(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read_block" ~args:"" */
	call read_block
	/* end call */
	/* mov ~dst:"0(%rbp)" ~src:"%rax" */
	movq %rax, 0(%rbp)
	/* end mov */
	/* mov ~dst:"%rax" ~src:"0(%rbp)" */
	movq 0(%rbp), %rax
	/* end mov */
	/* start epilog */
	addq $32, %rsp
	popq %rbp
	/* end epilog */
	ret
.p2align 3, 144
write:
	/* Retrolix function write. */
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	subq $48, %rsp
	/* end prolog */
	/* mov ~dst:"-32(%rbp)" ~src:"%rdi" */
	movq %rdi, -32(%rbp)
	/* end mov */
	/* mov ~dst:"-40(%rbp)" ~src:"%rsi" */
	movq %rsi, -40(%rbp)
	/* end mov */
	/* mov ~dst:"-8(%rbp)" ~src:"-32(%rbp)" */
	movq -32(%rbp), %r15
	movq %r15, -8(%rbp)
	/* end mov */
	/* mov ~dst:"-16(%rbp)" ~src:"$0" */
	movq $0, -16(%rbp)
	/* end mov */
	/* mov ~dst:"-24(%rbp)" ~src:"-40(%rbp)" */
	movq -40(%rbp), %r15
	movq %r15, -24(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-8(%rbp)" */
	movq -8(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-16(%rbp)" */
	movq -16(%rbp), %rsi
	/* end mov */
	/* mov ~dst:"%rdx" ~src:"-24(%rbp)" */
	movq -24(%rbp), %rdx
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$write_block" ~args:"" */
	call write_block
	/* end call */
	/* mov ~dst:"0(%rbp)" ~src:"%rax" */
	movq %rax, 0(%rbp)
	/* end mov */
	/* mov ~dst:"%rax" ~src:"0(%rbp)" */
	movq 0(%rbp), %rax
	/* end mov */
	/* start epilog */
	addq $48, %rsp
	popq %rbp
	/* end epilog */
	ret
.p2align 3, 144
make_matrix:
	/* Retrolix function make_matrix. */
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	subq $392, %rsp
	/* end prolog */
	/* mov ~dst:"-368(%rbp)" ~src:"%rdi" */
	movq %rdi, -368(%rbp)
	/* end mov */
	/* mov ~dst:"-376(%rbp)" ~src:"%rsi" */
	movq %rsi, -376(%rbp)
	/* end mov */
	/* mov ~dst:"-240(%rbp)" ~src:"-368(%rbp)" */
	movq -368(%rbp), %r15
	movq %r15, -240(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-240(%rbp)" */
	movq -240(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$allocate_block" ~args:"" */
	call allocate_block
	/* end call */
	/* mov ~dst:"-384(%rbp)" ~src:"%rax" */
	movq %rax, -384(%rbp)
	/* end mov */
	/* mov ~dst:"-224(%rbp)" ~src:"-368(%rbp)" */
	movq -368(%rbp), %r15
	movq %r15, -224(%rbp)
	/* end mov */
	/* mov ~dst:"-232(%rbp)" ~src:"$1" */
	movq $1, -232(%rbp)
	/* end mov */
	/* sub ~dst:"-216(%rbp)" ~srcl:"-224(%rbp)" ~srcr:"-232(%rbp)" */
	xorq %r15, %r15
	subq -232(%rbp), %r15
	addq -224(%rbp), %r15
	movq %r15, -216(%rbp)
	/* end sub */
	/* mov ~dst:"%rdi" ~src:"-216(%rbp)" */
	movq -216(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$ref" ~args:"" */
	call ref
	/* end call */
	/* mov ~dst:"-352(%rbp)" ~src:"%rax" */
	movq %rax, -352(%rbp)
	/* end mov */
l325:
	/* mov ~dst:"-200(%rbp)" ~src:"-352(%rbp)" */
	movq -352(%rbp), %r15
	movq %r15, -200(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-200(%rbp)" */
	movq -200(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-192(%rbp)" ~src:"%rax" */
	movq %rax, -192(%rbp)
	/* end mov */
	/* mov ~dst:"-208(%rbp)" ~src:"$0" */
	movq $0, -208(%rbp)
	/* end mov */
	/* conditional_jump ~cc:"ge" ~srcl:"-192(%rbp)" ~srcr:"-208(%rbp)" ~ll:"l280" ~lr:"l314" */
	movq -192(%rbp), %r15
	cmpq -208(%rbp), %r15
	jge l280
	jmp l314
	/* end conditional_jump */
l280:
	/* mov ~dst:"-152(%rbp)" ~src:"-384(%rbp)" */
	movq -384(%rbp), %r15
	movq %r15, -152(%rbp)
	/* end mov */
	/* mov ~dst:"-168(%rbp)" ~src:"-352(%rbp)" */
	movq -352(%rbp), %r15
	movq %r15, -168(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-168(%rbp)" */
	movq -168(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-160(%rbp)" ~src:"%rax" */
	movq %rax, -160(%rbp)
	/* end mov */
	/* mov ~dst:"-184(%rbp)" ~src:"-376(%rbp)" */
	movq -376(%rbp), %r15
	movq %r15, -184(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-184(%rbp)" */
	movq -184(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$allocate_block" ~args:"" */
	call allocate_block
	/* end call */
	/* mov ~dst:"-176(%rbp)" ~src:"%rax" */
	movq %rax, -176(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-152(%rbp)" */
	movq -152(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-160(%rbp)" */
	movq -160(%rbp), %rsi
	/* end mov */
	/* mov ~dst:"%rdx" ~src:"-176(%rbp)" */
	movq -176(%rbp), %rdx
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$write_block" ~args:"" */
	call write_block
	/* end call */
	/* mov ~dst:"-328(%rbp)" ~src:"%rax" */
	movq %rax, -328(%rbp)
	/* end mov */
	/* mov ~dst:"-136(%rbp)" ~src:"-376(%rbp)" */
	movq -376(%rbp), %r15
	movq %r15, -136(%rbp)
	/* end mov */
	/* mov ~dst:"-144(%rbp)" ~src:"$1" */
	movq $1, -144(%rbp)
	/* end mov */
	/* sub ~dst:"-128(%rbp)" ~srcl:"-136(%rbp)" ~srcr:"-144(%rbp)" */
	xorq %r15, %r15
	subq -144(%rbp), %r15
	addq -136(%rbp), %r15
	movq %r15, -128(%rbp)
	/* end sub */
	/* mov ~dst:"%rdi" ~src:"-128(%rbp)" */
	movq -128(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$ref" ~args:"" */
	call ref
	/* end call */
	/* mov ~dst:"-360(%rbp)" ~src:"%rax" */
	movq %rax, -360(%rbp)
	/* end mov */
l247:
	/* mov ~dst:"-112(%rbp)" ~src:"-360(%rbp)" */
	movq -360(%rbp), %r15
	movq %r15, -112(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-112(%rbp)" */
	movq -112(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-104(%rbp)" ~src:"%rax" */
	movq %rax, -104(%rbp)
	/* end mov */
	/* mov ~dst:"-120(%rbp)" ~src:"$0" */
	movq $0, -120(%rbp)
	/* end mov */
	/* conditional_jump ~cc:"ge" ~srcl:"-104(%rbp)" ~srcr:"-120(%rbp)" ~ll:"l169" ~lr:"l236" */
	movq -104(%rbp), %r15
	cmpq -120(%rbp), %r15
	jge l169
	jmp l236
	/* end conditional_jump */
l169:
	/* mov ~dst:"-24(%rbp)" ~src:"-384(%rbp)" */
	movq -384(%rbp), %r15
	movq %r15, -24(%rbp)
	/* end mov */
	/* mov ~dst:"-40(%rbp)" ~src:"-352(%rbp)" */
	movq -352(%rbp), %r15
	movq %r15, -40(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-40(%rbp)" */
	movq -40(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-32(%rbp)" ~src:"%rax" */
	movq %rax, -32(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-24(%rbp)" */
	movq -24(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-32(%rbp)" */
	movq -32(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read_block" ~args:"" */
	call read_block
	/* end call */
	/* mov ~dst:"-16(%rbp)" ~src:"%rax" */
	movq %rax, -16(%rbp)
	/* end mov */
	/* mov ~dst:"-56(%rbp)" ~src:"-360(%rbp)" */
	movq -360(%rbp), %r15
	movq %r15, -56(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-56(%rbp)" */
	movq -56(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-48(%rbp)" ~src:"%rax" */
	movq %rax, -48(%rbp)
	/* end mov */
	/* mov ~dst:"-80(%rbp)" ~src:"-360(%rbp)" */
	movq -360(%rbp), %r15
	movq %r15, -80(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-80(%rbp)" */
	movq -80(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-72(%rbp)" ~src:"%rax" */
	movq %rax, -72(%rbp)
	/* end mov */
	/* mov ~dst:"-96(%rbp)" ~src:"-352(%rbp)" */
	movq -352(%rbp), %r15
	movq %r15, -96(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-96(%rbp)" */
	movq -96(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-88(%rbp)" ~src:"%rax" */
	movq %rax, -88(%rbp)
	/* end mov */
	/* add ~dst:"-64(%rbp)" ~srcl:"-72(%rbp)" ~srcr:"-88(%rbp)" */
	xorq %r15, %r15
	addq -88(%rbp), %r15
	addq -72(%rbp), %r15
	movq %r15, -64(%rbp)
	/* end add */
	/* mov ~dst:"%rdi" ~src:"-16(%rbp)" */
	movq -16(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-48(%rbp)" */
	movq -48(%rbp), %rsi
	/* end mov */
	/* mov ~dst:"%rdx" ~src:"-64(%rbp)" */
	movq -64(%rbp), %rdx
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$write_block" ~args:"" */
	call write_block
	/* end call */
	/* mov ~dst:"-344(%rbp)" ~src:"%rax" */
	movq %rax, -344(%rbp)
	/* end mov */
	/* mov ~dst:"-296(%rbp)" ~src:"-360(%rbp)" */
	movq -360(%rbp), %r15
	movq %r15, -296(%rbp)
	/* end mov */
	/* mov ~dst:"0(%rbp)" ~src:"-360(%rbp)" */
	movq -360(%rbp), %r15
	movq %r15, 0(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"0(%rbp)" */
	movq 0(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-312(%rbp)" ~src:"%rax" */
	movq %rax, -312(%rbp)
	/* end mov */
	/* mov ~dst:"-8(%rbp)" ~src:"$1" */
	movq $1, -8(%rbp)
	/* end mov */
	/* sub ~dst:"-304(%rbp)" ~srcl:"-312(%rbp)" ~srcr:"-8(%rbp)" */
	xorq %r15, %r15
	subq -8(%rbp), %r15
	addq -312(%rbp), %r15
	movq %r15, -304(%rbp)
	/* end sub */
	/* mov ~dst:"%rdi" ~src:"-296(%rbp)" */
	movq -296(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-304(%rbp)" */
	movq -304(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$write" ~args:"" */
	call write
	/* end call */
	/* mov ~dst:"-336(%rbp)" ~src:"%rax" */
	movq %rax, -336(%rbp)
	/* end mov */
	jmp l247
l236:
	/*  Exit of while loop */
	/* mov ~dst:"-256(%rbp)" ~src:"-352(%rbp)" */
	movq -352(%rbp), %r15
	movq %r15, -256(%rbp)
	/* end mov */
	/* mov ~dst:"-280(%rbp)" ~src:"-352(%rbp)" */
	movq -352(%rbp), %r15
	movq %r15, -280(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-280(%rbp)" */
	movq -280(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-272(%rbp)" ~src:"%rax" */
	movq %rax, -272(%rbp)
	/* end mov */
	/* mov ~dst:"-288(%rbp)" ~src:"$1" */
	movq $1, -288(%rbp)
	/* end mov */
	/* sub ~dst:"-264(%rbp)" ~srcl:"-272(%rbp)" ~srcr:"-288(%rbp)" */
	xorq %r15, %r15
	subq -288(%rbp), %r15
	addq -272(%rbp), %r15
	movq %r15, -264(%rbp)
	/* end sub */
	/* mov ~dst:"%rdi" ~src:"-256(%rbp)" */
	movq -256(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-264(%rbp)" */
	movq -264(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$write" ~args:"" */
	call write
	/* end call */
	/* mov ~dst:"-320(%rbp)" ~src:"%rax" */
	movq %rax, -320(%rbp)
	/* end mov */
	jmp l325
l314:
	/*  Exit of while loop */
	/* mov ~dst:"-248(%rbp)" ~src:"-384(%rbp)" */
	movq -384(%rbp), %r15
	movq %r15, -248(%rbp)
	/* end mov */
	/* mov ~dst:"%rax" ~src:"-248(%rbp)" */
	movq -248(%rbp), %rax
	/* end mov */
	/* start epilog */
	addq $392, %rsp
	popq %rbp
	/* end epilog */
	ret
.p2align 3, 144
transpose:
	/* Retrolix function transpose. */
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	subq $376, %rsp
	/* end prolog */
	/* mov ~dst:"-344(%rbp)" ~src:"%rdi" */
	movq %rdi, -344(%rbp)
	/* end mov */
	/* mov ~dst:"-336(%rbp)" ~src:"%rsi" */
	movq %rsi, -336(%rbp)
	/* end mov */
	/* mov ~dst:"-352(%rbp)" ~src:"%rdx" */
	movq %rdx, -352(%rbp)
	/* end mov */
	/* mov ~dst:"-288(%rbp)" ~src:"-352(%rbp)" */
	movq -352(%rbp), %r15
	movq %r15, -288(%rbp)
	/* end mov */
	/* mov ~dst:"-296(%rbp)" ~src:"-336(%rbp)" */
	movq -336(%rbp), %r15
	movq %r15, -296(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-288(%rbp)" */
	movq -288(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-296(%rbp)" */
	movq -296(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$make_matrix" ~args:"" */
	call make_matrix
	/* end call */
	/* mov ~dst:"-360(%rbp)" ~src:"%rax" */
	movq %rax, -360(%rbp)
	/* end mov */
	/* mov ~dst:"-272(%rbp)" ~src:"-352(%rbp)" */
	movq -352(%rbp), %r15
	movq %r15, -272(%rbp)
	/* end mov */
	/* mov ~dst:"-280(%rbp)" ~src:"$1" */
	movq $1, -280(%rbp)
	/* end mov */
	/* sub ~dst:"-264(%rbp)" ~srcl:"-272(%rbp)" ~srcr:"-280(%rbp)" */
	xorq %r15, %r15
	subq -280(%rbp), %r15
	addq -272(%rbp), %r15
	movq %r15, -264(%rbp)
	/* end sub */
	/* mov ~dst:"%rdi" ~src:"-264(%rbp)" */
	movq -264(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$ref" ~args:"" */
	call ref
	/* end call */
	/* mov ~dst:"-368(%rbp)" ~src:"%rax" */
	movq %rax, -368(%rbp)
	/* end mov */
l590:
	/* mov ~dst:"-248(%rbp)" ~src:"-368(%rbp)" */
	movq -368(%rbp), %r15
	movq %r15, -248(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-248(%rbp)" */
	movq -248(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-240(%rbp)" ~src:"%rax" */
	movq %rax, -240(%rbp)
	/* end mov */
	/* mov ~dst:"-256(%rbp)" ~src:"$0" */
	movq $0, -256(%rbp)
	/* end mov */
	/* conditional_jump ~cc:"ge" ~srcl:"-240(%rbp)" ~srcr:"-256(%rbp)" ~ll:"l573" ~lr:"l579" */
	movq -240(%rbp), %r15
	cmpq -256(%rbp), %r15
	jge l573
	jmp l579
	/* end conditional_jump */
l573:
	/* mov ~dst:"-224(%rbp)" ~src:"-336(%rbp)" */
	movq -336(%rbp), %r15
	movq %r15, -224(%rbp)
	/* end mov */
	/* mov ~dst:"-232(%rbp)" ~src:"$1" */
	movq $1, -232(%rbp)
	/* end mov */
	/* sub ~dst:"-216(%rbp)" ~srcl:"-224(%rbp)" ~srcr:"-232(%rbp)" */
	xorq %r15, %r15
	subq -232(%rbp), %r15
	addq -224(%rbp), %r15
	movq %r15, -216(%rbp)
	/* end sub */
	/* mov ~dst:"%rdi" ~src:"-216(%rbp)" */
	movq -216(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$ref" ~args:"" */
	call ref
	/* end call */
	/* mov ~dst:"-328(%rbp)" ~src:"%rax" */
	movq %rax, -328(%rbp)
	/* end mov */
l556:
	/* mov ~dst:"-200(%rbp)" ~src:"-328(%rbp)" */
	movq -328(%rbp), %r15
	movq %r15, -200(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-200(%rbp)" */
	movq -200(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-192(%rbp)" ~src:"%rax" */
	movq %rax, -192(%rbp)
	/* end mov */
	/* mov ~dst:"-208(%rbp)" ~src:"$0" */
	movq $0, -208(%rbp)
	/* end mov */
	/* conditional_jump ~cc:"ge" ~srcl:"-192(%rbp)" ~srcr:"-208(%rbp)" ~ll:"l450" ~lr:"l545" */
	movq -192(%rbp), %r15
	cmpq -208(%rbp), %r15
	jge l450
	jmp l545
	/* end conditional_jump */
l450:
	/* mov ~dst:"-96(%rbp)" ~src:"-360(%rbp)" */
	movq -360(%rbp), %r15
	movq %r15, -96(%rbp)
	/* end mov */
	/* mov ~dst:"-112(%rbp)" ~src:"-368(%rbp)" */
	movq -368(%rbp), %r15
	movq %r15, -112(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-112(%rbp)" */
	movq -112(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-104(%rbp)" ~src:"%rax" */
	movq %rax, -104(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-96(%rbp)" */
	movq -96(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-104(%rbp)" */
	movq -104(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read_block" ~args:"" */
	call read_block
	/* end call */
	/* mov ~dst:"-88(%rbp)" ~src:"%rax" */
	movq %rax, -88(%rbp)
	/* end mov */
	/* mov ~dst:"-128(%rbp)" ~src:"-328(%rbp)" */
	movq -328(%rbp), %r15
	movq %r15, -128(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-128(%rbp)" */
	movq -128(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-120(%rbp)" ~src:"%rax" */
	movq %rax, -120(%rbp)
	/* end mov */
	/* mov ~dst:"-152(%rbp)" ~src:"-344(%rbp)" */
	movq -344(%rbp), %r15
	movq %r15, -152(%rbp)
	/* end mov */
	/* mov ~dst:"-168(%rbp)" ~src:"-328(%rbp)" */
	movq -328(%rbp), %r15
	movq %r15, -168(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-168(%rbp)" */
	movq -168(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-160(%rbp)" ~src:"%rax" */
	movq %rax, -160(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-152(%rbp)" */
	movq -152(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-160(%rbp)" */
	movq -160(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read_block" ~args:"" */
	call read_block
	/* end call */
	/* mov ~dst:"-144(%rbp)" ~src:"%rax" */
	movq %rax, -144(%rbp)
	/* end mov */
	/* mov ~dst:"-184(%rbp)" ~src:"-368(%rbp)" */
	movq -368(%rbp), %r15
	movq %r15, -184(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-184(%rbp)" */
	movq -184(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-176(%rbp)" ~src:"%rax" */
	movq %rax, -176(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-144(%rbp)" */
	movq -144(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-176(%rbp)" */
	movq -176(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read_block" ~args:"" */
	call read_block
	/* end call */
	/* mov ~dst:"-136(%rbp)" ~src:"%rax" */
	movq %rax, -136(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-88(%rbp)" */
	movq -88(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-120(%rbp)" */
	movq -120(%rbp), %rsi
	/* end mov */
	/* mov ~dst:"%rdx" ~src:"-136(%rbp)" */
	movq -136(%rbp), %rdx
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$write_block" ~args:"" */
	call write_block
	/* end call */
	/* mov ~dst:"-320(%rbp)" ~src:"%rax" */
	movq %rax, -320(%rbp)
	/* end mov */
	/* mov ~dst:"-48(%rbp)" ~src:"-328(%rbp)" */
	movq -328(%rbp), %r15
	movq %r15, -48(%rbp)
	/* end mov */
	/* mov ~dst:"-72(%rbp)" ~src:"-328(%rbp)" */
	movq -328(%rbp), %r15
	movq %r15, -72(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-72(%rbp)" */
	movq -72(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-64(%rbp)" ~src:"%rax" */
	movq %rax, -64(%rbp)
	/* end mov */
	/* mov ~dst:"-80(%rbp)" ~src:"$1" */
	movq $1, -80(%rbp)
	/* end mov */
	/* sub ~dst:"-56(%rbp)" ~srcl:"-64(%rbp)" ~srcr:"-80(%rbp)" */
	xorq %r15, %r15
	subq -80(%rbp), %r15
	addq -64(%rbp), %r15
	movq %r15, -56(%rbp)
	/* end sub */
	/* mov ~dst:"%rdi" ~src:"-48(%rbp)" */
	movq -48(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-56(%rbp)" */
	movq -56(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$write" ~args:"" */
	call write
	/* end call */
	/* mov ~dst:"-312(%rbp)" ~src:"%rax" */
	movq %rax, -312(%rbp)
	/* end mov */
	jmp l556
l545:
	/*  Exit of while loop */
	/* mov ~dst:"-8(%rbp)" ~src:"-368(%rbp)" */
	movq -368(%rbp), %r15
	movq %r15, -8(%rbp)
	/* end mov */
	/* mov ~dst:"-32(%rbp)" ~src:"-368(%rbp)" */
	movq -368(%rbp), %r15
	movq %r15, -32(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-32(%rbp)" */
	movq -32(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-24(%rbp)" ~src:"%rax" */
	movq %rax, -24(%rbp)
	/* end mov */
	/* mov ~dst:"-40(%rbp)" ~src:"$1" */
	movq $1, -40(%rbp)
	/* end mov */
	/* sub ~dst:"-16(%rbp)" ~srcl:"-24(%rbp)" ~srcr:"-40(%rbp)" */
	xorq %r15, %r15
	subq -40(%rbp), %r15
	addq -24(%rbp), %r15
	movq %r15, -16(%rbp)
	/* end sub */
	/* mov ~dst:"%rdi" ~src:"-8(%rbp)" */
	movq -8(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-16(%rbp)" */
	movq -16(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$write" ~args:"" */
	call write
	/* end call */
	/* mov ~dst:"-304(%rbp)" ~src:"%rax" */
	movq %rax, -304(%rbp)
	/* end mov */
	jmp l590
l579:
	/*  Exit of while loop */
	/* mov ~dst:"0(%rbp)" ~src:"-360(%rbp)" */
	movq -360(%rbp), %r15
	movq %r15, 0(%rbp)
	/* end mov */
	/* mov ~dst:"%rax" ~src:"0(%rbp)" */
	movq 0(%rbp), %rax
	/* end mov */
	/* start epilog */
	addq $376, %rsp
	popq %rbp
	/* end epilog */
	ret
.p2align 3, 144
check:
	/* Retrolix function check. */
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	subq $456, %rsp
	/* end prolog */
	/* mov ~dst:"-416(%rbp)" ~src:"%rdi" */
	movq %rdi, -416(%rbp)
	/* end mov */
	/* mov ~dst:"-424(%rbp)" ~src:"%rsi" */
	movq %rsi, -424(%rbp)
	/* end mov */
	/* mov ~dst:"-432(%rbp)" ~src:"%rdx" */
	movq %rdx, -432(%rbp)
	/* end mov */
	/* mov ~dst:"-440(%rbp)" ~src:"%rcx" */
	movq %rcx, -440(%rbp)
	/* end mov */
	/* mov ~dst:"-368(%rbp)" ~src:"$0" */
	movq $0, -368(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-368(%rbp)" */
	movq -368(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$ref" ~args:"" */
	call ref
	/* end call */
	/* mov ~dst:"-408(%rbp)" ~src:"%rax" */
	movq %rax, -408(%rbp)
	/* end mov */
	/* mov ~dst:"-352(%rbp)" ~src:"-432(%rbp)" */
	movq -432(%rbp), %r15
	movq %r15, -352(%rbp)
	/* end mov */
	/* mov ~dst:"-360(%rbp)" ~src:"$1" */
	movq $1, -360(%rbp)
	/* end mov */
	/* sub ~dst:"-344(%rbp)" ~srcl:"-352(%rbp)" ~srcr:"-360(%rbp)" */
	xorq %r15, %r15
	subq -360(%rbp), %r15
	addq -352(%rbp), %r15
	movq %r15, -344(%rbp)
	/* end sub */
	/* mov ~dst:"%rdi" ~src:"-344(%rbp)" */
	movq -344(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$ref" ~args:"" */
	call ref
	/* end call */
	/* mov ~dst:"-448(%rbp)" ~src:"%rax" */
	movq %rax, -448(%rbp)
	/* end mov */
l944:
	/* mov ~dst:"-328(%rbp)" ~src:"-448(%rbp)" */
	movq -448(%rbp), %r15
	movq %r15, -328(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-328(%rbp)" */
	movq -328(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-320(%rbp)" ~src:"%rax" */
	movq %rax, -320(%rbp)
	/* end mov */
	/* mov ~dst:"-336(%rbp)" ~src:"$0" */
	movq $0, -336(%rbp)
	/* end mov */
	/* conditional_jump ~cc:"ge" ~srcl:"-320(%rbp)" ~srcr:"-336(%rbp)" ~ll:"l927" ~lr:"l933" */
	movq -320(%rbp), %r15
	cmpq -336(%rbp), %r15
	jge l927
	jmp l933
	/* end conditional_jump */
l927:
	/* mov ~dst:"-304(%rbp)" ~src:"-440(%rbp)" */
	movq -440(%rbp), %r15
	movq %r15, -304(%rbp)
	/* end mov */
	/* mov ~dst:"-312(%rbp)" ~src:"$1" */
	movq $1, -312(%rbp)
	/* end mov */
	/* sub ~dst:"-296(%rbp)" ~srcl:"-304(%rbp)" ~srcr:"-312(%rbp)" */
	xorq %r15, %r15
	subq -312(%rbp), %r15
	addq -304(%rbp), %r15
	movq %r15, -296(%rbp)
	/* end sub */
	/* mov ~dst:"%rdi" ~src:"-296(%rbp)" */
	movq -296(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$ref" ~args:"" */
	call ref
	/* end call */
	/* mov ~dst:"-400(%rbp)" ~src:"%rax" */
	movq %rax, -400(%rbp)
	/* end mov */
l910:
	/* mov ~dst:"-280(%rbp)" ~src:"-400(%rbp)" */
	movq -400(%rbp), %r15
	movq %r15, -280(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-280(%rbp)" */
	movq -280(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-272(%rbp)" ~src:"%rax" */
	movq %rax, -272(%rbp)
	/* end mov */
	/* mov ~dst:"-288(%rbp)" ~src:"$0" */
	movq $0, -288(%rbp)
	/* end mov */
	/* conditional_jump ~cc:"ge" ~srcl:"-272(%rbp)" ~srcr:"-288(%rbp)" ~ll:"l804" ~lr:"l899" */
	movq -272(%rbp), %r15
	cmpq -288(%rbp), %r15
	jge l804
	jmp l899
	/* end conditional_jump */
l804:
	/* mov ~dst:"-176(%rbp)" ~src:"-416(%rbp)" */
	movq -416(%rbp), %r15
	movq %r15, -176(%rbp)
	/* end mov */
	/* mov ~dst:"-192(%rbp)" ~src:"-448(%rbp)" */
	movq -448(%rbp), %r15
	movq %r15, -192(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-192(%rbp)" */
	movq -192(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-184(%rbp)" ~src:"%rax" */
	movq %rax, -184(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-176(%rbp)" */
	movq -176(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-184(%rbp)" */
	movq -184(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read_block" ~args:"" */
	call read_block
	/* end call */
	/* mov ~dst:"-168(%rbp)" ~src:"%rax" */
	movq %rax, -168(%rbp)
	/* end mov */
	/* mov ~dst:"-208(%rbp)" ~src:"-400(%rbp)" */
	movq -400(%rbp), %r15
	movq %r15, -208(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-208(%rbp)" */
	movq -208(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-200(%rbp)" ~src:"%rax" */
	movq %rax, -200(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-168(%rbp)" */
	movq -168(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-200(%rbp)" */
	movq -200(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read_block" ~args:"" */
	call read_block
	/* end call */
	/* mov ~dst:"-160(%rbp)" ~src:"%rax" */
	movq %rax, -160(%rbp)
	/* end mov */
	/* mov ~dst:"-232(%rbp)" ~src:"-424(%rbp)" */
	movq -424(%rbp), %r15
	movq %r15, -232(%rbp)
	/* end mov */
	/* mov ~dst:"-248(%rbp)" ~src:"-448(%rbp)" */
	movq -448(%rbp), %r15
	movq %r15, -248(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-248(%rbp)" */
	movq -248(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-240(%rbp)" ~src:"%rax" */
	movq %rax, -240(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-232(%rbp)" */
	movq -232(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-240(%rbp)" */
	movq -240(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read_block" ~args:"" */
	call read_block
	/* end call */
	/* mov ~dst:"-224(%rbp)" ~src:"%rax" */
	movq %rax, -224(%rbp)
	/* end mov */
	/* mov ~dst:"-264(%rbp)" ~src:"-400(%rbp)" */
	movq -400(%rbp), %r15
	movq %r15, -264(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-264(%rbp)" */
	movq -264(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-256(%rbp)" ~src:"%rax" */
	movq %rax, -256(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-224(%rbp)" */
	movq -224(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-256(%rbp)" */
	movq -256(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read_block" ~args:"" */
	call read_block
	/* end call */
	/* mov ~dst:"-216(%rbp)" ~src:"%rax" */
	movq %rax, -216(%rbp)
	/* end mov */
	/* conditional_jump ~cc:"e" ~srcl:"-160(%rbp)" ~srcr:"-216(%rbp)" ~ll:"l734" ~lr:"l775" */
	movq -160(%rbp), %r15
	cmpq -216(%rbp), %r15
	je l734
	jmp l775
	/* end conditional_jump */
l734:
	/* mov ~dst:"-104(%rbp)" ~src:"-408(%rbp)" */
	movq -408(%rbp), %r15
	movq %r15, -104(%rbp)
	/* end mov */
	/* mov ~dst:"-128(%rbp)" ~src:"-408(%rbp)" */
	movq -408(%rbp), %r15
	movq %r15, -128(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-128(%rbp)" */
	movq -128(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-120(%rbp)" ~src:"%rax" */
	movq %rax, -120(%rbp)
	/* end mov */
	/* mov ~dst:"-136(%rbp)" ~src:"$1" */
	movq $1, -136(%rbp)
	/* end mov */
	/* add ~dst:"-112(%rbp)" ~srcl:"-120(%rbp)" ~srcr:"-136(%rbp)" */
	xorq %r15, %r15
	addq -136(%rbp), %r15
	addq -120(%rbp), %r15
	movq %r15, -112(%rbp)
	/* end add */
	/* mov ~dst:"%rdi" ~src:"-104(%rbp)" */
	movq -104(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-112(%rbp)" */
	movq -112(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$write" ~args:"" */
	call write
	/* end call */
	/* mov ~dst:"-392(%rbp)" ~src:"%rax" */
	movq %rax, -392(%rbp)
	/* end mov */
	jmp l782
l775:
	/* mov ~dst:"-152(%rbp)" ~src:"-408(%rbp)" */
	movq -408(%rbp), %r15
	movq %r15, -152(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-152(%rbp)" */
	movq -152(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-144(%rbp)" ~src:"%rax" */
	movq %rax, -144(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-144(%rbp)" */
	movq -144(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$print_int" ~args:"" */
	call print_int
	/* end call */
	/* mov ~dst:"-392(%rbp)" ~src:"%rax" */
	movq %rax, -392(%rbp)
	/* end mov */
l782:
	/*  Join control point */
	/* mov ~dst:"-64(%rbp)" ~src:"-400(%rbp)" */
	movq -400(%rbp), %r15
	movq %r15, -64(%rbp)
	/* end mov */
	/* mov ~dst:"-88(%rbp)" ~src:"-400(%rbp)" */
	movq -400(%rbp), %r15
	movq %r15, -88(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-88(%rbp)" */
	movq -88(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-80(%rbp)" ~src:"%rax" */
	movq %rax, -80(%rbp)
	/* end mov */
	/* mov ~dst:"-96(%rbp)" ~src:"$1" */
	movq $1, -96(%rbp)
	/* end mov */
	/* sub ~dst:"-72(%rbp)" ~srcl:"-80(%rbp)" ~srcr:"-96(%rbp)" */
	xorq %r15, %r15
	subq -96(%rbp), %r15
	addq -80(%rbp), %r15
	movq %r15, -72(%rbp)
	/* end sub */
	/* mov ~dst:"%rdi" ~src:"-64(%rbp)" */
	movq -64(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-72(%rbp)" */
	movq -72(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$write" ~args:"" */
	call write
	/* end call */
	/* mov ~dst:"-384(%rbp)" ~src:"%rax" */
	movq %rax, -384(%rbp)
	/* end mov */
	jmp l910
l899:
	/*  Exit of while loop */
	/* mov ~dst:"-24(%rbp)" ~src:"-448(%rbp)" */
	movq -448(%rbp), %r15
	movq %r15, -24(%rbp)
	/* end mov */
	/* mov ~dst:"-48(%rbp)" ~src:"-448(%rbp)" */
	movq -448(%rbp), %r15
	movq %r15, -48(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-48(%rbp)" */
	movq -48(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-40(%rbp)" ~src:"%rax" */
	movq %rax, -40(%rbp)
	/* end mov */
	/* mov ~dst:"-56(%rbp)" ~src:"$1" */
	movq $1, -56(%rbp)
	/* end mov */
	/* sub ~dst:"-32(%rbp)" ~srcl:"-40(%rbp)" ~srcr:"-56(%rbp)" */
	xorq %r15, %r15
	subq -56(%rbp), %r15
	addq -40(%rbp), %r15
	movq %r15, -32(%rbp)
	/* end sub */
	/* mov ~dst:"%rdi" ~src:"-24(%rbp)" */
	movq -24(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-32(%rbp)" */
	movq -32(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$write" ~args:"" */
	call write
	/* end call */
	/* mov ~dst:"-376(%rbp)" ~src:"%rax" */
	movq %rax, -376(%rbp)
	/* end mov */
	jmp l944
l933:
	/*  Exit of while loop */
	/* mov ~dst:"-16(%rbp)" ~src:"-408(%rbp)" */
	movq -408(%rbp), %r15
	movq %r15, -16(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-16(%rbp)" */
	movq -16(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$read" ~args:"" */
	call read
	/* end call */
	/* mov ~dst:"-8(%rbp)" ~src:"%rax" */
	movq %rax, -8(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-8(%rbp)" */
	movq -8(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$print_int" ~args:"" */
	call print_int
	/* end call */
	/* mov ~dst:"0(%rbp)" ~src:"%rax" */
	movq %rax, 0(%rbp)
	/* end mov */
	/* mov ~dst:"%rax" ~src:"0(%rbp)" */
	movq 0(%rbp), %rax
	/* end mov */
	/* start epilog */
	addq $456, %rsp
	popq %rbp
	/* end epilog */
	ret
.p2align 3, 144
.I_325745432:
	/* Initializer for main37. */
	/* start prolog */
	pushq %rbp
	movq %rsp, %rbp
	subq $136, %rsp
	/* end prolog */
	/* mov ~dst:"-88(%rbp)" ~src:"m1(%rip)" */
	movq m1(%rip), %r15
	movq %r15, -88(%rbp)
	/* end mov */
	/* mov ~dst:"-96(%rbp)" ~src:"n2(%rip)" */
	movq n2(%rip), %r15
	movq %r15, -96(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-88(%rbp)" */
	movq -88(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-96(%rbp)" */
	movq -96(%rbp), %rsi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$make_matrix" ~args:"" */
	call make_matrix
	/* end call */
	/* mov ~dst:"-112(%rbp)" ~src:"%rax" */
	movq %rax, -112(%rbp)
	/* end mov */
	/* mov ~dst:"-64(%rbp)" ~src:"-112(%rbp)" */
	movq -112(%rbp), %r15
	movq %r15, -64(%rbp)
	/* end mov */
	/* mov ~dst:"-72(%rbp)" ~src:"m1(%rip)" */
	movq m1(%rip), %r15
	movq %r15, -72(%rbp)
	/* end mov */
	/* mov ~dst:"-80(%rbp)" ~src:"n2(%rip)" */
	movq n2(%rip), %r15
	movq %r15, -80(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-64(%rbp)" */
	movq -64(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-72(%rbp)" */
	movq -72(%rbp), %rsi
	/* end mov */
	/* mov ~dst:"%rdx" ~src:"-80(%rbp)" */
	movq -80(%rbp), %rdx
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$transpose" ~args:"" */
	call transpose
	/* end call */
	/* mov ~dst:"-120(%rbp)" ~src:"%rax" */
	movq %rax, -120(%rbp)
	/* end mov */
	/* mov ~dst:"-40(%rbp)" ~src:"-120(%rbp)" */
	movq -120(%rbp), %r15
	movq %r15, -40(%rbp)
	/* end mov */
	/* mov ~dst:"-48(%rbp)" ~src:"n2(%rip)" */
	movq n2(%rip), %r15
	movq %r15, -48(%rbp)
	/* end mov */
	/* mov ~dst:"-56(%rbp)" ~src:"m1(%rip)" */
	movq m1(%rip), %r15
	movq %r15, -56(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-40(%rbp)" */
	movq -40(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-48(%rbp)" */
	movq -48(%rbp), %rsi
	/* end mov */
	/* mov ~dst:"%rdx" ~src:"-56(%rbp)" */
	movq -56(%rbp), %rdx
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$transpose" ~args:"" */
	call transpose
	/* end call */
	/* mov ~dst:"-128(%rbp)" ~src:"%rax" */
	movq %rax, -128(%rbp)
	/* end mov */
	/* mov ~dst:"-8(%rbp)" ~src:"-112(%rbp)" */
	movq -112(%rbp), %r15
	movq %r15, -8(%rbp)
	/* end mov */
	/* mov ~dst:"-16(%rbp)" ~src:"-128(%rbp)" */
	movq -128(%rbp), %r15
	movq %r15, -16(%rbp)
	/* end mov */
	/* mov ~dst:"-24(%rbp)" ~src:"m1(%rip)" */
	movq m1(%rip), %r15
	movq %r15, -24(%rbp)
	/* end mov */
	/* mov ~dst:"-32(%rbp)" ~src:"n2(%rip)" */
	movq n2(%rip), %r15
	movq %r15, -32(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"-8(%rbp)" */
	movq -8(%rbp), %rdi
	/* end mov */
	/* mov ~dst:"%rsi" ~src:"-16(%rbp)" */
	movq -16(%rbp), %rsi
	/* end mov */
	/* mov ~dst:"%rdx" ~src:"-24(%rbp)" */
	movq -24(%rbp), %rdx
	/* end mov */
	/* mov ~dst:"%rcx" ~src:"-32(%rbp)" */
	movq -32(%rbp), %rcx
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$check" ~args:"" */
	call check
	/* end call */
	/* mov ~dst:"-104(%rbp)" ~src:"%rax" */
	movq %rax, -104(%rbp)
	/* end mov */
	/* mov ~dst:"0(%rbp)" ~src:"$0" */
	movq $0, 0(%rbp)
	/* end mov */
	/* mov ~dst:"%rdi" ~src:"0(%rbp)" */
	movq 0(%rbp), %rdi
	/* end mov */
	/* call ~kind:"`Normal" ~f:"$print_int" ~args:"" */
	call print_int
	/* end call */
	/* mov ~dst:"main37(%rip)" ~src:"%rax" */
	movq %rax, main37(%rip)
	/* end mov */
	/* start epilog */
	addq $136, %rsp
	popq %rbp
	/* end epilog */
	ret
