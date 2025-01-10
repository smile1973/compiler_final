	.text
	.globl	my_malloc
my_malloc:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	call malloc
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	print_int
print_int:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	movq $0, %rax
	movq %rdi, %rsi
	leaq .LCd, %rdi
	call printf
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	print_bool
print_bool:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	cmpq $0, %rdi
	je print_bool_false
	movq $0, %rax
	leaq .LCtrue, %rdi
	jmp print_bool_end
print_bool_false:
	movq $0, %rax
	leaq .LCfalse, %rdi
print_bool_end:
	call printf
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	print_string
print_string:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	movq $0, %rax
	movq %rdi, %rsi
	leaq .LCs, %rdi
	call printf
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	print_none
print_none:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	movq $0, %rax
	leaq .LCnone, %rsi
	leaq .LCs, %rdi
	call printf
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	print_value
print_value:
	pushq %rbp
	movq %rsp, %rbp
	pushq %rbx
	movq %rdi, %rbx
	movq 0(%rbx), %rdx
	cmpq $0, %rdx
	jne print_value_not_none
	call print_none
	jmp print_value_end
print_value_not_none:
	cmpq $1, %rdx
	jne print_value_not_bool
	movq 8(%rbx), %rdi
	call print_bool
	jmp print_value_end
print_value_not_bool:
	cmpq $2, %rdx
	jne print_value_not_int
	movq 8(%rbx), %rdi
	call print_int
	jmp print_value_end
print_value_not_int:
	cmpq $3, %rdx
	jne print_value_not_str
	leaq 16(%rbx), %rdi
	call print_string
	jmp print_value_end
print_value_not_str:
	cmpq $4, %rdx
	jne print_value_end
	pushq %r12
	pushq %r13
	pushq %rbx
	leaq .LCstart, %rdi
	call printf
	movq 8(%rbx), %r12
	xorq %r13, %r13
print_list_loop:
	cmpq %r12, %r13
	je print_list_end
	cmpq $0, %r13
	je print_list_element
	leaq .LCcomma, %rdi
	call printf
print_list_element:
	movq %r13, %rcx
	imulq $16, %rcx
	addq $16, %rcx
	movq 0(%rbx,%rcx,1), %rdi
	call print_value
	incq %r13
	jmp print_list_loop
print_list_end:
	leaq .LCend, %rdi
	call printf
	popq %rbx
	popq %r13
	popq %r12
print_value_end:
	popq %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $1024, %rsp
	movq $64, %rdi
	call my_malloc
	movq $4, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 16(%rdx)
	pushq %rdx
	movq $48, %rdi
	call my_malloc
	movq $4, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 16(%rdx)
	pushq %rdx
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 32(%rdx)
	pushq %rdx
	popq %rax
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 32(%rdx)
	pushq %rdx
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $4, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 48(%rdx)
	pushq %rdx
	popq %rax
	pushq %rax
	movq $64, %rdi
	call my_malloc
	movq $4, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 16(%rdx)
	pushq %rdx
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 32(%rdx)
	pushq %rdx
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 48(%rdx)
	pushq %rdx
	popq %rax
	movq %rax, %rcx
	popq %rax
	movq 0(%rax), %r8
	movq 0(%rcx), %r9
	cmpq $4, %r8
	jne error
	cmpq $4, %r9
	jne error
	movq 8(%rax), %r8
	movq 8(%rcx), %r9
	pushq %rax
	pushq %rcx
	cmpq %r8, %r9
	jne .LC3
	popq %rcx
	popq %rax
	xorq %rdx, %rdx
.LC0:
	cmpq %r8, %rdx
	je .LC2
	movq %rdx, %r10
	imulq $16, %r10
	addq $16, %r10
	movq 0(%rax,%r10,1), %r11
	movq 0(%rcx,%r10,1), %r12
	movq 8(%r11), %r11
	movq 8(%r12), %r12
	cmpq %r11, %r12
	jne .LC3
	incq %rdx
	jmp .LC0
.LC2:
	movq $1, %rax
	jmp .LC1
.LC3:
	movq $0, %rax
.LC1:
	pushq %rax
	movq $16, %rdi
	call my_malloc
	popq %rdx
	movq $1, 0(%rax)
	movq %rdx, 8(%rax)
	movq 8(%rax), %rdx
	xorq $1, %rdx
	movq %rdx, 8(%rax)
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq $64, %rdi
	call my_malloc
	movq $4, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 16(%rdx)
	pushq %rdx
	movq $48, %rdi
	call my_malloc
	movq $4, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 16(%rdx)
	pushq %rdx
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 32(%rdx)
	pushq %rdx
	popq %rax
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 32(%rdx)
	pushq %rdx
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $4, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 48(%rdx)
	pushq %rdx
	popq %rax
	pushq %rax
	movq $64, %rdi
	call my_malloc
	movq $4, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 16(%rdx)
	pushq %rdx
	movq $48, %rdi
	call my_malloc
	movq $4, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 16(%rdx)
	pushq %rdx
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 32(%rdx)
	pushq %rdx
	popq %rax
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 32(%rdx)
	pushq %rdx
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 48(%rdx)
	pushq %rdx
	popq %rax
	movq %rax, %rcx
	popq %rax
	movq 0(%rax), %r8
	movq 0(%rcx), %r9
	cmpq $4, %r8
	jne error
	cmpq $4, %r9
	jne error
	movq 8(%rax), %r8
	movq 8(%rcx), %r9
	pushq %rax
	pushq %rcx
	cmpq %r8, %r9
	jne .LC7
	popq %rcx
	popq %rax
	xorq %rdx, %rdx
.LC4:
	cmpq %r8, %rdx
	je .LC6
	movq %rdx, %r10
	imulq $16, %r10
	addq $16, %r10
	movq 0(%rax,%r10,1), %r11
	movq 0(%rcx,%r10,1), %r12
	movq 8(%r11), %r11
	movq 8(%r12), %r12
	cmpq %r11, %r12
	jne .LC7
	incq %rdx
	jmp .LC4
.LC6:
	movq $1, %rax
	jmp .LC5
.LC7:
	movq $0, %rax
.LC5:
	pushq %rax
	movq $16, %rdi
	call my_malloc
	popq %rdx
	movq $1, 0(%rax)
	movq %rdx, 8(%rax)
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq $64, %rdi
	call my_malloc
	movq $4, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 16(%rdx)
	pushq %rdx
	movq $48, %rdi
	call my_malloc
	movq $4, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 16(%rdx)
	pushq %rdx
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 32(%rdx)
	pushq %rdx
	popq %rax
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 32(%rdx)
	pushq %rdx
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $4, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 48(%rdx)
	pushq %rdx
	popq %rax
	pushq %rax
	movq $64, %rdi
	call my_malloc
	movq $4, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 16(%rdx)
	pushq %rdx
	movq $48, %rdi
	call my_malloc
	movq $4, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 16(%rdx)
	pushq %rdx
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 32(%rdx)
	pushq %rdx
	popq %rax
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 32(%rdx)
	pushq %rdx
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $4, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 48(%rdx)
	pushq %rdx
	popq %rax
	movq %rax, %rcx
	popq %rax
	movq 0(%rax), %r8
	movq 0(%rcx), %r9
	cmpq $4, %r8
	jne error
	cmpq $4, %r9
	jne error
	movq 8(%rax), %r8
	movq 8(%rcx), %r9
	pushq %rax
	pushq %rcx
	cmpq %r8, %r9
	jne .LC11
	popq %rcx
	popq %rax
	xorq %rdx, %rdx
.LC8:
	cmpq %r8, %rdx
	je .LC10
	movq %rdx, %r10
	imulq $16, %r10
	addq $16, %r10
	movq 0(%rax,%r10,1), %r11
	movq 0(%rcx,%r10,1), %r12
	movq 8(%r11), %r11
	movq 8(%r12), %r12
	cmpq %r11, %r12
	jne .LC11
	incq %rdx
	jmp .LC8
.LC10:
	movq $1, %rax
	jmp .LC9
.LC11:
	movq $0, %rax
.LC9:
	pushq %rax
	movq $16, %rdi
	call my_malloc
	popq %rdx
	movq $1, 0(%rax)
	movq %rdx, 8(%rax)
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	movq %rax, -64(%rbp)
	movq $64, %rdi
	call my_malloc
	movq $4, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	movq -64(%rbp), %rdi
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 16(%rdx)
	pushq %rdx
	movq -64(%rbp), %rdi
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 32(%rdx)
	pushq %rdx
	movq -64(%rbp), %rdi
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 48(%rdx)
	pushq %rdx
	popq %rax
	pushq %rax
	movq $64, %rdi
	call my_malloc
	movq $4, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	movq -64(%rbp), %rdi
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 16(%rdx)
	pushq %rdx
	movq $32, %rdi
	call my_malloc
	movq $4, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq -64(%rbp), %rdi
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 16(%rdx)
	pushq %rdx
	popq %rax
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 32(%rdx)
	pushq %rdx
	movq -64(%rbp), %rdi
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 48(%rdx)
	pushq %rdx
	popq %rax
	movq %rax, %rcx
	popq %rax
	movq 0(%rax), %r8
	movq 0(%rcx), %r9
	cmpq $4, %r8
	jne error
	cmpq $4, %r9
	jne error
	movq 8(%rax), %r8
	movq 8(%rcx), %r9
	pushq %rax
	pushq %rcx
	cmpq %r8, %r9
	jne .LC15
	popq %rcx
	popq %rax
	xorq %rdx, %rdx
.LC12:
	cmpq %r8, %rdx
	je .LC14
	movq %rdx, %r10
	imulq $16, %r10
	addq $16, %r10
	movq 0(%rax,%r10,1), %r11
	movq 0(%rcx,%r10,1), %r12
	movq 8(%r11), %r11
	movq 8(%r12), %r12
	cmpq %r11, %r12
	jne .LC15
	incq %rdx
	jmp .LC12
.LC14:
	movq $1, %rax
	jmp .LC13
.LC15:
	movq $0, %rax
.LC13:
	pushq %rax
	movq $16, %rdi
	call my_malloc
	popq %rdx
	movq $1, 0(%rax)
	movq %rdx, 8(%rax)
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq $0, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
error_label:
	movq $0, %rax
	leaq .LCerror, %rdi
	call printf
	movq $10, %rdi
	call putchar
	movq $1, %rdi
	call exit
	.data
.LCnone:
	.string "None"
.LCtrue:
	.string "True"
.LCfalse:
	.string "False"
.LCcomma:
	.string ", "
.LCstart:
	.string "["
.LCend:
	.string "]"
.LCs:
	.string "%s"
.LCd:
	.string "%d"
.LCerror:
	.string "Runtime Error"
.LClen:
	.string "len: %d\n"
