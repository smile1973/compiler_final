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
	movq $111, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 16(%rdx)
	pushq %rdx
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $222, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 32(%rdx)
	pushq %rdx
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $333, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rcx
	popq %rax
	popq %rdx
	movq %rcx, 48(%rdx)
	pushq %rdx
	popq %rax
	movq %rax, -64(%rbp)
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq -64(%rbp), %rdx
	movq 0(%rdx), %rcx
	cmpq $4, %rcx
	jne error_label
	movq 8(%rdx), %rcx
	popq %rax
	movq 8(%rax), %r8
	cmpq $0, %r8
	jl error_label
	cmpq %rcx, %r8
	jge error_label
	imulq $16, %r8
	addq $16, %r8
	movq 0(%rdx,%r8,1), %rax
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
