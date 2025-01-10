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
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $2, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	cmpq %rcx, %rax
	setg %al
	movzbq %al, %rax
	pushq %rax
	movq $16, %rdi
	call my_malloc
	popq %rdx
	movq $1, 0(%rax)
	movq %rdx, 8(%rax)
	movq 8(%rax), %rdx
	cmpq $0, %rdx
	je .LC1
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq 0(%rax), %rdx
	cmpq $3, %rdx
	je handle_len
	cmpq $4, %rdx
	je handle_len
	jmp error_label
handle_len:
	popq %rax
	movq 8(%rax), %rsi
	pushq %rsi
	movq $16, %rdi
	call my_malloc
	popq %rsi
	movq $2, 0(%rax)
	movq %rsi, 8(%rax)
	movq 0(%rax), %rcx
	cmpq $4, %rcx
	je .LC2
	cmpq $3, %rcx
	jne error_label
.LC2:
	movq 8(%rax), %rdx
	cmpq $0, %rdx
	je .LC1
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $1, 8(%rax)
	jmp .LC0
.LC1:
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $0, 8(%rax)
.LC0:
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
