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
	.globl	print_value
print_value:
	pushq %rbp
	movq %rsp, %rbp
	pushq %rbx
	movq %rdi, %rbx
	movq 0(%rbx), %rdx
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
	jne print_value_end
	cmpq $3, %rdx
	jne print_value_end
	leaq 16(%rbx), %rdi
	call print_string
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
	movq $10, 8(%rax)
	movq %rax, -64(%rbp)
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $10, 8(%rax)
	movq %rax, -80(%rbp)
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $10, 8(%rax)
	movq %rax, -96(%rbp)
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $20, 8(%rax)
	movq %rax, -112(%rbp)
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $10, 8(%rax)
	movq %rax, -128(%rbp)
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $20, 8(%rax)
	movq %rax, -144(%rbp)
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $10, 8(%rax)
	movq %rax, -160(%rbp)
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $20, 8(%rax)
	movq %rax, -176(%rbp)
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $10, 8(%rax)
	movq %rax, -192(%rbp)
	movq -64(%rbp), %rax
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq -80(%rbp), %rax
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq -96(%rbp), %rax
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq -112(%rbp), %rax
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq -128(%rbp), %rax
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq -144(%rbp), %rax
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq -160(%rbp), %rax
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq -176(%rbp), %rax
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq -192(%rbp), %rax
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $4, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC0, %rsi
	call strcpy
	popq %rax
	movq %rax, -208(%rbp)
	movq -208(%rbp), %rax
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $5, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC1, %rsi
	call strcpy
	popq %rax
	pushq %rax
	movq 0(%rax), %rdx
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	popq %rcx
	cmpq $3, %rdx
	movq 8(%rcx), %rdx
	movq %rdx, 8(%rax)
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $5, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC3, %rsi
	call strcpy
	popq %rax
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $5, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC2, %rsi
	call strcpy
	popq %rax
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $5, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC5, %rsi
	call strcpy
	popq %rax
	call printf
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $5, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC4, %rsi
	call strcpy
	popq %rax
	call printf
	movq $10, %rdi
	call putchar
	movq $0, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
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
.LC5:
	.string "hello"
.LC3:
	.string "hello"
.LC2:
	.string "hello"
.LC0:
	.string "fuck"
.LC4:
	.string "world"
.LC1:
	.string "eeeee"
