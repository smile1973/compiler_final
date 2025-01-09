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
	jne print_value_end
	movq 8(%rbx), %rdi
	call print_int
print_value_end:
	popq %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $10, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $10, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $9, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	addq %rcx, %rax
	pushq %rax
	movq $16, %rdi
	call my_malloc
	popq %rdx
	movq $2, 0(%rax)
	movq %rdx, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	subq %rcx, %rax
	pushq %rax
	movq $16, %rdi
	call my_malloc
	popq %rdx
	movq $2, 0(%rax)
	movq %rdx, 8(%rax)
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $10, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $9, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $6, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	subq %rcx, %rax
	pushq %rax
	movq $16, %rdi
	call my_malloc
	popq %rdx
	movq $2, 0(%rax)
	movq %rdx, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $4, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	addq %rcx, %rax
	pushq %rax
	movq $16, %rdi
	call my_malloc
	popq %rdx
	movq $2, 0(%rax)
	movq %rdx, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	imulq %rcx, %rax
	pushq %rax
	movq $16, %rdi
	call my_malloc
	popq %rdx
	movq $2, 0(%rax)
	movq %rdx, 8(%rax)
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $10, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $3, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	addq %rcx, %rax
	pushq %rax
	movq $16, %rdi
	call my_malloc
	popq %rdx
	movq $2, 0(%rax)
	movq %rdx, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	cmpq $0, %rcx
	cqto
	idivq %rcx
	pushq %rax
	movq $16, %rdi
	call my_malloc
	popq %rdx
	movq $2, 0(%rax)
	movq %rdx, 8(%rax)
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $10, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $6, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	cmpq $0, %rcx
	cqto
	idivq %rcx
	movq %rdx, %rax
	pushq %rax
	movq $16, %rdi
	call my_malloc
	popq %rdx
	movq $2, 0(%rax)
	movq %rdx, 8(%rax)
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $10, 8(%rax)
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
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
	sete %al
	movzbq %al, %rax
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
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	cmpq %rcx, %rax
	sete %al
	movzbq %al, %rax
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
	setne %al
	movzbq %al, %rax
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
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	cmpq %rcx, %rax
	setne %al
	movzbq %al, %rax
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
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
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
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	cmpq %rcx, %rax
	setge %al
	movzbq %al, %rax
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
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	cmpq %rcx, %rax
	setle %al
	movzbq %al, %rax
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
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	cmpq %rcx, %rax
	setl %al
	movzbq %al, %rax
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
	movq $1, 0(%rax)
	movq $1, 8(%rax)
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $0, 8(%rax)
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $0, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	cmpq $0, %rax
	je .LC0
	andq %rcx, %rax
	jmp .LC0
.LC0:
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
	movq $1, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $0, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	cmpq $0, %rax
	jne .LC1
	orq %rcx, %rax
	jmp .LC1
.LC1:
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
	movq $1, 0(%rax)
	movq $0, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $1, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	cmpq $0, %rax
	jne .LC2
	orq %rcx, %rax
	jmp .LC2
.LC2:
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
	movq $1, 0(%rax)
	movq $0, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $0, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	cmpq $0, %rax
	jne .LC3
	orq %rcx, %rax
	jmp .LC3
.LC3:
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
	movq $1, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $0, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	cmpq %rcx, %rax
	sete %al
	movzbq %al, %rax
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
	movq $1, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $0, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	cmpq %rcx, %rax
	setne %al
	movzbq %al, %rax
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
	movq $1, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $0, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	cmpq %rcx, %rax
	setl %al
	movzbq %al, %rax
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
	movq $1, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $0, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	cmpq %rcx, %rax
	setle %al
	movzbq %al, %rax
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
	movq $1, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $0, 8(%rax)
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
	movq %rax, %rdi
	call print_value
	movq $10, %rdi
	call putchar
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $1, 0(%rax)
	movq $0, 8(%rax)
	movq %rax, %rcx
	popq %rax
	movq 8(%rax), %rax
	movq 8(%rcx), %rcx
	cmpq %rcx, %rax
	setge %al
	movzbq %al, %rax
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
	movq $.LC4, %rdi
	call printf
	movq $10, %rdi
	call putchar
	movq $.LC6, %rdi
	call printf
	movq $.LC5, %rdi
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
.LC6:
	.string "hello"
.LC4:
	.string "hello"
.LC5:
	.string "fox"
