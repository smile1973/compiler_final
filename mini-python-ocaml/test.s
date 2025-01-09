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
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $0, 8(%rax)
	movb $0, 16(%rax)
	pushq %rax
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $0, 8(%rax)
	movb $0, 16(%rax)
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
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $0, 8(%rax)
	movb $0, 16(%rax)
	pushq %rax
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC2, %rsi
	call strcpy
	popq %rax
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
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC5, %rsi
	call strcpy
	popq %rax
	pushq %rax
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC4, %rsi
	call strcpy
	popq %rax
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
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC7, %rsi
	call strcpy
	popq %rax
	pushq %rax
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC6, %rsi
	call strcpy
	popq %rax
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
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $0, 8(%rax)
	movb $0, 16(%rax)
	pushq %rax
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC8, %rsi
	call strcpy
	popq %rax
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
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC11, %rsi
	call strcpy
	popq %rax
	pushq %rax
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC10, %rsi
	call strcpy
	popq %rax
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
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC13, %rsi
	call strcpy
	popq %rax
	pushq %rax
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC12, %rsi
	call strcpy
	popq %rax
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
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $0, 8(%rax)
	movb $0, 16(%rax)
	pushq %rax
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC14, %rsi
	call strcpy
	popq %rax
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
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC17, %rsi
	call strcpy
	popq %rax
	pushq %rax
	movq $24, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	leaq 16(%rax), %rdi
	movq $.LC16, %rsi
	call strcpy
	popq %rax
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
.LC15:
	.string ""
.LC9:
	.string ""
.LC3:
	.string ""
.LC1:
	.string ""
.LC0:
	.string ""
.LC13:
	.string "a"
.LC8:
	.string "a"
.LC5:
	.string "a"
.LC2:
	.string "a"
.LC14:
	.string "b"
.LC12:
	.string "b"
.LC16:
	.string "ab"
.LC7:
	.string "ab"
.LC6:
	.string "ab"
.LC4:
	.string "da"
.LC17:
	.string "abc"
.LC11:
	.string "abc"
.LC10:
	.string "abc"
