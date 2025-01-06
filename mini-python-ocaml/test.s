	.text
.extern printf
.extern puts
.extern strlen
.extern malloc
.extern strcpy
.extern strcmp
.extern strcat
.extern exit

my_malloc:
    pushq %rbp
    movq %rsp, %rbp
    andq $-16, %rsp    
    call malloc
    movq %rbp, %rsp
    popq %rbp
    ret
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $8192, %rsp
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq $2, 8(%rax)
	movq %rax, %r11
	popq %rax
	movq 0(%rax), %rcx
	movq 0(%r11), %rdx
	cmpq $2, %rcx
	jne L4
	cmpq $2, %rdx
	jne L4
	movq 8(%rax), %rsi
	movq 8(%r11), %rdi
	addq %rdi, %rsi
	movq $16, %rdi
	call my_malloc
	movq $2, 0(%rax)
	movq %rsi, 8(%rax)
	jmp L6
L4:
	cmpq $3, %rcx
	jne L5
	cmpq $3, %rdx
	jne L5
	pushq %r11
	pushq %rax
	leaq 16(%rax), %rdi
	call strlen
	movq %rax, %r12
	popq %rax
	popq %r11
	pushq %r12
	pushq %r11
	pushq %rax
	leaq 16(%r11), %rdi
	call strlen
	movq %rax, %r13
	addq %r12, %r13
	addq $1, %r13
	movq %r13, %rdi
	call malloc
	popq %r12
	popq %r11
	popq %r14
	pushq %rax
	movq %rax, %rdi
	leaq 16(%r12), %rsi
	call strcpy
	popq %rax
	pushq %rax
	addq %r14, %rdi
	leaq 16(%r11), %rsi
	call strcat
	popq %rax
	movq $3, 0(%rax)
	movq %r13, 8(%rax)
	jmp L6
L5:
L5:
	cmpq $4, %rcx
	jne error
	cmpq $4, %rdx
	jne error
	pushq %rax
	pushq %r11
	movq 8(%rax), %r12
	movq 8(%r11), %r13
	movq %r12, %rdi
	addq %r13, %rdi
	pushq %rdi
	imulq $8, %rdi
	addq $16, %rdi
	call malloc
	popq %rdi
	movq $4, 0(%rax)
	movq %rdi, 8(%rax)
	movq %rax, %r14
	popq %r11
	popq %r15
	xorq %rcx, %rcx
copy_list1_loop:
	cmpq %r12, %rcx
	je copy_list1_end
	movq %rcx, %rdx
	imulq $8, %rdx
	addq $16, %rdx
	movq 0(%r15,%rdx,1), %rdi
	movq 0(%r14,%rdx,1), %rdi
	incq %rcx
	jmp copy_list1_loop
copy_list1_end:
	xorq %rcx, %rcx
copy_list2_loop:
	cmpq %r13, %rcx
	je copy_list2_end
	movq %rcx, %rdx
	imulq $8, %rdx
	addq $16, %rdx
	movq 0(%r11,%rdx,1), %rdi
	movq %r12, %r8
	addq %rcx, %r8
	imulq $8, %r8
	addq $16, %r8
	movq %rdi, 0(%r14,%r8,1)
	incq %rcx
	jmp copy_list2_loop
copy_list2_end:
	movq %r14, %rax
L6:
	movq 0(%rax), %rsi
	cmpq $0, %rsi
	je print_none_value
	cmpq $1, %rsi
	je print_bool_value
	cmpq $4, %rsi
	je print_list_value
	cmpq $2, %rsi
	je print_int_value
	jmp print_string_value
print_int_value:
	pushq %rax
	movq 8(%rax), %rsi
	movq $fmt_int, %rdi
	xorq %rax, %rax
	call printf
	popq %rax
	jmp print_end_value
print_string_value:
	leaq 16(%rax), %rsi
	movq $fmt_string, %rdi
	xorq %rax, %rax
	call printf
	jmp print_end_value
print_none_value:
	movq $str_none, %rsi
	movq $fmt_string, %rdi
	xorq %rax, %rax
	call printf
	jmp print_end_value
print_bool_value:
	movq 8(%rax), %r11
	cmpq $0, %r11
	je print_false_value
	movq $str_true, %rsi
	jmp print_bool_str_value
print_false_value:
	movq $str_false, %rsi
print_bool_str_value:
	movq $fmt_string, %rdi
	xorq %rax, %rax
	call printf
	jmp print_end_value
print_list_value:
	movq %rax, %rdi
	call print_list
print_end_value:
	movq $10, %rdi
	call putchar
	movq $0, %rax
	movq $0, %rax
	leave
	ret
compare_lists:
	pushq %rbp
	movq %rsp, %rbp
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	movq %rdi, %r12
	movq %rsi, %r13
	movq 8(%r12), %r15
	xorq %r14, %r14
compare_lists_loop:
	cmpq %r14, %r15
	je compare_lists_equal
	movq %r14, %rcx
	imulq $8, %rcx
	addq $16, %rcx
	movq 0(%r12,%rcx,1), %rdi
	movq 0(%r13,%rcx,1), %rsi
	pushq %r14
	pushq %r15
	pushq %r12
	pushq %r13
	movq %rdi, %rax
	movq %rsi, %r11
	movq $0, %rax
	movq 0(%rdi), %rcx
	movq 0(%rsi), %rdx
	cmpq %rcx, %rdx
	jne compare_lists_restore
	cmpq $2, %rcx
	je compare_lists_int
	cmpq $3, %rcx
	je compare_lists_string
	cmpq $4, %rcx
	je compare_lists_recurse
	jmp compare_lists_restore
compare_lists_int:
	movq 8(%rdi), %rax
	subq 8(%rsi), %rax
	jmp compare_lists_restore
compare_lists_string:
	pushq %rdi
	pushq %rsi
	leaq 16(%rdi), %rdi
	leaq 16(%rsi), %rsi
	call strcmp
	popq %rsi
	popq %rdi
	jmp compare_lists_restore
compare_lists_recurse:
	pushq %rdi
	pushq %rsi
	call compare_lists
	popq %rsi
	popq %rdi
compare_lists_restore:
	popq %r13
	popq %r12
	popq %r15
	popq %r14
	cmpq $0, %rax
	jne compare_lists_end
	incq %r14
	jmp compare_lists_loop
compare_lists_equal:
	xorq %rax, %rax
compare_lists_end:
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	movq %rbp, %rsp
	popq %rbp
	ret
print_element:
	pushq %rbp
	movq %rsp, %rbp
	movq 0(%rdi), %rsi
	cmpq $0, %rsi
	je print_element_none
	cmpq $1, %rsi
	je print_element_bool
	cmpq $3, %rsi
	je print_element_string
	cmpq $4, %rsi
	je print_element_list
	movq 8(%rdi), %rsi
	movq $fmt_int, %rdi
	movq $0, %rax
	call printf
	jmp print_element_end
print_element_none:
	movq $str_none, %rsi
	movq $fmt_string, %rdi
	movq $0, %rax
	call printf
	jmp print_element_end
print_element_bool:
	movq 8(%rdi), %rdx
	cmpq $0, %rdx
	je print_element_false
	movq $str_true, %rsi
	jmp print_element_do_bool
print_element_false:
	movq $str_false, %rsi
print_element_do_bool:
	movq $fmt_string, %rdi
	movq $0, %rax
	call printf
	jmp print_element_end
print_element_string:
	leaq 16(%rdi), %rsi
	movq $fmt_string, %rdi
	movq $0, %rax
	call printf
	jmp print_element_end
print_element_list:
	call print_list
print_element_end:
	movq %rbp, %rsp
	popq %rbp
	ret
print_list:
	pushq %rbp
	movq %rsp, %rbp
	movq $str_lbracket, %rdi
	call puts
	movq 8(%rdi), %r12
	xorq %r13, %r13
print_list_inner_loop:
	cmpq %r13, %r12
	je print_list_inner_end
	movq %r13, %rcx
	imulq $8, %rcx
	addq $16, %rcx
	pushq %rdi
	pushq %r12
	pushq %r13
	movq 0(%rdi,%rcx,1), %rdi
	call print_element
	popq %r13
	popq %r12
	popq %rdi
	incq %r13
	cmpq %r13, %r12
	je print_list_inner_end
	pushq %rdi
	movq $str_comma, %rdi
	call printf
	popq %rdi
	jmp print_list_inner_loop
print_list_inner_end:
	movq $str_rbracket, %rdi
	call puts
	leave
	ret
error:
	movq $error_msg, %rdi
	call puts
	movq $1, %rdi
	call exit
error_div_by_zero:
	movq $div_zero_msg, %rdi
	call puts
	movq $1, %rdi
	call exit
error_type:
	movq $type_error_msg, %rdi
	call puts
	movq $1, %rdi
	call exit
	.data
error_msg:
	.string "runtime error"
div_zero_msg:
	.string "division by zero"
type_error_msg:
	.string "type error"
fmt_int:
	.string "%d"
fmt_bool:
	.string "%s"
fmt_string:
	.string "%s"
str_true:
	.string "True"
str_false:
	.string "False"
str_none:
	.string "None"
str_lbracket:
	.string "["
str_rbracket:
	.string "]"
str_comma:
	.string ", "
