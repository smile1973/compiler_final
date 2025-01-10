
Boolean Condition:
	false -> None, False, 0, empty string, empty list
	true -> false 以外的

	and 運算 -> 如果第一個是 false, 後面不用算
	or 運算 -> 如果第一個是 true, 前面不用算

Iteration: 	for x in e: s
	e 要是 list

Operators:
	減 乘 除 MOD 加負號 -> 只有 int 能做這些
	加 -> two parameters of the same type
			string -> concatenation (in a new string)
			list -> concatenation (in a new list)
 
Comparison Operators: 	<, <=, >, >=, ==, !=
	always return a Boolean value (False being 0 and True being 1)
	對 int bool -> 數學操作
	對 list string -> 字典序比較

Build-in Functions:
	len -> 只能用在 list string

===========================================

[]比較 
str比較(怪怪的 可能是記憶體問題? print("b"<"c")會是false)

===========================================
for i in list * 3
list 比較 * 1
雙層list * 4
def * 8
[02] badmatrix -> 雙層list
[01] compare_list1 -> list 比較
[02] compare_list2 -> list 比較 + 雙層list
[03] concat2 -> def
[03] concat3 -> def + list add
[done] eval1 -> for i in list
[skip] fact -> rec
[skip] fib -> rec
[02] list2 -> 雙層list
[03] loop1 -> def + if
[skip] mandelbrot -> ?
[03] none -> def
[03] pascal -> def
[03] primes -> def + for
[done] print_list1 -> list() + range()
[02] print_list3 -> 雙層list
[skip] queen -> def + for
[done] range1 -> list() + range()
[done] range2 -> list() + range() + 取值
[done] range3 -> list() + range() + for i in list
[03] str1 -> def
[done] var3 -> for i in list

fail * 7
[] add1
[] bool1
[] bool2
[done] for1
[] len1
[] range1
[done] range2 