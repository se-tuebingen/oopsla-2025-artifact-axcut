; asmsyntax=nasm
;
; To create an executable:
; $ nasm -f elf64 fibonacci_recursive.x86.asm
; $ gcc -o fibonacci_recursive path/to/X86_64-infrastructure/driver.c fibonacci_recursive.x86.o

segment .note.GNU-stack noalloc noexec nowrite progbits

segment .text
  global asm_main0, _asm_main0
  global asm_main1, _asm_main1
  global asm_main2, _asm_main2
  global asm_main3, _asm_main3
  global asm_main4, _asm_main4
  global asm_main5, _asm_main5
asm_main0:
_asm_main0:
asm_main1:
_asm_main1:
asm_main2:
_asm_main2:
asm_main3:
_asm_main3:
asm_main4:
_asm_main4:
asm_main5:
_asm_main5:
; setup
; save registers
push rbx
push rbp
push r12
push r13
push r14
push r15

; reserve space for register spills
sub rsp, 2048
; initialize heap pointer
mov rbx, rdi
; initialize free pointer
mov rbp, rbx
add rbp, 64
; move parameters into place
mov rdx, rsi

; actual code
lab0:
mov rsi, 0
lea rdi, [rel lab2]
mov rcx, rdi
mov rdi, rdx
mov rdx, rcx
mov rax, rsi
jmp lab1

lab2:

lab2b0:
mov rdx, rdx
jmp cleanup

lab1:
cmp rdi, 0
je lab3
mov r9, 1
cmp rdi, r9
je lab4
mov r9, -1
mov r11, rdi
add r11, r9
mov rsi, rax
mov r9, rdi
mov rdi, rdx
mov rdx, r11
mov [rbx + 56], r9
mov qword [rbx + 48], 0
mov [rbx + 40], rdi
mov [rbx + 32], rsi
mov qword [rbx + 16], 0
mov rsi, rbx
mov rbx, [rbx + 0]
cmp rbx, 0
je lab16
mov qword [rsi + 0], 0
jmp lab17

lab16:
mov rbx, rbp
mov rbp, [rbp + 0]
cmp rbp, 0
je lab14
mov qword [rbx + 0], 0
mov rcx, [rbx + 48]
cmp rcx, 0
je lab7
cmp qword [rcx + 0], 0
je lab5
add qword [rcx + 0], -1
jmp lab6

lab5:
mov [rcx + 0], rbp
mov rbp, rcx

lab6:

lab7:
mov rcx, [rbx + 32]
cmp rcx, 0
je lab10
cmp qword [rcx + 0], 0
je lab8
add qword [rcx + 0], -1
jmp lab9

lab8:
mov [rcx + 0], rbp
mov rbp, rcx

lab9:

lab10:
mov rcx, [rbx + 16]
cmp rcx, 0
je lab13
cmp qword [rcx + 0], 0
je lab11
add qword [rcx + 0], -1
jmp lab12

lab11:
mov [rcx + 0], rbp
mov rbp, rcx

lab12:

lab13:
jmp lab15

lab14:
mov rbp, rbx
add rbp, 64

lab15:

lab17:
lea rdi, [rel lab18]
mov rcx, rdi
mov rdi, rdx
mov rdx, rcx
mov rax, rsi
jmp lab1

lab18:

lab18b0:
cmp qword [rsi + 0], 0
je lab22
add qword [rsi + 0], -1
mov rcx, [rsi + 48]
cmp rcx, 0
je lab19
add qword [rcx + 0], 1

lab19:
mov rcx, [rsi + 32]
cmp rcx, 0
je lab20
add qword [rcx + 0], 1

lab20:
mov rcx, [rsi + 16]
cmp rcx, 0
je lab21
add qword [rcx + 0], 1

lab21:
jmp lab23

lab22:
mov [rsi + 0], rbx
mov rbx, rsi

lab23:
mov r9, [rsi + 56]
mov rdi, [rsi + 40]
mov rsi, [rsi + 32]
mov r11, -2
mov r13, r9
add r13, r11
mov r9, rdx
mov rdx, r13
mov [rbx + 56], r9
mov qword [rbx + 48], 0
mov [rbx + 40], rdi
mov [rbx + 32], rsi
mov qword [rbx + 16], 0
mov rsi, rbx
mov rbx, [rbx + 0]
cmp rbx, 0
je lab35
mov qword [rsi + 0], 0
jmp lab36

lab35:
mov rbx, rbp
mov rbp, [rbp + 0]
cmp rbp, 0
je lab33
mov qword [rbx + 0], 0
mov rcx, [rbx + 48]
cmp rcx, 0
je lab26
cmp qword [rcx + 0], 0
je lab24
add qword [rcx + 0], -1
jmp lab25

lab24:
mov [rcx + 0], rbp
mov rbp, rcx

lab25:

lab26:
mov rcx, [rbx + 32]
cmp rcx, 0
je lab29
cmp qword [rcx + 0], 0
je lab27
add qword [rcx + 0], -1
jmp lab28

lab27:
mov [rcx + 0], rbp
mov rbp, rcx

lab28:

lab29:
mov rcx, [rbx + 16]
cmp rcx, 0
je lab32
cmp qword [rcx + 0], 0
je lab30
add qword [rcx + 0], -1
jmp lab31

lab30:
mov [rcx + 0], rbp
mov rbp, rcx

lab31:

lab32:
jmp lab34

lab33:
mov rbp, rbx
add rbp, 64

lab34:

lab36:
lea rdi, [rel lab37]
mov rcx, rdi
mov rdi, rdx
mov rdx, rcx
mov rax, rsi
jmp lab1

lab37:

lab37b0:
cmp qword [rsi + 0], 0
je lab41
add qword [rsi + 0], -1
mov rcx, [rsi + 48]
cmp rcx, 0
je lab38
add qword [rcx + 0], 1

lab38:
mov rcx, [rsi + 32]
cmp rcx, 0
je lab39
add qword [rcx + 0], 1

lab39:
mov rcx, [rsi + 16]
cmp rcx, 0
je lab40
add qword [rcx + 0], 1

lab40:
jmp lab42

lab41:
mov [rsi + 0], rbx
mov rbx, rsi

lab42:
mov r9, [rsi + 56]
mov rdi, [rsi + 40]
mov rsi, [rsi + 32]
mov r11, r9
add r11, rdx
mov rdx, r11
jmp rdi

lab4:
mov rsi, rax
mov rcx, rdi
mov rdi, rdx
mov rdx, rcx
jmp rdi

lab3:
mov rsi, rax
mov rcx, rdi
mov rdi, rdx
mov rdx, rcx
jmp rdi

; cleanup
cleanup:
; free space for register spills
add rsp, 2048
; restore registers
pop r15
pop r14
pop r13
pop r12
pop rbp
pop rbx
ret