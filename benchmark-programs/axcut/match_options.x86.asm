; asmsyntax=nasm
;
; To create an executable:
; $ nasm -f elf64 match_options.x86.asm
; $ gcc -o match_options path/to/X86_64-infrastructure/driver.c match_options.x86.o

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
jmp near lab2b0
jmp near lab2b1

lab2b0:
mov rdx, -1
mov rdx, rdx
jmp cleanup

lab2b1:
mov rdx, rdx
jmp cleanup

lab1:
cmp rdi, 0
je lab3
mov r9, -1
mov r11, rdi
add r11, r9
mov rsi, rax
mov rdi, rdx
mov rdx, r11
mov [rbx + 56], rdi
mov [rbx + 48], rsi
mov qword [rbx + 32], 0
mov qword [rbx + 16], 0
mov rsi, rbx
mov rbx, [rbx + 0]
cmp rbx, 0
je lab15
mov qword [rsi + 0], 0
jmp lab16

lab15:
mov rbx, rbp
mov rbp, [rbp + 0]
cmp rbp, 0
je lab13
mov qword [rbx + 0], 0
mov rcx, [rbx + 48]
cmp rcx, 0
je lab6
cmp qword [rcx + 0], 0
je lab4
add qword [rcx + 0], -1
jmp lab5

lab4:
mov [rcx + 0], rbp
mov rbp, rcx

lab5:

lab6:
mov rcx, [rbx + 32]
cmp rcx, 0
je lab9
cmp qword [rcx + 0], 0
je lab7
add qword [rcx + 0], -1
jmp lab8

lab7:
mov [rcx + 0], rbp
mov rbp, rcx

lab8:

lab9:
mov rcx, [rbx + 16]
cmp rcx, 0
je lab12
cmp qword [rcx + 0], 0
je lab10
add qword [rcx + 0], -1
jmp lab11

lab10:
mov [rcx + 0], rbp
mov rbp, rcx

lab11:

lab12:
jmp lab14

lab13:
mov rbp, rbx
add rbp, 64

lab14:

lab16:
lea rdi, [rel lab17]
mov rcx, rdi
mov rdi, rdx
mov rdx, rcx
mov rax, rsi
jmp lab1

lab17:
jmp near lab17b0
jmp near lab17b1

lab17b0:
cmp qword [rax + 0], 0
je lab21
add qword [rax + 0], -1
mov rcx, [rax + 48]
cmp rcx, 0
je lab18
add qword [rcx + 0], 1

lab18:
mov rcx, [rax + 32]
cmp rcx, 0
je lab19
add qword [rcx + 0], 1

lab19:
mov rcx, [rax + 16]
cmp rcx, 0
je lab20
add qword [rcx + 0], 1

lab20:
jmp lab22

lab21:
mov [rax + 0], rbx
mov rbx, rax

lab22:
mov rdx, [rax + 56]
mov rax, [rax + 48]
add rdx, 0
jmp rdx

lab17b1:
cmp qword [rsi + 0], 0
je lab26
add qword [rsi + 0], -1
mov rcx, [rsi + 48]
cmp rcx, 0
je lab23
add qword [rcx + 0], 1

lab23:
mov rcx, [rsi + 32]
cmp rcx, 0
je lab24
add qword [rcx + 0], 1

lab24:
mov rcx, [rsi + 16]
cmp rcx, 0
je lab25
add qword [rcx + 0], 1

lab25:
jmp lab27

lab26:
mov [rsi + 0], rbx
mov rbx, rsi

lab27:
mov rdi, [rsi + 56]
mov rsi, [rsi + 48]
mov r9, 1
mov r11, rdx
add r11, r9
mov rdx, r11
add rdi, 5
jmp rdi

lab3:
mov rsi, rax
mov rcx, rdi
mov rdi, rdx
mov rdx, rcx
add rdi, 5
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