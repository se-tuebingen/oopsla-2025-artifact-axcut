; asmsyntax=nasm
;
; To create an executable:
; $ nasm -f elf64 iterate_increment.x86.asm
; $ gcc -o iterate_increment path/to/X86_64-infrastructure/driver.c iterate_increment.x86.o

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
mov rdi, 0
mov r8, 0
lea r9, [rel lab2]
mov r10, 0
lea r11, [rel lab3]
mov rcx, r11
mov r11, rdx
mov rdx, rcx
mov rax, r10
jmp lab1

lab3:

lab3b0:
mov rdx, rdx
jmp cleanup

lab2:

lab2b0:
mov r9, 1
mov r11, rdi
add r11, r9
mov rsi, rax
mov rdi, rdx
mov rdx, r11
jmp rdi

lab1:
cmp r11, 0
je lab4
mov r13, -1
mov r15, r11
add r15, r13
cmp r8, 0
je lab5
add qword [r8 + 0], 1

lab5:
mov r10, rax
mov r11, rdx
mov rax, r8
mov rdx, r9
mov r13, r15
mov [rbx + 56], r13
mov qword [rbx + 48], 0
mov [rbx + 40], r11
mov [rbx + 32], r10
mov [rbx + 24], r9
mov [rbx + 16], r8
mov r8, rbx
mov rbx, [rbx + 0]
cmp rbx, 0
je lab17
mov qword [r8 + 0], 0
jmp lab18

lab17:
mov rbx, rbp
mov rbp, [rbp + 0]
cmp rbp, 0
je lab15
mov qword [rbx + 0], 0
mov rcx, [rbx + 48]
cmp rcx, 0
je lab8
cmp qword [rcx + 0], 0
je lab6
add qword [rcx + 0], -1
jmp lab7

lab6:
mov [rcx + 0], rbp
mov rbp, rcx

lab7:

lab8:
mov rcx, [rbx + 32]
cmp rcx, 0
je lab11
cmp qword [rcx + 0], 0
je lab9
add qword [rcx + 0], -1
jmp lab10

lab9:
mov [rcx + 0], rbp
mov rbp, rcx

lab10:

lab11:
mov rcx, [rbx + 16]
cmp rcx, 0
je lab14
cmp qword [rcx + 0], 0
je lab12
add qword [rcx + 0], -1
jmp lab13

lab12:
mov [rcx + 0], rbp
mov rbp, rcx

lab13:

lab14:
jmp lab16

lab15:
mov rbp, rbx
add rbp, 64

lab16:

lab18:
lea r9, [rel lab19]
mov rcx, r8
mov r8, rax
mov rax, rcx
mov rcx, r9
mov r9, rdx
mov rdx, rcx
jmp r9

lab19:

lab19b0:
cmp qword [rsi + 0], 0
je lab23
add qword [rsi + 0], -1
mov rcx, [rsi + 48]
cmp rcx, 0
je lab20
add qword [rcx + 0], 1

lab20:
mov rcx, [rsi + 32]
cmp rcx, 0
je lab21
add qword [rcx + 0], 1

lab21:
mov rcx, [rsi + 16]
cmp rcx, 0
je lab22
add qword [rcx + 0], 1

lab22:
jmp lab24

lab23:
mov [rsi + 0], rbx
mov rbx, rsi

lab24:
mov r11, [rsi + 56]
mov r9, [rsi + 40]
mov r8, [rsi + 32]
mov rdi, [rsi + 24]
mov rsi, [rsi + 16]
mov rcx, r9
mov r9, rdi
mov rdi, rdx
mov rdx, rcx
mov rax, r8
mov r8, rsi
jmp lab1

lab4:
cmp r8, 0
je lab27
cmp qword [r8 + 0], 0
je lab25
add qword [r8 + 0], -1
jmp lab26

lab25:
mov [r8 + 0], rbp
mov rbp, r8

lab26:

lab27:
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