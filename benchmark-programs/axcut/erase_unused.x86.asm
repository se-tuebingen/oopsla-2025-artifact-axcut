; asmsyntax=nasm
;
; To create an executable:
; $ nasm -f elf64 erase_unused.x86.asm
; $ gcc -o erase_unused path/to/X86_64-infrastructure/driver.c erase_unused.x86.o

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
mov r9, 0
mov r10, 0
lea r11, [rel lab3]
mov rcx, r11
mov r11, rdi
mov rdi, r9
mov r9, rdx
mov rdx, rcx
mov rsi, r8
mov rax, r10
jmp lab1

lab3:

lab3b0:
mov rdx, rdx
jmp cleanup

lab1:
cmp r11, r9
jl lab4
cmp rsi, 0
je lab7
cmp qword [rsi + 0], 0
je lab5
add qword [rsi + 0], -1
jmp lab6

lab5:
mov [rsi + 0], rbp
mov rbp, rsi

lab6:

lab7:
mov rsi, rax
mov rdi, rdx
mov rdx, r11
jmp rdi

lab4:
cmp rsi, 0
je lab10
cmp qword [rsi + 0], 0
je lab8
add qword [rsi + 0], -1
jmp lab9

lab8:
mov [rsi + 0], rbp
mov rbp, rsi

lab9:

lab10:
mov rdi, r11
mov r11, 1
mov r13, rdi
add r13, r11
mov rsi, rax
mov rcx, rdi
mov rdi, rdx
mov rdx, rcx
mov r11, r13
mov [rbx + 56], r11
mov qword [rbx + 48], 0
mov [rbx + 40], r9
mov qword [rbx + 32], 0
mov [rbx + 24], rdi
mov [rbx + 16], rsi
mov rsi, rbx
mov rbx, [rbx + 0]
cmp rbx, 0
je lab22
mov qword [rsi + 0], 0
jmp lab23

lab22:
mov rbx, rbp
mov rbp, [rbp + 0]
cmp rbp, 0
je lab20
mov qword [rbx + 0], 0
mov rcx, [rbx + 48]
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
mov rcx, [rbx + 32]
cmp rcx, 0
je lab16
cmp qword [rcx + 0], 0
je lab14
add qword [rcx + 0], -1
jmp lab15

lab14:
mov [rcx + 0], rbp
mov rbp, rcx

lab15:

lab16:
mov rcx, [rbx + 16]
cmp rcx, 0
je lab19
cmp qword [rcx + 0], 0
je lab17
add qword [rcx + 0], -1
jmp lab18

lab17:
mov [rcx + 0], rbp
mov rbp, rcx

lab18:

lab19:
jmp lab21

lab20:
mov rbp, rbx
add rbp, 64

lab21:

lab23:
lea rdi, [rel lab24]
mov r9, 0
mov r10, 0
mov r11, 0
mov rcx, rdi
mov rdi, r11
mov r11, r9
mov r9, rdx
mov rdx, rcx
mov rax, rsi
mov rsi, r10
jmp lab2

lab24:
jmp near lab24b0
jmp near lab24b1

lab24b0:
cmp qword [rax + 0], 0
je lab28
add qword [rax + 0], -1
mov rcx, [rax + 48]
cmp rcx, 0
je lab25
add qword [rcx + 0], 1

lab25:
mov rcx, [rax + 32]
cmp rcx, 0
je lab26
add qword [rcx + 0], 1

lab26:
mov rcx, [rax + 16]
cmp rcx, 0
je lab27
add qword [rcx + 0], 1

lab27:
jmp lab29

lab28:
mov [rax + 0], rbx
mov rbx, rax

lab29:
mov r9, [rax + 56]
mov rdi, [rax + 40]
mov rdx, [rax + 24]
mov rax, [rax + 16]
mov r10, 0
mov r11, 0
mov rcx, r11
mov r11, r9
mov r9, rdi
mov rdi, rcx
mov rsi, r10
jmp lab1

lab24b1:
cmp qword [r8 + 0], 0
je lab33
add qword [r8 + 0], -1
mov rcx, [r8 + 48]
cmp rcx, 0
je lab30
add qword [rcx + 0], 1

lab30:
mov rcx, [r8 + 32]
cmp rcx, 0
je lab31
add qword [rcx + 0], 1

lab31:
mov rcx, [r8 + 16]
cmp rcx, 0
je lab32
add qword [rcx + 0], 1

lab32:
jmp lab34

lab33:
mov [r8 + 0], rbx
mov rbx, r8

lab34:
mov r13, [r8 + 56]
mov r11, [r8 + 40]
mov r9, [r8 + 24]
mov r8, [r8 + 16]
mov r10, rax
mov rcx, r11
mov r11, rdx
mov rdx, rcx
mov rcx, r13
mov r13, rdi
mov rdi, rcx
mov [rbx + 56], r13
mov qword [rbx + 48], 0
mov [rbx + 40], r11
mov [rbx + 32], r10
mov qword [rbx + 16], 0
mov r10, rbx
mov rbx, [rbx + 0]
cmp rbx, 0
je lab46
mov qword [r10 + 0], 0
jmp lab47

lab46:
mov rbx, rbp
mov rbp, [rbp + 0]
cmp rbp, 0
je lab44
mov qword [rbx + 0], 0
mov rcx, [rbx + 48]
cmp rcx, 0
je lab37
cmp qword [rcx + 0], 0
je lab35
add qword [rcx + 0], -1
jmp lab36

lab35:
mov [rcx + 0], rbp
mov rbp, rcx

lab36:

lab37:
mov rcx, [rbx + 32]
cmp rcx, 0
je lab40
cmp qword [rcx + 0], 0
je lab38
add qword [rcx + 0], -1
jmp lab39

lab38:
mov [rcx + 0], rbp
mov rbp, rcx

lab39:

lab40:
mov rcx, [rbx + 16]
cmp rcx, 0
je lab43
cmp qword [rcx + 0], 0
je lab41
add qword [rcx + 0], -1
jmp lab42

lab41:
mov [rcx + 0], rbp
mov rbp, rcx

lab42:

lab43:
jmp lab45

lab44:
mov rbp, rbx
add rbp, 64

lab45:

lab47:
mov r11, 5
mov rcx, r9
mov r9, rdx
mov rdx, rcx
mov rcx, r11
mov r11, rdi
mov rdi, rcx
mov rax, r8
mov rsi, r10
jmp lab1

lab2:
cmp r9, 0
je lab48
mov r13, -1
mov r15, r9
add r15, r13
mov r10, rsi
mov rcx, r11
mov r13, r11
mov r11, rdi
mov rdi, rcx
mov r9, r15
mov [rbx + 56], r13
mov qword [rbx + 48], 0
mov [rbx + 40], r11
mov [rbx + 32], r10
mov qword [rbx + 16], 0
mov r10, rbx
mov rbx, [rbx + 0]
cmp rbx, 0
je lab60
mov qword [r10 + 0], 0
jmp lab61

lab60:
mov rbx, rbp
mov rbp, [rbp + 0]
cmp rbp, 0
je lab58
mov qword [rbx + 0], 0
mov rcx, [rbx + 48]
cmp rcx, 0
je lab51
cmp qword [rcx + 0], 0
je lab49
add qword [rcx + 0], -1
jmp lab50

lab49:
mov [rcx + 0], rbp
mov rbp, rcx

lab50:

lab51:
mov rcx, [rbx + 32]
cmp rcx, 0
je lab54
cmp qword [rcx + 0], 0
je lab52
add qword [rcx + 0], -1
jmp lab53

lab52:
mov [rcx + 0], rbp
mov rbp, rcx

lab53:

lab54:
mov rcx, [rbx + 16]
cmp rcx, 0
je lab57
cmp qword [rcx + 0], 0
je lab55
add qword [rcx + 0], -1
jmp lab56

lab55:
mov [rcx + 0], rbp
mov rbp, rcx

lab56:

lab57:
jmp lab59

lab58:
mov rbp, rbx
add rbp, 64

lab59:

lab61:
mov r11, 5
mov rcx, r11
mov r11, rdi
mov rdi, rcx
mov rsi, r10
jmp lab2

lab48:
lea rcx, [rel lab62]
add rcx, rdi
jmp rcx

lab62:
jmp near lab62b0
jmp near lab62b1

lab62b0:
add rdx, 0
jmp rdx

lab62b1:
cmp qword [rsi + 0], 0
je lab66
add qword [rsi + 0], -1
mov rcx, [rsi + 48]
cmp rcx, 0
je lab63
add qword [rcx + 0], 1

lab63:
mov rcx, [rsi + 32]
cmp rcx, 0
je lab64
add qword [rcx + 0], 1

lab64:
mov rcx, [rsi + 16]
cmp rcx, 0
je lab65
add qword [rcx + 0], 1

lab65:
jmp lab67

lab66:
mov [rsi + 0], rbx
mov rbx, rsi

lab67:
mov r9, [rsi + 56]
mov rdi, [rsi + 40]
mov rsi, [rsi + 32]
mov r8, rax
mov rcx, rdi
mov rdi, r9
mov r9, rdx
mov rdx, rcx
mov rax, rsi
add r9, 5
jmp r9

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