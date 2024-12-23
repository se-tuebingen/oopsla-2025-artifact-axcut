; asmsyntax=nasm
;
; To create an executable:
; $ nasm -f elf64 sum_range.x86.asm
; $ gcc -o sum_range path/to/X86_64-infrastructure/driver.c sum_range.x86.o

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
lea r9, [rel lab3]
mov [rbx + 56], r9
mov [rbx + 48], r8
mov qword [rbx + 32], 0
mov qword [rbx + 16], 0
mov r8, rbx
mov rbx, [rbx + 0]
cmp rbx, 0
je lab15
mov qword [r8 + 0], 0
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
lea r9, [rel lab17]
mov rcx, r9
mov r9, rdi
mov rdi, rdx
mov rdx, rcx
mov rax, r8
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
mov rsi, 0
mov rdi, 0
jmp lab2

lab17b1:
cmp qword [r8 + 0], 0
je lab26
add qword [r8 + 0], -1
mov rcx, [r8 + 48]
cmp rcx, 0
je lab23
add qword [rcx + 0], 1

lab23:
mov rcx, [r8 + 32]
cmp rcx, 0
je lab24
add qword [rcx + 0], 1

lab24:
mov rcx, [r8 + 16]
cmp rcx, 0
je lab25
add qword [rcx + 0], 1

lab25:
jmp lab27

lab26:
mov [r8 + 0], rbx
mov rbx, r8

lab27:
mov r9, [r8 + 56]
mov r8, [r8 + 48]
mov rsi, rax
mov rcx, r9
mov r9, rdi
mov rdi, rdx
mov rdx, rcx
mov rax, r8
mov [rbx + 56], r9
mov qword [rbx + 48], 0
mov [rbx + 40], rdi
mov [rbx + 32], rsi
mov qword [rbx + 16], 0
mov rsi, rbx
mov rbx, [rbx + 0]
cmp rbx, 0
je lab39
mov qword [rsi + 0], 0
jmp lab40

lab39:
mov rbx, rbp
mov rbp, [rbp + 0]
cmp rbp, 0
je lab37
mov qword [rbx + 0], 0
mov rcx, [rbx + 48]
cmp rcx, 0
je lab30
cmp qword [rcx + 0], 0
je lab28
add qword [rcx + 0], -1
jmp lab29

lab28:
mov [rcx + 0], rbp
mov rbp, rcx

lab29:

lab30:
mov rcx, [rbx + 32]
cmp rcx, 0
je lab33
cmp qword [rcx + 0], 0
je lab31
add qword [rcx + 0], -1
jmp lab32

lab31:
mov [rcx + 0], rbp
mov rbp, rcx

lab32:

lab33:
mov rcx, [rbx + 16]
cmp rcx, 0
je lab36
cmp qword [rcx + 0], 0
je lab34
add qword [rcx + 0], -1
jmp lab35

lab34:
mov [rcx + 0], rbp
mov rbp, rcx

lab35:

lab36:
jmp lab38

lab37:
mov rbp, rbx
add rbp, 64

lab38:

lab40:
mov rdi, 5
jmp lab2

lab3:

lab3b0:
mov rdx, rdx
jmp cleanup

lab1:
cmp r9, rdi
jl lab41
add rdx, 0
jmp rdx

lab41:
mov r10, rax
mov r11, rdx
mov rdx, r9
mov [rbx + 56], r11
mov [rbx + 48], r10
mov [rbx + 40], r9
mov qword [rbx + 32], 0
mov qword [rbx + 16], 0
mov r8, rbx
mov rbx, [rbx + 0]
cmp rbx, 0
je lab53
mov qword [r8 + 0], 0
jmp lab54

lab53:
mov rbx, rbp
mov rbp, [rbp + 0]
cmp rbp, 0
je lab51
mov qword [rbx + 0], 0
mov rcx, [rbx + 48]
cmp rcx, 0
je lab44
cmp qword [rcx + 0], 0
je lab42
add qword [rcx + 0], -1
jmp lab43

lab42:
mov [rcx + 0], rbp
mov rbp, rcx

lab43:

lab44:
mov rcx, [rbx + 32]
cmp rcx, 0
je lab47
cmp qword [rcx + 0], 0
je lab45
add qword [rcx + 0], -1
jmp lab46

lab45:
mov [rcx + 0], rbp
mov rbp, rcx

lab46:

lab47:
mov rcx, [rbx + 16]
cmp rcx, 0
je lab50
cmp qword [rcx + 0], 0
je lab48
add qword [rcx + 0], -1
jmp lab49

lab48:
mov [rcx + 0], rbp
mov rbp, rcx

lab49:

lab50:
jmp lab52

lab51:
mov rbp, rbx
add rbp, 64

lab52:

lab54:
lea r9, [rel lab55]
mov r11, 1
mov r13, rdx
add r13, r11
mov rax, r8
mov rdx, r9
mov r9, r13
jmp lab1

lab55:
jmp near lab55b0
jmp near lab55b1

lab55b0:
cmp qword [rax + 0], 0
je lab59
add qword [rax + 0], -1
mov rcx, [rax + 48]
cmp rcx, 0
je lab56
add qword [rcx + 0], 1

lab56:
mov rcx, [rax + 32]
cmp rcx, 0
je lab57
add qword [rcx + 0], 1

lab57:
mov rcx, [rax + 16]
cmp rcx, 0
je lab58
add qword [rcx + 0], 1

lab58:
jmp lab60

lab59:
mov [rax + 0], rbx
mov rbx, rax

lab60:
mov rdi, [rax + 56]
mov rsi, [rax + 48]
mov rdx, [rax + 40]
mov r8, 0
mov r9, 0
mov rcx, r9
mov r9, rdi
mov rdi, rdx
mov rdx, rcx
mov rax, r8
mov r8, rsi
add r9, 5
jmp r9

lab55b1:
cmp qword [r8 + 0], 0
je lab64
add qword [r8 + 0], -1
mov rcx, [r8 + 48]
cmp rcx, 0
je lab61
add qword [rcx + 0], 1

lab61:
mov rcx, [r8 + 32]
cmp rcx, 0
je lab62
add qword [rcx + 0], 1

lab62:
mov rcx, [r8 + 16]
cmp rcx, 0
je lab63
add qword [rcx + 0], 1

lab63:
jmp lab65

lab64:
mov [r8 + 0], rbx
mov rbx, r8

lab65:
mov r11, [r8 + 56]
mov r10, [r8 + 48]
mov r9, [r8 + 40]
mov r8, rax
mov rcx, r9
mov r9, rdx
mov rdx, rcx
mov rcx, r11
mov r11, rdi
mov rdi, rcx
mov rsi, r10
mov [rbx + 56], r11
mov qword [rbx + 48], 0
mov [rbx + 40], r9
mov [rbx + 32], r8
mov qword [rbx + 16], 0
mov r8, rbx
mov rbx, [rbx + 0]
cmp rbx, 0
je lab77
mov qword [r8 + 0], 0
jmp lab78

lab77:
mov rbx, rbp
mov rbp, [rbp + 0]
cmp rbp, 0
je lab75
mov qword [rbx + 0], 0
mov rcx, [rbx + 48]
cmp rcx, 0
je lab68
cmp qword [rcx + 0], 0
je lab66
add qword [rcx + 0], -1
jmp lab67

lab66:
mov [rcx + 0], rbp
mov rbp, rcx

lab67:

lab68:
mov rcx, [rbx + 32]
cmp rcx, 0
je lab71
cmp qword [rcx + 0], 0
je lab69
add qword [rcx + 0], -1
jmp lab70

lab69:
mov [rcx + 0], rbp
mov rbp, rcx

lab70:

lab71:
mov rcx, [rbx + 16]
cmp rcx, 0
je lab74
cmp qword [rcx + 0], 0
je lab72
add qword [rcx + 0], -1
jmp lab73

lab72:
mov [rcx + 0], rbp
mov rbp, rcx

lab73:

lab74:
jmp lab76

lab75:
mov rbp, rbx
add rbp, 64

lab76:

lab78:
mov r9, 5
mov rcx, r9
mov r9, rdi
mov rdi, rdx
mov rdx, rcx
mov rax, r8
mov r8, rsi
add r9, 5
jmp r9

lab2:
lea rcx, [rel lab79]
add rcx, rdi
jmp rcx

lab79:
jmp near lab79b0
jmp near lab79b1

lab79b0:
mov rdi, 0
mov rsi, rax
mov rcx, rdi
mov rdi, rdx
mov rdx, rcx
jmp rdi

lab79b1:
cmp qword [rsi + 0], 0
je lab83
add qword [rsi + 0], -1
mov rcx, [rsi + 48]
cmp rcx, 0
je lab80
add qword [rcx + 0], 1

lab80:
mov rcx, [rsi + 32]
cmp rcx, 0
je lab81
add qword [rcx + 0], 1

lab81:
mov rcx, [rsi + 16]
cmp rcx, 0
je lab82
add qword [rcx + 0], 1

lab82:
jmp lab84

lab83:
mov [rsi + 0], rbx
mov rbx, rsi

lab84:
mov r9, [rsi + 56]
mov rdi, [rsi + 40]
mov rsi, [rsi + 32]
mov rcx, rsi
mov rsi, rax
mov rax, rcx
mov rcx, rdi
mov rdi, rdx
mov rdx, rcx
mov [rbx + 56], r9
mov qword [rbx + 48], 0
mov [rbx + 40], rdi
mov [rbx + 32], rsi
mov qword [rbx + 16], 0
mov rsi, rbx
mov rbx, [rbx + 0]
cmp rbx, 0
je lab96
mov qword [rsi + 0], 0
jmp lab97

lab96:
mov rbx, rbp
mov rbp, [rbp + 0]
cmp rbp, 0
je lab94
mov qword [rbx + 0], 0
mov rcx, [rbx + 48]
cmp rcx, 0
je lab87
cmp qword [rcx + 0], 0
je lab85
add qword [rcx + 0], -1
jmp lab86

lab85:
mov [rcx + 0], rbp
mov rbp, rcx

lab86:

lab87:
mov rcx, [rbx + 32]
cmp rcx, 0
je lab90
cmp qword [rcx + 0], 0
je lab88
add qword [rcx + 0], -1
jmp lab89

lab88:
mov [rcx + 0], rbp
mov rbp, rcx

lab89:

lab90:
mov rcx, [rbx + 16]
cmp rcx, 0
je lab93
cmp qword [rcx + 0], 0
je lab91
add qword [rcx + 0], -1
jmp lab92

lab91:
mov [rcx + 0], rbp
mov rbp, rcx

lab92:

lab93:
jmp lab95

lab94:
mov rbp, rbx
add rbp, 64

lab95:

lab97:
lea rdi, [rel lab98]
mov rcx, rsi
mov rsi, rax
mov rax, rcx
mov rcx, rdi
mov rdi, rdx
mov rdx, rcx
jmp lab2

lab98:

lab98b0:
cmp qword [rsi + 0], 0
je lab102
add qword [rsi + 0], -1
mov rcx, [rsi + 48]
cmp rcx, 0
je lab99
add qword [rcx + 0], 1

lab99:
mov rcx, [rsi + 32]
cmp rcx, 0
je lab100
add qword [rcx + 0], 1

lab100:
mov rcx, [rsi + 16]
cmp rcx, 0
je lab101
add qword [rcx + 0], 1

lab101:
jmp lab103

lab102:
mov [rsi + 0], rbx
mov rbx, rsi

lab103:
mov r9, [rsi + 56]
mov rdi, [rsi + 40]
mov rsi, [rsi + 32]
mov r11, r9
add r11, rdx
mov rdx, r11
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