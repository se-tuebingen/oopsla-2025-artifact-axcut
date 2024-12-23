; asmsyntax=nasm
;
; To create an executable:
; $ nasm -f elf64 lookup_tree.x86.asm
; $ gcc -o lookup_tree path/to/X86_64-infrastructure/driver.c lookup_tree.x86.o

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
cmp qword [rsi + 0], 0
je lab21
add qword [rsi + 0], -1
mov rcx, [rsi + 48]
cmp rcx, 0
je lab18
add qword [rcx + 0], 1

lab18:
mov rcx, [rsi + 32]
cmp rcx, 0
je lab19
add qword [rcx + 0], 1

lab19:
mov rcx, [rsi + 16]
cmp rcx, 0
je lab20
add qword [rcx + 0], 1

lab20:
jmp lab22

lab21:
mov [rsi + 0], rbx
mov rbx, rsi

lab22:
mov rdi, [rsi + 56]
mov rsi, [rsi + 48]
mov rcx, rdi
mov rdi, rdx
mov rdx, rcx
mov rax, rsi
mov [rbx + 56], rdi
mov qword [rbx + 48], 0
mov qword [rbx + 32], 0
mov qword [rbx + 16], 0
mov rsi, rbx
mov rbx, [rbx + 0]
cmp rbx, 0
je lab34
mov qword [rsi + 0], 0
jmp lab35

lab34:
mov rbx, rbp
mov rbp, [rbp + 0]
cmp rbp, 0
je lab32
mov qword [rbx + 0], 0
mov rcx, [rbx + 48]
cmp rcx, 0
je lab25
cmp qword [rcx + 0], 0
je lab23
add qword [rcx + 0], -1
jmp lab24

lab23:
mov [rcx + 0], rbp
mov rbp, rcx

lab24:

lab25:
mov rcx, [rbx + 32]
cmp rcx, 0
je lab28
cmp qword [rcx + 0], 0
je lab26
add qword [rcx + 0], -1
jmp lab27

lab26:
mov [rcx + 0], rbp
mov rbp, rcx

lab27:

lab28:
mov rcx, [rbx + 16]
cmp rcx, 0
je lab31
cmp qword [rcx + 0], 0
je lab29
add qword [rcx + 0], -1
jmp lab30

lab29:
mov [rcx + 0], rbp
mov rbp, rcx

lab30:

lab31:
jmp lab33

lab32:
mov rbp, rbx
add rbp, 64

lab33:

lab35:
mov rdi, 0
jmp lab2

lab17b1:
cmp qword [r8 + 0], 0
je lab39
add qword [r8 + 0], -1
mov rcx, [r8 + 48]
cmp rcx, 0
je lab36
add qword [rcx + 0], 1

lab36:
mov rcx, [r8 + 32]
cmp rcx, 0
je lab37
add qword [rcx + 0], 1

lab37:
mov rcx, [r8 + 16]
cmp rcx, 0
je lab38
add qword [rcx + 0], 1

lab38:
jmp lab40

lab39:
mov [r8 + 0], rbx
mov rbx, r8

lab40:
mov r9, [r8 + 56]
mov r8, [r8 + 48]
mov rcx, r8
mov r8, rsi
mov rsi, rax
mov rax, rcx
mov rcx, r9
mov r9, rdi
mov rdi, rdx
mov rdx, rcx
mov [rbx + 56], r9
mov [rbx + 48], r8
mov [rbx + 40], rdi
mov [rbx + 32], rsi
mov qword [rbx + 16], 0
mov rsi, rbx
mov rbx, [rbx + 0]
cmp rbx, 0
je lab52
mov qword [rsi + 0], 0
jmp lab53

lab52:
mov rbx, rbp
mov rbp, [rbp + 0]
cmp rbp, 0
je lab50
mov qword [rbx + 0], 0
mov rcx, [rbx + 48]
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
mov rcx, [rbx + 32]
cmp rcx, 0
je lab46
cmp qword [rcx + 0], 0
je lab44
add qword [rcx + 0], -1
jmp lab45

lab44:
mov [rcx + 0], rbp
mov rbp, rcx

lab45:

lab46:
mov rcx, [rbx + 16]
cmp rcx, 0
je lab49
cmp qword [rcx + 0], 0
je lab47
add qword [rcx + 0], -1
jmp lab48

lab47:
mov [rcx + 0], rbp
mov rbp, rcx

lab48:

lab49:
jmp lab51

lab50:
mov rbp, rbx
add rbp, 64

lab51:

lab53:
mov rdi, 5
jmp lab2

lab3:

lab3b0:
mov rdx, rdx
jmp cleanup

lab1:
cmp r9, rdi
jl lab54
mov rsi, rax
mov rcx, rdi
mov rdi, rdx
mov rdx, rcx
add rdi, 0
jmp rdi

lab54:
mov r11, 1
mov r13, r9
add r13, r11
mov r8, rax
mov r9, rdx
mov rdx, r13
mov [rbx + 56], r9
mov [rbx + 48], r8
mov qword [rbx + 32], 0
mov qword [rbx + 16], 0
mov r8, rbx
mov rbx, [rbx + 0]
cmp rbx, 0
je lab66
mov qword [r8 + 0], 0
jmp lab67

lab66:
mov rbx, rbp
mov rbp, [rbp + 0]
cmp rbp, 0
je lab64
mov qword [rbx + 0], 0
mov rcx, [rbx + 48]
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
mov rcx, [rbx + 32]
cmp rcx, 0
je lab60
cmp qword [rcx + 0], 0
je lab58
add qword [rcx + 0], -1
jmp lab59

lab58:
mov [rcx + 0], rbp
mov rbp, rcx

lab59:

lab60:
mov rcx, [rbx + 16]
cmp rcx, 0
je lab63
cmp qword [rcx + 0], 0
je lab61
add qword [rcx + 0], -1
jmp lab62

lab61:
mov [rcx + 0], rbp
mov rbp, rcx

lab62:

lab63:
jmp lab65

lab64:
mov rbp, rbx
add rbp, 64

lab65:

lab67:
lea r9, [rel lab68]
mov rcx, r9
mov r9, rdx
mov rdx, rcx
mov rax, r8
jmp lab1

lab68:
jmp near lab68b0
jmp near lab68b1

lab68b0:
cmp qword [rsi + 0], 0
je lab72
add qword [rsi + 0], -1
mov rcx, [rsi + 48]
cmp rcx, 0
je lab69
add qword [rcx + 0], 1

lab69:
mov rcx, [rsi + 32]
cmp rcx, 0
je lab70
add qword [rcx + 0], 1

lab70:
mov rcx, [rsi + 16]
cmp rcx, 0
je lab71
add qword [rcx + 0], 1

lab71:
jmp lab73

lab72:
mov [rsi + 0], rbx
mov rbx, rsi

lab73:
mov rdi, [rsi + 56]
mov rsi, [rsi + 48]
mov rcx, rdi
mov rdi, rdx
mov rdx, rcx
mov rax, rsi
mov [rbx + 56], rdi
mov qword [rbx + 48], 0
mov qword [rbx + 32], 0
mov qword [rbx + 16], 0
mov rsi, rbx
mov rbx, [rbx + 0]
cmp rbx, 0
je lab85
mov qword [rsi + 0], 0
jmp lab86

lab85:
mov rbx, rbp
mov rbp, [rbp + 0]
cmp rbp, 0
je lab83
mov qword [rbx + 0], 0
mov rcx, [rbx + 48]
cmp rcx, 0
je lab76
cmp qword [rcx + 0], 0
je lab74
add qword [rcx + 0], -1
jmp lab75

lab74:
mov [rcx + 0], rbp
mov rbp, rcx

lab75:

lab76:
mov rcx, [rbx + 32]
cmp rcx, 0
je lab79
cmp qword [rcx + 0], 0
je lab77
add qword [rcx + 0], -1
jmp lab78

lab77:
mov [rcx + 0], rbp
mov rbp, rcx

lab78:

lab79:
mov rcx, [rbx + 16]
cmp rcx, 0
je lab82
cmp qword [rcx + 0], 0
je lab80
add qword [rcx + 0], -1
jmp lab81

lab80:
mov [rcx + 0], rbp
mov rbp, rcx

lab81:

lab82:
jmp lab84

lab83:
mov rbp, rbx
add rbp, 64

lab84:

lab86:
mov rdi, 0
cmp rsi, 0
je lab87
add qword [rsi + 0], 1

lab87:
mov r8, rax
mov r9, rdx
mov rax, rsi
mov rdx, rdi
add r9, 5
jmp r9

lab68b1:
cmp qword [r8 + 0], 0
je lab91
add qword [r8 + 0], -1
mov rcx, [r8 + 48]
cmp rcx, 0
je lab88
add qword [rcx + 0], 1

lab88:
mov rcx, [r8 + 32]
cmp rcx, 0
je lab89
add qword [rcx + 0], 1

lab89:
mov rcx, [r8 + 16]
cmp rcx, 0
je lab90
add qword [rcx + 0], 1

lab90:
jmp lab92

lab91:
mov [r8 + 0], rbx
mov rbx, r8

lab92:
mov r9, [r8 + 56]
mov r8, [r8 + 48]
mov rcx, r8
mov r8, rsi
mov rsi, rax
mov rax, rcx
mov rcx, r9
mov r9, rdi
mov rdi, rdx
mov rdx, rcx
mov [rbx + 56], r9
mov [rbx + 48], r8
mov [rbx + 40], rdi
mov [rbx + 32], rsi
mov qword [rbx + 16], 0
mov rsi, rbx
mov rbx, [rbx + 0]
cmp rbx, 0
je lab104
mov qword [rsi + 0], 0
jmp lab105

lab104:
mov rbx, rbp
mov rbp, [rbp + 0]
cmp rbp, 0
je lab102
mov qword [rbx + 0], 0
mov rcx, [rbx + 48]
cmp rcx, 0
je lab95
cmp qword [rcx + 0], 0
je lab93
add qword [rcx + 0], -1
jmp lab94

lab93:
mov [rcx + 0], rbp
mov rbp, rcx

lab94:

lab95:
mov rcx, [rbx + 32]
cmp rcx, 0
je lab98
cmp qword [rcx + 0], 0
je lab96
add qword [rcx + 0], -1
jmp lab97

lab96:
mov [rcx + 0], rbp
mov rbp, rcx

lab97:

lab98:
mov rcx, [rbx + 16]
cmp rcx, 0
je lab101
cmp qword [rcx + 0], 0
je lab99
add qword [rcx + 0], -1
jmp lab100

lab99:
mov [rcx + 0], rbp
mov rbp, rcx

lab100:

lab101:
jmp lab103

lab102:
mov rbp, rbx
add rbp, 64

lab103:

lab105:
mov rdi, 5
cmp rsi, 0
je lab106
add qword [rsi + 0], 1

lab106:
mov r8, rax
mov r9, rdx
mov rax, rsi
mov rdx, rdi
add r9, 5
jmp r9

lab2:
lea rcx, [rel lab107]
add rcx, rdi
jmp rcx

lab107:
jmp near lab107b0
jmp near lab107b1

lab107b0:
cmp qword [rsi + 0], 0
je lab111
add qword [rsi + 0], -1
mov rcx, [rsi + 48]
cmp rcx, 0
je lab108
add qword [rcx + 0], 1

lab108:
mov rcx, [rsi + 32]
cmp rcx, 0
je lab109
add qword [rcx + 0], 1

lab109:
mov rcx, [rsi + 16]
cmp rcx, 0
je lab110
add qword [rcx + 0], 1

lab110:
jmp lab112

lab111:
mov [rsi + 0], rbx
mov rbx, rsi

lab112:
mov rdi, [rsi + 56]
mov rsi, rax
mov rcx, rdi
mov rdi, rdx
mov rdx, rcx
jmp rdi

lab107b1:
cmp qword [rsi + 0], 0
je lab116
add qword [rsi + 0], -1
mov rcx, [rsi + 48]
cmp rcx, 0
je lab113
add qword [rcx + 0], 1

lab113:
mov rcx, [rsi + 32]
cmp rcx, 0
je lab114
add qword [rcx + 0], 1

lab114:
mov rcx, [rsi + 16]
cmp rcx, 0
je lab115
add qword [rcx + 0], 1

lab115:
jmp lab117

lab116:
mov [rsi + 0], rbx
mov rbx, rsi

lab117:
mov r9, [rsi + 56]
mov r8, [rsi + 48]
mov rdi, [rsi + 40]
mov rsi, [rsi + 32]
cmp rsi, 0
je lab120
cmp qword [rsi + 0], 0
je lab118
add qword [rsi + 0], -1
jmp lab119

lab118:
mov [rsi + 0], rbp
mov rbp, rsi

lab119:

lab120:
mov rsi, r8
mov rdi, r9
jmp lab2

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