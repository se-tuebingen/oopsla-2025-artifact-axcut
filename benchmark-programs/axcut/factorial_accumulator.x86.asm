; asmsyntax=nasm
;
; To create an executable:
; $ nasm -f elf64 factorial_accumulator.x86.asm
; $ gcc -o factorial_accumulator path/to/X86_64-infrastructure/driver.c factorial_accumulator.x86.o

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
mov rdi, 1
mov r8, 0
lea r9, [rel lab2]
mov rcx, r9
mov r9, rdi
mov rdi, rdx
mov rdx, rcx
mov rax, r8
jmp lab1

lab2:

lab2b0:
mov rdx, rdx
jmp cleanup

lab1:
cmp rdi, 0
je lab3
mov r11, -1
mov r13, rdi
add r13, r11
mov r11, r13
mov r13, rdi
imul r13, r9
mov rdi, r11
mov r9, r13
mov r11, 1000000007
mov rcx, rdx
mov r13, rax
mov rax, r9
cqo
idiv r11
mov rax, r13
mov r13, rdx
mov rdx, rcx
mov r9, r13
jmp lab1

lab3:
mov rsi, rax
mov rdi, rdx
mov rdx, r9
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