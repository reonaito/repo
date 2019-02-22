section .data
        str1 db "窓は閉めましたよね？ (y/n)", 10
        str2 db "窓を閉めてください．鳩が入ります．", 10
        str3 db "お疲れ様でした．", 10
        input db 64

section .text
global  start
start:  mov rax, 0x2000004
        mov rdi, 1
        mov rsi, str1
        mov rdx, 37
        syscall

loop:   mov rax, 0x2000003
        mov rdi, 0
        mov rsi, input
        mov rdx, 64
        syscall

        mov rbx, input
        mov ax, [rbx]
        cmp ax, 2681
        je exit

        mov rax, 0x2000004
        mov rdi, 1
        mov rsi, str2
        mov rdx, 52
        syscall
        jmp loop

exit:   mov rax, 0x2000004
        mov rdi, 1
        mov rsi, str3
        mov rdx, 25
        syscall

        mov rax, 0x2000001
        mov rdi, 0
        syscall
