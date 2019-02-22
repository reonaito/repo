        .data
str1:   .asciiz "窓は閉めましたよね？ (y/n)\n"
str2:   .asciiz "窓を閉めてください．鳩が入ります．\n"
str3:   .asciiz "お疲れ様でした．\n"
        .align 2
input:  .space 16

        .text
        .globl main
main:   li $v0, 4
        la $a0, str1
        syscall

loop:   li $v0, 8
        la $a0, input
        li $a1, 16
        syscall
        lh $t0, 0($a0)
        li $t1, 2681
        beq $t0, $t1, exit

        li $v0, 4
        la $a0, str2
        syscall
        j loop

exit:   li $v0, 4
        la $a0, str3
        syscall

        jr $ra
