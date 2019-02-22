# 窓を閉めてください．鳩が入ります．

## オリジナルのプログラム（C言語）

```c
#include <stdio.h>

int main(int argc, char* argv[]){
  printf("窓は閉めましたよね？ (y/n)\n");
  while(1){
    char c;
    scanf("%c", &c);
    if (c != 'y'){
      printf("窓を閉めてください．鳩が入ります．\n");
    }
    else break;
  }
  printf("お疲れ様でした．\n");
  return 0;
}
```

実はこのコードには `scanf` の仕様に起因するバグがあり，ユーザが `n` など `y` 以外の文字を入力してリターンキーを押すと， `窓を閉めてください．鳩が入ります．` が2回表示される．   
以下のように修正すると正しい（と思われる）挙動になる．

```c
#include <stdio.h>

int main(int argc, char* argv[]){
  printf("窓は閉めましたよね？ (y/n)\n");
  while(1){
    char str[256];
    scanf("%s", str);
    if (str[0] != 'y'){
      printf("窓を閉めてください．鳩が入ります．\n");
    }
    else break;
  }
  printf("お疲れ様でした．\n");
  return 0;
}
```

## Racket版

```
#lang racket

(display "窓は閉めましたよね？ (y/n)\n")
(call/cc
 (lambda (break)
   (let f ()
     (let ((c (read-line)))
       (if (not (equal? c "y"))
           (display "窓を閉めてください．鳩が入ります．\n")
           (break))
       (f)))))
(display "お疲れ様でした．\n")
```

Racketには文という概念がない．当然 `while` 文はない．  
無限ループは再帰呼び出しによって実装する．  
`break` を実装するためには `call/cc` を使う必要がある．  
`break` を実装せず，入力が `y` でないときのみ再帰するという実装でも良い．

## Racket + syntax-case版

```
#lang racket

(define-syntax f
  (begin
    (display "窓は閉めましたよね？ (y/n)\n")
    (lambda (x)
      (syntax-case x (y)
        ((_) (let ((c (read))) #`(f #,c)))
        ((_ y) #'(display "お疲れ様でした．\n"))
        (_
         (begin
           (display "窓を閉めてください．鳩が入ります．\n")
           #'(f)))))))
(f)
```

入力された文字がマクロの第1引数になるようにマクロ展開する．  
第1引数が `y` なら `お疲れ様でした` に展開し終了．

## MIPS版

```
brew install spim
```

```
        .data
str1:   .asciiz "窓は閉めましたよね？ (y/n)\n"
str2:   .asciiz "窓を閉めてください．鳩が入ります．\n"
str3:   .asciiz "お疲れ様でした．\n"

        .align 2            #MIPSのデータ格納と関係？
input:  .space 16           #入力データ2文字分のメモリ領域

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
        li $t1, 2681        #"y\n"
        beq $t0, $t1, exit

        li $v0, 4
        la $a0, str2
        syscall
        j loop

exit:   li $v0, 4
        la $a0, str3
        syscall

        jr $ra
```

```
spim -file hato_mips.asm
```

## x86 64bit

### ハローワールド

```
section .data
hello_world db "Hello World!", 0x0a

section .text
global start

start:
mov rax, 0x2000004      ; System call write = 4
mov rdi, 1              ; Write to standard out = 1
mov rsi, hello_world    ; The address of hello_world string
mov rdx, 14             ; The size to write
syscall                 ; Invoke the kernel
mov rax, 0x2000001      ; System call number for exit = 1
mov rdi, 0              ; Exit success = 0
syscall                 ; Invoke the kernel
```

```
nasm -f macho64 hello.s
```

NASMはx86のためのアセンブラである．  
`-f macho64` は，64bitのmacOSのオブジェクトファイルとして生成するオプション．

```
ld -macosx_version_min 10.7.0 -lSystem -o hello hello.o
```

`ld` を用いると，オブジェクトファイルから実行可能ファイルを生成できる．  
`-macosx_version_min` は，生成したファイルを実行するMacのバージョンを指定するオプションである．  
`-lSystem` は，よくわからない．
省いても動くものは動く．

```
./hello
```

### 鳩

```
section .data
        str1 db "窓は閉めましたよね？ (y/n)", 10
        str2 db "窓を閉めてください．鳩が入ります．", 10
        str3 db "お疲れ様でした．", 10
        input db 64

section .text
global  start
start:  mov rax, 0x2000004  ;write
        mov rdi, 1          ;stdout
        mov rsi, str1
        mov rdx, 37         ;37 bytes
        syscall

loop:   mov rax, 0x2000003  ;read
        mov rdi, 0          ;stdin
        mov rsi, input
        mov rdx, 64         ;64 bytes
        syscall

        mov rbx, input
        mov ax, [rbx]
        cmp ax, 2681        ;y\n
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
```

```
nasm -f macho64 hato_x86.s
ld -macosx_version_min 10.7.0 -lSystem hato_x86.o
./a.out
```
