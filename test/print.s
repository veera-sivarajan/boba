        .globl main
        .LC0:
        .string "%d\n"
main:

        MOVQ  $100, %rbx
        MOVQ  $100, %r10
        MOVQ  %r10, %rax
        IMUL %rbx
        MOVQ  %rax, %r11
        MOVQ  $10, %rbx
        MOVQ  %r11, %rax
        CQO
        IDIV %rbx
        MOVQ  %rax, %r10

        pushq %rbp
        movq %rsp, %rbp
        movq  %r10, %rsi
        leaq .LC0(%rip), %rax
        movq %rax, %rdi
        movl $0, %eax
        call printf@PLT
        movl $0, %eax
        popq %rbp
        ret
