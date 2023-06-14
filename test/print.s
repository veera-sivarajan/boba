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
        PUSHq %rbp
        MOVQ %rsp, %rbp
        MOVQ  %r10, %rsi
        LEAQ .LC0(%rip), %rax
        MOVQ %rax, %rdi
        MOVL $0, %eax
        CALL printf@PLT
        MOVL $0, %eax
        POPQ %rbp
        ret
