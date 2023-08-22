.globl main
.format_true:
        .string "true"
.format_false:
        .string "false"
.L0:
        .string "%d\n"
        .text
main:
        pushq %rbp
        movq  %rsp, %rbp
        subq  $8, %rsp
        pushq %rbx
        pushq %r12
        pushq %r13
        pushq %r14
        pushq %r15
        movl  $2, %ebx
        movl  %ebx, -4(%rbp)
        movl  $5, %ebx
        movl  %ebx, -4(%rbp)
        movl  -4(%rbp), %ebx
        movl  %ebx, %esi
        leaq  .L0(%rip), %rbx
        movq  %rbx, %rdi
        xor   %eax, %eax
        call  printf@PLT
        movl  $0, %eax
.main_epilogue:
        popq  %r15
        popq  %r14
        popq  %r13
        popq  %r12
        popq  %rbx
        movq  %rbp, %rsp
        popq  %rbp
        ret   
.data
