.globl main
.format_string:
        .string "%s\n"
.format_number:
        .string "%d\n"
.format_true:
        .string "true\n"
.format_false:
        .string "false\n"
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
        leaq  .format_number(%rip), %rax
        movq  %rax, %rdi
        xor   %eax, %eax
        call  printf@PLT
.main_epilogue:
        popq  %r15
        popq  %r14
        popq  %r13
        popq  %r12
        popq  %rbx
        addq  $8, %rsp
        movq  %rbp, %rsp
        popq  %rbp
        ret   
.data
