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
        subq  $0, %rsp
        pushq %rbx
        pushq %r12
        pushq %r13
        pushq %r14
        pushq %r15
        movl  $2, %ebx
        movl  $1, %r10d
        cmp   %r10d, %ebx
        jng   .L0
        movl  $2, %ebx
        andq  $-16, %rsp
        movl  %ebx, %esi
        leaq  .format_number(%rip), %rax
        movq  %rax, %rdi
        xor   %eax, %eax
        call  printf@PLT
        jmp   .L1
.L0:
        movl  $1, %ebx
        andq  $-16, %rsp
        movl  %ebx, %esi
        leaq  .format_number(%rip), %rax
        movq  %rax, %rdi
        xor   %eax, %eax
        call  printf@PLT
.L1:
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
