# header
.globl main
.LC0:
.string "%d\n"
main: 
# generated assembly
MOVQ $1, %rbx
MOVQ $1, %r10
ADDQ %rbx, %r10
# code for printf()
PUSHQ %rbp
MOVQ %rsp, %rbp
MOVQ  %r10, %rsi
LEAQ .LC0(%rip), %rax
MOVQ %rax, %rdi
MOVL $0, %eax
CALL printf@PLT
MOVL $0, %eax
POPQ %rbp
RET
