.text
.globl syscall1
syscall1:
    pushq %rbp
    movq %rsp, %rbp
    movl %rdi, %rax
    movl %rsi, %rdi
    syscall
    leave
    ret
.type syscall1, @function
.size syscall1, .-syscall1

.text
.globl syscall3
syscall3:
    pushq %rbp
    movq %rsp, %rbp
    movl %rdi, %rax
    movl %rsi, %rdi
    movl %rdx, %rsi
    movl %rcx, %rdx
    syscall
    leave
    ret
.type syscall3, @function
.size syscall3, .-syscall3

.text
.globl exit
exit:
    pushq %rbp
    movq %rsp, %rbp
    movl $60, %rax
    syscall
    leave
    ret
.type exit, @function
.size exit, .-exit
