global _start

section .text
_start:
    sub rsp, 8
    call main
    add rsp, 8

    mov rdi, [-8+rsp]
    mov rax, 60
    syscall
