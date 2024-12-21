global main

section .text
main:
    sub rsp, 8
    call _main
    add rsp, 8
    mov rax, [-8+rsp]
    ret
