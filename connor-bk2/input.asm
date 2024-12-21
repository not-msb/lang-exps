format elf executable

; Resulting Asm

  ; Builtin Runtime
segment readable executable
_start:
    call main

    mov eax, 1
    mov ebx, 0
    int 80h

@read:
    mov eax, 3
    mov ebx, [esp+4]
    mov ecx, [esp+8]
    mov edx, [esp+12]
    int 80h
    ret

@write:
    mov eax, 4
    mov ebx, [esp+4]
    mov ecx, [esp+8]
    mov edx, [esp+12]
    int 80h
    ret

  ; User written Code
main:
    push 51 ; header.len
    push header
    push 1
    call @write
    add esp, 12

    call compile

    push 12 ; footer.len
    push footer
    push 1
    call @write
    add esp, 12
    ret

compile:
    push 1
    push char
    push 0
    call @read
    add esp, 12

    cmp eax, 0
    je .end
    movzx eax, byte [char]
    sub al, 43
    cmp al, 50
    ja .end

    cmp al, 3
    jg .lt
    mov ebx, [.ptrTable+4*eax]
    mov ecx, [.lenTable+4*eax]
    jmp .cmpEnd

.ptrTable: dd plus, comma, minus, dot
.lenTable: dd 5, 12, 5, 13

    .lt:
        cmp al, 17
        jne .gt
        mov ebx, lt
        mov ecx, 4
        jmp .cmpEnd
    .gt:
        cmp al, 19
        jne .l_bracket
        mov ebx, gt
        mov ecx, 4
        jmp .cmpEnd
    .l_bracket:
        cmp al, 48
        jne .r_bracket
        mov ebx, l_bracket
        mov ecx, 10
        jmp .cmpEnd
    .r_bracket:
        cmp al, 50
        jne compile
        mov ebx, r_bracket
        mov ecx, 1

.cmpEnd:
    push ecx
    push ebx
    push 1
    call @write
    add esp, 12
    call compile
.end:
    ret

  ; Constant Data
segment readable writable
header: db "#include<unistd.h>", 10, "char r[65536],*e=r;", 10, "int main(){", 10
footer: db "return 0;", 10, "}", 10
char: db 0
gt:        db "++e;"
lt:        db "--e;"
plus:      db "++*e;"
minus:     db "--*e;"
dot:       db "write(1,e,1);"
comma:     db "read(0,e,0);"
l_bracket: db "while(*e){"
r_bracket: db "}"
