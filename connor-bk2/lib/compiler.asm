reg_table:
.eax db 0
.ecx db 0
.edx db 0
.ebx db 0

REG_EAX = 0
REG_ECX = 1
REG_EDX = 2
REG_EBX = 3

IR_SIZE = 3
IR_RET = 0
IR_COPY = 1

; compile(output: [*]const u8, input: [*]const IrCode, length: u32) u32
compile:
    push ebx
    jmp .body
.loop:
    add ecx, IR_SIZE
    dec edx
.body:
    cmp edx, 0
    je .end

    movzx eax, byte [ecx]
    mov eax, [.table+4*eax]
    jmp eax
.table:
    dd .ret, .copy

.ret:
    movzx eax, byte [ecx+1]
    lea eax, [0xc0 + 8*eax]

    mov byte [ebx+1], 0xc3
    add ebx, 1
    jmp .loop
.copy:
    movzx eax, byte [ecx+2]
    lea eax, [0xc0 + 8*eax]
    add al, byte [ecx+1]

    mov byte [ebx], 0x89
    mov byte [ebx+1], al
    add ebx, 2
    jmp .loop
;.mov:
;    mov al, byte [ecx+1]
;    cmp al, REG_AX
;    jge .mov16
;    add al, 0xb8
;
;    mov byte [ebx], al
;    mov eax, dword [ecx+2]
;    mov dword [ebx+1], eax
;    add ebx, 5
;    jmp .loop
;.mov16:
;    add al, 0xb8
;    sub al, REG_AX
;    mov byte [ebx], 0x66
;
;    mov byte [ebx+1], al
;    mov ax, word [ecx+2]
;    mov word [ebx+2], ax
;    add ebx, 4
;    jmp .loop
.end:
    pop eax
    sub ebx, eax
    xchg eax, ebx
    ret
