; utos(buffer: [10]const u8, x: u32) u32
; edi - esi
utos:
    push edi
    mov eax, esi
    mov esi, 10
    add edi, 9
    jmp .body
.loop:
    dec edi
.body:
    cmp eax, 0
    je .end

    xor edx, edx
    div esi
    add edx, '0'

    mov byte [edi], dl
    jmp .loop
.end:
    pop eax
    sub edi, eax
    mov eax, 9
    sub eax, edi
    ret

; stou(buffer: [*]const u8, length: u32) u32
; edi - esi
stou:
    mov ebx, 1
    xor ecx, ecx
    jmp .body
.loop:
    dec esi
.body:
    cmp esi, 0
    je .end

    movzx eax, byte [edi+esi-1]
    sub eax, '0'
    imul eax, ebx
    imul ebx, 10
    add ecx, eax

    jmp .loop
.end:
    mov eax, ecx
    ret
