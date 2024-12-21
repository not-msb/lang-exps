format elf executable

; Resulting Asm

  ; Builtin Runtime
  ; @write: volatile (i32, [*]u8, usize) -> isize
  ; @read: volatile (i32, [*]mut u8, usize) -> isize
  ; @memcpy: ([*]u8, [*]u8, usize) -> void
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

@memcpy:
    mov edx, dword [esp+12]
    cmp edx, 0
    je .end
    lea edx, [edx-1]
    mov eax, [esp+8]
    mov ebx, [esp+4]
.loop:
    mov cl, byte [eax+edx]
    mov byte [ebx+edx], cl
    sub edx, 1
    jnb .loop
.end:
    ret

    ; Compiled
main:
    ; Call @debug
    sub esp, 1
    mov byte [esp], 65
    call @debug
    add esp, 1
    ; End Call @debug
    mov al, 0
    mov byte [esp+4], al
    ret

;main:
;    sub esp, 12
;    mov dword [esp], 100
;    call itoa
;    add esp, 4
;
;    mov eax, 4
;    mov ebx, 1
;    mov ecx, [esp]
;    mov edx, [esp+4]
;    int 80h
;    add esp, 8
;    ret
;
;ALLOC = 4
;itoa:
;.L0:
;    sub esp, ALLOC
;    cmp dword [esp+ALLOC+4], 0
;    mov dword [esp+ALLOC-4], 0 ; Allocced Size = 4
;    jne .L2
;.L1:
;    mov eax, [S0]
;    mov dword [esp+ALLOC+8], eax
;    mov eax, [S0+4]
;    mov dword [esp+ALLOC+8+4], eax
;    add esp, ALLOC
;    ret
;.L2:
;    mov eax, dword [esp+ALLOC+4]
;    cmp eax, 0
;    je .L4
;.L3:
;    xor edx, edx
;    mov ecx, 10
;    div ecx
;    mov dword [esp+ALLOC+4], eax
;
;    add dl, '0'
;    lea eax, [C0+31]
;    sub eax, dword [esp+ALLOC-4]
;    mov byte [eax], dl
;    add dword [esp+ALLOC-4], 1
;    jmp .L2
;.L4:
;    mov ecx, dword [esp+ALLOC-4]
;    lea eax, [C0+32]
;    sub eax, ecx
;    mov dword [esp+ALLOC+8], eax
;    mov dword [esp+ALLOC+8+4], ecx
;    add esp, ALLOC
;    ret

;;;;;
;main:
;.L0:
;    sub esp, 1
;    mov byte [esp], 5
;.L1:
;    mov al, byte [esp]
;    cmp al, 0
;    jne .L2
;    jmp .L3
;.L2:
;    mov al, byte [esp]
;    mov bl, al
;    push eax
;    call @debug
;    pop eax
;    sub al, 1
;    mov byte [esp], al
;    jmp .L1
;.L3:
;    add esp, 1
;    ret
;
;;;;;

@debug:
    mov al, byte [esp+4]
    mov byte [debug_buffer], al
    add byte [buffer], '0'

    mov eax, 4
    mov ebx, 1
    mov ecx, debug_buffer
    mov edx, 1
    int 80h
    ret

segment readable writable
debug_buffer:
dd 0

S0:
dd C1
dd 1

C0:
rb 32

C1:
db "0"
