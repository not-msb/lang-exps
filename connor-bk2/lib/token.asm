TOKEN_SIZE = 9
TOKEN_IDENTIFIER = 0
TOKEN_INTEGER = 1
TOKEN_PLUS = 2
TOKEN_MINUS = 3
TOKEN_STAR = 4
TOKEN_SLASH = 5
TOKEN_ASSIGN = 6

; tokenize(output: [*]Token, input: [*]const u8, length: u32) u32
tokenize:
    push ebp
    mov ebp, esp
    push edi
    push esi
    xor eax, eax
    jmp .loop

.loopRet:
    inc eax
    add ebx, TOKEN_SIZE
    inc ecx
    dec edx

.loop:
    cmp edx, 0
    je .end

    .whitespaceCheck:
        push eax
        mov al, byte[ecx]
        call isWhitespace
        cmp al, 0
        pop eax
        je .whitespaceCheckEnd
        inc ecx
        dec edx
    .whitespaceCheckEnd:

    ;.assignCheck:
    ;    push ecx
    ;    mov edi, tokenWord.assign
    ;    mov esi, ecx
    ;    mov ecx, tokenWord.assign.len
    ;    call memcmp
    ;    pop ecx
    ;    jz .assign
    ;.assignCheckEnd:

    cmp byte [ecx], 43
    je .plus
    cmp byte [ecx], 45
    je .minus
    cmp byte [ecx], 42
    je .star
    cmp byte [ecx], 47
    je .slash

    .alphaCheck:
        push edx
        push ecx
        push ebx
        push eax

        mov ebx, isAlpha
        call takeWhile

        cmp al, 0
        pop eax
        pop ebx
        jne .identifier
        pop ecx
        pop edx
    .alphaCheckEnd:

    .digitCheck:
        push edx
        push ecx
        push ebx
        push eax

        mov ebx, isDigit
        call takeWhile

        cmp al, 0
        pop eax
        pop ebx
        jne .integer
        pop ecx
        pop edx
    .digitCheckEnd:

    jmp errorHandle.token

.identifier:
    mov byte [ebx], TOKEN_IDENTIFIER
    pop dword [ebx+1]
    pop dword [ebx+5]
    sub dword [ebx+5], edx
    jmp .loopRet

.integer:
    mov byte [ebx], TOKEN_INTEGER

    pop edi
    pop esi
    sub esi, edx
    push eax
    push ebx
    push ecx

    call stou

    pop ecx
    pop ebx
    mov dword [ebx+1], eax
    pop eax

    jmp .loopRet

.plus:
    mov byte [ebx], TOKEN_PLUS
    jmp .loopRet

.minus:
    mov byte [ebx], TOKEN_MINUS
    jmp .loopRet

.star:
    mov byte [ebx], TOKEN_STAR
    jmp .loopRet

.slash:
    mov byte [ebx], TOKEN_SLASH
    jmp .loopRet

.assign:
    mov byte [ebx], TOKEN_ASSIGN
    add ecx, tokenWord.assign.len-1
    sub edx, tokenWord.assign.len-1

    jmp .loopRet

.end:
    pop esi
    pop edi
    pop ebp
    ret

; isWhitespace(c: u8) bool
; al
isWhitespace:
    cmp al, 32
    je .end

    sub al, 9
    cmp al, 13-9
    setna al
    ret

.end:
    mov al, 1
    ret

; isAlpha(c: u8) bool
; al
isAlpha:
    or al, 0x20
    sub al, 'a'
    cmp al, 'z'-'a'
    setna al
    ret

; isAlpha(c: u8) bool
; al
isDigit:
    sub al, '0'
    cmp al, '9'-'0'
    setna al
    ret

; takeWhile(f: fn(u8) bool, input: [*]const u8, length: u32) u32
takeWhile:
    push ebp
    mov ebp, esp
    push edx
    jmp .body
.loop:
    inc ecx
    dec edx
.body:
    cmp edx, 0
    je .end

    mov al, byte [ecx]
    call ebx
    cmp al, 0
    jne .loop
.end:
    pop eax
    sub eax, edx
    pop ebp
    ret
