;; This file is meant to be used on 32bit linux
;; The actual compiler is desgned to be usable on bare-metal
format elf executable

SYS_EXIT equ 1
SYS_READ equ 3
SYS_WRITE equ 4
SYS_OPEN equ 5
SYS_CLOSE equ 6

O_RDONLY equ 0

MAX_FILE_LEN equ 32 * 1024
MAX_TOKEN_COUNT equ 1024
MAX_ASM_COUNT equ 1024
MAX_BIN_COUNT equ 1024

include "lib/macros.asm"

segment readable executable
entry _start
_start:
    push ebp
    mov ebp, esp

    MSysOpen input.name, O_RDONLY, 0

    cmp eax, -1
    je errorHandle.fileOpen
    mov [input.desc], eax

    MSysRead [input.desc], input.buffer, MAX_FILE_LEN

    cmp eax, -1
    je errorHandle.fileRead

    mov ebx, token_buffer
    mov ecx, input.buffer
    mov edx, eax
    call tokenize

    mov ebx, token_buffer
.tokenPrintLoop:
    cmp eax, 0
    je .tokenPrintLoopEnd
    push eax
    push ebx

    call printToken

    pop ebx
    pop eax
    dec eax
    add ebx, TOKEN_SIZE
    jmp .tokenPrintLoop
.tokenPrintLoopEnd:

    MSysClose [input.desc]

    cmp eax, -1
    je errorHandle.fileClose

    jmp exit

putN:
    mov eax, SYS_WRITE
    mov ebx, 1
    mov ecx, newline
    mov edx, 1
    int 80h
    ret

; printToken(token: *const Token) void
printToken:
    push edi
    push esi
    movzx eax, byte [ebx]
    mov eax, [.table+4*eax]
    jmp eax
.table:
    dd .identifier, .integer, .plus, .minus, .star, .slash, .assign
.unknown:
    mov ecx, tokenMsg.unknown
    mov edx, tokenMsg.unknown.len
    jmp .print
.identifier:
    push ebx
    MPutS tokenMsg.identifier
    pop ebx

    mov eax, SYS_WRITE
    mov ecx, dword [ebx+1]
    mov edx, dword [ebx+5]
    mov ebx, 1
    int 80h

    call putN
    jmp .end
.integer:
    push ebx
    MPutS tokenMsg.integer
    pop ebx

    mov edi, int_buffer
    mov esi, dword [ebx+1]
    call utos

    mov edx, eax
    lea ecx, [int_buffer+10]
    sub ecx, edx
    mov eax, SYS_WRITE
    mov ebx, 1
    int 80h

    call putN
    jmp .end
.plus:
    mov ecx, tokenMsg.plus
    mov edx, tokenMsg.plus.len
    jmp .print
.minus:
    mov ecx, tokenMsg.minus
    mov edx, tokenMsg.minus.len
    jmp .print
.star:
    mov ecx, tokenMsg.star
    mov edx, tokenMsg.star.len
    jmp .print
.slash:
    mov ecx, tokenMsg.slash
    mov edx, tokenMsg.slash.len
    jmp .print
.assign:
    mov ecx, tokenMsg.assign
    mov edx, tokenMsg.assign.len
    jmp .print
.print:
    mov eax, SYS_WRITE
    mov ebx, 1
    int 80h
.end:
    pop esi
    pop edi
    ret

errorHandle:
.fileRead:
    mov ecx, errorMsg.fileRead
    mov edx, errorMsg.fileRead.len
    jmp .print

.fileOpen:
    mov ecx, errorMsg.fileOpen
    mov edx, errorMsg.fileOpen.len
    jmp .print

.fileClose:
    mov ecx, errorMsg.fileClose
    mov edx, errorMsg.fileClose.len
    jmp .print

.token:
    mov ecx, errorMsg.token
    mov edx, errorMsg.token.len
    jmp .print

.print:
    mov eax, SYS_WRITE
    mov ebx, 1
    int 80h
    jmp exit

exit:
    pop ebp
    mov eax, SYS_EXIT
    mov ebx, 0
    int 80h

include "lib/token.asm"
include "lib/compiler.asm"
include "lib/string.asm"
include "lib/mem.asm"

segment readable writable
input:
.name strDef "input.con", 0
.desc dd 0
newline db 10
errorMsg:
.fileRead strDef "[error] Couldn't read file", 10
.fileOpen strDef "[error] Couldn't open file", 10
.fileClose strDef "[error] Couldn't close file", 10
.token strDef "[error] Couldn't tokenize", 10
tokenMsg:
.unknown strDef "[token] Unknown", 10
.identifier strDef "[token] Identifier:", 10
.integer strDef "[token] Integer:", 10
.plus strDef "[token] Plus", 10
.minus strDef "[token] Minus", 10
.star strDef "[token] Star", 10
.slash strDef "[token] Slash", 10
.assign strDef "[token] Assign", 10

tokenWord:
.assign strDef "<-"

int_buffer rb 10
input.buffer rb MAX_FILE_LEN
token_buffer rb TOKEN_SIZE * MAX_TOKEN_COUNT
bin_buffer rb MAX_BIN_COUNT
