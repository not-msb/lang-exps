AST_SIZE = 9

ast_cursor:
.pos dd 0 ; u32
.data dd 0 ; [*]const Token

; isToken(kind: u8) bool
macro isToken kind {
    cmp byte [ast_cursor.data+TOKEN_SIZE*ast_cursor.pos], kind
    xor eax, eax
    sete al
    add dword [ast_cursor.pos], eax
}

; parse(output: [*]Ast, input: [*]const Token, length: u32) u32
parse:
    isToken TOKEN_INTEGER
    jne .end
    push edx
    push ecx
    push ebx
    push eax

    MSysWrite 1, input.name, input.name.len

    pop eax
    pop ebx
    pop ecx
    pop edx
.end:
    ret

; expr() ?Ast
expr:
    ret
