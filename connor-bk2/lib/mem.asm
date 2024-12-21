; memory functions use convention edi - esi - ecx

; memcmp(dst: [*]const u8, src: [*]const u8, length: u32) u8
; jz for result
memcmp:
    cld
    cmp ecx, ecx
    repe cmpsb
    ret
