struc strDef [params] {
    common
        . db params
        .len = $ - .
}

macro irCopy dst, src {
    db IR_COPY
    db dst
    db src
}

; MPutS(msg: []const u8) i32
macro MPutS msg {
    mov eax, SYS_WRITE
    mov ebx, 1
    mov ecx, msg
    mov edx, msg#.len
    int 80h
}

; MSysOpen(path: [*:0]const u8, flags: i32, mode: u32) i32
macro MSysOpen path, flags, mode {
    mov eax, SYS_OPEN
    mov ebx, path
    mov ecx, flags
    mov edx, mode
    int 80h
}

; MSysClose(fd: i32) i32
macro MSysClose fd {
    mov eax, SYS_CLOSE
    mov ebx, fd
    int 80h
}

; MSysWrite(fd: i32, buf: [*]const u8, count: u32) i32
macro MSysWrite fd, buf, count {
    mov eax, SYS_WRITE
    mov ebx, fd
    mov ecx, buf
    mov edx, count
    int 80h
}

; MSysRead(fd: i32, buf: [*]u8, count: u32) i32
macro MSysRead fd, buf, count {
    mov eax, SYS_READ
    mov ebx, fd
    mov ecx, buf
    mov edx, count
    int 80h
}
