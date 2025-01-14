;
; hello-world.asm
;
; Prints "Hello, world!" to the stdout.
;
message:
    .wstr "Hello, world!\n"

    adr r1, message
    adr r4, loop
    mov r3, 1
loop:
    ldr r2, [r0, r1]
    adr r6, next
    adr r7, end
    mov r7, r6, r2
    jmp [r0, r7]
next:
    out r2
    add r1, r3
    jmp [r0, r4]

end:
    halt
