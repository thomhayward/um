;
; cat.asm
;
; Read from stdin and echo to stdout.
;
main:
        ; set r2 to 0xffffffff
        nand r2

        ; setup branches
        adr  r6, output
        adr  r5, loop

loop:
        ; read stdin, r1 will contain 0xffffffff if we've reached EOF.
        in   r1

        ; set r3 to 0 if r2 == r1
        nand r3, r2, r1

        ; setup branch
        adr  r4, end
        ; overwrite r4 with $output iff r3 == 0.
        mov  r4, r6, r3
        jmp  [r0, r4]

output:
        ; write to stdout
        out  r1
        jmp  [r0, r5]

end:
        halt
