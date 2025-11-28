# UM-32 "Universal Machine"

An implementation of the UM-32 "Universal Machine" as described by the [Cult of the Bound Variable](http://boundvariable.org).

## Usage

Run the benchmark:
```sh
; cargo run --release -- files/sandmark.umz
```

## Assembler

The project includes a builtin assembler for UM-32 programs. Enable the `asm` feature to use it:

```sh
; cargo build --release --features asm
```

This builds the `uasm` binary which can assemble `.asm` or `.uasm` files into UM-32 binaries:

```sh
; cargo run --release --features asm --bin uasm -- files/hello-world.asm -o hello.um
; cargo run --release -- hello.um
Hello, world!
```

### Example Assembly Program

Here's a simple "Hello, world!" program (`files/hello-world.asm`):

```asm
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
```
