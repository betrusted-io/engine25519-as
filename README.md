# The Engine Assembler

Provides an assembler-as-macro that lets you assemble The Engine bytecode
in your program. For example, to create an infinite loop, run:

```rust
    let mcode = engine25519_as::assemble_engine25519!(
        start:
            brz start, #0
    );
```

## Disassembly

You can use a basic disassembler provided by this crate. To disassemble a single opcode, run:

```rust
    let opcode = engine25519_as::disasm::Opcode::from_i32(0x14_7985);
    println!("Opcode: {}", opcode);
```

## Debugging the Macro

To debug the macro, use the `nightly` toolchain and enable the `macro-debug` feature.

An easy way to do this is:

```sh
$ cargo +nightly test --features macro-debug
...
```
