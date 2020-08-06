#![cfg_attr(feature = "macro-debug", feature(trace_macros))]
#![cfg_attr(feature = "macro-debug", feature(log_syntax))]
#[cfg(feature = "macro-debug")]
trace_macros! {true}

pub mod disasm;

/*
   opcodes

   PSA  Wd, Ra
   PSB  Wd, Rb
   MSK  Wd, Ra, Rb
   XOR  Wd, Ra, Rb
   NOT  Wd, Ra
   ADD  Wd, Ra, Rb
   SUB  Wd, Ra, Rb
   MUL  Wd, Ra, Rb
   TRD  Wd, Ra, Rb
   BRZ  offset, Ra

   all Rx can be expressed as #Rx which summons the constant at #Rx

   examples of syntax
   ADD  r0, r1, r2   // adds r1 and r2, stores into r0
   ADD  r31, #4, #8  // takes constants in table positions 4 and 8, adds them, stores into r31
   PSA  r1, #0       // takes constant in table position 0, stores in r1
   BRZ  -0x3, r4     // if r4 is 0, mpc = mpc - 0x3
   BRZ  0x2, r1      // if r1 is 0, mpc = mpc + 0x2
 loop:
   BRZ  loop, r2     // if r2 is 0, go to label "loop"


    Basic opcode format:
        0b_rrrrrrrrr_ddddd_y_bbbbb_x_aaaaa_oooooo
    Where:
        o: Opcode
        a: Source register a
        x: `1` if A is a constant rather than register
        b: Source register b
        y: `1` if B is a constant rather than a register
        d: Destination register index
        r: Reserved (set to 0)

*/

/// A compile-time map from identifiers to arbitrary (heterogeneous) expressions
#[macro_export]
#[doc(hidden)]
macro_rules! ident_map {
    ( $name:ident = { $($key:ident => $e:expr),* $(,)* } ) => {
        macro_rules! $name {
            $(
                ( $key ) => { $e };
            )*
            // Empty invocation expands to nothing. Needed when the map is empty.
            () => {};
        }
    };
}

/// Returns the number of comma-separated expressions passed to it
#[macro_export]
#[doc(hidden)]
macro_rules! codelen {
    () => { 0 };
    ( $one:expr ) => { 1 };
    ( $first:expr, $($rest:expr),+ ) => { 1 + codelen!($($rest),+) };
}

/// Replace elements of "arrays" of expressions with sorted replacements by
/// seeking along a "positional array" containing n expressions, then replacing
/// elements in the source array.
///
/// Expands to the first array with replacements applied. The length doesn't
/// change.
#[macro_export]
#[doc(hidden)]
macro_rules! lockstep_replace {
    ( [ $($result:expr),* ], [], ) => {
        // `src` is empty, no relocations. We're done!
        [ $($result),* ]
    };
    ( [ $($result:expr),* ], [ $($src:expr,)+ ], ) => {
        // Empty list of replacements, but still `src` to go
        [ $($result,)* $($src),+ ]
    };
    ( [ $($result:expr),* ], [ $($src:expr,)* ], [], [], $( [ $($pos:expr,)* ], [ $($rep:expr,)* ], )* ) => {
        // All replacements applied. Pop the current replacement and continue.
        lockstep_replace!(
            [ $($result),* ],
            [ $($src,)* ],
            $(
                [ $($pos,)* ],
                [ $($rep,)* ],
            )*
        )
    };
    ( [ $($result:expr),* ], [ $src1_replaced:expr, $($src:expr,)* ], [], [ $rep1:expr, $($rep_rest:expr,)* ], $( [ $pos1:expr, $($pos:expr,)* ], [ $($rep:expr,)* ], )* ) => {
        // Position of a replacement reached (or: inside a replacement)
        // Coupled with a seek step
        lockstep_replace!(
            [ $($result,)* $rep1 | $src1_replaced ],
            [ $($src,)* ],
            [],
            [ $($rep_rest,)* ],
            $(
                [ $($pos,)* ],
                [ $($rep,)* ],
            )*
        )
    };
    ( [ $($result:expr),* ], [ $src1:expr, $($src:expr,)* ], $( [ $pos1:expr, $($pos:expr,)* ], [ $($rep:expr,)* ], )+ ) => {
        // Seek to next replacement position (simultaneously for all
        // replacements)
        lockstep_replace!(
            [ $($result,)* $src1 ],
            [ $($src,)* ],
            $(
                [ $($pos,)* ],
                [ $($rep,)* ],
            )+
        )
    };
}

/// Performs relocation of machine code based on given labels and relocations.
/// Looks up label names in an `ident_map`. Expands to the relocated machine
/// code.
///
/// Relocation formats:
/// { $label as ABS16 @ [$lockstepmcpos] }

#[macro_export]
#[doc(hidden)]
macro_rules! reloc {
    ( { $($attr:tt)* }
      [ $( [ $($pos:expr),* ],
           [ $($rep:expr),* ] ),* ],
      $lblmap:ident,
      [ $($mcode:expr),* ],
      [/* empty relocation list */] ) => {
        lockstep_replace!([], [ $($mcode,)* ], $( [ $($pos,)* ], [ $($rep,)* ], )*)
    };
    ( { start: $start:expr }  [ $( [ $($pos:expr),* ], [ $($rep:expr),* ] ),* ], $lblmap:ident, [ $($mcode:expr),* ], [ { $lbl:ident as ABS16 @ [$($lockstepmcpos:expr),*] } $(,$reloc:tt)* ] ) => {
        // Replace 2 Bytes with the absolute address
        // Relocation position is given as "lock-step MC pos", an expression
        // list that's as long as all mcode before the relocation should happen.
        reloc!(
            { start: $start }
            [ $( [ $($pos),* ], [ $($rep),* ] ,)*
            [ $($lockstepmcpos),* ], [ ($lblmap!($lbl) + $start) as u8, (($lblmap!($lbl) + $start) >> 8) as u8 ] ],
            $lblmap, [ $($mcode),* ], [ $($reloc),* ])
    };
    ( { $($attr:tt)* }  [ $( [ $($pos:expr),* ], [ $($rep:expr),* ] ),* ], $lblmap:ident, [ $($mcode:expr),* ], [ { $lbl:ident as PCREL @ [$($lockstepmcpos:expr,)*] } $(,$reloc:tt)* ] ) => {
        // OR in the PC-relative address to the opcode
        reloc!(
            { $($attr)* }
            [ $( [ $($pos),* ], [ $($rep),* ] ,)*
            [ $($lockstepmcpos),* ], [ ((( $lblmap!($lbl) as i32 - (codelen!($($lockstepmcpos),*)) as i32 - 1 ) & 0x3FF ) << 23) ] ],
            $lblmap,
            [ $($mcode),* ], [ $($reloc),* ])
    };
}

// $attr is a label
// $mcode is a list that contains the machine code opcodes
// $ident is the identifier list
// $reloc is the relocation list

#[macro_export]
#[doc(hidden)]
macro_rules! asm_ {
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        // EOF
    ) => {{
        ident_map!(labelmap = {
            $($lbl => $lblval),*
        });
        reloc!({ $($attr)* } [], labelmap, [ $($mcode),* ], [ $($reloc),* ])
    }};

    // ==================================================================================
    // ==================================================================================
    // ==================================================================================

    // Opcode assembly table.
    // Note that the weird order is required because macros try to match each arm in order, but
    // don't backtrack when a NT is parsed

    // ADD
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        add % $wd:tt, % $ra:tt , % $rb:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* $wd << 18 | 0 << 11 | $ra << 6 | 0 << 17 | $rb << 12 | 0x5 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        add % $wd:tt, % $ra:tt , # $rb:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* $wd << 18 | 0 << 11 | $ra << 6 | 1 << 17 | $rb << 12 | 0x5 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        add % $wd:tt, # $ra:tt , % $rb:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* $wd << 18 | 1 << 11 | $ra << 6 | 0 << 17 | $rb << 12 | 0x5 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        add % $wd:tt, # $ra:tt , # $rb:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* $wd << 18 | 1 << 11 | $ra << 6 | 1 << 17 | $rb << 12 | 0x5 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // BRZ
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        brz $label:ident , %$ra:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0 << 23 | 0 << 11 | $ra << 6 | 0x9 ],
            [ $($lbl => $lblval),* ], [ $($reloc,)* { $label as PCREL @ [$($mcode,)*] } ], $($rest)*)
    };

    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        brz $label:ident , #$ra:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0 << 23 | 1 << 11 | $ra << 6 | 0x9 ],
            [ $($lbl => $lblval),* ], [ $($reloc,)* { $label as PCREL @ [$($mcode,)*] } ], $($rest)*)
    };

    // UDF
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
    udf
$($rest:tt)* ) => {
    asm_!({ $($attr)* } [ $($mcode,)* -0x7FFF_FFFF ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
};

    // ==================================================================================
    // ==================================================================================
    // ==================================================================================

    // Check for labels
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        $label:ident :
    $($rest:tt)* ) => {
        asm_!(
            { $($attr)* }
            [ $($mcode),* ],
            [ $($lbl => $lblval,)* $label => codelen!($($mcode),*) ],
            [ $($reloc),* ],
            $($rest)*
        )
    };
}

#[macro_export]
macro_rules! assemble_engine25519 {
    ( {
        start: $start:expr,
        code: {
            $($tokens:tt)*
        }
    } ) => {
        asm_!({ start: $start } [], [], [], $($tokens)*)
    };
    ( $($tokens:tt)* ) => {
        assemble_engine25519!({
            start: 0,
            code: {
                $($tokens)*
            }
        })
    };
}

#[cfg(test)]
fn assert_assembly(gen: &[i32], reference: &[i32]) {
    if gen != reference {
        println!("Generated output:");
        for op in gen {
            disasm::print_opcode(*op);
        }
        println!("Reference output:");
        for op in reference {
            disasm::print_opcode(*op);
        }
        panic!("Outputs are not equal!");
    }
}

/// Does anything work
#[test]
fn basic_syntax() {
    let mcode = assemble_engine25519!(
        start:
            add %0, %1, %2
            add %2, %3, #4
            add %5, #6, %7
            add %8, #9, #10
            brz start, #11
    );
    #[allow(clippy::eq_op)]
    #[allow(clippy::identity_op)]
    assert_assembly(
        &mcode,
        &[
            0 << 18 | 0 << 11 | 1 << 6 | 0 << 17 | 2 << 12 | 0x5,
            2 << 18 | 0 << 11 | 3 << 6 | 1 << 17 | 4 << 12 | 0x5,
            5 << 18 | 1 << 11 | 6 << 6 | 0 << 17 | 7 << 12 | 0x5,
            8 << 18 | 1 << 11 | 9 << 6 | 1 << 17 | 10 << 12 | 0x5,
            0x3FB << 23 | 1 << 11 | 11 << 6 | 0x9,
        ],
    );
}

/// Test simple label relocation
#[test]
fn simple_jmp() {
    let mcode = assemble_engine25519!(
        start:
            brz start, #0
    );
    assert_assembly(&mcode,&[-1 << 23 | 1 << 11 | 0x9]);
}

/// More complex label relocation
#[test]
fn complex_jmp() {
    let mcode = assemble_engine25519!(
        start:
            brz start, #0
            brz mid, #0
        mid:
            brz start, #0
            brz mid, #0
            brz end, #0
        end:
            brz start, #0
    );

    #[allow(clippy::identity_op)]
    assert_assembly(&mcode,&[
        -1 << 23 | 1 << 11 | 0x9,
        0 << 23 | 1 << 11 | 0x9,
        -3 << 23 | 1 << 11 | 0x9,
        -2 << 23 | 1 << 11 | 0x9,
        0 << 23 | 1 << 11 | 0x9,
        -6 << 23 | 1 << 11 | 0x9,
    ]);
}
