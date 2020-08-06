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

// /// Does anything work
// #[test]
// fn basic_syntax() {
//     let mcode = assemble_engine25519!(
//         start:
//             add %0, %1, %2
//             add %2, %3, #4
//             add %5, #6, %7
//             add %8, #9, #10
//             brz start, #11
//     );
//     assert_eq!(
//         mcode,
//         [
//             0 << 16 | 0 << 11 | 1 << 6 | 0 << 17 | 2 << 12 | 0x5,
//             2 << 16 | 0 << 11 | 3 << 6 | 1 << 17 | 4 << 12 | 0x5,
//             5 << 16 | 1 << 11 | 6 << 6 | 0 << 17 | 7 << 12 | 0x5,
//             8 << 16 | 1 << 11 | 9 << 6 | 1 << 17 | 10 << 12 | 0x5,
//             0x3FC << 23 | 0 << 11 | 11 << 6 | 0x9,
//         ]
//     );
// }

pub enum Operand {
    Register(i32),
    Constant(i32),
}

impl Operand {
    pub fn from_i32_r(op: i32) -> Operand {
        if (op >> 5) & 1 == 0 {
            Operand::Register(op & (32 - 1))
        } else {
            Operand::Constant(op & (32 - 1))
        }
    }

    pub fn from_i32_w(op: i32) -> Operand {
        Operand::Register(op & (32 - 1))
    }
}
#[cfg(feature = "std")]
impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Operand::*;
        match self {
            Register(r) => write!(f, "%{}", r),
            Constant(c) => write!(f, "#{}", c),
        }
    }
}

pub enum Opcode {
    PSA(Operand, Operand),
    PSB(Operand, Operand),
    MSK(Operand, Operand),
    XOR,
    NOT,
    ADD(Operand, Operand, Operand),
    SUB,
    MUL,
    TRD,
    BRZ(Operand, i32),
    UDF,
}

impl Opcode {
    pub fn from_i32(op: i32) -> Opcode {
        match op & (32 - 1) {
            0 => Opcode::PSA(Operand::from_i32_r(op >> 6), Operand::from_i32_w(op >> 18)),
            1 => Opcode::PSB(Operand::from_i32_r(op >> 6), Operand::from_i32_w(op >> 18)),
            2 => Opcode::MSK(Operand::from_i32_r(op >> 6), Operand::from_i32_r(op >> 12)),
            3 => Opcode::XOR,
            4 => Opcode::NOT,
            5 => Opcode::ADD(Operand::from_i32_r(op >> 6), Operand::from_i32_r(op >> 12), Operand::from_i32_w(op >> 18)),
            6 => Opcode::SUB,
            7 => Opcode::MUL,
            8 => Opcode::TRD,
            9 => Opcode::BRZ(Operand::from_i32_r(op >> 6), op >> 23),
            _ => Opcode::UDF,
        }
    }
}

#[cfg(feature = "std")]
impl std::fmt::Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Opcode::*;
        match self {
            PSA(ra, rd) => write!(f, "PSA {}, {}", ra, rd),
            PSB(ra, rd) => write!(f, "PSB {}, {}", ra, rd),
            MSK(ra, rb) => write!(f, "MSK {}, {}", ra, rb),
            XOR => write!(f, "XOR"),
            NOT => write!(f, "NOT"),
            ADD(ra, rb, rd) => write!(f, "ADD {}, {}, {}", rd, ra, rb),
            SUB => write!(f, "SUB"),
            MUL => write!(f, "MUL"),
            TRD => write!(f, "TRD"),
            BRZ(ra, rb) => write!(f, "BRZ {}, {}", rb, ra),
            _ => write!(f, "invalid"),
        }
    }
}

#[cfg(test)]
fn print_opcode(op: i32) {
    println!(
        "{:09b} {:05b} {:01b} {:05b} {:01b} {:05b} {:05b} | {}",
        (op >> 23) & (512 - 1),
        (op >> 18) & (32 - 1),
        (op >> 17) & 1,
        (op >> 12) & (32 - 1),
        (op >> 11) & 1,
        (op >> 6) & (32 - 1),
        op & (32 - 1),
        Opcode::from_i32(op),
    );
}

/// Simple jump
#[test]
fn simple_jmp() {
    let mcode = assemble_engine25519!(
            start:
                add %8, #9, #11
            mid:
                brz start, #11
                add %8, #9, #12
                add %8, #9, #10
    );
    println!("Assembled output:");
    for op in &mcode {
        print_opcode(*op);
    }

    let target = [
        8 << 18 | 1 << 11 | 9 << 6 | 1 << 17 | 11 << 12 | 0x5,
        0x3FE << 23 | 0 << 11 | 11 << 6 | 0x9,
        8 << 18 | 1 << 11 | 9 << 6 | 1 << 17 | 12 << 12 | 0x5,
        8 << 18 | 1 << 11 | 9 << 6 | 1 << 17 | 10 << 12 | 0x5,
    ];
    println!("Target output:");
    for op in &target {
        print_opcode(*op);
    }

    assert_eq!(mcode, target);
}

/*
/// Test simple label relocation
#[test]
fn simple_jmp() {
    let mcode = assemble_engine25519!(
        start: brz start, r0
    );
    assert_eq!(mcode, [ 0x4C, 0x00, 0x00 ]);
}

/// Has to work without any relocations (label references)
#[test]
fn no_reloc() {
    let mcode = assemble6502!(
        start:
            lda #0xfb
    );
    assert_eq!(mcode, [ 0xA9, 0xFB ]);
}

/// Has to work without any labels
#[test]
fn no_label() {
    let mcode = assemble6502!(
        lda #0xfb
        lda #0xab
    );
    assert_eq!(mcode, [ 0xA9, 0xFB, 0xA9, 0xAB ]);
}
*/
