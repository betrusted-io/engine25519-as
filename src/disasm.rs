
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
    END,
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
            5 => Opcode::ADD(
                Operand::from_i32_r(op >> 6),
                Operand::from_i32_r(op >> 12),
                Operand::from_i32_w(op >> 18),
            ),
            6 => Opcode::SUB,
            7 => Opcode::MUL,
            8 => Opcode::TRD,
            9 => Opcode::BRZ(Operand::from_i32_r(op >> 6), op >> 23),
            10 => Opcode::END,
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
            END => write!(f, "END"),
            _ => write!(f, "invalid"),
        }
    }
}

#[cfg(feature = "std")]
pub fn print_opcode(op: i32) {
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
