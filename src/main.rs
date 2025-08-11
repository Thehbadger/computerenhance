use clap::{Parser, ValueEnum};
use std::{
    fmt::Display,
    fs::File,
    io::{Read, Write},
    path::PathBuf,
};

use crate::simulator::{Register, run_simulation};

mod simulator;

/// Arguments to the program.
#[derive(Parser, Debug)]
struct Args {
    /// Which mode to run.
    #[arg(value_enum)]
    mode: Mode,
    /// ASM input file.
    file: PathBuf,
    /// Output file.
    out: PathBuf,
}

#[derive(Copy, Clone, Debug, ValueEnum)]
enum Mode {
    Decomp,
    Sim,
}

/*~~~~~~~~~~~~~~~~~~~~~~Instruction Structs~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#[derive(Debug)]
enum Instruction {
    MovR(RegisterOp),
    MovIR(MovIrOp),
    AddRegMemory(RegisterOp),
    AddImmediateRegister(ImmediateOp),
    AddImmediateAccumulator(AccumulatorOp),
    SubRegMemory(RegisterOp),
    SubImmediateRegister(ImmediateOp),
    SubImmediateAccumulator(AccumulatorOp),
    CmpRegMemory(RegisterOp),
    CmpImmediateRegister(ImmediateOp),
    CmpImmediateAccumulator(AccumulatorOp),
    Je(JmpOp),
    Jl(JmpOp),
    Jle(JmpOp),
    Jb(JmpOp),
    Jbe(JmpOp),
    Jp(JmpOp),
    Jo(JmpOp),
    Js(JmpOp),
    Jne(JmpOp),
    Jnl(JmpOp),
    Jg(JmpOp),
    Jnb(JmpOp),
    Ja(JmpOp),
    Jnp(JmpOp),
    Jno(JmpOp),
    Jns(JmpOp),
    Loop(JmpOp),
    Loopz(JmpOp),
    Loopnz(JmpOp),
    Jcxz(JmpOp),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Instruction::*;
        match self {
            MovR(mov_r) => write_reg_mem("mov", mov_r, f),
            MovIR(mov_ir) => {
                write!(f, "mov {}, {}", mov_ir.register, mov_ir.data)
            }
            AddRegMemory(register_op) => write_reg_mem("add", register_op, f),
            AddImmediateRegister(immediate_op) => write_immediate_rm("add", immediate_op, f),
            AddImmediateAccumulator(accumulator_op) => {
                write_immediate_accumulator("add", accumulator_op, f)
            }
            SubRegMemory(register_op) => write_reg_mem("sub", &register_op, f),
            SubImmediateRegister(immediate_op) => write_immediate_rm("sub", immediate_op, f),
            SubImmediateAccumulator(accumulator_op) => {
                write_immediate_accumulator("sub", accumulator_op, f)
            }

            CmpRegMemory(register_op) => write_reg_mem("cmp", &register_op, f),
            CmpImmediateRegister(immediate_op) => write_immediate_rm("cmp", immediate_op, f),
            CmpImmediateAccumulator(accumulator_op) => {
                write_immediate_accumulator("cmp", accumulator_op, f)
            }
            Je(jmp_op) => write_jmp("je", jmp_op, f),
            Jl(jmp_op) => write_jmp("jl", jmp_op, f),
            Jle(jmp_op) => write_jmp("jle", jmp_op, f),
            Jb(jmp_op) => write_jmp("jb", jmp_op, f),
            Jbe(jmp_op) => write_jmp("jbe", jmp_op, f),
            Jp(jmp_op) => write_jmp("jp", jmp_op, f),
            Jo(jmp_op) => write_jmp("jo", jmp_op, f),
            Js(jmp_op) => write_jmp("js", jmp_op, f),
            Jne(jmp_op) => write_jmp("jne", jmp_op, f),
            Jnl(jmp_op) => write_jmp("jnl", jmp_op, f),
            Jg(jmp_op) => write_jmp("jg", jmp_op, f),
            Jnb(jmp_op) => write_jmp("jnb", jmp_op, f),
            Ja(jmp_op) => write_jmp("ja", jmp_op, f),
            Jnp(jmp_op) => write_jmp("jnp", jmp_op, f),
            Jno(jmp_op) => write_jmp("jno", jmp_op, f),
            Jns(jmp_op) => write_jmp("jns", jmp_op, f),
            Loop(jmp_op) => write_jmp("loop", jmp_op, f),
            Loopz(jmp_op) => write_jmp("loopz", jmp_op, f),
            Loopnz(jmp_op) => write_jmp("loopnz", jmp_op, f),
            Jcxz(jmp_op) => write_jmp("jcxz", jmp_op, f),
        }
    }
}

fn write_reg_mem(
    name: &str,
    instruction: &RegisterOp,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    if instruction.direction {
        write!(
            f,
            "{name} {}, {}",
            instruction.register, instruction.register_memory
        )?;
        if let Some(disp) = instruction.displacement {
            if disp != 0 {
                write!(f, "+ {disp}]")
            } else {
                write!(f, "]")
            }
        } else {
            write!(f, "]")
        }
    } else {
        write!(f, "{name} {}", instruction.register_memory)?;
        if let Some(disp) = instruction.displacement {
            if disp != 0 {
                write!(f, "+ {disp}],")?;
            } else {
                write!(f, "],")?;
            }
        } else {
            write!(f, ",")?;
        }
        write!(f, " {}", instruction.register)
    }
}

/// These instructions look like `inst [reg_mem + disp], data`.
fn write_immediate_rm(
    name: &str,
    instruction: &ImmediateOp,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    let width = match instruction.width {
        true => "word",
        false => "byte",
    };
    if let Some(disp) = instruction.displacement {
        if let RegisterMemoryEncoding::DIRECT = instruction.registery_memory {
            write!(f, "{name} {width} [{disp}], {}", instruction.data)
        } else {
            write!(
                f,
                "{name} {width} [{} + {}], {} ",
                instruction.registery_memory, disp, instruction.data
            )
        }
    } else {
        if let ModMode::Register = instruction.mode {
            write!(
                f,
                "{name} {}, {}",
                instruction.registery_memory, instruction.data
            )
        } else {
            write!(
                f,
                "{name} {width} {}, {}",
                instruction.registery_memory, instruction.data
            )
        }
    }
}

fn write_immediate_accumulator(
    name: &str,
    instruction: &AccumulatorOp,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    if instruction.width {
        write!(f, "{name} ax, {}", instruction.data)
    } else {
        write!(f, "{name} al, {}", instruction.data)
    }
}

fn write_jmp(name: &str, instruction: &JmpOp, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{name} {}", instruction.inc)
}

#[allow(dead_code)]
#[derive(Debug)]
struct RegisterOp {
    direction: bool,
    word_byte: bool,
    mode: ModMode,
    register: Register,
    register_memory: RegisterMemoryEncoding,
    displacement: Option<u16>,
}

#[allow(dead_code)]
#[derive(Debug)]
struct MovIrOp {
    word_byte: bool,
    register: Register,
    data: u16,
}

#[allow(dead_code)]
#[derive(Debug)]
struct ImmediateOp {
    sign_extension: bool,
    width: bool,
    mode: ModMode,
    registery_memory: RegisterMemoryEncoding,
    displacement: Option<u16>,
    data: u16,
}

#[derive(Debug)]
struct AccumulatorOp {
    width: bool,
    data: u16,
}

#[derive(Debug)]
struct JmpOp {
    inc: i8,
}

#[derive(Debug)]
struct InstructionBuilder {
    opcode: Option<OpCode>,
    direction: Option<bool>,
    width: Option<bool>,
    sign_extension: Option<bool>,
    mode: Option<ModMode>,
    register: Option<Register>,
    register_memory: Option<RegisterMemoryEncoding>,
    displacement: Option<[u8; 2]>,
    data: Option<[u8; 2]>,
}

impl InstructionBuilder {
    pub fn new() -> Self {
        Self {
            opcode: None,
            direction: None,
            width: None,
            sign_extension: None,
            mode: None,
            register: None,
            register_memory: None,
            displacement: None,
            data: None,
        }
    }
    pub fn build(self) -> Instruction {
        use Instruction::*;
        match self.opcode.unwrap() {
            OpCode::MovR => MovR(RegisterOp {
                direction: self.direction.unwrap(),
                word_byte: self.width.unwrap(),
                mode: self.mode.unwrap(),
                register: self.register.unwrap(),
                register_memory: self.register_memory.unwrap(),
                displacement: self.displacement.map(u16::from_be_bytes),
            }),
            OpCode::MovIR => MovIR(MovIrOp {
                word_byte: self.width.unwrap(),
                register: self.register.unwrap(),
                data: self.data.map(u16::from_be_bytes).unwrap(),
            }),
            OpCode::AddRegMemory => AddRegMemory(RegisterOp {
                direction: self.direction.unwrap(),
                word_byte: self.width.unwrap(),
                mode: self.mode.unwrap(),
                register: self.register.unwrap(),
                register_memory: self.register_memory.unwrap(),
                displacement: self.displacement.map(u16::from_be_bytes),
            }),
            OpCode::AddSubCmpImmediateRegister => unreachable!(),
            OpCode::AddImmediateRegister => AddImmediateRegister(ImmediateOp {
                sign_extension: self.sign_extension.unwrap(),
                width: self.width.unwrap(),
                mode: self.mode.unwrap(),
                registery_memory: self.register_memory.unwrap(),
                displacement: self.displacement.map(u16::from_be_bytes),
                data: self.data.map(u16::from_be_bytes).unwrap(),
            }),
            OpCode::AddImmediateAccumulator => AddImmediateAccumulator(AccumulatorOp {
                width: self.width.unwrap(),
                data: self.data.map(u16::from_be_bytes).unwrap(),
            }),
            OpCode::SubRegMemory => SubRegMemory(RegisterOp {
                direction: self.direction.unwrap(),
                word_byte: self.width.unwrap(),
                mode: self.mode.unwrap(),
                register: self.register.unwrap(),
                register_memory: self.register_memory.unwrap(),
                displacement: self.displacement.map(u16::from_be_bytes),
            }),
            OpCode::SubImmediateRegister => SubImmediateRegister(ImmediateOp {
                sign_extension: self.sign_extension.unwrap(),
                width: self.width.unwrap(),
                mode: self.mode.unwrap(),
                registery_memory: self.register_memory.unwrap(),
                displacement: self.displacement.map(u16::from_be_bytes),
                data: self.data.map(u16::from_be_bytes).unwrap(),
            }),
            OpCode::SubImmediateAccumulator => SubImmediateAccumulator(AccumulatorOp {
                width: self.width.unwrap(),
                data: self.data.map(u16::from_be_bytes).unwrap(),
            }),
            OpCode::CmpRegMemory => CmpRegMemory(RegisterOp {
                direction: self.direction.unwrap(),
                word_byte: self.width.unwrap(),
                mode: self.mode.unwrap(),
                register: self.register.unwrap(),
                register_memory: self.register_memory.unwrap(),
                displacement: self.displacement.map(u16::from_be_bytes),
            }),
            OpCode::CmpImmediateRegister => CmpImmediateRegister(ImmediateOp {
                sign_extension: self.sign_extension.unwrap(),
                width: self.width.unwrap(),
                mode: self.mode.unwrap(),
                registery_memory: self.register_memory.unwrap(),
                displacement: self.displacement.map(u16::from_be_bytes),
                data: self.data.map(u16::from_be_bytes).unwrap(),
            }),
            OpCode::CmpImmediateAccumulator => CmpImmediateAccumulator(AccumulatorOp {
                width: self.width.unwrap(),
                data: self.data.map(u16::from_be_bytes).unwrap(),
            }),
            OpCode::Je => Je(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Jl => Jl(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Jle => Jle(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Jb => Jb(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Jbe => Jbe(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Jp => Jp(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Jo => Jo(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Js => Js(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Jne => Jne(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Jnl => Jnl(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Jg => Jg(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Jnb => Jnb(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Ja => Ja(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Jnp => Jnp(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Jno => Jno(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Jns => Jns(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Loop => Loop(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Loopz => Loopz(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Loopnz => Loopnz(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
            OpCode::Jcxz => Jcxz(JmpOp {
                inc: self.data.unwrap()[1].cast_signed(),
            }),
        }
    }
}

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~Decode Data Structures~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#[derive(Debug, Clone, Copy)]
enum OpCode {
    // Movs
    MovR,
    MovIR,
    // Adds
    AddRegMemory,
    /// Add, Sub, Cmp to IR are all the same at the first byte.
    AddSubCmpImmediateRegister,
    AddImmediateRegister,
    AddImmediateAccumulator,
    // Subs
    SubRegMemory,
    SubImmediateRegister,
    SubImmediateAccumulator,
    // Cmps
    CmpRegMemory,
    CmpImmediateRegister,
    CmpImmediateAccumulator,
    // Jmps
    Je,
    Jl,
    Jle,
    Jb,
    Jbe,
    Jp,
    Jo,
    Js,
    Jne,
    Jnl,
    Jg,
    Jnb,
    Ja,
    Jnp,
    Jno,
    Jns,
    Loop,
    Loopz,
    Loopnz,
    Jcxz,
}

#[derive(Debug)]
enum ModMode {
    MemNoDis,
    Mem8Dis,
    Mem16Dis,
    Register,
}

impl ModMode {
    pub fn from_byte(input: u8) -> ModMode {
        let mask: u8 = 0b1100_0000;
        let cleared_input = input & mask;
        match cleared_input {
            0b0000_0000 => Self::MemNoDis,
            0b0100_0000 => Self::Mem8Dis,
            0b1000_0000 => Self::Mem16Dis,
            0b1100_0000 => Self::Register,
            _ => unreachable!("data should always be masked."),
        }
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug)]
enum RegisterMemoryEncoding {
    AL,
    AX,
    CL,
    CX,
    DL,
    DX,
    BL,
    BX,
    AH,
    SP,
    CH,
    BP,
    DH,
    SI,
    BH,
    DI,
    /// (BX) + (SI)
    BXSI,
    /// (BX) + (DI)
    BXDI,
    /// (BP) + (SI)
    BPSI,
    /// (BP) + (DI)
    BPDI,
    /// (SI)
    SI_,
    /// (DI)
    DI_,
    /// Direct Address
    DIRECT,
    /// (BX)
    BX_,
    /// (BX) + (SI) + D8
    BXSID8,
    /// (BX) + (DI) + D8
    BXDID8,
    /// (BP) + (SI) + D8
    BPSID8,
    /// (BP) + (DI) + D8
    BPDID8,
    /// (SI) + D8
    SID8,
    /// (DI) + D8
    DID8,
    /// (BP) + D8
    BPD8,
    /// (BX) + D8
    BXD8,
    /// (BX) + (SI) + D16
    BXSID16,
    /// (BX) + (DI) + D16
    BXDID16,
    /// (BP) + (SI) + D16
    BPSID16,
    /// (BP) + (DI) + D16
    BPDID16,
    /// (SI) + D16
    SID16,
    /// (DI) + D16
    DID16,
    /// (BP) + D16
    BPD16,
    /// (BX) + D16
    BXD16,
}

impl RegisterMemoryEncoding {
    fn from_byte(mode: ModMode, width: bool, input: u8) -> Self {
        use RegisterMemoryEncoding::*;
        match mode {
            ModMode::MemNoDis => match input {
                0b0000_0000 => BXSI,
                0b0000_0001 => BXDI,
                0b0000_0010 => BPSI,
                0b0000_0011 => BPDI,
                0b0000_0100 => SI_,
                0b0000_0101 => DI_,
                0b0000_0110 => DIRECT,
                0b0000_0111 => BX_,
                _ => panic!("Invalid Input"),
            },
            ModMode::Mem8Dis => match input {
                0b0000_0000 => BXSID8,
                0b0000_0001 => BXDID8,
                0b0000_0010 => BPSID8,
                0b0000_0011 => BPDID8,
                0b0000_0100 => SID8,
                0b0000_0101 => DID8,
                0b0000_0110 => BPD8,
                0b0000_0111 => BXD8,
                _ => panic!("Invalid Input"),
            },
            ModMode::Mem16Dis => match input {
                0b0000_0000 => BXSID16,
                0b0000_0001 => BXDID16,
                0b0000_0010 => BPSID16,
                0b0000_0011 => BPDID16,
                0b0000_0100 => SID16,
                0b0000_0101 => DID16,
                0b0000_0110 => BPD16,
                0b0000_0111 => BXD16,
                _ => panic!("Invalid Input"),
            },
            ModMode::Register => match (width, input) {
                (true, 0b0000_0000) => AX,
                (false, 0b0000_0000) => AL,
                (true, 0b0000_0001) => CX,
                (false, 0b0000_0001) => CL,
                (true, 0b0000_0010) => DX,
                (false, 0b0000_0010) => DL,
                (true, 0b0000_0011) => BX,
                (false, 0b0000_0011) => BL,
                (true, 0b0000_0100) => SP,
                (false, 0b0000_0100) => AH,
                (true, 0b0000_0101) => BP,
                (false, 0b0000_0101) => CH,
                (true, 0b0000_0110) => SI,
                (false, 0b0000_0110) => DH,
                (true, 0b0000_0111) => DI,
                (false, 0b0000_0111) => BH,
                (_, _) => panic!("Invalid input"),
            },
        }
    }
}

impl Display for RegisterMemoryEncoding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RegisterMemoryEncoding::*;
        write!(
            f,
            "{}",
            match self {
                AL => "al",
                AX => "ax",
                CL => "cl",
                CX => "cx",
                DL => "dl",
                DX => "dx",
                BL => "bl",
                BX => "bx",
                AH => "ah",
                SP => "sp",
                CH => "ch",
                BP => "bp",
                DH => "dh",
                SI => "si",
                BH => "bh",
                DI => "di",
                BXSI => "[bx + si]",
                BXDI => "[bx + di]",
                BPSI => "[bp + si]",
                BPDI => "[bp + di]",
                SI_ => "[si]",
                DI_ => "[di]",
                DIRECT => unimplemented!(),
                BX_ => "[bx]",
                BXSID8 => "[bx + si",
                BXDID8 => "[bx + di",
                BPSID8 => "[bp + si",
                BPDID8 => "[bp + di",
                SID8 => "[si",
                DID8 => "[di",
                BPD8 => "[bp",
                BXD8 => "[bx",
                BXSID16 => "[bx + si",
                BXDID16 => "[bx + di",
                BPSID16 => "[bp + si",
                BPDID16 => "[bp + di",
                SID16 => "[si",
                DID16 => "[di",
                BPD16 => "[bp",
                BXD16 => "[bx",
            }
        )
    }
}

/*~~~~~~~~~~~~~~~~~~~~~~~ Process Common Instructions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

fn process_add_sub_cmp_immediate_register_b1(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    builder.sign_extension = Some(((0b0000_0010 & byte) >> 1) == 1);
    builder.width = Some(0b0000_0001 & byte == 1);
    machine.current_state = States::Byte2;
}

fn process_add_sub_cmp_immediate_register_b2(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    builder.mode = Some(ModMode::from_byte(byte));
    builder.opcode = Some(match 0b0011_1000 & byte {
        0b0000_0000 => OpCode::AddImmediateRegister,
        0b0010_1000 => OpCode::SubImmediateRegister,
        0b0011_1000 => OpCode::CmpImmediateRegister,
        _ => unimplemented!("Unimplemented immediate command."),
    });
    match builder.mode.as_ref().unwrap() {
        ModMode::MemNoDis => {
            let masked_byte = 0b0000_0111 & byte;
            builder.register_memory = Some(RegisterMemoryEncoding::from_byte(
                ModMode::MemNoDis,
                builder.width.unwrap(),
                masked_byte,
            ));
            if let Some(RegisterMemoryEncoding::DIRECT) = builder.register_memory {
                machine.current_state = States::Byte3;
            } else {
                // Jump directly to read data in byte 5.
                machine.current_state = States::Byte5;
            }
        }
        ModMode::Mem8Dis => {
            let masked_byte = 0b0000_0111 & byte;
            builder.register_memory = Some(RegisterMemoryEncoding::from_byte(
                ModMode::Mem8Dis,
                builder.width.unwrap(),
                masked_byte,
            ));
            machine.current_state = States::Byte3;
        }
        ModMode::Mem16Dis => {
            let masked_byte = 0b0000_0111 & byte;
            builder.register_memory = Some(RegisterMemoryEncoding::from_byte(
                ModMode::Mem16Dis,
                builder.width.unwrap(),
                masked_byte,
            ));
            machine.current_state = States::Byte3;
        }
        ModMode::Register => {
            let masked_byte = 0b0000_0111 & byte;
            builder.register_memory = Some(RegisterMemoryEncoding::from_byte(
                ModMode::Register,
                builder.width.unwrap(),
                masked_byte,
            ));
            // Jump directly to read data in byte 5.
            machine.current_state = States::Byte5;
        }
    }
}

fn process_add_sub_cmp_immediate_register_b3(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    match builder.mode.as_ref().unwrap() {
        ModMode::MemNoDis => {
            if let Some(RegisterMemoryEncoding::DIRECT) = builder.register_memory {
                builder.displacement = Some([0, byte]);
                machine.current_state = States::Byte4;
            } else {
                panic!("Not direct address, should not have byte 3.")
            }
        }
        ModMode::Register => panic!("Invalid state"),
        ModMode::Mem8Dis => {
            builder.displacement = Some([0, byte]);
            machine.current_state = States::Byte5;
        }
        ModMode::Mem16Dis => {
            builder.displacement = Some([0, byte]);
            machine.current_state = States::Byte4;
        }
    }
}

fn process_add_sub_cmp_immediate_register_b4(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    match builder.mode.as_ref().unwrap() {
        ModMode::MemNoDis => {
            if let Some(RegisterMemoryEncoding::DIRECT) = builder.register_memory {
                let disp = builder.displacement.as_mut().unwrap();
                disp[0] = byte;
                machine.current_state = States::Byte5;
            } else {
                panic!();
            }
        }
        ModMode::Mem8Dis | ModMode::Register => unreachable!("Invalid"),
        ModMode::Mem16Dis => {
            let disp = builder.displacement.as_mut().unwrap();
            disp[0] = byte;
            machine.current_state = States::Byte5;
        }
    }
}

fn process_add_sub_cmp_immediate_register_b5(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    builder.data = Some([0, byte]);
    if !builder.sign_extension.unwrap() & builder.width.unwrap() {
        machine.current_state = States::Byte6;
    } else {
        let final_builder = machine
            .current_instruction
            .replace(InstructionBuilder::new())
            .unwrap();
        let instruction = final_builder.build();
        machine.instruction_buffer.push(instruction);
        machine.current_state = States::Byte1;
    }
}

fn process_add_sub_cmp_immediate_register_b6(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    let data = builder.data.as_mut().unwrap();
    data[0] = byte;
    let final_builder = machine
        .current_instruction
        .replace(InstructionBuilder::new())
        .unwrap();
    let instruction = final_builder.build();
    machine.instruction_buffer.push(instruction);
    machine.current_state = States::Byte1;
}

/// Standard way to process byte two for instructions with `mode reg r/m` as byte two.
fn process_standard_b2(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    builder.mode = Some(ModMode::from_byte(byte));
    let masked_register = (0b0011_1000 & byte) >> 3;
    builder.register = Some(Register::from_byte(builder.width.unwrap(), masked_register));
    match builder.mode.as_ref().unwrap() {
        ModMode::MemNoDis => {
            let masked_byte = 0b0000_0111 & byte;
            builder.register_memory = Some(RegisterMemoryEncoding::from_byte(
                ModMode::MemNoDis,
                builder.width.unwrap(),
                masked_byte,
            ));
            if let Some(RegisterMemoryEncoding::DIRECT) = builder.register_memory {
                machine.current_state = States::Byte3;
            } else {
                let final_builder = machine
                    .current_instruction
                    .replace(InstructionBuilder::new())
                    .unwrap();
                let instruction = final_builder.build();
                machine.instruction_buffer.push(instruction);
                machine.current_state = States::Byte1;
            }
        }
        ModMode::Mem8Dis => {
            let masked_byte = 0b0000_0111 & byte;
            builder.register_memory = Some(RegisterMemoryEncoding::from_byte(
                ModMode::Mem8Dis,
                builder.width.unwrap(),
                masked_byte,
            ));
            machine.current_state = States::Byte3;
        }
        ModMode::Mem16Dis => {
            let masked_byte = 0b0000_0111 & byte;
            builder.register_memory = Some(RegisterMemoryEncoding::from_byte(
                ModMode::Mem16Dis,
                builder.width.unwrap(),
                masked_byte,
            ));
            machine.current_state = States::Byte3;
        }
        ModMode::Register => {
            let masked_byte = 0b0000_0111 & byte;
            builder.register_memory = Some(RegisterMemoryEncoding::from_byte(
                ModMode::Register,
                builder.width.unwrap(),
                masked_byte,
            ));
            let final_builder = machine
                .current_instruction
                .replace(InstructionBuilder::new())
                .unwrap();
            let instruction = final_builder.build();
            machine.instruction_buffer.push(instruction);
            machine.current_state = States::Byte1;
        }
    }
}

/// Standard way to process byte 3 which is normally `(DISP-LO)`
fn process_standard_b3(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    match builder.mode.as_ref().unwrap() {
        ModMode::MemNoDis => {
            if let Some(RegisterMemoryEncoding::DIRECT) = builder.register_memory {
                builder.displacement = Some([0, byte]);
                machine.current_state = States::Byte4;
            } else {
                panic!("Not direct address, should not have byte 3.")
            }
        }
        ModMode::Register => panic!("Invalid state"),
        ModMode::Mem8Dis => {
            builder.displacement = Some([0, byte]);
            let final_builder = machine
                .current_instruction
                .replace(InstructionBuilder::new())
                .unwrap();
            let instruction = final_builder.build();
            machine.instruction_buffer.push(instruction);
            machine.current_state = States::Byte1;
        }
        ModMode::Mem16Dis => {
            builder.displacement = Some([0, byte]);
            machine.current_state = States::Byte4;
        }
    }
}

/// Standard way to process byte 4 which is normally `(DISP-HIGH)`
fn process_standard_b4(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    match builder.mode.as_ref().unwrap() {
        ModMode::MemNoDis => {
            if let Some(RegisterMemoryEncoding::DIRECT) = builder.register_memory {
                let disp = builder.displacement.as_mut().unwrap();
                disp[0] = byte;
                let final_builder = machine
                    .current_instruction
                    .replace(InstructionBuilder::new())
                    .unwrap();
                let instruction = final_builder.build();
                machine.instruction_buffer.push(instruction);
                machine.current_state = States::Byte1;
            } else {
                panic!("Not direct address, should not have byte 4.")
            }
        }
        ModMode::Mem8Dis | ModMode::Register => unreachable!("Invalid"),
        ModMode::Mem16Dis => {
            let disp = builder.displacement.as_mut().unwrap();
            disp[0] = byte;
            let final_builder = machine
                .current_instruction
                .replace(InstructionBuilder::new())
                .unwrap();
            let instruction = final_builder.build();
            machine.instruction_buffer.push(instruction);
            machine.current_state = States::Byte1;
        }
    }
}

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~Process Mov Specific Instructions~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

fn process_mov_r_b1(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    builder.direction = Some(0b0000_0010 & byte == 2);
    builder.width = Some(0b0000_0001 & byte == 1);
    machine.current_state = States::Byte2;
}

fn process_mov_ir_b1(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    let word_byte = ((0b0000_1000 & byte) >> 3) == 1;
    let masked_byte = 0b0000_0111 & byte;
    builder.register = Some(Register::from_byte(word_byte, masked_byte));
    builder.width = Some(word_byte);
    machine.current_state = States::Byte2;
}

/// Also is used for other immediate to register instructions
fn process_mov_ir_b2(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    builder.data = Some([0, byte]);
    if builder.width.unwrap() {
        machine.current_state = States::Byte3;
    } else {
        let final_builder = machine
            .current_instruction
            .replace(InstructionBuilder::new())
            .unwrap();
        let instruction = final_builder.build();
        machine.instruction_buffer.push(instruction);
        machine.current_state = States::Byte1;
    }
}

/// Also is used for other immediate to register instructions
fn process_mov_ir_b3(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    let data = builder.data.as_mut().unwrap();
    data[0] = byte;
    let final_builder = machine
        .current_instruction
        .replace(InstructionBuilder::new())
        .unwrap();
    let instruction = final_builder.build();
    machine.instruction_buffer.push(instruction);
    machine.current_state = States::Byte1;
}

/*~~~~~~~~~~~~~~~~~~~~~Process Add Specific Instructions~~~~~~~~~~~~~~~~~~~~*/

/// Also is used by Sub and CMP
fn process_add_reg_mem_b1(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    let word_byte = (0b0000_0001 & byte) == 1;
    let direction_byte = ((0b0000_0010 & byte) >> 1) == 1;
    builder.width = Some(word_byte);
    builder.direction = Some(direction_byte);
    machine.current_state = States::Byte2;
}

fn process_add_sub_cmp_immediate_accumulator_b1(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    let word_byte = (0b0000_0001 & byte) == 1;
    builder.width = Some(word_byte);
    machine.current_state = States::Byte2;
}

fn process_jmp_b1(machine: &mut StateMachine, _byte: u8) {
    machine.current_state = States::Byte2;
}

fn process_jmp_b2(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    // We reuse the first byte of the data field to store the signed increment.
    builder.data = Some([0, byte]);
    let final_builder = machine
        .current_instruction
        .replace(InstructionBuilder::new())
        .unwrap();
    let instruction = final_builder.build();
    machine.instruction_buffer.push(instruction);
    machine.current_state = States::Byte1;
}

/*~~~~~~~~~~~~~~~~~~~~State Machine~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

enum States {
    Byte1,
    Byte2,
    Byte3,
    Byte4,
    Byte5,
    Byte6,
}

struct StateMachine {
    current_state: States,
    current_instruction: Option<InstructionBuilder>,
    instruction_buffer: Vec<Instruction>,
}

impl StateMachine {
    pub fn new() -> Self {
        Self {
            current_state: States::Byte1,
            current_instruction: None,
            instruction_buffer: Vec::new(),
        }
    }
    pub fn process_input(&mut self, byte: u8) {
        use States::*;
        if self.current_instruction.is_none() {
            self.current_instruction = Some(InstructionBuilder::new());
        }

        // TODO: Some debug prints
        // for inst in self.instruction_buffer.iter() {
        //     println!("{inst:?}");
        //     println!("{inst}");
        // }
        // println!("----------------");

        match self.current_state {
            Byte1 => self.process_byte1(byte),
            Byte2 => self.process_byte2(byte),
            Byte3 => self.process_byte3(byte),
            Byte4 => self.process_byte4(byte),
            Byte5 => self.process_byte5(byte),
            Byte6 => self.process_byte6(byte),
        }
    }
    fn process_byte1(&mut self, byte: u8) {
        let builder = self
            .current_instruction
            .as_mut()
            .expect("fsm always initializes");
        use OpCode::*;
        let opcode = match byte {
            0b1000_1000..=0b1000_1111 => MovR,
            0b1011_0000..=0b1011_1111 => MovIR,
            0b0000_0000..=0b0000_0011 => AddRegMemory,
            0b1000_0000..=0b1000_0011 => AddSubCmpImmediateRegister,
            0b0000_0100..=0b0000_0101 => AddImmediateAccumulator,
            0b0010_1000..=0b0010_1011 => SubRegMemory,
            0b0010_1100..=0b0010_1101 => SubImmediateAccumulator,
            0b0011_1000..=0b0011_1011 => CmpRegMemory,
            0b0011_1100..=0b0011_1101 => CmpImmediateAccumulator,
            0b0111_0100 => Je,
            0b0111_1100 => Jl,
            0b0111_1110 => Jle,
            0b0111_0010 => Jb,
            0b0111_0110 => Jbe,
            0b0111_1010 => Jp,
            0b0111_0000 => Jo,
            0b0111_1000 => Js,
            0b0111_0101 => Jne,
            0b0111_1101 => Jnl,
            0b0111_1111 => Jg,
            0b0111_0011 => Jnb,
            0b0111_0111 => Ja,
            0b0111_1011 => Jnp,
            0b0111_0001 => Jno,
            0b0111_1001 => Jns,
            0b1110_0010 => Loop,
            0b1110_0001 => Loopz,
            0b1110_0000 => Loopnz,
            0b1110_0011 => Jcxz,
            x => unimplemented!("This opcode is not implemented: {:#010b}", x),
        };
        builder.opcode = Some(opcode);
        match opcode {
            MovR => process_mov_r_b1(self, byte),
            MovIR => process_mov_ir_b1(self, byte),
            AddRegMemory | SubRegMemory | CmpRegMemory => process_add_reg_mem_b1(self, byte),
            AddSubCmpImmediateRegister => process_add_sub_cmp_immediate_register_b1(self, byte),
            AddImmediateAccumulator | SubImmediateAccumulator | CmpImmediateAccumulator => {
                process_add_sub_cmp_immediate_accumulator_b1(self, byte)
            }
            Je => process_jmp_b1(self, byte),
            Jl => process_jmp_b1(self, byte),
            Jle => process_jmp_b1(self, byte),
            Jb => process_jmp_b1(self, byte),
            Jbe => process_jmp_b1(self, byte),
            Jp => process_jmp_b1(self, byte),
            Jo => process_jmp_b1(self, byte),
            Js => process_jmp_b1(self, byte),
            Jne => process_jmp_b1(self, byte),
            Jnl => process_jmp_b1(self, byte),
            Jg => process_jmp_b1(self, byte),
            Jnb => process_jmp_b1(self, byte),
            Ja => process_jmp_b1(self, byte),
            Jnp => process_jmp_b1(self, byte),
            Jno => process_jmp_b1(self, byte),
            Jns => process_jmp_b1(self, byte),
            Loop => process_jmp_b1(self, byte),
            Loopz => process_jmp_b1(self, byte),
            Loopnz => process_jmp_b1(self, byte),
            Jcxz => process_jmp_b1(self, byte),
            // None of these are known at byte 1.
            AddImmediateRegister | SubImmediateRegister | CmpImmediateRegister => {
                unreachable!("This opcode cannot known in byte 1.")
            }
        }
    }

    fn process_byte2(&mut self, byte: u8) {
        let builder = self.current_instruction.as_ref().unwrap();
        use OpCode::*;
        match builder.opcode.unwrap() {
            MovR => process_standard_b2(self, byte),
            MovIR => process_mov_ir_b2(self, byte),
            AddRegMemory | SubRegMemory | CmpRegMemory => process_standard_b2(self, byte),
            AddSubCmpImmediateRegister => process_add_sub_cmp_immediate_register_b2(self, byte),
            AddImmediateAccumulator | SubImmediateAccumulator | CmpImmediateAccumulator => {
                process_mov_ir_b2(self, byte)
            }
            Je => process_jmp_b2(self, byte),
            Jl => process_jmp_b2(self, byte),
            Jle => process_jmp_b2(self, byte),
            Jb => process_jmp_b2(self, byte),
            Jbe => process_jmp_b2(self, byte),
            Jp => process_jmp_b2(self, byte),
            Jo => process_jmp_b2(self, byte),
            Js => process_jmp_b2(self, byte),
            Jne => process_jmp_b2(self, byte),
            Jnl => process_jmp_b2(self, byte),
            Jg => process_jmp_b2(self, byte),
            Jnb => process_jmp_b2(self, byte),
            Ja => process_jmp_b2(self, byte),
            Jnp => process_jmp_b2(self, byte),
            Jno => process_jmp_b2(self, byte),
            Jns => process_jmp_b2(self, byte),
            Loop => process_jmp_b2(self, byte),
            Loopz => process_jmp_b2(self, byte),
            Loopnz => process_jmp_b2(self, byte),
            Jcxz => process_jmp_b2(self, byte),
            // None of these are known yet.
            AddImmediateRegister | SubImmediateRegister | CmpImmediateRegister => {
                unreachable!("This opcode cannot known in byte 2 yet.")
            }
        }
    }

    fn process_byte3(&mut self, byte: u8) {
        let builder = self.current_instruction.as_ref().unwrap();
        use OpCode::*;
        match builder.opcode.unwrap() {
            MovR => process_standard_b3(self, byte),
            MovIR => process_mov_ir_b3(self, byte),
            AddRegMemory | SubRegMemory | CmpRegMemory => process_standard_b3(self, byte),
            AddImmediateRegister | SubImmediateRegister | CmpImmediateRegister => {
                process_add_sub_cmp_immediate_register_b3(self, byte)
            }
            AddImmediateAccumulator | SubImmediateAccumulator | CmpImmediateAccumulator => {
                process_mov_ir_b3(self, byte)
            }
            x => unimplemented!("{x:?} is unimplemented on byte 3"),
        }
    }

    fn process_byte4(&mut self, byte: u8) {
        let builder = self.current_instruction.as_ref().unwrap();
        use OpCode::*;
        match builder.opcode.unwrap() {
            MovR => process_standard_b4(self, byte),
            AddRegMemory | SubRegMemory | CmpRegMemory => process_standard_b4(self, byte),
            AddImmediateRegister | SubImmediateRegister | CmpImmediateRegister => {
                process_add_sub_cmp_immediate_register_b4(self, byte)
            }
            x => unimplemented!("{x:?} is unimplemented on byte 4"),
        }
    }

    fn process_byte5(&mut self, byte: u8) {
        let builder = self.current_instruction.as_ref().unwrap();
        use OpCode::*;
        match builder.opcode.unwrap() {
            AddImmediateRegister | SubImmediateRegister | CmpImmediateRegister => {
                process_add_sub_cmp_immediate_register_b5(self, byte)
            }
            x => unimplemented!("{x:?} is unimplemented on byte 5"),
        }
    }

    fn process_byte6(&mut self, byte: u8) {
        let builder = self.current_instruction.as_ref().unwrap();
        use OpCode::*;
        match builder.opcode.unwrap() {
            AddImmediateRegister | SubImmediateRegister | CmpImmediateRegister => {
                process_add_sub_cmp_immediate_register_b6(self, byte)
            }
            x => unimplemented!("{x:?} is unimplemented on byte 6"),
        }
    }

    pub fn finalize(self) -> Vec<Instruction> {
        self.instruction_buffer
    }
}

fn main() {
    let args = Args::parse();
    let file_path = args.file;
    let mut file = File::open(file_path).unwrap();

    let mut read_buffer = Vec::new();

    let _bytes_read = file.read_to_end(&mut read_buffer).unwrap();

    let mut fsm = StateMachine::new();
    for byte in read_buffer {
        fsm.process_input(byte);
    }
    let instructions = fsm.finalize();

    let mut out_file = std::fs::File::create(args.out).unwrap();
    match args.mode {
        Mode::Decomp => {
            out_file.write_all(b"bits 16\n").unwrap();

            for inst in instructions {
                println!("{inst:?}");
                write!(&mut out_file, "\n{inst}").unwrap();
            }
        }
        Mode::Sim => {
            let (regs, flags) = run_simulation(instructions);
            write!(&mut out_file, "{regs}\n\n").unwrap();
            write!(&mut out_file, "{flags}").unwrap()
        }
    }
}
