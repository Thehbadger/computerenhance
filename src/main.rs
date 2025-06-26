use clap::Parser;
use std::{
    fmt::Display,
    fs::File,
    io::{Read, Write},
    path::PathBuf,
};

/// Arguments to the program.
#[derive(Parser, Debug)]
struct Args {
    /// ASM input file.
    file: PathBuf,
    /// Output file.
    out: PathBuf,
}

/*~~~~~~~~~~~~~~~~~~~~~~Instruction Structs~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#[derive(Debug)]
enum Instruction {
    MovR(MovR),
    MovIR(MovIR),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::MovR(mov_r) => {
                if mov_r.direction {
                    write!(f, "mov {}, {}", mov_r.register, mov_r.register_memory)?;
                    if let Some(disp) = mov_r.displacement {
                        if disp != 0 {
                            write!(f, "+ {disp}]")
                        } else {
                            write!(f, "]")
                        }
                    } else {
                        write!(f, "]")
                    }
                } else {
                    write!(f, "mov {}", mov_r.register_memory)?;
                    if let Some(disp) = mov_r.displacement {
                        if disp != 0 {
                            write!(f, "+ {disp}],")?;
                        } else {
                            write!(f, "],")?;
                        }
                    } else {
                        write!(f, ",")?;
                    }
                    write!(f, " {}", mov_r.register)
                }
            }
            Instruction::MovIR(mov_ir) => {
                write!(f, "mov {}, {}", mov_ir.register, mov_ir.data)
            }
        }
    }
}

#[allow(dead_code)]
#[derive(Debug)]
struct MovR {
    direction: bool,
    word_byte: bool,
    mode: ModMode,
    register: Register,
    register_memory: RegisterMemoryEncoding,
    displacement: Option<u16>,
}

#[allow(dead_code)]
#[derive(Debug)]
struct MovIR {
    word_byte: bool,
    register: Register,
    data: u16,
}

#[derive(Debug)]
struct InstructionBuilder {
    opcode: Option<OpCode>,
    direction: Option<bool>,
    word_byte: Option<bool>,
    mode: Option<ModMode>,
    register: Option<Register>,
    register_memory: Option<RegisterMemoryEncoding>,
    displacement: Option<[u8; 2]>,
}

impl InstructionBuilder {
    pub fn new() -> Self {
        Self {
            opcode: None,
            direction: None,
            word_byte: None,
            mode: None,
            register: None,
            register_memory: None,
            displacement: None,
        }
    }
    pub fn build(self) -> Instruction {
        match self.opcode.unwrap() {
            OpCode::MovR => Instruction::MovR(MovR {
                direction: self.direction.unwrap(),
                word_byte: self.word_byte.unwrap(),
                mode: self.mode.unwrap(),
                register: self.register.unwrap(),
                register_memory: self.register_memory.unwrap(),
                displacement: self.displacement.map(u16::from_be_bytes),
            }),
            OpCode::MovIR => Instruction::MovIR(MovIR {
                word_byte: self.word_byte.unwrap(),
                register: self.register.unwrap(),
                data: self.displacement.map(u16::from_be_bytes).unwrap(),
            }),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum OpCode {
    MovR,
    MovIR,
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

#[derive(Debug)]
enum Register {
    AL,
    CL,
    DL,
    BL,
    AH,
    CH,
    DH,
    BH,
    AX,
    CX,
    DX,
    BX,
    SP,
    BP,
    SI,
    DI,
}

impl Register {
    pub fn from_byte(w_flag: bool, masked_byte: u8) -> Self {
        use Register::*;
        match (w_flag, masked_byte) {
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
            (x, y) => unreachable!("all posbile masked values are covered: {x}, {y:#010b}"),
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let val = match self {
            Register::AL => "al",
            Register::CL => "cl",
            Register::DL => "dl",
            Register::BL => "bl",
            Register::AH => "ah",
            Register::CH => "ch",
            Register::DH => "dh",
            Register::BH => "bh",
            Register::AX => "ax",
            Register::CX => "cx",
            Register::DX => "dx",
            Register::BX => "bx",
            Register::SP => "sp",
            Register::BP => "bp",
            Register::SI => "si",
            Register::DI => "di",
        };
        write!(f, "{val}")
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug)]
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

/*~~~~~~~~~~~~~~~~~~~~~~~ Process Mov Instructions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

fn process_mov_r_b1(builder: &mut InstructionBuilder, byte: u8) {
    builder.direction = Some(0b0000_0010 & byte == 2);
    builder.word_byte = Some(0b0000_0001 & byte == 1);
}

fn process_mov_r_b2(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    builder.mode = Some(ModMode::from_byte(byte));
    let masked_register = (0b0011_1000 & byte) >> 3;
    builder.register = Some(Register::from_byte(
        builder.word_byte.unwrap(),
        masked_register,
    ));
    match builder.mode.as_ref().unwrap() {
        ModMode::MemNoDis => {
            // TODO: Doesn't handle 110 Direct Address
            let masked_byte = 0b0000_0111 & byte;
            builder.register_memory = Some(RegisterMemoryEncoding::from_byte(
                ModMode::MemNoDis,
                builder.word_byte.unwrap(),
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
        ModMode::Mem8Dis => {
            let masked_byte = 0b0000_0111 & byte;
            builder.register_memory = Some(RegisterMemoryEncoding::from_byte(
                ModMode::Mem8Dis,
                builder.word_byte.unwrap(),
                masked_byte,
            ));
            machine.current_state = States::Byte3;
        }
        ModMode::Mem16Dis => {
            let masked_byte = 0b0000_0111 & byte;
            builder.register_memory = Some(RegisterMemoryEncoding::from_byte(
                ModMode::Mem16Dis,
                builder.word_byte.unwrap(),
                masked_byte,
            ));
            machine.current_state = States::Byte3;
        }
        ModMode::Register => {
            let masked_byte = 0b0000_0111 & byte;
            builder.register_memory = Some(RegisterMemoryEncoding::from_byte(
                ModMode::Register,
                builder.word_byte.unwrap(),
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

fn process_mov_r_b3(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    match builder.mode.as_ref().unwrap() {
        ModMode::MemNoDis => unimplemented!("Not handling direct address."),
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

fn process_mov_r_b4(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    match builder.mode.as_ref().unwrap() {
        ModMode::MemNoDis | ModMode::Mem8Dis | ModMode::Register => unreachable!("Invalid"),
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

fn process_mov_ir_b1(builder: &mut InstructionBuilder, byte: u8) {
    let word_byte = ((0b0000_1000 & byte) >> 3) == 1;
    let masked_byte = 0b0000_0111 & byte;
    builder.register = Some(Register::from_byte(word_byte, masked_byte));
    builder.word_byte = Some(word_byte);
}

fn process_mov_ir_b2(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
    builder.displacement = Some([0, byte]);
    if builder.word_byte.unwrap() {
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

fn process_mov_ir_b3(machine: &mut StateMachine, byte: u8) {
    let builder = machine.current_instruction.as_mut().unwrap();
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

/*~~~~~~~~~~~~~~~~~~~~State Machine~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

enum States {
    Byte1,
    Byte2,
    Byte3,
    Byte4,
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
        match self.current_state {
            Byte1 => self.process_byte1(byte),
            Byte2 => self.process_byte2(byte),
            Byte3 => self.process_byte3(byte),
            Byte4 => self.process_byte4(byte),
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
            x => unimplemented!("This opcode is not implemented: {:#010b}", x),
        };
        builder.opcode = Some(opcode);
        match opcode {
            MovR => process_mov_r_b1(builder, byte),
            MovIR => process_mov_ir_b1(builder, byte),
        }
        self.current_state = States::Byte2;
    }

    fn process_byte2(&mut self, byte: u8) {
        let builder = self.current_instruction.as_ref().unwrap();
        match builder.opcode.unwrap() {
            OpCode::MovR => process_mov_r_b2(self, byte),
            OpCode::MovIR => process_mov_ir_b2(self, byte),
        }
    }

    fn process_byte3(&mut self, byte: u8) {
        let builder = self.current_instruction.as_ref().unwrap();
        match builder.opcode.unwrap() {
            OpCode::MovR => process_mov_r_b3(self, byte),
            OpCode::MovIR => process_mov_ir_b3(self, byte),
        }
    }

    fn process_byte4(&mut self, byte: u8) {
        let builder = self.current_instruction.as_ref().unwrap();
        match builder.opcode.unwrap() {
            OpCode::MovR => process_mov_r_b4(self, byte),
            OpCode::MovIR => unreachable!("Not possible"),
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
    out_file.write_all(b"bits 16\n").unwrap();

    for inst in instructions {
        println!("{inst:?}");
        write!(&mut out_file, "\n{inst}").unwrap();
    }
}
