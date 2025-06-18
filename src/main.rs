use clap::Parser;
use std::{
    fmt::Display,
    fs::File,
    io::{Read, Write},
    path::PathBuf,
};

#[derive(Parser, Debug)]
struct Args {
    file: PathBuf,
    out: PathBuf,
}

#[derive(Debug)]
enum OpCode {
    MovR,
}

impl OpCode {
    pub fn from_byte(input: u8) -> Result<OpCode, String> {
        let mask: u8 = 0b1111_1100;
        let cleared_input = input & mask;
        match cleared_input {
            0b1000_1000 => Ok(OpCode::MovR),
            _ => Err("Invalid opcode.".to_string()),
        }
    }
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::MovR => write!(f, "mov"),
        }
    }
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
    pub fn from_byte(w_flag: bool, byte: u8) -> Self {
        let masked = 0b0011_1000 & byte;
        use Register::*;
        match (w_flag, masked) {
            (true, 0b0000_0000) => AX,
            (false, 0b0000_0000) => AL,
            (true, 0b0000_1000) => CX,
            (false, 0b0000_1000) => CL,
            (true, 0b0001_0000) => DX,
            (false, 0b0001_0000) => DL,
            (true, 0b0001_1000) => BX,
            (false, 0b0001_1000) => BL,
            (true, 0b0010_0000) => SP,
            (false, 0b0010_0000) => AH,
            (true, 0b0010_1000) => BP,
            (false, 0b0010_1000) => CH,
            (true, 0b0011_0000) => SI,
            (false, 0b0011_0000) => DH,
            (true, 0b0011_1000) => DI,
            (false, 0b0011_1000) => BH,
            _ => unreachable!("all posbile masked values are covered."),
        }
    }

    pub fn from_byte2(w_flag: bool, byte: u8) -> Self {
        let masked = 0b0000_0111 & byte;
        use Register::*;
        match (w_flag, masked) {
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
            _ => unreachable!("all posbile masked values are covered."),
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

#[derive(Debug)]
struct Instruction {
    opcode: OpCode,
    direction: bool,
    word_byte: bool,
    mode: ModMode,
    register: Register,
    register_memory: Register,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}, {}",
            self.opcode, self.register_memory, self.register
        )
    }
}

#[derive(Debug)]
struct InstructionBuilder {
    opcode: Option<OpCode>,
    direction: Option<bool>,
    word_byte: Option<bool>,
    mode: Option<ModMode>,
    register: Option<Register>,
    register_memory: Option<Register>,
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
        }
    }
    pub fn build(self) -> Instruction {
        Instruction {
            opcode: self.opcode.unwrap(),
            direction: self.direction.unwrap(),
            word_byte: self.word_byte.unwrap(),
            mode: self.mode.unwrap(),
            register: self.register.unwrap(),
            register_memory: self.register_memory.unwrap(),
        }
    }
}

enum States {
    Byte1,
    Byte2,
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
        if let None = self.current_instruction {
            self.current_instruction = Some(InstructionBuilder::new());
        }
        match self.current_state {
            Byte1 => self.process_byte1(byte),
            Byte2 => self.process_byte2(byte),
        }
    }
    fn process_byte1(&mut self, byte: u8) {
        let builder = self
            .current_instruction
            .as_mut()
            .expect("fsm always initializes");
        builder.opcode = Some(OpCode::from_byte(byte).unwrap());
        builder.direction = Some(0b0000_0010 & byte == 3);
        builder.word_byte = Some(0b0000_0001 & byte == 1);
        self.current_state = States::Byte2;
    }

    fn process_byte2(&mut self, byte: u8) {
        let mut builder = self.current_instruction.take().unwrap();
        builder.mode = Some(ModMode::from_byte(byte));
        builder.register = Some(Register::from_byte(builder.word_byte.unwrap(), byte));
        // TODO: We're ignoring the different MOD modes here, which will change how this field works.
        builder.register_memory = Some(Register::from_byte2(builder.word_byte.unwrap(), byte));
        let instruction = builder.build();
        self.instruction_buffer.push(instruction);
        self.current_instruction = Some(InstructionBuilder::new());
        self.current_state = States::Byte1;
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
        write!(&mut out_file, "\n{}", inst).unwrap();
    }
}
