use std::fmt::Display;

use crate::{Instruction, Mov_IR, RegisterOp};

#[derive(Clone, Copy, Debug)]
pub enum Register {
    U8Register(U8Register),
    U16Register(U16Register),
}

#[derive(Clone, Copy, Debug)]
pub enum U16Register {
    AX,
    BX,
    CX,
    DX,
    SP,
    BP,
    SI,
    DI,
}

#[derive(Clone, Copy, Debug)]
pub enum U8Register {
    AL,
    BL,
    CL,
    DL,
    AH,
    BH,
    CH,
    DH,
}

impl Register {
    pub fn from_byte(w_flag: bool, masked_byte: u8) -> Self {
        use U8Register::*;
        use U16Register::*;
        match (w_flag, masked_byte) {
            (true, 0b0000_0000) => Register::U16Register(AX),
            (false, 0b0000_0000) => Register::U8Register(AL),
            (true, 0b0000_0001) => Register::U16Register(CX),
            (false, 0b0000_0001) => Register::U8Register(CL),
            (true, 0b0000_0010) => Register::U16Register(DX),
            (false, 0b0000_0010) => Register::U8Register(DL),
            (true, 0b0000_0011) => Register::U16Register(BX),
            (false, 0b0000_0011) => Register::U8Register(BL),
            (true, 0b0000_0100) => Register::U16Register(SP),
            (false, 0b0000_0100) => Register::U8Register(AH),
            (true, 0b0000_0101) => Register::U16Register(BP),
            (false, 0b0000_0101) => Register::U8Register(CH),
            (true, 0b0000_0110) => Register::U16Register(SI),
            (false, 0b0000_0110) => Register::U8Register(DH),
            (true, 0b0000_0111) => Register::U16Register(DI),
            (false, 0b0000_0111) => Register::U8Register(BH),
            (x, y) => unreachable!("all posbile masked values are covered: {x}, {y:#010b}"),
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use U8Register::*;
        use U16Register::*;
        let val = match self {
            Register::U8Register(AL) => "al",
            Register::U8Register(CL) => "cl",
            Register::U8Register(DL) => "dl",
            Register::U8Register(BL) => "bl",
            Register::U8Register(AH) => "ah",
            Register::U8Register(CH) => "ch",
            Register::U8Register(DH) => "dh",
            Register::U8Register(BH) => "bh",
            Register::U16Register(AX) => "ax",
            Register::U16Register(CX) => "cx",
            Register::U16Register(DX) => "dx",
            Register::U16Register(BX) => "bx",
            Register::U16Register(SP) => "sp",
            Register::U16Register(BP) => "bp",
            Register::U16Register(SI) => "si",
            Register::U16Register(DI) => "di",
        };
        write!(f, "{val}")
    }
}

pub struct SimulationRegisters {
    ax: [u8; 2],
    bx: [u8; 2],
    cx: [u8; 2],
    dx: [u8; 2],
    sp: [u8; 2],
    bp: [u8; 2],
    si: [u8; 2],
    di: [u8; 2],
}

impl Display for SimulationRegisters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ax: {:#06x} ({})\n",
            u16::from_le_bytes(self.ax),
            u16::from_le_bytes(self.ax)
        )?;
        write!(
            f,
            "bx: {:#06x} ({})\n",
            u16::from_le_bytes(self.bx),
            u16::from_le_bytes(self.bx)
        )?;
        write!(
            f,
            "cx: {:#06x} ({})\n",
            u16::from_le_bytes(self.cx),
            u16::from_le_bytes(self.cx)
        )?;
        write!(
            f,
            "dx: {:#06x} ({})\n",
            u16::from_le_bytes(self.dx),
            u16::from_le_bytes(self.dx)
        )?;
        write!(
            f,
            "sp: {:#06x} ({})\n",
            u16::from_le_bytes(self.sp),
            u16::from_le_bytes(self.sp)
        )?;
        write!(
            f,
            "bp: {:#06x} ({})\n",
            u16::from_le_bytes(self.bp),
            u16::from_le_bytes(self.bp)
        )?;
        write!(
            f,
            "si: {:#06x} ({})\n",
            u16::from_le_bytes(self.si),
            u16::from_le_bytes(self.si)
        )?;
        write!(
            f,
            "di: {:#06x} ({})\n",
            u16::from_le_bytes(self.di),
            u16::from_le_bytes(self.di)
        )
    }
}

impl SimulationRegisters {
    pub fn new() -> Self {
        SimulationRegisters {
            ax: [0; 2],
            bx: [0; 2],
            cx: [0; 2],
            dx: [0; 2],
            sp: [0; 2],
            bp: [0; 2],
            si: [0; 2],
            di: [0; 2],
        }
    }

    pub fn set_u16(&mut self, reg: U16Register, val: u16) {
        match reg {
            U16Register::AX => self.ax = val.to_le_bytes(),
            U16Register::BX => self.bx = val.to_le_bytes(),
            U16Register::CX => self.cx = val.to_le_bytes(),
            U16Register::DX => self.dx = val.to_le_bytes(),
            U16Register::SP => self.sp = val.to_le_bytes(),
            U16Register::BP => self.bp = val.to_le_bytes(),
            U16Register::SI => self.si = val.to_le_bytes(),
            U16Register::DI => self.di = val.to_le_bytes(),
        }
    }

    pub fn set_u8(&mut self, reg: U8Register, val: u8) {
        match reg {
            U8Register::AL => self.ax[0] = val,
            U8Register::BL => self.bx[0] = val,
            U8Register::CL => self.cx[0] = val,
            U8Register::DL => self.dx[0] = val,
            U8Register::AH => self.ax[1] = val,
            U8Register::BH => self.bx[1] = val,
            U8Register::CH => self.cx[1] = val,
            U8Register::DH => self.dx[1] = val,
        }
    }

    pub fn get_u16(&self, reg: U16Register) -> u16 {
        match reg {
            U16Register::AX => u16::from_le_bytes(self.ax),
            U16Register::BX => u16::from_le_bytes(self.bx),
            U16Register::CX => u16::from_le_bytes(self.cx),
            U16Register::DX => u16::from_le_bytes(self.dx),
            U16Register::SP => u16::from_le_bytes(self.sp),
            U16Register::BP => u16::from_le_bytes(self.bp),
            U16Register::SI => u16::from_le_bytes(self.si),
            U16Register::DI => u16::from_le_bytes(self.di),
        }
    }

    pub fn get_u8(&self, reg: U8Register) -> u8 {
        match reg {
            U8Register::AL => self.ax[0],
            U8Register::BL => self.bx[0],
            U8Register::CL => self.cx[0],
            U8Register::DL => self.dx[0],
            U8Register::AH => self.ax[1],
            U8Register::BH => self.bx[1],
            U8Register::CH => self.cx[1],
            U8Register::DH => self.dx[1],
        }
    }
}

pub fn run_simulation(instructions: Vec<Instruction>) -> SimulationRegisters {
    let mut registers = SimulationRegisters::new();
    use Instruction::*;
    for inst in instructions {
        match inst {
            MovR(registerOp) => sim_mov_r(&mut registers, &registerOp),
            MovIR(mov_ir) => sim_mov_ir(&mut registers, &mov_ir),
            _ => unimplemented!(),
        }
    }
    registers
}

/*~~~~~~~~~~~~~~~~~~~~~~Instruction Simulations~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

fn sim_mov_ir(registers: &mut SimulationRegisters, inst: &Mov_IR) {
    match inst.register {
        Register::U8Register(reg) => registers.set_u8(reg, inst.data.try_into().unwrap()),
        Register::U16Register(reg) => registers.set_u16(reg, inst.data),
    }
}

fn sim_mov_r(registers: &mut SimulationRegisters, inst: &RegisterOp) {
    use U8Register::*;
    use U16Register::*;
    let source = inst.register;
    let dest = match inst.register_memory {
        crate::RegisterMemoryEncoding::AL => Register::U8Register(AL),
        crate::RegisterMemoryEncoding::AX => Register::U16Register(AX),
        crate::RegisterMemoryEncoding::CL => Register::U8Register(CL),
        crate::RegisterMemoryEncoding::CX => Register::U16Register(CX),
        crate::RegisterMemoryEncoding::DL => Register::U8Register(DL),
        crate::RegisterMemoryEncoding::DX => Register::U16Register(DX),
        crate::RegisterMemoryEncoding::BL => Register::U8Register(BL),
        crate::RegisterMemoryEncoding::BX => Register::U16Register(BX),
        crate::RegisterMemoryEncoding::AH => Register::U8Register(AH),
        crate::RegisterMemoryEncoding::SP => Register::U16Register(SP),
        crate::RegisterMemoryEncoding::CH => Register::U8Register(CH),
        crate::RegisterMemoryEncoding::BP => Register::U16Register(BP),
        crate::RegisterMemoryEncoding::DH => Register::U8Register(DH),
        crate::RegisterMemoryEncoding::SI => Register::U16Register(SI),
        crate::RegisterMemoryEncoding::BH => Register::U8Register(BH),
        crate::RegisterMemoryEncoding::DI => Register::U16Register(DI),
        _ => unreachable!(),
    };

    if !inst.direction {
        use Register::*;
        match source {
            U8Register(src_reg) => {
                let source_value = registers.get_u8(src_reg);
                match dest {
                    U8Register(dest_reg) => registers.set_u8(dest_reg, source_value),
                    U16Register(dest_reg) => registers.set_u16(dest_reg, source_value.into()),
                }
            }
            Register::U16Register(src_reg) => {
                let source_value = registers.get_u16(src_reg);
                match dest {
                    U8Register(_) => unreachable!(
                        "Cant move 16 bit value to 8 bit register. This is an invalid program."
                    ),
                    U16Register(dest_reg) => registers.set_u16(dest_reg, source_value),
                }
            }
        }
    } else {
        use Register::*;
        match dest {
            U8Register(src_reg) => {
                let source_value = registers.get_u8(src_reg);
                match source {
                    U8Register(dest_reg) => registers.set_u8(dest_reg, source_value),
                    U16Register(dest_reg) => registers.set_u16(dest_reg, source_value.into()),
                }
            }
            Register::U16Register(src_reg) => {
                let source_value = registers.get_u16(src_reg);
                match source {
                    U8Register(_) => unreachable!(
                        "Cant move 16 bit value to 8 bit register. This is an invalid program."
                    ),
                    U16Register(dest_reg) => registers.set_u16(dest_reg, source_value),
                }
            }
        }
    }
}
