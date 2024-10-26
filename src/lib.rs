const MEMORY_SIZE: usize = 1 << 16; // 16-bit address space
const REGISTER_COUNT: usize = 8; // 8 general-purpose registers

#[derive(Clone, Copy, Debug)]
enum Register {
    R0 = 0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
}

/// The actual LC-3 instructions are basically incoded as u16
/// For example, ADD (R0, R1, 3) is encoded as 0001 000 000 1 00011
/// That encoding is avoided here
/// That means here the program will be written as vector of instructions(mentioned below)
/// That is why ADD_IMM is other immediate instructions are defined here
#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    ADD(usize, usize, usize),   // ADD DR = SR1 + SR2 (Register mode)
    ADD_IMM(usize, usize, u16), // ADD DR = SR1 + imm5 (Immediate mode)

    AND(usize, usize, usize),   // AND DR = SR1 & SR2 (Register mode)
    AND_IMM(usize, usize, u16), // AND DR = SR1 & imm5 (Immediate mode, upto 2^5 - 1 = 31 max value )

    NOT(usize, usize), // NOT DR = ~SR

    BR(u16, bool, bool, bool), // BR(nzp), conditional branch based on condition codes N, Z, P

    JMP(usize), // JMP to address in BaseR

    JSR(u16),    // JSR PC-relative subroutine call
    JSRR(usize), // JSRR jumps to address in BaseR for subroutine call

    LD(usize, u16),         // LD DR = memory[PC + offset9]
    LDI(usize, u16),        // LDI DR = memory[memory[PC + offset9]]
    LDR(usize, usize, u16), // LDR DR = memory[BaseR + offset6]

    LEA(usize, u16), // LEA DR = PC + offset9

    ST(usize, u16),         // ST memory[PC + offset9] = SR
    STI(usize, u16),        // STI memory[memory[PC + offset9]] = SR
    STR(usize, usize, u16), // STR memory[BaseR + offset6] = SR

    TRAP(u8), // TRAP trapvect8 (system call)
}

pub struct VM {
    memory: [u16; MEMORY_SIZE],       // 65,536 memory locations
    registers: [u16; REGISTER_COUNT], // 8 general-purpose registers
    pc: u16,                          // Program counter
    condition_flags: u16,             // N=1, Z=2, P=4 flags
    execution_trace: Vec<ExecutionTrace>, // To store the execution trace
}

impl VM {
    pub fn new() -> VM {
        VM {
            memory: [0; MEMORY_SIZE],
            registers: [0; REGISTER_COUNT],
            pc: 0,
            condition_flags: 0,
            execution_trace: Vec::new(),
        }
    }

    // Execute an instruction
    pub fn execute(&mut self, instr: Instruction) {

        // Log the current state before executing the instruction
        self.log_execution_trace(instr);

        match instr {
            Instruction::ADD(dr, sr1, sr2) => self.add(dr, sr1, sr2),
            Instruction::ADD_IMM(dr, sr1, imm5) => self.add_imm(dr, sr1, imm5),
            Instruction::AND(dr, sr1, sr2) => self.and(dr, sr1, sr2),
            Instruction::AND_IMM(dr, sr1, imm5) => self.and_imm(dr, sr1, imm5),
            Instruction::NOT(dr, sr) => self.not(dr, sr),
            Instruction::BR(offset, n, z, p) => self.branch(offset, n, z, p),
            Instruction::JMP(base_r) => self.jmp(base_r),
            Instruction::JSR(offset) => self.jsr(offset),
            Instruction::JSRR(base_r) => self.jsrr(base_r),
            Instruction::LD(dr, offset) => self.ld(dr, offset),
            Instruction::LDI(dr, offset) => self.ldi(dr, offset),
            Instruction::LDR(dr, base_r, offset) => self.ldr(dr, base_r, offset),
            Instruction::LEA(dr, offset) => self.lea(dr, offset),
            Instruction::ST(sr, offset) => self.st(sr, offset),
            Instruction::STI(sr, offset) => self.sti(sr, offset),
            Instruction::STR(sr, base_r, offset) => self.str(sr, base_r, offset),
            Instruction::TRAP(trap_vector) => self.trap(trap_vector),
        }
    }

    fn update_flags(&mut self, r: usize) {
        if self.registers[r] == 0 {
            self.condition_flags = 2;
        } else if self.registers[r] >> 15 & 1 == 1
        /* a 1 in the left-most bit indicates negative */
        {
            self.condition_flags = 1;
        } else {
            self.condition_flags = 4;
        }
    }

    fn log_execution_trace(&mut self, instruction: Instruction) {
        let trace_entry = ExecutionTrace::new(self, instruction);
        self.execution_trace.push(trace_entry);
    }

}

impl VM {
    fn add(&mut self, dr: usize, sr1: usize, sr2: usize) {
        self.registers[dr] = self.registers[sr1] + self.registers[sr2];
        self.update_flags(dr);
    }

    fn add_imm(&mut self, dr: usize, sr1: usize, imm5: u16) {
        self.registers[dr] = self.registers[sr1] + imm5;
        self.update_flags(dr);
    }

    fn and(&mut self, dr: usize, sr1: usize, sr2: usize) {
        self.registers[dr] = self.registers[sr1] & self.registers[sr2];
        self.update_flags(dr);
    }

    fn and_imm(&mut self, dr: usize, sr1: usize, imm5: u16) {
        self.registers[dr] = self.registers[sr1] & imm5;
        self.update_flags(dr);
    }

    fn not(&mut self, dr: usize, sr: usize) {
        self.registers[dr] = !self.registers[sr];
        self.update_flags(dr);
    }

    fn branch(&mut self, offset: u16, n: bool, z: bool, p: bool) {
        // Check if the branch should be taken based on the condition flags
        if (n && self.condition_flags == 1) || // N (Negative)
           (z && self.condition_flags == 2) || // Z (Zero)
           (p && self.condition_flags == 4) // P (Positive)
        {
            // Update the PC by the offset if any of the conditions match
            self.pc = self.pc + offset;
        }
    }

    fn jmp(&mut self, base_r: usize) {
        self.pc = self.registers[base_r];
    }

    fn jsr(&mut self, offset: u16) {
        self.registers[7] = self.pc; // Save current PC to R7
        self.pc = self.pc + offset;
    }

    fn jsrr(&mut self, base_r: usize) {
        self.registers[7] = self.pc; // Save current PC to R7
        self.pc = self.registers[base_r];
    }

    fn ld(&mut self, dr: usize, offset: u16) {
        let address = self.pc + offset;
        self.registers[dr] = self.memory[address as usize];
        self.update_flags(dr);
    }

    fn ldi(&mut self, dr: usize, offset: u16) {
        let address = ((self.pc as u16) + offset) as usize;
        let indirect_address = self.memory[address] as usize;
        self.registers[dr] = self.memory[indirect_address];
        self.update_flags(dr);
    }

    fn ldr(&mut self, dr: usize, base_r: usize, offset: u16) {
        let address = (self.registers[base_r] as u16 + offset) as usize;
        self.registers[dr] = self.memory[address];
        self.update_flags(dr);
    }

    fn st(&mut self, sr: usize, offset: u16) {
        let address = ((self.pc as u16) + offset) as usize;
        self.memory[address] = self.registers[sr];
    }

    fn sti(&mut self, sr: usize, offset: u16) {
        let address = ((self.pc as u16) + offset) as usize;
        let indirect_address = self.memory[address] as usize;
        self.memory[indirect_address] = self.registers[sr];
    }

    fn str(&mut self, sr: usize, base_r: usize, offset: u16) {
        let address = (self.registers[base_r] as u16 + offset) as usize;
        self.memory[address] = self.registers[sr];
    }

    fn lea(&mut self, dr: usize, offset: u16) {
        self.registers[dr] = self.pc + offset;
        self.update_flags(dr);
    }

    fn trap(&mut self, trap_vector: u8) {
        // Save current PC in R7 and set PC to trap vector
        self.registers[7] = self.pc;
        self.pc = trap_vector as u16;
    }
}


#[derive(Debug, Clone)]
pub struct ExecutionTrace {
    pub pc: u16,                     // Program counter
    pub registers: [u16; REGISTER_COUNT], // State of the registers
    pub memory: Vec<u16>,            // Memory snapshot at the point (you may choose to only log changes)
    pub condition_flags: u16,        // Condition flags (N, Z, P)
    pub instruction: Instruction,    // Instruction being executed at this step
}

impl ExecutionTrace {
    pub fn new(vm: &VM, instruction: Instruction) -> Self {
        // Create a snapshot of the current state of the VM
        ExecutionTrace {
            pc: vm.pc,
            registers: vm.registers,
            memory: vm.memory.to_vec(), // If you only want memory changes, you can optimize this later
            condition_flags: vm.condition_flags,
            instruction,
        }
    }
}