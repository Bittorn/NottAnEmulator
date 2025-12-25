package main

import "core:container/small_array"

// Code translated from C++ by the legendary OneLoneCoder
// https://github.com/OneLoneCoder/olcNES

cpu_a: u8       = 0x00		// Accumulator Register
cpu_x: u8       = 0x00		// X Register
cpu_y: u8       = 0x00		// Y Register
cpu_stkp: u8    = 0x00		// Stack Pointer (points to location on bus)
cpu_pc: u16     = 0x0000	// Program Counter
cpu_status: u8  = 0x00		// Status Register
	
// Helper variables
cpu_fetched: u8         = 0x00      // Represents the working input value to the ALU
cpu_temp: u16           = 0x0000    // A convenience variable used everywhere
cpu_addr_abs: u16       = 0x0000    // All used memory addresses end up in here
cpu_addr_rel: u16       = 0x00      // Represents absolute address following a branch
cpu_opcode: u8          = 0x00      // Is the instruction byte
cpu_cycles: u8          = 0         // Counts how many cycles the instruction has remaining
cpu_clock_count: u32    = 0         // A global accumulation of the number of clocks

// Returns the value of a specific bit of the status register
get_flag :: proc(flag: FLAGS6502) -> u8 {
	return ((cpu_status & u8(flag)) > 0) ? 1 : 0
}

// Sets or clears a specific bit of the status register
set_flag :: proc(flag: FLAGS6502, value: bool) {
	if (value) {
		cpu_status |= u8(flag)
	} else {
		cpu_status &= ~u8(flag)
	}
}

/* Forces the 6502 into a known state. This is hard-wired inside the CPU. The
	registers are set to 0x00, the status register is cleared except for unused
	bit which remains at 1. An absolute address is read from location 0xFFFC
	which contains a second address that the program counter is set to. This 
	allows the programmer to jump to a known and programmable location in the
	memory to start executing from. Typically the programmer would set the value
	at location 0xFFFC at compile time. */
reset_cpu :: proc() {
	// Get address to set program counter to
    cpu_addr_abs = 0xFFFC
	lo := u16(read(cpu_addr_abs + 0))
	hi := u16(read(cpu_addr_abs + 1))

	// Set it
	cpu_pc = (hi << 8) | lo

	// Reset internal registers
	cpu_a = 0
	cpu_x = 0
	cpu_y = 0
	cpu_stkp = 0xFD
	cpu_status = 0x00 | u8(FLAGS6502.U)

	// // Clear internal helper variables
	cpu_addr_rel = 0x0000
	cpu_addr_abs = 0x0000
	cpu_fetched = 0x00

	// Reset takes time
	cpu_cycles = 8
}

/* Interrupt requests are a complex operation and only happen if the
	"disable interrupt" flag is 0. IRQs can happen at any time, but
	you dont want them to be destructive to the operation of the running 
	program. Therefore the current instruction is allowed to finish
	(which I facilitate by doing the whole thing when cycles == 0) and 
	then the current program counter is stored on the stack. Then the
	current status register is stored on the stack. When the routine
	that services the interrupt has finished, the status register
	and program counter can be restored to how they where before it 
	occurred. This is impemented by the "RTI" instruction. Once the IRQ
	has happened, in a similar way to a reset, a programmable address
	is read form hard coded location 0xFFFE, which is subsequently
	set to the program counter. */
irq_cpu :: proc() {
    // If interrupts are allowed
	if (get_flag(FLAGS6502.I) == 0) {
		// Push the program counter to the stack.
		// 16-bit so that takes two pushes
		write(0x0100 + u16(cpu_stkp), u8((cpu_pc >> 8) & 0x00FF))
		cpu_stkp = cpu_stkp - 1
		write(0x0100 + u16(cpu_stkp), u8(cpu_pc & 0x00FF))
		cpu_stkp = cpu_stkp - 1

		// Then Push the status register to the stack
		set_flag(FLAGS6502.B, false)
		set_flag(FLAGS6502.U, true)
		set_flag(FLAGS6502.I, true)
		write(0x0100 + u16(cpu_stkp), cpu_status)
		cpu_stkp = cpu_stkp - 1

		// Read new program counter location from fixed address
		cpu_addr_abs = 0xFFFE
		lo := u16(read(cpu_addr_abs + 0))
		hi := u16(read(cpu_addr_abs + 1))
		cpu_pc = (hi << 8) | lo

		// IRQs take time
		cpu_cycles = 7
	}
}

/* A Non-Maskable Interrupt cannot be ignored. It behaves in exactly the
	same way as a regular IRQ, but reads the new program counter address
	from location 0xFFFA. */
nmi_cpu :: proc() {
    write(0x0100 + u16(cpu_stkp), u8((cpu_pc >> 8) & 0x00FF))
	cpu_stkp = cpu_stkp - 1
	write(0x0100 + u16(cpu_stkp), u8(cpu_pc & 0x00FF))
	cpu_stkp = cpu_stkp - 1

	set_flag(FLAGS6502.B, false)
	set_flag(FLAGS6502.U, true)
	set_flag(FLAGS6502.I, true)
	write(0x0100 + u16(cpu_stkp), cpu_status)
	cpu_stkp = cpu_stkp - 1

	cpu_addr_abs = 0xFFFA
	lo := u16(read(cpu_addr_abs + 0))
	hi := u16(read(cpu_addr_abs + 1))
	cpu_pc = (hi << 8) | lo

	cpu_cycles = 8
}

// Perform one clock cycles worth of emulation
clock_cpu :: proc() {
    /* Each instruction requires a variable number of clock cycles to execute.
		In this emulation, we only care about the final result, and so perform
		the entire computation in one hit. In hardware, each clock cycle would
		perform "microcode" style transformations of the CPUs state.

		To remain compliant with connected devices, it's important that the 
		emulation also takes "time" in order to execute instructions, so we
		implement that delay by simply counting down the cycles required by 
		the instruction. When it reaches 0, the instruction is complete, and
		the next one is ready to be executed. */
	
	if (cpu_cycles == 0) {
		/* Read next instruction byte. This 8-bit value is used to index
			the translation table to get the relevant information about
			how to implement the instruction */
		cpu_opcode = read(cpu_pc)

		// Always set the unused status flag bit to 1
		set_flag(FLAGS6502.U, true);
		
		// Increment program counter, we read the opcode byte
		cpu_pc = cpu_pc + 1

		// Get Starting number of cycles
		cpu_cycles = cpu_lookup[cpu_opcode].cycles;

		// Perform fetch of intermmediate data using the
		// required addressing mode
		additional_cycle1: u8 = ^cpu_lookup[cpu_opcode].addrmode();

		// Perform operation
		additional_cycle2: u8 = ^cpu_lookup[cpu_opcode].operate();

		// The addressmode and opcode may have altered the number
		// of cycles this instruction requires before its completed
		cpu_cycles += (additional_cycle1 & additional_cycle2);

		// Always set the unused status flag bit to 1
		set_flag(FLAGS6502.U, true);
	}
}

// Enums are printed as their name, not their value
FLAGS6502 :: enum u8 {
	C = 1 << 0,	// Carry Bit
	Z = 1 << 1,	// Zero
	I = 1 << 2,	// Disable Interrupts
	D = 1 << 3,	// Decimal Mode (unused in this implementation)
	B = 1 << 4,	// Break
	U = 1 << 5,	// Unused
	V = 1 << 6,	// Overflow
	N = 1 << 7,	// Negative
}

CPU_INSTRUCTION :: struct {
	name: string,
	operate: ^proc() -> u8, // might not be correct
	addrmode: ^proc() -> u8, // also might not be correct
	cycles: u8,
}

// The most brute-force solution but it works!
cpu_lookup := [256]CPU_INSTRUCTION{
	{ "BRK", &BRK, &IMM, 7 },{ "ORA", &ORA, &IZX, 6 },{ "???", &XXX, &IMP, 2 },{ "???", &XXX, &IMP, 8 },{ "???", &NOP, &IMP, 3 },{ "ORA", &ORA, &ZP0, 3 },{ "ASL", &ASL, &ZP0, 5 },{ "???", &XXX, &IMP, 5 },{ "PHP", &PHP, &IMP, 3 },{ "ORA", &ORA, &IMM, 2 },{ "ASL", &ASL, &IMP, 2 },{ "???", &XXX, &IMP, 2 },{ "???", &NOP, &IMP, 4 },{ "ORA", &ORA, &ABS, 4 },{ "ASL", &ASL, &ABS, 6 },{ "???", &XXX, &IMP, 6 },
	{ "BPL", &BPL, &REL, 2 },{ "ORA", &ORA, &IZY, 5 },{ "???", &XXX, &IMP, 2 },{ "???", &XXX, &IMP, 8 },{ "???", &NOP, &IMP, 4 },{ "ORA", &ORA, &ZPX, 4 },{ "ASL", &ASL, &ZPX, 6 },{ "???", &XXX, &IMP, 6 },{ "CLC", &CLC, &IMP, 2 },{ "ORA", &ORA, &ABY, 4 },{ "???", &NOP, &IMP, 2 },{ "???", &XXX, &IMP, 7 },{ "???", &NOP, &IMP, 4 },{ "ORA", &ORA, &ABX, 4 },{ "ASL", &ASL, &ABX, 7 },{ "???", &XXX, &IMP, 7 },
	{ "JSR", &JSR, &ABS, 6 },{ "AND", &AND, &IZX, 6 },{ "???", &XXX, &IMP, 2 },{ "???", &XXX, &IMP, 8 },{ "BIT", &BIT, &ZP0, 3 },{ "AND", &AND, &ZP0, 3 },{ "ROL", &ROL, &ZP0, 5 },{ "???", &XXX, &IMP, 5 },{ "PLP", &PLP, &IMP, 4 },{ "AND", &AND, &IMM, 2 },{ "ROL", &ROL, &IMP, 2 },{ "???", &XXX, &IMP, 2 },{ "BIT", &BIT, &ABS, 4 },{ "AND", &AND, &ABS, 4 },{ "ROL", &ROL, &ABS, 6 },{ "???", &XXX, &IMP, 6 },
	{ "BMI", &BMI, &REL, 2 },{ "AND", &AND, &IZY, 5 },{ "???", &XXX, &IMP, 2 },{ "???", &XXX, &IMP, 8 },{ "???", &NOP, &IMP, 4 },{ "AND", &AND, &ZPX, 4 },{ "ROL", &ROL, &ZPX, 6 },{ "???", &XXX, &IMP, 6 },{ "SEC", &SEC, &IMP, 2 },{ "AND", &AND, &ABY, 4 },{ "???", &NOP, &IMP, 2 },{ "???", &XXX, &IMP, 7 },{ "???", &NOP, &IMP, 4 },{ "AND", &AND, &ABX, 4 },{ "ROL", &ROL, &ABX, 7 },{ "???", &XXX, &IMP, 7 },
	{ "RTI", &RTI, &IMP, 6 },{ "EOR", &EOR, &IZX, 6 },{ "???", &XXX, &IMP, 2 },{ "???", &XXX, &IMP, 8 },{ "???", &NOP, &IMP, 3 },{ "EOR", &EOR, &ZP0, 3 },{ "LSR", &LSR, &ZP0, 5 },{ "???", &XXX, &IMP, 5 },{ "PHA", &PHA, &IMP, 3 },{ "EOR", &EOR, &IMM, 2 },{ "LSR", &LSR, &IMP, 2 },{ "???", &XXX, &IMP, 2 },{ "JMP", &JMP, &ABS, 3 },{ "EOR", &EOR, &ABS, 4 },{ "LSR", &LSR, &ABS, 6 },{ "???", &XXX, &IMP, 6 },
	{ "BVC", &BVC, &REL, 2 },{ "EOR", &EOR, &IZY, 5 },{ "???", &XXX, &IMP, 2 },{ "???", &XXX, &IMP, 8 },{ "???", &NOP, &IMP, 4 },{ "EOR", &EOR, &ZPX, 4 },{ "LSR", &LSR, &ZPX, 6 },{ "???", &XXX, &IMP, 6 },{ "CLI", &CLI, &IMP, 2 },{ "EOR", &EOR, &ABY, 4 },{ "???", &NOP, &IMP, 2 },{ "???", &XXX, &IMP, 7 },{ "???", &NOP, &IMP, 4 },{ "EOR", &EOR, &ABX, 4 },{ "LSR", &LSR, &ABX, 7 },{ "???", &XXX, &IMP, 7 },
	{ "RTS", &RTS, &IMP, 6 },{ "ADC", &ADC, &IZX, 6 },{ "???", &XXX, &IMP, 2 },{ "???", &XXX, &IMP, 8 },{ "???", &NOP, &IMP, 3 },{ "ADC", &ADC, &ZP0, 3 },{ "ROR", &ROR, &ZP0, 5 },{ "???", &XXX, &IMP, 5 },{ "PLA", &PLA, &IMP, 4 },{ "ADC", &ADC, &IMM, 2 },{ "ROR", &ROR, &IMP, 2 },{ "???", &XXX, &IMP, 2 },{ "JMP", &JMP, &IND, 5 },{ "ADC", &ADC, &ABS, 4 },{ "ROR", &ROR, &ABS, 6 },{ "???", &XXX, &IMP, 6 },
	{ "BVS", &BVS, &REL, 2 },{ "ADC", &ADC, &IZY, 5 },{ "???", &XXX, &IMP, 2 },{ "???", &XXX, &IMP, 8 },{ "???", &NOP, &IMP, 4 },{ "ADC", &ADC, &ZPX, 4 },{ "ROR", &ROR, &ZPX, 6 },{ "???", &XXX, &IMP, 6 },{ "SEI", &SEI, &IMP, 2 },{ "ADC", &ADC, &ABY, 4 },{ "???", &NOP, &IMP, 2 },{ "???", &XXX, &IMP, 7 },{ "???", &NOP, &IMP, 4 },{ "ADC", &ADC, &ABX, 4 },{ "ROR", &ROR, &ABX, 7 },{ "???", &XXX, &IMP, 7 },
	{ "???", &NOP, &IMP, 2 },{ "STA", &STA, &IZX, 6 },{ "???", &NOP, &IMP, 2 },{ "???", &XXX, &IMP, 6 },{ "STY", &STY, &ZP0, 3 },{ "STA", &STA, &ZP0, 3 },{ "STX", &STX, &ZP0, 3 },{ "???", &XXX, &IMP, 3 },{ "DEY", &DEY, &IMP, 2 },{ "???", &NOP, &IMP, 2 },{ "TXA", &TXA, &IMP, 2 },{ "???", &XXX, &IMP, 2 },{ "STY", &STY, &ABS, 4 },{ "STA", &STA, &ABS, 4 },{ "STX", &STX, &ABS, 4 },{ "???", &XXX, &IMP, 4 },
	{ "BCC", &BCC, &REL, 2 },{ "STA", &STA, &IZY, 6 },{ "???", &XXX, &IMP, 2 },{ "???", &XXX, &IMP, 6 },{ "STY", &STY, &ZPX, 4 },{ "STA", &STA, &ZPX, 4 },{ "STX", &STX, &ZPY, 4 },{ "???", &XXX, &IMP, 4 },{ "TYA", &TYA, &IMP, 2 },{ "STA", &STA, &ABY, 5 },{ "TXS", &TXS, &IMP, 2 },{ "???", &XXX, &IMP, 5 },{ "???", &NOP, &IMP, 5 },{ "STA", &STA, &ABX, 5 },{ "???", &XXX, &IMP, 5 },{ "???", &XXX, &IMP, 5 },
	{ "LDY", &LDY, &IMM, 2 },{ "LDA", &LDA, &IZX, 6 },{ "LDX", &LDX, &IMM, 2 },{ "???", &XXX, &IMP, 6 },{ "LDY", &LDY, &ZP0, 3 },{ "LDA", &LDA, &ZP0, 3 },{ "LDX", &LDX, &ZP0, 3 },{ "???", &XXX, &IMP, 3 },{ "TAY", &TAY, &IMP, 2 },{ "LDA", &LDA, &IMM, 2 },{ "TAX", &TAX, &IMP, 2 },{ "???", &XXX, &IMP, 2 },{ "LDY", &LDY, &ABS, 4 },{ "LDA", &LDA, &ABS, 4 },{ "LDX", &LDX, &ABS, 4 },{ "???", &XXX, &IMP, 4 },
	{ "BCS", &BCS, &REL, 2 },{ "LDA", &LDA, &IZY, 5 },{ "???", &XXX, &IMP, 2 },{ "???", &XXX, &IMP, 5 },{ "LDY", &LDY, &ZPX, 4 },{ "LDA", &LDA, &ZPX, 4 },{ "LDX", &LDX, &ZPY, 4 },{ "???", &XXX, &IMP, 4 },{ "CLV", &CLV, &IMP, 2 },{ "LDA", &LDA, &ABY, 4 },{ "TSX", &TSX, &IMP, 2 },{ "???", &XXX, &IMP, 4 },{ "LDY", &LDY, &ABX, 4 },{ "LDA", &LDA, &ABX, 4 },{ "LDX", &LDX, &ABY, 4 },{ "???", &XXX, &IMP, 4 },
	{ "CPY", &CPY, &IMM, 2 },{ "CMP", &CMP, &IZX, 6 },{ "???", &NOP, &IMP, 2 },{ "???", &XXX, &IMP, 8 },{ "CPY", &CPY, &ZP0, 3 },{ "CMP", &CMP, &ZP0, 3 },{ "DEC", &DEC, &ZP0, 5 },{ "???", &XXX, &IMP, 5 },{ "INY", &INY, &IMP, 2 },{ "CMP", &CMP, &IMM, 2 },{ "DEX", &DEX, &IMP, 2 },{ "???", &XXX, &IMP, 2 },{ "CPY", &CPY, &ABS, 4 },{ "CMP", &CMP, &ABS, 4 },{ "DEC", &DEC, &ABS, 6 },{ "???", &XXX, &IMP, 6 },
	{ "BNE", &BNE, &REL, 2 },{ "CMP", &CMP, &IZY, 5 },{ "???", &XXX, &IMP, 2 },{ "???", &XXX, &IMP, 8 },{ "???", &NOP, &IMP, 4 },{ "CMP", &CMP, &ZPX, 4 },{ "DEC", &DEC, &ZPX, 6 },{ "???", &XXX, &IMP, 6 },{ "CLD", &CLD, &IMP, 2 },{ "CMP", &CMP, &ABY, 4 },{ "NOP", &NOP, &IMP, 2 },{ "???", &XXX, &IMP, 7 },{ "???", &NOP, &IMP, 4 },{ "CMP", &CMP, &ABX, 4 },{ "DEC", &DEC, &ABX, 7 },{ "???", &XXX, &IMP, 7 },
	{ "CPX", &CPX, &IMM, 2 },{ "SBC", &SBC, &IZX, 6 },{ "???", &NOP, &IMP, 2 },{ "???", &XXX, &IMP, 8 },{ "CPX", &CPX, &ZP0, 3 },{ "SBC", &SBC, &ZP0, 3 },{ "INC", &INC, &ZP0, 5 },{ "???", &XXX, &IMP, 5 },{ "INX", &INX, &IMP, 2 },{ "SBC", &SBC, &IMM, 2 },{ "NOP", &NOP, &IMP, 2 },{ "???", &SBC, &IMP, 2 },{ "CPX", &CPX, &ABS, 4 },{ "SBC", &SBC, &ABS, 4 },{ "INC", &INC, &ABS, 6 },{ "???", &XXX, &IMP, 6 },
	{ "BEQ", &BEQ, &REL, 2 },{ "SBC", &SBC, &IZY, 5 },{ "???", &XXX, &IMP, 2 },{ "???", &XXX, &IMP, 8 },{ "???", &NOP, &IMP, 4 },{ "SBC", &SBC, &ZPX, 4 },{ "INC", &INC, &ZPX, 6 },{ "???", &XXX, &IMP, 6 },{ "SED", &SED, &IMP, 2 },{ "SBC", &SBC, &ABY, 4 },{ "NOP", &NOP, &IMP, 2 },{ "???", &XXX, &IMP, 7 },{ "???", &NOP, &IMP, 4 },{ "SBC", &SBC, &ABX, 4 },{ "INC", &INC, &ABX, 7 },{ "???", &XXX, &IMP, 7 },
}