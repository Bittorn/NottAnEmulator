package main

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
		cpu_cycles = lookup[cpu_opcode].cycles;

		// Perform fetch of intermmediate data using the
		// required addressing mode
		additional_cycle1: u8 = *lookup[cpu_opcode].addrmode();

		// Perform operation
		additional_cycle2: u8 = *lookup[cpu_opcode].operate();

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