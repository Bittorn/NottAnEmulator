package main

// Code translated from C++ by the legendary OneLoneCoder
// https://github.com/OneLoneCoder/olcNES

a: u8       = 0x00		// Accumulator Register
x: u8       = 0x00		// X Register
y: u8       = 0x00		// Y Register
stkp: u8    = 0x00		// Stack Pointer (points to location on bus)
pc: u16     = 0x0000	// Program Counter
status: u8  = 0x00		// Status Register
	
// Helper variables
fetched: u8         = 0x00      // Represents the working input value to the ALU
temp: u16           = 0x0000    // A convenience variable used everywhere
addr_abs: u16       = 0x0000    // All used memory addresses end up in here
addr_rel: u16       = 0x00      // Represents absolute address following a branch
opcode: u8          = 0x00      // Is the instruction byte
cycles: u8          = 0         // Counts how many cycles the instruction has remaining
clock_count: u32    = 0         // A global accumulation of the number of clocks

// Returns the value of a specific bit of the status register
get_flag :: proc(flag: FLAGS) -> u8 {
	return ((status & u8(flag)) > 0) ? 1 : 0
}

// Sets or clears a specific bit of the status register
set_flag :: proc(flag: FLAGS, value: bool) {
	if (value) {
		status |= u8(flag)
	} else {
		status &= ~u8(flag)
	}
}

/* Forces the 6502 into a known state. This is hard-wired inside the CPU. The
	registers are set to 0x00, the status register is cleared except for unused
	bit which remains at 1. An absolute address is read from location 0xFFFC
	which contains a second address that the program counter is set to. This 
	allows the programmer to jump to a known and programmable location in the
	memory to start executing from. Typically the programmer would set the value
	at location 0xFFFC at compile time. */
reset :: proc() {
	// Get address to set program counter to
    addr_abs = 0xFFFC
	lo := u16(read(addr_abs + 0))
	hi := u16(read(addr_abs + 1))

	// Set it
	pc = (hi << 8) | lo

	// Reset internal registers
	a = 0
	x = 0
	y = 0
	stkp = 0xFD
	status = 0x00 | u8(FLAGS.U)

	// // Clear internal helper variables
	addr_rel = 0x0000
	addr_abs = 0x0000
	fetched = 0x00

	// Reset takes time
	cycles = 8
}

/* IRQs can happen at any time, but you don't
	want them to be destructive to the operation of the running 
	program. Therefore, the current instruction is allowed to finish
	(which is facilitated by doing the whole thing when cycles == 0) and 
	then the current program counter is stored on the stack. Then the
	current status register is stored on the stack. When the routine
	that services the interrupt has finished, the status register
	and program counter can be restored to how they where before it 
	occurred. This is implemented by the "RTI" instruction. Once the IRQ
	has happened, in a similar way to a reset, a programmable address
	is read from the hard coded location 0xFFFE, which is subsequently
	set to the program counter. */
irq :: proc() {
    // If interrupts are allowed
	if (get_flag(FLAGS.I) == 0) {
		// Push the program counter to the stack.
		// 16-bit so that takes two pushes
		write(0x0100 + u16(stkp), u8((pc >> 8) & 0x00FF))
		stkp = stkp - 1
		write(0x0100 + u16(stkp), u8(pc & 0x00FF))
		stkp = stkp - 1

		// Then Push the status register to the stack
		set_flag(FLAGS.B, false)
		set_flag(FLAGS.U, true)
		set_flag(FLAGS.I, true)
		write(0x0100 + u16(stkp), status)
		stkp = stkp - 1

		// Read new program counter location from fixed address
		addr_abs = 0xFFFE
		lo := u16(read(addr_abs + 0))
		hi := u16(read(addr_abs + 1))
		pc = (hi << 8) | lo

		// IRQs take time
		cycles = 7
	}
}

/* A Non-Maskable Interrupt cannot be ignored. It behaves in exactly the
	same way as a regular IRQ, but reads the new program counter address
	from location 0xFFFA. */
nmi :: proc() {
    write(0x0100 + u16(stkp), u8((pc >> 8) & 0x00FF))
	stkp = stkp - 1
	write(0x0100 + u16(stkp), u8(pc & 0x00FF))
	stkp = stkp - 1

	set_flag(FLAGS.B, false)
	set_flag(FLAGS.U, true)
	set_flag(FLAGS.I, true)
	write(0x0100 + u16(stkp), status)
	stkp = stkp - 1

	addr_abs = 0xFFFA
	lo := u16(read(addr_abs + 0))
	hi := u16(read(addr_abs + 1))
	pc = (hi << 8) | lo

	cycles = 8
}

// Perform one clock cycles worth of emulation
clock :: proc() {
    /* Each instruction requires a variable number of clock cycles to execute.
		In this emulation, we only care about the final result, and so perform
		the entire computation in one hit. In hardware, each clock cycle would
		perform "microcode" style transformations of the CPUs state.

		To remain compliant with connected devices, it's important that the 
		emulation also takes "time" in order to execute instructions, so we
		implement that delay by simply counting down the cycles required by 
		the instruction. When it reaches 0, the instruction is complete, and
		the next one is ready to be executed. */
	
	if (cycles == 0) {
		/* Read next instruction byte. This 8-bit value is used to index
			the translation table to get the relevant information about
			how to implement the instruction */
		opcode = read(pc)

		// Always set the unused status flag bit to 1
		set_flag(FLAGS.U, true);
		
		// Increment program counter, we read the opcode byte
		pc = pc + 1

		// Get Starting number of cycles
		cycles = lookup[opcode].cycles;

		// Perform fetch of intermmediate data using the
		// required addressing mode
		additional_cycle1: u8 = ^lookup[opcode].addrmode();

		// Perform operation
		additional_cycle2: u8 = ^lookup[opcode].operate();

		// The addressmode and opcode may have altered the number
		// of cycles this instruction requires before its completed
		cycles += (additional_cycle1 & additional_cycle2);

		// Always set the unused status flag bit to 1
		set_flag(FLAGS.U, true);
	}
}

// Enums are printed as their name, not their value
FLAGS :: enum u8 {
	C = 1 << 0,	// Carry Bit
	Z = 1 << 1,	// Zero
	I = 1 << 2,	// Disable Interrupts
	D = 1 << 3,	// Decimal Mode (unused in this implementation)
	B = 1 << 4,	// Break
	U = 1 << 5,	// Unused
	V = 1 << 6,	// Overflow
	N = 1 << 7,	// Negative
}