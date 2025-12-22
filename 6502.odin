package main

a: u8       = 0x00		// Accumulator Register
x: u8       = 0x00		// X Register
y: u8       = 0x00		// Y Register
stkp: u8    = 0x00		// Stack Pointer (points to location on bus)
pc: u16     = 0x0000	// Program Counter
status: u8  = 0x00		// Status Register
	
// Assisstive variables to facilitate emulation
fetched: u8         = 0x00      // Represents the working input value to the ALU
temp: u16           = 0x0000    // A convenience variable used everywhere
addr_abs: u16       = 0x0000    // All used memory addresses end up in here
addr_rel: u16       = 0x00      // Represents absolute address following a branch
opcode: u8          = 0x00      // Is the instruction byte
cycles: u8          = 0         // Counts how many cycles the instruction has remaining
clock_count: u32    = 0         // A global accumulation of the number of clocks

reset :: proc() {
    addr_abs = 0xFFFC
	// lo: u16 = read(addr_abs + 0);
	// hi: u16 = read(addr_abs + 1);

	// Set it
	// pc = (hi << 8) | lo;

	// Reset internal registers
	a = 0
	x = 0
	y = 0
	stkp = 0xFD
	status = 0x00 | u8(FLAGS.U)

	// Clear internal helper variables
	addr_rel = 0x0000
	addr_abs = 0x0000
	fetched = 0x00

	// Reset takes time
	cycles = 8
}

irq :: proc() {
    // do something
}

nmi :: proc() {
    // do something
}

clock :: proc() {
    // do something
}

FLAGS :: enum {
	C = (1 << 0),	// Carry Bit
	Z = (1 << 1),	// Zero
	I = (1 << 2),	// Disable Interrupts
	D = (1 << 3),	// Decimal Mode (unused in this implementation)
	B = (1 << 4),	// Break
	U = (1 << 5),	// Unused
	V = (1 << 6),	// Overflow
	N = (1 << 7)	// Negative
}