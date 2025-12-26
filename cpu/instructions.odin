package main

INSTRUCTION :: struct {
	name: string,
	operate: proc() -> u8, // might not be correct
	addrmode: proc() -> u8, // also might not be correct
	cycles: u8,
}

// The most brute-force solution but it works!
cpu_lookup := [256]INSTRUCTION{
	{ "BRK", BRK, IMM, 7 },{ "ORA", ORA, IZX, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 3 },{ "ORA", ORA, ZP0, 3 },{ "ASL", ASL, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "PHP", PHP, IMP, 3 },{ "ORA", ORA, IMM, 2 },{ "ASL", ASL, IMP, 2 },{ "???", XXX, IMP, 2 },{ "???", NOP, IMP, 4 },{ "ORA", ORA, ABS, 4 },{ "ASL", ASL, ABS, 6 },{ "???", XXX, IMP, 6 },
	{ "BPL", BPL, REL, 2 },{ "ORA", ORA, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "ORA", ORA, ZPX, 4 },{ "ASL", ASL, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "CLC", CLC, IMP, 2 },{ "ORA", ORA, ABY, 4 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "ORA", ORA, ABX, 4 },{ "ASL", ASL, ABX, 7 },{ "???", XXX, IMP, 7 },
	{ "JSR", JSR, ABS, 6 },{ "AND", AND, IZX, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "BIT", BIT, ZP0, 3 },{ "AND", AND, ZP0, 3 },{ "ROL", ROL, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "PLP", PLP, IMP, 4 },{ "AND", AND, IMM, 2 },{ "ROL", ROL, IMP, 2 },{ "???", XXX, IMP, 2 },{ "BIT", BIT, ABS, 4 },{ "AND", AND, ABS, 4 },{ "ROL", ROL, ABS, 6 },{ "???", XXX, IMP, 6 },
	{ "BMI", BMI, REL, 2 },{ "AND", AND, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "AND", AND, ZPX, 4 },{ "ROL", ROL, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "SEC", SEC, IMP, 2 },{ "AND", AND, ABY, 4 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "AND", AND, ABX, 4 },{ "ROL", ROL, ABX, 7 },{ "???", XXX, IMP, 7 },
	{ "RTI", RTI, IMP, 6 },{ "EOR", EOR, IZX, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 3 },{ "EOR", EOR, ZP0, 3 },{ "LSR", LSR, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "PHA", PHA, IMP, 3 },{ "EOR", EOR, IMM, 2 },{ "LSR", LSR, IMP, 2 },{ "???", XXX, IMP, 2 },{ "JMP", JMP, ABS, 3 },{ "EOR", EOR, ABS, 4 },{ "LSR", LSR, ABS, 6 },{ "???", XXX, IMP, 6 },
	{ "BVC", BVC, REL, 2 },{ "EOR", EOR, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "EOR", EOR, ZPX, 4 },{ "LSR", LSR, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "CLI", CLI, IMP, 2 },{ "EOR", EOR, ABY, 4 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "EOR", EOR, ABX, 4 },{ "LSR", LSR, ABX, 7 },{ "???", XXX, IMP, 7 },
	{ "RTS", RTS, IMP, 6 },{ "ADC", ADC, IZX, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 3 },{ "ADC", ADC, ZP0, 3 },{ "ROR", ROR, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "PLA", PLA, IMP, 4 },{ "ADC", ADC, IMM, 2 },{ "ROR", ROR, IMP, 2 },{ "???", XXX, IMP, 2 },{ "JMP", JMP, IND, 5 },{ "ADC", ADC, ABS, 4 },{ "ROR", ROR, ABS, 6 },{ "???", XXX, IMP, 6 },
	{ "BVS", BVS, REL, 2 },{ "ADC", ADC, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "ADC", ADC, ZPX, 4 },{ "ROR", ROR, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "SEI", SEI, IMP, 2 },{ "ADC", ADC, ABY, 4 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "ADC", ADC, ABX, 4 },{ "ROR", ROR, ABX, 7 },{ "???", XXX, IMP, 7 },
	{ "???", NOP, IMP, 2 },{ "STA", STA, IZX, 6 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 6 },{ "STY", STY, ZP0, 3 },{ "STA", STA, ZP0, 3 },{ "STX", STX, ZP0, 3 },{ "???", XXX, IMP, 3 },{ "DEY", DEY, IMP, 2 },{ "???", NOP, IMP, 2 },{ "TXA", TXA, IMP, 2 },{ "???", XXX, IMP, 2 },{ "STY", STY, ABS, 4 },{ "STA", STA, ABS, 4 },{ "STX", STX, ABS, 4 },{ "???", XXX, IMP, 4 },
	{ "BCC", BCC, REL, 2 },{ "STA", STA, IZY, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 6 },{ "STY", STY, ZPX, 4 },{ "STA", STA, ZPX, 4 },{ "STX", STX, ZPY, 4 },{ "???", XXX, IMP, 4 },{ "TYA", TYA, IMP, 2 },{ "STA", STA, ABY, 5 },{ "TXS", TXS, IMP, 2 },{ "???", XXX, IMP, 5 },{ "???", NOP, IMP, 5 },{ "STA", STA, ABX, 5 },{ "???", XXX, IMP, 5 },{ "???", XXX, IMP, 5 },
	{ "LDY", LDY, IMM, 2 },{ "LDA", LDA, IZX, 6 },{ "LDX", LDX, IMM, 2 },{ "???", XXX, IMP, 6 },{ "LDY", LDY, ZP0, 3 },{ "LDA", LDA, ZP0, 3 },{ "LDX", LDX, ZP0, 3 },{ "???", XXX, IMP, 3 },{ "TAY", TAY, IMP, 2 },{ "LDA", LDA, IMM, 2 },{ "TAX", TAX, IMP, 2 },{ "???", XXX, IMP, 2 },{ "LDY", LDY, ABS, 4 },{ "LDA", LDA, ABS, 4 },{ "LDX", LDX, ABS, 4 },{ "???", XXX, IMP, 4 },
	{ "BCS", BCS, REL, 2 },{ "LDA", LDA, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 5 },{ "LDY", LDY, ZPX, 4 },{ "LDA", LDA, ZPX, 4 },{ "LDX", LDX, ZPY, 4 },{ "???", XXX, IMP, 4 },{ "CLV", CLV, IMP, 2 },{ "LDA", LDA, ABY, 4 },{ "TSX", TSX, IMP, 2 },{ "???", XXX, IMP, 4 },{ "LDY", LDY, ABX, 4 },{ "LDA", LDA, ABX, 4 },{ "LDX", LDX, ABY, 4 },{ "???", XXX, IMP, 4 },
	{ "CPY", CPY, IMM, 2 },{ "CMP", CMP, IZX, 6 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 8 },{ "CPY", CPY, ZP0, 3 },{ "CMP", CMP, ZP0, 3 },{ "DEC", DEC, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "INY", INY, IMP, 2 },{ "CMP", CMP, IMM, 2 },{ "DEX", DEX, IMP, 2 },{ "???", XXX, IMP, 2 },{ "CPY", CPY, ABS, 4 },{ "CMP", CMP, ABS, 4 },{ "DEC", DEC, ABS, 6 },{ "???", XXX, IMP, 6 },
	{ "BNE", BNE, REL, 2 },{ "CMP", CMP, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "CMP", CMP, ZPX, 4 },{ "DEC", DEC, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "CLD", CLD, IMP, 2 },{ "CMP", CMP, ABY, 4 },{ "NOP", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "CMP", CMP, ABX, 4 },{ "DEC", DEC, ABX, 7 },{ "???", XXX, IMP, 7 },
	{ "CPX", CPX, IMM, 2 },{ "SBC", SBC, IZX, 6 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 8 },{ "CPX", CPX, ZP0, 3 },{ "SBC", SBC, ZP0, 3 },{ "INC", INC, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "INX", INX, IMP, 2 },{ "SBC", SBC, IMM, 2 },{ "NOP", NOP, IMP, 2 },{ "???", SBC, IMP, 2 },{ "CPX", CPX, ABS, 4 },{ "SBC", SBC, ABS, 4 },{ "INC", INC, ABS, 6 },{ "???", XXX, IMP, 6 },
	{ "BEQ", BEQ, REL, 2 },{ "SBC", SBC, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "SBC", SBC, ZPX, 4 },{ "INC", INC, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "SED", SED, IMP, 2 },{ "SBC", SBC, ABY, 4 },{ "NOP", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "SBC", SBC, ABX, 4 },{ "INC", INC, ABX, 7 },{ "???", XXX, IMP, 7 },
}

// =-=-=-=-=	ADDRESSING MODES	=-=-=-=-=

/* The 6502 can address between 0x0000 - 0xFFFF. The high byte is often referred
	to as the "page", and the low byte is the offset into that page. This implies
	there are 256 pages, each containing 256 bytes.

	Several addressing modes have the potential to require an additional clock
	cycle if they cross a page boundary. This is combined with other instructions
	that enable this additional clock cycle. So each addressing function returns
	a flag saying it has potential, as does each instruction. If both instruction
	and address function return 1, then an additional clock cycle is required. */

/* Address Mode: Implied
	There is no additional data required for this instruction.
	The instruction does something simple like sets a status bit.
	However, this implementation just targets the accumulator,
	for instructions like PHA */
IMP :: proc() -> u8 {
	fetched = a
	return 0
}

/* Address Mode: Immediate
	The instruction expects the next byte to be used as a value, so we'll prep
	the read address to point to the next byte */
IMM :: proc() -> u8 {
	addr_abs = pc + 1
	return 0
}

/* Address Mode: Zero Page
	To save program bytes, ZP0 allows the programmer to absolutely address
	a location in the first 0xFF bytes of address range. Clearly this only
	requires one byte instead of the usual two */
ZP0 :: proc() -> u8 {
	addr_abs = u16(read(pc))
	pc = pc + 1
	addr_abs &= 0x00FF
	return 0
}

/* Address Mode: Zero Page with X Offset
	Same as ZP0, but the contents of register X is added to the supplied
	single byte address. This is useful for iterating through ranges within
	the first page */
ZPX :: proc() -> u8 {
	addr_abs = u16(read(pc) + x)
	pc = pc + 1
	addr_abs &= 0x00FF
	return 0
}

/* Address Mode: Zero Page with Y Offset
	Same as ZPX, but the contents of register Y is added to the supplied
	single byte address */
ZPY :: proc() -> u8 {
	addr_abs = u16(read(pc) + y)
	pc = pc + 1
	addr_abs &= 0x00FF
	return 0
}

/* Address Mode: Relative
	Exclusive to branch instructions, the address must reside within
	-128 to +127 of the branch instruction. This means programmers
	cannot branch directly to any address in the addressable range */
REL :: proc() -> u8 {
	addr_rel = u16(read(pc))
	pc = pc + 1
	if ((addr_rel & 0x80) > 0xFF00) { // TODO: confirm this works
		addr_rel |= 0xFF00
	}
	return 0
}

/* Address Mode: Absolute
	A full 16-bit address is loaded and used */
ABS :: proc() -> u8 {
	lo: u16 = u16(read(pc))
	pc = pc + 1
	hi: u16 = u16(read(pc))
	pc = pc + 1

	addr_abs = (hi << 8) | lo

	return 0
}

/* Address Mode: Absolute with X Offset
	Same as ABS, but the X register is added to the supplied two byte address.
	If the resulting address changes the page, an additional clock
	cycle is required */
ABX :: proc() -> u8 {
	lo: u16 = u16(read(pc))
	pc = pc + 1
	hi: u16 = u16(read(pc))
	pc = pc + 1

	addr_abs = (hi << 8) | lo
	addr_abs += u16(x)

	if ((addr_abs & 0xFF00) != (hi << 8)) {
		return 1
	} else {
		return 0
	}
}

/* Address Mode: Absolute with Y Offset
	Same as ABX, but the Y register is added to the supplied two byte address */
ABY :: proc() -> u8 {
	lo: u16 = u16(read(pc))
	pc = pc + 1
	hi: u16 = u16(read(pc))
	pc = pc + 1

	addr_abs = (hi << 8) | lo
	addr_abs += u16(y)

	if ((addr_abs & 0xFF00) != (hi << 8)) {
		return 1
	} else {
		return 0
	}
}

/* Address Mode: Indirect
	The supplied 16-bit address is read to get the actual 16-bit address.
	This instruction is unusual in that it has a bug in the hardware!
	If the low byte of the supplied address is 0xFF, then we need to
	cross a page boundary to read the high byte of the actual address.
	Instead, on the 6502 as designed, it wraps back around in the same page,
	producing an invalid actual address */
IND :: proc() -> u8 {
	ptr_lo: u16 = u16(read(pc))
	pc = pc + 1
	ptr_hi: u16 = u16(read(pc))
	pc = pc + 1

	ptr: u16 = (ptr_hi << 8) | ptr_lo

	if (ptr_lo == 0x00FF) { // Simulate page boundary bug
		addr_abs = u16((read(ptr & 0xFF00) << 8) | read(ptr + 0))
	} else { // Behave normalls
		addr_abs = u16((read(ptr + 1) << 8) | read(ptr + 0))
	}

	return 0
}

/* Address Mode: Indirect X
	The supplied 8-bit address is offset by register X to index a location
	in page 0x00. The actual 16-bit address is read from this location */
IZX :: proc() -> u8 {
	t: u16 = u16(read(pc))
	pc = pc + 1

	lo: u16 = u16(read(t + u16(x) & 0x00FF))
	hi: u16 = u16(read(t + u16(x + 1) & 0x00FF))

	addr_abs = (hi << 8) | lo

	return 0
}

/* Address Mode: Indirect Y
	The supplied 8-bit address indexes a location in page 0x00. From here the
	actual 16-bit address is read, and the contents of register Y is added
	as an offset. If the offset causes a page change, an additional clock cycle
	is required */
IZY :: proc() -> u8 {
	t: u16 = u16(read(pc))
	pc = pc + 1

	lo: u16 = u16(read(t & 0x00FF))
	hi: u16 = u16(read((t + 1) & 0x00FF))

	addr_abs = (hi << 8) | lo
	addr_abs += u16(y)

	if ((addr_abs & 0xFF00) != (hi << 8)) {
		return 1
	} else {
		return 0
	}
}

// =-=-=-=-=	CPU INSTRUCTIONS	=-=-=-=-=