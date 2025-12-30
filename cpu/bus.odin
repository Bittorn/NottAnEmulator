package main

// Dummy RAM for now
ram : [64 * 1024]u8

// Reads an 8-bit byte from the bus, located at the specified 16-bit address
read :: proc(addr: u16, read_only: bool = true) -> u8 {
	if (addr >= 0x0000 && addr <= 0xFFFF) {
		return ram[addr]
	}

	return 0x00
}

// Writes a byte to the bus at the specified address
write :: proc(addr: u16, data: u8) {
	if (addr >= 0x0000 && addr <= 0xFFFF) {
		ram[addr] = data
    }
}