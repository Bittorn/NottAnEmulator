package main

import "core:fmt"

nott_version : string : "v0.0.1"

main :: proc() {
	fmt.println("NottAnEmulator", nott_version) // NOTE: auto-places spaces between args
}