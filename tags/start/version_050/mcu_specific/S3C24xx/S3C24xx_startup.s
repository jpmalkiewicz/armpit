@---------------------------------------------------------------------------------------------------
@
@  ARMPIT SCHEME Version 050
@
@  ARMPIT SCHEME is distributed under The MIT License.

@  Copyright (c) 2006-2012 Hubert Montas

@ Permission is hereby granted, free of charge, to any person obtaining
@ a copy of this software and associated documentation files (the "Software"),
@ to deal in the Software without restriction, including without limitation
@ the rights to use, copy, modify, merge, publish, distribute, sublicense,
@ and/or sell copies of the Software, and to permit persons to whom the
@ Software is furnished to do so, subject to the following conditions:
@
@ The above copyright notice and this permission notice shall be included
@ in all copies or substantial portions of the Software.
@
@ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
@ OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
@ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
@ THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
@ OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
@ ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
@ OTHER DEALINGS IN THE SOFTWARE.
@
@---------------------------------------------------------------------------------------------------


	@ disable watchdog
	ldr	rva, =0x53000000
	set	rvb, #0x00
	str	rvb, [rva]
	@ disable interrupts (via mask)
	ldr	rva, =int_base
	ldr	rvb, =0xffffffff
	str	rvb, [rva, #0x08]		@ disable ints in INTMSK
	ldr	rvb, =0x07ff
	str	rvb, [rva, #0x1c]		@ disable sub-ints in INTSUBMSK
	@ configure PLL
	ldr	rva, =0x4c000000
	set	rvb, #0x03		@ rvb <- 3, Hclk = Fclk/2, Pclk = Hclk/2
	str	rvb, [rva, #0x14]

.ifdef	native_usb
	ldr	rvb, =0x078023
	str	rvb, [rva, #0x08]	@ set UPLL to 48MHz (for USB)
	nop
	nop
	nop
	nop
	nop
	nop
	nop
.endif	@ native_usb
	
	ldr	rvb, =PLL_PM_parms
	str	rvb, [rva, #0x04]
	@ use CPS15 to switch from Fast Bus Mode to Sync/Async Mode
	mrc	p15, 0, rvb, c1, c0, 0
	orr	rvb, rvb, #0xC0000000	@ set clock mode to asynchronous
	mcr	p15, 0, rvb, c1, c0, 0
	@ configure external memory
	@ BANK 0, (autoconf) intel flash 28F128J3-D75 (same as EP9302) (16MB)
	@ Bank 6,7, SDRAM, Micron MT48LC16M16A2, BG-75, 4M x 16 X 4 banks (32MB)
	ldr	rva, =0x48000000
	ldr	rvb, =0x11000000
	str	rvb, [rva]		@ Bank 6,7 (SDRAM) data bus width = 16 bits, no wait
	ldr	rvb, =0x018001
	str	rvb, [rva, #0x1c]	@ Bank 6, SDRAM, 9 bit col address, 2clk CAS->RAS (orig. 0x018005)
	str	rvb, [rva, #0x20]	@ Bank 7, SDRAM, 9 bit col address
	ldr	rvb, =0x8404e9
	str	rvb, [rva, #0x24]	@ SDRAM precharge (originally:	0x9c0459)
	set	rvb, #0xb2
	str	rvb, [rva, #0x28]	@ Burst enable, 128MB/128MB memory map (p. 208)
	set	rvb, #0x20
	str	rvb, [rva, #0x2c]	@ Bank 6, CL = 2 (originally CL3, 0x30)
	str	rvb, [rva, #0x30]	@ Bank 7, CL = 2
	@ copy scheme code to SDRAM
	bl	codcpy
	@ initialize TTB (Translation Table Base) for Default Memory space (not cacheable, not buffered)
	ldr	rvc, =0x0C12		@ rvc <- r/w permitted, domain 0, not cacheable/buffered, 1MB sect.
	set	rva, #RAMBOTTOM
	orr	rva, rva, #0x010000	@ rva <- address of start of TTB (64kb into SDRAM)
	set	rvb, rvc		@ rvb <- section 0 descriptor
ttbst0:	str	rvb, [rva, rvb, LSR #18] @ store section descriptor in Translation Table
	add	rvb, rvb, #0x00100000
	eq	rvb, rvc
	bne	ttbst0
	@ continue initializing TTB, for Scheme core and SDRAM (cacheable, buffered)
	ldr	rvc, =0x0C1E		@ rvc <- r/w permitted, domain 0, cacheable/buffered, 1MB sect.
	set	rvb, rvc		@ rvb <- section 0 descriptor
	str	rvb, [rva, rvb, LSR #18] @ store section descriptor in Translation Table
	orr	rvb, rvb, #RAMBOTTOM
ttbst1:	str	rvb, [rva, rvb, LSR #18] @ store section descriptor in Translation Table
	add	rvb, rvb, #0x00100000
	tst	rvb, #0x02000000
	beq	ttbst1
	@ re-map RAMBOTTOM to 0x00 (for scheme code - read-only from 0x00 -- read/write from #RAMBOTTOM)
	set	rvb, rvc
	orr	rvb, rvb, #RAMBOTTOM
	str	rvb, [rva]		@ store section descriptor in Translation Table
	@ use coprocessor 15 to set domain access control, TTB base and enable MMU
	set	rvb, #0x01		@ rvb <- domain 0 uses client access perms (A & P bits checked)
	mcr	p15, 0, rvb, c3, c0, 0	@ set domain access into CP15 register 3
	mcr	p15, 0, rva, c2, c0, 0	@ set TTB base address into CP15 register 2
	mrc	p15, 0, rvb, c1, c0, 0	@ rvb <- contents of control register (CP15 reg. 1)
	orr	rvb, rvb, #0x5000	@ rvb <- contents orred with Icache enable, round-robin
	orr	rvb, rvb, #0x0005	@ rvb <- contents orred with Dcache and MMU enable
	@ jump to non-remapped SDRAM to enable MMU
	ldr	rvc, =enbttb
	set	rva, #RAMBOTTOM
	orr	rva, rva, #0x00100000
	ldmia	rvc, {fre,cnt,sv1-sv5}
	stmia	rva, {fre,cnt,sv1-sv5}
	set	pc,  rva		@ jump to copied cache/MMU initialization
	
enbttb:	@ code copied to SDRAM to execute from non-remapped space
	@ as cache/MMU is enabled
	mcr	p15, 0, rvb, c1, c0, 0	@ set cache/MMU enbale into CP15 register 1
	nop
	nop
	nop
	nop
	nop
	@ jump to remainder of initialization
	set	pc, #0x00

codcpy:	@ copy scheme to SDRAM address 0x30000000 (RAMBOTTOM)
	ldr	r8,  = _text_section_address_	@ start of source
	ldr	r9,  = _startcode	@ start of destination (from build_link file)
	ldr	r10, = _endcode		@ end of destination (from build_link file)
	orr	r9,  r9,  #RAMBOTTOM
	orr	r10, r10, #RAMBOTTOM
	add	r10, r10,  #4
codcp0:	ldmia	r8!, {r0-r7}
	stmia	r9!, {r0-r7}
	cmp	r9,  r10
	bmi	codcp0
	set	pc,  lnk

