/*------------------------------------------------------------------------------
@
@  ARMPIT SCHEME Version 070
@
@  ARMPIT SCHEME is distributed under The MIT License.

@  Copyright (c) 2006-2014 Hubert Montas

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
@-----------------------------------------------------------------------------*/


	@ disable watchdog
	write	0, 0x53000000, #0

	@ disable interrupts (via mask)
	write	-1,     int_base, #0x08	@ disable ints in INTMSK
	write	0x07ff, rva,      #0x1c	@ disable sub-ints in INTSUBMSK

	@ configure PLL
	write	3, 0x4c000000, #0x14	@ Hclk = Fclk/2, Pclk = Hclk/2
  .ifdef native_usb
	write	0x078023, rva, #0x08	@ set UPLL to 48MHz (for USB)
	wait	8
  .endif @ native_usb
	write	PLL_PM_parms, rva, #0x04

	@ use CPS15 to switch from Fast Bus Mode to Sync/Async Mode
	mrc	p15, 0, rvb, c1, c0, 0
	orr	rvb, rvb, #0xC0000000	@ set clock mode to asynchronous
	mcr	p15, 0, rvb, c1, c0, 0

	@ configure external memory
	@ BANK 0, (autoconf) intel flash 28F128J3-D75 (same as EP9302) (16MB)
	@ Bank 6,7, SDRAM, Micron MT48LC16M16A2, BG-75, 4M x 16 X 4 banks (32MB)
	write	0x11000000,0x48000000,#0 @ Bank 6,7 SDRAM dat bus wdt=16b,nowait
	write	0x018001, rva, #0x1c	@ Bank 6,SDRAM,9bit col adrs,2clkCAS>RAS
	write	rvb,      rva, #0x20	@ Bank 7,SDRAM,9bit col adrs
	write	0x8404e9, rva, #0x24	@ SDRAM precharge (originally: 0x9c0459)
	write	0xb2,     rva, #0x28	@ Burst enable, 128/128MB mem map (p208)
	write	0x20,     rva, #0x2c	@ Bank 6, CL = 2 (originally CL3, 0x30)
	write	rvb,      rva, #0x30	@ Bank 7, CL = 2

	@ copy scheme code to SDRAM
	bl	codcpy

	@ initialize TTB (Translat Table Base) Dflt Mem space, not cach, not bfr
	ldr	rvc, =0x0C12		@ rvc <- r/w perm,dom 0,not cach/bfr,1MB
	ldr	rva, =TTB_address	@ rva <- address of start of TTB
	set	rvb, rvc		@ rvb <- section 0 descriptor
ttbst0:	str	rvb, [rva, rvb, LSR #18] @ store section desc in Translation Tbl
	add	rvb, rvb, #(1<<20)
	eq	rvb, rvc
	bne	ttbst0
	@ continue initializing TTB, for Scheme core SDRAM (cacheable, buffered)
	ldr	rvc, =0x0C1E		 @ rvc <- r/w perm,domain 0,cach/bfr,1MB
	str	rvc, [rva, rvc, LSR #18] @ store section desc in Translation Tbl
	orr	rvb, rvc, #RAMBOTTOM
ttbst1:	str	rvb, [rva, rvb, LSR #18] @ store section desc in Translation Tbl
	add	rvb, rvb, #(1<<20)
	tst	rvb, #(1<<25)
	beq	ttbst1

	@ remap RAMBOTTOM to 0x00 (scheme code r/w frm 0x00 and #RAMBOTTOM)
	orr	rvb, rvc, #RAMBOTTOM
	str	rvb, [rva]		@ store section desc in Translation Tbl

	@ use coprocessor 15 to set domain access control, TTB base, enable MMU
	set	rvb, 0x01		@ rvb <- domain 0 client access perms
	mcr	p15, 0, rvb, c3, c0, 0	@ set domain access into CP15 reg 3
	mcr	p15, 0, rva, c2, c0, 0	@ set TTB base address into CP15 reg 2
	mrc	p15, 0, rvb, c1, c0, 0	@ rvb <- contents of ctrl reg CP15 reg 1
	orr	rvb, rvb, #0x5000	@ rvb <- contents ord w/Icache,rnd-robin
	orr	rvb, rvb, #0x0005	@ rvb <- contents ord w/Dcache,MMU enab

	@ jump to non-remapped SDRAM to enable MMU
	ldr	rvc, =enbttb
	set	rva, RAMBOTTOM
	orr	rva, rva, #(1<<20)
	ldmia	rvc, {fre,cnt,sv1-sv5}
	stmia	rva, {fre,cnt,sv1-sv5}
	set	pc,  rva		@ jump to copied cache/MMU init
	
enbttb:	@ code copied to SDRAM to execute from non-remapped space
	@ as cache/MMU is enabled
	mcr	p15, 0, rvb, c1, c0, 0	@ set cache/MMU enable into CP15 reg 1
	nop
	nop
	nop
	nop
	nop
	@ jump to remainder of initialization
	set	pc, 0x00

codcpy:	@ copy scheme to SDRAM address 0x30000000 (RAMBOTTOM)
	ldr	sv5, =_text_section_address_	@ start of source
	ldr	env, =_startcode		@ start of dest, build_link file
	ldr	dts, =_enddata			@ end   of dest, build_link file
	orr	env, env, #RAMBOTTOM
	orr	dts, dts, #RAMBOTTOM
	add	dts, dts,  #4
codcp0:	ldmia	sv5!, {fre-sv4}
	stmia	env!, {fre-sv4}
	cmp	env, dts
	bmi	codcp0
	set	pc,  lnk



