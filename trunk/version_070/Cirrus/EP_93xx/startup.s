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

@-------------------------------------------------------------------------------
@		
@  STARTUP CODE FOR EP93xx
@
@	boot section, starts with "CRUS" at 0x60000000 for EP9302 Boot Process
@		
@-------------------------------------------------------------------------------

.ascii	"CRUS"
	@ clear watchdog and 64Hz timer interrupts
	write	0,            0x80930000, #0x18	@ TINT clear
	write	rvb,          rva,        #0x1c	@ Watchdog clear
	write16	0xAA55,       0x80940000, #0x00	@ disable watchdog

	@ configure external memory:	 FLASH
	write	0x100014a2,   0x80080018, #0x00	@ SMCBCR[6]

	@ configure clocks/plls
	write	PLL_PM_parms, 0x80930000, #0x20	@ ClkSet1	
	nop					@ 5 nops after ClkSet1 (ref.man)
	nop
	nop
	nop
	nop
	write	0x300dc317,   rva,        #0x24	@ ClkSet2

	@ use CPS15 to switch from Fast Bus Mode to Sync/Async Mode
	mrc	p15, 0, rvb, c1, c0, 0
	orr	rvb,  rvb, #0xC0000000	@ set clock mode to asynchronous
	mcr	p15, 0, rvb, c1, c0, 0

	@ cfg ext SDRAM: RASCAS2,brst4,cas2,SROMLL=1(AD12,13-Ba0,1),4bnk,16bit
	wait	1<<15				@ wait for stabilization
	write	0x0021002C, 0x80060000, #0x1c	@ SDRAMDevCfg[3] <- config
	wait	1<<15			@ wait for stabilization
	write	0x80000003, rva,        #0x04	@ GIConfig <- NOP, MRS=1,Init=1
	wait	1<<15				@ wait for stabilization
	write	0x80000000, rva,        #0x04	@ GIConfig <- ena clk, fre run
	write	0x00000000, 0x00000000, #0x00	@ wrt to bank 0=prech all:errata
	write	0x00200000, 0x00200000, #0x00	@ wrt to bank 1=prech all:errata
	write	0x00400000, 0x00400000, #0x00	@ wrt to bank 2=prech all:errata
	write	0x00600000, 0x00600000, #0x00	@ wrt to bank 3=prech all:errata
	write	0x80000001, 0x80060000, #0x04	@ GIConfig <- prchal,MRS=0,Ini=1
	write	0x0A,       rva,        #0x08	@ RefrshTimr <- 10 clock cycles
	wait	1<<15				@ wait for stabilization
	write	0x0208,     rva,        #0x08	@ SDRAM RefrshTimr <- 516 (8 ms)
	write	0x80000002, 0x80060000, #0x04	@ GIConfig <- mod ac,MRS=1,Ini=0
	read	rvb, 0x4600, #0x00		@ set WBM=0,TM=0,CAS2,SEQ,BL=8
	write	0x80000000, 0x80060000, #0x04	@ GIConfig <- nrml,en ck,fre run

	@ initialize TTB (Translat Table Base) Dflt Mem space, not cach, not bfr
	set	rvc, 0x0C12		@ rvc <- r/w perm,dom 0,not cach/bfr,1MB
	set	rva, TTB_address	@ rva <- address of start of TTB
	set	rvb, rvc		@ rvb <- section 0 descriptor
ttbst0:	str	rvb, [rva, rvb, LSR #18] @ store section desc in Translation Tbl
	add	rvb, rvb, #(1<<20)
	eq	rvb, rvc
	bne	ttbst0

	@ continue initializing TTB, for SDRAM (cacheable, buffered)
	set	sv1, rva		@ sv1 <- address of start of TTB
	ldr	sv2, =0x0C1E		@ sv2 <- r/w perm,domain 0,cach/bfr,1MB
	add	sv3, sv2, #(1<<24)
	add	sv4, sv2, #(4<<24)
	add	sv5, sv4, #(1<<24)
ttbst1:	write	sv2, sv1, #0		@ store section desc in Translation Tbl
	write	sv3, sv1, #32		@ store section desc in Translation Tbl
	write	sv4, sv1, #64		@ store section desc in Translation Tbl
	write	sv5, sv1, #96		@ store section desc in Translation Tbl
	add	sv1, sv1, #4
	add	sv2,  sv2, #(1<<20)
	add	sv3,  sv3, #(1<<20)
	add	sv4,  sv4, #(1<<20)
	add	sv5,  sv5, #(1<<20)
	tst	sv3,  #(1<<23)
	beq	ttbst1

	@ use coprocessor 15 to set domain access control, TTB base and enable MMU
	set	rvb, 0x01		@ rvb <- domain 0 client access perms
	mcr	p15, 0, rvb, c3, c0, 0	@ set domain access into CP15 reg 3
	wait	1<<15			@ wait a bit
	mcr	p15, 0, rva, c2, c0, 0	@ set TTB base address into CP15 reg 2
	wait	1<<15			@ wait a bit
	mrc	p15, 0, rvb, c1, c0, 0	@ rvb <- contents of ctrl reg CP15 reg 1
	orr	rvb, rvb, #0x5000	@ rvb <- contents ord w/Icache,rnd-robin
	orr	rvb, rvb, #0x0005	@ rvb <- contents ord w/Dcache,MMU enab
	mcr	p15, 0, rvb, c1, c0, 0	@ set cache/MMU enbale into CP15 reg 1
	wait	1<<15			@ wait a bit

	@ copy ARMPIT Scheme code to SDRAM
	bl	codcpy

	@ jump to start running Scheme code
	set	pc,  0x00

codcpy:	@ copy scheme to SDRAM address 0x00000000
	ldr	sv5, =_text_section_address_	@ start of source
	ldr	env, =_startcode		@ start of dest, build_link file
	ldr	dts, =_enddata			@ end   of dest, build_link file
	add	dts, dts,  #4
codcp0:	ldmia	sv5!, {fre-sv4}
	stmia	env!, {fre-sv4}
	cmp	env, dts
	bmi	codcp0
	set	pc,  lnk



