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
@ STARTUP CODE FOR LPC 2800 -- .data section
@
@	data section (boot) for LPC 2800
@		
@ .data section should be stored in FLASH starting at 0x10400000 (bottom of
@  FLASH) and should be "linked" to start at that same address.
@ It is where system starts execution -- set up clocks, MMU, copy .text code
@ to RAM and branch to RAM
@
@-------------------------------------------------------------------------------

	/* helper macro */
.macro	frcdst	adr, val
	ldr	rva, =\adr		@ rva <- address
	ldr	rvc, =\val		@ rvc <- value
	bl	frcdst
.endm
	
	/* pre-set common values */
	set	fre, 0
	set	sv1, 1
	set	sv2, 2
	set	sv3, 3
	set	sv4, 4
	set	sv5, 5
	set	cnt, 8

	/* setup Main PLL */
	write	sv1, 0x80004000, #0xce8	@ LPPDN  <- 0x01, power down Main PLL
	write	sv1, rva,        #0xce4	@ LPFIN  <- 0x01, 12MHz clk to Main PLL
  .ifndef native_usb			@ use 60 MHz Clock without USB, 16.67 ns
	write	sv4, rva,        #0xcf8	@ LPMSEL <- 0x04,Fclkout 60MHz=(4+1)*12
  .else					@ use 48 MHz Clock with USB,	20.83 ns
	write	sv3, rva,        #0xcf8	@ LPMSEL <- 0x03,Fclkout 48MHz=(3+1)*12
  .endif
	write	sv1, rva,        #0xcfc	@ LPPSEL <- CCO=2^2*Fclkout = 240/192MHz
	write	fre, rva,        #0xce8	@ LPPDN  <- 0x00, power up Main PLL 
	rgwfbt	rva, #0xcf0, 0, 1	@ wait for PLL locked

	/* select and spread clocks */
	@ Clock selection stage-- SYS, then UART
	bl	clksel			@ select clocks in SYSSCR
	add	rva,  rva, #0x1c	@ rva <- UARTSCR
	bl	clksel			@ select clocks in UARTSCR
	@ set up fractional dividers
	frcdst	0x800043fc, 0x00060206	@ DIV0 1/2 System clock = 30 MHz
	frcdst	0x80004400, 0x00058386	@ MCI clk of SD/MMC = 5/12 sys clk=25MHz
	frcdst	0x80004404, 0x00060086	@ USB clock = 4/5 system clock = 48 MHz
	frcdst	0x80004408, 0x00078486	@ LCD clock = 1/10 system clock = 6 MHz
	frcdst	0x8000440C, 0x00070406	@ USB clock = 1/5 system clock = 12 MHz
	@ clock spreading stage
	write	sv1,  0x80004000,#0x300	@ MCIESR0   <- 0x01 -- frac div 0, 30MHZ
	write	sv3,  rva,       #0x304	@ MCIESR1   <- 0x03 -- frac div 1, 25MHZ
	write	sv1,  rva,       #0x320	@ LCDESR0   <- 0x01 -- frac div 0, 30MHZ
	write	0x07, rva,       #0x324	@ LCDESR1   <- 0x07 -- frac div 3,  6MHZ
	write	sv1,  rva,       #0x328	@ DMAESR0   <- 0x01 -- frac div 0, 30MHZ
	write	sv1,  rva,       #0x32c	@ DMAESR1   <- 0x01 -- frac div 0, 30MHZ
	write	fre,  rva,       #0x330	@ USBESR0   <- 0x00 -- no divider, 48MHZ
	@ configure base control register
	write	sv1,  rva,       #0x3f0	@ SYSBCR <- 1, start fractional dividers

	/* Initialize EMC controller for FLASH and SDRAM */
	write	sv1,   0x80008000, #0x00 @ EMCCONTROL <- 1 -- Enable EMC
	write	0x100, 0x80005064, #0x00 @ EMCMISC <- no addrss bit shft on FLASH
	write	0x81,  0x80008000,#0x200 @ EMCSTATICCONFIG0 <- 16bit FLASH device
  .ifndef native_usb			@ use 60 MHz Clock without USB, 16.67 ns
	write	cnt, rva, #0x020C	@ EMCSTATICWAITRD0 <- 120 / 16.6 + 1 = 8
	write	fre, rva, #0x28		@ EMCDYNAMICREADCONFIG <- 1, Cmd delayed
	write	sv2, rva, #0x30		@ EMCDYNAMICRP   <- 20 / 16.6 + 1 = 2
	write	sv3, rva, #0x34		@ EMCDYNAMICRAS  <- 45 / 16.6 + 1 = 3
	write	sv5, rva, #0x38		@ EMCDYNAMICSREX <- 67 / 16.6 + 1 = 5
	write	sv1, rva, #0x3C		@ EMCDYNAMICAPR  <- SDRAM_TAPR = 1
	write	sv4, rva, #0x40		@ EMCDYNAMICDAL  <- 2 + 20/16.6 + 1 = 4
	write	sv3, rva, #0x44		@ EMCDYNAMICWR   <- SDRAM_TWR
	write	sv4, rva, #0x48		@ EMCDYNAMICRC   <- 65 / 16.6 + 1 = 4
	write	sv4, rva, #0x4C		@ EMCDYNAMICRFC  <- 66 / 16.6 + 1 = 4
	write	sv5, rva, #0x50		@ EMCDYNAMICXSR  <- 67 / 16.6 + 1 = 5
	write	sv1, rva, #0x54		@ EMCDYNAMICRRD  <- 15 / 16.6 + 1
	write	sv3, rva, #0x58		@ EMCDYNAMICMRD  <- SDRAM_TMRD
  .else					@ use 48 MHz Clock with USB, 20.83 ns
	write	  6, rva, #0x020C	@ EMCSTATICWAITRD0 <- 120 / 20.8 + 1 = 6
	write	fre, rva, #0x28		@ EMCDYNAMICREADCONFIG <- Cmd delayed
	write	sv1, rva, #0x30		@ EMCDYNAMICRP   <- 20 / 20.8 + 1 = 1
	write	sv3, rva, #0x34		@ EMCDYNAMICRAS  <- 45 / 20.8 + 1 = 3
	write	sv4, rva, #0x38		@ EMCDYNAMICSREX <- 67 / 20.8 + 1 = 4
	write	sv1, rva, #0x3C		@ EMCDYNAMICAPR  <- SDRAM_TAPR = 1
	write	sv3, rva, #0x40		@ EMCDYNAMICDAL  <- 2 + 20/20.8 + 1 = 3
	write	sv3, rva, #0x44		@ EMCDYNAMICWR   <- SDRAM_TWR
	write	sv4, rva, #0x48		@ EMCDYNAMICRC   <- 65 / 20.8 + 1 = 4
	write	sv4, rva, #0x4C		@ EMCDYNAMICRFC  <- 66 / 20.8 + 1 = 4
	write	sv4, rva, #0x50		@ EMCDYNAMICXSR  <- 67 / 20.8 + 1 = 4
	write	sv1, rva, #0x54		@ EMCDYNAMICRRD  <- 15 / 20.8 + 1 = 1
	write	sv3, rva, #0x58		@ EMCDYNAMICMRD  <- SDRAM_TMRD
  .endif
	write	0x1680, rva, #0x100	@ EMCDYNAMICCONFIG <- 13 row 9 col SDRAM
	rgrmw	rva, #0x104, 0x0202	@ EMCDYNAMICRASCAS <- 2 AHB HCLK cyc lat
	@ Initialize SDRAM chip (JEDEC)
	write	0x4183, rva, #0x20	@ EMCDYNAMICCONTROL <- 0x4183, NOP
	wait	0x080000		@ wait for stabilization
	rgcpbt	rva, #0x20, 7, 0	@ EMCDYNAMICCONTROL SDRAMINI prchrg all
	write	sv1,    rva, #0x24	@ EMCDYNAMICREFRESH <- 1
	wait	0x080000		@ wait for stabilization
  .ifndef native_usb			@ use 60 MHz Clock without USB, 16.67 ns
	write	58,    rva, #0x24	@ EMCDYNAMICREFRESH<-(15625/16.6+1)>>4
  .else					@ use 48 MHz Clock with USB, 20.83 ns
	write	46,    rva, #0x24	@ EMCDYNAMICREFRESH<-(15625/20.833+1)>>4
  .endif
	read	rvb,    rva, #0x20
	eor	rvb, rvb, #0x0180
	write	rvb,    rva, #0x20	@ EMCDYNAMICCONTROL SDRAMIN MODE command
	read16	rvb, 0x30023000, #0x00	@ RAM mode reg <- seq brst 8 words CAS-2
	write	0x4000,0x80008000,#0x20	@ EMCDYNAMICCONTROL <- NORMAL RPout ctrl
	rgcpbt	rva, #0x100, 19, 1	@ EMCDYNAMICCONFIG BUF_EN <- 1 enab bfrs

	/* configure and enable cache */
	write	sv1,  0x80104000, #0x04	@ CACHE_SETTINGS <- 1, reset cache
	write	fre,  rva,        #0x04	@ CACHE_SETTINGS <- 0, de-assert reset
	rgwfbt	rva, #0x00, 0, 0	@ wait for cache reset complete
	write	0xff, rva,        #0x08	@ CACHE_PAGE_CTRL <- 0xff cach all 16 pg
	set	rvb, 0x180		@ rvb <- initial targt addrss=0x30000000
	set	rvc, 0x18		@ rvc <- initial addrss trnslt tbl offst
cachni:	write	rvb, rva, rvc		@ store targt page in addrss trnslt tabl
	eq	rvc, #0x54		@ are we done?
	addne	rvb, rvb, #1		@	if not, rvb <- nxt target addrss
	addne	rvc, rvc, #4		@	if not, rvc <- nxt table  offset
	bne	cachni			@	if not, jump to init addrss tabl
	write	6,    rva,        #0x04	@ enable instruction and data caching

	/* copy scheme code to SDRAM */
	bl	codcpy

	/* jump to scheme */
	set	pc,  0x00

codcpy:	/* copy scheme to SDRAM address 0x00000000 */
	ldr	r8,  = _text_section_address_	@ start of source
	ldr	r9,  = _startcode	@ start of destin (from build_link file)
	ldr	r10, = _enddata		@ end of destination (from build_link file)
	add	r10, r10,  #4
codcp0:	ldmia	r8!, {r0-r7}
	stmia	r9!, {r0-r7}
	cmp	r9,  r10
	bmi	codcp0
	set	pc,  lnk

clksel:	/* perform clock selection */
	@ in:	fre, sv1-sv5, cnt <- values 0, 1 to 5, 8
	@ in:	rva <- base clock control register (eg. SYSSCR)
	@ mods:	rvb, rvc
	read	rvb, rva, #0		@ r7  <- content of SYSSCR
	and	rvb, rvb, #0x0C		@ r7  <- SYSSCR w/clrd side select bits
	read	rvc, rva, #0x084	@ r14 <- content of SYSSSR
	tst	rvc, #1			@ is side 1 selected?
	writeeq	cnt, rva, #0x02C	@ 	if not, SYSFSR1 <- 0x08,Main PLL
	orreq	rvb, rvb, sv1		@	if not, r6 <- SYSFSR1 (side 1)
	writene	cnt, rva, #0x058	@	if so,  SYSFSR2 <- 0x08,Main PLL
	orrne	rvb, rvb, sv2		@	if so,  r6 <- SYSFSR2 (side 2)
	write	rvb, rva, #0		@ select appropriate side
	set	pc,  lnk

frcdst:	/* fractional divisor setup */
	@ on entry:	rva <- fractional divisor setup register, eg. SYSFDCR1
	@ on entry:	rvc <- MSUB=-n, MADD=m-n, stretch, reset parameters
	@ modifies:	rvb
	rgcpbt	rva, #0x00, 0, 0	@ SYSFDCR1 <- stop fractional divider
	write	rvc, rva, #0x00		@ set MSUB, MADD, stretch, reset cfg
	rgcpbt	rva, #0x00, 1, 0	@ clear reset bit
	rgcpbt	rva, #0x00, 0, 1	@ restart the fractional divider
	set	pc,  lnk



