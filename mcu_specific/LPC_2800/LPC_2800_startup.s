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

@---------------------------------------------------------------------------------------
@		
@ STARTUP CODE FOR LPC 2800 -- .data section
@
@	data section (boot) for LPC 2800
@		
@ .data section should be stored in FLASH starting at 0x10400000 (bottom of FLASH)
@ and should be "linked" to start at that same address.
@ It is where system starts execution -- set up clocks, MMU, copy .text code to RAM
@ branch to RAM
@---------------------------------------------------------------------------------------
	
	@ pre-set common values
	set	r0,  #0
	set	r1,  #1
	set	r2,  #2
	set	r3,  #3
	set	r4,  #4
	set	r5,  #5
	set	r8,  #8
	@ setup Main PLL
	ldr	r6,  =0x80004000	@ r6  <- address of SYSSCR (base)
	str	r1,  [r6, #0xce8]	@ LPPDN  <- 0x01, power down Main PLL 
	str	r1,  [r6, #0xce4]	@ LPFIN  <- 0x01, select 12MHz clock as input to Main PLL
.ifndef native_usb
	@ use 60 MHz Clock without USB	(16.67 ns)
	str	r4,  [r6, #0xcf8]	@ LPMSEL <- 0x04, Fclkout is 60MHz = (4 + 1)* 12MHz
.else
	@ use 48 MHz Clock with USB	(20.83 ns)
	str	r3,  [r6, #0xcf8]	@ LPMSEL <- 0x03, Fclkout is 48MHz = (3 + 1)* 12MHz
.endif	
	str	r1,  [r6, #0xcfc]	@ LPPSEL <- 0x01, CCO = 2^2 * Fclkout = 240MHz or 192MHz
	str	r0,  [r6, #0xce8]	@ LPPDN  <- 0x00, power up Main PLL 
pllwt0:	ldr	r7,  [r6, #0xcf0]	@ r7 <- LPLOCK, read PLL status
	tst	r7,  r1			@ is PLL locked?
	beq	pllwt0			@	if not, jump to keep waiting
	@ Clock selection stage-- SYS
	ldr	r7,  [r6]		@ r7  <- content of SYSSCR
	and	r7,  r7, #0x0C		@ r7  <- SYSSCR with cleared side selection bits
	ldr	r14, [r6, #0x084]	@ r14 <- content of SYSSSR
	tst	r14, r1			@ is side 1 selected?
	streq	r8,  [r6, #0x02C]	@ 	if not, SYSFSR1 <- 0x08, select Main PLL as clock source
	orreq	r7,  r7, r1		@	if not, r6 <- SYSFSR1 (to select side 1)
	strne	r8,  [r6, #0x058]	@	if so,  SYSFSR2 <- 0x08, select Main PLL as clock source
	orrne	r7,  r7, r2		@	if so,  r6 <- SYSFSR2 (to select side 2)
	str	r7,  [r6]		@ select appropriate side
	@ Clock selection stage-- UART
	ldr	r7,  [r6, #0x01c]	@ r7  <- content of UARTSCR
	and	r7,  r7, #0x0C		@ r7  <- UARTSCR with cleared side selection bits
	ldr	r14, [r6, #0x0a0]	@ r14 <- content of UARTSSR
	tst	r14, r1			@ is side 1 selected?
	streq	r8,  [r6, #0x048]	@	if not, UARTFSR1 <- 0x08, select Main PLL as clock source
	orreq	r7,  r7, r1		@	if not, r6 <- UARTFSR1
	strne	r8,  [r6, #0x074]	@	if so,  UARTFSR2 <- 0x08, select Main PLL as clock source
	orrne	r7,  r7, r2		@	if so,  r6 <- UARTFSR2
	str	r7,  [r6, #0x01c]	@ select appropriate side
	@ fractional dividers setup: 0 -- 1/2 System clock = 30 MHz
	ldr	r7,  [r6, #0x3fc]	@ r7 <- content of SYSFDCR0
	bic	r7,  r7, r1		@ r7 <- SYSFDCR0 with run bit cleared
	str	r7,  [r6, #0x3fc]	@ SYSFDCR0 <- stop the fractional divider
	ldr	r7,  =0x00060206	@ MSUB=-n=-0x40, MADD=m-n=0x40, stretch, reset
	str	r7,  [r6, #0x3fc]	@ set configuration
	ldr	r7,  [r6, #0x3fc]	@ r7 <- content of SYSFDCR0
	bic	r7,  r7, r2		@ r7 <- SYSFDCR0 with reset bit cleared
	str	r7,  [r6, #0x3fc]	@ clear reset bit
	ldr	r7,  [r6, #0x3fc]	@ r7 <- content of SYSFDCR0
	orr	r7,  r7, r1		@ r7 <- SYSFDCR0 with run bit
	str	r7,  [r6, #0x3fc]	@ restart the fractional divider
	@ fractional dividers setup: 1 -- MCI clock of SD/MMC = 5/12 system clock = 25 MHz
	ldr	r6,  =0x80004400	@ r6 <- SYSFDCR1
	ldr	r7,  [r6]
	bic	r7,  r7, #0x01
	str	r7,  [r6]		@ SYSFDCR1 <- stop the fractional divider
	ldr	r7,  =0x00058386	@ MSUB=-n, MADD=m-n, stretch, reset
	str	r7,  [r6]		@ set configuration
	ldr	r7,  [r6]
	bic	r7,  r7, #0x02
	str	r7,  [r6]		@ clear reset bit
	ldr	r7,  [r6]
	orr	r7,  r7, #0x01
	str	r7,  [r6]		@ restart the fractional divider
	@ fractional dividers setup: 2 -- USB clock = 4/5 system clock = 48 MHz
	ldr	r6,  =0x80004404	@ r6 <- SYSFDCR2
	ldr	r7,  [r6]
	bic	r7,  r7, #0x01
	str	r7,  [r6]		@ SYSFDCR2 <- stop the fractional divider
	ldr	r7,  =0x00060086	@ MSUB=-n=-64, MADD=m-n=16, stretch, reset
	str	r7,  [r6]		@ set configuration
	ldr	r7,  [r6]
	bic	r7,  r7, #0x02
	str	r7,  [r6]		@ clear reset bit
	ldr	r7,  [r6]
	orr	r7,  r7, #0x01
	str	r7,  [r6]		@ restart the fractional divider
	@ fractional dividers setup: 3 -- LCD clock = 1/10 system clock = 6 MHz
	ldr	r6,  =0x80004408	@ r6 <- SYSFDCR3
	ldr	r7,  [r6]
	bic	r7,  r7, #0x01
	str	r7,  [r6]		@ SYSFDCR3 <- stop the fractional divider
	ldr	r7,  =0x00078486	@ MSUB=-n, MADD=m-n, stretch, reset
	str	r7,  [r6]		@ set configuration
	ldr	r7,  [r6]
	bic	r7,  r7, #0x02
	str	r7,  [r6]		@ clear reset bit
	ldr	r7,  [r6]
	orr	r7,  r7, #0x01
	str	r7,  [r6]		@ restart the fractional divider
	@ fractional dividers setup: 4 -- USB clock = 1/5 system clock = 12 MHz
	ldr	r6,  =0x8000440C	@ r6 <- SYSFDCR4
	ldr	r7,  [r6]
	bic	r7,  r7, #0x01
	str	r7,  [r6]		@ SYSFDCR4 <- stop the fractional divider
	ldr	r7,  =0x00070406	@ MSUB=-n=-32, MADD=m-n=128, stretch, reset
	str	r7,  [r6]		@ set configuration
	ldr	r7,  [r6]
	bic	r7,  r7, #0x02
	str	r7,  [r6]		@ clear reset bit
	ldr	r7,  [r6]
	orr	r7,  r7, #0x01
	str	r7,  [r6]		@ restart the fractional divider
	@ Clock spreading stage
	ldr	r6,  =0x80004000	@ r6  <- address of SYSSCR (base)
	str	r1,  [r6, #0x300]	@ MCIESR0   <- 0x01 -- fractional divider 0, 30 MHZ
	str	r3,  [r6, #0x304]	@ MCIESR1   <- 0x03 -- fractional divider 1, 25 MHZ
	str	r1,  [r6, #0x320]	@ LCDESR0   <- 0x01 -- fractional divider 0, 30 MHZ
	set	r8,  #0x07		@ r8  <- 7
	str	r8,  [r6, #0x324]	@ LCDESR1   <- 0x07 -- fractional divider 3,  6 MHZ
	str	r1,  [r6, #0x328]	@ DMAESR0   <- 0x01 -- fractional divider 0, 30 MHZ
	str	r1,  [r6, #0x32c]	@ DMAESR1   <- 0x01 -- fractional divider 0, 30 MHZ
	str	r0,  [r6, #0x330]	@ USBESR0   <- 0x00 -- no divider, 48 MHZ
	@ power control configuration (nil)
	@ base control register configuration
	str	r1,  [r6, #0x3f0]	@ SYSBCR <- 1, start fractional dividers
	@ Initialize EMC controller for FLASH and SDRAM
	ldr	r6,  =0x80008000	@ r6  <- EMC Control
	str	r1,  [r6]		@ EMCCONTROL <- 1 -- Enable EMC
	ldr	r8,  =0x80005064	@ r6  <- EMC Misc
	set	r7,  #0x0100
	str	r7,  [r8]		@ EMCMISC <- 0x0100, no address bit shift on FLASH (Static Memory)
	set	r7,  #0x81
	str	r7,  [r6, #0x0200]	@ EMCSTATICCONFIG0 <- 0x240, 16-bit FLASH device
	ldr	r8,  =0x020C
.ifndef native_usb
	@ use 60 MHz Clock without USB	(16.67 ns)
	set	r7,  #8
	str	r7,  [r6, r8]		@ EMCSTATICWAITRD0 <- 120 / 16.6 + 1 = 8, FLASH read access delay
	str	r0,  [r6, #0x28]	@ EMCDYNAMICREADCONFIG <- 1, Cmd delayed, using AHBHCLKDELAY *** vs 0
	str	r2,  [r6, #0x30]	@ EMCDYNAMICRP   <- 20 / 16.6 + 1 = 2
	str	r3,  [r6, #0x34]	@ EMCDYNAMICRAS  <- 45 / 16.6 + 1 = 3
	str	r5,  [r6, #0x38]	@ EMCDYNAMICSREX <- 67 / 16.6 + 1 = 5
	str	r1,  [r6, #0x3C]	@ EMCDYNAMICAPR  <- SDRAM_TAPR = 1
	str	r4,  [r6, #0x40]	@ EMCDYNAMICDAL  <- 2 + 20/16.6 + 1 = 4
	str	r3,  [r6, #0x44]	@ EMCDYNAMICWR   <- SDRAM_TWR
	str	r4,  [r6, #0x48]	@ EMCDYNAMICRC   <- 65 / 16.6 + 1 = 4
	str	r4,  [r6, #0x4C]	@ EMCDYNAMICRFC  <- 66 / 16.6 + 1 = 4
	str	r5,  [r6, #0x50]	@ EMCDYNAMICXSR  <- 67 / 16.6 + 1 = 5
	str	r1,  [r6, #0x54]	@ EMCDYNAMICRRD  <- 15 / 16.6 + 1
	str	r3,  [r6, #0x58]	@ EMCDYNAMICMRD  <- SDRAM_TMRD
.else
	@ use 48 MHz Clock with USB	(20.83 ns)
	set	r7,  #6
	str	r7,  [r6, r8]		@ EMCSTATICWAITRD0 <- 120 / 20.8 + 1 = 6, FLASH read access delay
	str	r0,  [r6, #0x28]	@ EMCDYNAMICREADCONFIG <- 1, Cmd delayed, using AHBHCLKDELAY *** vs 0
	str	r1,  [r6, #0x30]	@ EMCDYNAMICRP   <- 20 / 20.8 + 1 = 1
	str	r3,  [r6, #0x34]	@ EMCDYNAMICRAS  <- 45 / 20.8 + 1 = 3
	str	r4,  [r6, #0x38]	@ EMCDYNAMICSREX <- 67 / 20.8 + 1 = 4
	str	r1,  [r6, #0x3C]	@ EMCDYNAMICAPR  <- SDRAM_TAPR = 1
	str	r3,  [r6, #0x40]	@ EMCDYNAMICDAL  <- 2 + 20/20.8 + 1 = 3
	str	r3,  [r6, #0x44]	@ EMCDYNAMICWR   <- SDRAM_TWR
	str	r4,  [r6, #0x48]	@ EMCDYNAMICRC   <- 65 / 20.8 + 1 = 4
	str	r4,  [r6, #0x4C]	@ EMCDYNAMICRFC  <- 66 / 20.8 + 1 = 4
	str	r4,  [r6, #0x50]	@ EMCDYNAMICXSR  <- 67 / 20.8 + 1 = 4
	str	r1,  [r6, #0x54]	@ EMCDYNAMICRRD  <- 15 / 20.8 + 1 = 1
	str	r3,  [r6, #0x58]	@ EMCDYNAMICMRD  <- SDRAM_TMRD
.endif
	ldr	r7,  =0x1680
	str	r7,  [r6, #0x100]	@ EMCDYNAMICCONFIG <- 0x0000680, 13 row, 9 - col, SDRAM
	ldr	r7,  [r6, #0x0104]
	orr	r7,  r7, #0x0200
	orr	r7,  r7, #0x0002
	str	r7,  [r6, #0x0104]	@ EMCDYNAMICRASCAS <- 2, 2,  bit 9:8, 1:0<-10 -- 2 AHB HCLK cycles lat
	@ Initialize SDRAM chip (JEDEC)
	ldr	r7,  =0x4183
	str	r7,  [r6, #0x20]	@ EMCDYNAMICCONTROL <- 0x4183, bits 8:7<-11, NOP
	bl	waitld			@ wait for stabilization
	ldr	r7,  [r6, #0x20]
	bic	r7,  r7, #0x0080
	str	r7,  [r6, #0x20]	@ EMCDYNAMICCONTROL, SDRAMINI <- 2, bits 8:7<-10, PALL (prchrge all)
	str	r1,  [r6, #0x24]	@ EMCDYNAMICREFRESH <- 1
	bl	waitld			@ wait for stabilization
.ifndef native_usb
	@ use 60 MHz Clock without USB	(16.67 ns)
	ldr	r7,  =58
	str	r7,  [r6, #0x24]	@ EMCDYNAMICREFRESH <-  (15625 / 16.6 + 1) >> 4
	ldr	r7,  [r6, #0x20]
.else
	@ use 48 MHz Clock with USB	(20.83 ns)	
	ldr	r7,  =46
	str	r7,  [r6, #0x24]	@ EMCDYNAMICREFRESH <-  (15625 / 20.833 + 1) >> 4
	ldr	r7,  [r6, #0x20]
.endif
	eor	r7,  r7, #0x0180
	str	r7,  [r6, #0x20]	@ EMCDYNAMICCONTROL, SDRAMINI <- 1, bits 8:7<-01, MODE command
	ldr	r8,  =0x30023000	@ r8  <- RAm Code for sequential burst of 8 words, CAS-2
	ldrh	r8,  [r8]		@ set mode into RAM mode register
	ldr	r7,  =0x4000
	str	r7,  [r6, #0x20]	@ EMCDYNAMICCONTROL <- 0x4000, NORMAL mode, with RPout ctrl for FLASH
	ldr	r7,  [r6, #0x100]
	orr	r7,  r7, #0x080000
	str	r7,  [r6, #0x100]	@ EMCDYNAMICCONFIG, BUF_EN <- 1, bit 19, enable r/w buffers
	@ configure and enable cache
	ldr	r6,  =0x80104000	@ r6  <- CACHE_RST_START
	str	r1,  [r6, #4]		@ CACHE_SETTINGS <- 1, reset cache
	str	r0,  [r6, #4]		@ CACHE_SETTINGS <- 0, de-assert reset
cachwt:	ldr	r7,  [r6]		@ r7  <- CACHE_RST_START
	tst	r7,  r1			@ is cache reset complete?
	bne	cachwt			@	if not, jump to keep waiting
	set	r7,  #0xff		@ r7  <- #xff
	str	r7,  [r6, #8]		@ CACHE_PAGE_CTRL <- 0xff, allow caching of all 16 pages
	set	r7,  #0x180		@ r7  <- initial target address = 0x30000000 (SDRAM)
	set	r9,  #0x18		@ r9  <- initial address translation table offset
cachni:	str	r7,  [r6, r9]		@ store target page address in address translation table
	eq	r9,  #0x54		@ are we done?
	addne	r7,  r7, #1		@	if not, r7  <- next target address
	addne	r9,  r9,  #4		@	if not, r9  <- next table offset
	bne	cachni			@	if not, jump to keep initializing address table
	set	r7,  #6			@ r7  <- 0x06
	str	r7,  [r6, #4]		@ enable instruction and data caching
	@ copy scheme code to SDRAM
	bl	codcpy
	@ jump to scheme
	set	pc,  #0x00

waitld:	@ wait countdown loop
	ldr	r10, =0x080000
waitl0:	subs	r10, r10, #1
	seteq	pc,  lr
	b	waitl0

codcpy:	@ copy scheme to SDRAM address 0x00000000
	ldr	r8,  = _text_section_address_	@ start of source
	ldr	r9,  = _startcode	@ start of destination (from build_link file)
	ldr	r10, = _endcode		@ end of destination (from build_link file)
	add	r10, r10,  #4
codcp0:	ldmia	r8!, {r0-r7}
	stmia	r9!, {r0-r7}
	cmp	r9,  r10
	bmi	codcp0
	set	pc,  lnk

