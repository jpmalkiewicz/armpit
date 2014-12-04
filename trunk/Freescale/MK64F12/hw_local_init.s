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

	/* set flash configuration bytes at address 0x400 in code space */
	@ pages: 662, 674
	.balign 1024
	.word	0xffffffff, 0xffffffff, 0xffffffff, 0xfffffffe

_func_
hw_cfg:	/* configure security, clocks, flash */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ out:	env	<- scgc_base  =  SIM_SCGC base adrs periph clken
	@ ret:	via lnk
	@ initialize interrupts

	/* Note: SIM registers (eg. SCGC) accessible in privileged mode only */
	/*       MCG registers writable in privileged mode only              */
	/*       -- all that even after opening up AIPS-Lite below           */
	/*       watchdog must be disabled early on                          */
	swi	run_prvlgd		@ enter privileged mode

	/* disable watchdog */
	write16	0xc520, 0x40052000,#0xe	@ WDOGUNLOCK   <- unlock KEY 1
	write16	0xd928,        rva,#0xe	@ WDOGUNLOCK   <- unlock KEY 2
	write16	0x01d2,        rva,#0x0	@ WDOG_STCTRLH <- disable WDOG

	/* allow 4-byte stack alignment (clear STKALIGN in CCR) */
	write	fre, 0xe000ed14, #0x00

	/* allow user-mode peripheral access in AIPS-Lite0 and AIPS-Lite1 */
	@ pages: 93, 177, 437
	set	rva, 0x40000000		@ rva <- AIPS0_MPRA
	orr	rvb, rva, #0x80000	@ rvb <- AIPS1_MPRA
	set	rvc, 0x20		@ rvc <- offset to AIPSn_PACRD + 4
hwilp1:	write	fre, rva, rvc		@ AIPS0_PACRn <- allow user access
	write	fre, rvb, rvc		@ AIPS1_PACRn <- allow user access
	add	rvc, rvc, #4
	eq	rvc, #0x30		@ done?
	bne	hwilp1			@	if not, jump back to continue
	set	rvc, 0x40		@ rvc <- offset to AIPSn_PACRE
hwilp0:	write	fre, rva, rvc		@ AIPS0_PACRn <- allow user access
	write	fre, rvb, rvc		@ AIPS1_PACRn <- allow user access
	add	rvc, rvc, #4		@ rvc <- offset to next AIPSn_PACRn
	eq	rvc, #0x84		@ done?
	bne	hwilp0			@	if not, jump back to continue

	/* disable MPU to allow all masters to access all slaves (p. 418) */
	rgcpbt	0x4000d000, #0x00, 0, 0	@ MPU_CESR <- cleared VLD (MPU ena) bit

	/* pre-set common registers */
	set	env, scgc_base		@ env <- SIM_SCGC base adrs periph clken

	/* configure clocks */
	@ config EXTAL0 func on PTA8 (KSZ8081RNACA 50MHz clk) p.249,277,282,1757
	rgcpbt	env, #0x10, 9, 1		@ SCGC5  <- enab clk PORTA, p314
	rgrmw	ioporta_pcr, #(18<<2), (1<<24)|0x700, xxx @ PORTA_PCR18 <-EXTAL0
	@ connect EXTAL0 (50 MHz) to FLL
	write8	0x20,      mcg_base, #0x01	@ MCG_C2 <- very hi freq rng sel
	write8	0xb8,           rva, #0x00	@ MCG_C1 <- FLL in = EXTAL0/1536
	@ Configure PLL: Fout = 48 * 50 MHz / 20 = 120 MHz
	write8	(20-1),         rva, #0x04	@ MCG_C5 <- divide by 20, p.589
	write8	(1<<6)|(48-24), rva, #5		@ MCG_C6 <- sel PLL mult=48 p590
	@ set div for clocks: sys=120MHz, bus=60MHz, FlexBus=40MHz, flash=24MHz
	write	0x01240000, env, #0x1c		@ CLKDIV1 <- OUTDIV 1-4=1,2,3,5
	r8wfbt	rva, #0x06, 6, 1		@ wait on PLL lock bit in MCG_S
	write8	0x38, rva, #0x00		@ MCG_C1 <- conn PLL as clk src
	@ enable external reference clock (OSCERCLK) for SDHC, ADC (maybe more)
	write8	1<<7, osc_base, #0x00		@ OSC_CR <- enab OSCERCLK 50 MHz

	/* power-up timer0, timer1, i2c0, i2c1 */
	rgcpbf	env, #0x14, 24, 26, 3		@ SCGC6 <- enab clocks to FTM0,1
	rgcpbf	env, #0x0c,  6,  8, 3		@ SCGC4 <- enab clocks to I2c0,1

	/* return */
	swi	run_normal			@ return to normal/user run mode
	set	pc,  lnk



