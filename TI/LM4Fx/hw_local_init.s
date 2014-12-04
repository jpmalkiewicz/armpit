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


_func_
hw_cfg:	/* configure clocks, isrs */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ out:	cortex	env	<- rcgc_base = peripheral RCGC base adrs
	@ out:	AM335x	env	<- PER_CM_base  = CM_PER  base address
	@ out:	AM335x	dts	<- SCM_base     = Control Module base (L4_WKUP)
	@ out:	AM335x	glv	<- WKUP_CM_base	= CM_WKUP base address
	@ out:	OMAP3	env	<- PER_CM_base  = CM_PER  base address
	@ out:	OMAP3	dts	<- CORE_CM_base	= CM_CORE base address
	@ out:	OMAP3	glv	<- WKUP_CM_base	= CM_WKUP base address
	@ out:	OMAP4	env	<- L3INIT_CM2_base = L3INIT_CM2 base
	@ out:	OMAP4	dts	<- L4PER_CM2_base  = L4PER_CM2 base
	@ out:	OMAP4	glv	<- SCM_PADCONF     = SYSCTRL_PADCONF_CORE
	@ ret:	via lnk

	/* pre-set common registers */
	set	env, rcgc_base		@ env      <- peripheral RCGC base adrs
	set	glv, pr_base		@ glv      <- Periph. Ready base adrs
	set	dts, sys_base		@ dts      <- System Control, RCC base

	/* allow 4-byte stack alignment (clear STKALIGN in CCR) */
	stackalign_4			@ cortex-m3/m4 macro

	/* initialize clocks */
  .ifndef EK_TM4C1294
	rgcpbf	dts, #rcc,   6, 11, 0x15	@ RCC  <- XTAL=0x15 = 16MHz Xtal
	rgcpbt	dts, #rcc,   0,  0		@ RCC  <- MOSCDIS=0,ena main osc
	rgcpbf	dts, #rcc2, 22, 32, 0x304	@ RCC2 <- SYSDIV2=5 w/LSB2,80MHz
	rgcpbt	dts, #rcc,  22,  1		@ RCC  <- USESYSDIV=1,ena f div
	rgcpbf	dts, #rcc2,  4,  7, 0x00	@ RCC2 <- OSCSRC2=0,ck=main osc
	rgcpbf	dts, #rcc2, 13, 15, 0x00	@ RCC2 <- PWRDN2=0,pwr PLL & USB
	rgwfbf	dts, #0x50,  6,  8, 0x03	@ wait for PLL & USB Trdy bits
	rgcpbt	dts, #rcc2, 11,  0		@ RCC2 <- BYPASS2=0, connect PLL
  .else
	write	fre, dts, #0x7c		@ MOSCCTL  <- clear NoXTal & PWRDN bits
	rgwfbt	dts, #0x50, 8, 1	@ wait for MOSCPUPRIS bit set in RIS
	write	0x03300000, dts, #0xb0	@ RSCLKCFG <- OSCCLK=MOSC, PLLSRC=XTal
	write	sv4, dts, #0x164	@ PLLFREQ1 <- N=4, Q=0, fvco=480MHz
	write	0x00800060, dts, #0x160	@ PLLFREQ0 <- MINT=0x60, MFRAC=0, PWR=1
	rgcpbt	dts, #0xb0, 30, 1	@ RSCLKCFG <- accept new PLL freq parms
	write	0x01950195, dts, #0xc0	@ MEMTIM0  <- 5 wait states for 120 MHz
	rgwfbt	dts, #0x168, 0, 1	@ wait for PLL locked bit set
	rgrmw	dts, #0xb0,1<<30,(9<<28)|3 @ RSCLKCFG <- ck=PLL,120MHz,MTM=1
  .endif

	/* power-up timer0 and timer1 */
	write	sv3, env, #0x04		@ RCGCTIMER <- enab clk for sel Timer(s)

	/* return */
	set	pc,  lnk



