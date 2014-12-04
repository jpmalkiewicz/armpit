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


	/* i2c polling routines for communication with PMIC (common) */
	.include "i2c_poll_hw.s"	@ hw_i2c_reset,hw_i2c_write,hw_i2c_read

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

	/* ---------- set common addresses ---------- */
	set	env, PER_CM_base	@ env <- CM_PER  base address
	set	dts, CORE_CM_base	@ dts <- CM_CORE base address
	set	glv, WKUP_CM_base	@ glv <- CM_WKUP base address

	/* ---------- disable watchdog 2 ---------- */
	@ ensure clocks are enabled (use: glv<-CM_WKUP)
	rgcpbt	glv, #F_clck, 5, 1	@ cm_fclken_wkup <- enable WDT2 Fclk
	rgcpbt	glv, #I_clck, 5, 1	@ cm_iclken_wkup <- enable WDT2 Iclk
	@ disable watchdog 2
	write	0xAAAA,0x48314000,#0x48	@ WDT2_WSPR <- unlock watchdog, step 1
	rgwfbt	rva, #0x34, 4, 0	@ wait for W_PEND_WSPR clrd in WDT2_WWPS
	write	0x5555, rva, #0x48	@ WDT2_WSPR <- unlock watchdog, step 2
	rgwfbt	rva, #0x34, 4, 0	@ wait for W_PEND_WSPR clrd in WDT2_WWPS

	/* ---------- configure DPLL1: MPU dpll ---------- */
	rgcpbf	0x48004904, #0x00, 0, 3, 0x05 @ cm_clken_pll <- stop/bypass dpll
	rgwfbt	rva, #0x20, 0, 0	@ wait for pll stop in cm_idlest_ckgen
	write	PLL1_parms, rva, #0x3c	@ cm_clksel1_pll_mpu <- MPU PLL freq
	write	sv1, rva, #0x40		@ cm_clksel2_pll_mpu <- M2 = 0x01
  .ifndef TI_Beagle_XM
	rgcpbf	rva, #0x00, 0, 8, 0x37	@ cm_clken_pll <- dpll lock, fsel=3
  .else
	rgcpbf	rva, #0x00, 0, 4, 0x07	@ cm_clken_pll <- dpll lock, DM37 nofsel
  .endif
	rgwfbt	rva, #0x20, 0, 1	@ wait for pll lock in cm_idlest_ckgen

	/* ---------- configure DPLL4: peripherals dpll ---------- */
	rgrmw	0x48004d00, #0, 7<<16, 0xf0011000 @ cm_clken_pll <- stp/byp dpll
	rgwfbt	rva, #0x20, 1, 0	@ wait for pll stop in cm_idlest_ckgen
	write	PLL4_parms, rva, #0x44	@ cm_clksel2_pll <- core PLL freq
	write	0x09, rva, #0x48	@ cm_clksel3_pll <- M2=9: 96MHz=864MHz/9
	rgrmw	rva, #0x00, 7<<16	@ cm_clken_pll   <- dpll lock
	rgwfbt	rva, #0x20, 1, 1	@ wait for pll lock in cm_idlest_ckgen

	/* ---------- configure GPTimer1 and GPTimer2 ---------- */
	@ enable clocks for GPTimer1, set source as sysclock (use: glv<-CM_WKUP)
	rgcpbt	glv, #F_clck, 0, 1	@ cm_fclken_wkup <- enable GPTimer1 Fclk
	rgcpbt	glv, #I_clck, 0, 1	@ cm_iclken_wkup <- enable GPTimer1 Iclk
	rgcpbt	glv, #0x40,   0, 1	@ cm_clksel_wkup <- GPTimer1 src=sys clk
	@ enable clocks for GPTimer2, set source as sysclock (use: env<-CM_PER)
	rgcpbt	env, #F_clck, 3, 1	@ cm_fclken_per  <- enable GPTimer2 Fclk
	rgcpbt	env, #I_clck, 3, 1	@ cm_iclken_per  <- enable GPTimer2 Iclk
	rgcpbt	env, #0x40,   0, 1	@ cm_clksel_per  <- GPTimer2 src=sys clk

	/* ---------- configure I2C1 ---------- */
	rgcpbt	dts, #F_clck, 15, 1	@ cm_fclken_core <- enable MMC1 Fclk
	rgcpbt	dts, #I_clck, 15, 1	@ cm_iclken_core <- enable MMC1 Iclk
	@ configure I2C1 operation mode (aliased to i2c0)
	set	rvc, lnk		@ rvc <- lnk, saved
	hwi2rst	i2c0_base		@ reset I2C0 (i.e. I2C1 on MCU)
	set	lnk, rvc		@ lnk <- lnk, restored

	/* initialize interrupts */
	write	  0x60, int_base, #0xa8	@ enable timer 0, 1 ints in INTMSK
	write	0x0400, rva, #0xc8	@ enable uart 0, 1 ints in INTMSK

	/* return */
	set	pc,  lnk


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg



