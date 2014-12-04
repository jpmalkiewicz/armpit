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
	set	env, L3INIT_CM2_base	@ env <- L3INIT_CM2 base
	set	dts, L4PER_CM2_base	@ dts <- L4PER_CM2 base
	set	glv, SCM_PADCONF	@ glv <- SYSCTRL_PADCONF_CORE

	/* enable interrupt controller for this cpu */
	write	sv1, GICC_base, #0x00	@ GICC_CTRL / ICCICR <- enabled

	/* jump to start scheme if not running on cpu0 */
	swi	run_prvlgd
	mrc	p15, 0, rva, c0, c0, 5	@ rva <- Multiproc Affinity reg, MPIDR
	swi	run_no_irq
	tst	rva, #3			@ self-identify as cpu1?
	bne	scinit			@	if so,  jump to start scheme

	/* ---------- disable watchdog 2 ---------- */
	write	0xAAAA,0x4a314000,#0x48	@ WDT2_WSPR <- unlock watchdog, step 1
	rgwfbt	rva, #0x34, 4, 0	@ wait for W_PEND_WSPR clrd in WDT2_WWPS
	write	0x5555, rva, #0x48	@ WDT2_WSPR <- unlock watchdog, step 2
	rgwfbt	rva, #0x34, 4, 0	@ wait for W_PEND_WSPR clrd in WDT2_WWPS

	/* ---------- configure vdd_mpu, mpu voltage ---------- */
	@ set VDD_MPU (OMAP4430) = VCORE1 (TWL6030) to vdd_mpu, in PRM_VC_
	write	0x1f1f1f1f, 0x4a307b00, #0xac     @ _CFG_I2C_MODE <- i2c5 speed
	write	0x1005512|(vdd_mpu<<16),rva,#0xa0 @ _VAL_BYPASS <- VCORE1=vddmpu
	wait	0x1000000			  @ wait a bit

	/* ---------- configure the MPU DPLL ---------- */
	set	rvc, lnk		 @ rvc <- lnk, saved
	set	rva, 0x4A004160		 @ rva <- CM_CLKMODE_DPLL_MPU
	bl	pllbyp			 @ bypass the MPU DPLL
	write	fre, rva, #0x08		 @ CM_AUTOIDLE_DPLL_MPU <- no idle
	write	(375<<8)|div_mpu,rva,#12 @ CM_CLKSEL_DPLL_MPU <- M=375,N=div_mpu
	write	0x0101, rva, #0x10	 @ CM_DIV_M2_DPLL_MPU <- M2=0x01,no idle
	bl	plllok			 @ lock the MPU DPLL

	/* ---------- configure PER DPLL ---------- */
	set	rva, 0x4A008140		@ rva <- CM_CLKMODE_DPLL_PER
	bl	pllbyp			@ bypass the PER DPLL
	write	fre,    rva, #0x08	@ CM_AUTOIDLE_DPLL_PER <- no idle
	write	0x1400, rva, #0x0c	@ CM_CLKSEL_DPLL_PER <- M=0x14, N=0x00
	write	0x0108, rva, #0x10	@ CM_DIV_M2_DPLL_PER <- M2=0x08,no idle
	write	0x0106, rva, #0x14	@ CM_DIV_M3_DPLL_PER <- M3=0x06,no idle
	write	0x010c, rva, #0x18	@ CM_DIV_M4_DPLL_PER <- M4=0x0c,no idle
	write	0x0109, rva, #0x1c	@ CM_DIV_M5_DPLL_PER <- M5=0x09,no idle
	write	0x0104, rva, #0x20	@ CM_DIV_M6_DPLL_PER <- M6=0x04,no idle
	write	0x0105, rva, #0x24	@ CM_DIV_M7_DPLL_PER <- M7=0x05,no idle
	bl	plllok			@ lock the PER DPLL

	/* ---------- configure i2c1 ---------- */
	write	sv2, dts, #0xa0		@ CM_L4PER_I2C1_CLKCTRL  <- enable
	hwi2rst	i2c0_base		@ reset I2C0 (i.e. I2C1 on MCU)
	set	lnk, rvc		@ lnk <- lnk, restored

	/* ---------- configure GPTimer1 and GPTimer2 ---------- */
	write	sv2, WKUP_CM_base, #0x40 @ CM_WKUP_GPTIMER1_CLKCTRL  <- enable
	write	sv2, dts, #0x38		 @ CM_L4PER_GPTIMER2_CLKCTRL <- enable

	/* ---------- enable the interrupt distributor ---------- */
	write	sv1, GICD_base, #0x00	  @ GICD_CTRL/ICDDCR <- enabled
	@ enable ints in controller: uart3=int 74, GPTimer1 & 2=int 37 & 38
	write	1<<10,GICD_base+0x100,#12 @ GICD_ISENABLER/ICDISER-3  2*32+10+32
	write	 3<<5, rva, #0x08	  @ GICD_ISENABLER/ICDISER-2 1*32+5,6+32
	@ set cpu targets for interrupts
  .ifndef enable_a9_mpcore
	write8	sv1,GICD_base+0x800,#(74+32) @ GICD_ITARGETSR/ICDIPTR-74 <- cpu0
	write8	sv1, rva, #(37+32)	   @ GICD_ITARGETSR/ICDIPTR-37 <- cpu0
	write8	sv1, rva, #(38+32)	   @ GICD_ITARGETSR/ICDIPTR-38 <- cpu0
  .else
    .ifdef native_usb
	write8	sv2,GICD_base+0x800,#(74+32) @ GICD_ITARGETSR/ICDIPTR-74 <- cpu1
    .else
	write8	sv1,GICD_base+0x800,#(74+32) @ GICD_ITARGETSR/ICDIPTR-74 <- cpu0
    .endif
	write8	sv1, rva, #(37+32)	   @ GICD_ITARGETSR/ICDIPTR-37 <- cpu0
	write8	sv2, rva, #(38+32)	   @ GICD_ITARGETSR/ICDIPTR-38 <- cpu1
  .endif

	/* return */
	set	pc,  lnk


	/* ---------- utility functions to bypass/lock a dpll ---------- */
pllbyp:	@ bypass a pll
	@ on entry:	rva <- address of CM_CLKMODE_DPLL for PLL (eg. MPU)
	@ modifies:	rvb
	write	sv4, rva, #0x00		@ CM_CLKMODE_DPLL <- bypass
	rgwfbt	rva, #0x04, 8, 1	@ wait for bypass bit in CM_IDLEST_DPLL
	set	pc,  lnk

plllok:	@ lock a pll
	@ on entry:	rva <- address of CM_CLKMODE_DPLL for PLL (eg. MPU)
	@ modifies:	rvb
	write	7, rva, #0x00		@ CM_CLKMODE_DPLL <- lock
	rgwfbt	rva, #0x04, 0, 1	@ wait for lock bit in CM_IDLEST_DPLL
	set	pc,  lnk


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg



