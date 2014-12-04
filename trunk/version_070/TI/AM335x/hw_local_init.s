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

	/* switch to privileged mode for CM_PER/CM_WKUP and pin cfg control */
	swi	run_prvlgd		@ set Thread mode, privileged, no IRQ

	/* ---------- set common addresses ---------- */
	set	env, PER_CM_base	@ env <- CM_PER  base address
	set	dts, SCM_base		@ dts <- Control Module base (L4_WKUP)
	set	glv, WKUP_CM_base	@ glv <- CM_WKUP base address

	/* ---------- disable watchdog 1 ---------- */
	write	0xAAAA,0x44E35000,#0x48	@ WDT1_WSPR <- unlock watchdog, step 1
	rgwfbt	rva, #0x34, 4, 0	@ wait for W_PEND_WSPR clrd in WDT1_WWPS
	write	0x5555, rva, #0x48	@ WDT1_WSPR <- unlock watchdog, step 2
	rgwfbt	rva, #0x34, 4, 0	@ wait for W_PEND_WSPR clrd in WDT1_WWPS

	/* ---------- configure i2c0 ---------- */
	@ enable I2C0 module and configure I2C0 pins (SCL=C16, SDA=C17, Mode 0)
	write	sv2,  glv, #0xB8	@ CM_WKUP_I2C0_CLKCTRL  <- enable I2C0
	add	rva,  dts, #0x0900	@ rva <- conf_mmc0_clk base address
	write	0x28, rva, #0x88	@ i2c0_sda <- uart0_rxd
	write	 rvb, rva, #0x8c	@ i2c_scl <- uart0_txd
	@ initialize and reset I2C0 module
	set	sv5, lnk		@ sv5 <- lnk, saved
	hwi2rst	i2c0_base		@ reset I2C0 (i.e. I2C1 on MCU)

	/* ---------- configure vdd_mpu, mpu voltage ---------- */
	@ set VDD_MPU (AM3359) = VDCDC2 (TPS65217c) to needed level, vdd_mpu
	set	rva, i2c0_base		@ rva <- I2C0 base address
	@ level 2 protection, pass 1
	hwi2wr	0x7d0b24^0x0f0000,   i2	@ PASSWORD(#x0b) <- DEFDCDC2(#x0f)
	hwi2wr	(vdd_mpu<<16)|0xf24, i2	@ DEFDCDC2(#x0f) <- VDCDC2=vdd_mpu
	@ level 2 protection, pass 2
	hwi2wr	0x7d0b24^0x0f0000,   i2	@ PASSWORD(#x0b) <- DEFDCDC2(#x0f)
	hwi2wr	(vdd_mpu<<16)|0xf24, i2	@ DEFDCDC2(#x0f) <- VDCDC2=vdd_mpu
	@ wait for power good
dcdc2w:	hwi2rd	0x000c24, i1		@ rvc <- byte from i2c reg PGOOD(#x0c)
	tst	rvc, #0x08		@ is DCDC2 regulated?
	beq	dcdc2w			@	if not, jump to keep waiting
	@ restore pre-set common values
	set	lnk, sv5
	set	sv3, 3
	set	sv5, 5

	/* ---------- configure the MPU PLL ---------- */
	rgcpbf	glv, #0x88, 0, 3, 0x04	@ CM_CLKMODE_DPLL_MPU <- bypass mpu pll
	rgwfbt	glv, #0x20, 8, 1	@ wait for bypass in CM_IDLEST_DPLL_MPU
	rgcpbf	glv, #0x2c, 0, 19, (freq_mpu<<8)|23 @ _DPLL_MPU <- M=f_mpu,N=23
	write	sv1, glv, #0xa8		@ CM_DIV_M2_DPLL_MPU <- MPU = freq/1
	rgrmw	glv, #0x88, 0x07	@ CM_CLKMODE_DPLL_MPU <- lock mpu pll
	rgwfbt	glv, #0x20, 0, 1	@ wait for lock in CM_IDLEST_DPLL_MPU

	/* ---------- configure CORE PLL ---------- */
	@ for CORE PLL: ROM configuration is OK:
	@   CM_CLKSEL_DPLL_CORE <- M=1000,N=23 (2GHz) 
	@   CM_DIV_Mx_DPLL_CORE <- M4=10(200MHz), M5=8(250MHz), M6=4(500MHz)

	/* ---------- configure PER PLL ---------- */
	@ for PER PLL:  ROM configuration is OK:
	@   CM_CLKSEL_DPLL_PER <- DIV=4, M=960, N=23 (1.62GHz, 960MHz)
	@   CM_DIV_M2_DPLL_PER <- M2=5 (192MHz, 96, 48, 24MHz)

	/* ---------- configure DMTimer2 and DMTimer3 ---------- */
	@ power-up DMTimer2 & DMTimer3 (use: env <- CM_PER  base)
	write	sv2, env, #0x80		@ CM_PER_TIMER2_CLKCTRL <- enable
	write	sv2, env, #0x84		@ CM_PER_TIMER3_CLKCTRL <- enable
	write	sv1, 0x44e00500, #0x08	@ CLKSEL_TIMER2_CLK <- CLK_M_OSC clock
	write	sv1, rva, #0x0c		@ CLKSEL_TIMER3_CLK <- CLK_M_OSC clock

	/* ---------- initialize interrupts ---------- */
	write	0x0130, int_base, #0xc8	@ INTC_MIR_CLEAR2 <- uart0,DMTmr2,3 ints

	/* ---------- finish-up ---------- */
	swi	run_no_irq		@ set Thread mode, unprvlgd, no IRQ, usr
	set	pc,  lnk		@ return


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg



