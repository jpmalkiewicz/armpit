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
piocfg:	/* configure GPIO for LED(s), button(s) */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	cortex	env	<- rcgc_base = peripheral RCGC base adrs
	@ in:	AM335x	env	<- PER_CM_base  = CM_PER  base address
	@ in:	AM335x	dts	<- SCM_base     = Control Module base (L4_WKUP)
	@ in:	AM335x	glv	<- WKUP_CM_base	= CM_WKUP base address
	@ in:	OMAP3	env	<- PER_CM_base  = CM_PER  base address
	@ in:	OMAP3	dts	<- CORE_CM_base	= CM_CORE base address
	@ in:	OMAP3	glv	<- WKUP_CM_base	= CM_WKUP base address
	@ in:	OMAP4	env	<- L3INIT_CM2_base = L3INIT_CM2 base
	@ in:	OMAP4	dts	<- L4PER_CM2_base  = L4PER_CM2 base
	@ in:	OMAP4	glv	<- SCM_PADCONF     = SYSCTRL_PADCONF_CORE
	@ ret:	via lnk

	/* cortex-M3/M4: LM3S, LM4F */
  .ifdef cortex	@ LM3S, LM4F
	@ initialize LED gpio pins
	rgrmw	env, #0x08, ENABLE_PORTS	@ RCGC <- enab clk for port(s)
    .ifdef LM_4Fxxx
	rgwfbm	glv, #0x08, ENABLE_PORTS	@ wait for all periph bits ready
    .endif
	write	ALLLED, LEDPINSEL, #0x0400	@ GPIODIR  <- led dirs = output
	write	rvb,    LEDPINSEL+0x500, #0x08	@ GPIODR8R <- led drive = 8 mA
	write	rvb,    rva, #0x1c		@ GPIODEN  <- led pins active
	@ initialize boot-override button
	rgcpbt	BOOTOVERRID_PRT+0x500,#0x1c,BOOTOVERRID_BUT,1 @ GPIODEN <- digin
	write	1<<BOOTOVERRID_BUT, rva, #0x10	@ GPIOPUR  <- weak pullup on btn
  .endif @ cortex (LM3S, LM4F)

	/* cortex-A8/A9: AM335x, OMAP3, OMAP4 */
  .ifdef cortex_a8
    .ifdef AM335x	/* AM335x */
	@ enable GPIO1 module and cfg pins/pads for GPIO: set MuxMode to mode 7
	@  gpio1_21, pin V15, signal name GPMC_A5
	@  gpio1_22, pin U15, signal name GPMC_A6
	@  gpio1_23, pin T15, signal name GPMC_A7
	swi	run_prvlgd				@ privileged, no IRQ
	write	0x040002, env, #0xac	@ CM_PER_GPIO1_CLKCTRL  <- enable GPIO1
	write	0x0f, SCM_base+0x800, #(rled_pin<<2)	@ GPMC_A5 <- gpio1_21
	write	 rvb, rva, #(yled_pin<<2)		@ GPMC_A6 <- gpio1_22
	write	 rvb, rva, #(gled_pin<<2)		@ GPMC_A7 <- gpio1_23
	swi	run_no_irq				@ unprvlgd mode, no IRQ
    .else
      .ifndef cortex_a9	/* OMAP3 */
	@ clocks for GPIOn enabled in startup.s
	write16	sv4, SCM_base+RLED_PDCNF_OFST,#0 @ ctrl_padconf <- red LED cfg
	write16	sv4, SCM_base+GLED_PDCNF_OFST,#0 @ ctrl_padconf <- grn LED cfg
      .else 		/* OMAP4 */
	@ power-up gpio4 and configure pin functions (multiplex)
	@ Parlor button gpio_121, pin AG24 (bottom ball) mode 3
	@ Parlor LED    gpio_122, pin AH24 (bottom ball) mode 3
	@ (use: dts <- L4PER_CM2 base, glv <- SYSCTRL_PADCONF_CORE)
	write	  0x0101, dts, #0x70	@ CM_L4PER_GPIO4_CLKCTRL <- enab altclk
	write 0x0003051b, glv, #0x0114	@ PAD0_ABE_DMIC_DIN2_PAD1_ABE_DMIC_DIN3
      .endif
    .endif
	@ Set LED pins as outputs
	rgrmw	LEDIO, #io_dir, ALLLED, xxx
  .endif @ cortex_a8

	/* return */
	set	pc,  lnk



