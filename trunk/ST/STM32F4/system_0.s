/*------------------------------------------------------------------------------
@
@  ARMPIT SCHEME Version 070
@
@  ARMPIT SCHEME is distributed under The MIT License.

@  Copyright (c) 2012-2014 Petr Cermak

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

/* ==========================================================================

	start of sub-environment and sub-obarray

	(don't forget ENDSUBOBAENV at end of file)

========================================================================== */

	STARTSUBOBAENV system_0

/*------------------------------------------------------------------------------
@  register address bindings
@-----------------------------------------------------------------------------*/

	BNDREG	"rcc",	   rcc_base
	BNDREG	"gioa",	   ioporta_base
	BNDREG	"giob",	   ioportb_base
	BNDREG	"gioc",	   ioportc_base
	BNDREG	"giod",	   ioportd_base
	BNDREG	"gioe",	   ioporte_base
	BNDREG	"giof",	   ioportf_base
	BNDREG	"giog",	   ioportg_base
	BNDREG	"gioh",	   ioporth_base
	BNDREG	"gioi",	   ioporti_base
	BNDREG	"tmr1",	   timer1_base_a
	BNDREG	"tmr2",	   timer2_base		@ tmr2 (armpit timer 0 w/r interrupts)
	BNDREG	"tmr3",	   timer3_base		@ tmr3 (armpit timer 1 w/r interrupts)
	BNDREG	"tmr4",	   timer4_base
	BNDREG	"tmr5",	   timer5_base
	BNDREG	"tmr6",	   timer6_base
	BNDREG	"tmr7",	   timer7_base
	BNDREG	"tmr8",	   timer8_base
	BNDREG	"tmr9",	   timer9_base
	BNDREG	"tmr10",   timer10_base
	BNDREG	"tmr11",   timer11_base
	BNDREG	"tmr12",   timer12_base
	BNDREG	"tmr13",   timer13_base
	BNDREG	"tmr14",   timer14_base
	BNDREG	"i2c1",	   i2c0_base		@ i2c1 (armpit I2C0 port)
	BNDREG	"i2c2",	   i2c1_base		@ i2c2 (armpit I2C1 port)
	BNDREG	"spi1",	   spi1_base
	BNDREG	"spi2",	   spi2_base
	BNDREG	"spi3",	   spi3_base
	BNDREG	"spi4",	   spi4_base
	BNDREG	"spi5",	   spi5_base
	BNDREG	"spi6",	   spi6_base
	BNDREG	"adc1",	   adc1_base
	BNDREG	"adc2",	   adc2_base
	BNDREG	"adc3",	   adc3_base
	BNDREG	"sdio",	   sdio_base
	BNDREG	"fsmc",	   fsmc_base
	BNDREG	"afm",	   afm_base
	BNDREG	"lcdb",	   lcd_base

/*------------------------------------------------------------------------------
@  utility functions
@-----------------------------------------------------------------------------*/

	/* (tic-start bool) */
	@ interrupt if bool is not #f
	systick_start			@ cortex-m3/m4 macro

	/* (tic-read) */
	systick_read			@ cortex-m3/m4 macro

	/* (tic-stop) */
	systick_stop			@ cortex-m3/m4 macro

	/* (RNG seed) */
	@ Park-Miller next number from seed
	simple_random_gen		@ cortex-m3/m4 macro


	/* (config-power rcof bit val) */
	PRIMIT	"config-power", cfpwr, pfun, 3
	@ in:	sv1 <- rcof (offset in rcc)	(scheme int)
	@ in:	sv2 <- bit position		(scheme int)
	@ in:	sv3 <- val (1 or 0)		(scheme int)
	@ out:	sv1 <- npo
	set	sv4, sv3
	set	sv3, sv2
	set	rva, rcc_base
	int2raw	rvb, sv1
	b	rcpbit

	/* (config-pin port pin mode . otyp ospd pupd af) */
	PRIMIT	"config-pin", cfgpn, pfun, 3
	@ in:	sv1 <- port (eg. giob)			(scheme int)
	@ in:	sv2 <- pin  (0 to 15)			(scheme int)
	@ in:	sv3 <- mode (0=in,1=out,2=af,3=analog)	(scheme int)
	@ in:	sv4 <- (otyp ospd pupd af) optional	(scheme int)
	@	otyp: 0=push-pull, 1=open-drain
	@	ospd: 0=low,  1=medium,  2=fast,     3=high speed
	@	pupd: 0=none, 1=pull-up, 2=pull-down
	@	af:   0-15 = alternate function selection
	@ out:	sv1 <- npo
	set	sv5, i0			@ sv5 <- offset to _MODER: 0x00
	set	rva, 1			@ rva <- log2 field width  (2 bits)
	bl	cfgpnr			@ configure _MODER with cfg in sv3
	set	rva, 0			@ rva <- log2 field width  (1 bit)
	bl	cfgpnr			@ configure _OTYPER with cfg in sv3
	set	rva, 1			@ rva <- log2 field width  (2 bits)
	bl	cfgpnr			@ configure _OSPEEDR with cfg in sv3
	set	rva, 1			@ rva <- log2 field width  (2 bits)
	bl	cfgpnr			@ configure _PUPDR with cfg in sv3
	set	rva, 2			@ rva <- log2 field width  (4 bits)
	cmp	sv2, #((8<<2)|i0)	@ is pin 0-7 (vs 8-15)?
	itE	mi
	setmi	sv5, (0x20<<2)|i0	@ 	if so,  sv5 <- _AFRL ofst: 0x20	
	setpl	sv5, (0x24<<2)|i0	@ 	if not, sv5 <- _AFRH ofst: 0x24
	bl	cfgpnr			@ configure _AFR-L/H with cfg in sv3
	b	adr_npofxt

_func_
cfgpnr:	@ clear (mask) then set gpio configuration
	@ in:	rva <- field size (log2: 0=1bit, 1=2bit, 2=4bit) (raw)
	@ in:	sv1 <- register base
	@ in:	sv2 <- pin
	@ in:	sv3 <- bit setting value		(scheme int)
	@ in:	sv4 <- remaining config list
	@ in:	sv5 <- register offset			(scheme int)
	@ out:	sv3 <- next config
	@ out:	sv4 <- rest of config list (updated)
	@ out:	sv5 <- incremented register offset	(scheme int)
	@ mods:	rva, rvb, rvc, sv5
	@ returns via:	lnk
	set	rvc, 0x03		@ rvc <- dflt bit-clearing mask (2bit)
	eq	rva, #0			@ just 1-bit in field?
	it	eq
	seteq	rvc, 0x01		@	if so, rvc <- 1-bit clearing msk
	eq	rva, #2			@ really 4-bits in field?
	it	eq
	seteq	rvc, 0x0f		@	if so, rvc <- 4-bit clearing msk
	int2raw	rvb, sv2		@ rvb <- pin (raw)
	lsl	rvb, rvb, rva		@ rvb <- bit-field start pos
	cmp	rvb, #32		@ field-start overflow to next reg?
	it	pl
	subpl	rvb, rvb, #32		@	if so,  rvb <- updated start
	lsl	rvc, rvc, rvb		@ rvc <- bit-clearing mask, in-place
	int2raw	rva, sv3
	lsl	rvb, rva, rvb		@ rvb <- bit-setting value, in-place
	bic	rva, sv1, #3
	lsl	rva, rva, #2		@ rva <- full port address
	add	rva, rva, sv5, lsr #2	@ rva <- full port address + offset
	read	rva, rva, #0		@ rva <- GPIOx_REG, current config
	bic	rva, rva, rvc		@ rva <- config with cleared bits
	orr	rvb, rva, rvb		@ rvb <- config with set bits
	bic	rva, sv1, #3
	lsl	rva, rva, #2		@ rva <- full port address
	add	rva, rva, sv5, lsr #2	@ rva <- full port address + offset
	write	rvb, rva, #0		@ GPIOx_REG <- config updated
	@ exit if done, else update cfg list, reg offset and return
	pairp	sv4			@ more parameters?
	bne	adr_npofxt		@	if not, return with npo
	snoc	sv3, sv4, sv4		@ sv3 <- nxt-cfg, sv4 <- (rest-cfg)
	add	sv5, sv5, #0x10		@ sv5 <- register offset, incremented
	set	pc,  lnk


	/* (pin-set port pin) */
	PRIMIT	"pin-set", pnset, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gio0/1 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	write16	rvc, rva, #0x18
	b	adr_npofxt

	/* (pin-clear port pin) */
	PRIMIT	"pin-clear", pnclr, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gio0/1 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	write16	rvc, rva, #0x1a
	b	adr_npofxt

	/* (pin-set? port pin) */
	PRIMIT	"pin-set?", pnstq, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- #t/#f pin status (#t = high)
	@ rva <- gio0/1 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	read	rvb, rva, #io_state
	tst	rvb, rvc
	b	adr_notfxt

	/* (spi-put port val) */
	PRIMIT	"spi-put", spput, pfun, 2, oregent, null
	@ in:	sv1 <- port (spi0, spi1, ...)	(scheme int)
	@ in:	sv2 <- val
	@ out:	sv1 <- npo
	rgwfbt	rva, #spi_status, spi_txrdy_bit, 1, rvc
	write	rvb, rva, #spi_thr
	b	adr_npofxt

	/* (spi-get port) */
	PRIMIT	"spi-get", spget, pfun, 1, oregent, null
	@ in:	sv1 <- port (spi0, spi1, ...)	(scheme int)
	@ out:	sv1 <- data from spi		(scheme int)
	rgwfbt	rva, #spi_status, spi_rxrdy_bit, 1
	read	rvb, rva, #spi_rhr
	raw2int	sv1, rvb
	set	pc,  cnt

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/* -------------------------------------------------------------------------- */

ENDSUBOBAENV

/* -------------------------------------------------------------------------- */



