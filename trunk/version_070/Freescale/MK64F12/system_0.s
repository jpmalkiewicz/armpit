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

/* ==========================================================================

	start of sub-environment and sub-obarray

	(don't forget ENDSUBOBAENV at end of file)

========================================================================== */

	STARTSUBOBAENV system_0

/*------------------------------------------------------------------------------
@  register address bindings
@-----------------------------------------------------------------------------*/

	BNDREG	"gioa",	ioporta_base
	BNDREG	"giob",	ioportb_base
	BNDREG	"gioc",	ioportc_base
	BNDREG	"giod",	ioportd_base
	BNDREG	"gioe",	ioporte_base
	BNDREG	"tmr0",	timer0_base
	BNDREG	"tmr1",	timer1_base
	BNDREG	"tmr2",	timer2_base
	BNDREG	"i2c0",	i2c0_base
	BNDREG	"i2c1",	i2c1_base
	BNDREG	"spi0",	spi0_base
	BNDREG	"spi1",	spi1_base
	BNDREG	"adc0",	adc0_base
	BNDREG	"adc1",	adc1_base
	BNDREG	"sdhc",	sdhc0_base

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

	/* (config-power scgc bit val) */
	PRIMIT	"config-power", cfpwr, pfun, 3
	@ in:	sv1 <- scgc 			(scheme int)
	@	(eg. 1, 2 for SCGC1, SCGC2, ...)
	@ in:	sv2 <- bit position		(scheme int)
	@ in:	sv3 <- val (1 or 0)		(scheme int)
	@ out:	sv1 <- npo
	set	rva, scgc_base		@ rva <- address of SCGC1
	bic	rvb, sv1, #3
	sub	rvb, rvb, #4		@ rvb <- offset of SCGCn from SCGC1
	add	rva, rva, rvb		@ rva <- address of SCGCn
	int2raw	rvb, sv2		@ rvb <- bit position (raw int)
	ash	rvc, 1, rvb		@ rvc <- bit at bit position
	swi	run_prvlgd		@ privileged mode (needed to r/w SCGCn)
	read	rvb, rva, #0x00		@ rvb <- SCGCn (contents)
	eq	sv3, #i0		@ clearing bit?
	itE	eq
	biceq	rvb, rvb, rvc		@	if so,  rvb <- bit cleared
	orrne	rvb, rvb, rvc		@	if not, rvb <- bit set
	write	rvb, rva, #0x00		@ SCGCn <- updated
	swi	run_normal		@ return to normal mode
	b	adr_npofxt

	/* (config-pin port pin mux . dir) */
	PRIMIT	"config-pin", cfgpn, pfun, 3
	@ in:	sv1 <- port (eg. giob)		(scheme int)
	@ in:	sv2 <- pin  (0 to 31)		(scheme int)
	@ in:	sv3 <- mux: alt_n, strength, pull-up/down (scheme int)
	@ in:	sv4 <- dir = () or (0) for input, (1) for gpio output
	@ out:	sv1 <- npo
	@ set mux in PORTx_PCRn (p. 276, 282)
	bic	rva, sv1, #3
	lsl	rva, rva, #2		@ rva <- full GPIOx base address
	set	rvb, ioporta_base
	sub	rvb, rva, rvb		@ rvb <- GPIOx offset from GPIOA
	set	rvc, ioporta_pcr
	add	rvc, rvc, rvb, lsl #6	@ rvc <- PORTx_PCR0 base address
	bic	rvb, sv2, #3		@ rvb <- PCRn offset
	add	rvc, rvc, rvb		@ rvc <- PORTx_PCRn address
	int2raw	rvb, sv3		@ rvb <- mux (raw int)
	write	rvb, rvc, #0x00		@ PORTx_PCRn <- mux
	@ set pin direction in GPIOx_PDOR, if needed (p. 1761)
	pairp	sv4			@ dir (gpio) is given?
	bne	adr_npofxt		@	if not, return
	car	sv4, sv4		@ sv4 <- dir value
	int2raw	rvb, sv2
	ash	rvc, 1, rvb		@ rvc <- bit at bit position for pin
	read	rvb, rva, #io_dir	@ rvb <- GPIOx_PDOR
	eq	rvb, #i0		@ pin direction is input?
	itE	eq
	biceq	rvb, rvb, rvc		@	if so,  rvb <- bit cleared
	orrne	rvb, rvb, rvc		@	if not, rvb <- bit set
	write	rvb, rva, #io_dir	@ GPIOx_PDOR <- dir bit updated
	b	adr_npofxt

	/* (pin-set port pin) */
	PRIMIT	"pin-set", pnset, pfun, 2, oregent, null
	@ in:	sv1 <- port (eg. giob)		(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gioa-h as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	write	rvc, rva, #io_set
	b	adr_npofxt

	/* (pin-clear port pin) */
	PRIMIT	"pin-clear", pnclr, pfun, 2, oregent, null
	@ in:	sv1 <- port (eg. giob)		(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gioa-h as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	write	rvc, rva, #io_clear
	b	adr_npofxt

	/* (pin-set? port pin) */
	PRIMIT	"pin-set?", pnstq, pfun, 2, oregent, null
	@ in:	sv1 <- port (eg. giob)		(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- #t/#f pin status (#t = high)
	@ rva <- gioa-h as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	read	rvb, rva, #io_state
	tst	rvb, rvc
	b	adr_notfxt

	/* (read-u8 port offset) */
	PRIMIT	"read-u8", readu8, pfun, 2, oregent, null
	@ in:	sv1 <- port (eg. giob)		(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- #t/#f pin status (#t = high)
	@ rva <- gioa-h as full address (from sv1, through regent)
	@ rvb <- offset as raw int	(from sv2, through regent)
	read8	rvb, rva, rvb
	raw2int	sv1, rvb
	set	pc,  cnt

	/* (write-u8 val port offset) */
	PRIMIT	"write-u8", writeu8, pfun, 3
	@ in:	sv1 <- value			(scheme int)
	@ in:	sv2 <- port (eg. giob)		(scheme int)
	@ in:	sv3 <- offset			(scheme int)
	@ out:	sv1 <- npo
	int2raw	rva, sv1
	int2raw	rvb, sv2
	int2raw	rvc, sv3
	lsl	rvb, rvb, #4
	write8	rva, rvb, rvc
	b	adr_npofxt

	/* (read-u16 port offset) */
	PRIMIT	"read-u16", readu16, pfun, 2, oregent, null
	@ in:	sv1 <- port (eg. giob)		(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- #t/#f pin status (#t = high)
	@ rva <- gioa-h as full address (from sv1, through regent)
	@ rvb <- offset as raw int	(from sv2, through regent)
	read16	rvb, rva, rvb
	raw2int	sv1, rvb
	set	pc,  cnt

	/* (write-u16 val port offset) */
	PRIMIT	"write-u16", writeu16, pfun, 3
	@ in:	sv1 <- value			(scheme int)
	@ in:	sv2 <- port (eg. giob)		(scheme int)
	@ in:	sv3 <- offset			(scheme int)
	@ out:	sv1 <- npo
	int2raw	rva, sv1
	int2raw	rvb, sv2
	int2raw	rvc, sv3
	lsl	rvb, rvb, #4
	write16	rva, rvb, rvc
	b	adr_npofxt

	/* (spi-put port val) */
	PRIMIT	"spi-put", spput, pfun, 2, oregent, null
	@ in:	sv1 <- port (spi0 or spi1)	(scheme int)
	@ in:	sv2 <- val
	@ out:	sv1 <- npo
	rgwfbt	rva, #spi_status, spi_txrdy_bit, 1, rvc
	write	rvb, rva, #spi_thr
	b	adr_npofxt

	/* (spi-get port) */
	PRIMIT	"spi-get", spget, pfun, 1, oregent, null
	@ in:	sv1 <- port (spi0 or spi1)	(scheme int)
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



