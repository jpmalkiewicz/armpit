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

	BNDREG	"rcc",	rcc_base
	BNDREG	"gioa",	ioporta_base
	BNDREG	"giob",	ioportb_base
	BNDREG	"gioc",	ioportc_base
	BNDREG	"giod",	ioportd_base
	BNDREG	"gioe",	ioporte_base
	BNDREG	"giof",	ioportf_base
	BNDREG	"giog",	ioportg_base
	BNDREG	"gioh",	ioporth_base
	BNDREG	"tmr0",	timer0_base
	BNDREG	"tmr1",	timer1_base
	BNDREG	"tmr2",	timer2_base
	BNDREG	"tmr3",	timer3_base
	BNDREG	"i2c0",	i2c0_base
	BNDREG	"i2c1",	i2c1_base
	BNDREG	"ssi0",	ssi0_base
	BNDREG	"ssi1",	ssi1_base
	BNDREG	"adc0",	adc0_base
	BNDREG	"pwm0",	pwm0_base

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

	/* (config-power rgc bit val) */
	PRIMIT	"config-power", cfpwr, pfun, 3
	@ in:	sv1 <- rgc (eg. 2 for RCGC2)	(scheme int)
	@ in:	sv2 <- bit position		(scheme int)
	@ in:	sv3 <- val (1 or 0)		(scheme int)
	@ out:	sv1 <- npo
	set	sv4, sv3
	set	sv3, sv2
	set	rva, rcc_base
	bic	rvb, sv1, #0x03
	orr	rvb, rvb, #0x100
	b	rcpbit
	
	/* (config-pin port pin den odr pur pdr dir dr2r dr8r afun) */
	PRIMIT	"config-pin", cfgpn, pfun, 2, oregent, null
	@ in:	sv1 <- port (eg. giob)		(scheme int)
	@ in:	sv2 <- pin  (0 to 7)		(scheme int)
	@ in:	sv3 <- (den odr pur pdr dir dr2r dr8r afun)
	@ out:	sv1 <- npo
	@ rva <- gioa-h as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	add	rva, rva, #0x0500
	ash	rvc, 1, rvb
	add	rva, rva, #0x001c	@ rva <- port + #x51c, for den
	bl	pcfghl
	sub	rva, rva, #0x0010	@ rva <- port + #x50c, for odr
	bl	pcfghl
	add	rva, rva, #0x0004	@ rva <- port + #x510, for pur
	bl	pcfghl
	add	rva, rva, #0x0004	@ rva <- port + #x514, for pdr
	bl	pcfghl
	sub	rva, rva, #0x0014
	sub	rva, rva, #0x0100	@ rva <- port + #x400, for dir
	bl	pcfghl
	add	rva, rva, #0x0100	@ rva <- port + #x500, for dr2r
	bl	pcfgdr
	add	rva, rva, #0x0008	@ rva <- port + #x508, for dr8r
	bl	pcfgdr
	sub	rva, rva, #0x0100
	add	rva, rva, #0x0018	@ rva <- port + #x420, for afun
	bl	pcfghl
	b	adr_npofxt
		
pcfgdr:	@ helper entry for dr2r and dr8r
	nullp	sv3
	beq	adr_npofxt
	car	sv2, sv3
	eq	sv2, #i0
	itT	eq
	cdreq	sv3, sv3
	seteq	pc,  lnk
pcfghl:	@ helper function, general entry
	nullp	sv3
	beq	adr_npofxt
	snoc	sv2, sv3, sv3
	read	rvb, rva, #0
	eq	sv2, #i0
	itE	eq
	biceq	rvb, rvb, rvc
	orrne	rvb, rvb, rvc
	write	rvb, rva, #0
	set	pc,  lnk

	/* (pin-set port pin) */
	PRIMIT	"pin-set", pnset, pfun, 2, oregent, null
	@ in:	sv1 <- port (eg. giob)		(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gioa-h as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 4, rvb
	write	0xff, rva, rvc
	b	adr_npofxt

	/* (pin-clear port pin) */
	PRIMIT	"pin-clear", pnclr, pfun, 2, oregent, null
	@ in:	sv1 <- port (eg. giob)		(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gioa-h as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 4, rvb
	write	0x00, rva, rvc
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



