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

	BNDREG	"rcc",	   rcc_base
	BNDREG	"gioa",	   ioporta_base
	BNDREG	"giob",	   ioportb_base
	BNDREG	"gioc",	   ioportc_base
	BNDREG	"giod",	   ioportd_base
	BNDREG	"gioe",	   ioporte_base
	BNDREG	"giof",	   ioportf_base
	BNDREG	"giog",	   ioportg_base
	BNDREG	"tmr2",	   timer0_base		@ tmr2 (armpit timer 0 w/r interrupts)
	BNDREG	"tmr3",	   timer1_base		@ tmr3 (armpit timer 1 w/r interrupts)
	BNDREG	"tmr4",	   timer4_base
	BNDREG	"tmr5",	   timer5_base
	BNDREG	"tmr6",	   timer6_base
	BNDREG	"tmr7",	   timer7_base
	BNDREG	"i2c2",	   i2c1_base		@ i2c2 (armpit I2C1 port)
	BNDREG	"spi1",	   spi1_base
	BNDREG	"spi2",	   spi2_base
	BNDREG	"spi3",	   spi3_base
	BNDREG	"adc1",	   adc1_base
	BNDREG	"adc2",	   adc2_base
	BNDREG	"adc3",	   adc3_base
	BNDREG	"sdio",	   sdio_base
	BNDREG	"fsmc",	   fsmc_base
	BNDREG	"afio",	   afio_base

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

	/* (config-pin port pin cnf mode) */
	PRIMIT	"config-pin", cfgpn, pfun, 4
	@ in:	sv1 <- port (eg. giob)		(scheme int)
	@ in:	sv2 <- pin  (0 to 15)		(scheme int)
	@ in:	sv3 <- cnf			(scheme int)
	@ in:	sv4 <- mode			(scheme int)
	@ out:	sv1 <- npo
	int2raw	rva, sv1
	lsl	rva, rva, #4
	cmp	sv2, #33
	it	pl
	addpl	rva, rva, #4
	orr	sv5, rva, #i0
	read	rvb, rva, #0
	int2raw	rvc, sv2
	and	rvc, rvc, #0x07
	lsl	rvc, rvc, #2
	ash	rva, 0x0f, rvc
	bic	rvb, rvb, rva
	bic	rva, sv3, #0x03
	lsl	rva, rva, rvc
	orr	rvb, rvb, rva
	int2raw	rva, sv4
	lsl	rva, rva, rvc
	orr	rvb, rvb, rva
	bic	rva, sv5, #i0
	write	rvb, rva, #0
	b	adr_npofxt

	/* (pin-set port pin) */
	PRIMIT	"pin-set", pnset, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gio0/1 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	write	rvc, rva, #io_set
	b	adr_npofxt

	/* (pin-clear port pin) */
	PRIMIT	"pin-clear", pnclr, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gio0/1 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	write	rvc, rva, #io_clear
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



