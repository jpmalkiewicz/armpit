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

	BNDREG	"pmc",     PMC_base		@ pmc  <- PMC
	BNDREG	"gioa",	   pioa_base
	BNDREG	"giob",	   piob_base
	BNDREG	"gioc",	   pioc_base
	BNDREG	"tmr0",	   timer0_base
	BNDREG	"tmr1",	   timer1_base
	BNDREG	"i2c0",	   i2c0_base
	BNDREG	"spi0",	   spi0_base
	BNDREG	"pwm0",	   pwm0_base
	BNDREG	"adc0",	   adc0_base

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


	/* (config-power bit val) */
	PRIMIT	"config-power", cfpwr, pfun, 2
	@ in:	sv1 <- bit position	(scheme int)
	@ in:	sv2 <- val (1 or 0)	(scheme int)
	@ out:	sv1 <- npo
	set	rva, PMC_base
	int2raw	rvb, sv1
	ash	rvc, 1, rvb
	eq	sv2, #i1		@ enable power?
	itE	eq
	writeeq	rvc, rva, #0x10		@	if so,  PMC_PCER <- enable  peripheral clock
	writene	rvc, rva, #0x14		@	if not, PMC_PCDR <- disable peripheral clock
	b	adr_npofxt			@ return

	/* (config-pin pin dig ddir pup afab adir) */
	PRIMIT	"config-pin", cfgpn, pfun, 1
	@ pin:		0-31,  for PA0 to PA31
	@ dig:		0/1,   1 -> digital function (0 for alternate)
	@ ddir:		0/1,   1 -> output direction (digital)
	@ pup:		0/1,   1 -> pull-up resistor
	@ afab:		0/1/2, 1 -> periph A function, 2 -> periph B function
	@ adir:		0/1,   1 -> output write enable (alternate function) 
	@ in:	sv1 <- pin (0 to 31)		(scheme int)
	@ in:	sv2 <- (pin dig pup afab dir)	(list)
	@ out:	sv1 <- npo
	int2raw	rva, sv1
	ash	rvb, 1, rva
	set	rva, pioa_base
	@ digital function configuration
	nullp	sv2			@ no more configuration?
	beq	adr_npofxt		@	if so,  exit
	snoc	sv1, sv2, sv2		@ sv1 <- dig,  sv2 <- (ddir pup afab adir)
	eq	sv1, #i1		@ digital?
	itE	eq
	writeeq	rvb, rva, #0x00		@	if so,  PIO_PER <- enable  digital function
	writene	rvb, rva, #0x04		@	if not, PIO_PDR <- disable digital function
	@ digital direction configuration
	nullp	sv2			@ no more configuration?
	beq	adr_npofxt		@	if so,  exit
	snoc	sv1, sv2, sv2		@ sv1 <- ddir,  sv2 <- (pup afab adir)
	eq	sv1, #i1		@ output?
	itE	eq
	writeeq	rvb, rva, #0x10		@	if so,  PIO_OER <- enable  output direction
	writene	rvb, rva, #0x14		@	if not, PIO_ODR <- disable output direction
	@ pull-up resistor configuration
	nullp	sv2			@ no more configuration?
	beq	adr_npofxt		@	if so,  exit
	snoc	sv1, sv2, sv2		@ sv1 <- pup,  sv2 <- (afab adir)
	eq	sv1, #i1		@ pull-up?
	itE	eq
	writeeq	rvb, rva, #0x60		@	if so,  PIO_PUER <- enable  pull-up
	writene	rvb, rva, #0x64		@	if not, PIO_PUDR <- disable pull-up
	@ alternate function configuration
	nullp	sv2			@ no more configuration?
	beq	adr_npofxt		@	if so,  exit
	snoc	sv1, sv2, sv2		@ sv1 <- afab,  sv2 <- (adir)
	eq	sv1, #i1		@ peripheral A function?
	it	eq
	writeeq	rvb, rva, #0x70		@	if so,  PIO_ASR <- enable peripheral A function
	eq	sv1, #9			@ peripheral B function?
	it	eq
	writeeq	rvb, rva, #0x74		@	if so,  PIO_BSR <- enable peripheral B function
	@ output write configuration
	nullp	sv2			@ no more configuration?
	beq	adr_npofxt		@	if so,  exit
	snoc	sv1, sv2, sv2		@ sv1 <- adir,  sv2 <- ()
	eq	sv1, #i1		@ output write?
	itE	eq
	writeeq	rvb, rva, #0xa0		@	if so,  PIO_OWER <- enable  output write
	writene	rvb, rva, #0xa4		@	if not, PIO_OWDR <- disable output write
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

	/* (stop tmr) */
	PRIMIT	"stop", pfun, 1, oregent, null
	@ in:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ out:	sv1 <- npo
	write	2, rva, #0x00
	b	adr_npofxt

	/* (restart tmr) */
	PRIMIT	"restart", pfun, 1, oregent, null
	@ in:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ out:	sv1 <- npo
	write	5, rva, #0x00
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
	and	rvb, rvb, #0xff
	raw2int	sv1, rvb
	set	pc,  cnt


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/* -------------------------------------------------------------------------- */

ENDSUBOBAENV

/* -------------------------------------------------------------------------- */



