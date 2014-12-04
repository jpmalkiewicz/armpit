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
	BNDREG	"EIC",	   int_base
	BNDREG	"rtc0",	   rtc0_base
	BNDREG	"gio0",	   ioport0_base
	BNDREG	"gio1",	   ioport1_base
	BNDREG	"tmr0",	   timer0_base		@ tmr0 (armpit timer 0 w/r interrupts)
	BNDREG	"tmr1",	   timer1_base		@ tmr1 (armpit timer 1 w/r interrupts)
	BNDREG	"tmr2",	   timer2_base
	BNDREG	"tmr3",	   timer3_base
	BNDREG	"i2c0",	   i2c0_base		@ i2c0 (armpit I2C0 port)
	BNDREG	"i2c1",	   i2c1_base		@ i2c1 (armpit I2C1 port)
	BNDREG	"spi0",	   spi0_base
	BNDREG	"spi1",	   spi1_base
	BNDREG	"adc0",	   adc0_base

/*------------------------------------------------------------------------------
@  utility functions
@-----------------------------------------------------------------------------*/

	/* (RNG seed) */
	@ Park-Miller next number from seed
	simple_random_gen		@ ARMv4T, ARMv5TEJ, ARMv7A macro


	/* (config-power bit val) */
	PRIMIT	"config-power", cfpwr, pfun, 2
	@ sets/clears bit in PCU_BOOTCR (only BSPI0 and ADC are in there)
	@ in:	sv1 <- bit position	(scheme int)
	@ in:	sv2 <- val (1 or 0)	(scheme int)
	@ out:	sv1 <- npo
	set	sv3, sv1
	set	sv4, sv2
	set	rva, rcc_base
	set	rvb, 0x50
	b	rcpbit

	/* (config-pin main sub cfg) */
	PRIMIT	"config-pin", cfgpn, pfun, 3
	@ main:		0/1,  port
	@ sub:		0-15, pin on port
	@ cfg:		3-bit value for PC2, PC1, PC0,
	@		#b000 = analog input,	#b001  = TTL input
	@		#b010 = CMOS input,	#b011  = input with pull-up/dn
	@		#b100 = open-drain out,	#b101 = push-pull output
	@		#b110 = AF open-drain,	#b111 = AF push-pull
	@ in:	sv1 <- main (0 or 1)		(scheme int)
	@ in:	sv2 <- sub  (0 to 15)		(scheme int)
	@ in:	sv3 <- cfg  (eg. #b001)		(scheme int)
	@ out:	sv1 <- npo
	intgrp	sv3
	bne	adr__err
	set	rva, ioport1_base
	eq	sv1, #i0
	seteq	rva, ioport0_base
	eqne	sv1, #i1
	bne	adr__err
	int2raw	rvb, sv2
	ash	rvc, 1, rvb
	@ configure PC0
	read	rvb, rva, #0x00
	tst	sv3, #(1<<2)
	biceq	rvb, rvb, rvc
	orrne	rvb, rvb, rvc
	write	rvb, rva, #0x00
	@ configure PC1
	read	rvb, rva, #0x04
	tst	sv3, #(1<<3)
	biceq	rvb, rvb, rvc
	orrne	rvb, rvb, rvc
	write	rvb, rva, #0x04
	@ configure PC2
	read	rvb, rva, #0x08
	tst	sv3, #(1<<4)
	biceq	rvb, rvb, rvc
	orrne	rvb, rvb, rvc
	write	rvb, rva, #0x08
	b	adr_npofxt

	/* (pin-set port pin) */
	PRIMIT	"pin-set", pnset, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ pre-entry:	regent sets rva to reg adrs (sv1), rvb to ofst (raw sv2)
	@ rva <- gio0/1 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	swi	run_no_irq
	rgrmw	rva, #io_state, rvc	@ set pin
	swi	run_normal
	b	adr_npofxt

	/* (pin-clear port pin) */
	PRIMIT	"pin-clear", pnclr, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ pre-entry:	regent sets rva to reg adrs (sv1), rvb to ofst (raw sv2)
	@ rva <- gio0/1 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	swi	run_no_irq
	rgrmw	rva, #io_state, rvc,xxx	@ clear pin
	swi	run_normal
	b	adr_npofxt

	/* (pin-set? port pin) */
	PRIMIT	"pin-set?", pnstq, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- #t/#f pin status (#t = high)
	@ pre-entry:	regent sets rva to reg adrs (sv1), rvb to ofst (raw sv2)
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
	@ pre-entry:	regent sets rva to reg adrs (sv1), rvb to ofst (raw sv2)
	write	0, rva, #0x14
	b	adr_npofxt

	/* (restart tmr) */
	PRIMIT	"restart", pfun, 1, oregent, null
	@ in:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ out:	sv1 <- npo
	@ pre-entry:	regent sets rva to reg adrs (sv1), rvb to ofst (raw sv2)
	write	0x8000, rva, #0x14
	write	0,      rva, #0x10
	b	adr_npofxt

	/* (spi-put port val) */
	PRIMIT	"spi-put", spput, pfun, 2, oregent, null
	@ in:	sv1 <- port (spi0 or spi1)	(scheme int)
	@ in:	sv2 <- val
	@ out:	sv1 <- npo
	@ pre-entry:	regent sets rva to reg adrs (sv1), rvb to ofst (raw sv2)
	rgwfbt	rva, #spi_status, spi_txrdy_bit, 1, rvc
	lsl	rvb, rvb, #8
	write	rvc, rva, #spi_thr
	b	adr_npofxt

	/* (spi-get port) */
	PRIMIT	"spi-get", spget, pfun, 1, oregent, null
	@ in:	sv1 <- port (spi0 or spi1)	(scheme int)
	@ out:	sv1 <- data from spi		(scheme int)
	@ pre-entry:	regent sets rva to reg adrs (sv1), rvb to ofst (raw sv2)
	rgwfbt	rva, #spi_status, spi_rxrdy_bit, 1
	read	rvb, rva, #spi_rhr
	lsr	rvb, rvb, #8
	raw2int	sv1, rvb
	set	pc,  cnt


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/* -------------------------------------------------------------------------- */

ENDSUBOBAENV

/* -------------------------------------------------------------------------- */



