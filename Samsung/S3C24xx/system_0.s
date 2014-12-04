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
@-------------------------------------------------------------------------------*/

/* ==========================================================================

	start of sub-environment and sub-obarray

	(don't forget ENDSUBOBAENV at end of file)

========================================================================== */

	STARTSUBOBAENV system_0

/*------------------------------------------------------------------------------
@  register address bindings
@-----------------------------------------------------------------------------*/

	BNDREG	"sysc",    sys_ctrl		@ sysc <- sys_ctrl
	BNDREG	"VIC",	   int_base
	BNDREG	"rtc0",	   rtc0_base
	BNDREG	"gioa",	   ioA_base
	BNDREG	"giob",	   ioB_base
	BNDREG	"gioc",	   ioC_base
	BNDREG	"giod",	   ioD_base
	BNDREG	"gioe",	   ioE_base
	BNDREG	"giof",	   ioF_base
	BNDREG	"giog",	   ioG_base
	BNDREG	"gioh",	   ioH_base
	BNDREG	"tmr0",	   timer0_base
	BNDREG	"i2c0",	   i2c0_base
	BNDREG	"i2c1",	   i2c1_base
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
	@ sets/clears bit in CLKCON (peripherals enabled by default at startup)
	@ in:	sv1 <- bit position	(scheme int)
	@ in:	sv2 <- val (1 or 0)	(scheme int)
	@ out:	sv1 <- npo
	set	sv3, sv1
	set	sv4, sv2
	set	rva, sys_ctrl
	set	rvb, 0x0c
	b	rcpbit

	/* (config-pin port pin cfg . <pup>) */
	PRIMIT	"config-pin", cfgpn, pfun, 3, oregent, null
	@ port:		giob-gioh, port
	@ pin:		0-15, pin on port
	@ cfg:		0-3,  #b00=input, #b01=output, #b10=AF 1, #b11=AF 2
	@ pup:		0/1,  1 = enable pull-up (default)
	@ in:	sv1 <- port (giob to gioh)		(scheme int)
	@ in:	sv2 <- pin  (0 to 15)			(scheme int)
	@ in:	sv3 <- cfg  (0 to  3)			(scheme int)
	@ in:	sv4 <- (<pup>)				(list)
	@ out:	sv1 <- npo
	@ rva <- giob-h as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	@ configure pin mode
	set	sv5, rva
	ldr	rvc, [sv5, #0x00]
	and	rvb, rvb, #0x0f
	lsl	rvb, rvb, #1
	ash	rva, 3, rvb
	bic	rvc, rvc, rva
	int2raw	rva, sv3
	and	rva, rva, #0x03
	lsl	rva, rva, rvb
	orr	rvc, rvc, rva
	str	rvc, [sv5, #0x00]
	@ configure pull-up
	pntrp	sv4
	bne	adr_npofxt
	car	sv4, sv4
	int2raw	rvb, sv2
	and	rvb, rvb, #0x0f
	ash	rvc, 1, rvb
	ldr	rva, [sv5, #0x08]
	eq	sv4, #i0		@ disable pull-up?
	orreq	rva, rva, rvc		@	if so,  set   disable pullup bit
	bicne	rva, rva, rvc		@	if not, clear disable pullup bit
	str	rva, [sv5, #0x08]
	b	adr_npofxt

	/* (pin-set port pin) */
	PRIMIT	"pin-set", pnset, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gio0/1 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	swi	run_no_irq
	ldr	rvb, [rva, #io_state]	@ rvb <- pin statuses
	orr	rvb, rvb, rvc
	str	rvb, [rva, #io_state]	@ set pin
	swi	run_normal
	b	adr_npofxt

	/* (pin-clear port pin) */
	PRIMIT	"pin-clear", pin_clear, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gio0/1 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	swi	run_no_irq
	ldr	rvb, [rva, #io_state]	@ rvb <- pin statuses
	bic	rvb, rvb, rvc
	str	rvb, [rva, #io_state]	@ clear pin
	swi	run_normal
	b	adr_npofxt

	/* (pin-set? port pin) */
	PRIMIT	"pin-set?", pin_setp, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- #t/#f pin status (#t = high)
	@ rva <- gio0/1 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	ldr	rvb, [rva, #io_state]
	tst	rvb, rvc
	b	adr_notfxt

	/* (stop tmr) */
	PRIMIT	"stop", pfun, 2, oregent, null
	@ stops channel (0-4) of tmr0
	@ in:	sv1 <- tmr (tmr0 only)	(scheme int)
	@ in:	sv2 <- chan = 0 to 4, or null
	@ out:	sv1 <- npo
	nullp	sv2			@ was channel specified?
	seteq	rvb, 0			@	if not, assume channel 0
	eq	rvb, #0			@ is channel = 0?
	lslne	rvb, rvb, #2		@	if not, rvb <- 4*channel
	addne	rvb, rvb, #4		@	if not, rvb <- 4*channel+4=offst
	set	rvc, 0x1f		@ rvc <- channel clearing code
	lsl	rvc, rvc, rvb		@ rvc <- clearing code shiftd to channel
	ldr	rvb, [rva, #0x08]	@ rvb <- current timer control
	bic	rvb, rvb, rvc		@ rvb <- timer control with channel clrd
	str	rvb, [rva, #0x08]	@ set updated control in timer
	b	adr_npofxt

	/* (restart tmr) */
	PRIMIT	"restart", pfun, 1, oregent, null
	@ restarts tmr0 in one-shot mode
	@ in:	sv1 <- tmr (tmr0 only)	(scheme int)
	@ out:	sv1 <- npo
	ldr	rvb, [rva, #0x08]
	bic	rvb, rvb, #0x0f
	str	rvb, [rva, #0x08]
	orr	rvb, rvb, #0x02
	str	rvb, [rva, #0x08]
	eor	rvb, rvb, #0x03
	str	rvb, [rva, #0x08]
	b	adr_npofxt

	/* (spi-put port val) */
	PRIMIT	"spi-put", spput, pfun, 2, oregent, null
	@ in:	sv1 <- port (spi0 or spi1)	(scheme int)
	@ in:	sv2 <- val
	@ out:	sv1 <- npo
	ldr	rvc, [rva, #spi_status]	@ ssta
	tst	rvc, #spi_txrdy
	beq	adr_spput
	str	rvb, [rva, #spi_thr]
	b	adr_npofxt

	/* (spi-get port) */
	PRIMIT	"spi-get", spget, pfun, 1, oregent, null
	@ in:	sv1 <- port (spi0 or spi1)	(scheme int)
	@ out:	sv1 <- data from spi		(scheme int)
	ldr	rvb, [rva, #spi_status]
	tst	rvb, #spi_rxrdy
	beq	adr_spget
	ldr	rvb, [rva, #spi_rhr]
	raw2int	sv1, rvb
	set	pc,  cnt

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/* -------------------------------------------------------------------------- */

ENDSUBOBAENV

/* -------------------------------------------------------------------------- */



