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

	BNDREG	"sysc",    sys_ctrl		@ sysc <- sys_ctrl
	BNDREG	"VIC",	   int_base
	BNDREG	"rtc0",	   rtc0_base
	BNDREG	"gioa",	   ioA_base
	BNDREG	"gioe",	   ioE_base
	BNDREG	"giof",	   ioF_base
	BNDREG	"gioh",	   ioH_base
	BNDREG	"tmr0",	   timer0_base
	BNDREG	"tmr1",	   timer1_base
	BNDREG	"i2c0",	   i2c0_base
	BNDREG	"i2c1",	   i2c1_base
	BNDREG	"spi0",	   spi0_base
	BNDREG	"adc0",	   adc0_base
	BNDREG	"pwm0",	   pwm0_base
	BNDREG	"pwm1",	   pwm1_base

/*------------------------------------------------------------------------------
@  utility functions
@-----------------------------------------------------------------------------*/

	/* (RNG seed) */
	@ Park-Miller next number from seed
	simple_random_gen		@ ARMv4T, ARMv5TEJ, ARMv7A macro


	/* (config-power bit val) */
	PRIMIT	"config-power", cfpwr, pfun, 2
	@ sets/clears bit in DeviceCfg (peripherals are enabled by default at startup)
	@ in:	sv1 <- bit position	(scheme int)
	@ in:	sv2 <- val (1 or 0)	(scheme int)
	@ out:	sv1 <- npo
	set	sv3, sv1
	set	sv4, sv2
	write	0xaa, sys_ctrl, #0xc0	@ unlock syscon registers
	set	rvb, 0x80
	b	rcpbit

	/* (pin-set-dir port pin dir) */
	PRIMIT	"pin-set-dir", pin_set_dir, pfun, 3, oregent, null
	@ in:	sv1 <- port (gioa, gioe, giof or gioh)	(scheme int)
	@ in:	sv2 <- pin  (0 to 7)			(scheme int)
	@ in:	sv3 <- dir  (0=input, 1=output)		(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gioa, gioe, giof or gioh as full address (from sv1, through regent)
	and	rvc, rva, #0xff
	cmp	rvc, #0x10
	setmi	rvb, io_dir		@ rvb <- 0x08 = offset to pin dir reg in gioa-d
	setpl	rvb, io_dir_high
	set	sv4, sv3
	set	sv3, sv2
	b	rcpbit

	/* (pin-set port pin) */
	PRIMIT	"pin-set", pnset, pfun, 2, oregent, null
	@ in:	sv1 <- port (gioa, gioe, giof or gioh)	(scheme int)
	@ in:	sv2 <- pin				(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gioa, gioe, giof or gioh as full address (from sv1, through regent)
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
	@ in:	sv1 <- port (gioa, gioe, giof or gioh)	(scheme int)
	@ in:	sv2 <- pin				(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gioa, gioe, giof or gioh as full address (from sv1, through regent)
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
	@ in:	sv1 <- port (gioa, gioe, giof or gioh)	(scheme int)
	@ in:	sv2 <- pin				(scheme int)
	@ out:	sv1 <- #t/#f pin status (#t = high)
	@ rva <- gioa, gioe, giof or gioh as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	ldr	rvb, [rva, #io_state]
	tst	rvb, rvc
	b	adr_notfxt

	/* (stop tmr) */
	PRIMIT	"stop", pfun, 1, oregent, null
	@ in:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ out:	sv1 <- npo
	write	0, rva, #0x08
	b	adr_npofxt

	/* (restart tmr) */
	PRIMIT	"restart", pfun, 1, oregent, null
	@ in:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ out:	sv1 <- npo
	write	0xc8, rva, #0x08
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



