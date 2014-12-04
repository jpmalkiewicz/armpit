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

	BNDREG	"sysc",    sys_ctrl		@ sysc <- SYSAHBCLKCTRL
	BNDREG	"VIC",	   int_enab_base	@ VIC  <- 0xe000e100 (int enab)
	BNDREG	"rtc0",	   rtc0_base
	BNDREG	"gio0",	   io0_base
	BNDREG	"gio1",	   io1_base
	BNDREG	"gio2",	   io2_base
	BNDREG	"gio3",	   io3_base
	BNDREG	"gio4",	   io4_base
	BNDREG	"tmr0",	   timer0_base
	BNDREG	"tmr1",	   timer1_base
	BNDREG	"i2c0",	   i2c0_base
	BNDREG	"i2c1",	   i2c1_base
	BNDREG	"spi0",	   spi0_base
	BNDREG	"spi1",	   spi1_base
	BNDREG	"pwm1",	   pwm1_base
	BNDREG	"adc0",	   adc0_base
	BNDREG	"psl0",	   PINSEL0		@ psl0 <- pinsel0
	BNDREG	"pmd0",	   PINMODE0		@ pmd0 <- pinmode0

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
	@ sets or clears bit in PCONP
	@ in:	sv1 <- bit position	(scheme int)
	@ in:	sv2 <- val (1 or 0)	(scheme int)
	@ out:	sv1 <- npo
	set	sv3, sv1
	set	sv4, sv2
	set	rva, sys_ctrl
	set	rvb, 0xc4
	b	rcpbit

	/* (config-pin main sub cfg . <mod> <od>) */
	PRIMIT	"config-pin", cfgpn, pfun, 3
	@ configure a pin, from P0.0 to P2.13
	@ cfg:		#b00 to #b11
	@ <mod>:	#b00 = pull-up, #b01 = repeater, #b10 = no pull-up/dn, #b11 = pull-down
	@ <od>:		0 or 1, open drain
	@ in:	sv1 <- main (0 to 2)				(scheme int)
	@ in:	sv2 <- sub  (0 to 31)				(scheme int)
	@ in:	sv3 <- cfg (eg. #b01)				(scheme int)
	@ in:	sv4 <- (<mod> <od>) = rest of input args	(list)
	@ out:	sv1 <- npo
	int2raw	rvc, sv1		@ rvc <- main part of pin (raw int)
	lsl	rvc, rvc, #3		@ rvc <- pinsel/mod offset from pinsel0/mod0
	cmp	sv2, #65		@ is pin (sub) > 15?
	it	pl
	addpl	rvc, rvc, #4		@	if so, rvc <- offset adjusted for pins 16-31
	raw2int	sv5, rvc		@ sv5 <- pinsel/mod offset, saved (scheme int)
	@ update pin configuration
	set	rva, PINSEL0		@ rva <- pinsel base
	set	sv1, sv3		@ sv1 <- cfg
	bl	pcfgph			@ rvb <- updtd pin cfg word, rvc <- ofst
	write	rvb, PINSEL0, rvc	@ update pin configuration in pinsel
	@ update pin mode
	nullp	sv4			@ mod specified?
	beq	adr_npofxt			@	if not, return
	set	rva, PINMODE0		@ rva <- pinmod base
	snoc	sv1, sv4, sv4		@ sv1 <- mod, sv4 <- (<od>)
	bl	pcfgph			@ rvb <- updtd pin mode wrd, rvc <- ofst
	write	rvb, PINMODE0, rvc
	@ update pin open-drain functionality
	nullp	sv4			@ open-drain specified?
	beq	adr_npofxt			@	if not, return
	set	sv3, sv2		@ sv3 <- sub, 0 to 31 = bit pos	(scheme int)
	car	sv4, sv4		@ sv4 <- od, 0 or 1		(scheme int)
	set	rva, PINMODE_OD0	@ rva <- pinmod open-drain base address
	lsr	rvb, rvc, #1		@ rvb <- OD register offset, unaligned
	bic	rvb, rvb, #0x03		@ rvb <- OD register offset, aligned
	b	rcpbit			@ jump to register-copy-bit


_func_
pcfgph:	@ update configuration/mode bits helper function
	@ in:	rva <- pinsel/pinmod base address
	@ in:	rvc <- offset of pin conf/mod, from base	(raw int)
	@ in:	sv1 <- new configuration/mode			(scheme int)
	@ in:	sv2 <- sub (input arg of config-pin function)	(scheme int)
	@ in:	sv5 <- offset of pin conf/mod, from base	(scheme int)
	@ out:	rvb <- updated configuration/mode word		(raw int)
	@ modifies:	rva, rvb, rvc
	read	rva, rva, rvc		@ rva <- current pin configurations/mode
	int2raw	rvc, sv2		@ rvc <- bit mask position/2
	and	rvc, rvc, #0x0f		@ rvc <- bit mask position modulo 16
	lsl	rvc, rvc, #1		@ rvc <- mask position
	ash	rvb, 3, rvc		@ rvb <- mask, shifted in place
	bic	rva, rva, rvb		@ rva <- pin configurations/modes with mask cleared
	int2raw	rvb, sv1		@ rvb <- new configuration/mode
	lsl	rvb, rvb, rvc		@ rvb <- new configuration/mode, shifted in place
	orr	rvb, rva, rvb		@ rvb <- full new configuration/mode
	int2raw	rvc, sv5		@ rvc <- pinsel/mod offset
	set	pc,  lnk

	/* (pin-set-dir port pin dir) */
	PRIMIT	"pin-set-dir", pstdr, pfun, 3, oregent, null
	@ in:	sv1 <- port (gio0 to gio4)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ in:	sv3 <- dir  (0=input, 1=output)	(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gio0-4 as full address (from sv1, through regent)
	set	rvb, io_dir		@ rvb <- 0x08 = offset to pin dir reg in gio0-4
	set	sv4, sv3
	set	sv3, sv2
	b	rcpbit

	/* (pin-set port pin) */
	PRIMIT	"pin-set", pnset, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio0 to gio4)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gio0-4 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	write	rvc, rva, #io_set
	b	adr_npofxt

	/* (pin-clear port pin) */
	PRIMIT	"pin-clear", pnclr, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio0 to gio4)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gio0-4 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	write	rvc, rva, #io_clear
	b	adr_npofxt

	/* (pin-set? port pin) */
	PRIMIT	"pin-set?", pnstq, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio0 to gio4)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- #t/#f pin status (#t = high)
	@ rva <- gio0-4 as full address (from sv1, through regent)
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



