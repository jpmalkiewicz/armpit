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

	BNDREG	"scu",	   sys_ctrl		@ scu  <- SCU base
	BNDREG	"VIC",	   int_base
	BNDREG	"rtc0",	   rtc0_base
	BNDREG	"gio0",	   ioport0_base
	BNDREG	"gio1",	   ioport1_base
	BNDREG	"gio2",	   ioport2_base
	BNDREG	"gio3",	   ioport3_base
	BNDREG	"gio4",	   ioport4_base
	BNDREG	"gio5",	   ioport5_base
	BNDREG	"gio6",	   ioport6_base
	BNDREG	"gio7",	   ioport7_base
	BNDREG	"gio8",	   ioport8_base
	BNDREG	"gio9",	   ioport9_base
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
	@ sets/clears bit in SCU_PCGR1
	@ asserts/de-asserts reset in SCU_PRR1
	@ in:	sv1 <- bit position	(scheme int)
	@ in:	sv2 <- val (1 or 0)	(scheme int)
	@ out:	sv1 <- npo
	int2raw	rvb, sv1
	ash	rvc, 1, rvb
	@ set/clear power bit
	read	rvb, sys_ctrl, #0x18
	eq	sv2, #i1
	orreq	rvb, rvb, rvc
	bicne	rvb, rvb, rvc
	write	rvb, rva, #0x18		@ SCU_PCGR1 <- updated power state
	@ set/clear reset bit
	read	rvb, rva, #0x20
	eq	sv2, #i1
	orreq	rvb, rvb, rvc
	bicne	rvb, rvb, rvc
	write	rvb, rva, #0x20		@ SCU_PRR1 <- updated reset state
	b	adr_npofxt

	/* (config-pin main sub . <ocfg> <afin> <otyp> <ana>) */
	PRIMIT	"config-pin", cfgpn, pfun, 2
	@ main:		0-7, port
	@ sub:		0-7, pin on port
	@ ocfg:		0-3, #b00=input, #b01=alt output 1, #b10=alt out 2, #b11=alt out 3
	@ afin:		0/1, alternate input 1
	@ otyp:		0/1, 0 = push-pull, 1 = open collector (gpio output only)
	@ ana:		0/1, 1 = analog in (gpio4 only)
	@ in:	sv1 <- main (0 or 7)			(scheme int)
	@ in:	sv2 <- sub  (0 to 7)			(scheme int)
	@ in:	sv3 <- (<ocfg> <afin> <otyp> <ana>)	(list)
	@ out:	sv1 <- npo
	set	sv4, sv1
	int2raw	rvb, sv1
	set	rva, sys_ctrl
	add	rva, rva, rvb, lsl #2
	@ configure output mode
	pntrp	sv3
	bne	adr_npofxt
	snoc	sv1, sv3, sv3
	read	rvb, rva, #0x44
	orr	sv5, rva, #i0		@ sv5 <- rva, saved
	int2raw	rvc, sv2
	and	rvc, rvc, #0x07
	lsl	rvc, rvc, #1
	ash	rva, 3, rvc
	bic	rvb, rvb, rva
	int2raw	rva, sv1
	and	rva, rva, #0x03
	lsl	rva, rva, rvc
	orr	rvb, rvb, rva
	bic	rva, sv5, #i0		@ rva <- rva, restored
	write	rvb, rva, #0x44
	@ set single bit mask
	int2raw	rvb, sv2
	ash	rvc, 1, rvb
	@ configure input mode
	read	rvb, rva, #0x64
	bl	pcfgph
	write	rvb, rva, #0x64
	@ configure output type
	read	rvb, rva, #0x84
	bl	pcfgph
	write	rvb, rva, #0x84
	@ configure analog input
	eq	sv4, #((4 << 2) | i0)
	bne	adr_npofxt
	read	rvb, sys_ctrl, #0xbc
	bl	pcfgph
	write	rvb, rva,      #0xbc
	b	adr_npofxt
	
pcfgph:	@ pin configuration helper
	pntrp	sv3
	bne	adr_npofxt
	snoc	sv1, sv3, sv3
	eq	sv1, #i1
	orreq	rvb, rvb, rvc
	bicne	rvb, rvb, rvc
	set	pc,  lnk

	/* (pin-set-dir port pin dir) */
	PRIMIT	"pin-set-dir", pnstdr, pfun, 3, oregent, null
	@ in:	sv1 <- port (gio0 to gio7)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ in:	sv3 <- dir  (0=input, 1=output)	(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gio0-7 as full address (from sv1, through regent)
	set	rvb, io_dir		@ rvb <- 0x08 = offset to pin dir reg in gio0/1
	set	sv4, sv3
	set	sv3, sv2
	b	rcpbit

	/* (pin-set port pin) */
	PRIMIT	"pin-set", pnset, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio0-7)		(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gio0-7 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 4, rvb
	write	0xff, rva, rvc
	b	adr_npofxt

	/* (pin-clear port pin) */
	PRIMIT	"pin-clear", pnclr, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio0-7)		(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gio0-7 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 4, rvb
	write	0x00, rva, rvc
	b	adr_npofxt

	/* (pin-set? port pin) */
	PRIMIT	"pin-set?", pnstq, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio0-7)		(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- #t/#f pin status (#t = high)
	@ rva <- gio0-7 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	read	rvb, rva, #io_state
	tst	rvb, rvc
	b	adr_notfxt

	/* (stop tmr) */
	PRIMIT	"stop", pfun, 1, oregent, null
	@ in:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ out:	sv1 <- npo
	write	0, rva, #0x14
	b	adr_npofxt

	/* (restart tmr) */
	PRIMIT	"restart", pfun, 1, oregent, null
	@ in:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ out:	sv1 <- npo
	write	0x8000, rva, #0x14
	write	0,      rva, #0x10	@ Note:	this may be read-only
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



