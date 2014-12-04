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
	BNDREG	"gio0",	   io0_base
	BNDREG	"gio1",	   io1_base
	BNDREG	"gio2",	   io2_base
	BNDREG	"gio3",	   io3_base
	BNDREG	"gio4",	   io4_base
	BNDREG	"gio5",	   io5_base
	BNDREG	"gio6",	   io6_base
	BNDREG	"gio7",	   io7_base
	BNDREG	"tmr0",	   timer0_base
	BNDREG	"tmr1",	   timer1_base
	BNDREG	"i2c0",	   i2c0_base
	BNDREG	"adc0",	   adc0_base
	BNDREG	"mci",	   mci_base
	BNDREG	"gdma",	   gdma_base		@ gdma <- gpdma
	BNDREG	"lcd",	   lcd_base

/*------------------------------------------------------------------------------
@  utility functions
@-----------------------------------------------------------------------------*/

	/* (RNG seed) */
	@ Park-Miller next number from seed
	simple_random_gen		@ ARMv4T, ARMv5TEJ, ARMv7A macro


	/* (config-pin main sub m0 m1) */
	PRIMIT	"config-pin", cfgpn, pfun, 4
	@ m0 m1:	0 0 -> GP input,	0 1 -> GP output low
	@		1 0 -> peripheral func,	1 1 -> GP output high
	@ in:	sv1 <- main (0 to  7)		(scheme int)
	@ in:	sv2 <- sub  (0 to 31)		(scheme int)
	@ in:	sv3 <- m0   (0 or  1)		(scheme int)
	@ in:	sv4 <- m1   (0 or  1)		(scheme int)
	@ out:	sv1 <- npo
	@ calculate address
	int2raw	rvc, sv1
	set	rvb, 0x40
	mul	rvb, rvc, rvb
	set	rva, io0_base
	add	rva, rva, rvb
	@ position update bit
	int2raw	rvc, sv2
	ash	rvb, 1, rvc
	@ set MODE 0
	eq	sv3, #i1
	writeeq	rvb, rva, #0x14
	writene	rvb, rva, #0x18
	@ set MODE 1
	eq	sv4, #i1
	writeeq	rvb, rva, #0x24
	writene	rvb, rva, #0x28
	@ return
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
	write	0, rva, #0x08
	b	adr_npofxt

	/* (restart tmr) */
	PRIMIT	"restart", pfun, 1, oregent, null
	@ in:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ out:	sv1 <- npo
	write	2344, rva, #0x00
	write	0xc8, rva, #0x08
	b	adr_npofxt


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/* -------------------------------------------------------------------------- */

ENDSUBOBAENV

/* -------------------------------------------------------------------------- */



