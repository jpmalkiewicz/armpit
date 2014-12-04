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
	BNDREG	"psl0",	   PINSEL0		@ psl0 <- pinsel0
	BNDREG	"rtc0",	   rtc0_base
	BNDREG	"gio0",	   io0_base
	BNDREG	"gio1",	   io1_base
	BNDREG	"tmr0",	   timer0_base
	BNDREG	"tmr1",	   timer1_base
	BNDREG	"i2c0",	   i2c0_base
	BNDREG	"i2c1",	   i2c1_base
	BNDREG	"spi0",	   spi0_base
	BNDREG	"spi1",	   spi1_base
	BNDREG	"pwm0",	   pwm0_base
	BNDREG	"adc0",	   adc0_base
  .ifndef LPC2478_STK
	BNDREG	"adc1",	   adc1_base
  .endif
  .ifdef LPC2478_STK
	BNDREG	"pwm1",	   pwm1_base
	BNDREG	"pmd0",	   pmod0_base		@ pmd0 <- pinmod0
	BNDREG	"mci",	   mci_base
	BNDREG	"gdma",	   gdma_base		@ gdma <- gpdma
	BNDREG	"bdma",	   bdma_base		@ bdma <- dma 512B buffr
  .endif

/*------------------------------------------------------------------------------
@  utility functions
@-----------------------------------------------------------------------------*/

	/* (RNG seed) */
	@ Park-Miller next number from seed
	simple_random_gen		@ ARMv4T, ARMv5TEJ, ARMv7A macro


	/* (config-power bit val) */
	PRIMIT	"config-power", cfpwr, pfun, 2
	@ in:	sv1 <- bit position	(scheme int)
	@ in:	sv2 <- val (1 or 0)	(scheme int)
	@ out:	sv1 <- npo
	set	sv3, sv1
	set	sv4, sv2
	set	rva, sys_ctrl
	set	rvb, 0xc4
	b	rcpbit

	/* (config-pin main sub cfg <mod>) */
	PRIMIT	"config-pin", cfgpn, pfun, 4
	@ in:	sv1 <- main (0 or 1)		(scheme int)
	@ in:	sv2 <- sub  (0 to 31)		(scheme int)
	@ in:	sv3 <- cfg (eg. #b01)		(scheme int)
	@ in:	sv4 <- mod or null (eg. #b11)	(scheme int or no arg)
	@ out:	sv1 <- npo
	int2raw	rvc, sv1
	lsl	rvc, rvc, #3
	cmp	sv2, #65
	addpl	rvc, rvc, #4
	raw2int	sv5, rvc
	read	rva, PINSEL0, rvc
	int2raw	rvc, sv2
	and	rvc, rvc, #0x0f
	lsl	rvc, rvc, #1
	ash	rvb, 3, rvc
	bic	rva, rva, rvb
	int2raw	rvb, sv3
	lsl	rvb, rvb, rvc
	orr	rvb, rva, rvb
	int2raw	rvc, sv5
	write	rvb, PINSEL0, rvc
  .ifdef LPC2478_STK
	int2raw	rvc, sv5
	read	rva, pmod0_base, rvc
	int2raw	rvc, sv2
	and	rvc, rvc, #0x0f
	lsl	rvc, rvc, #1
	ash	rvb, 3, rvc
	bic	rva, rva, rvb
	int2raw	rvb, sv4
	lsl	rvb, rvb, rvc
	orr	rvb, rva, rvb
	int2raw	rvc, sv5
	write	rvb, pmod0_base, rvc
  .endif
	b	adr_npofxt

	/* (pin-set-dir port pin dir) */
	PRIMIT	"pin-set-dir", pstdr, pfun, 3, oregent, null
	@ in:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ in:	sv3 <- dir  (0=input, 1=output)	(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gio0/1 as full address (from sv1, through regent)
	set	rvb, io_dir		@ rvb <- 0x08 = offset to pin dir reg in gio0/1
	set	sv4, sv3
	set	sv3, sv2
	b	rcpbit

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
	write	0, rva, #0x04
	b	adr_npofxt

	/* (restart tmr) */
	PRIMIT	"restart", pfun, 1, oregent, null
	@ in:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ out:	sv1 <- npo
	write	2, rva, #0x04
	write	1, rva, #0x04
	b	adr_npofxt

  .ifndef LPC2478_STK

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
	write	rvb, rva, #spi_rhr
	raw2int	sv1, rvb
	set	pc,  cnt

  .endif @ .ifndef LPC2478_STK


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/* -------------------------------------------------------------------------- */

ENDSUBOBAENV

/* -------------------------------------------------------------------------- */



