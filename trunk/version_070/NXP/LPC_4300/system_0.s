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

	BNDREG	"VIC",	   int_enab_base	@ VIC  <- 0xe000e100 (int enab)
	BNDREG	"cgu",	   CGU_base
	BNDREG	"rtc0",	   rtc0_base
	BNDVAR	"gio0",	   i0
	BNDVAR	"gio1",	   i1
	BNDVAR	"gio2",	   i2
	BNDVAR	"gio3",	   i3
	BNDVAR	"gio4",	   i4
	BNDVAR	"gio5",	   i5
	BNDVAR	"gio6",	   i6
	BNDVAR	"gio7",	   i7
	BNDREG	"tmr0",	   timer0_base
	BNDREG	"tmr1",	   timer1_base
	BNDREG	"i2c0",	   i2c0_base
	BNDREG	"i2c1",	   i2c1_base
	BNDREG	"spi0",	   spi0_base
	BNDREG	"spi1",	   spi1_base
	BNDREG	"pwm1",	   pwm1_base
	BNDREG	"adc0",	   adc0_base
	BNDREG	"mmc",	   mmc_base
	BNDREG	"lcdb",	   lcd_base

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


	/* (config-pin main sub cfg) */
	PRIMIT	"config-pin", cfgpn, pfun, 3
	@ configure a pin, from P0_0 to PF_11
	@ cfg:		#b gifudmmm, bits: 	
	@		g=filter,i=inbuffer,f=fast,u=~pullup,d=pulldown,mmm=mode
	@ in:	sv1 <- main (0 to 15 for P0 to PF)	(scheme int)
	@ in:	sv2 <- sub  (0 to 15 for Pn_0 to Pn_15)	(scheme int)
	@ in:	sv3 <- cfg (eg. #x00)			(scheme int)
	@ out:	sv1 <- npo
	int2raw	rvc, sv1		@ rvc <- main part of pin (raw int)
	set	rvb, 0x80
	mul	rvb, rvb, rvc
	set	rva, SCU_SFSP0_n	@ rva <- SCU Pin cfg reg for P0_0
	add	rva, rva, rvb		@ rva <- SCU Pin cfg reg for Pmain_0
	bic	rvb, sv2, #3
	add	rva, rva, rvb		@ rva <- SCU Pin cfg reg for Pmain_sub
	int2raw	rvb, sv3
	write	rvb, rva, #0
	b	adr_npofxt

	/* (pin-set-dir port pin dir) */
	PRIMIT	"pin-set-dir", pstdr, pfun, 3
	@ in:	sv1 <- port (gio0 to gio7)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ in:	sv3 <- dir  (0=input, 1=output)	(scheme int)
	@ out:	sv1 <- npo
	set	rva, io0_base
	add	rva, rva, sv1
	bic	rva, rva, #3
	set	rvb, io_dir		@ rvb <- offset to pin dir reg in gio0-4
	set	sv4, sv3
	set	sv3, sv2
	b	rcpbit

	/* (pin-set port pin) */
	PRIMIT	"pin-set", pnset, pfun, 2
	@ in:	sv1 <- port (gio0 to gio7)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	set	rva, io0_base
	add	rva, rva, sv1
	bic	rva, rva, #3
	int2raw	rvb, sv2
	ash	rvc, 1, rvb
	write	rvc, rva, #io_set
	b	adr_npofxt

	/* (pin-clear port pin) */
	PRIMIT	"pin-clear", pnclr, pfun, 2
	@ in:	sv1 <- port (gio0 to gio7)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	set	rva, io0_base
	add	rva, rva, sv1
	bic	rva, rva, #3
	int2raw	rvb, sv2
	ash	rvc, 1, rvb
	write	rvc, rva, #io_clear
	b	adr_npofxt

	/* (pin-set? port pin) */
	PRIMIT	"pin-set?", pnstq, pfun, 2
	@ in:	sv1 <- port (gio0 to gio7)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- #t/#f pin status (#t = high)
	set	rva, io0_base
	add	rva, rva, sv1
	bic	rva, rva, #3
	int2raw	rvb, sv2
	ash	rvc, 1, rvb
	write	rvb, rva, #io_dir	@ rvb <- pin directions
	tst	rvb, rvc		@ is this an input pin w/input buffer?
	itE	eq
	readeq	rvb, rva, #io_state	@	if so,  rvb <- input  pin status
	readne	rvb, rva, #io_set	@	if not, rvb <- output pin status
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

	/* (bnld bytevector) */
	PRIMIT	"bnld", pfun, 1
	@ copy code (.text) to destination
	ldr	rva, [sv1, #0]		@ rvc <- src/target end offset
	ldr	rvc, [sv1, #8]		@ rvc <- src/target end offset
	sub	rvc, rvc, rva
	ldr	sv2, [sv1, #4]		@ sv2 <- destination for code copy
bnldl1:	subs	rvc, rvc, #4
	ldr	rva, [sv1, rvc]
	str	rva, [sv2, rvc]
	bne	bnldl1
	@ copy data (.data) to destination
	ldr	rva, [sv1, #-4]		@ rva <- bytevector tag
	lsr	sv4, rva, #6		@ sv4 <- size of bytevector (scheme int)
	ldr	sv2, [sv1, #12]		@ sv2 <- destination for data copy
	ldr	rva, [sv1, #0]		@ rvc <- src/target end offset
	ldr	rvc, [sv1, #8]		@ rvc <- src start offset in bytevector
	sub	rvc, rvc, rva
	set	rvb, 0			@ rvb <- destination start offset
bnldl2:	ldr	rva, [sv1, rvc]
	str	rva, [sv2, rvb]
	add	rvc, rvc, #4
	add	rvb, rvb, #4
	lsr	rva, sv4, #2
	cmp	rvc, rva
	bmi	bnldl2
	@ set obarray and env in global vector
	swi	run_no_irq
	ldr	rvb, [sv1, #24]
	lsr	rvb, rvb, #6
	bic	rvb, rvb, #3
	ldr	rva, [sv1, #20]
	vcrfi	sv3, glv, 13		@ sv3 <- bult-in environment
	str	rva, [sv3, rvb]
	ldr	rva, [sv1, #16]
	vcrfi	sv3, glv, 17		@ sv3 <- bult-in obarray
	str	rva, [sv3, rvb]
	@ connect linkages to paptbl
	vcrfi	sv3, glv, 16		@ sv3 <- pre-entry/link table
	set	rvc, 32
bnldl3:	ldr	rvb, [sv1, rvc]
	add	rvc, rvc, #4
	ldr	rva, [sv1, rvc]
	add	rvc, rvc, #4
	eq	rvb, #0
	it	ne
	strne	rva, [sv3, rvb, lsl #2]
	bne	bnldl3
	@ finish up
	swi	run_normal
	set	sv1, true
	set	pc,  cnt


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/* -------------------------------------------------------------------------- */

ENDSUBOBAENV

/* -------------------------------------------------------------------------- */



