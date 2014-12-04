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

	BNDREG	"wkup_cm", WKUP_CM_base
	BNDREG	"per_cm",  PER_CM_base
	BNDREG	"sysc",    SCM_base
	BNDREG	"VIC",	   int_base
	BNDREG	"gio1",	   io1_base
	BNDREG	"gio2",	   io2_base
	BNDREG	"gio3",	   io3_base
	BNDREG	"tmr1",	   timer0_base    	@ tmr1 <- DMTimer2
	BNDREG	"tmr2",	   timer2_base   	@ tmr2 <- DMTimer2
	BNDREG	"tmr3",	   timer3_base   	@ tmr3 <- DMTimer3
	BNDREG	"i2c0",	   i2c0_base		@ i2c0 <- I2C1 (set in OMAP_35xx.h)
	BNDREG	"i2c1",	   i2c1_base
	BNDREG	"spi1",	   spi1_base
	BNDREG	"spi2",	   spi2_base
	BNDREG	"mci",	   mmc0_base

/*------------------------------------------------------------------------------
@  utility functions
@-----------------------------------------------------------------------------*/


	/* (RNG seed) */
	@ Park-Miller next number from seed
	simple_random_gen		@ ARMv4T, ARMv5TEJ, ARMv7A macro

	/* (config-pad ofst cfg) */
	PRIMIT	"config-pad", cfg_pad, pfun, 2
	@ sets pin configuration in SCM at specified offset
	@ in:	sv1 <- ofst (eg. #x0854 -> gpmc_a5)	(scheme int)
	@ in:	sv2 <- cfg  (eg. #x0f -> mod 7 no pu/d)	(scheme int)
	@ out:	sv1 <- npo
	int2raw	rvc, sv1		@ rvc <- offset (raw int)
	int2raw	rvb, sv2		@ rvb <- cfg    (raw int)
	set	rva, SCM_base		@ rva <- Control Module base (L4_WKUP)
	add	rva, rva, rvc		@ rva <- full config address
	swi	run_prvlgd		@ set Thread mode, privileged, no IRQ
	write	rvb, rva, #0		@ set config into register
	swi	run_normal		@ set Thread mode, unprivileged, with IRQ (user)
	@ done
	b	adr_npofxt

	/* (pin-set-dir port pin dir) */
	PRIMIT	"pin-set-dir", pin_set_dir, pfun, 3, oregent, null
	@ OMAP uses 0 for output, 1 for input, but we change that
	@ to 1 for output and 0 for input so it is the same as other
	@ MCUs
	@ in:	sv1 <- port (gio1 to gio3)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ in:	sv3 <- dir  (0=input, 1=output)	(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gio1-6 as full address (from sv1, through regent)
	set	rvb, io_dir		@ rvb <- 0x08 = offset to pin dir reg in gio0/1
	eq	sv3, #i1		@ setting to ouput?
	seteq	sv4, i0			@	if so,  sv4 <- 0 for OMAP
	setne	sv4, i1			@	if not, sv4 <- 1 for OMAP
	set	sv3, sv2
	b	rcpbit

	/* (pin-set port pin) */
	PRIMIT	"pin-set", pin_set, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio1 to gio6)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gio1-6 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	write	rvc, rva, #io_set
	b	adr_npofxt

	/* (pin-clear port pin) */
	PRIMIT	"pin-clear", pin_clear, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio1 to gio6)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- npo
	@ rva <- gio1-6 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	write	rvc, rva, #io_clear
	b	adr_npofxt

	/* (pin-set? port pin) */
	PRIMIT	"pin-set?", pin_setp, pfun, 2, oregent, null
	@ in:	sv1 <- port (gio1 to gio6)	(scheme int)
	@ in:	sv2 <- pin			(scheme int)
	@ out:	sv1 <- #t/#f pin status (#t = high)
	@ rva <- gio1-6 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	ash	rvc, 1, rvb
	read	rvb, rva, #io_dir	  @ rvb <- pin directions (0=out, 1=in)
	tst	rvb, rvc
	readeq	rvb, rva, #io_out_state	@ rvb <- output state
	readne	rvb, rva, #io_state	@ rvb <- input state
	tst	rvb, rvc
	b	adr_notfxt

	/* (stop tmr) */
	PRIMIT	"stop", pfun, 1, oregent, null
	@ in:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ out:	sv1 <- npo
	write	0, rva, #0x38
	b	adr_npofxt

	/* (restart tmr) */
	PRIMIT	"restart", pfun, 1, oregent, null
	@ in:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ out:	sv1 <- npo
	write	0,    rva, #0x3c
	write	0x41, rva, #0x38
	b	adr_npofxt

	/* (i2c-reset i2c-base) */
	PRIMIT	"i2c-reset", i2c_reset, pfun, 1, oregent, null
	@ in:	sv1 <- i2c-base (i2c0 or i2c1)		(scheme int)
	@ out:	sv1 <- npo
	bl	hw_i2c_reset		@ perform i2c-reset (input data: rva)
	b	adr_npofxt			@ return with npo

	/* (i2c-read i2c-base target-address <register> <count>) */
	PRIMIT	"i2c-read", i2c_read, pfun, 3
	@ in:	sv1 <- i2c-base (i2c0 or i2c1)		(scheme int)
	@ in:	sv2 <- target-address
	@ in:	sv3 <- (<register> <count>)
	@ out:	sv1 <- byte or double-byte read		(scheme int)
	@ note 1:	count is 1 or 2 and defaults to 1.
	@ note 2:	set reg to '() if there is no reg and count needs to be 2.
	int2raw	rva, sv1		@ rva <- i2c-base address (raw int)
	lsl	rva, rva, #4		@ rva <- full i2c-base address
	int2raw	rvc, sv2
	and	rvc, rvc, #0xff
	int2raw	rvb, sv3
	orr	rvc, rvc, rvb, lsl #8
	set	rvb, sv3
	pntrp	sv4
	careq	sv3, sv4
	setne	sv3, i1
	nullp	rvb
	eoreq	sv3, sv3, #3
	bl	hw_i2c_read		@ rvc <- data from i2c-read (input data: rva, rvc, sv3)
	raw2int	sv1, rvc		@ sv1 <- i2c data (scheme int)
	set	pc,  cnt		@ return

	/* (i2c-write i2c-base target-address <byte1> <byte2> <byte3>) */
	PRIMIT	"i2c-write", i2c_write, pfun, 1
	@ in:	sv1 <- i2c-base (i2c0 or i2c1)		(scheme int)
	@ in:	sv2 <- (target-address <byte1> <byte2> <byte3>)
	@ out:	sv1 <- npo
	@ get target address into rvc
	snoc	sv4, sv5, sv2		@ sv4 <- target-address, sv5 <- (<byte1> <byte2> <byte3>)
	int2raw	rvc, sv4		@ rvc <- target-address (raw int)
	and	rvc, rvc, #0xff		@ rvc <- target-address, trimmed
	@ pack bytes to write into rvc, and update count in sv3
	set	sv3, i0			@ sv3 <- initial number of bytes to write (scheme int)
	set	rvb, 8			@ rvb <- shift in rvc-pack for next data byte
pi2ct0:	@ loop
	nullp	sv5			@ done with data bytes?
	beq	pi2ct1			@	if so,  jump to finish up
	snoc	sv4, sv5, sv5		@ sv4 <- 1st byte, sv5 <- list of remaining bytes
	int2raw	rva, sv4		@ rva <- 1st byte (raw int)
	and	rva, rva, #0xff		@ rva <- 1st byte, trimmed
	lsl	rva, rva, rvb		@ rva <- 1st byte, shifted in place
	orr	rvc, rvc, rva		@ rvc <- updated address/data pack
	add	sv3, sv3, #4		@ sv3 <- updated number of bytes to write (scheme int)
	add	rvb, rvb, #8		@ rvb <- updated shift for next byte
	cmp	rvb, #32		@ max number of bytes not exceeded?
	bmi	pi2ct0			@	if so,  jump to continue packing bytes
pi2ct1:	@ finish up
	int2raw	rva, sv1		@ rva <- i2c-base address (raw int)
	lsl	rva, rva, #4		@ rva <- full i2c-base address
	bl	hw_i2c_write		@ perform i2c-write (input data: rva, rvc, sv3)
	b	adr_npofxt			@ return with npo

	/* (rd16 reg ofst) */
	PRIMIT	"rd16", pfun, 2, oregent, null
	@ in:	sv1 <- reg				(scheme int)
	@ in:	sv2 <- ofst				(scheme int)
	@ out:	sv1 <- 16-bit value from reg+ofst	(scheme int)
	read16	rvc, rva, rvb
	raw2int	sv1, rvc
	set	pc,  cnt

	/* (wr16 val reg ofst) */
	PRIMIT	"wr16", pfun, 3
	@ in:	sv1 <- val (16-bit)			(scheme int)
	@ in:	sv2 <- reg				(scheme int)
	@ in:	sv3 <- ofst				(scheme int)
	@ out:	sv1 <- npo
	int2raw	rva, sv2
	lsl	rva, rva, #4
	int2raw	rvb, sv3
	int2raw	rvc, sv1
	write16	rvc, rva, rvb
	b	adr_npofxt			@ return with npo

	/* (_sgb block-number) */
	PRIMIT	"_sgb", pfun, 1
	@ get a block of data (512 bytes) from sd-card (eg. after sd-init)
	@ in:	sv1 <- block-number			(scheme int)
	@ out:	sv1 <- sd block data			(bytevector)
	@ allocate buffer
	set	rvb, 0x82
	lsl	rvb, rvb, #2
	bl	adr__alo
	set	rvc, 0x80
	lsl	rvc, rvc, #10
	orr	rvc, rvc, #0x6f		@ rvc <- bytevector tag
	str	rvc, [rva]
	add	rva, rva, rvb		@ rva <- address of next free cell (level 2 reserved)
	sub	sv3, rva, rvb		@ sv3 <- allocated block [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	@	get block of data
	set	rvc, sv1		@ rvc <- block number for _sgb (scheme int)
	bl	_sgb			@ sv3 <- block data
	set	sv1, sv3		@ sv1 <- block data
	set	pc,  cnt		@ return

	/* (_spb data-block block-number) */
	PRIMIT	"_spb", pfun, 2
	@ put a block of data (512 bytes) onto sd-card (eg. after sd-init)
	@ in:	sv1 <- data block			(bytevector)
	@ in:	sv2 <- block-number			(scheme int)
	@ out:	sv1 <- npo
	set	rvc, sv2		@ rvc <- block number for _spb (scheme int)
	set	sv3, sv1		@ sv3 <- block data for _spb
	bl	_spb			@ perform write
	b	adr_npofxt			@ return with npo


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/* -------------------------------------------------------------------------- */

ENDSUBOBAENV

/* -------------------------------------------------------------------------- */



