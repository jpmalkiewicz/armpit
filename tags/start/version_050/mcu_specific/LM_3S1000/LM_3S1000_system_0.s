@---------------------------------------------------------------------------------------------------
@
@  ARMPIT SCHEME Version 050
@
@  ARMPIT SCHEME is distributed under The MIT License.

@  Copyright (c) 2006-2012 Hubert Montas

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
@---------------------------------------------------------------------------------------------------

.balign	4

	@-------.-------.-------.-------.-------+
s0_env:	@	system 0 sub-environment	|
	@-------.-------.-------.-------.-------+

		VECSIZE	(end_of_s0_env - s0_env - 4) >> 2

	@-------.-------.-------.-------.-------+
	@	register address bindings	|
	@-------.-------.-------.-------.-------+
	
		.word	s_rcc,	(rcc_base     >> 2) | i0	@ rcc  <- rcc base
		.word	sgioa,	(ioporta_base >> 2) | i0	@ gioa
		.word	sgiob,	(ioportb_base >> 2) | i0	@ giob
		.word	sgioc,	(ioportc_base >> 2) | i0	@ gioc
		.word	sgiod,	(ioportd_base >> 2) | i0	@ giod
		.word	sgioe,	(ioporte_base >> 2) | i0	@ gioe
		.word	sgiof,	(ioportf_base >> 2) | i0	@ giof
		.word	sgiog,	(ioportg_base >> 2) | i0	@ giog
		.word	sgioh,	(ioporth_base >> 2) | i0	@ gioh
		.word	stmr0,	(timer0_base  >> 2) | i0	@ tmr0
		.word	stmr1,	(timer1_base  >> 2) | i0	@ tmr1
		.word	stmr2,	(timer2_base  >> 2) | i0	@ tmr2
		.word	stmr3,	(timer3_base  >> 2) | i0	@ tmr3
		.word	suar0,	(uart0_base   >> 2) | i0	@ uar0
		.word	suar1,	(uart1_base   >> 2) | i0	@ uar1
		.word	spwm0,	(pwm0_base    >> 2) | i0	@ pwm0
		.word	s_i2c0,	(i2c0_base    >> 2) | i0	@ i2c0
		.word	s_i2c1,	(i2c1_base    >> 2) | i0	@ i2c1
		.word	sssi0,	(ssi0_base    >> 2) | i0	@ ssi0
		.word	sssi1,	(ssi1_base    >> 2) | i0	@ ssi1
		.word	sadc0,	(adc0_base    >> 2) | i0	@ adc0

	@-------.-------.-------.-------.-------+
	@	utility functions (system 0)	|
	@-------.-------.-------.-------.-------+

		.word	scfpwr, pcfpwr			@ config-power
		.word	scfgpn, pcfgpn			@ config-pin
		.word	spnset, ppnset			@ pin-set
		.word	spnclr, ppnclr			@ pin-clear
		.word	spnstq, ppnstq			@ pin-set?
		.word	ststrt, ptstrt			@ tic-start (systick timer)
		.word	ststop, ptstop			@ tic-stop  (systick timer)
		.word	stkred, ptkred			@ tic-read  (systick timer)
		.word	sspput, pspput			@ spi-put
		.word	sspget, pspget			@ spi-get
	

end_of_s0_env:	@ end of system 0 env vector


@---------------------------------------------------------------------------------------------------------
@  register address bindings -- names
@---------------------------------------------------------------------------------------------------------

.balign	4
	
s_rcc:	SYMSIZE	3
	.ascii	"rcc"
	.balign 4

sgioa:	SYMSIZE	4
	.ascii	"gioa"
	.balign 4

sgiob:	SYMSIZE	4
	.ascii	"giob"
	.balign 4

sgioc:	SYMSIZE	4
	.ascii	"gioc"
	.balign 4

sgiod:	SYMSIZE	4
	.ascii	"giod"
	.balign 4

sgioe:	SYMSIZE	4
	.ascii	"gioe"
	.balign 4

sgiof:	SYMSIZE	4
	.ascii	"giof"
	.balign 4

sgiog:	SYMSIZE	4
	.ascii	"giog"
	.balign 4

sgioh:	SYMSIZE	4
	.ascii	"gioh"
	.balign 4

stmr0:	SYMSIZE	4
	.ascii	"tmr0"
	.balign 4

stmr1:	SYMSIZE	4
	.ascii	"tmr1"
	.balign 4

stmr2:	SYMSIZE	4
	.ascii	"tmr2"
	.balign 4

stmr3:	SYMSIZE	4
	.ascii	"tmr3"
	.balign 4

suar0:	SYMSIZE	4
	.ascii	"uar0"
	.balign 4

suar1:	SYMSIZE	4
	.ascii	"uar1"
	.balign 4

spwm0:	SYMSIZE	4
	.ascii	"pwm0"
	.balign 4

s_i2c0:	SYMSIZE	4
	.ascii	"i2c0"
	.balign 4

s_i2c1:	SYMSIZE	4
	.ascii	"i2c1"
	.balign 4

sssi0:	SYMSIZE	4
	.ascii	"ssi0"
	.balign 4

sssi1:	SYMSIZE	4
	.ascii	"ssi1"
	.balign 4

sadc0:	SYMSIZE	4
	.ascii	"adc0"
	.balign 4

	
@---------------------------------------------------------------------------------------------------------
@  utility functions
@---------------------------------------------------------------------------------------------------------

.balign	4

scfpwr:	SYMSIZE	12
	.ascii	"config-power"
	.balign 4
	
pcfpwr:	@ (config-power rgc bit val)
	@ on entry:	sv1 <- rgc (eg. 2 for RCGC2)	(scheme int)
	@ on entry:	sv2 <- bit position		(scheme int)
	@ on entry:	sv3 <- val (1 or 0)		(scheme int)
	@ on exit:	sv1 <- npo
	PFUNC	3			@ primitive function, three input args
	set	sv4, sv3
	set	sv3, sv2
	ldr	rva, =rcc_base
	bic	rvb, sv1, #0x03
	orr	rvb, rvb, #0x100
	b	rcpbit
	
.balign	4
	
scfgpn:	SYMSIZE	10
	.ascii	"config-pin"
	.balign 4
	
pcfgpn:	@ (config-pin port pin den odr pur pdr dir dr2r dr8r afun)
	@ on entry:	sv1 <- port (eg. giob)		(scheme int)
	@ on entry:	sv2 <- pin  (0 to 7)		(scheme int)
	@ on entry:	sv3 <- (den odr pur pdr dir dr2r dr8r afun)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gioa-h as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	add	rva, rva, #0x0500
	set	rvc, #1
	lsl	rvc, rvc, rvb
	add	rva, rva, #0x001c	@ rva <- port + #x51c, for den
	bl	pcfghl
	sub	rva, rva, #0x0010	@ rva <- port + #x50c, for odr
	bl	pcfghl
	add	rva, rva, #0x0004	@ rva <- port + #x510, for pur
	bl	pcfghl
	add	rva, rva, #0x0004	@ rva <- port + #x514, for pdr
	bl	pcfghl
	sub	rva, rva, #0x0014
	sub	rva, rva, #0x0100	@ rva <- port + #x400, for dir
	bl	pcfghl
	add	rva, rva, #0x0100	@ rva <- port + #x500, for dr2r
	bl	pcfgdr
	add	rva, rva, #0x0008	@ rva <- port + #x508, for dr8r
	bl	pcfgdr
	sub	rva, rva, #0x0100
	add	rva, rva, #0x0018	@ rva <- port + #x420, for afun
	bl	pcfghl
	b	npofxt
		
pcfgdr:	@ helper entry for dr2r and dr8r
	nullp	sv3
	beq	npofxt
	car	sv2, sv3
	eq	sv2, #i0
	itT	eq
	cdreq	sv3, sv3
	seteq	pc,  lnk
pcfghl:	@ helper function, general entry
	nullp	sv3
	beq	npofxt
	snoc	sv2, sv3, sv3
	ldr	rvb, [rva]
	eq	sv2, #i0
	itE	eq
	biceq	rvb, rvb, rvc
	orrne	rvb, rvb, rvc
	str	rvb, [rva]
	set	pc,  lnk
	
.balign	4
	
spnset:	SYMSIZE	7
	.ascii	"pin-set"
	.balign 4
	
ppnset:	@ (pin-set port pin)
	@ on entry:	sv1 <- port (eg. giob)		(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gioa-h as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	set	rvc, #4
	lsl	rvc, rvc, rvb
	set	rvb, #0xff
	str	rvb, [rva, rvc]
	b	npofxt
		
.balign	4
	
spnclr:	SYMSIZE	9
	.ascii	"pin-clear"
	.balign 4
	
ppnclr:	@ (pin-clear port pin)
	@ on entry:	sv1 <- port (eg. giob)		(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gioa-h as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	set	rvc, #4
	lsl	rvc, rvc, rvb
	set	rvb, #0x00
	str	rvb, [rva, rvc]
	b	npofxt
	
.balign	4
	
spnstq:	SYMSIZE	8
	.ascii	"pin-set?"
	.balign 4
	
ppnstq:	@ (pin-set? port pin)
	@ on entry:	sv1 <- port (eg. giob)		(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- #t/#f pin status (#t = high)
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gioa-h as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	set	rvc, #1
	lsl	rvc, rvc, rvb
	ldr	rvb, [rva, #io_state]
	tst	rvb, rvc
	b	notfxt
	
.balign	4
	
ststop:	SYMSIZE	8
	.ascii	"tic-stop"
	.balign 4
	
ptstop:	@ (tic-stop)
	@ stop the systick timer
	@ on exit:	sv1 <- npo
	PFUNC	0			@ primitive function, no input args
	ldr	rva, =systick_base
	set	rvb, #0
	str	rvb, [rva, #tick_ctrl]
	b	npofxt

.balign	4
	
ststrt:	SYMSIZE	9
	.ascii	"tic-start"
	.balign 4
	
ptstrt:	@ (tic-start bool)
	@ start the systick timer (without interrupt generation if bool = #f)
	@ on entry:	sv1 <- #f (no interrupts) or anything else, including null (interrupts)
	@ on exit:	sv1 <- npo
	PFUNC	1			@ primitive function, one input arg
	ldr	rva, =systick_base
	set	rvb, #0
	str	rvb, [rva, #tick_ctrl]
	str	rvb, [rva, #tick_val]
	eq	sv1, #f
	itE	eq
	seteq	rvb, #0x05
	setne	rvb, #0x07
	str	rvb, [rva, #tick_ctrl]
	b	npofxt

.balign	4
	
stkred:	SYMSIZE	8
	.ascii	"tic-read"
	.balign 4
	
ptkred:	@ (tic-read)
	@ read current value of the systick timer
	@ on exit:	sv1 <- value from systick timer
	PFUNC	0			@ primitive function, no input args
	ldr	rva, =systick_base
	ldr	rvb, [rva, #tick_val]
	raw2int	sv1, rvb
	set	pc,  cnt

.balign	4
	
sspput:	SYMSIZE	7
	.ascii	"spi-put"
	.balign 4
	
pspput:	@ (spi-put port val)
	@ on entry:	sv1 <- port (spi0 or spi1)	(scheme int)
	@ on entry:	sv2 <- val
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
psppt0:	ldr	rvc, [rva, #spi_status]
	tst	rvc, #spi_txrdy
	beq	psppt0
	str	rvb, [rva, #spi_thr]
	b	npofxt

.balign	4
	
sspget:	SYMSIZE	7
	.ascii	"spi-get"
	.balign 4
	
pspget:	@ (spi-get port)
	@ on entry:	sv1 <- port (spi0 or spi1)	(scheme int)
	@ on exit:	sv1 <- data from spi		(scheme int)
	EPFUNC	0, oregent, 1		@ primitive, init-sv4 = none, fentry = regent, narg = 1
pspgt0:	ldr	rvb, [rva, #spi_status]
	tst	rvb, #spi_rxrdy
	beq	pspgt0
	ldr	rvb, [rva, #spi_rhr]
	raw2int	sv1, rvb
	set	pc,  cnt


@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg


	