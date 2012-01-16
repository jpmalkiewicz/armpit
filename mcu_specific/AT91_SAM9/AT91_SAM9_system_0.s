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

@-------------------------------------------------------------------------------------
@
@ Contributions:
@
@     This file includes contributions by Robbie Dinn, marked <RDC>
@
@-------------------------------------------------------------------------------------

.balign	4

	@-------.-------.-------.-------.-------+
s0_env:	@	system 0 sub-environment	|
	@-------.-------.-------.-------.-------+

		VECSIZE	(end_of_s0_env - s0_env - 4) >> 2

	@-------.-------.-------.-------.-------+
	@	register address bindings	|
	@-------.-------.-------.-------.-------+
	
		.word	s_pmc,	(PMC_base    >> 2) | i0		@ pmc  <- PMC
		.word	sAIC,	(int_base    >> 2) | i0		@ AIC  <- AIC_IVR
		.word	sgioa,	(pioa_base   >> 2) | i0		@ gioa <- pioa
		.word	sgiob,	(piob_base   >> 2) | i0		@ giob <- piob
		.word	sgioc,	(pioc_base   >> 2) | i0		@ gioc <- pioc
		.word	stmr0,	(timer0_base >> 2) | i0		@ tmr0
		.word	stmr1,	(timer1_base >> 2) | i0		@ tmr1
		.word	suar0,	(uart0_base  >> 2) | i0		@ uar0
		.word	suar1,	(uart1_base  >> 2) | i0		@ uar1
		.word	spwm0,	(pwm0_base   >> 2) | i0		@ pwm0
		.word	s_i2c0,	(i2c0_base   >> 2) | i0		@ i2c0
		.word	sspi0,	(spi0_base   >> 2) | i0		@ spi0
		.word	sadc0,	(adc0_base   >> 2) | i0		@ adc0

	@-------.-------.-------.-------.-------+
	@	utility functions (system 0)	|
	@-------.-------.-------.-------.-------+

		.word	scfpwr, pcfpwr			@ config-power
		.word	scfgpn, pcfgpn			@ config-pin
		.word	spnset, ppnset			@ pin-set
		.word	spnclr, ppnclr			@ pin-clear
		.word	spnstq, ppnstq			@ pin-set?
		.word	ststrt, ptstrt			@ restart (timer)
		.word	ststop, ptstop			@ stop (timer)
		.word	sspput, pspput			@ spi-put
		.word	sspget, pspget			@ spi-get

end_of_s0_env:	@ end of system 0 env vector
	

@---------------------------------------------------------------------------------------------------------
@  register address bindings -- names
@---------------------------------------------------------------------------------------------------------

.balign	4

s_pmc:	SYMSIZE	3
	.ascii	"pmc"
	.balign 4

sAIC:	SYMSIZE	3
	.ascii	"AIC"
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

stmr0:	SYMSIZE	4
	.ascii	"tmr0"
	.balign 4

stmr1:	SYMSIZE	4
	.ascii	"tmr1"
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

sspi0:	SYMSIZE	4
	.ascii	"spi0"
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
	
pcfpwr:	@ (config-power bit val)
	@ on entry:	sv1 <- bit position	(scheme int)
	@ on entry:	sv2 <- val (1 or 0)	(scheme int)
	@ on exit:	sv1 <- npo
	PFUNC	2			@ primitive function, two input args
	ldr	rva, =PMC_base
	int2raw	rvb, sv1
	set	rvc, #1
	lsl	rvc, rvc, rvb
	eq	sv2, #i1		@ enable power?
	streq	rvc, [rva, #0x10]	@	if so,  PMC_PCER <- enable  peripheral clock
	strne	rvc, [rva, #0x14]	@	if not, PMC_PCDR <- disable peripheral clock
	b	npofxt			@ return
	
.balign	4
	
scfgpn:	SYMSIZE	10
	.ascii	"config-pin"
	.balign 4
	
pcfgpn:	@ (config-pin port pin dig ddir pup afab adir)
	@ port:		port   (gioa to gioc)
	@ pin:		0-31,  eg. for PA0 to PA31
	@ dig:		0/1,   1 -> digital function (0 for alternate)
	@ ddir:		0/1,   1 -> output direction (digital)
	@ pup:		0/1,   1 -> pull-up resistor
	@ afab:		0/1/2, 1 -> periph A function, 2 -> periph B function
	@ adir:		0/1,   1 -> output write enable (alternate function) 
	@ on entry:	sv1 <- port (gioa-c)		(scheme int)
	@ on entry:	sv2 <- pin (0 to 31)		(scheme int)
	@ on entry:	sv3 <- (pin dig pup afab dir)	(list)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gioa-c as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	set	rvc, #1			@ rvc <- 1
	lsl	rvb, rvc, rvb		@ rvb <- bit in pin position
	@ digital function configuration
	nullp	sv3			@ no more configuration?
	beq	npofxt			@	if so,  exit
	snoc	sv1, sv2, sv3		@ sv1 <- dig,  sv2 <- (ddir pup afab adir)
	eq	sv1, #i1		@ digital?
	streq	rvb, [rva, #0x00]	@	if so,  PIO_PER <- enable  digital function
	strne	rvb, [rva, #0x04]	@	if not, PIO_PDR <- disable digital function
	@ digital direction configuration
	nullp	sv2			@ no more configuration?
	beq	npofxt			@	if so,  exit
	snoc	sv1, sv2, sv2		@ sv1 <- ddir,  sv2 <- (pup afab adir)
	eq	sv1, #i1		@ output?
	streq	rvb, [rva, #0x10]	@	if so,  PIO_OER <- enable  output direction
	strne	rvb, [rva, #0x14]	@	if not, PIO_ODR <- disable output direction
	@ pull-up resistor configuration
	nullp	sv2			@ no more configuration?
	beq	npofxt			@	if so,  exit
	snoc	sv1, sv2, sv2		@ sv1 <- pup,  sv2 <- (afab adir)
	eq	sv1, #i1		@ pull-up?
	streq	rvb, [rva, #0x60]	@	if so,  PIO_PUER <- enable  pull-up
	strne	rvb, [rva, #0x64]	@	if not, PIO_PUDR <- disable pull-up
	@ alternate function configuration
	nullp	sv2			@ no more configuration?
	beq	npofxt			@	if so,  exit
	snoc	sv1, sv2, sv2		@ sv1 <- afab,  sv2 <- (adir)
	eq	sv1, #i1		@ peripheral A function?
	streq	rvb, [rva, #0x70]	@	if so,  PIO_ASR <- enable peripheral A function
	eq	sv1, #9			@ peripheral B function?
	streq	rvb, [rva, #0x74]	@	if so,  PIO_BSR <- enable peripheral B function
	@ output write configuration
	nullp	sv2			@ no more configuration?
	beq	npofxt			@	if so,  exit
	snoc	sv1, sv2, sv2		@ sv1 <- adir,  sv2 <- ()
	eq	sv1, #i1		@ output write?
	streq	rvb, [rva, #0xa0]	@	if so,  PIO_OWER <- enable  output write
	strne	rvb, [rva, #0xa4]	@	if not, PIO_OWDR <- disable output write
	b	npofxt

.balign	4
	
spnset:	SYMSIZE	7
	.ascii	"pin-set"
	.balign 4
	
ppnset:	@ (pin-set port pin)
	@ on entry:	sv1 <- port (gioa or giob)	(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gioa/b as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	set	rvc, #1
	lsl	rvc, rvc, rvb
	str	rvc, [rva, #io_set]
	b	npofxt
		
.balign	4
	
spnclr:	SYMSIZE	9
	.ascii	"pin-clear"
	.balign 4
	
ppnclr:	@ (pin-clear port pin)
	@ on entry:	sv1 <- port (gioa or giob)	(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gioa/b as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	set	rvc, #1
	lsl	rvc, rvc, rvb
	str	rvc, [rva, #io_clear]
	b	npofxt
	
.balign	4
	
spnstq:	SYMSIZE	8
	.ascii	"pin-set?"
	.balign 4
	
ppnstq:	@ (pin-set? port pin)
	@ on entry:	sv1 <- port (gioa or giob)	(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- #t/#f pin status (#t = high)
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gioa/b as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	set	rvc, #1
	lsl	rvc, rvc, rvb
	ldr	rvb, [rva, #io_state]
	tst	rvb, rvc
	b	notfxt
	
.balign	4
	
ststop:	SYMSIZE	4
	.ascii	"stop"
	.balign 4
	
ptstop:	@ (stop tmr)
	@ on entry:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 1		@ primitive, init-sv4 = none, fentry = regent, narg = 1
	set	rvb, #2
	str	rvb, [rva, #0x00]
	b	npofxt

.balign	4
	
ststrt:	SYMSIZE	7
	.ascii	"restart"
	.balign 4
	
ptstrt:	@ (restart tmr)
	@ on entry:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 1		@ primitive, init-sv4 = none, fentry = regent, narg = 1
	set	rvb, #5
	str	rvb, [rva, #0x00]
	b	npofxt

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
	and	rvb, rvb, #0xff
	raw2int	sv1, rvb
	set	pc,  cnt


@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

