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
		.word	stmr2,	(timer0_base  >> 2) | i0	@ tmr2 (armpit timer 0 w/r interrupts)
		.word	stmr3,	(timer1_base  >> 2) | i0	@ tmr3 (armpit timer 1 w/r interrupts)
		.word	stmr4,	(timer4_base  >> 2) | i0	@ tmr4
		.word	stmr5,	(timer5_base  >> 2) | i0	@ tmr5
		.word	stmr6,	(timer6_base  >> 2) | i0	@ tmr6
		.word	stmr7,	(timer7_base  >> 2) | i0	@ tmr7
		.word	suar1,	(uart0_base   >> 2) | i0	@ uar1 (USART1, armpit UAR0 port)
		.word	suar2,	(uart1_base   >> 2) | i0	@ uar2 (USART2, armpit UAR1 port)
		.word	s_i2c1,	(i2c0_base    >> 2) | i0	@ i2c1 (armpit I2C0 port)
		.word	s_i2c2,	(i2c1_base    >> 2) | i0	@ i2c2 (armpit I2C1 port)
		.word	sspi1,	(spi1_base    >> 2) | i0	@ spi1
		.word	sspi2,	(spi2_base    >> 2) | i0	@ spi2
		.word	sspi3,	(spi3_base    >> 2) | i0	@ spi3
		.word	sadc1,	(adc1_base    >> 2) | i0	@ adc1
		.word	sadc2,	(adc2_base    >> 2) | i0	@ adc2
		.word	sadc3,	(adc3_base    >> 2) | i0	@ adc3
		.word	s_sdio,	(sdio_base    >> 2) | i0	@ sdio
		.word	s_fsmc,	(fsmc_base    >> 2) | i0	@ fsmc
		.word	s_afio,	(afio_base    >> 2) | i0	@ afio

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
	.balign

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

stmr2:	SYMSIZE	4
	.ascii	"tmr2"
	.balign 4

stmr3:	SYMSIZE	4
	.ascii	"tmr3"
	.balign 4

stmr4:	SYMSIZE	4
	.ascii	"tmr4"
	.balign 4

stmr5:	SYMSIZE	4
	.ascii	"tmr5"
	.balign 4

stmr6:	SYMSIZE	4
	.ascii	"tmr6"
	.balign 4

stmr7:	SYMSIZE	4
	.ascii	"tmr7"
	.balign 4

suar1:	SYMSIZE	4
	.ascii	"uar1"
	.balign 4

suar2:	SYMSIZE	4
	.ascii	"uar2"
	.balign 4

s_i2c1:	SYMSIZE	4
	.ascii	"i2c1"
	.balign 4

s_i2c2:	SYMSIZE	4
	.ascii	"i2c2"
	.balign 4

sspi1:	SYMSIZE	4
	.ascii	"spi1"
	.balign 4

sspi2:	SYMSIZE	4
	.ascii	"spi2"
	.balign 4

sspi3:	SYMSIZE	4
	.ascii	"spi3"
	.balign 4

sadc1:	SYMSIZE	4
	.ascii	"adc1"
	.balign 4

sadc2:	SYMSIZE	4
	.ascii	"adc2"
	.balign 4

sadc3:	SYMSIZE	4
	.ascii	"adc3"
	.balign 4

s_sdio:	SYMSIZE	4
	.ascii	"sdio"
	.balign 4

s_fsmc:	SYMSIZE	4
	.ascii	"fsmc"
	.balign 4

s_afio:	SYMSIZE	4
	.ascii	"afio"
	.balign 4

	
@---------------------------------------------------------------------------------------------------------
@  utility functions
@---------------------------------------------------------------------------------------------------------

	
.balign	4
	
scfpwr:	SYMSIZE	12
	.ascii	"config-power"
	.balign 4
	
pcfpwr:	@ (config-power rcof bit val)
	@ on entry:	sv1 <- rcof (offset in rcc)	(scheme int)
	@ on entry:	sv2 <- bit position		(scheme int)
	@ on entry:	sv3 <- val (1 or 0)		(scheme int)
	@ on exit:	sv1 <- npo
	PFUNC	3			@ primitive function, three input args
	set	sv4, sv3
	set	sv3, sv2
	ldr	rva, =rcc_base
	int2raw	rvb, sv1
	b	rcpbit
	
.balign	4
	
scfgpn:	SYMSIZE	10
	.ascii	"config-pin"
	.balign 4
	
pcfgpn:	@ (config-pin port pin cnf mode)
	@ on entry:	sv1 <- port (eg. giob)		(scheme int)
	@ on entry:	sv2 <- pin  (0 to 15)		(scheme int)
	@ on entry:	sv3 <- cnf			(scheme int)
	@ on entry:	sv4 <- mode			(scheme int)
	@ on exit:	sv1 <- npo
	PFUNC	4			@ primitive function, four input args
	int2raw	rvc, sv1
	lsl	rvc, rvc, #4
	cmp	sv2, #33
	it	pl
	addpl	rvc, rvc, #4
	orr	sv5, rvc, #i0
	ldr	rva, [rvc]
	int2raw	rvc, sv2
	and	rvc, rvc, #0x07
	lsl	rvc, rvc, #2
	set	rvb, #0x0f
	lsl	rvb, rvb, rvc
	bic	rva, rva, rvb
	bic	rvb, sv3, #0x03
	lsl	rvb, rvb, rvc
	orr	rva, rva, rvb
	int2raw	rvb, sv4
	lsl	rvb, rvb, rvc
	orr	rva, rva, rvb
	bic	rvc, sv5, #i0
	str	rva, [rvc]
	b	npofxt

.balign	4
	
spnset:	SYMSIZE	7
	.ascii	"pin-set"
	.balign 4
	
ppnset:	@ (pin-set port pin)
	@ on entry:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gio0/1 as full address (from sv1, through regent)
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
	@ on entry:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gio0/1 as full address (from sv1, through regent)
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
	@ on entry:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- #t/#f pin status (#t = high)
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gio0/1 as full address (from sv1, through regent)
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



	