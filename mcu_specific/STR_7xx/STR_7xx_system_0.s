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

		.word	s_rcc,	(rcc_base     >> 2) | i0	@ rcc  <- rcc_base
		.word	sEIC,	(int_base     >> 2) | i0	@ EIC  <- 0xE000F800
		.word	srtc0,	(rtc0_base    >> 2) | i0	@ rtc0
		.word	sgio0,	(ioport0_base >> 2) | i0	@ gio0
		.word	sgio1,	(ioport1_base >> 2) | i0	@ gio1
		.word	stmr0,	(timer0_base  >> 2) | i0	@ tmr0
		.word	stmr1,	(timer1_base  >> 2) | i0	@ tmr1
		.word	stmr2,	(timer2_base  >> 2) | i0	@ tmr2
		.word	stmr3,	(timer3_base  >> 2) | i0	@ tmr3
		.word	suar0,	(uart0_base   >> 2) | i0	@ uar0
		.word	suar1,	(uart1_base   >> 2) | i0	@ uar1
		.word	s_i2c0,	(i2c0_base    >> 2) | i0	@ i2c0
		.word	s_i2c1,	(i2c1_base    >> 2) | i0	@ i2c1
		.word	sspi0,	(spi0_base    >> 2) | i0	@ spi0
		.word	sspi1,	(spi1_base    >> 2) | i0	@ spi1
		.word	sadc0,	(adc0_base    >> 2) | i0	@ adc0

	@-------.-------.-------.-------.-------+
	@	utility functions (system 0)	|
	@-------.-------.-------.-------.-------+

		.word	scfpwr, pcfpwr			@ config-power
		.word	scfgpn, pcfgpn			@ config-pin
		.word	spnset, ppnset			@ pin-set
		.word	spnclr, ppnclr			@ pin-clear
		.word	spnstq, ppnstq			@ pin-set?
		.word	ststrt, ptstrt			@ restart (timer)
		.word	ststop, ptstop			@ stop    (timer)
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
	
sEIC:	SYMSIZE	3
	.ascii	"EIC"
	.balign 4
	
srtc0:	SYMSIZE	4
	.ascii	"rtc0"
	.balign 4
	
sgio0:	SYMSIZE	4
	.ascii	"gio0"
	.balign 4
	
sgio1:	SYMSIZE	4
	.ascii	"gio1"
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
	
s_i2c0:	SYMSIZE	4
	.ascii	"i2c0"
	.balign 4
	
s_i2c1:	SYMSIZE	4
	.ascii	"i2c1"
	.balign 4
	
sspi0:	SYMSIZE	4
	.ascii	"spi0"
	.balign 4
	
sspi1:	SYMSIZE	4
	.ascii	"spi1"
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
	@ sets/clears bit in PCU_BOOTCR (only BSPI0 and ADC are in there)
	@ on entry:	sv1 <- bit position	(scheme int)
	@ on entry:	sv2 <- val (1 or 0)	(scheme int)
	@ on exit:	sv1 <- npo
	PFUNC	2			@ primitive function, two input args
	set	sv3, sv1
	set	sv4, sv2
	ldr	rva, =rcc_base
	set	rvb, #0x50
	b	rcpbit
	
.balign	4
	
scfgpn:	SYMSIZE	10
	.ascii	"config-pin"
	.balign 4
	
pcfgpn:	@ (config-pin main sub cfg)
	@ main:		0/1,  port
	@ sub:		0-15, pin on port
	@ cfg:		3-bit value for PC2, PC1, PC0,
	@		#b000 = analog input,	#b001  = TTL input
	@		#b010 = CMOS input,	#b011  = input with pull-up/dn
	@		#b100 = open-drain out,	#b101 = push-pull output
	@		#b110 = AF open-drain,	#b111 = AF push-pull
	@ on entry:	sv1 <- main (0 or 1)		(scheme int)
	@ on entry:	sv2 <- sub  (0 to 15)		(scheme int)
	@ on entry:	sv3 <- cfg  (eg. #b001)		(scheme int)
	@ on exit:	sv1 <- npo
	PFUNC	3			@ primitive function, three input args
	intgrp	sv3
	bne	corerr
	ldr	rva, =ioport1_base
	eq	sv1, #i0
	ldreq	rva, =ioport0_base
	eqne	sv1, #i1
	bne	corerr
	int2raw	rvb, sv2
	set	rvc, #1
	lsl	rvc, rvc, rvb
	@ configure PC0
	ldr	rvb, [rva, #0x00]
	tst	sv3, #0x04
	biceq	rvb, rvb, rvc
	orrne	rvb, rvb, rvc
	str	rvb, [rva, #0x00]
	@ configure PC1
	ldr	rvb, [rva, #0x04]
	tst	sv3, #0x08
	biceq	rvb, rvb, rvc
	orrne	rvb, rvb, rvc
	str	rvb, [rva, #0x04]
	@ configure PC2
	ldr	rvb, [rva, #0x08]
	tst	sv3, #0x10
	biceq	rvb, rvb, rvc
	orrne	rvb, rvb, rvc
	str	rvb, [rva, #0x08]
	b	npofxt

.balign	4
	
spnset:	SYMSIZE	7
	.ascii	"pin-set"
	.balign 4
	
ppnset:	@ (pin-set port pin)
	@ on entry:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- npo
	@ pre-entry:	regent sets rva to register address (sv1) and rvb to offset (raw sv2)
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gio0/1 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	set	rvc, #1
	lsl	rvc, rvc, rvb
	swi	run_no_irq
	ldr	rvb, [rva, #io_state]	@ rvb <- pin statuses
	orr	rvb, rvb, rvc
	str	rvb, [rva, #io_state]	@ set pin
	swi	run_normal
	b	npofxt
		
.balign	4
	
spnclr:	SYMSIZE	9
	.ascii	"pin-clear"
	.balign 4
	
ppnclr:	@ (pin-clear port pin)
	@ on entry:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- npo
	@ pre-entry:	regent sets rva to register address (sv1) and rvb to offset (raw sv2)
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gio0/1 as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	set	rvc, #1
	lsl	rvc, rvc, rvb
	swi	run_no_irq
	ldr	rvb, [rva, #io_state]	@ rvb <- pin statuses
	bic	rvb, rvb, rvc
	str	rvb, [rva, #io_state]	@ clear pin
	swi	run_normal
	b	npofxt
	
.balign	4
	
spnstq:	SYMSIZE	8
	.ascii	"pin-set?"
	.balign 4
	
ppnstq:	@ (pin-set? port pin)
	@ on entry:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- #t/#f pin status (#t = high)
	@ pre-entry:	regent sets rva to register address (sv1) and rvb to offset (raw sv2)
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gio0/1 as full address (from sv1, through regent)
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
	@ pre-entry:	regent sets rva to register address (sv1) and rvb to offset (raw sv2)
	EPFUNC	0, oregent, 1		@ primitive, init-sv4 = none, fentry = regent, narg = 1
	set	rvb, #0
	str	rvb, [rva, #0x14]
	b	npofxt

.balign	4
	
ststrt:	SYMSIZE	7
	.ascii	"restart"
	.balign 4
	
ptstrt:	@ (restart tmr)
	@ on entry:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ on exit:	sv1 <- npo
	@ pre-entry:	regent sets rva to register address (sv1) and rvb to offset (raw sv2)
	EPFUNC	0, oregent, 1		@ primitive, init-sv4 = none, fentry = regent, narg = 1
	set	rvb, #0x8000
	str	rvb, [rva, #0x14]
	set	rvb, #0
	str	rvb, [rva, #0x10]
	b	npofxt

.balign	4
	
sspput:	SYMSIZE	7
	.ascii	"spi-put"
	.balign 4
	
pspput:	@ (spi-put port val)
	@ on entry:	sv1 <- port (spi0 or spi1)	(scheme int)
	@ on entry:	sv2 <- val
	@ on exit:	sv1 <- npo
	@ pre-entry:	regent sets rva to register address (sv1) and rvb to offset (raw sv2)
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	lsl	rvb, rvb, #8
psppt0:	ldr	rvc, [rva, #spi_status]	@ ssta
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
	@ pre-entry:	regent sets rva to register address (sv1) and rvb to offset (raw sv2)
	EPFUNC	0, oregent, 1		@ primitive, init-sv4 = none, fentry = regent, narg = 1
pspgt0:	ldr	rvb, [rva, #spi_status]
	tst	rvb, #spi_rxrdy
	beq	pspgt0
	ldr	rvb, [rva, #spi_rhr]
	lsr	rvb, rvb, #8
	raw2int	sv1, rvb
	set	pc,  cnt



@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

