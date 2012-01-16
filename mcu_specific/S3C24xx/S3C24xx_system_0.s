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
	
		.word	ssysc,	(sys_ctrl     >> 2) | i0	@ sysc  <- SCU base
		.word	sVIC,	(int_base     >> 2) | i0	@ VIC   <- VIC 0
		.word	srtc0,	(rtc0_base    >> 2) | i0	@ rtc0
		.word	sgioa,	(ioA_base     >> 2) | i0	@ gioa
		.word	sgiob,	(ioB_base     >> 2) | i0	@ giob
		.word	sgioc,	(ioC_base     >> 2) | i0	@ gioc
		.word	sgiod,	(ioD_base     >> 2) | i0	@ giod
		.word	sgioe,	(ioE_base     >> 2) | i0	@ gioe
		.word	sgiof,	(ioF_base     >> 2) | i0	@ giof
		.word	sgiog,	(ioG_base     >> 2) | i0	@ giog
		.word	sgioh,	(ioH_base     >> 2) | i0	@ gioh
		.word	stmr0,	(timer0_base  >> 2) | i0	@ tmr0
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

ssysc:	SYMSIZE	4
	.ascii	"sysc"
	.balign 4

sVIC:	SYMSIZE	3
	.ascii	"VIC"
	.balign 4

srtc0:	SYMSIZE	4
	.ascii	"rtc0"
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
	@ sets/clears bit in CLKCON (peripherals are enabled by default at startup)
	@ on entry:	sv1 <- bit position	(scheme int)
	@ on entry:	sv2 <- val (1 or 0)	(scheme int)
	@ on exit:	sv1 <- npo
	PFUNC	2			@ primitive function, two input args
	set	sv3, sv1
	set	sv4, sv2
	ldr	rva, =sys_ctrl
	set	rvb, #0x0c
	b	rcpbit
	
.balign	4
	
scfgpn:	SYMSIZE	10
	.ascii	"config-pin"
	.balign 4
	
pcfgpn:	@ (config-pin port pin cfg . <pup>)
	@ port:		giob-gioh, port
	@ pin:		0-15, pin on port
	@ cfg:		0-3,  #b00 = input, #b01 = output, #b10 = AF 1, #b11 = AF 2
	@ pup:		0/1,  1 = enable pull-up (default)
	@ on entry:	sv1 <- port (giob to gioh)		(scheme int)
	@ on entry:	sv2 <- pin  (0 to 15)			(scheme int)
	@ on entry:	sv3 <- cfg  (0 to  3)			(scheme int)
	@ on entry:	sv4 <- (<pup>)				(list)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 3		@ primitive, init-sv4 = none, fentry = regent, narg = 3
	@ rva <- giob-h as full address (from sv1, through regent)
	@ rvb <- pin    as raw int	(from sv2, through regent)
	@ configure pin mode
	set	sv5, rva
	ldr	rvc, [sv5, #0x00]
	and	rvb, rvb, #0x0f
	lsl	rvb, rvb, #1
	set	rva, #0x03
	lsl	rva, rva, rvb
	bic	rvc, rvc, rva
	int2raw	rva, sv3
	and	rva, rva, #0x03
	lsl	rva, rva, rvb
	orr	rvc, rvc, rva
	str	rvc, [sv5, #0x00]
	@ configure pull-up
	pntrp	sv4
	bne	npofxt
	car	sv4, sv4
	int2raw	rvb, sv2
	and	rvb, rvb, #0x0f
	set	rvc, #1
	lsl	rvc, rvc, rvb
	ldr	rva, [sv5, #0x08]
	eq	sv4, #i0		@ disable pull-up?
	orreq	rva, rva, rvc		@	if so,  set   disable pull-up bit
	bicne	rva, rva, rvc		@	if not, clear disable pull-up bit
	str	rva, [sv5, #0x08]
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
	
ptstop:	@ (stop tmr chan)
	@ stops channel (0-4) of tmr0
	@ on entry:	sv1 <- tmr (tmr0 only)	(scheme int)
	@ on entry:	sv2 <- chan = 0 to 4, or null
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	nullp	sv2			@ was channel specified?
	seteq	rvb, #0			@	if not, assume channel 0
	eq	rvb, #0			@ is channel = 0?
	lslne	rvb, rvb, #2		@	if not, rvb <- 4*channel
	addne	rvb, rvb, #4		@	if not, rvb <- 4*channel + 4 = offset
	set	rvc, #0x1f		@ rvc <- channel clearing code
	lsl	rvc, rvc, rvb		@ rvc <- clearing code shifted to channel
	ldr	rvb, [rva, #0x08]	@ rvb <- current timer control
	bic	rvb, rvb, rvc		@ rvb <- timer control with channel cleared
	str	rvb, [rva, #0x08]	@ set updated control in timer
	b	npofxt

.balign	4
	
ststrt:	SYMSIZE	7
	.ascii	"restart"
	.balign 4
	
ptstrt:	@ (restart tmr)
	@ restarts tmr0 in one-shot mode
	@ on entry:	sv1 <- tmr (tmr0 only)	(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 1		@ primitive, init-sv4 = none, fentry = regent, narg = 1
	ldr	rvb, [rva, #0x08]
	bic	rvb, rvb, #0x0f
	str	rvb, [rva, #0x08]
	orr	rvb, rvb, #0x02
	str	rvb, [rva, #0x08]
	eor	rvb, rvb, #0x03
	str	rvb, [rva, #0x08]
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

