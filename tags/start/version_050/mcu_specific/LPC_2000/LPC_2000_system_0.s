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
	
		.word	ssysc,	(sys_ctrl    >> 2) | i0		@ sysc <- sys_ctrl
		.word	sVIC,	(int_base    >> 2) | i0		@ VIC
		.word	spsl0,	(PINSEL0     >> 2) | i0		@ psl0 <- pinsel0
		.word	srtc0,	(rtc0_base   >> 2) | i0		@ rtc0
		.word	sgio0,	(io0_base    >> 2) | i0		@ gio0
		.word	sgio1,	(io1_base    >> 2) | i0		@ gio1
		.word	stmr0,	(timer0_base >> 2) | i0		@ tmr0
		.word	stmr1,	(timer1_base >> 2) | i0		@ tmr1
		.word	suar0,	(uart0_base  >> 2) | i0		@ uar0
		.word	suar1,	(uart1_base  >> 2) | i0		@ uar1
		.word	spwm0,	(pwm0_base   >> 2) | i0		@ pwm0
		.word	s_i2c0,	(i2c0_base   >> 2) | i0		@ i2c0
		.word	s_i2c1,	(i2c1_base   >> 2) | i0		@ i2c1
		.word	sspi0,	(spi0_base   >> 2) | i0		@ spi0
		.word	sspi1,	(spi1_base   >> 2) | i0		@ spi1
		.word	sadc0,	(adc0_base   >> 2) | i0		@ adc0
.ifndef LPC2478_STK
		.word	sadc1,	(adc1_base   >> 2) | i0		@ adc1
.endif
.ifdef LPC2478_STK
		.word	spwm1,	(pwm1_base   >> 2) | i0		@ pwm1
		.word	spmd0,	(pmod0_base  >> 2) | i0		@ pmd0 <- pinmod0
		.word	smci,	(mci_base    >> 2) | i0		@ mci
		.word	sgdma,	(gdma_base   >> 2) | i0		@ gdma <- gpdma
		.word	sbdma,	(bdma_base   >> 2) | i0		@ bdma <- dma 512 byte buffer
.endif

	@-------.-------.-------.-------.-------+
	@	utility functions (system 0)	|
	@-------.-------.-------.-------.-------+

		.word	scfpwr, pcfpwr			@ config-power
		.word	scfgpn, pcfgpn			@ config-pin
		.word	spstdr, ppstdr			@ pin-set-dir
		.word	spnset, ppnset			@ pin-set
		.word	spnclr, ppnclr			@ pin-clear
		.word	spnstq, ppnstq			@ pin-set?
		.word	ststrt, ptstrt			@ restart (timer)
		.word	ststop, ptstop			@ stop (timer)
.ifndef LPC2478_STK
		.word	sspput, pspput			@ spi-put
		.word	sspget, pspget			@ spi-get
.endif
	

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

spsl0:	SYMSIZE	4
	.ascii	"psl0"
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

sspi0:	SYMSIZE	4
	.ascii	"spi0"
	.balign 4

sspi1:	SYMSIZE	4
	.ascii	"spi1"
	.balign 4

sadc0:	SYMSIZE	4
	.ascii	"adc0"
	.balign 4

.ifndef LPC2478_STK
	
sadc1:	SYMSIZE	4
	.ascii	"adc1"
	.balign 4

.endif

.ifdef LPC2478_STK
	
spwm1:	SYMSIZE	4
	.ascii	"pwm1"
	.balign 4

spmd0:	SYMSIZE	4
	.ascii	"pmd0"
	.balign 4

smci:	SYMSIZE	3
	.ascii	"mci"
	.balign 4

sgdma:	SYMSIZE	4
	.ascii	"gdma"
	.balign 4

sbdma:	SYMSIZE	4
	.ascii	"bdma"
	.balign 4

.endif
	
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
	set	sv3, sv1
	set	sv4, sv2
	ldr	rva, =sys_ctrl
	set	rvb, #0xc4
	b	rcpbit
	
.balign	4
	
scfgpn:	SYMSIZE	10
	.ascii	"config-pin"
	.balign 4
	
pcfgpn:	@ (config-pin main sub cfg <mod>)
	@ on entry:	sv1 <- main (0 or 1)		(scheme int)
	@ on entry:	sv2 <- sub  (0 to 31)		(scheme int)
	@ on entry:	sv3 <- cfg (eg. #b01)		(scheme int)
	@ on entry:	sv4 <- mod or null (eg. #b11)	(scheme int or no arg)
	@ on exit:	sv1 <- npo
	PFUNC	4			@ primitive function, four input args
	int2raw	rvc, sv1
	lsl	rvc, rvc, #3
	cmp	sv2, #65
	addpl	rvc, rvc, #4
	raw2int	sv5, rvc
	ldr	rvb, =PINSEL0
	ldr	rva, [rvb, rvc]
	int2raw	rvc, sv2
	and	rvc, rvc, #0x0f
	lsl	rvc, rvc, #1
	set	rvb, #0x03
	lsl	rvb, rvb, rvc
	bic	rva, rva, rvb
	int2raw	rvb, sv3
	lsl	rvb, rvb, rvc
	orr	rva, rva, rvb
	int2raw	rvc, sv5
	ldr	rvb, =PINSEL0
	str	rva, [rvb, rvc]
.ifdef LPC2478_STK
	int2raw	rvc, sv5
	ldr	rvb, =pmod0_base
	ldr	rva, [rvb, rvc]
	int2raw	rvc, sv2
	and	rvc, rvc, #0x0f
	lsl	rvc, rvc, #1
	set	rvb, #0x03
	lsl	rvb, rvb, rvc
	bic	rva, rva, rvb
	int2raw	rvb, sv4
	lsl	rvb, rvb, rvc
	orr	rva, rva, rvb
	int2raw	rvc, sv5
	ldr	rvb, =pmod0_base
	str	rva, [rvb, rvc]
.endif
	b	npofxt

.balign	4
	
spstdr:	SYMSIZE	11
	.ascii	"pin-set-dir"
	.balign 4
	
ppstdr:	@ (pin-set-dir port pin dir)
	@ on entry:	sv1 <- port (gio0 or gio1)	(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on entry:	sv3 <- dir  (0=input, 1=output)	(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 3		@ primitive, init-sv4 = none, fentry = regent, narg = 3
	@ rva <- gio0/1 as full address (from sv1, through regent)
	set	rvb, #io_dir		@ rvb <- 0x08 = offset to pin dir reg in gio0/1
	set	sv4, sv3
	set	sv3, sv2
	b	rcpbit
	
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
	
ststop:	SYMSIZE	4
	.ascii	"stop"
	.balign 4
	
ptstop:	@ (stop tmr)
	@ on entry:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 1		@ primitive, init-sv4 = none, fentry = regent, narg = 1
	set	rvb, #0
	str	rvb, [rva, #0x04]
	b	npofxt

.balign	4
	
ststrt:	SYMSIZE	7
	.ascii	"restart"
	.balign 4
	
ptstrt:	@ (restart tmr)
	@ on entry:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 1		@ primitive, init-sv4 = none, fentry = regent, narg = 1
	set	rvb, #2
	str	rvb, [rva, #0x04]
	set	rvb, #1
	str	rvb, [rva, #0x04]
	b	npofxt


.ifndef LPC2478_STK
	
.balign	4
	
sspput:	SYMSIZE	7
	.ascii	"spi-put"
	.balign 4
	
pspput:	@ (spi-put port val)
	@ on entry:	sv1 <- port (spi0 or spi1)	(scheme int)
	@ on entry:	sv2 <- val
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
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

.endif	@ .ifndef LPC2478_STK

	
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

