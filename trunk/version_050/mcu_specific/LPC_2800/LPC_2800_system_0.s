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
		.word	srtc0,	(rtc0_base   >> 2) | i0		@ rtc0
		.word	sgio0,	(io0_base    >> 2) | i0		@ gio0
		.word	sgio1,	(io1_base    >> 2) | i0		@ gio1
		.word	sgio2,	(io2_base    >> 2) | i0		@ gio2
		.word	sgio3,	(io3_base    >> 2) | i0		@ gio3
		.word	sgio4,	(io4_base    >> 2) | i0		@ gio4
		.word	sgio5,	(io5_base    >> 2) | i0		@ gio5
		.word	sgio6,	(io6_base    >> 2) | i0		@ gio6
		.word	sgio7,	(io7_base    >> 2) | i0		@ gio7
		.word	stmr0,	(timer0_base >> 2) | i0		@ tmr0
		.word	stmr1,	(timer1_base >> 2) | i0		@ tmr1
		.word	suar0,	(uart0_base  >> 2) | i0		@ uar0
		.word	s_i2c0,	(i2c0_base   >> 2) | i0		@ i2c0
		.word	sadc0,	(adc0_base   >> 2) | i0		@ adc0
		.word	smci,	(mci_base    >> 2) | i0		@ mci
		.word	sgdma,	(gdma_base   >> 2) | i0		@ gdma <- gpdma
		.word	slcd,	(lcd_base    >> 2) | i0		@ lcd

	@-------.-------.-------.-------.-------+
	@	utility functions (system 0)	|
	@-------.-------.-------.-------.-------+

		.word	scfgpn, pcfgpn			@ config-pin
		.word	spnset, ppnset			@ pin-set
		.word	spnclr, ppnclr			@ pin-clear
		.word	spnstq, ppnstq			@ pin-set?
		.word	ststrt, ptstrt			@ restart (timer)
		.word	ststop, ptstop			@ stop (timer)
	

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

sgio0:	SYMSIZE	4
	.ascii	"gio0"
	.balign 4

sgio1:	SYMSIZE	4
	.ascii	"gio1"
	.balign 4

sgio2:	SYMSIZE	4
	.ascii	"gio2"
	.balign 4

sgio3:	SYMSIZE	4
	.ascii	"gio3"
	.balign 4

sgio4:	SYMSIZE	4
	.ascii	"gio4"
	.balign 4

sgio5:	SYMSIZE	4
	.ascii	"gio5"
	.balign 4

sgio6:	SYMSIZE	4
	.ascii	"gio6"
	.balign 4

sgio7:	SYMSIZE	4
	.ascii	"gio7"
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

s_i2c0:	SYMSIZE	4
	.ascii	"i2c0"
	.balign 4

sadc0:	SYMSIZE	4
	.ascii	"adc0"
	.balign 4

smci:	SYMSIZE	3
	.ascii	"mci"
	.balign 4

sgdma:	SYMSIZE	4
	.ascii	"gdma"
	.balign 4

slcd:	SYMSIZE	3
	.ascii	"lcd"
	.balign 4

	
@---------------------------------------------------------------------------------------------------------
@  utility functions
@---------------------------------------------------------------------------------------------------------

.balign	4
	
scfgpn:	SYMSIZE	10
	.ascii	"config-pin"
	.balign 4
	
pcfgpn:	@ (config-pin main sub m0 m1)
	@ m0 m1:	0 0 -> GP input,	0 1 -> GP output low
	@		1 0 -> peripheral func,	1 1 -> GP output high
	@ on entry:	sv1 <- main (0 to  7)		(scheme int)
	@ on entry:	sv2 <- sub  (0 to 31)		(scheme int)
	@ on entry:	sv3 <- m0   (0 or  1)		(scheme int)
	@ on entry:	sv4 <- m1   (0 or  1)		(scheme int)
	@ on exit:	sv1 <- npo
	PFUNC	4			@ primitive function, four input args
	@ calculate address
	int2raw	rvc, sv1
	set	rvb, #0x40
	mul	rvb, rvc, rvb
	ldr	rva, =io0_base
	add	rva, rva, rvb
	@ position update bit
	int2raw	rvc, sv2
	set	rvb, #1
	lsl	rvb, rvb, rvc
	@ set MODE 0
	eq	sv3, #i1
	streq	rvb, [rva, #0x14]
	strne	rvb, [rva, #0x18]
	@ set MODE 1
	eq	sv4, #i1
	streq	rvb, [rva, #0x24]
	strne	rvb, [rva, #0x28]
	@ return
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
	
ststop:	SYMSIZE	4
	.ascii	"stop"
	.balign 4
	
ptstop:	@ (stop tmr)
	@ on entry:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 1		@ primitive, init-sv4 = none, fentry = regent, narg = 1
	set	rvb, #0
	str	rvb, [rva, #0x08]
	b	npofxt

.balign	4
	
ststrt:	SYMSIZE	7
	.ascii	"restart"
	.balign 4
	
ptstrt:	@ (restart tmr)
	@ on entry:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 1		@ primitive, init-sv4 = none, fentry = regent, narg = 1
	ldr	rvb, =2344
	str	rvb, [rva, #0x00]
	set	rvb, #0xc8
	str	rvb, [rva, #0x08]
	b	npofxt


@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

