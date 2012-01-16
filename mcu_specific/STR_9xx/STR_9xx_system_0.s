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
	
		.word	s_scu,	(sys_ctrl     >> 2) | i0	@ scu  <- SCU base
		.word	sVIC,	(int_base     >> 2) | i0	@ VIC  <- VIC 0
		.word	srtc0,	(rtc0_base    >> 2) | i0	@ rtc0
		.word	sgio0,	(ioport0_base >> 2) | i0	@ gio0
		.word	sgio1,	(ioport1_base >> 2) | i0	@ gio1
		.word	sgio2,	(ioport2_base >> 2) | i0	@ gio2
		.word	sgio3,	(ioport3_base >> 2) | i0	@ gio3
		.word	sgio4,	(ioport4_base >> 2) | i0	@ gio4
		.word	sgio5,	(ioport5_base >> 2) | i0	@ gio5
		.word	sgio6,	(ioport6_base >> 2) | i0	@ gio6
		.word	sgio7,	(ioport7_base >> 2) | i0	@ gio7
		.word	sgio8,	(ioport8_base >> 2) | i0	@ gio8
		.word	sgio9,	(ioport9_base >> 2) | i0	@ gio9
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
		.word	spstdr, ppstdr			@ pin-set-dir
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

s_scu:	SYMSIZE	3
	.ascii	"scu"
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

sgio8:	SYMSIZE	4
	.ascii	"gio8"
	.balign 4

sgio9:	SYMSIZE	4
	.ascii	"gio9"
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
	@ sets/clears bit in SCU_PCGR1
	@ asserts/de-asserts reset in SCU_PRR1
	@ on entry:	sv1 <- bit position	(scheme int)
	@ on entry:	sv2 <- val (1 or 0)	(scheme int)
	@ on exit:	sv1 <- npo
	PFUNC	2			@ primitive function, two input args
	ldr	rva, =sys_ctrl
	int2raw	rvb, sv1
	set	rvc, #1
	lsl	rvc, rvc, rvb
	@ set/clear power bit
	ldr	rvb, [rva, #0x18]
	eq	sv2, #i1
	orreq	rvb, rvb, rvc
	bicne	rvb, rvb, rvc
	str	rvb, [rva, #0x18]	@ SCU_PCGR1 <- updated power state
	@ set/clear reset bit
	ldr	rvb, [rva, #0x20]
	eq	sv2, #i1
	orreq	rvb, rvb, rvc
	bicne	rvb, rvb, rvc
	str	rvb, [rva, #0x20]	@ SCU_PRR1 <- updated reset state
	b	npofxt
	
.balign	4
	
scfgpn:	SYMSIZE	10
	.ascii	"config-pin"
	.balign 4
	
pcfgpn:	@ (config-pin main sub . <ocfg> <afin> <otyp> <ana>)
	@ main:		0-7, port
	@ sub:		0-7, pin on port
	@ ocfg:		0-3, #b00=input, #b01=alt output 1, #b10=alt out 2, #b11=alt out 3
	@ afin:		0/1, alternate input 1
	@ otyp:		0/1, 0 = push-pull, 1 = open collector (gpio output only)
	@ ana:		0/1, 1 = analog in (gpio4 only)
	@ on entry:	sv1 <- main (0 or 7)			(scheme int)
	@ on entry:	sv2 <- sub  (0 to 7)			(scheme int)
	@ on entry:	sv3 <- (<ocfg> <afin> <otyp> <ana>)	(list)
	@ on exit:	sv1 <- npo
	PFUNC	2			@ primitive function, two input args
	set	sv4, sv1
	int2raw	rvb, sv1
	ldr	rvc, =sys_ctrl
	add	rvc, rvc, rvb, lsl #2
	@ configure output mode
	pntrp	sv3
	bne	npofxt
	snoc	sv1, sv3, sv3
	ldr	rva, [rvc, #0x44]
	orr	sv5, rvc, #i0
	int2raw	rvc, sv2
	and	rvc, rvc, #0x07
	lsl	rvc, rvc, #1
	set	rvb, #0x03
	lsl	rvb, rvb, rvc
	bic	rva, rva, rvb
	int2raw	rvb, sv1
	and	rvb, rvb, #0x03
	lsl	rvb, rvb, rvc
	orr	rva, rva, rvb
	bic	rvc, sv5, #i0
	str	rva, [rvc, #0x44]
	@ set single bit mask
	int2raw	rva, sv2
	set	rvb, #1
	lsl	rvb, rvb, rva
	@ configure input mode
	ldr	rva, [rvc, #0x64]
	bl	pcfgph
	str	rva, [rvc, #0x64]
	@ configure output type
	ldr	rva, [rvc, #0x84]
	bl	pcfgph
	str	rva, [rvc, #0x84]
	@ configure analog input
	eq	sv4, #((4 << 2) | i0)
	bne	npofxt
	ldr	rvc, =sys_ctrl
	ldr	rva, [rvc, #0xbc]
	bl	pcfgph
	str	rva, [rvc, #0xbc]
	b	npofxt
	
pcfgph:	@ pin configuration helper
	pntrp	sv3
	bne	npofxt
	snoc	sv1, sv3, sv3
	eq	sv1, #i1
	orreq	rva, rva, rvb
	bicne	rva, rva, rvb
	set	pc,  lnk
	
.balign	4
	
spstdr:	SYMSIZE	11
	.ascii	"pin-set-dir"
	.balign 4
	
ppstdr:	@ (pin-set-dir port pin dir)
	@ on entry:	sv1 <- port (gio0 to gio7)	(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on entry:	sv3 <- dir  (0=input, 1=output)	(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 3		@ primitive, init-sv4 = none, fentry = regent, narg = 3
	@ rva <- gio0-7 as full address (from sv1, through regent)
	set	rvb, #io_dir		@ rvb <- 0x08 = offset to pin dir reg in gio0/1
	set	sv4, sv3
	set	sv3, sv2
	b	rcpbit
	
.balign	4
	
spnset:	SYMSIZE	7
	.ascii	"pin-set"
	.balign 4
	
ppnset:	@ (pin-set port pin)
	@ on entry:	sv1 <- port (gio0-7)		(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gio0-7 as full address (from sv1, through regent)
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
	@ on entry:	sv1 <- port (gio0-7)		(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gio0-7 as full address (from sv1, through regent)
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
	@ on entry:	sv1 <- port (gio0-7)		(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- #t/#f pin status (#t = high)
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gio0-7 as full address (from sv1, through regent)
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
	str	rvb, [rva, #0x14]
	b	npofxt

.balign	4
	
ststrt:	SYMSIZE	7
	.ascii	"restart"
	.balign 4
	
ptstrt:	@ (restart tmr)
	@ on entry:	sv1 <- tmr (tmr0 or tmr1)	(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 1		@ primitive, init-sv4 = none, fentry = regent, narg = 1
	set	rvb, #0x8000
	str	rvb, [rva, #0x14]
	set	rvb, #0
	str	rvb, [rva, #0x10]	@ Note:	this may be read-only
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

