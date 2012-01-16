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
		.word	sVIC,	(int_en_base >> 2) | i0		@ VIC  <- 0xe000e100 (interrupt enable)
		.word	spsl0,	(PINSEL0     >> 2) | i0		@ psl0 <- pinsel0
		.word	srtc0,	(rtc0_base   >> 2) | i0		@ rtc0
		.word	sgio0,	(io0_base    >> 2) | i0		@ gio0
		.word	sgio1,	(io1_base    >> 2) | i0		@ gio1
		.word	sgio2,	(io2_base    >> 2) | i0		@ gio2
		.word	sgio3,	(io3_base    >> 2) | i0		@ gio3
		.word	sgio4,	(io4_base    >> 2) | i0		@ gio4
		.word	stmr0,	(timer0_base >> 2) | i0		@ tmr0
		.word	stmr1,	(timer1_base >> 2) | i0		@ tmr1
		.word	suar0,	(uart0_base  >> 2) | i0		@ uar0
		.word	suar1,	(uart1_base  >> 2) | i0		@ uar1
		.word	spwm1,	(pwm1_base   >> 2) | i0		@ pwm1
		.word	s_i2c0,	(i2c0_base   >> 2) | i0		@ i2c0
		.word	s_i2c1,	(i2c1_base   >> 2) | i0		@ i2c1
		.word	sspi0,	(spi0_base   >> 2) | i0		@ spi0
		.word	sspi1,	(spi1_base   >> 2) | i0		@ spi1
		.word	sadc0,	(adc0_base   >> 2) | i0		@ adc0
		.word	spmd0,	(PINMODE0    >> 2) | i0		@ pmd0 <- pinmode0

	@-------.-------.-------.-------.-------+
	@	utility functions (system 0)	|
	@-------.-------.-------.-------.-------+
	
		.word	scfpwr, pcfpwr			@ config-power
		.word	scfgpn, pcfgpn			@ config-pin
		.word	spstdr, ppstdr			@ pin-set-dir
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

sgio2:	SYMSIZE	4
	.ascii	"gio2"
	.balign 4

sgio3:	SYMSIZE	4
	.ascii	"gio3"
	.balign 4

sgio4:	SYMSIZE	4
	.ascii	"gio4"
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

spwm1:	SYMSIZE	4
	.ascii	"pwm1"
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

spmd0:	SYMSIZE	4
	.ascii	"pmd0"
	.balign 4

	
@---------------------------------------------------------------------------------------------------------
@  utility functions
@---------------------------------------------------------------------------------------------------------

.balign	4

scfpwr:	SYMSIZE	12
	.ascii	"config-power"
	.balign 4
	
pcfpwr:	@ (config-power bit val)
	@ sets or clears bit in PCONP
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
	
pcfgpn:	@ (config-pin main sub cfg . <mod> <od>)
	@ configure a pin, from P0.0 to P2.13
	@ cfg:		#b00 to #b11
	@ <mod>:	#b00 = pull-up, #b01 = repeater, #b10 = no pull-up/dn, #b11 = pull-down
	@ <od>:		0 or 1, open drain
	@ on entry:	sv1 <- main (0 to 2)				(scheme int)
	@ on entry:	sv2 <- sub  (0 to 31)				(scheme int)
	@ on entry:	sv3 <- cfg (eg. #b01)				(scheme int)
	@ on entry:	sv4 <- (<mod> <od>) = rest of input args	(list)
	@ on exit:	sv1 <- npo
	PFUNC	3			@ primitive function, three input args
	int2raw	rvc, sv1		@ rvc <- main part of pin (raw int)
	lsl	rvc, rvc, #3		@ rvc <- pinsel/mod offset from pinsel0/mod0
	cmp	sv2, #65		@ is pin (sub) > 15?
	it	pl
	addpl	rvc, rvc, #4		@	if so, rvc <- offset adjusted for pins 16-31
	raw2int	sv5, rvc		@ sv5 <- pinsel/mod offset, saved (scheme int)
	@ update pin configuration
	ldr	rvb, =PINSEL0		@ rvb <- pinsel base
	set	sv1, sv3		@ sv1 <- cfg
	bl	pcfgph			@ rva <- updated pin configuration word, rvc <- offset
	ldr	rvb, =PINSEL0		@ rvb <- pinsel base
	str	rva, [rvb, rvc]		@ update pin configuration in pinsel
	@ update pin mode
	nullp	sv4			@ mod specified?
	beq	npofxt			@	if not, return
	ldr	rvb, =PINMODE0		@ rvb <- pinmod base
	snoc	sv1, sv4, sv4		@ sv1 <- mod, sv4 <- (<od>)
	bl	pcfgph			@ rva <- updated pin mode word, rvc <- offset
	ldr	rvb, =PINMODE0
	str	rva, [rvb, rvc]
	@ update pin open-drain functionality
	nullp	sv4			@ open-drain specified?
	beq	npofxt			@	if not, return
	set	sv3, sv2		@ sv3 <- sub, 0 to 31 = bit pos	(scheme int)
	car	sv4, sv4		@ sv4 <- od, 0 or 1		(scheme int)
	ldr	rva, =PINMODE_OD0	@ rva <- pinmod open-drain base address
	lsr	rvb, rvc, #1		@ rvb <- OD register offset, unaligned
	bic	rvb, rvb, #0x03		@ rvb <- OD register offset, aligned
	b	rcpbit			@ jump to register-copy-bit

_func_
pcfgph:	@ update configuration/mode bits helper function
	@ on entry:	rvb <- pinsel/pinmod base address
	@ on entry:	rvc <- offset of pin conf/mod, from base	(raw int)
	@ on entry:	sv1 <- new configuration/mode			(scheme int)
	@ on entry:	sv2 <- sub (input arg of config-pin function)	(scheme int)
	@ on entry:	sv5 <- offset of pin conf/mod, from base	(scheme int)
	@ on exit:	rva <- updated configuration/mode word		(raw int)
	@ modifies:	rva, rvb
	ldr	rva, [rvb, rvc]		@ rva <- current pin configurations/mode
	int2raw	rvc, sv2		@ rvc <- bit mask position/2
	and	rvc, rvc, #0x0f		@ rvc <- bit mask position modulo 16
	lsl	rvc, rvc, #1		@ rvc <- mask position
	set	rvb, #0x03		@ rvb <- base mask
	lsl	rvb, rvb, rvc		@ rvb <- mask, shifted in place
	bic	rva, rva, rvb		@ rva <- pin configurations/modes with mask cleared
	int2raw	rvb, sv1		@ rvb <- new configuration/mode
	lsl	rvb, rvb, rvc		@ rvb <- new configuration/mode, shifted in place
	orr	rva, rva, rvb		@ rva <- full new configuration/mode
	int2raw	rvc, sv5		@ rvc <- pinsel/mod offset
	set	pc,  lnk
	
.balign	4
	
spstdr:	SYMSIZE	11
	.ascii	"pin-set-dir"
	.balign 4
	
ppstdr:	@ (pin-set-dir port pin dir)
	@ on entry:	sv1 <- port (gio0 to gio4)	(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on entry:	sv3 <- dir  (0=input, 1=output)	(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 3		@ primitive, init-sv4 = none, fentry = regent, narg = 3
	@ rva <- gio0-4 as full address (from sv1, through regent)
	set	rvb, #io_dir		@ rvb <- 0x08 = offset to pin dir reg in gio0-4
	set	sv4, sv3
	set	sv3, sv2
	b	rcpbit
	
.balign	4
	
spnset:	SYMSIZE	7
	.ascii	"pin-set"
	.balign 4
	
ppnset:	@ (pin-set port pin)
	@ on entry:	sv1 <- port (gio0 to gio4)	(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gio0-4 as full address (from sv1, through regent)
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
	@ on entry:	sv1 <- port (gio0 to gio4)	(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- npo
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gio0-4 as full address (from sv1, through regent)
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
	@ on entry:	sv1 <- port (gio0 to gio4)	(scheme int)
	@ on entry:	sv2 <- pin			(scheme int)
	@ on exit:	sv1 <- #t/#f pin status (#t = high)
	EPFUNC	0, oregent, 2		@ primitive, init-sv4 = none, fentry = regent, narg = 2
	@ rva <- gio0-4 as full address (from sv1, through regent)
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

