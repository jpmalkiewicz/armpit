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
	
ISR_vector:

	VECSIZE	num_interrupts

	.word	i0			@ 0
	.word	i0			@ 1
	.word	i0			@ 2
	.word	i0			@ 3
	.word	i0			@ 4
	.word	i0			@ 5
	.word	i0			@ 6
	.word	i0			@ 7
	.word	i0			@ 8
	.word	i0			@ 9
	.word	i0			@ 10
	.word	i0			@ 11
	.word	i0			@ 12
	.word	i0			@ 13
	.word	i0			@ 14
	.word	i0			@ 15
	.word	i0			@ 16
	.word	i0			@ 17
	.word	i0			@ 18
	.word	i0			@ 19
	.word	i0			@ 20
	.word	i0			@ 21
	.word	i0			@ 22
	.word	i0			@ 23
	.word	i0			@ 24
	.word	i0			@ 25
	.word	i0			@ 26
	.word	i0			@ 27
	.word	i0			@ 28
	.word	i0			@ 29
	.word	i0			@ 30
	.word	i0			@ 31
	.word	i0			@ 32
	.word	i0			@ 33
	.word	i0			@ 34
	.word	i0			@ 35
	.word	i0			@ 36
	.word	ptmisr			@ 37:	timer0
	.word	ptmisr			@ 38:	timer1
	.word	i0			@ 39
	.word	i0			@ 40
	.word	i0			@ 41
	.word	i0			@ 42
	.word	i0			@ 43
	.word	i0			@ 44
	.word	i0			@ 45
	.word	i0			@ 46
	.word	i0			@ 47
	.word	i0			@ 48
	.word	i0			@ 49
	.word	i0			@ 50
	.word	i0			@ 51
	.word	i0			@ 52
	.word	i0			@ 53
	.word	i0			@ 54
	.word	i0			@ 55
	.word	pi2isr			@ 56:	i2c0 (if included)
	.word	pi2isr			@ 57:	i2c1 (if included)
	.word	i0			@ 58
	.word	i0			@ 59
	.word	i0			@ 60
	.word	i0			@ 61
	.word	i0			@ 62
	.word	i0			@ 63
	.word	i0			@ 64
	.word	i0			@ 65
	.word	i0			@ 66
	.word	i0			@ 67
	.word	i0			@ 68
	.word	i0			@ 69
	.word	i0			@ 70
	.word	i0			@ 71
	.word	i0			@ 72
	.word	i0			@ 73
	.word	puaisr			@ 74:	uart0/1
	.word	i0			@ 75
	.word	i0			@ 76
	.word	i0			@ 77
	.word	i0			@ 78
	.word	i0			@ 79
	.word	i0			@ 80
	.word	i0			@ 81
	.word	i0			@ 82
	.word	i0			@ 83
	.word	i0			@ 84
	.word	i0			@ 85
	.word	i0			@ 86
	.word	i0			@ 87
	.word	i0			@ 88
	.word	i0			@ 89
	.word	i0			@ 90
	.word	i0			@ 91
	.word	usbisr			@ 92:	USB - no DMA (if included)
	.word	usbisr			@ 93:	USB - DMA (if included)
	.word	i0			@ 94
	.word	i0			@ 95


hwinit:	@ Enable the I/F clocks for Timer1 and Timer2, set source as sys clock
	ldr	rva, =WKUP_CM_base		@ GPTimer1
	ldr	rvb, [rva, #0x40]
	orr	rvb, rvb, #0x01
	str	rvb, [rva, #0x40]
	ldr	rva, =PER_CM_base		@ GPTimer2
	ldr	rvb, [rva, #0x40]
	orr	rvb, rvb, #0x01
	str	rvb, [rva, #0x40]
	
.ifdef TI_Beagle

	@ configure LEDs
	@ configure pins/pads for GPIO: set MuxMode to mode 4, p.130, p.35
	@  gpio_149 on CONTROL_PADCONF_UART1_TX  pad control
	@  gpio_150 on CONTROL_PADCONF_UART1_CTS pad control
	ldr	rva, =SCM_base		@ rva <- SCM base (System Control Module, p.131)
	add	rva, rva, #0x0100
	ldr	rvb, [rva, #0x7c]	@ CONTROL_PADCONF_UART1_TX (CTS is at +4)
	bic	rvb, rvb, #0x00ff0000
	bic	rvb, rvb, #0xff000000
	orr	rvb, rvb, #0x00040000
	str	rvb, [rva, #0x7c]
	ldr	rvb, [rva, #0x80]	@ CONTROL_PADCONF_UART1_CTS
	bic	rvb, rvb, #0x0007
	bic	rvb, rvb, #0x0100
	orr	rvb, rvb, #0x04
	str	rvb, [rva, #0x80]

.endif	@ TI_Beagle
	
.ifdef TI_Beagle_XM

	@ configure LEDs
	@ configure pins/pads for GPIO: set MuxMode to mode 4, p.130, p.35
	@  gpio_149 on CONTROL_PADCONF_UART1_TX  pad control
	@  gpio_150 on CONTROL_PADCONF_UART1_CTS pad control
	ldr	rva, =SCM_base		@ rva <- SCM base (System Control Module, p.131)
	add	rva, rva, #0x0100
	ldr	rvb, [rva, #0x7c]	@ CONTROL_PADCONF_UART1_TX (CTS is at +4)
	bic	rvb, rvb, #0x00ff0000
	bic	rvb, rvb, #0xff000000
	orr	rvb, rvb, #0x00040000
	str	rvb, [rva, #0x7c]
	ldr	rvb, [rva, #0x80]	@ CONTROL_PADCONF_UART1_CTS
	bic	rvb, rvb, #0x0007
	bic	rvb, rvb, #0x0100
	orr	rvb, rvb, #0x04
	str	rvb, [rva, #0x80]

.endif	@ TI_Beagle_XM
	
.ifdef GMX_OVERO_TIDE	@ LEDs on Thumbo board

	@ configure LEDs on Thumbo
	@ configure pins/pads for GPIO: set MuxMode to mode 4, p.130, p.35
	@  gpio_21 on CONTROL_PADCONF_ETK_D6 pad control
	@  gpio_22 on CONTROL_PADCONF_ETK_D8 pad control
	ldr	rva, =SCM_base		@ rva <- SCM base (System Control Module, p.131)
	add	rva, rva, #0x0500
	ldr	rvb, [rva, #0xe8]
	bic	rvb, rvb, #0x00ff0000
	bic	rvb, rvb, #0xff000000
	orr	rvb, rvb, #0x00040000
	str	rvb, [rva, #0xe8]
	ldr	rvb, [rva, #0xec]
	bic	rvb, rvb, #0x0007
	bic	rvb, rvb, #0x0100
	orr	rvb, rvb, #0x04
	str	rvb, [rva, #0xec]

.endif	@ GMX_OVERO_TIDE

	@ Set LED pins as outputs
	ldr	rva, =LEDIO
	ldr	rvb, [rva, #io_dir]
	bic	rvb, rvb, #ALLLED
	str	rvb, [rva, #io_dir]
	bl	gldon
	@ initialization of UART3 (aka uart0) for 9600 8N1 operation
	ldr	rva, =SCM_base		@ rva <- SCM base (System Control Module, p.131)
	add	rva, rva, #0x0100
	ldr	rvb, [rva, #0x9c]	@ CONTROL_PADCONF_UART3_RX
	bic	rvb, rvb, #0x00ff0000
	bic	rvb, rvb, #0xff000000
	orr	rvb, rvb, #0x01000000
	str	rvb, [rva, #0x9c]
	ldr	rvb, [rva, #0xa0]
	bic	rvb, rvb, #0x00ff
	bic	rvb, rvb, #0x0100
	orr	rvb, rvb, #0x0000
	str	rvb, [rva, #0xa0]
	@ assumes:	power, clocks, pins configured by ROM
	ldr	rva, =uart0_base
	set	rvb, #0x07
	str	rvb, [rva, #0x20]	@ MDR1 <- disable uart operation
	set	rvb, #0x80
	str	rvb, [rva, #0x0c]	@ LCR <- enable divisor latch
	set	rvb, #UART_DIVL
	str	rvb, [rva, #0x00]	@ DLL <- 26 (low divisor for 115200 baud)
	set	rvb, #UART_DIVH
	str	rvb, [rva, #0x04]	@ DLH <-  0 (high divisor for 115200 baud)
	set	rvb, #0x03
	str	rvb, [rva, #0x0c]	@ LCR <- disable divisor latch, set 8N1 format
	set	rvb, #0x00
	str	rvb, [rva, #0x20]	@ MDR1 <- enable uart operation (16x mode)
	set	rvb, #0x01
	str	rvb, [rva, #0x04]	@ UCON0 <- enable Rx interrupt
	@ I2C
	ldr	rva,  =i2c0_base	@ rva  <- I2C0 (i.e. I2C1 on MCU) base address
	bl	hw_i2c_reset
	@ USB 
	ldr	rva, =USB_CONF
	set	rvb, #0x00
	str	rvb, [rva]		@ USB_CONF <- USB device is not yet configured
	@ initialize interrupts
	ldr	rva, =int_base
	set	rvb, #0x60		@ timer 0 and 1 (1 and 2 on MCU)
	str	rvb, [rva, #0xa8]	@ enable timer 0, 1 ints in INTMSK
	set	rvb, #0x0400		@ uart 0 and 1 (uart 3 on MCU)
	str	rvb, [rva, #0xc8]	@ enable uart 0, 1 ints in INTMSK
	@ turn one board LED off
	bl	rldoff

.ifndef	live_SD
		
	@ unlock flash
	ldr	sv1, =GPMC_base
	ldr	sv2, =F_START_PAGE
	ldr	sv3, =F_END_PAGE
	set	rvb, #0x23		@ rvb <- flash unlock start command
	orr	rvb, rvb, rvb, lsl #16
	str	rvb, [sv1, #0x7c]	@ set read command in gpmc
	eor	rva, sv2, #0x88000000	@ rva <- flash start destination address
	lsr	rva, rva, #11		@ rva <- destination, shifted
	bic	rvb, rva, #0xff0000	@ rva <- destination, low 16 bits
	and	rvc, rvb, #0xff00
	bic	rvb, rvb, #0xff00
	orr	rvb, rvb, rvc, lsl #8
	str	rvb, [sv1, #0x80]	@ set block/page(low) address in gpmc
	lsr	rva, rva, #16		@ rva <- destination, shifted
	str	rva, [sv1, #0x80]	@ set block/page(high) address in gpmc
	set	rvb, #0x24		@ rvb <- flash unlock end command
	orr	rvb, rvb, rvb, lsl #16
	str	rvb, [sv1, #0x7c]	@ set read command in gpmc
	eor	rva, sv3, #0x88000000	@ rva <- flash end destination address
	lsr	rva, rva, #11		@ rva <- destination, shifted
	bic	rvb, rva, #0xff0000	@ rva <- destination, low 16 bits
	and	rvc, rvb, #0xff00
	bic	rvb, rvb, #0xff00
	orr	rvb, rvb, rvc, lsl #8
	str	rvb, [sv1, #0x80]	@ set block/page(low) address in gpmc
	lsr	rva, rva, #16		@ rva <- destination, shifted
	str	rva, [sv1, #0x80]	@ set block/page(high) address in gpmc
	bl	flstwt
	@ copy file NAND Flash into Shadow SDRAM (32MB)
	ldr	sv1, =GPMC_base
	ldr	sv2, =F_START_PAGE
	ldr	sv3, =F_END_PAGE
flcp_1:	set	rvb, #0x00		@ rvb <- flash read command
	str	rvb, [sv1, #0x7c]	@ set read command in gpmc
	str	rvb, [sv1, #0x80]	@ set byte start offset(low) address=0 in gpmc
	eor	rva, sv2, #0x88000000	@ rva <- flash destination address
	lsr	rva, rva, #11		@ rva <- destination, shifted
	bic	rvb, rva, #0xff0000	@ rva <- destination, low 16 bits
	and	rvc, rvb, #0xff00
	bic	rvb, rvb, #0xff00
	orr	rvb, rvb, rvc, lsl #8
	str	rvb, [sv1, #0x80]	@ set block/page(low) address in gpmc
	lsr	rva, rva, #16		@ rva <- destination, shifted
	str	rva, [sv1, #0x80]	@ set block/page(high) address in gpmc
	set	rvb, #0x30		@ rvb <- flash read confirm command
	orr	rvb, rvb, rvb, lsl #16
	str	rvb, [sv1, #0x7c]	@ set confirm command in gpmc
	bl	flstwt
	set	rvb, #0x00		@ rvb <- flash read command
	str	rvb, [sv1, #0x7c]	@ set read command in gpmc
	set	rvc, #0
flcp_2:	ldr	rvb, [sv1, #0x84]	@ rvb <- data from Flash (16-bits)
	str	rvb, [sv2, rvc]		@ store it in RAM
	add	rvc, rvc, #4
	eq	rvc, #F_PAGE_SIZE
	bne	flcp_2
	add	sv2, sv2, #F_PAGE_SIZE
	eq	sv2, sv3
	bne	flcp_1

.endif	
	
.ifdef	native_usb

	ldr	rva, =USB_LineCoding
	ldr	rvb, =115200
	str	rvb, [rva]		@ 115200 bauds
	set	rvb, #0x00080000
	str	rvb, [rva, #0x04]	@ 8 data bits, no parity, 1 stop bit
	ldr	rva, =USB_CHUNK
	str	fre, [rva]		@ zero bytes remaining to send at startup
	ldr	rva, =USB_ZERO
	str	fre, [rva]		@ alternate interface and device/interface status = 0
	ldr	rva, =USB_CONF
	str	fre, [rva]		@ USB device is not yet configured
	ldr	rva, =USB_FSHS_MODE
	str	fre, [rva]		@ indicate that USB is not yet in HS mode
	
  .ifdef TI_Beagle_XM

	@ configure USB pads to mode 0
	ldr	rva, =SCM_base		@ rva <- SCM base (System Control Module, p.131)
	add	rva, rva, #0x0100
	set	rvc, #0x9c
usbpcf:	add	rvc, rvc, #4
	ldr	rvb, [rva, rvc]		@rvb <-  CONTROL_PADCONF_HSUSB0
	cmp	rvc, #0xa4
	bicpl	rvb, rvb, #0x00000007
	cmp	rvc, #0xb8
	bicmi	rvb, rvb, #0x00070000
	str	rvb, [rva, rvc]
	bmi	usbpcf
	@ try to get USB running (1st try -- didn't seem to work without i2c below)
	ldr	rva, =usb_base
	add	rva, rva, #0x0400
	set	rvb, #0x02
	str	rvb, [rva, #0x04]	@ reset usb otg, no clock auto-gating
usbrwt:	ldr	rvb, [rva, #0x08]
	tst	rvb, #1
	beq	usbrwt
	set	rvb, #0x1000
	orr	rvb, rvb, #0x08
	str	rvb, [rva, #0x04]	@ run with no idle clock
	set	rvb, #0
	str	rvb, [rva, #0x14]	@ de-assert standby
	@ power-up USB-OTG PHY in TPS69950/TWL4030 (DM3730 / beagleBoard-XM)
	ldr	rva, =i2c1_base		@ rva <- I2C1 base address
	ldr	rvc, =0x00d94b		@ rvc <- slave-adr = #x4b, reg = #xd9, val = #x00
	set	sv3, #i2		@ sv3 <- number of i2c bytes to write (1 reg + 1 data)
	bl	hw_i2c_write		@ clear sleep mode in vusb_dedicated2
	ldr	rvc, =0xe0cc4b		@ rvc <- slave-adr = #x4b, reg = #xcc, val = #xe0
	set	sv3, #i2		@ sv3 <- number of i2c bytes to write (1 reg + 1 data)
	bl	hw_i2c_write		@ set vusb1v5_dev_grp to all groups
	ldr	rvc, =0xe0cf4b		@ rvc <- slave-adr = #x4b, reg = #xcf, val = #xe0
	set	sv3, #i2		@ sv3 <- number of i2c bytes to write (1 reg + 1 data)
	bl	hw_i2c_write		@ set vusb1v8_dev_grp to all groups
usbplw:	@ wait for USB-OTG PHY PLL lock
	set	rvc, #(1 << 24)
usbpw0:	@ wait a bit
	subs	rvc, rvc, #1
	bne	usbpw0	
	ldr	rvc, =0x00ff48		@ rvc <- slave-adr = #x48, reg = #xff
	set	sv3, #i1		@ sv3 <- number of i2c bytes to write (1 reg)
	bl	hw_i2c_read		@ rvc <- byte read from remote i2c register PHY_CLK_CTRL_STS
	tst	rvc, #0x01		@ is PLL locked?
	beq	usbplw			@	if not, jump to keep waiting
	@ enable USB-OTG power
	ldr	rvc, =0x20ac48		@ rvc <- slave-adr = #x48, reg = #xac, val = #x20
	set	sv3, #i2		@ sv3 <- number of i2c bytes to write (1 reg + 1 data)
	bl	hw_i2c_write		@ set OTG_EN in POWER_CTRL

  .endif

	@ configure interrupts and peripheral
	ldr	rva, =int_base
	set	rvb, #0x10000000	@ usb = interrupt 92
	str	rvb, [rva, #0xc8]	@ enable usb ints (clear bit in INTMSK)
	ldr	rva, =usb_base
	ldr	rvb, =0xffff
	strh	rvb, [rva, #0x06]	@ INTRTXE  <- enable EP0-15 transmit interrupts
	ldr	rvb, =0xfffe
	strh	rvb, [rva, #0x08]	@ INTRRXE  <- enable EP1-15 receive  interrupts
	set	rvb, #0x04
	strb	rvb, [rva, #0x0b]	@ INTRUSBE <- enable reset interrupt
  .ifndef has_HS_USB
	set	rvb, #0x40
  .else
	set	rvb, #0x60
  .endif
	strb	rvb, [rva, #0x01]	@ POWER    <- set softcon and possibly HSEN

.endif
		
	@ turn the other board LED off
	bl	gldoff
	@ end of the hardware initialization
	b	scinit			@ jump to start scheme


.ifndef	live_SD
		
flstwt:	@ wait for flash ready
	@ on entry:	sv1 <- GPMC_base
	@ modifies:	rvb
	set	rvb, #0x70
	orr	rvb, rvb, rvb, lsl #16
	str	rvb, [sv1, #0x7c]	@ set get status command in gpmc
flstw0:	ldr	rvb, [sv1, #0x84]	@ rvb <- flash status
	and	rvb, rvb, #0x60
	eq	rvb, #0x60
	bne	flstw0
	set	pc,  lnk

.endif
	
	
@------------------------------------------------------------------------------------------------
@  OMAP 35xx / DM 37xx
@
@	 1- Initialization from FLASH, writing to and erasing FLASH
@	 2- I2C Interrupt routine
@
@------------------------------------------------------------------------------------------------
	
@
@ 1- Initialization from FLASH, writing to and erasing FLASH
@

.ifdef TI_Beagle

FlashInitCheck: @ return status of flash init enable/override gpio pin (GPIO1:gpio_7, SYS_Boot5) in rva
	ldr	rvb, =io1_base			@ rvb <- address of GPIO for USER button
	ldr	rvb, [rvb, #io_state]		@ rvb <- status of all pins
	and	rva, rvb, #0x80			@ rva <- status of gpio_7 only (high if pressed)
	eor	rva, rva, #0x80			@ rva <- return value (gpio_7 inverted, low if pressed)
	set	pc,  lnk			@ return

.endif	@ TI_Beagle
	
.ifdef TI_Beagle_XM

FlashInitCheck: @ return status of flash init enable/override gpio pin (GPIO1:gpio_7, SYS_Boot5) in rva
	ldr	rvb, =io1_base			@ rvb <- address of GPIO for USER button
	ldr	rvb, [rvb, #io_state]		@ rvb <- status of all pins
	and	rva, rvb, #0x80			@ rva <- status of gpio_7 only (high if pressed)
	eor	rva, rva, #0x80			@ rva <- return value (gpio_7 inverted, low if pressed)
	set	pc,  lnk			@ return

.endif	@ TI_Beagle_XM
	
.ifdef GMX_OVERO_TIDE	@ button on Thumbo board

FlashInitCheck: @ return status of flash init enable/override gpio pin (GPIO1:gpio_14) in rva
	ldr	rvb, =io1_base			@ rvb <- address of GPIO for Thumbo button
	ldr	rvb, [rvb, #io_state]		@ rvb <- status of all pins
	and	rva, rvb, #(1 << 14)		@ rva <- status of gpio_14 only (low if pressed)
	set	pc,  lnk			@ return

.endif	@ GMX_OVERO_TIDE


.ifndef	live_SD
	
wrtfla:	@ write to flash, sv2 is page address, sv4 is file descriptor
	swi	run_no_irq			@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk}	@ store scheme registers onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5,  env, dts, glv} @ store scheme registers onto stack
	@ copy buffer data from file descriptor (sv4) (RAM) to RAM and FLASH buffer (sv2)
	ldr	sv1, =GPMC_base
	vcrfi	sv3, sv4, 3			@ sv3 <- buffer address
	add	sv3, sv3, #4
	@ configure gpmc
	set	rvb, #0x80		@ rvb <- flash write command
	orr	rvb, rvb, rvb, lsl #16
	str	rvb, [sv1, #0x7c]	@ set read command in gpmc
	set	rvb, #0x00		@ rvb <- 0, start offset for write
	str	rvb, [sv1, #0x80]	@ set byte start offset(low) address=0 in gpmc
	eor	rva, sv2, #0x88000000	@ rva <- flash destination address
	lsr	rva, rva, #11		@ rva <- destination, shifted
	bic	rvb, rva, #0xff0000	@ rva <- destination, low 16 bits
	and	rvc, rvb, #0xff00
	bic	rvb, rvb, #0xff00
	orr	rvb, rvb, rvc, lsl #8
	str	rvb, [sv1, #0x80]	@ set block/page(low) address in gpmc
	lsr	rva, rva, #16		@ rva <- destination, shifted
	str	rva, [sv1, #0x80]	@ set block/page(high) address in gpmc
	@ write data
	set	rvc, #0
wrtfl1:	ldr	rvb, [sv1, #0x54]	@ rvb <- write buffer status
	tst	rvb, #0x01
	beq	wrtfl1
	ldr	rvb, [sv3, rvc]
	str	rvb, [sv2, rvc]
	str	rvb, [sv1, #0x84]
	add	rvc, rvc, #4
	eq	rvc, #F_PAGE_SIZE
	bne	wrtfl1
	@ complete write, check status
	set	rvb, #0x10		@ rvb <- flash write confirm command
	str	rvb, [sv1, #0x7c]	@ set confirm command in gpmc
	bl	flstwt
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ restore scheme registers from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk}	@ restore scheme registers from stack
	orr	fre, fre, #0x02			@ fre <- fre-ptr de-reserved
	swi	run_normal			@ enable interrupts (user mode)
	set	pc,  lnk			@ return

ersfla:	@ erase flash sector that contains page address in sv2
	swi	run_no_irq			@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk}	@ store scheme registers onto stack
	stmfd	sp!, {sv1}			@ store scheme registers onto stack
	@ perform FLash block erase
	ldr	sv1, =GPMC_base
	set	rvb, #0x60		@ rvb <- flash block erase command
	orr	rvb, rvb, rvb, lsl #16
	str	rvb, [sv1, #0x7c]	@ set block erase command in gpmc
	eor	rva, sv2, #0x88000000	@ rva <- flash destination address
	lsr	rva, rva, #11		@ rva <- destination, shifted
	bic	rvb, rva, #0xff0000	@ rva <- destination, low 16 bits
	and	rvc, rvb, #0xff00
	bic	rvb, rvb, #0xff00
	orr	rvb, rvb, rvc, lsl #8
	str	rvb, [sv1, #0x80]	@ set block/page(low) address in gpmc
	lsr	rva, rva, #16		@ rva <- destination, shifted
	str	rva, [sv1, #0x80]	@ set block/page(high) address in gpmc
	set	rvb, #0xd0		@ rvb <- flash block erase confirm command
	str	rvb, [sv1, #0x7c]	@ set confirm command in gpmc
	@ erase corresponding shadow RAM
	set	rvc, #0
	mvn	rvb, rvc
ersfl1:	str	rvb, [sv2, rvc]		@ store #xffffffff in RAM
	add	rvc, rvc, #4
	eq	rvc, #0x20000		@ 128kB / block
	bne	ersfl1
	@ check flash status/wait for flash ready
	bl	flstwt
	@ finish up
	ldmfd	sp!, {sv1}			@ restore scheme registers from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk}	@ restore scheme registers from stack
	orr	fre, fre, #0x02			@ fre <- fre-ptr de-reserved
	swi	run_normal			@ enable interrupts (user mode)
	set	pc,  lnk			@ return


.ifdef TI_Beagle
flashsectors:	@ 256 x 128KB RAM Blocks shadowing file FLASH (top 32MB) of Micron MT29F2G16ABC on board
.word	0x86000000, 0x86020000,  0x86040000,  0x86060000,  0x86080000,  0x860A0000,  0x860C0000,  0x860E0000
.word	0x86100000, 0x86120000,  0x86140000,  0x86160000,  0x86180000,  0x861A0000,  0x861C0000,  0x861E0000
.word	0x86200000, 0x86220000,  0x86240000,  0x86260000,  0x86280000,  0x862A0000,  0x862C0000,  0x862E0000
.word	0x86300000, 0x86320000,  0x86340000,  0x86360000,  0x86380000,  0x863A0000,  0x863C0000,  0x863E0000
.word	0x86400000, 0x86420000,  0x86440000,  0x86460000,  0x86480000,  0x864A0000,  0x864C0000,  0x864E0000
.word	0x86500000, 0x86520000,  0x86540000,  0x86560000,  0x86580000,  0x865A0000,  0x865C0000,  0x865E0000
.word	0x86600000, 0x86620000,  0x86640000,  0x86660000,  0x86680000,  0x866A0000,  0x866C0000,  0x866E0000
.word	0x86700000, 0x86720000,  0x86740000,  0x86760000,  0x86780000,  0x867A0000,  0x867C0000,  0x867E0000
.word	0x86800000, 0x86820000,  0x86840000,  0x86860000,  0x86880000,  0x868A0000,  0x868C0000,  0x868E0000
.word	0x86900000, 0x86920000,  0x86940000,  0x86960000,  0x86980000,  0x869A0000,  0x869C0000,  0x869E0000
.word	0x86A00000, 0x86a20000,  0x86a40000,  0x86a60000,  0x86a80000,  0x86aA0000,  0x86aC0000,  0x86aE0000
.word	0x86B00000, 0x86b20000,  0x86b40000,  0x86b60000,  0x86b80000,  0x86bA0000,  0x86bC0000,  0x86bE0000
.word	0x86C00000, 0x86c20000,  0x86c40000,  0x86c60000,  0x86c80000,  0x86cA0000,  0x86cC0000,  0x86cE0000
.word	0x86D00000, 0x86d20000,  0x86d40000,  0x86d60000,  0x86d80000,  0x86dA0000,  0x86dC0000,  0x86dE0000
.word	0x86E00000, 0x86e20000,  0x86e40000,  0x86e60000,  0x86e80000,  0x86eA0000,  0x86eC0000,  0x86eE0000
.word	0x86F00000, 0x86f20000,  0x86f40000,  0x86f60000,  0x86f80000,  0x86fA0000,  0x86fC0000,  0x86fE0000
.word	0x87000000, 0x87020000,  0x87040000,  0x87060000,  0x87080000,  0x870A0000,  0x870C0000,  0x870E0000
.word	0x87100000, 0x87120000,  0x87140000,  0x87160000,  0x87180000,  0x871A0000,  0x871C0000,  0x871E0000
.word	0x87200000, 0x87220000,  0x87240000,  0x87260000,  0x87280000,  0x872A0000,  0x872C0000,  0x872E0000
.word	0x87300000, 0x87320000,  0x87340000,  0x87360000,  0x87380000,  0x873A0000,  0x873C0000,  0x873E0000
.word	0x87400000, 0x87420000,  0x87440000,  0x87460000,  0x87480000,  0x874A0000,  0x874C0000,  0x874E0000
.word	0x87500000, 0x87520000,  0x87540000,  0x87560000,  0x87580000,  0x875A0000,  0x875C0000,  0x875E0000
.word	0x87600000, 0x87620000,  0x87640000,  0x87660000,  0x87680000,  0x876A0000,  0x876C0000,  0x876E0000
.word	0x87700000, 0x87720000,  0x87740000,  0x87760000,  0x87780000,  0x877A0000,  0x877C0000,  0x877E0000
.word	0x87800000, 0x87820000,  0x87840000,  0x87860000,  0x87880000,  0x878A0000,  0x878C0000,  0x878E0000
.word	0x87900000, 0x87920000,  0x87940000,  0x87960000,  0x87980000,  0x879A0000,  0x879C0000,  0x879E0000
.word	0x87A00000, 0x87a20000,  0x87a40000,  0x87a60000,  0x87a80000,  0x87aA0000,  0x87aC0000,  0x87aE0000
.word	0x87B00000, 0x87b20000,  0x87b40000,  0x87b60000,  0x87b80000,  0x87bA0000,  0x87bC0000,  0x87bE0000
.word	0x87C00000, 0x87c20000,  0x87c40000,  0x87c60000,  0x87c80000,  0x87cA0000,  0x87cC0000,  0x87cE0000
.word	0x87D00000, 0x87d20000,  0x87d40000,  0x87d60000,  0x87d80000,  0x87dA0000,  0x87dC0000,  0x87dE0000
.word	0x87E00000, 0x87e20000,  0x87e40000,  0x87e60000,  0x87e80000,  0x87eA0000,  0x87eC0000,  0x87eE0000
.word	0x87F00000, 0x87f20000,  0x87f40000,  0x87f60000,  0x87f80000,  0x87fA0000,  0x87fC0000,  0x87fE0000
.word	0x88000000
.endif	@ .ifdef TI_Beagle


.endif	@ .ifndef live_SD

	
@------------------------------------------------------------------------------------------------
@
@ 2- SD card low-level interface
@
@------------------------------------------------------------------------------------------------

.ifdef	onboard_SDFT
		
_sgb:	@ [internal only]
	@ sd-get-block internal func
	@ on entry:  rvc <- block number to be read (scheme int)
	@ on entry:  sv3 <- buffer in which to store block data (scheme bytevector)
	@ on exit:   sv3 <- updated buffer
	@ modifies:  sv3, sv5, rva, rvb, rvc
	bic	sv5, lnk, #lnkbit0
sgb_sr:	@ start/restart transfer
	bl	sd_pre
	@ set arg and perform command
	set	rvb, rvc
	bl	sd_arg
	ldr	rvb, =0x113a0010	@ 11|+24<cmd17, 3|23:20<nrml,dat,idxchk, a|19:16<crc,48b,rsp, 1|4<rd
	bl	sd_cmd
	eq	rva, #0
	beq	sgb_sr
sgb_wt:	@ wait for data (loop)
	ldr	rvb, [rva, #0x30]	@ rvb <- MMCHS_STAT
	tst	rvb, #0x8000
	bne	sgb_sr
	tst	rvb, #0x20
	beq	sgb_wt
	orr	rvb, rvb, #0x20
	str	rvb, [rva, #0x30]
	@ get data
	set	rvc, #4
sgb_cp:	@ data-read loop
	ldr	rvb, [rva, #0x20]	@ rvb <- MMCHS_DATA
	str	rvb, [sv3, rvc]
	eq	rvc, #512
	addne	rvc, rvc, #4
	bne	sgb_cp
	ldr	rvb, [rva, #0x30]	@ rvb <- MMCHS_STAT
	orr	rvb, rvb, #0x02
	str	rvb, [rva, #0x30]
	@ return
	orr	lnk, sv5, #lnkbit0
	set	pc,  lnk

_spb:	@ [internal only]
	@ sd-put-block internal func
	@ on entry:  rvc <- block number to be write (scheme int)
	@ on entry:  sv3 <- buffer with block data to write to sd (scheme bytevector)
	@ modifies:  sv5, rva, rvb, rvc
	bic	sv5, lnk, #lnkbit0
spb_sr:	@ start/restart transfer
	bl	sd_pre
	@ set arg and perform command
	set	rvb, rvc
	bl	sd_arg
	ldr	rvb, =0x183a0000	@ 18|+24<cmd24, 3|23:20<nrml,dat,idxchk, a|19:16<crc,48b,rsp, 0|4<wrt
	bl	sd_cmd
	eq	rva, #0
	beq	spb_sr
spb_wc:	@ wait for card ready
	ldr	rvb, [rva, #0x24]	@ rvb <- MMCHS_PSTATE
	tst	rvb, #0x0400
	beq	spb_wc
	@ put data
	set	rvc, #4
spb_cp:	@ data-write loop
	ldr	rvb, [sv3, rvc]
	str	rvb, [rva, #0x20]	@ MMCHS_DATA <- data word
	eq	rvc, #512
	addne	rvc, rvc, #4
	bne	spb_cp
spb_wt:	@ wait for end of write
	ldr	rvb, [rva, #0x30]	@ rvb <- MMCHS_STAT
	tst	rvb, #0x8000
	bne	spb_er			@ jump to error/restart transfer
	tst	rvb, #0x02
	beq	spb_wt
	orr	rvb, rvb, #0x02
	str	rvb, [rva, #0x30]	@ MMCHS_STAT <- clear state
	@ return
	orr	lnk, sv5, #lnkbit0
	set	pc,  lnk
spb_er:	@ re-get arg and restart
	ldr	rvc, [rva, #0x08]	@ arg <- MMCHS_ARG
	lsr	rvc, rvc, #7
	b	spb_sr

sd_pre:	@ mci-prep subroutine
	ldr	rva, =sd_mci
	add	rva, rva, #0x100
	ldr	rvb, [rva, #0x24]	@ rvb <- MMCHS_PSTATE
	tst	rvb, #0x07
	beq	sd_pr0
	@ reset MMC data/command lines
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_SYSCTL
	orr	rvb, rvb, #0x02000000
	str	rvb, [rva, #0x2c]
	ldr	rvb, [rva, #0x2c]
	orr	rvb, rvb, #0x04000000
	str	rvb, [rva, #0x2c]
sd_pr0:	@ clear previous state
	ldr	rvb, [rva, #0x30]	@ rvb <- MMCHS_STAT
	str	rvb, [rva, #0x30]
	@ set data length
	set	rvb, #512
	str	rvb, [rva, #0x04]	@ MMCHS_BLK <- 512
	set	pc,  lnk

sd_arg:	@ mci-arg subroutine (set arg)
	@ on entry: rvb <- arg (0 as raw int, or block number as scheme int)
	ldr	rva, =sd_mci
	add	rva, rva, #0x100
	bic	rvb, rvb, #0x03
	lsl	rvb, rvb, #7
	str	rvb, [rva, #0x08]	@ MMCHS_ARG <- arg
	set	pc,  lnk
		
sd_cmd:	@ mci-cmd subroutine (put cmd)
	@ on entry: rvb <- cmd
	ldr	rva, =sd_mci
	add	rva, rva, #0x100
	str	rvb, [rva, #0x0c]	@ MMCHS_CMD <- cmd
sd_cm0:	ldr	rvb, [rva, #0x30]	@ rvb <- MMCHS_STAT
	tst	rvb, #0x8000
	setne	rva, #0
	setne	pc,  lnk
	tst	rvb, #0x01
	beq	sd_cm0
	@ clear state, get response, wait for write-ready
	set	rvb, #0x01
	str	rvb, [rva, #0x30]
	ldr	rvb, [rva, #0x10]	@ rvb <- MMCHS_RSP10
	set	pc,  lnk

sd_slo:	@ configure mci speed (low = 400 KHz), 1-bit bus, clock enabled
	@ (perform reset first)
	@ modifies:	rva, rvb
	ldr	rva, =sd_mci
	@ reset mci peripheral
	ldr	rvb, [rva, #0x10]	@ rvb <- MMCHS_SYSCONFIG
	orr	rvb, rvb, #0x01
	str	rvb, [rva, #0x10]
sd_srw:	@ wait for reset
	ldr	rvb, [rva, #0x14]	@ rvb <- MMCHS_SYSSTATUS
	tst	rvb, #0x01
	beq	sd_srw
	@ reset mci peripheral
	add	rva, rva, #0x100	@ rva <- mci base address + 0x100 (for ldr/str)
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_SYSCTL
	orr	rvb, rvb, #(1 << 24)
	str	rvb, [rva, #0x2c]
sd_saw:	@ wait for reset
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_SYSCTL
	tst	rvb, #(1 << 24)
	bne	sd_saw
	@ 3-Volt operation
	ldr	rvb, [rva, #0x28]	@ rvb <- MMCHS_HCTL
	bic	rvb, rvb, #(0x7 << 9)
	orr	rvb, rvb, #(0x6 << 9)
	str	rvb, [rva, #0x28]
	@ set timeout counter (#xe), 160 KHz clock (#x258)
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_SYSCTL
	bic	rvb, rvb, #(0xff << 6)
	bic	rvb, rvb, #(0x3f << 14)
	orr	rvb, rvb, #(0x58 << 6)
	orr	rvb, rvb, #(0x3a << 14)
	str	rvb, [rva, #0x2c]
	@ enable clock
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_SYSCTL
	orr	rvb, rvb, #0x01
	str	rvb, [rva, #0x2c]	@ MMCHS_SYSCTL <- clock enabled
sd_sbw:	@ wait for clock stable
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_SYSCTL
	tst	rvb, #0x02
	beq	sd_sbw
	@ connect clock-out to card
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_SYSCTL
	orr	rvb, rvb, #0x04
	str	rvb, [rva, #0x2c]	@ MMCHS_SYSCTL <- card clocked
	@ power-up the bus
	ldr	rvb, [rva, #0x28]	@ rvb <- MMCHS_HCTL
	orr	rvb, rvb, #(1 << 8)
	str	rvb, [rva, #0x28]
	@ enable events
	ldr	rvb, =0x307f0033
	str	rvb, [rva, #0x34]
	@ send initialization stream (2 x CMD0, with init-stream bit set in CON)
	sub	rva, rva, #0x100	@ rva <- mci base address
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_CON
	orr	rvb, rvb, #0x02
	str	rvb, [rva, #0x2c]	@ set init-stream bit
	add	rva, rva, #0x100	@ rva <- mci base address + 0x100 (for ldr/str)
	set	rvb, #0
	str	rvb, [rva, #0x0c]	@ MMCHS_CMD <- CMD0
sd_scw:	@ wait for command complete
	ldr	rvb, [rva, #0x30]	@ rvb <- MMCHS_STAT
	tst	rvb, #0x01
	beq	sd_scw
	str	rvb, [rva, #0x30]	@ clear command-complete bit
	set	rvb, #0
	str	rvb, [rva, #0x0c]	@ MMCHS_CMD <- CMD0
sd_sdw:	@ wait for command complete
	ldr	rvb, [rva, #0x30]	@ rvb <- MMCHS_STAT
	tst	rvb, #0x01
	beq	sd_sdw
	str	rvb, [rva, #0x30]	@ clear command-complete bit
	sub	rva, rva, #0x100	@ rva <- mci base address
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_CON
	bic	rvb, rvb, #0x02
	str	rvb, [rva, #0x2c]	@ clear init-stream bit
	add	rva, rva, #0x100	@ rva <- mci base address + 0x100 (for ldr/str)
	@ disconnect clock-out to card
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_SYSCTL
	bic	rvb, rvb, #0x04
	str	rvb, [rva, #0x2c]	@ MMCHS_SYSCTL <- card not clocked
	@ disable clock
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_SYSCTL
	bic	rvb, rvb, #0x01
	str	rvb, [rva, #0x2c]	@ MMCHS_SYSCTL <- clock disabled
	@ change clock freq
	bic	rvb, rvb, #0x0ff00
	bic	rvb, rvb, #0x000c0
	orr	rvb, rvb, #0x03c00	@ 400 KHz
	str	rvb, [rva, #0x2c]	@ MMCHS_SYSCTL <- new speed
	@ enable clock
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_SYSCTL
	orr	rvb, rvb, #0x01
	str	rvb, [rva, #0x2c]	@ MMCHS_SYSCTL <- clock enabled
sd_slw:	@ wait for clock stable
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_SYSCTL
	tst	rvb, #0x02
	beq	sd_slw
	@ re-connect clock-out to card
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_SYSCTL
	orr	rvb, rvb, #0x04
	str	rvb, [rva, #0x2c]	@ MMCHS_SYSCTL <- card clocked
	set	pc,  lnk

sd_fst:	@ configure mci speed (high = 19 MHz), wide bus, clock enabled
	@ modifies:	rva, rvb
	ldr	rva, =sd_mci
	add	rva, rva, #0x100
	@ disconnect clock-out to card
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_SYSCTL
	bic	rvb, rvb, #0x04
	str	rvb, [rva, #0x2c]	@ MMCHS_SYSCTL <- card not clocked
	@ disable clock
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_SYSCTL
	bic	rvb, rvb, #0x01
	str	rvb, [rva, #0x2c]	@ MMCHS_SYSCTL <- clock disabled
	@ change clock freq
	bic	rvb, rvb, #0x0ff00
	bic	rvb, rvb, #0x000c0
	orr	rvb, rvb, #0x00140	@ 19 MHz
	str	rvb, [rva, #0x2c]	@ MMCHS_SYSCTL <- new speed
	@ enable clock
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_SYSCTL
	orr	rvb, rvb, #0x01
	str	rvb, [rva, #0x2c]	@ MMCHS_SYSCTL <- clock enabled
sd_fsw:	@ wait for clock stable
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_SYSCTL
	tst	rvb, #0x02
	beq	sd_fsw
	@ re-connect clock-out to card
	ldr	rvb, [rva, #0x2c]	@ rvb <- MMCHS_SYSCTL
	orr	rvb, rvb, #0x04
	str	rvb, [rva, #0x2c]	@ MMCHS_SYSCTL <- card clocked
	@ 4-bit bus
	ldr	rvb, [rva, #0x28]	@ rvb <- MMCHS_HCTL
	orr	rvb, rvb, #0x02
	str	rvb, [rva, #0x28]
	set	pc,  lnk
	
sdpcmd:	@ function to write a command to SD/MMC card during initialization
	@ on entry:	sv4 <- cmd (scheme int)
	@ on entry:	rvc <- arg (raw int)
	@ on exit:	rvb <- response0
	@ modifies:	rva, rvb
	ldr	rva, =sd_mci
	add	rva, rva, #0x100
	ldr	rvb, [rva, #0x30]	@ rvb <- MMCHS_STAT
	str	rvb, [rva, #0x30]	@ clear stat flags
	str	rvc, [rva, #0x08]	@ MMCHS_ARG <- arg
	int2raw	rvb, sv4
	and	rvb, rvb, #0xff
	lsl	rvb, rvb, #24
	eq	sv4, #i0
	beq	sdpcm0
	eq	sv4, #((41 << 2) | i0)
	orreq	rvb, rvb, #(0x02 << 16)
	beq	sdpcm0
	tst	sv4, #0x10000000
	orreq	rvb, rvb, #(0x1a << 16)
	orrne	rvb, rvb, #(0x09 << 16)
	eq	sv4, #((0x06 << 2) | i0)
	eqne	sv4, #((0x07 << 2) | i0)
	orreq	rvb, rvb, #(0x1b << 16)
sdpcm0:	@ continue
	str	rvb, [rva, #0x0c]	@ MMCHS_CMD <- cmd
sdpcmb:	@ wait for cmd complete
	ldr	rvb, [rva, #0x30]	@ rvb <- MMCHS_STAT
	tst	rvb, #0x8000
	tsteq	rvb, #0x01
	beq	sdpcmb
	@ wait for transfer complete if needed
	eq	sv4, #((0x06 << 2) | i0)
	eqne	sv4, #((0x07 << 2) | i0)
	bne	sdpcm1
sdpcmt:	ldr	rvb, [rva, #0x30]	@ rvb <- MMCHS_STAT
	tst	rvb, #0x8000
	tsteq	rvb, #0x02
	beq	sdpcmt
sdpcm1:	@ continue
	set	rvb, #0x1000000
sdpcmw:	@ wait a bit more (some cards seem to need this)
	subs	rvb, rvb, #1
	bne	sdpcmw
	@ if CMD3 (get address), check status and exit with indicator if bad
	eq	sv4, #0x0d		@ CMD3?
	bne	sdpcmc
	ldr	rvb, [rva, #0x30]	@ rvb <- MMCHS_STAT
	tst	rvb, #0x8000
	setne	rvb, #0
	setne	pc,  lnk
sdpcmc:	@ continue
	@ clear state, get response
	set	rvb, #0x01
	str	rvb, [rva, #0x30]
	ldr	rvb, [rva, #0x10]	@ rvb <- MMCHS_RSP10
	@ return
	set	pc,  lnk

.endif	@ onboard_SDFT

.ltorg

@------------------------------------------------------------------------------------------------
@  OMAP_35xx
@
@ 2- I2C polling and Interrupt routine
@
@------------------------------------------------------------------------------------------------

hw_i2c_reset: @ reset the i2c module
	@ on entry:	rva <- base address of i2c module
	@ modifies:	rvb
	set	rvb, #0x02
	strh	rvb, [rva, #0x20]	@ reset in I2C_SYSC
i2crst:	@ wait for reset completion
	set	rvb, #0x8000
	strh	rvb, [rva, #i2c_cset]	@ enable i2c in I2C_CON
	ldrh	rvb, [rva, #0x10]	@ rvb <- reset status from I2C_SYSS
	tst	rvb, #1
	beq	i2crst
	@ configure i2c module
	set	rvb, #0x01
	strh	rvb, [rva, #0x20]	@ enable auto-idling in I2C_SYSC
	set	rvb, #0x07
	strh	rvb, [rva, #0x30]	@ set prescaler I2C_PSC
	set	rvb, #0x08
	strh	rvb, [rva, #0x34]	@ set low clock duration I2C_SCLL
	set	rvb, #0x0A
	strh	rvb, [rva, #0x38]	@ set high clock duration I2C_SCLH
	set	rvb, #mcu_id
	strh	rvb, [rva, #i2c_address] @ set mcu address in I2C_OA0
	set	rvb, #0x8000
	strh	rvb, [rva, #i2c_cset]	@ enable i2c in I2C_CON
	set	pc,  lnk

hw_i2c_write: @ write bytes (reg+data) to an i2c slave
	@ on entry:	rva <- base address of i2c module
	@ on entry:	rvc <- address and bytes to write, slave-adr=lsB, last-data=msB
	@ on entry:	sv3 <- number of reg+data bytes to write (scheme int)
	@ modifies:	rvb, rvc, sv3
	@ wait for bus not busy
	ldrh	rvb, [rva, #i2c_status]	@ rvb <- current status
	eq	rvb, #0			@ is status zero (module fresh out of reset)?
	beq	i2c_wc			@	if so,  jump to continue
	tst	rvb, #(1 << 12)		@ is bus busy?
	bne	hw_i2c_write		@	if so,  jump to keep waiting
	tst	rvb, #(1 << 2)		@ is module command-ready?
	beq	hw_i2c_write		@	if not, jump to keep waiting
	@ check for state errors
	ldrh	rvb, [rva, #i2c_status]	@ rvb <- current status
	tst	rvb, #0x08		@ is module RdRdy (a state error)?
	beq	i2c_wc			@	if not, jump to continue
	@ clear Tx and Rx FIFOs
	ldr	rvb, =0x4040
	strh	rvb, [rva, #0x14]	@ clear Tx and Rx FIFOs
	set	rvb, #(1 << 24)		@ rvb <- wait countdown
i2c_w7:	subs	rvb, rvb, #1		@ done waiting for reset?
	bne	i2c_w7			@	if not, jump to keep waiting
	ldrh	rvb, [rva, #i2c_status]	@ rvb <- current status from I2C_STAT
i2c_wc:	@ continue and configure module for transmission as master
	strh	rvb, [rva, #i2c_status]	@ clear current status bits in I2C_STAT
	ldrh	rvb, [rva, #i2c_sadr]	@ rvb <- remote slave address from I2C_SA
	eor	rvb, rvb, rvc
	tst	rvb, #0xff
	beq	i2c_w6
	@ wait a bit on slave address change
	set	rvb, #(1 << 24)
i2c_w5:	subs	rvb, rvb, #1
	bne	i2c_w5
	and	rvb, rvc, #0xff
	strh	rvb, [rva, #i2c_sadr]	@ set remote slave address in I2C_SA
i2c_w6:	@ continue with same slave address as prior i2c op
	int2raw	rvb, sv3
	strh	rvb, [rva, #i2c_cnt]	@ set number of bytes to send in I2C_CNT
	dsb
	set	rvb, #0x8600
	orr	rvb, rvb, #0x03
	strh	rvb, [rva, #i2c_cset]	@ set master Tx with Start+Stop in I2C_CON
i2c_w0:	@ loop to write data bytes
	eq	sv3, #i0		@ nothing left to write?
	seteq	pc,  lnk		@	if so,  return
	ldrh	rvb, [rva, #i2c_status]	@ rvb <- current status from I2C_STAT
	tst	rvb, #(1 << 4)		@ is module TxRdy?
	beq	i2c_w0			@	if not, jump to keep waiting
	lsr	rvc, rvc, #8		@ rvc <- remaining data to write
	and	rvb, rvc, #0xff		@ rvb <- byte to send now
	strh	rvb, [rva, #i2c_thr]	@ send register or data byte via I2C_DATA
	sub	sv3, sv3, #4		@ sv3 <- updated number of bytes to send
	b	i2c_w0			@ jump to send next byte
	
hw_i2c_read: @ read bytes from an i2c slave
	@ Normally performed after hw_i2c_write which sets register to read from
	@ on entry:	rva <- base address of i2c module
	@ on entry:	rvc <- slave's i2c address
	@ on entry:	sv3 <- number of data bytes to read = 1 or 2 (scheme int)
	@ on exit:	rvc <- bytes read
	@ modifies:	rvb, rvc, sv3
	@ wait for bus not busy
	ldrh	rvb, [rva, #i2c_status]	@ rvb <- current status
	eq	rvb, #0
	beq	i2c_rc
	tst	rvb, #(1 << 2)		@ is module command-ready?
	beq	hw_i2c_read
	tst	rvb, #(1 << 12)
	bne	hw_i2c_read
	ldrh	rvb, [rva, #i2c_status]	@ rvb <- current status
	tst	rvb, #0x08		@ is module RdRdy (a state error)?
	beq	i2c_rc			@	if not, jump to continue
	@ clear Tx and Rx FIFOs
	ldr	rvb, =0x4040
	strh	rvb, [rva, #0x14]	@ clear Tx and Rx FIFOs
	set	rvb, #(1 << 24)
i2c_r7:	subs	rvb, rvb, #1
	bne	i2c_r7
	ldrh	rvb, [rva, #i2c_status]	@ rvb <- current status
i2c_rc:	@ continue and configure module for reception as master
	strh	rvb, [rva, #i2c_status]	@ clear current status bits
	ldrh	rvb, [rva, #i2c_sadr]	@ rvb <- remote slave address from I2C_SA
	eor	rvb, rvb, rvc
	tst	rvb, #0xff
	beq	i2c_r6
	@ wait a bit on slave address change
	set	rvb, #(1 << 24)
i2c_r5:	subs	rvb, rvb, #1
	bne	i2c_r5
	and	rvb, rvc, #0xff
	strh	rvb, [rva, #i2c_sadr]	@ set remote slave address in I2C_SA
i2c_r6:	@ continue with same slave address as prior i2c op
	set	rvb, #0x01
	strh	rvb, [rva, #i2c_cnt]	@ set number of bytes to send in I2C_CNT
	dsb
	set	rvb, #0x8600
	orr	rvb, rvb, #0x01
	strh	rvb, [rva, #i2c_cset]	@ set master Tx with Start in I2C_CON
i2c_r0:	@ write destination register
	ldrh	rvb, [rva, #i2c_status]	@ rvb <- current status from I2C_STAT
	tst	rvb, #(1 << 4)		@ is module TxRdy?
	beq	i2c_r0			@	if not, jump to keep waiting
	lsr	rvb, rvc, #8		@ rvc <- remaining data to write
	and	rvb, rvb, #0xff		@ rvb <- byte to send now
	strh	rvb, [rva, #i2c_thr]	@ send register or data byte via I2C_DATA
i2c_r1:	@ wait for module ready
	ldrh	rvb, [rva, #i2c_status]	@ rvb <- current status from I2C_STAT
	tst	rvb, #(1 << 2)		@ is module command-ready?
	beq	i2c_r1			@	if not, jump to keep waiting
	int2raw	rvb, sv3
	strh	rvb, [rva, #i2c_cnt]	@ set number of bytes to read in I2C_CNT
	dsb
	set	rvb, #0x8400
	orr	rvb, rvb, #0x03
	strh	rvb, [rva, #i2c_cset]	@ set master Rx with Start+Stop in I2C_CON
	set	rvc, #0			@ rvc <- initial result
i2c_r2:	@ loop to read data bytes
	tst	sv3, #0x1c		@ nothing left to read?
	seteq	pc,  lnk		@	if so,  return
	ldrh	rvb, [rva, #i2c_status]	@ rvb <- current status from I2C_STAT
	tst	rvb, #(1 << 3)		@ is module RxRdy?
	beq	i2c_r2			@	if not, jump to keep waiting
	ldrh	rvb, [rva, #i2c_rhr]	@ rvb <- received data byte from I2C_DATA
	and	rvb, rvb, #0xff		@ rvb <- received byte, trimmed
	tst	sv3, #(1 << 16)
	lslne	rvb, rvb, #8
	orr	rvc, rvc, rvb		@	if so,  rvc <- updated result
	sub	sv3, sv3, #4		@ sv3 <- updated number of bytes to receive
	eor	sv3, sv3, #(1 << 16)
	set	rvb, #(1 << 3)
	strh	rvb, [rva, #i2c_status]	@ clear RxRdy bit in I2C_STAT
	b	i2c_r2			@ jump to get next byte
	
	
.ifdef	include_i2c

hwi2cr:	@ write-out additional address registers, if needed
	
hwi2ni:	@ initiate i2c read/write, as master
	
hwi2st:	@ get i2c interrupt status and base address
	
i2c_hw_branch:	@ process interrupt
	
hwi2we:	@ set busy status/stop bit at end of write as master
	
hwi2re:	@ set stop bit if needed at end of read-as-master
	
hwi2cs:	@ clear SI
	
i2cstp:	@ prepare to end Read as Master transfer
	
i2putp:	@ Prologue:	write additional address bytes to i2c, from buffer or r12 (prologue)
	
i2pute:	@ Epilogue:	set completion status if needed (epilogue)
	set	pc,  lnk

.endif
