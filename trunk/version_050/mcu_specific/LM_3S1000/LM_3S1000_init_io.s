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
	.word	puaisr			@ 5:	uart0
	.word	puaisr			@ 6:	uart1
	.word	i0			@ 7
	.word	pi2isr			@ 8:	i2c0 (if included)
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
	.word	ptmisr			@ 19:	timer0
	.word	i0			@ 20
	.word	ptmisr			@ 21:	timer1
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
	.word	pi2isr			@ 37:	i2c1 (if included)
	.word	i0			@ 38
	.word	i0			@ 39
	.word	i0			@ 40
	.word	i0			@ 41
	.word	i0			@ 42
	.word	i0			@ 43
	.word	usbisr			@ 44:	USB OTG FS Device (if included)
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
	.word	i0			@ 56
	.word	i0			@ 57
	.word	i0			@ 58
	.word	i0			@ 59
	.word	i0			@ 60
	.word	i0			@ 61
	.word	i0			@ 62
	.word	i0			@ 63


_func_	
hwinit:
	@ pre-set common values
	set	r0,  #0
	set	r1,  #1
	set	r2,  #2
	set	r3,  #3
	set	r4,  #4
	set	r5,  #5

.ifndef TI_EvalBot

	@ initialization of clocks
	ldr	r6,  =sys_base
	ldr	r7,  [r6,  #rcc]
	bic	r7,  r7, #0x03C0
	orr	r7,  r7, #0x0380
	str	r7,  [r6,  #rcc]	@ RCC      <- XTAL = 0xE for 8 MHz crystal
	ldr	r7,  [r6,  #rcc]
	bic	r7,  r7, #0x01
	str	r7,  [r6,  #rcc]	@ RCC      <- MOSCDIS = 0, enable main oscillator
	ldr	r7,  [r6,  #rcc]
	bic	r7,  r7, #0x07800000
	orr	r7,  r7, #0x01800000
	str	r7,  [r6,  #rcc]	@ RCC      <- SYSDIV = 0x3, divide by 4 (later on, PLL output = 50MHz)
	ldr	r7,  [r6,  #rcc]
	orr	r7,  r7, #0x00400000
	str	r7,  [r6,  #rcc]	@ RCC      <- USESYSDIV = 1, enable freq division by 4
	ldr	r7,  [r6,  #rcc]
	bic	r7,  r7, #0x30
	str	r7,  [r6,  #rcc]	@ RCC      <- OSCSRC = 0, choose main oscillator as clock
	ldr	r7,  [r6,  #rcc]
	bic	r7,  r7, #0x2000
	str	r7,  [r6,  #rcc]	@ RCC      <- PWRDN = 0, power up PLL
hwiwt0:	ldr	r7,  [r6,  #0x50]
	tst	r7,  #0x40		@ RIS      <- wait for PLL Tready  bit
	beq	hwiwt0
	ldr	r7,  [r6,  #rcc]
	bic	r7,  r7, #0x0800
	str	r7,  [r6,  #rcc]	@ RCC      <- BYPASS = 0, connect PLL

.else	@ TI_EvalBot

	@ initialization of clocks
	ldr	r6,  =sys_base
	ldr	r7,  [r6,  #rcc]
	bic	r7,  r7, #0x07C0
	orr	r7,  r7, #0x0540
	str	r7,  [r6,  #rcc]	@ RCC      <- XTAL = 0x15 for 16 MHz crystal
	
	ldr	r7,  [r6,  #rcc]
	bic	r7,  r7, #0x01
	str	r7,  [r6,  #rcc]	@ RCC      <- MOSCDIS = 0, enable main oscillator
	
	ldr	r7,  [r6,  #rcc2]
	bic	r7,  r7, #0x1fc00000
	orr	r7,  r7, #0xc1000000
	str	r7,  [r6,  #rcc2]	@ RCC2     <- USE SYSDIV2 = 0x5 with LSB2, divide by 5 (80MHz)
	ldr	r7,  [r6,  #rcc]
	orr	r7,  r7, #0x00400000
	str	r7,  [r6,  #rcc]	@ RCC      <- USESYSDIV = 1, enable freq division
	
	ldr	r7,  [r6,  #rcc2]
	bic	r7,  r7, #0x70
	str	r7,  [r6,  #rcc2]	@ RCC2     <- OSCSRC2 = 0, choose main oscillator as clock
	ldr	r7,  [r6,  #rcc2]
	bic	r7,  r7, #0x6000
	str	r7,  [r6,  #rcc2]	@ RCC2     <- PWRDN2 = 0, power up PLL and USB PLL
hwiwt0:	ldr	r7,  [r6,  #0x50]
	tst	r7,  #0x40		@ RIS      <- wait for PLL Tready  bit
	it	ne
	tstne	r7,  #0x80		@ RIS      <- wait for USB PLL Tready  bit
	beq	hwiwt0
	ldr	r7,  [r6,  #rcc2]
	bic	r7,  r7, #0x0800
	str	r7,  [r6,  #rcc2]	@ RCC2     <- BYPASS2 = 0, connect PLL

.endif

	@ initialization of USB configuration
	ldr	r7,  =USB_CONF
	str	r0,  [r7]		@ USB_CONF <- USB device is not yet configured
	@ initialize Cortex-M3 SysTick Timer
	ldr	r6,  =systick_base
.ifndef	TI_EvalBot
	ldr	r7,  =499999
.else
	ldr	r7,  =799999
.endif
	str	r7,  [r6, #tick_load]	@ SYSTICK-RELOAD  <- value for 10ms timing at 50 or 80 MHz
	str	r0,  [r6, #tick_val]	@ SYSTICK-VALUE   <- 0
	str	r5,  [r6, #tick_ctrl]	@ SYSTICK-CONTROL <- 5 = enabled, no interrupt, run from cpu clock
	@ initialization of LED gpio pins
	ldr	r6,  =sys_base
	add	r6,  r6, #0x0100
	ldr	r7,  [r6,  #0x08]
.ifdef	EVB_LM3S1968
	orr	r7,  r7, #0x40		@ bit 6, for port G clock enable
.endif
.ifdef	IDM_LM3S1958
	orr	r7,  r7, #0x40		@ bit 6, for port G clock enable
.endif
.ifdef	EVB_LM3S6965
	orr	r7,  r7, #0x20		@ bit 5, for port F clock enable
.endif
.ifdef	TI_EvalBot
	orr	r7,  r7, #0x28		@ bits 3 and 5, for ports D and F clock enable
.endif
	str	r7,  [r6,  #0x08]	@ RCGC2    <- enable clock for LED Port
	ldr	r7,  =ALLLED
	ldr	r8,  =LEDPINSEL
	str	r7,  [r8, #0x0400]	@ GPIODIR  <- all led directions set to output
	add	r8,  r8, #0x0500
	str	r7,  [r8, #0x08]	@ GPIODR8R <- all led have 8 mA drive
	str	r7,  [r8, #0x1c]	@ GPIODEN  <- all led pins active (non tri-state)
	@ initialization of boot-override button
.ifdef	EVB_LM3S1968
	ldr	r8,  =ioportg_base+0x500
	ldr	r7,  [r8,  #0x1c]
	orr	r7,  r7, #0x08
	str	r7,  [r8,  #0x1c]	@ GPIODEN  <- set UP-button (PG3) as digital in (for boot bypass)
	set	r7,  #0x08
	str	r7,  [r8,  #0x10]	@ GPIOPUR  <- add weak pull-up to UP-button (PG3)
.endif
.ifdef	IDM_LM3S1958
	ldr	r8,  =ioportg_base+0x500
	ldr	r7,  [r8,  #0x1c]
	orr	r7,  r7, #0x08
	str	r7,  [r8,  #0x1c]	@ GPIODEN  <- set PG3 as digital in (for boot bypass)
	set	r7,  #0x08
	str	r7,  [r8,  #0x10]	@ GPIOPUR  <- add weak pull-up to PG3
.endif
.ifdef	EVB_LM3S6965
	ldr	r8,  =ioportf_base+0x500
	ldr	r7,  [r8,  #0x1c]
	orr	r7,  r7, #0x02
	str	r7,  [r8,  #0x1c]	@ GPIODEN  <- set SELECT-button (PF1) as digital in (for boot bypass)
	set	r7,  #0x02
	str	r7,  [r8,  #0x10]	@ GPIOPUR  <- add weak pull-up to UP-button (PF1)
.endif
.ifdef	TI_EvalBot
	ldr	r8,  =ioportd_base+0x500
	ldr	r7,  [r8,  #0x1c]
	orr	r7,  r7, #0x40
	str	r7,  [r8,  #0x1c]	@ GPIODEN  <- set SW1-button (PD6) as digital in (for boot bypass)
	set	r7,  #0x40
	str	r7,  [r8,  #0x10]	@ GPIOPUR  <- add weak pull-up to SW1-button (PD6)
.endif
	@ initialization of UART0 for 9600 8N1 operation
	ldr	r7,  [r6,  #0x08]
	orr	r7,  r7, #0x01
	str	r7,  [r6,  #0x08]	@ RCGC2    <- 0x01, bit 0, enab clk for Port A, UART0=PA0(Rx),PA1(Tx)
	ldr	r8,  =ioporta_base+0x500
	ldr	r9,  =ioporta_base+0x400
	ldr	r7,  =0x1ACCE551
	str	r7,  [r8,  #0x20]	@ GPIOLOCK <- PORT A, unlock AFSEL
	str	r3,  [r9,  #0x20]	@ GPIOAFSEL<- UART0 function selected for pins, (GPIO Chapt. p.169)
	str	r3,  [r8,  #0x1c]	@ GPIODEN  <- UART0 pins active (non tri-state), (GPIO Chapt. p.169)
	ldr	r8,  =0x035003
	ldr	r7,  [r6,  #0x04]
	orr	r7,  r7, r8
	str	r7,  [r6,  #0x04]	@ RCGC1    <- enable clock for UART0,1, I2C0,1, Timer0,1
	nop
	nop
	nop
	nop
	ldr	r8,  =uart0_base
	ldr	r7,  [r8,  #0x30]
	bic	r7,  r7, #0x01
	str	r7,  [r8,  #0x30]	@ UARTCTL  <- disable UART0
	ldr	r7,  =UART0_IDIV
	str	r7,  [r8,  #0x24]	@ UARTIBRD
	ldr	r7,  =UART0_FDIV
	str	r7,  [r8,  #0x28]	@ UARTFBRD
	set	r7,  #0x60
	str	r7,  [r8,  #0x2c]	@ UARTLCRH <- 8,N,1, no fifo
	set	r7,  #0x10
	str	r7,  [r8,  #0x38]	@ UARTIM   <- allow Rx interrupt to vic
	ldr	r9,  =0x0301
	ldr	r7,  [r8,  #0x30]
	orr	r7,  r7,  r9
	str	r7,  [r8,  #0x30]	@ UARTCTL  <- enable UART0, Tx, Rx
	
	@ initialization of SD card pins

.ifdef	onboard_SDFT

	@ either:	
	@   SSI0:
	@     SSI pins on gpio A: PA.2,4,5 (port A is unlocked above, in UART initialization)
	@     CS  pin  on gpio A or D (eg. PD0 on EVB_LM3S6965)
	@ or:
	@   SSI1:
	@     SSI pins on gpio E: PE.0,2,3
	@     CS  pin  on gpio E (eg. PE1 on IDM_LM3S1958)
	ldr	r7,  =sd_spi
	ldr	r8,  =ssi0_base
	eq	r7,  r8			@ SSI is SSI0?
	ldr	r7,  [r6,  #0x04]	@ r7       <- RCGC1
	itE	eq
	orreq	r7,  r7, #0x10		@	if so,  r7 <- bit 4, for SSI0 clock enable
	orrne	r7,  r7, #0x20		@	if not, r7 <- bit 5, for SSI1 clock enable
	str	r7,  [r6, #0x04]	@ RCGC1    <- enable clock for SSI0 or SSI1
	ldr	r7,  [r6, #0x08]	@ r7       <- RCGC2
	itE	eq
	orreq	r7,  r7, #0x09		@	if so,  r7 <- bit 0 & 3, for ports A (SSI0) & D clock enable
	orrne	r7,  r7, #0x10		@	if not, r7 <- bit 4, for port E clock enable (SSI1)
	str	r7,  [r6, #0x08]	@ RCGC2    <- enable clock for Port(s)
	@ set SSI interface to low speed
	ldr	r8,  =sd_spi
	str	r0,  [r8,  #4]
  .ifndef TI_EvalBot
	set	rvb, #0x4000
  .else
	set	rvb, #0x6400
  .endif
	orr	r7,  r7, #0x07
	str	r7,  [r8, #0]
	str	r2,  [r8, #0x10]
	str	r2,  [r8, #0x04]
	@ configure chip-select pin and de-select card
	ldr	r8,  =sd_cs_gpio 
	ldr	r7,  [r8, #0x0400]
	orr	r7,  r7,  #(sd_cs >> 2)
	str	r7,  [r8, #0x0400]	@ GPIODIR  <- PA3 | PE1 is output
	add	r8,  r8,  #0x500
	ldr	r7,  [r8,  #0x1c]
	orr	r7,  r7,  #(sd_cs >> 2)
	str	r7,  [r8, #0x1c]	@ GPIODEN  <- PA3 | PE1 is digital
	ldr	r7,  [r8,  #0x10]
	orr	r7,  r7,  #(sd_cs >> 2)
	str	r7,  [r8,  #0x10]	@ GPIOPUR  <- PA3 | PE1 has weak pull-up
	ldr	r8,  =sd_cs_gpio
	set	r7,  #0xff
	str	r7,  [r8, #sd_cs]	@ de-select SD card (set PE1 high)
	@ configure SSI pins
	itE	eq
	seteq	r9,  #0x34		@	if so,  r9 <- PA2,4,5 are cfg as SSI for SSI0
	setne	r9,  #0x0d		@	if not, r9 <- PE0,2,3 are cfg as SSI for SSI1
	ldr	r8,  =sd_spi_gpio + 0x0500
	ldr	r7,  [r8,  #0x1c]
	orr	r7,  r7,  r9
	str	r7,  [r8,  #0x1c]	@ GPIODEN  <- PA2,4,5 | PE0,2,3 are digital
	ldr	r7,  [r8,  #0x10]
	orr	r7,  r7,  r9
	str	r7,  [r8,  #0x10]	@ GPIOPUR  <- PA2,4,5 | PE0,2,3 have weak pull-up
	sub	r8,  r8, #0x0100
	ldr	r7,  [r8,  #0x20]
	orr	r7,  r7,  r9
	str	r7,  [r8,  #0x20]	@ GPIOAFSEL <- PA2,4,5 | PE0,2,3 are SSI
	
.endif	@  onboard_SDFT

	@ initialization of mcu-id for variables (normally I2c address if slave enabled)
	ldr	r6,  =i2c0_base		@ r6  <- I2C0 base address
	set	r7,  #0x30
	str	r7,  [r6,  #0x20]	@ I2C0MCR     <- enable master and slave units
	set	r7,  #mcu_id
	str	r7,  [r6, #i2c_address]	@ I2C0ADR <- set mcu address
	@ I2C pin initialization is missing here *****************************
	@ enf of the hardware initialization
	set	pc,  lnk


@------------------------------------------------------------------------------------------------
@  LM_3S1000
@
@	 1- Initialization from FLASH, writing to and erasing FLASH
@	 2- I2C Interrupt routine
@
@------------------------------------------------------------------------------------------------
	
@
@ 1- Initialization from FLASH, writing to and erasing FLASH
@

_func_	
FlashInitCheck: @ return status of flash init enable/override gpio pin
		@ (eg. PG.3 or PF.1 -- Up or Select button) in rva (inverted)
.ifdef	EVB_LM3S1968
	ldr	rva, =ioportg_base		@ rva <- GPIO port G where PG3 is located
	ldr	rva, [rva, #0x20]		@ rva <- status of input pin 3 (bit 3, lsl 2) on Port G
.endif
.ifdef	IDM_LM3S1958
	ldr	rva, =ioportg_base		@ rva <- GPIO port G where PG3 is located
	ldr	rva, [rva, #0x20]		@ rva <- status of input pin 3 (bit 3, lsl 2) on Port G
.endif
.ifdef	EVB_LM3S6965
	ldr	rva, =ioportf_base		@ rva <- GPIO port F where PF1 is located
	ldr	rva, [rva, #0x08]		@ rva <- status of input pin 1 (bit 1, lsl 2) on Port F
.endif
.ifdef	TI_EvalBot
	ldr	rva, =ioportd_base		@ rva <- GPIO port D where PD6 (SW1) is located
	ldr	rva, [rva, #0x100]		@ rva <- status of input pin 6 (bit 6, lsl 2) on Port D
.endif
	set	pc,  lnk			@ return

_func_	
wrtfla:	@ write to flash, sv2 = page address, sv4 = file descriptor
_func_	
libwrt:	@ write to on-chip lib flash (lib shares on-chip file flash)
	swi	run_no_irq			@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, sv3, sv5}	@ store scheme registers onto stack
	ldr	rva, =flashcr_base		@ rva <- flash registers base address
	vcrfi	sv3, sv4, 3			@ sv3 <- buffer address from file descriptor
	add	sv3, sv3, #4			@ sv3 <- address of data in buffer
	set	sv5, #0				@ sv5 <- 0, start offset for read/write
wrtfl0:	@ write #F_PAGE_SIZE bytes to flash
	ldr	rvb, [sv3, sv5]			@ rvb <- word to write, from buffer
	str	rvb, [rva, #0x04]		@ write word to flash data buffer (FMD)
	add	rvb, sv2, sv5			@ rvb <- destination address in FLASH
	str	rvb, [rva, #0x00]		@ write destination address to flash register (FMA)
	ldr	rvb, =0xA4420001		@ rvb <- flash write key with write bit
	str	rvb, [rva, #0x08]		@ initiate write via FMC (Flash Control)	
wrtfl1:	ldr	rvb, [rva, #0x08]		@ rvb <- FLASH status
	tst	rvb, #0x01			@ is write bit still asserted?
	bne	wrtfl1				@	if so,  jump to keep waiting
	add	sv5, sv5, #4			@ sv5 <- offset of next word
	eq	sv5, #F_PAGE_SIZE		@ done?
	bne	wrtfl0				@	if not, jump to keep writing
	@ exit
	ldmfd	sp!, {rva, rvb, sv3, sv5}	@ restore scheme registers from stack
	swi	run_normal			@ enable interrupts (user mode)
	set	pc,  lnk			@ return

_func_	
ersfla:	@ erase flash sector that contains page address in sv2
_func_	
libers:	@ erase on-chip lib flash sector (lib shares on-chip file flash)
	swi	run_no_irq			@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb}			@ store scheme registers onto stack
	ldr	rva, =flashcr_base		@ rva <- flash registers base address
	str	sv2, [rva, #0x00]		@ set page to erase in FMA register (Flash address)
	ldr	rvb, =0xA4420002		@ rvb <- flash write key with erase bit
	str	rvb, [rva, #0x08]		@ start erasure via FMC (Flash Control)
ersfl0:	ldr	rvb, [rva, #0x08]		@ rvb <- FLASH status
	tst	rvb, #0x02			@ is erase bit still asserted?
	bne	ersfl0				@	if so,  jump to keep waiting
	ldmfd	sp!, {rva, rvb}			@ restore scheme registers from stack
	swi	run_normal			@ enable interrupts (user mode)
	set	pc,  lnk			@ return

.ifndef TI_EValBot

.balign	4
flashsectors:	@ 256 x 1KB FLASH sectors of LM3S1958, LM3S1968, LM3S6965
lib_sectors:	@ lib shares on-chip file flash
.word	0x00000000, 0x00000400, 0x00000800, 0x00000C00, 0x00001000, 0x00001400, 0x00001800, 0x00001C00
.word	0x00002000, 0x00002400, 0x00002800, 0x00002C00, 0x00003000, 0x00003400, 0x00003800, 0x00003C00
.word	0x00004000, 0x00004400, 0x00004800, 0x00004C00, 0x00005000, 0x00005400, 0x00005800, 0x00005C00
.word	0x00006000, 0x00006400, 0x00006800, 0x00006C00, 0x00007000, 0x00007400, 0x00007800, 0x00007C00
.word	0x00008000, 0x00008400, 0x00008800, 0x00008C00, 0x00009000, 0x00009400, 0x00009800, 0x00009C00
.word	0x0000a000, 0x0000a400, 0x0000a800, 0x0000aC00, 0x0000b000, 0x0000b400, 0x0000b800, 0x0000bC00
.word	0x0000c000, 0x0000c400, 0x0000c800, 0x0000cC00, 0x0000d000, 0x0000d400, 0x0000d800, 0x0000dC00
.word	0x0000e000, 0x0000e400, 0x0000e800, 0x0000eC00, 0x0000f000, 0x0000f400, 0x0000f800, 0x0000fC00
.word	0x00010000, 0x00010400, 0x00010800, 0x00010C00, 0x00011000, 0x00011400, 0x00011800, 0x00011C00
.word	0x00012000, 0x00012400, 0x00012800, 0x00012C00, 0x00013000, 0x00013400, 0x00013800, 0x00013C00
.word	0x00014000, 0x00014400, 0x00014800, 0x00014C00, 0x00015000, 0x00015400, 0x00015800, 0x00015C00
.word	0x00016000, 0x00016400, 0x00016800, 0x00016C00, 0x00017000, 0x00017400, 0x00017800, 0x00017C00
.word	0x00018000, 0x00018400, 0x00018800, 0x00018C00, 0x00019000, 0x00019400, 0x00019800, 0x00019C00
.word	0x0001a000, 0x0001a400, 0x0001a800, 0x0001aC00, 0x0001b000, 0x0001b400, 0x0001b800, 0x0001bC00
.word	0x0001c000, 0x0001c400, 0x0001c800, 0x0001cC00, 0x0001d000, 0x0001d400, 0x0001d800, 0x0001dC00
.word	0x0001e000, 0x0001e400, 0x0001e800, 0x0001eC00, 0x0001f000, 0x0001f400, 0x0001f800, 0x0001fC00
.word	0x00020000, 0x00020400, 0x00020800, 0x00020C00, 0x00021000, 0x00021400, 0x00021800, 0x00021C00
.word	0x00022000, 0x00022400, 0x00022800, 0x00022C00, 0x00023000, 0x00023400, 0x00023800, 0x00023C00
.word	0x00024000, 0x00024400, 0x00024800, 0x00024C00, 0x00025000, 0x00025400, 0x00025800, 0x00025C00
.word	0x00026000, 0x00026400, 0x00026800, 0x00026C00, 0x00027000, 0x00027400, 0x00027800, 0x00027C00
.word	0x00028000, 0x00028400, 0x00028800, 0x00028C00, 0x00029000, 0x00029400, 0x00029800, 0x00029C00
.word	0x0002a000, 0x0002a400, 0x0002a800, 0x0002aC00, 0x0002b000, 0x0002b400, 0x0002b800, 0x0002bC00
.word	0x0002c000, 0x0002c400, 0x0002c800, 0x0002cC00, 0x0002d000, 0x0002d400, 0x0002d800, 0x0002dC00
.word	0x0002e000, 0x0002e400, 0x0002e800, 0x0002eC00, 0x0002f000, 0x0002f400, 0x0002f800, 0x0002fC00
.word	0x00030000, 0x00030400, 0x00030800, 0x00030C00, 0x00031000, 0x00031400, 0x00031800, 0x00031C00
.word	0x00032000, 0x00032400, 0x00032800, 0x00032C00, 0x00033000, 0x00033400, 0x00033800, 0x00033C00
.word	0x00034000, 0x00034400, 0x00034800, 0x00034C00, 0x00035000, 0x00035400, 0x00035800, 0x00035C00
.word	0x00036000, 0x00036400, 0x00036800, 0x00036C00, 0x00037000, 0x00037400, 0x00037800, 0x00037C00
.word	0x00038000, 0x00038400, 0x00038800, 0x00038C00, 0x00039000, 0x00039400, 0x00039800, 0x00039C00
.word	0x0003a000, 0x0003a400, 0x0003a800, 0x0003aC00, 0x0003b000, 0x0003b400, 0x0003b800, 0x0003bC00
.word	0x0003c000, 0x0003c400, 0x0003c800, 0x0003cC00, 0x0003d000, 0x0003d400, 0x0003d800, 0x0003dC00
.word	0x0003e000, 0x0003e400, 0x0003e800, 0x0003eC00, 0x0003f000, 0x0003f400, 0x0003f800, 0x0003fC00
.word	0x00040000, 0x00050000

.else	@ TI_EvalBot

.balign	4
flashsectors:	@ 64 x 4 KB FLASH sectors of LM3S9B92 (best to use 4 KB sectors for erase, p.307)
lib_sectors:	@ lib shares on-chip file flash
.word	0x00000000, 0x00001000, 0x00002000, 0x00003000, 0x00004000, 0x00005000, 0x00006000, 0x00007000
.word	0x00008000, 0x00009000, 0x0000a000, 0x0000b000, 0x0000c000, 0x0000d000, 0x0000e000, 0x0000f000
.word	0x00010000, 0x00011000, 0x00012000, 0x00013000, 0x00014000, 0x00015000, 0x00016000, 0x00017000
.word	0x00018000, 0x00019000, 0x0001a000, 0x0001b000, 0x0001c000, 0x0001d000, 0x0001e000, 0x0001f000
.word	0x00020000, 0x00021000, 0x00022000, 0x00023000, 0x00024000, 0x00025000, 0x00026000, 0x00027000
.word	0x00028000, 0x00029000, 0x0002a000, 0x0002b000, 0x0002c000, 0x0002d000, 0x0002e000, 0x0002f000
.word	0x00030000, 0x00031000, 0x00032000, 0x00033000, 0x00034000, 0x00035000, 0x00036000, 0x00037000
.word	0x00038000, 0x00039000, 0x0003a000, 0x0003b000, 0x0003c000, 0x0003d000, 0x0003e000, 0x0003f000
.word	0x00040000, 0x00050000
	
.endif

	
@------------------------------------------------------------------------------------------------
@
@ 2- SD card low-level interface
@
@------------------------------------------------------------------------------------------------

.ifdef	onboard_SDFT

_func_	
sd_cfg:	@ configure spi speed (high), phase, polarity
	ldr	rva, =sd_spi
	set	rvb, #0
	str	rvb, [rva, #0x04]	@ SSI0CR1  <- disable SSI1
  .ifndef TI_EvalBot
	set	rvb, #0x0300
  .else
	set	rvb, #0x0500
  .endif
	orr	rvb, rvb, #0x07
	str	rvb, [rva]		@ SSI0CR0  <- PHA 0, POL 0, SPI mode, SCR 3
	set	rvb, #2
	str	rvb, [rva, #0x10]	@ SSI0CPSR <- set prescale to 2
	str	rvb, [rva, #0x04]	@ SSI0CR1  <- enable SSI1
	set	pc,  lnk

_func_	
sd_slo:	@ configure spi speed (low), phase, polarity
	ldr	rva, =sd_spi
	set	rvb, #0
	str	rvb, [rva, #0x04]	@ SSI0CR1  <- disable SSI1
  .ifndef TI_EvalBot
	set	rvb, #0x4000
  .else
	set	rvb, #0x6400
  .endif
	orr	rvb, rvb, #0x07
	str	rvb, [rva]		@ SSI0CR0  <- PHA 0, POL 0, SPI mode, SCR 3
	set	rvb, #2
	str	rvb, [rva, #0x10]	@ SSI0CPSR <- set prescale to 2
	str	rvb, [rva, #0x04]	@ SSI0CR1  <- enable SSI1
	set	pc,  lnk

_func_	
sd_sel:	@ select SD-card subroutine
	ldr	rva, =sd_cs_gpio
	set	rvb, #0
	str	rvb, [rva, #sd_cs]	@ clear-pin
	set	pc,  lnk
	
_func_	
sd_dsl:	@ de-select SD-card subroutine
	ldr	rva, =sd_cs_gpio
	set	rvb, #0xff
	str	rvb, [rva, #sd_cs]	@ set-pin
	set	pc,  lnk
	
_func_	
sd_get:	@ _sgb get sub-routine
	set	rvb, #0xff
_func_	
sd_put:	@ _sgb put sub-routine
	ldr	rva, =sd_spi
	ldr	rva, [rva, #spi_status]	@ ssta
	tst	rva, #spi_txrdy		@ sdtr
	beq	sd_put
	ldr	rva, =sd_spi
	and	rvb, rvb, #0xff
	str	rvb, [rva, #spi_thr]	@ sdtx (sdat)
sd_gpw:	@ wait
	ldr	rvb, [rva, #spi_status]	@ ssta
	tst	rvb, #spi_rxrdy		@ sdrr
	beq	sd_gpw
	ldr	rvb, [rva, #spi_rhr]	@ sdrx (sdat)
	set	pc, lnk

.endif	@ onboard_SDFT

.ltorg

	
@------------------------------------------------------------------------------------------------
@
@ 2- I2C Interrupt routine
@
@------------------------------------------------------------------------------------------------

_func_	
hwi2cr:	@ write-out additional address registers, if needed
_func_	
hwi2ni:	@ initiate i2c read/write, as master
_func_	
hwi2st:	@ get i2c interrupt status and base address
_func_	
i2c_hw_branch:	@ process interrupt
_func_	
hwi2we:	@ set busy status/stop bit at end of write as master
_func_	
hwi2re:	@ set stop bit if needed at end of read-as-master
_func_	
hwi2cs:	@ clear SI
_func_	
i2cstp:	@ prepare to end Read as Master transfer
_func_	
i2putp:	@ Prologue:	write additional address bytes to i2c, from buffer or r12 (prologue)
_func_	
i2pute:	@ Epilogue:	set completion status if needed (epilogue)
	set	pc,  lnk

