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
.ifndef	connectivity_ln
	.word	usbisr			@ 19:	USB_HP (if included)
	.word	usbisr			@ 20:	USB_LP (if included)
.else
	.word	i0			@ 19
	.word	i0			@ 20
.endif
	.word	i0			@ 21
	.word	i0			@ 22
	.word	i0			@ 23
	.word	ptmisr			@ 24:	timer0
	.word	i0			@ 25
	.word	i0			@ 26
	.word	i0			@ 27
	.word	ptmisr			@ 28:	timer1 (STM32 timer 2)
	.word	ptmisr			@ 29:	timer2 (STM32 timer 3)
	.word	i0			@ 30
	.word	pi2isr			@ 31:	i2c0 (if included)
	.word	i0			@ 32
	.word	pi2isr			@ 33:	i2c1 (if included)
	.word	i0			@ 34
	.word	i0			@ 35
	.word	i0			@ 36
	.word	puaisr			@ 37:	uart0 (STM32 USART 1) (unless swapped, then uart1)
	.word	puaisr			@ 38:	uart1 (STM32 USART 2) (unless swapped, then uart0)
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
	.word	i0			@ 56
	.word	i0			@ 57
	.word	i0			@ 58
	.word	i0			@ 59
	.word	i0			@ 60
	.word	i0			@ 61
	.word	i0			@ 62
	.word	i0			@ 63
.ifdef	connectivity_ln
	.word	i0			@ 64
	.word	i0			@ 65
	.word	i0			@ 66
	.word	usbisr			@ 67:	USB OTG FS  (if included)
.endif
	

_func_	
hwinit:
	@ pre-set common values
	set	r0,  #0
	set	r1,  #1
	set	r2,  #2
	set	r3,  #3
	set	r4,  #4
	set	r5,  #5
	@ initialization of clocks
	ldr	r10, =rcc_base
	ldr	r7,  [r10]
	orr	r7,  r7, #0x010000
	str	r7,  [r10]		@ RCC_CR    <- set HSEON
hwiwt0:	ldr	r7,  [r10]
	tst	r7,  #0x020000		@ RCC_CR    <- wait for HSERdy bit
	beq	hwiwt0
	ldr	r6,  =flashcr_base
	set	r7,  #0x32
	str	r7,  [r6]		@ FLASH_ACR <- enable buffer, 2 wait states as SYSCLK will be 72 MHz
.ifdef	connectivity_ln
	ldr	r7,  =Clock_parms2
	str	r7,  [r10, #0x2c]	@ RCC_CFGR2 <- PLL2 x1/5->PLL src, PLL3 x10/5->50MHz, PLL2 x8/5->40MHz
	ldr	r7,  [r10]
	orr	r7,  r7, #0x04000000
	str	r7,  [r10]		@ RCC_CR    <- turn PLL2 on
hwiwt2:	ldr	r7,  [r10]
	tst	r7,  #0x08000000	@ RCC_CR    <- wait for PLL2 to be locked
	beq	hwiwt2
	orr	r7,  r7, #0x10000000
	str	r7,  [r10]		@ RCC_CR    <- turn PLL3 on
hwiwt3:	ldr	r7,  [r10]
	tst	r7,  #0x20000000	@ RCC_CR    <- wait for PLL3 to be locked
	beq	hwiwt3
.endif
	ldr	r7,  =Clock_parms
	str	r7,  [r10, #0x04]	@ RCC_CFGR  <- USB->48MHz, PLL x9->72MHz->AHB,36MHz->ADC->APB1->APB2
	ldr	r7,  [r10]
	orr	r7,  r7, #0x01000000
	str	r7,  [r10]		@ RCC_CR    <- turn PLL on
hwiwt1:	ldr	r7,  [r10, #0x04]
	tst	r7,  #0x08		@ RCC_CFGR  <- wait for PLL to be connected
	beq	hwiwt1
	@ initialization of FLASH
	ldr	r7,  =0x45670123
	str	r7,  [r6,  #0x04]	@ FLASH_KEYR <- KEY1, start to unlock flash registers
	ldr	r7,  =0xcdef89ab
	str	r7,  [r6,  #0x04]	@ FLASH_KEYR <- KEY2, finish unlocking of flash registers
	@ initialize Cortex-M3 SysTick Timer
	ldr	r6,  =systick_base
	ldr	r7,  =719999
	str	r7,  [r6, #tick_load]	@ SYSTICK-RELOAD  <- value for 10ms timing at 72MHz
	str	r0,  [r6, #tick_val]	@ SYSTICK-VALUE   <- 0
	str	r5,  [r6, #tick_ctrl]	@ SYSTICK-CONTROL <- 5 = enabled, no interrupt, run from cpu clock
	@ initialization of LED gpio pins
.ifdef STM32_H103
	set	r7,  #0x10
	str	r7,  [r10, #24]		@ RCC_APB2ENR <- enable clock for I/O Port C
	ldr	r6,  =ioportc_base
	ldr	r7,  [r6,  #0x04]
	bic	r7,  r7, #0x0F0000
	orr	r7,  r7, #0x070000
	str	r7,  [r6,  #0x04]	@ GPIOC_CRH   <- PC12 (LED) configured as open drain output
.endif
.ifdef STM32_H107
	set	r7,  #0x1d
	str	r7,  [r10, #24]		@ RCC_APB2ENR <- enable clock for I/O Ports A, B and C, and AFIO
	ldr	r6,  =ioportc_base
	ldr	r7,  [r6,  #0x00]
	bic	r7,  r7, #(0xFF << 24)
	orr	r7,  r7, #(0x33 << 24)
	str	r7,  [r6,  #0x00]	@ GPIOC_CRL   <- PC6 and PC7 (LED) configured as push-pull output
.endif
.ifdef STM32_DT_Board
	set	r7,  #0x14
	str	r7,  [r10, #24]		@ RCC_APB2ENR <- enable clock for I/O Ports A and C
	ldr	r6,  =ioporta_base
	ldr	r7,  [r6,  #0x04]
	bic	r7,  r7, #0x0F
	orr	r7,  r7, #0x07
	str	r7,  [r6,  #0x04]	@ GPIOA_CRH   <- PA8 (LED) configured as open drain output
.endif
.ifdef STM32_LCD
	set	r7,  #0x2c
	str	r7,  [r10, #24]		@ RCC_APB2ENR <- enable clock for I/O Ports A, B and D
	ldr	r6,  =ioportb_base
	ldr	r7,  [r6,  #0x00]
	bic	r7,  r7, #0x0F00
	orr	r7,  r7, #0x0700
	str	r7,  [r6,  #0x00]	@ GPIOB_CRL   <- PB2 (for LED -- no LED on board) open drain output
.endif
	@ initialization of USART1 for 9600 8N1 operation
.ifndef connectivity_ln
	ldr	r7,  [r10, #24]
	ldr	r8,  =0x4004
	orr	r7,  r7, r8
	str	r7,  [r10, #24]		@ RCC_APB2ENR <- enable clock for USART1 and I/O Port A
	ldr	r6,  =ioporta_base
	ldr	r7,  [r6,  #0x04]
	bic	r7,  r7, #0x00F0
	orr	r7,  r7, #0x00B0
	str	r7,  [r6,  #0x04]	@ GPIOA_CRH   <- PA9 (USART1 Tx) configured as AF out, push-pull
.else

  .ifndef swap_default_usart
	
	ldr	r6,  =afio_base
	ldr	r7,  [r6, #0x04]
	orr	r7,  r7, #0x06		@ USART1 -> PB, I2C1 -> PB
	orr	r7,  r7, #(1 << 12)	@ TIM4 -> PD
	orr	r7,  r7, #0xc0		@ TIM1 -> PE
	orr	r7,  r7, #(6 << 12)	@ CAN1 -> PD
	str	r7,  [r6, #0x04]	@ AFIO_MAPR <- remap, uasrt1 Tx-Rx=PB6-7, tim4=PD12-15, I2C1=PB8-9
	ldr	r7,  [r10, #24]
	orr	r7,  r7, #0x4000
	str	r7,  [r10, #24]		@ RCC_APB2ENR <- enable clock for USART1
	ldr	r6,  =ioportb_base
	ldr	r7,  [r6,  #0x00]
	bic	r7,  r7, #(0xff << 24)
	orr	r7,  r7, #(0x8b << 24)
	str	r7,  [r6,  #0x00]	@ GPIOB_CRL   <- PB6/PB7 (USART1 Tx/Rx) cfg AF out, push-pull & input

  .else
	
	ldr	r7,  [r10, #0x1c]
	orr	r7,  r7, #0x20000
	str	r7,  [r10, #0x1c]	@ RCC_APB1ENR <- enable clock for USART2 (Armpit UAR0 for this MCU)
	ldr	r6,  =ioporta_base
	ldr	r7,  [r6,  #0x00]
	bic	r7,  r7, #0xFF00
	orr	r7,  r7, #0x8B00
	str	r7,  [r6,  #0x00]	@ GPIOA_CRL   <- PA2/PA3 (USART2 Tx/Rx) cfg AF out, push-pull & input

  .endif
	
.endif
	ldr	r6,  =uart0_base
	ldr	r7,  =UART0_DIV
	str	r7,  [r6,  #0x08]	@ USART_BRR   <- 9600 bauds
	ldr	r7,  =0x202c
	str	r7,  [r6,  #0x0c]	@ USART_CR1   <- USART, Tx and Rx enabled at 8N1, with Rx interrupt
	@ initialization of mcu-id for variables (normally I2c address if slave enabled)
	set	r7,  #0x200000
	str	r7,  [r10, #28]
	ldr	r6,  =I2C0ADR
	set	r7,  #mcu_id
	str	r7,  [r6]
	@ initialization of APB1 and APB2 Peripheral Clock Power
	ldr	r7,  [r10, #24]
	ldr	r8,  =0x00005E7D
	orr	r7,  r7, r8
	str	r7,  [r10, #24]		@ RCC_APB2ENR <- enable clk for AFIO,Port ABCDE,ADC1,2,TIM1,SPI1
	ldr	r7,  [r10, #28]
	ldr	r8,  =0x00624007	@ TIM2,3,4, SPI2, USART2, I2C1,2
	orr	r7,  r7, r8
	str	r7,  [r10, #28]		@ RCC_APB1ENR <- enable clock for TIM2,3,4, SPI2, UART2,3, I2C1,2
	
	@ initialization of SD card pins

.ifdef	onboard_SDFT
	
  .ifdef sd_is_on_spi

	@ configure pins and SPI peripheral
	@ SPIn_CR1  <- disable SPIn (clear faults if any)
	ldr	r6, =sd_spi
	ldr	r7, [r6, #0x08]		@ SPIn_SR, read to clear fault flags if any
	set	r7, #0x00
	str	r7, [r6, #0x00]		@ SPIn_CR1  <- disable SPIn (clear faults if any)
	@ configure CS pin as gpio out
 	ldr	r6, =sd_cs_gpio
	ldr	r7, [r6, #0x04]		@ r7 <- config of pins 8-15 (change offset to 0 for pins 0-7)
	bic	r7, r7, #0x0f
	orr	r7, r7, #0x01		@ r7 <- bits for PA.8 config as GPIO out, push-pull, 10 MHz
	str	r7, [r6, #0x04]
	@ de-select sd
	set	r7, #sd_cs
	str	r7, [r6, #0x14]	@ clear CS pin
	@ de-select inbound SS pin
 	ldr	r6, =sd_spi_gpio
	ldr	r7, [r6, #0x00]		@ r7 <- config of pins 0-7
	bic	r7, r7, #0x0f0000
	orr	r7, r7, #0x080000	@ r7 <- bits for PA.4 (SPI SS) config as input
	str	r7, [r6, #0x00]
	set	r7, #(1 << 4)
	str	r7, [r6, #0x10]		@ set SS pin (PA.4)
	@ configure sck, miso and mosi pins as spi (AF push-pull out in GPIOn_CRL/CRH)
	ldr	r7, [r6, #0x00]		@ r7 <- config of pins 0-7
	bic	r7, r7, #0xff000000
	bic	r7, r7, #0x00f00000
	orr	r7, r7, #0xBB000000
	orr	r7, r7, #0x00B00000	@ r7 <- bits for PA.5,6,7 config as AFIO (SPI)
	str	r7, [r6, #0x00]
	@ low speed (approx 400KHz)
	ldr	r6, =sd_spi
	ldr	r7, [r6, #0x08]		@ SPIn_SR, read to clear fault flags if any
	set	r7, #0x00
	str	r7, [r6, #0x00]		@ SPIn_CR1  <- disable SPIn (clear faults if any)
	set	r7, #0x74
	str	r7, [r6, #0x00]		@ SPIn_CR1 <- PHA 0, POL 0, 8bit, Mstr, Enab, 280 KHz

  .endif @ sd_is_on_spi
	
  .ifdef sd_is_on_mci

	@ power/clock the SDIO peripheral
	ldr	r7, [r10, #0x14]
	orr	r7, r7, #0x0400
	str	r7, [r10, #0x14]	@ RCC_AHBENR <- power-up sdio
	@ configure SDIO pins: PD2, PC8-12
	ldr	r6,  =ioportd_base
	ldr	r7, [r6]
	bic	r7, r7, #0x0f00
	orr	r7, r7, #0x0b00
	str	r7, [r6]		@ IOPORTD CRL <- set PD2 to SDIO function (AFSEL, Push-Pull)
	ldr	r6,  =ioportc_base
	ldr	r7, [r6, #4]
	lsr	r7, r7, #20
	lsl	r7, r7, #20
	ldr	r8, =0x0bbbbb
	orr	r7, r7, r8
	str	r7, [r6, #4]		@ IOPORTC CRH <- set PC8-12 to SDIO function (AFSEL, Push-Pull)
	@ power-up and power-on mci peripheral function
	ldr	r6, =sd_mci
	str	r2, [r6]		@ set MCI to power-up phase
	set	r7, #0x0b2
	orr	r7, r7, #0x4100
	str	r7, [r6, #0x04]	@ enable 400KHz MCI CLK, narrow bus
mcipw0:	str	r3, [r6]		@ set MCI to power-on phase
	ldr	r7, [r6]
	eq	r7, #3
	bne	mcipw0

  .endif @ sd_is_on_mci
	
.endif	@ onboard_SDFT

	@ initialization of USB configuration
	ldr	r6,  =USB_CONF
	str	r0,  [r6]		@ USB_CONF <- USB device is not yet configured
	
.ifdef	native_usb

  .ifdef STM32_H103
	@ check if USB is powered (PC4 USB-P power pin), otherwise, return
	ldr	r6,  =ioportc_base
	ldr	r7,  [r6,  #0x08]
	tst	r7, #(1 << 4)
	it	eq
	seteq	pc,  lnk
  .endif
  .ifdef STM32_H107
	@ check if USB is powered (PA9 OTG_VBUS power pin), otherwise, return
	ldr	r6,  =ioporta_base
	ldr	r7,  [r6,  #0x08]
	tst	r7, #(1 << 9)
	it	eq
	seteq	pc,  lnk
  .endif
  .ifdef STM32_LCD
	@ check if USB is powered (PA0 USB-P power pin), otherwise, return
	ldr	r6,  =ioporta_base
	ldr	r7,  [r6,  #0x08]
	tst	r7, #0x01
	it	eq
	seteq	pc,  lnk
  .endif
	@ enable USB clock
  .ifndef connectivity_ln
	ldr	r7,  [r10, #0x1c]
	orr	r7,  r7, #0x00800000
	str	r7,  [r10, #0x1c]	@ RCC_APB1ENR <- enable clock for USB
  .else
	ldr	r7,  [r10, #0x14]
	orr	r7,  r7, #0x1000
	str	r7,  [r10, #0x14]	@ RCC_AHBENR  <- enable USB OTG FS clock (for 72 MHz main clock)
  .endif
	@ initialization of USB device controller -- **** MUST BE LAST ****
	ldr	r6,  =USB_LineCoding
	ldr	r7,  =115200
	str	r7,  [r6]		@ 115200 bauds
	set	r7,  #0x00080000
	str	r7,  [r6,  #0x04]	@ 8 data bits, no parity, 1 stop bit
	ldr	r6,  =USB_CHUNK
	str	r0,  [r6]		@ zero bytes remaining to send at startup
	ldr	r6,  =USB_ZERO
	str	r0,  [r6]		@ alternate interface and device/interface status = 0
	ldr	r6,  =USB_CONF
	str	r0,  [r6]		@ USB device is not yet configured
  .ifndef connectivity_ln

.ifdef	debug_usb
	
	@ DEBUG
	ldr	r6, =RAMTOP
	add	r7, r6, #4
	str	r7, [r6]
	add	r6, r6, #4
	set	r7, #0
dbgini:	str	r7, [r6]
	add	r6, r6, #4
	tst	r6, #0x1000		@ for STM32_H103 (16KB to 20KB into RAM)
	beq	dbgini

.endif
	
	set	r5,  #0xA0000000
	ldr	r6,  =usb_base
	str	r1,  [r6,  #0x40]	@ USB_CNTR   -> exit power down mode
	@ need to make sure there's enough time between exitng pwr mode (above) and exitng reset mode (below)
	@ if needed, block below could probably be moved to after buffer alloc table initialization
	@ or, branch-link to a wait loop
	set	r7,  #0x80		@ 00
hwiwt3:	subs	r7,  r7, #1
	bne	hwiwt3
	str	r0,  [r6,  #0x40]	@ USB_CNTR   -> exit reset mode
	str	r0,  [r6,  #0x40]	@ USB_CNTR   -> exit reset mode (again, to make sure)
	str	r0,  [r6,  #0x44]	@ USB_ISTR   -> clear potential spurious pending interrupts
	set	r7,  #0x9C00
	str	r7,  [r6,  #0x40]	@ USB_CNTR   -> generate interrupts on ctr, wakeup, suspend, reset
	@ end of said 'moveable?' block
	str	r0,  [r6,  #0x50]	@ USB_BTABLE    -> buffer allocation table starts at offset 0
	add	r9,  r6, #0x0400
	set	r7,  #0x80
	str	r7,  [r9]		@ USB_ADR0_TX   -> EP0 send buffer start offset = 0x0100 (2 x 0x80)
	str	r0,  [r9,  #0x04]	@ USB_COUNT0_TX -> 0 bytes to transmit
	set	r7,  #0x88
	str	r7,  [r9,  #0x08]	@ USB_ADR0_RX   -> EP0 receive buffer start offset = 0x0110 (2 x 0x88)
	set	r7,  #0x1000
	str	r7,  [r9,  #0x0c]	@ USB_COUNT0_RX -> blk sz = 2bytes, buf sz = 8bytes, 0 bytes received
	set	r7,  #0x90
	str	r7,  [r9,  #0x10]	@ USB_ADR1_TX   -> EP1 send buffer start offset = 0x0110
	str	r0,  [r9,  #0x14]	@ USB_COUNT1_TX -> 0 bytes to transmit
	set	r7,  #0x98
	str	r7,  [r9,  #0x18]	@ USB_ADR1_RX   -> EP1 receive buffer start offset = 0x0118
	set	r7,  #0x1000
	str	r7,  [r9,  #0x1c]	@ USB_COUNT1_RX -> blk sz = 2bytes, buf sz = 8bytes, 0 bytes received
	set	r7,  #0xa0
	str	r7,  [r9,  #0x20]	@ USB_ADR2_TX   -> EP2 send buffer start offset = 0x0120
	str	r0,  [r9,  #0x24]	@ USB_COUNT2_TX -> 0 bytes to transmit
	set	r7,  #0xe0
	str	r7,  [r9,  #0x28]	@ USB_ADR2_RX   -> EP2 receive buffer start offset = 0x0160
	set	r7,  #0x8400
	str	r7,  [r9,  #0x2c]	@ USB_COUNT2_RX -> blk sz = 32bytes, buf sz = 64bytes, 0 bytes rcvd
	ldr	r7,  =0x01a0
	str	r7,  [r9,  #0x30]	@ USB_ADR3_TX   -> EP3 send buffer start offset = 0x0120
	str	r0,  [r9,  #0x34]	@ USB_COUNT3_TX -> 0 bytes to transmit
	ldr	r7,  =0x01e0
	str	r7,  [r9,  #0x38]	@ USB_ADR3_RX   -> EP3 receive buffer start offset = 0x0160
	set	r7,  #0x8400
	str	r7,  [r9,  #0x3c]	@ USB_COUNT3_RX -> blk sz = 32bytes, buf sz = 64bytes, 0 bytes rcvd
	@ if needed, block below could probably be moved to after buffer alloc table initialization
	@ or, branch-link to a wait loop
	ldr	r7,  =0x3230
	str	r7,  [r6]		@ USB_EP0R      -> configure enpoint 0 as control EP
	set	r7,  #0x80
	str	r7,  [r6,  #0x4c]	@ USB_DADDR	-> enable USB, address is 0
	
  .else	@ connectivity line USB OTG FS
	@
	@ Note:	This interface is not operational in this version
	@	Code below is under construction.
	@

.ifdef	debug_usb
	
	@ DEBUG
	ldr	r6, =RAMTOP
	add	r7, r6, #4
	str	r7, [r6]
	add	r6, r6, #4
	set	r7, #0
dbgini:	str	r7, [r6]
	add	r6, r6, #4
	tst	r6, #0x10000
	beq	dbgini

.endif
	
	ldr	r6,  =ioporta_base
	ldr	r7, [r6, #4]
	bic	r7, r7, #0x0ff000
	orr	r7, r7, #0x0bb000
	str	r7, [r6, #4]
	
	ldr	r6,  =usb_base
@	ldr	r7,  =0x40002480
	ldr	r7,  =0x40002487
	str	r7,  [r6, #0x0c]	@ OTG_FS_GUSBCFG <- force device mode, trdt=9 (72MHz)
	set	r7,  #0x81
	str	r7,  [r6, #0x08]	@ OTG_FS_GAHBCFG <- unmask global USB_OTG interrupts, Txlevel = 0
@	ldr	r7,  =0x0C3800
@	ldr	r7,  =0x0C3830
@	ldr	r7,  =0x0C3810
	ldr	r7,  =0x0C1810
@	ldr	r7,  =0x0C1800		@ <-$
	str	r7,  [r6, #0x18]	@ OTG_FS_GINTMSK <- unmask OUT, IN, ENUMDN, RESET, SUSPEND interrupts
	add	r6,  r6, #0x0800

	ldr	r7,  [r6]
	orr	r7,  r7,  r3
	str	r7,  [r6]		@ OTG_FS_DCFG    <- device is full speed, address 0
@	str	r3,  [r6]		@ OTG_FS_DCFG    <- device is full speed, address 0
	
	sub	r6,  r6, #0x0800
	set	r7,  #0x090000
	str	r7,  [r6, #0x38]	@ OTG_FS_GCCFG   <- power up transceiver, VBus sensing (device mode)
	
  .endif @ for ifndef connectivity_ln
	
  .ifdef STM32_H103
	@ signify to USB host that a device is attached on USB bus (set PC11, DISC, low)
	ldr	r6,  =ioportc_base
	ldr	r7,  [r6,  #0x04]
	bic	r7,  r7, #0x00F000
	orr	r7,  r7, #0x007000
	str	r7,  [r6,  #0x04]	@ GPIOC_CRH   <- PC11 (DISC) configured as GP out, open drain
	set	r7,  #0x0800		@ r7 <- pin 11
	str	r7,  [r6,  #io_clear]
  .endif
  .ifdef STM32_LCD
	@ signify to USB host that a device is attached on USB bus (set PD3, DISC, low)
	ldr	r6,  =ioportd_base
	ldr	r7,  [r6,  #0x00]
	bic	r7,  r7, #0x00F000
	orr	r7,  r7, #0x007000
	str	r7,  [r6,  #0x00]	@ GPIOA_CRL   <- PD3 (DISC) configured as GP out, open drain
	set	r7,  #0x08		@ r7 <- pin 3
	str	r7,  [r6,  #io_clear]
  .endif
	
.endif	@ native_usb

	@ enf of the hardware initialization
	set	pc,  lnk


@------------------------------------------------------------------------------------------------
@ STM32x
@
@	 1- Initialization from FLASH, writing to and erasing FLASH
@	 2- I2C Interrupt routine
@
@------------------------------------------------------------------------------------------------
	
@
@ 1- Initialization from FLASH, writing to and erasing FLASH
@

.ifdef STM32_H103
_func_	
FlashInitCheck: @ return status of flash init enable/override gpio pin (PA.0) in rva
	ldr	rva, =ioporta_base		@ rva <- GPIO port A where PA0 is located
	ldr	rva, [rva, #0x08]		@ rva <- status of input pins on Port A
	and	rva, rva, #0x01			@ rva <- status of PA0 only (non-zero if PA0 is high)
	set	pc,  lnk			@ return
.endif

.ifdef STM32_H107
_func_	
FlashInitCheck: @ return status of flash init enable/override gpio pin (WKUP button, PA.0) in rva
	ldr	rva, =ioporta_base		@ rva <- GPIO port A where PA0 is located
	ldr	rva, [rva, #0x08]		@ rva <- status of input pins on Port A
	and	rva, rva, #0x01			@ rva <- status of PA0 only (non-zero if PA0 is high)
	set	pc,  lnk			@ return
.endif

.ifdef STM32_DT_Board
_func_	
FlashInitCheck: @ return status of flash init enable/override gpio pin (SW1, PC.9) in rva
	ldr	rva, =ioportc_base		@ rva <- GPIO port C where PC9 is located
	ldr	rva, [rva, #0x08]		@ rva <- status of input pins on Port C
	and	rva, rva, #0x0200		@ rva <- status of PC9 only (non-zero if PC9 is high)
	set	pc,  lnk			@ return
.endif

.ifdef STM32_LCD
_func_	
FlashInitCheck: @ return status of flash init enable/override gpio pin (I2C1_SDA1, PB.7) in rva
	ldr	rva, =ioportb_base		@ rva <- GPIO port B where PB7 is located
	ldr	rva, [rva, #0x08]		@ rva <- status of input pins on Port B
	and	rva, rva, #0x0080		@ rva <- status of PB7 only (non-zero if PB7 is high)
	set	pc,  lnk			@ return
.endif


_func_	
wrtfla:	@ write to flash, sv2 = page address, sv4 = file descriptor
_func_	
libwrt:	@ write to on-chip lib flash (lib shares on-chip file flash)
	
	set	rvc, #F_PAGE_SIZE
_func_
wrtfle:	@ [internal entry] (for wrtflr = file pseudo-erase)

	stmfd	sp!, {sv3}			@ store scheme registers onto stack
	ldr	rva, =0x40022000		@ rva <- flash registers base address
	set	rvb, #0x01			@ rvb <- bit 0, PG (program)
	str	rvb, [rva, #0x10]		@ set program command in FLASH_CR
	vcrfi	sv3, sv4, 3			@ sv3 <- buffer address from file descriptor

	@ check for file pseudo-erasure
	eq	rvc, #0
@	eq	rvc, #i0
@	itT	eq
	it	eq
	strheq	rvc, [sv2]			@ write half-word to flash
@	seteq	rvc, #0
	beq	wrtfl1

@	set	rvc, #F_PAGE_SIZE
wrtfl0:	@ write #F_PAGE_SIZE bytes to flash
	add	rvc, rvc, #2
	ldrh	rvb, [sv3, rvc]			@ rvb <- half-word to write, from buffer
	sub	rvc, rvc, #4
	strh	rvb, [sv2, rvc]			@ write half-word to flash
wrtfl1:	@ wait for flash ready
	ldr	rvb, [rva, #0x0c]		@ rvb <- FLASH_SR
	tst	rvb, #0x01			@ is BSY still asserted?
	bne	wrtfl1				@	if so,  jump to keep waiting
	eq	rvc, #0				@ done?
	bne	wrtfl0				@	if not, jump to keep writing
	@ exit
	set	rvb, #0x00			@ rvb <- 0
	str	rvb, [rva, #0x10]		@ clear contents of FLASH_CR
	ldmfd	sp!, {sv3}			@ restore scheme registers from stack
	set	pc,  lnk			@ return

_func_	
wrtflr:	@ pseudo-erase a file flash page, sv2 = page address, sv4 = file descriptor
	@ Note:	overwriting a flash cell with anything other than 0x0000 can produce
	@	errors on this MCU. For this reason, #0 is used here (rather than #i0).
	set	rvc, #0
@	set	rvc, #i0
	b	wrtfle

_func_	
ersfla:	@ erase flash sector that contains page address in sv2
_func_	
libers:	@ erase on-chip lib flash sector (lib shares on-chip file flash)
	ldr	rva, =0x40022000		@ rva <- flash registers base address
	set	rvb, #0x02			@ rvb <- bit 1, PER (page erase)
	str	rvb, [rva, #0x10]		@ set page erase command in FLASH_CR
	str	sv2, [rva, #0x14]		@ set page to erase in FLASH_AR
	set	rvb, #0x42			@ rvb <- bits 6 and 1, STRT (start) and PER (page erase)
	str	rvb, [rva, #0x10]		@ start erase page via FLASH_CR (stalls cpu if run from flash)
ersfl0:	ldr	rvb, [rva, #0x0c]		@ rvb <- FLASH_SR
	tst	rvb, #0x01			@ is BSY still asserted?
	bne	ersfl0				@	if so,  jump to keep waiting
	set	rvb, #0x00			@ rvb <- 0
	str	rvb, [rva, #0x10]		@ clear contents of FLASH_CR
	set	pc,  lnk			@ return

	
.ifdef STM32_H103
.balign	4
flashsectors:	@ 128 x 1KB FLASH sectors of STM32F103RBT6
lib_sectors:	@ lib shares on-chip file flash
.word	0x08000000, 0x08000400, 0x08000800, 0x08000C00, 0x08001000, 0x08001400, 0x08001800, 0x08001C00
.word	0x08002000, 0x08002400, 0x08002800, 0x08002C00, 0x08003000, 0x08003400, 0x08003800, 0x08003C00
.word	0x08004000, 0x08004400, 0x08004800, 0x08004C00, 0x08005000, 0x08005400, 0x08005800, 0x08005C00
.word	0x08006000, 0x08006400, 0x08006800, 0x08006C00, 0x08007000, 0x08007400, 0x08007800, 0x08007C00
.word	0x08008000, 0x08008400, 0x08008800, 0x08008C00, 0x08009000, 0x08009400, 0x08009800, 0x08009C00
.word	0x0800a000, 0x0800a400, 0x0800a800, 0x0800aC00, 0x0800b000, 0x0800b400, 0x0800b800, 0x0800bC00
.word	0x0800c000, 0x0800c400, 0x0800c800, 0x0800cC00, 0x0800d000, 0x0800d400, 0x0800d800, 0x0800dC00
.word	0x0800e000, 0x0800e400, 0x0800e800, 0x0800eC00, 0x0800f000, 0x0800f400, 0x0800f800, 0x0800fC00
.word	0x08010000, 0x08010400, 0x08010800, 0x08010C00, 0x08011000, 0x08011400, 0x08011800, 0x08011C00
.word	0x08012000, 0x08012400, 0x08012800, 0x08012C00, 0x08013000, 0x08013400, 0x08013800, 0x08013C00
.word	0x08014000, 0x08014400, 0x08014800, 0x08014C00, 0x08015000, 0x08015400, 0x08015800, 0x08015C00
.word	0x08016000, 0x08016400, 0x08016800, 0x08016C00, 0x08017000, 0x08017400, 0x08017800, 0x08017C00
.word	0x08018000, 0x08018400, 0x08018800, 0x08018C00, 0x08019000, 0x08019400, 0x08019800, 0x08019C00
.word	0x0801a000, 0x0801a400, 0x0801a800, 0x0801aC00, 0x0801b000, 0x0801b400, 0x0801b800, 0x0801bC00
.word	0x0801c000, 0x0801c400, 0x0801c800, 0x0801cC00, 0x0801d000, 0x0801d400, 0x0801d800, 0x0801dC00
.word	0x0801e000, 0x0801e400, 0x0801e800, 0x0801eC00, 0x0801f000, 0x0801f400, 0x0801f800, 0x0801fC00
.word	0x08020000, 0x08800000
.endif
	
.ifdef STM32_H107
.balign	4
flashsectors:	@ 128 x 2KB FLASH sectors of STM32F107VCT6
lib_sectors:	@ lib shares on-chip file flash
.word	0x08000000, 0x08000800, 0x08001000, 0x08001800, 0x08002000, 0x08002800, 0x08003000, 0x08003800
.word	0x08004000, 0x08004800, 0x08005000, 0x08005800, 0x08006000, 0x08006800, 0x08007000, 0x08007800
.word	0x08008000, 0x08008800, 0x08009000, 0x08009800, 0x0800a000, 0x0800a800, 0x0800b000, 0x0800b800
.word	0x0800c000, 0x0800c800, 0x0800d000, 0x0800d800, 0x0800e000, 0x0800e800, 0x0800f000, 0x0800f800
.word	0x08010000, 0x08010800, 0x08011000, 0x08011800, 0x08012000, 0x08012800, 0x08013000, 0x08013800
.word	0x08014000, 0x08014800, 0x08015000, 0x08015800, 0x08016000, 0x08016800, 0x08017000, 0x08017800
.word	0x08018000, 0x08018800, 0x08019000, 0x08019800, 0x0801a000, 0x0801a800, 0x0801b000, 0x0801b800
.word	0x0801c000, 0x0801c800, 0x0801d000, 0x0801d800, 0x0801e000, 0x0801e800, 0x0801f000, 0x0801f800
.word	0x08020000, 0x08020800, 0x08021000, 0x08021800, 0x08022000, 0x08022800, 0x08023000, 0x08023800
.word	0x08024000, 0x08024800, 0x08025000, 0x08025800, 0x08026000, 0x08026800, 0x08027000, 0x08027800
.word	0x08028000, 0x08028800, 0x08029000, 0x08029800, 0x0802a000, 0x0802a800, 0x0802b000, 0x0802b800
.word	0x0802c000, 0x0802c800, 0x0802d000, 0x0802d800, 0x0802e000, 0x0802e800, 0x0802f000, 0x0802f800
.word	0x08030000, 0x08030800, 0x08031000, 0x08031800, 0x08032000, 0x08032800, 0x08033000, 0x08033800
.word	0x08034000, 0x08034800, 0x08035000, 0x08035800, 0x08036000, 0x08036800, 0x08037000, 0x08037800
.word	0x08038000, 0x08038800, 0x08039000, 0x08039800, 0x0803a000, 0x0803a800, 0x0803b000, 0x0803b800
.word	0x0803c000, 0x0803c800, 0x0803d000, 0x0803d800, 0x0803e000, 0x0803e800, 0x0803f000, 0x0803f800
.word	0x08040000, 0x08800000
.endif

.ifdef STM32_DT_Board
.balign	4
flashsectors:	@ 256 x 2KB FLASH sectors of STM32F103ZET6
lib_sectors:	@ lib shares on-chip file flash
.word	0x08000000, 0x08000800, 0x08001000, 0x08001800, 0x08002000, 0x08002800, 0x08003000, 0x08003800
.word	0x08004000, 0x08004800, 0x08005000, 0x08005800, 0x08006000, 0x08006800, 0x08007000, 0x08007800
.word	0x08008000, 0x08008800, 0x08009000, 0x08009800, 0x0800a000, 0x0800a800, 0x0800b000, 0x0800b800
.word	0x0800c000, 0x0800c800, 0x0800d000, 0x0800d800, 0x0800e000, 0x0800e800, 0x0800f000, 0x0800f800
.word	0x08010000, 0x08010800, 0x08011000, 0x08011800, 0x08012000, 0x08012800, 0x08013000, 0x08013800
.word	0x08014000, 0x08014800, 0x08015000, 0x08015800, 0x08016000, 0x08016800, 0x08017000, 0x08017800
.word	0x08018000, 0x08018800, 0x08019000, 0x08019800, 0x0801a000, 0x0801a800, 0x0801b000, 0x0801b800
.word	0x0801c000, 0x0801c800, 0x0801d000, 0x0801d800, 0x0801e000, 0x0801e800, 0x0801f000, 0x0801f800
.word	0x08020000, 0x08020800, 0x08021000, 0x08021800, 0x08022000, 0x08022800, 0x08023000, 0x08023800
.word	0x08024000, 0x08024800, 0x08025000, 0x08025800, 0x08026000, 0x08026800, 0x08027000, 0x08027800
.word	0x08028000, 0x08028800, 0x08029000, 0x08029800, 0x0802a000, 0x0802a800, 0x0802b000, 0x0802b800
.word	0x0802c000, 0x0802c800, 0x0802d000, 0x0802d800, 0x0802e000, 0x0802e800, 0x0802f000, 0x0802f800
.word	0x08030000, 0x08030800, 0x08031000, 0x08031800, 0x08032000, 0x08032800, 0x08033000, 0x08033800
.word	0x08034000, 0x08034800, 0x08035000, 0x08035800, 0x08036000, 0x08036800, 0x08037000, 0x08037800
.word	0x08038000, 0x08038800, 0x08039000, 0x08039800, 0x0803a000, 0x0803a800, 0x0803b000, 0x0803b800
.word	0x0803c000, 0x0803c800, 0x0803d000, 0x0803d800, 0x0803e000, 0x0803e800, 0x0803f000, 0x0803f800
.word	0x08040000, 0x08040800, 0x08041000, 0x08041800, 0x08042000, 0x08042800, 0x08043000, 0x08043800
.word	0x08044000, 0x08044800, 0x08045000, 0x08045800, 0x08046000, 0x08046800, 0x08047000, 0x08047800
.word	0x08048000, 0x08048800, 0x08049000, 0x08049800, 0x0804a000, 0x0804a800, 0x0804b000, 0x0804b800
.word	0x0804c000, 0x0804c800, 0x0804d000, 0x0804d800, 0x0804e000, 0x0804e800, 0x0804f000, 0x0804f800
.word	0x08050000, 0x08050800, 0x08051000, 0x08051800, 0x08052000, 0x08052800, 0x08053000, 0x08053800
.word	0x08054000, 0x08054800, 0x08055000, 0x08055800, 0x08056000, 0x08056800, 0x08057000, 0x08057800
.word	0x08058000, 0x08058800, 0x08059000, 0x08059800, 0x0805a000, 0x0805a800, 0x0805b000, 0x0805b800
.word	0x0805c000, 0x0805c800, 0x0805d000, 0x0805d800, 0x0805e000, 0x0805e800, 0x0805f000, 0x0805f800
.word	0x08060000, 0x08060800, 0x08061000, 0x08061800, 0x08062000, 0x08062800, 0x08063000, 0x08063800
.word	0x08064000, 0x08064800, 0x08065000, 0x08065800, 0x08066000, 0x08066800, 0x08067000, 0x08067800
.word	0x08068000, 0x08068800, 0x08069000, 0x08069800, 0x0806a000, 0x0806a800, 0x0806b000, 0x0806b800
.word	0x0806c000, 0x0806c800, 0x0806d000, 0x0806d800, 0x0806e000, 0x0806e800, 0x0806f000, 0x0806f800
.word	0x08070000, 0x08070800, 0x08071000, 0x08071800, 0x08072000, 0x08072800, 0x08073000, 0x08073800
.word	0x08074000, 0x08074800, 0x08075000, 0x08075800, 0x08076000, 0x08076800, 0x08077000, 0x08077800
.word	0x08078000, 0x08078800, 0x08079000, 0x08079800, 0x0807a000, 0x0807a800, 0x0807b000, 0x0807b800
.word	0x0807c000, 0x0807c800, 0x0807d000, 0x0807d800, 0x0807e000, 0x0807e800, 0x0807f000, 0x0807f800
.word	0x08080000, 0x08800000
.endif

.ifdef STM32_LCD
.balign	4
flashsectors:	@ 256 x 2KB FLASH sectors of STM32F103ZET6
lib_sectors:	@ lib shares on-chip file flash
.word	0x08000000, 0x08000800, 0x08001000, 0x08001800, 0x08002000, 0x08002800, 0x08003000, 0x08003800
.word	0x08004000, 0x08004800, 0x08005000, 0x08005800, 0x08006000, 0x08006800, 0x08007000, 0x08007800
.word	0x08008000, 0x08008800, 0x08009000, 0x08009800, 0x0800a000, 0x0800a800, 0x0800b000, 0x0800b800
.word	0x0800c000, 0x0800c800, 0x0800d000, 0x0800d800, 0x0800e000, 0x0800e800, 0x0800f000, 0x0800f800
.word	0x08010000, 0x08010800, 0x08011000, 0x08011800, 0x08012000, 0x08012800, 0x08013000, 0x08013800
.word	0x08014000, 0x08014800, 0x08015000, 0x08015800, 0x08016000, 0x08016800, 0x08017000, 0x08017800
.word	0x08018000, 0x08018800, 0x08019000, 0x08019800, 0x0801a000, 0x0801a800, 0x0801b000, 0x0801b800
.word	0x0801c000, 0x0801c800, 0x0801d000, 0x0801d800, 0x0801e000, 0x0801e800, 0x0801f000, 0x0801f800
.word	0x08020000, 0x08020800, 0x08021000, 0x08021800, 0x08022000, 0x08022800, 0x08023000, 0x08023800
.word	0x08024000, 0x08024800, 0x08025000, 0x08025800, 0x08026000, 0x08026800, 0x08027000, 0x08027800
.word	0x08028000, 0x08028800, 0x08029000, 0x08029800, 0x0802a000, 0x0802a800, 0x0802b000, 0x0802b800
.word	0x0802c000, 0x0802c800, 0x0802d000, 0x0802d800, 0x0802e000, 0x0802e800, 0x0802f000, 0x0802f800
.word	0x08030000, 0x08030800, 0x08031000, 0x08031800, 0x08032000, 0x08032800, 0x08033000, 0x08033800
.word	0x08034000, 0x08034800, 0x08035000, 0x08035800, 0x08036000, 0x08036800, 0x08037000, 0x08037800
.word	0x08038000, 0x08038800, 0x08039000, 0x08039800, 0x0803a000, 0x0803a800, 0x0803b000, 0x0803b800
.word	0x0803c000, 0x0803c800, 0x0803d000, 0x0803d800, 0x0803e000, 0x0803e800, 0x0803f000, 0x0803f800
.word	0x08040000, 0x08040800, 0x08041000, 0x08041800, 0x08042000, 0x08042800, 0x08043000, 0x08043800
.word	0x08044000, 0x08044800, 0x08045000, 0x08045800, 0x08046000, 0x08046800, 0x08047000, 0x08047800
.word	0x08048000, 0x08048800, 0x08049000, 0x08049800, 0x0804a000, 0x0804a800, 0x0804b000, 0x0804b800
.word	0x0804c000, 0x0804c800, 0x0804d000, 0x0804d800, 0x0804e000, 0x0804e800, 0x0804f000, 0x0804f800
.word	0x08050000, 0x08050800, 0x08051000, 0x08051800, 0x08052000, 0x08052800, 0x08053000, 0x08053800
.word	0x08054000, 0x08054800, 0x08055000, 0x08055800, 0x08056000, 0x08056800, 0x08057000, 0x08057800
.word	0x08058000, 0x08058800, 0x08059000, 0x08059800, 0x0805a000, 0x0805a800, 0x0805b000, 0x0805b800
.word	0x0805c000, 0x0805c800, 0x0805d000, 0x0805d800, 0x0805e000, 0x0805e800, 0x0805f000, 0x0805f800
.word	0x08060000, 0x08060800, 0x08061000, 0x08061800, 0x08062000, 0x08062800, 0x08063000, 0x08063800
.word	0x08064000, 0x08064800, 0x08065000, 0x08065800, 0x08066000, 0x08066800, 0x08067000, 0x08067800
.word	0x08068000, 0x08068800, 0x08069000, 0x08069800, 0x0806a000, 0x0806a800, 0x0806b000, 0x0806b800
.word	0x0806c000, 0x0806c800, 0x0806d000, 0x0806d800, 0x0806e000, 0x0806e800, 0x0806f000, 0x0806f800
.word	0x08070000, 0x08070800, 0x08071000, 0x08071800, 0x08072000, 0x08072800, 0x08073000, 0x08073800
.word	0x08074000, 0x08074800, 0x08075000, 0x08075800, 0x08076000, 0x08076800, 0x08077000, 0x08077800
.word	0x08078000, 0x08078800, 0x08079000, 0x08079800, 0x0807a000, 0x0807a800, 0x0807b000, 0x0807b800
.word	0x0807c000, 0x0807c800, 0x0807d000, 0x0807d800, 0x0807e000, 0x0807e800, 0x0807f000, 0x0807f800
.word	0x08080000, 0x08800000
.endif

.ltorg

@------------------------------------------------------------------------------------------------
@
@ 2- SD card low-level interface
@
@------------------------------------------------------------------------------------------------

.ifdef	onboard_SDFT
	
  .ifdef sd_is_on_spi

_func_	
sd_cfg:	@ configure spi speed (high), phase, polarity
	@ modifies:	rva, rvb
	ldr	rva, =sd_spi
	ldr	rvb, [rva, #0x08]	@ SPIn_SR, read to clear fault flags if any
	set	rvb, #0x00
	str	rvb, [rva, #0x00]	@ SPIn_CR1  <- disable SPIn (clear faults if any)
	set	rvb, #0x44
	str	rvb, [rva, #0x00]	@ SPIn_CR1 <- PHA 0, POL 0, 8bit, Mstr, Enab, 18 MHz
	set	pc,  lnk

_func_	
sd_slo:	@ configure spi speed (low), phase, polarity
	@ modifies:	rva, rvb
	ldr	rva, =sd_spi
	ldr	rvb, [rva, #0x08]	@ SPIn_SR, read to clear fault flags if any
	set	rvb, #0x00
	str	rvb, [rva, #0x00]	@ SPIn_CR1  <- disable SPIn (clear faults if any)
	set	rvb, #0x74
	str	rvb, [rva, #0x00]	@ SPIn_CR1 <- PHA 0, POL 0, 8bit, Mstr, Enab, 280 KHz
	set	pc,  lnk

_func_	
sd_sel:	@ select SD-card subroutine
	@ modifies:	rva, rvb
	ldr	rva, =sd_cs_gpio
	set	rvb, #sd_cs
	str	rvb, [rva, #io_clear]	@ clear CS pin
	set	pc,  lnk
	
_func_	
sd_dsl:	@ de-select SD-card subroutine
	@ modifies:	rva, rvb
	ldr	rva, =sd_cs_gpio
	set	rvb, #sd_cs
	str	rvb, [rva, #io_set]	@ set CS pin
	set	pc,  lnk
	
_func_	
sd_get:	@ _sgb get sub-routine
	@ modifies:	rva, rvb
	set	rvb, #0xff
_func_	
sd_put:	@ _sgb put sub-routine
	@ modifies:	rva, rvb
	ldr	rva, =sd_spi
	ldr	rva, [rva, #spi_status]	@ ssta
	tst	rva, #spi_txrdy
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

  .endif  @ sd_is_on_spi

  .ifdef sd_is_on_mci

_func_
_sgb:	@ [internal only]
	@ sd-get-block internal func
	@ on entry:  rvc <- block number to be read (scheme int)
	@ on entry:  sv3 <- buffer in which to store block data (scheme bytevector)
	@ on exit:   sv3 <- updated buffer
	@ modifies:  sv3, sv5, rva, rvb, rvc
	bic	sv5, lnk, #lnkbit0	@ sv5 <- lnk, saved
sgb_sr:	@ start/restart transfer
	@ prepare for read-block
	bl	sd_pre			@ prepare mci
	set	rvb, rvc
	bl	sd_arg			@ set arg (block number) in MCIargument
	@ send cmd 17 (read single block)
	set	rvb, #17
	bl	sd_cmd
	eq	rva, #0
	itT	ne
	ldrne	rvc, [rva, #0x08]
	lsrne	rvc, rvc, #7
	bne	sgb_sr
	@ MCIDataCtl <- 512B, block, from card
	ldr	rva, =sd_mci		@ rva <- mci address
	set	rvb, #0x93
	str	rvb, [rva, #0x2c]
	@ get and save data
	set	rvc, #4
sgb_gd:	@ get-data loop
	ldr	rvb, [rva, #0x34]	@ stat
	tst	rvb, #0x3f		@ error?
	itT	ne
	ldrne	rvc, [rva, #0x08]
	lsrne	rvc, rvc, #7
	bne	sd_cm1			@	if so,  jump to restart
	tst	rvb, #0x220000		@ is data available?
	beq	sgb_gd
	ldr	rvb, [rva, #0x80]
	str	rvb, [sv3, rvc]
	eq	rvc, #512
	it	ne
	addne	rvc, rvc, #4
	bne	sgb_gd
	@ return
	orr	lnk, sv5, #lnkbit0
	set	pc,  lnk

_func_
_spb:	@ [internal only]
	@ sd-put-block internal func
	@ on entry:  rvc <- block number to be written (scheme int)
	@ on entry:  sv3 <- buffer with block data to write to sd (scheme bytevector)
	@ modifies:  sv5, rva, rvb, rvc
	bic	sv5, lnk, #lnkbit0	@ sv5 <- lnk, saved
spb_sr:	@ start/restart transfer
	@ prepare for write-block
	bl	sd_pre		@ prepare mci
	set	rvb, rvc
	bl	sd_arg		@ set arg (block number) in MCIargument
	@ send cmd 24 (write single block)
	set	rvb, #24
	bl	sd_cmd
	eq	rva, #0
	itT	ne
	ldrne	rvc, [rva, #0x08]
	lsrne	rvc, rvc, #7
	bne	spb_sr
	@ MCIDataCtl <- 512B, block, to card
	ldr	rva, =sd_mci		@ rva <- mci address
	set	rvb, #0x91
	str	rvb, [rva, #0x2c]
	@ write data
	set	rvc, #4
	adr	lnk, spb_sr
spb_wd:	@ write-data loop
	ldr	rvb, [rva, #0x34]	@ stat
	tst	rvb, #0x3f		@ error?
	itT	ne
	ldrne	rvc, [rva, #0x08]
	lsrne	rvc, rvc, #7
	bne	sd_cm1			@ if so,  jump to restart
	tst	rvb, #0x044000
	beq	spb_wd
	ldr	rvb, [sv3, rvc]
	str	rvb, [rva, #0x80]
	eq	rvc, #512
	it	ne
	addne	rvc, rvc, #4
	bne	spb_wd
	@ wait for DataBlockEnd
	adr	lnk, spb_sr
spb_wt:	@ wait loop
	ldr	rvb, [rva, #0x34]	@ stat
	tst	rvb, #0x3f		@ error?
	itT	ne
	ldrne	rvc, [rva, #0x08]
	lsrne	rvc, rvc, #7
	bne	sd_cm1			@ jump to restart
	tst	rvb, #0x0400
	beq	spb_wt
	ldr	rvc, [rva, #0x14]	@ rvc <- response0	
spb_ts:	@ wait for card in ready-tran state
	bl	sd_pre		@ prepare mci
	set	rvb, #0
	bl	sd_arg		@ set arg (eg. block number) in MCIargument
	set	rvb, #13
	bl	sd_cmd
	eq	rva, #0
	it	ne
	eqne	rvb, #9
	bne	spb_ts
	@ return
	orr	lnk, sv5, #lnkbit0
	set	pc,  lnk

_func_	
sd_pre:	@ mci-prep subroutine
	set	rvb, #0
	ldr	rva, =sd_mci
	str	rvb, [rva, #0x0c]	@ clear previous MCI command
	set	rvb, #0x0700
	orr	rvb, rvb, #0xff
	str	rvb, [rva, #0x38]	@ clear MCI Stat flags
	set	rvb, #(1 << 27)
	str	rvb, [rva, #0x24]	@ set timeout to > 1e8 in MCIDataTimer
	set	rvb, #512
	str	rvb, [rva, #0x28]	@ set MCIDataLength to 512
	set	pc,  lnk

_func_	
sd_arg:	@ mci-arg subroutine (set arg)
	@ on entry: rvb <- arg (0 as raw int, or block number as scheme int)
	ldr	rva, =sd_mci
	bic	rvb, rvb, #3
	lsl	rvb, rvb, #7
	str	rvb, [rva, #0x08]	@ set arg in MCIargument
	set	pc,  lnk
	
_func_	
sd_cmd:	@ mci-cmd subroutine (put cmd)
	@ on entry: rvb <- cmd
	orr	rvb, rvb, #0x0440
	ldr	rva, =sd_mci		@ rva <- mci address
	str	rvb, [rva, #0x0c]
sd_cm0:	@ comand wait loop
	ldr	rvb, [rva, #0x34]	@ stat
	tst	rvb, #0x04		@ cmd timeout?
	bne	sd_cm1
	tst	rvb, #0x40
	beq	sd_cm0
	@ get response
	ldr	rvb, [rva, #0x14]	@ response
	lsr	rvb, rvb, #8
	and	rvb, rvb, #0x0f
	eq	rvb, #9			@ was cmd received while card ready and in tran state?
	itT	eq
	seteq	rva, #0
	seteq	pc,  lnk
_func_	
sd_cm1:	@ wait then restart transfer
	@ [also: internal entry]
	set	rvb, #(1 << 18)
sd_cm2:	@ wait loop
	subs	rvb, rvb, #1
	bne	sd_cm2
	ldr	rva, =sd_mci	
	ldr	rvb, [rva, #0x14]	@ response
	lsr	rvb, rvb, #8
	and	rvb, rvb, #0x0f
	set	pc,  lnk
	
_func_	
sd_slo:	@ configure mci speed (low = 400 KHz), 1-bit bus, clock enabled
	ldr	rva, =sd_mci
	set	rvb, #0x4100
	orr	rvb, rvb, #0xb2
	str	rvb, [rva, #0x04]	@ set MCI to 400KHz, 1-bit bus, CLK enabled, HW flow control
	set	pc,  lnk

_func_	
sd_fst:	@ configure mci speed (high = 2 MHz), wide bus, clock enabled
	ldr	rva, =sd_mci
	set	rvb, #0x4900
	orr	rvb, rvb, #0x22
	str	rvb, [rva, #0x04]        @ set MCI to 2 MHz, wide bus, CLK enabled, HW flow control
	set	pc,  lnk

_func_	
sdpcmd:	@ function to write a command to SD/MMC card during initialization
	@ on entry:	sv4 <- cmd (scheme int)
	@ on entry:	rvc <- arg (raw int)
	@ on exit:	rvb <- response0
	@ modifies:	rva, rvb
	ldr	rva, =sd_mci
	set	rvb, #0
	str	rvb, [rva, #0x0c]	@ clear previous cmd
	set	rvb, #0x0700
	orr	rvb, rvb, #0xff
	str	rvb, [rva, #0x38]	@ clear stat flags
	str	rvc, [rva, #0x08]	@ set arg in MCIargument
	int2raw	rvb, sv4
	and	rvb, rvb, #0xff
	orr	rvb, rvb, #0x0400
	eq	sv4, #i0
	it	ne
	orrne	rvb, rvb, #0x40
	tst	sv4, #0x10000000
	it	ne
	orrne	rvb, rvb, #0x80
	str	rvb, [rva, #0x0c]	@ send cmd
sdpcmb:	@ wait for mci not busy
	ldr	rvb, [rva, #0x34]
	tst	rvb, #0x3800
	bne	sdpcmb
	set	rvb, #0x200000
sdpcmw:	@ wait a bit more (some cards seem to need this)
	subs	rvb, rvb, #1
	bne	sdpcmw
	@ if CMD3 (get address), check status and exit with indicator if bad
	eq	sv4, #0x0d		@ CMD3?
	bne	sdpcmc
	ldr	rvb, [rva, #0x34]
	eq	rvb, #0x40
	itT	ne
	setne	rvb, #0
	setne	pc,  lnk
sdpcmc:	@ continue
	ldr	rvb, [rva, #0x34]
	lsl	rvb, rvb, #21
	lsr	rvb, rvb, #21
	str	rvb, [rva, #0x38]	@ clear status register
	ldr	rvb, [rva, #0x14]	@ rvb <- response0
	set	pc,  lnk
  	
  .endif  @ sd_is_on_mci

.endif
	
@
@ 2- I2C Interrupt routine
@

.ifdef	include_i2c

_func_
hwi2cr:	@ write-out additional address registers, if needed
	@ on entry:	sv5 <- i2c[0/1]buffer
	@ on entry:	rva <- i2c[0/1] base address (also I2CONSET)
	@ modifies:	rvb
	set	rvb, #0			@ rvb <- 0 bytes to send (scheme int)
	tbsti	rvb, sv5, 3		@ store number of bytes to send in i2c buffer[12]
	@ initiate i2c read/write, as master
	set	rvb, #0
	strh	rvb, [rva, #i2c_stat1]	@ clear SR1 clear-able error bits
	swi	run_normal		@ re-enable interrupts
	ldrh	rvb, [rva, #i2c_cr1]	@ rvb <- current content of I2C[0/1]_CR
	orr	rvb, rvb, #0x100	@ rvb <- contents orred with start bit
	strh	rvb, [rva, #i2c_cr1]	@ initiate bus mastering (write start to I2C[0/1]_CR)
hwi2r0:	@ wait for mcu address and registers to have been transmitted
	swi	run_no_irq			@ disable interrupts
	tbrfi	rvb, sv5, 1		@ rvb <- data ready status from i2cbuffer[4]
	eq	rvb, #f			@ is i2c data ready = #f (i.e. addresses have been transmitted)
	it	eq
	seteq	pc,  lnk		@	if so, jump to continue
	swi	run_normal		@ re-enable interrupts
	b	hwi2r0			@ jump to keep waiting

_func_
hwi2ni:	@ initiate i2c read/write, as master
	@ possibly as a re-start condition during read (after writing address bytes)
	@ on entry:	rva <- i2c[0/1] base address (also I2CONSET)
	@ modifies:	rvb
	set	rvb, #0
	strh	rvb, [rva, #i2c_stat1]	@ clear SR1 clear-able error bits
	ldrh	rvb, [rva, #i2c_cr1]	@ rvb <- current content of I2C[0/1]_CR
	orr	rvb, rvb, #0x100	@ rvb <- contents orred with start bit
	strh	rvb, [rva, #i2c_cr1]	@ initiate bus mastering (write start to I2C[0/1]_CR)
	ldrh	rvb, [rva, #i2c_cr2]	@ rvb <- current content of I2C[0/1]_CR2
	orr	rvb, rvb, #0x0400	@ rvb <- start generating Tx interrupts (in case this is re-start)
	strh	rvb, [rva, #i2c_cr2]	@ update I2C[0/1]_CR2
	set	pc,  lnk
	
_func_
hwi2st:	@ get i2c interrupt status and base address
	@ on entry:	rva <- i2c[0/1] base address
	@ modifies:	rvb
	ldrh	rvb, [rva, #i2c_stat2]	@ rvb <- I2C Status from SR2
	tst	rvb, #1			@ are we in slave mode?
	ldrh	rvb, [rva, #i2c_stat1]	@ rvb <- I2C Status from SR1
	itE	eq
	biceq	rvb, rvb, #0x20		@	if so,  rvb <- clr bit 5 (rsvd), indicates slave mode
	orrne	rvb, rvb, #0x20		@	if not, rvb <- set bit 5 (rsvd), indicates master mode
	@ get rid of BTF	
	bic	rvb, rvb, #0x04
	set	pc,  lnk

_func_
i2c_hw_branch:	@ process interrupt
	eq	rvb, #0x02		@ Slave Read/Write -- my address recognzd,  EV1	- ADDR
	beq	i2c_hw_slv_ini
	eq	rvb, #0x40		@ Slave Read  -- new data received,	    EV2	- RxNE
	beq	i2c_hw_rs_get
	eq	rvb, #0x80		@ Slave Write -- master requests byte,	    EV3	- TxE
	beq	i2c_hw_ws_put
	tst	rvb, #0x0400		@ Slave Write -- NAK received, Tx done,	  EV3-1	- AF
	bne	i2c_hw_ws_end
	tst	rvb, #0x0010		@ Slave Read  -- STOP received, EV4	- STOPF
	bne	i2c_hw_rs_end
	tst	rvb, #0x01		@ Master Read/Write -- bus now mastered,    EV5	- SB, MSL
	bne	i2c_hw_mst_bus
	eq	rvb, #0x21		@ Master Read/Write -- bus now mastered,    EV5	- SB, MSL
	beq	i2c_hw_mst_bus
	tst	rvb, #0x02		@ Master Read/Write -- slave ackn. address, EV6	- ADDR
	bne	i2c_hw_mst_ini
	eq	rvb, #0x60		@ Master Read -- new byte received,	    EV7	- RxNE, MSL
	beq	i2c_hw_rm_get
	eq	rvb, #0xA0		@ Master Write -- slave ok to receive data, EV8	- TxE, MSL
	beq	i2c_wm_put
	set	pc,  lnk
	
_func_
i2c_hw_slv_ini: @ Slave Read/Write -- my address recognized  (EV1)
	tbrfi	rva, sv2, 0		@ r6  <- channel-busy status
	eq	rva, #f			@ is channel free?
	itT	eq
	seteq	rva, #i0		@	if so,  rva <- 0 (scheme int)
	tbstieq rva, sv2, 0		@	if so,  store 0 (scheme int) as channel-busy
	set	rva, #0			@ r6  <- 0
	tbsti	rva, sv2, 4		@ store 0 as number of bytes sent/received
	b	i2cxit

_func_
i2c_hw_rs_get:	
	tbrfi	rvb, sv2, 4		@ r7  <- number of bytes sent
	eq	rvb, #0
	itT	eq
	tbstieq rvb, sv2, 2		@	if so,  store 0 as data received so far (clear rcvd dat)
	bleq	yldon
	b	i2c_rs_get

_func_
i2c_hw_ws_put:
	tbrfi	rvb, sv2, 4		@ rvb <- number of bytes sent
	eq	rvb, #0
	beq	i2wsp0
	tbrfi	rva, sv2, 3		@ rva <- number of bytes to send
	eq	rva, rvb
	bne	i2c_ws_put
	b	i2cxit
	
i2wsp0:	@ set number of bytes to send
	bl	gldon
	tbrfi	rva, sv2, 0		@ r6  <- channel-busy status
	eq	rva, #i0		@ was channel free at start of transfer?
	itTTT	eq
	tbstieq rva, sv2, 1		@	if so,  store 0 as data-not-ready/#address-bytes
	ldreq	rva, =eof_char		@	if so,  rva <- eof-character
	streq	rva, [glv, sv1]		@	if so,  store eof-character as object to send
	seteq	rva, #4			@	if so,  rva <- 4 (raw int) = number of bytes to send
	it	eq
	tbstieq rva, sv2, 3		@	if so,  store 4 as number of bytes to send
	b	i2c_ws_put

_func_
i2c_hw_ws_end:	@ Slave Write -- NAK received, Tx done,	  EV3-1	- SR2, AF, #0x10
	ldrh	rvb, [sv3, #i2c_stat1]	@ rvb <- current content of I2C[0/1]_STAT1
	bic	rvb, rvb, #0x0400	@ rvb <- contents with cleared AF bit
	strh	rvb, [sv3, #i2c_stat1]	@ clear AF bit
	b	i2c_ws_end

_func_
i2c_hw_rs_end:	@ Slave Read -- STOP or re-START received
	ldrh	rvb, [sv3, #i2c_cr1]	@ rvb <- current content of I2C[0/1]_CR1
	bic	rvb, rvb, #0x0200	@ rvb <- contents with cleared stop bit
	strh	rvb, [sv3, #i2c_cr1]	@ clear stop bit
	b	i2c_rs_end

_func_
i2c_hw_mst_bus:	@ Master Read/Write -- bus now mastered (EV5)
	bl	gldon
	tbrfi	rva, sv2, 0		@ rva <- address of mcu to send data to (scheme int)
	lsr	rva, rva, #1		@ rva <- mcu-id as int -- note: ends with 0 (i.e. divide by 2)
	strb	rva, [sv3, #i2c_thr]	@ set address of mcu to send data to
	@ wait for target to be addressed (avoids getting a TxE interrupt before then)
	@ a bit risky if remote device doesn't exist we get jammed inside interrupt!!!
	set	rvb, #0x1000000
i2c_hw_mst_bwt:
	subs	rvb, rvb, #1
	beq	i2cxit
	ldrh	rva, [sv3, #i2c_stat1]
	tst	rva, #0x02
	beq	i2c_hw_mst_bwt
_func_
i2c_hw_mst_ini: @ Master Read/Write -- slave aknowledged address (EV6)
	ldrh	rvb, [sv3, #i2c_stat2]	@ rvb <- I2C Status from SR2 (to clear THIS interrupt)
	tbrfi	rvb, sv2, 0		@ rva <- adr of mcu to wrt/rd dat to/frm (scheme int{w}/float{r})
	tst	rvb, #0x02		@ is this a write operation?
	beq	i2c_wm_ini
	ldrh	rvb, [sv3, #i2c_cr1]	@ rvb <- current content of I2C[0/1]_CR
	orr	rvb, rvb, #0x0400	@ rvb <- contents with ack bit set
	strh	rvb, [sv3, #i2c_cr1]	@ set ack in cr	
	b	i2c_rm_ini

_func_
hwi2we:	@ set busy status/stop bit at end of write as master
	@ on entry:	sv2 <- i2c[0/1] buffer address
	@ on entry:	sv3 <- i2c[0/1] base address
	@ on entry:	rvb <- #f
hwi2ww:	@ wait for either TxE or BTF to be set before setting STOP condition
	ldrh	rvb, [sv3, #i2c_stat1]	@ rvb <- current content of I2C[0/1]_STAT1
	tst	rvb, #0x84
	beq	hwi2ww
	tbrfi	rva, sv2, 3		@ r6  <- number of data bytes to send (raw int)
	eq	rva, #0			@ were we sending 0 bytes (i.e. rd as mstr & done wrt adr byte)
	beq	hwi2wv
	set	rvb, #f			@ rvb <- #f
	tbsti	rvb, sv2, 0		@ set busy status to #f (transfer done)
	ldrh	rvb, [sv3, #i2c_cr1]	@ rvb <- current content of I2C[0/1]_CR1
	orr	rvb, rvb, #0x0200	@ rvb <- contents orred with stop bit
	strh	rvb, [sv3, #i2c_cr1]	@ initiate stop (write stop to I2C[0/1]_CR1)
	set	pc,  lnk

hwi2wv:	@ prepare for re-start
	ldrh	rvb, [sv3, #i2c_cr2]	@ rvb <- current content of I2C[0/1]_CR2
	tst	rvb, #0x02		@ is interface busy?
	bne	hwi2wv
	bic	rvb, rvb, #0x0400	@ rvb <- stop generating Tx interrupts
	strh	rvb, [sv3, #i2c_cr2]	@ update I2C[0/1]_CR2
	set	pc,  lnk
	
_func_
i2c_hw_rm_get:
	ldrh	rvb, [sv3, #i2c_cr1]	@ rvb <- current content of I2C[0/1]_CR
	tst	rvb, #0x0400		@ is ack bit asserted?
	bne	i2c_rm_get		@	if so,  jump to perform normal read
	b	i2c_rm_end		@ jump to end of read as master (nack was set on prior byte)

_func_
hwi2re:	@ set stop bit if needed at end of read-as-master
	ldrh	rvb, [sv3, #i2c_cr1]	@ rvb <- current content of I2C[0/1]_CR1
	orr	rvb, rvb, #0x0600	@ rvb <- contents orred with stop bit and ack bit
	strh	rvb, [sv3, #i2c_cr1]	@ initiate stop (write stop to I2C[0/1]_CR1)

hwi2ry:	@ wait for device not busy, no longer master
	ldrh	rvb, [sv3, #i2c_stat2]
	tst	rvb, #0x03
	bne	hwi2ry
hwi2rz:	@ flush DR
	ldrh	rvb, [sv3, #i2c_stat1]
	tst	rvb, #0x40
	it	ne
	ldrbne	rvb, [sv3, #i2c_rhr]
	bne	hwi2rz	
	set	pc,  lnk
	
_func_
hwi2cs:	@ clear interrupt (if it needs a read of SR2 to clear)
	ldrh	rva, [sv3, #i2c_stat2]
	set	pc,  lnk
	
_func_
i2cstp:	@ prepare to end Read as Master transfer
	ldrh	rvb, [sv3, #i2c_cr1]	@ rvb <- current content of I2C[0/1]_CR
	bic	rvb, rvb, #0x0400	@ rvb <- contents with ack bit cleared
	strh	rvb, [sv3, #i2c_cr1]	@ set nak in cr
	set	pc,  lnk

_func_
i2putp:	@ Prologue:	write additional address bytes to i2c, from buffer or r12 (prologue)
	@ check if address bytes need sending
	@ if not, return via lnk unless #bytes to write is zero -> if so, jump to i2pute o2 i2c_wm_end
	@ if so, write them out and subtract count, then skip i2putc -> jump to i2pute
	@ with link set to i2cxit
	tbrfi	rva, sv2, 1		@ rva <- number of additional address bytes to send (scheme int)
	eq	rva, #i0		@ no more address bytes to send?
	itTT	eq
	tbrfieq rva, sv2, 3		@	if so,  rva <- number of data bytes to send (raw int)
	tbrfieq rvb, sv2, 4		@	if so,  rvb <- number of data bytes sent (raw int)
	eqeq	rva, rvb		@	if so,  are we done sending data?
	beq	i2c_wm_end		@		if so, jump to stop or restart x-fer and exit
	tbrfi	rvb, sv2,  1		@ rvb <- number of address bytes remaining to send (scheme int)
	eq	rvb, #i0		@ done sending address bytes?
	it	eq	
	seteq	pc,  lnk		@	if so,  return
	and	rvb, rvb, #0x03
	eq	rvb, #i0
	bne	i2cxit	
	tbrfi	rvb, sv2,  1		@ rvb <- number of address bytes remaining to send (scheme int)
	sub	rvb, rvb, #4		@ rvb <- updated number of address bytes to send (scheme int)
	tbsti	rvb, sv2, 1		@ store updated num of address bytes to send in i2cbuffer[4]
	add	rva, sv2, #8		@ rva <- address of additional address bytes in i2cbuffer
	lsr	rvb, rvb, #2
	ldrb	rva, [rva, rvb]		@ rva <- next address byte to send
	strb	rva, [sv3, #i2c_thr]	@ put next data byte in I2C data register
	b	i2cxit

_func_
i2pute:	@ Epilogue:	set completion status if needed (epilogue)
	tbrfi	rva, sv2, 3		@ rva <- number of data bytes to send (raw int)
	tbrfi	rvb, sv2, 4		@ rvb <- number of data bytes sent (raw int)
	eq	rva, rvb		@ done sending?
	beq	i2c_wm_end		@	if so,  jump to end transfer
	set	pc,  lnk


.endif

