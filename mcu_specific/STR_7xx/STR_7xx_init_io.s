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

	.word	ptmisr			@ 0:	timer0
	.word	i0			@ 1
	.word	i0			@ 2
	.word	i0			@ 3
	.word	i0			@ 4
	.word	i0			@ 5
	.word	i0			@ 6
	.word	pi2isr			@ 7:	i2c0 (if included)
	.word	pi2isr			@ 8:	i2c1 (if included)
	.word	puaisr			@ 9:	uart0
	.word	puaisr			@ 10:	uart1
	.word	i0			@ 11
	.word	i0			@ 12
	.word	i0			@ 13
	.word	i0			@ 14
	.word	i0			@ 15
	.word	i0			@ 16
	.word	i0			@ 17
	.word	i0			@ 18
	.word	ptmisr			@ 19:	timer1
	.word	i0			@ 20
	.word	i0			@ 21
	.word	i0			@ 22
	.word	i0			@ 23
	.word	i0			@ 24
	.word	i0			@ 25
	.word	usbisr			@ 26:	USB LP (if included)
	.word	i0			@ 27
	.word	i0			@ 28
	.word	i0			@ 29
	.word	i0			@ 30
	.word	i0			@ 31


hwinit:	@ pre-set common values
	set	r0,  #0
	set	r1,  #1
	set	r2,  #2
	set	r3,  #3
	@ initialization of voltage and clocks
	@ wait for voltage regulator to stabilize
	ldr	r6,  =rcc_base
hwiwt0:	ldr	r7,  [r6,  #0x54]
	tst	r7,  #0x1000		@ PRCCU PCU_PWRCR	Voltage Regulator OK bit test
	beq	hwiwt0
	@ initialize the phase-locked loop (PLL1) to get 48MHZ operation
	ldr	r7,  =PLL_parms
	str	r7,  [r6,  #0x18]	@ RCCU_PLL1CR  <- PLL_parms
	str	r0,  [r6,  #0x44]	@ RCCU PCU_PDIVR <- APB2 and APB1 clocks are master clock (48 MHz)
hwiwt1:	ldr	r7,  [r6,  #0x08]
	tst	r7,  #0x02		@ RCCU_CFR,		PLL1-Locked bit test
	beq	hwiwt1
	ldr	r7,  =0x8009
	str	r7,  [r6,  #0x08]	@ RCCU_CFR		connect PLL1 (keep x-tal div2 enabled)
	@ initialization of gpio pins
	@ Make ALLLED open drain outputs <- set ALLLED bits of LED IOPORT/LEDPINSEL PC0, PC1 & PC2 to 0, 0 & 1
	ldr	r6,  =LEDPINSEL
	ldr	r8,  =ALLLED
	ldr	r7,  [r6]
	bic	r7,  r7, r8
	str	r7,  [r6]		@ IOPORT1 PC0      <- set LED PINS to open drain outputs (phase 1)
	ldr	r7,  [r6,  #0x04]
	bic	r7,  r7, r8
	str	r7,  [r6,  #0x04]	@ IOPORT1 PC1      <- set LED PINS to open drain outputs (phase 2)
	ldr	r7,  [r6,  #0x08]
	bic	r7,  r7, r8
	orr	r7,  r7, r8
	str	r7,  [r6,  #0x08]	@ IOPORT1 PC2      <- set LED PINS to open drain outputs (phase 3)
	@ initialization of interrupts
	ldr	r6,  =EIC_base
	ldr	r7,  =genisr
	str	r7,  [r6,  #0x18]	@ EIC_IVR	<- default IRQ handler (sets upper 16 bits of IVR)
	lsl	r7,  r7, #16
	orr	r7,  r7, #0x07		@ all IRQs have priority of 7
	str	r7,  [r6,  #0x60]	@ TIMER0 ISR = EIC_SIR0		<- genisr (IRQ0  = offset #x60 + #x00)
	str	r7,  [r6,  #0x84]	@ UART0 ISR  = EIC_SIR9		<- genisr (IRQ9  = offset #x60 + #x24)
	str	r7,  [r6,  #0x88]	@ UART1 ISR  = EIC_SIR10	<- genisr (IRQ10 = offset #x60 + #x28)
	str	r7,  [r6,  #0x9c]	@ I2C0 ISR   = EIC_SIR15	<- genisr (IRQ15 = offset #x60 + #x3C)
	str	r7,  [r6,  #0xa0]	@ I2C1 ISR   = EIC_SIR16	<- genisr (IRQ16 = offset #x60 + #x40)
	str	r7,  [r6,  #0xac]	@ TIMER1 ISR = EIC_SIR19	<- genisr (IRQ19 = offset #x60 + #x4C)
	str	r7,  [r6,  #0x7c]	@ I2C0 ISR   = EIC_SIR7		<- genisr (IRQ7  = offset #x60 + #x1C)
	str	r7,  [r6,  #0x80]	@ I2C1 ISR   = EIC_SIR8		<- genisr (IRQ8  = offset #x60 + #x20)
	ldr	r7,  =scheme_ints_enb
	str	r7,  [r6,  #0x20]	@ EIC_IER	<- enable scheme and uart ints in EIC
	ldr	r7,  =int_clear_vals
	str	r7,  [r6,  #0x40]	@ EIC_IPR	<- clear any pending interrupts in EIC
	str	r1,  [r6]		@ EIC_ICR	<- enable EIC IRQ output
	@ initialization of mcu-id for variables (normally I2c address if slave enabled)
	ldr	r6,  =I2C0ADR
	set	r7,  #mcu_id
	str	r7,  [r6]		@ I2C0ADR <- set mcu address
	ldr	r6,  =0xC0002010
	str	r7,  [r6]		@ I2C1ADR <- set mcu address
	@ initialization of UART0 for 9600 8N1 operation
	@ TX <- P0.9 (port 0, bit 9, PC0,1,2 <- 1,1,1 = Push-Pull output)
	@ RX <- P0.8 (port 0, bit 8, PC0,1,2 <- 1,0,0 = TTL input)
	ldr	r6,  =uart0_base
	ldr	r7,  =UART0_DIV
	str	r7,  [r6]		@ UART0_BR <- divisor for selected baud rate (eg. 9600)
	ldr	r7,  =0x0589
	str	r7,  [r6,  #0x0c]	@ UART0_CR <- enable UART0 with 8N1 mode and FIFO on
	str	r0,  [r6,  #0x20]	@ UART0_TxRSTR	<- reset Tx FIFO
	str	r0,  [r6,  #0x24]	@ UART0_RxRSTR	<- reset Rx FIFO
	@ select UART function
	ldr	r6,  =ioport0_base
	ldr	r7,  [r6]
	orr	r7,  r7, #0x0300
	str	r7,  [r6]		@ IOPORT0 PC0	<- set I/O (phase 1)
	ldr	r7,  [r6,  #0x04]
	bic	r7,  r7, #0x0300
	orr	r7,  r7, #0x0200
	str	r7,  [r6,  #0x04]	@ IOPORT0 PC1	<- set I/O (phase 2)
	ldr	r7,  [r6,  #0x08]
	bic	r7,  r7, #0x0300
	orr	r7,  r7, #0x0200
	str	r7,  [r6,  #0x08]	@ IOPORT0 PC2	<- set I/O (phase 3)
	@ enable UART receive interrupts
	ldr	r6,  =uart0_base
	str	r1,  [r6,  #0x10]	@ UART0_IE	<- enable RxBufNotEmpty Interrupt
	ldr	r6,  =USB_CONF
	str	r0,  [r6]		@ USB_CONF <- USB device is not yet configured

.ifdef	onboard_SDFT
	
  .ifdef sd_is_on_spi

	@ disable BSPI
	ldr	r6, =sd_spi
 	set	r7, #0x00
	str	r7, [r6, #0x08]	@ CSR1    <- disabled
	@ configure CS pin as gpio out
	ldr	r6, =sd_cs_gpio
	ldr	r7, [r6, #0x00]
	orr	r7, r7, #sd_cs
	str	r7, [r6, #0x00]
	ldr	r7, [r6, #0x04]
	bic	r7, r7, #sd_cs
	str	r7, [r6, #0x04]
	ldr	r7, [r6, #0x08]
	orr	r7, r7, #sd_cs
	str	r7, [r6, #0x08]
	@ de-select sd
	ldr	r7, [r6, #0x0C]	@ rvb <- pin statuses
	orr	r7, r7, #sd_cs
	str	r7, [r6, #0x0C]	@ set CS pin
	@ configure inbound CS (slave CS) to weak pull-up and set it high
	ldr	r6, =sd_spi_gpio
	ldr	r7, [r6, #0x00]
	orr	r7, r7, #(1 << 7)
	str	r7, [r6, #0x00]
	ldr	r7, [r6, #0x04]
	bic	r7, r7, #(1 << 7)
	str	r7, [r6, #0x04]
	ldr	r7, [r6, #0x08]
	orr	r7, r7, #(1 << 7)
	str	r7, [r6, #0x08]
	ldr	r7, [r6, #0x0C]	@ rvb <- pin statuses
	orr	r7, r7, #(1 << 7)
	str	r7, [r6, #0x0C]	@ set CS pin
	@ configure: P0.4 <- S1.MISO, P0.5 <- S1.MOSI, P0.6 <- S1.SCLK
	ldr	r6, =sd_spi_gpio
	ldr	r7, [r6, #0x00]
	orr	r7, r7, #(0x7 << 4)
	str	r7, [r6, #0x00]
	ldr	r7, [r6, #0x04]
	orr	r7, r7, #(0x7 << 4)
	str	r7, [r6, #0x04]
	ldr	r7, [r6, #0x08]
	orr	r7, r7, #(0x7 << 4)
	str	r7, [r6, #0x08]
	 @ low-speed (approx 400 KHz)
	ldr	r6, =sd_spi
	set	r7, #0x41
	str	r7, [r6, #0x0c]	@ CSR2    <- disabled
	set	r7, #0x00
	str	r7, [r6, #0x08]	@ CSR1    <- disabled
	set	r7, #0x78
	str	r7, [r6, #0x10]	@ CLK_DIV <- 48 MHz / 120 = 400 KHz
	set	r7, #0x01
	str	r7, [r6, #0x08]	@ CSR1    <- enable, master, POL=PHA=0, 8-bit
	set	r7, #0x03
	str	r7, [r6, #0x08]	@ CSR1    <- enable, master, POL=PHA=0, 8-bit
	
  .endif @sd_is_on_spi

.endif	@ onboard_SDFT
	
	@ copy FLASH writing code to RAM
	ldr	r6,  =flsRAM		@ r6 <- start address of flashing code
	ldr	r7,  =flsRND		@ r7 <- end address of flashing code
	ldr	r9,  =heaptop1		@ r9 <- RAM target address
	add	r9,  r9, #4
hwiwt6:	ldr	r10, [r6]		@ r10 <- next flashing code instruction
	str	r10, [r9]		@ store it in free RAM
	eq	r6,  r7			@ done copying the flashing code?
	addne	r6,  r6,  #4		@	if not, r6  <- next flashing code source address
	addne	r9,  r9,  #4		@	if not, r9  <- next flashing code target address
	bne	hwiwt6			@	if not, jump to keep copying flashing code to RAM

.ifdef	native_usb


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
	set	r5,  #0xA0000000
	ldr	r6,  =usb_base
	set	r7,  #0x10
	str	r7,  [r5,  #0x1c]	@ RCCU_PER   -> enable USB kernel
	str	r1,  [r6,  #0x40]	@ USB_CNTR   -> exit power down mode
	set	r7,  #0x0150
	str	r7,  [r5,  #0x4c]	@ PCU_PLL2CR -> PLL2 at 12x4MHz, (input 3-5MHZ) ref is HCLK
hwiwt2:	ldr	r7,  [r5,  #0x4c]
	tst	r7,  #0x8000		@ PCU_PLL2CR,	PLL2-Locked bit test
	beq	hwiwt2
	@ need to make sure there's enough time between exitng pwr mode (above) and exiting reset mode (below)
	@ if needed, block below could probably be moved to after buffer alloc table initialization
	@ or, branch-link to a wait loop
	set	r7,  #0x10		@ 00
hwiwt3:	subs	r7,  r7, #1
	bne	hwiwt3
	str	r0,  [r6,  #0x40]	@ USB_CNTR   -> exit reset mode
	str	r0,  [r6,  #0x40]	@ USB_CNTR   -> exit reset mode (again, to make sure)
	str	r0,  [r6,  #0x44]	@ USB_ISTR   -> clear potential spurious pending interrupts
	set	r7,  #0x01D0
	str	r7,  [r5,  #0x4C]	@ PCU_PLL2CR -> connect PLL2
	set	r7,  #0x9C00
	str	r7,  [r6,  #0x40]	@ USB_CNTR   -> generate interrupts on ctr, wakeup, suspend, reset
	@ end of said 'moveable?' block
	ldr	r10, =EIC_base
	ldr	r7,  =genisr
	lsl	r7,  r7, #16
	orr	r7,  r7, #0x07		@ all IRQs have priority of 7
	str	r7,  [r10, #0x78]	@ USB HP ISR = EIC_SIR6		<- usbisr (IRQ6  = offset #x60 + #x18)
	str	r7,  [r10, #0xc8]	@ USB LP ISR = EIC_SIR26	<- usbisr (IRQ26 = offset #x60 + #x68)
	str	r0,  [r6,  #0x50]	@ USB_BTABLE    -> buffer allocation table starts at offset 0
	sub	r9,  r6, #0x0800
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
	str	r7,  [r9,  #0x2c]	@ USB_COUNT2_RX -> blk sz = 32bytes, buf sz = 64bytes, 0 byts received
	ldr	r7,  =0x01a0
	str	r7,  [r9,  #0x30]	@ USB_ADR3_TX   -> EP3 send buffer start offset = 0x0120
	str	r0,  [r9,  #0x34]	@ USB_COUNT3_TX -> 0 bytes to transmit
	ldr	r7,  =0x01e0
	str	r7,  [r9,  #0x38]	@ USB_ADR3_RX   -> EP3 receive buffer start offset = 0x0160
	set	r7,  #0x8400
	str	r7,  [r9,  #0x3c]	@ USB_COUNT3_RX -> blk sz = 32bytes, buf sz = 64bytes, 0 byts received
	@ if needed, block below could probably be moved to after buffer alloc table initialization
	@ or, branch-link to a wait loop
	ldr	r7,  =0x3230
	str	r7,  [r6]		@ USB_EP0R      -> configure enpoint 0 as control EP
	set	r7,  #0x80
	str	r7,  [r6,  #0x4c]	@ USB_DADDR	-> enable USB, address is 0

.endif	@ native_usb

	@ enf of the hardware initialization
	set	pc,  lnk


@------------------------------------------------------------------------------------------------
@ STR_7xx
@
@	 1- Initialization from FLASH, writing to and erasing FLASH
@	 2- I2C Interrupt routine
@
@------------------------------------------------------------------------------------------------

@
@ 1- Initialization from FLASH and writing to FLASH
@

FlashInitCheck: @ return status of flash init enable/override gpio pin (P0.3) in r6
	ldr	rva, =ioport0_base
	ldr	rva, [rva, #io_set]		@ rva <- data values from IOPORT0_PD
	and	rva, rva, #0x08			@ rva <- status of P0.3
	set	pc,  lnk			@ return

wrtfla:	@ write to flash, sv2 = page address, sv4 = file descriptor
libwrt:	@ write to on-chip lib flash (lib shares on-chip file flash)
	@ uses 56 bytes of user-stack space
	stmfd	sp!, {rva, rvb, sv3, rvc, lnk}	@ store scheme registers onto stack
	@ copy buffer data from file descriptor (sv4) (RAM) to STR7 FLASH buffer (sv2)
	vcrfi	sv3, sv4, 3			@ sv3 <- buffer address
	set	rvc, #0
wrtfl0:	swi	run_normal			@ enable interrupts (user mode)
	set	rvb, #1
	ldr	rva, =EIC_base
	str	rvb, [rva]
	set	rvb, #0x100000			@ rvb <- FLASH_base
	ldr	rva, [rvb, #0x14]
	tst	rva, #0xf6
	bne	wrterr
	set	rva, #0
	str	rva, [rvb, #0x14]
	set	rvb, #0x100000			@ rvb <- FLASH_base
	ldr	rva, [rvb]			@ rva <- status of flash banks from FLASH_CR0
	tst	rva, #0x70000000		@ was resume bit set or is WPG/DWPG stuck?
	@ wait if resuming/recovering
	seteq	rva, #1
@	setne	rva, #0x100000			@ rva <- wait, approx 40 ms
	setne	rva, #0x2000000			@ rva <- wait, approx 1 s
wrtfw0:	subs	rva, rva, #1
	bne	wrtfw0
	ldr	rva, [rvb]			@ rva <- status of flash banks from FLASH_CR0
	tst	rva, #0x70000000		@ was resume bit set or is WPG/DWPG stuck?
@	bne	rsmwrf
	ldrne	rva, [rvb, #0x10]		@ rva <-  troublesome address in FLASH_AR
	bne	wrterr
	eq	rvc, #F_PAGE_SIZE		@ done writing page?
	beq	wrtfxt				@	if so,  jump to finish up
	add	rva, sv2, rvc
	str	rva, [rvb, #0x10]		@ store address at which to write in FLASH_AR
	add	rvc, rvc, #4
	ldr	rva, [sv3, rvc]			@ rva, rvc <- next source data word
	str	rva, [rvb, #0x08]		@ store data words in FLASH_DR0-1
	add	rvc, rvc, #4
	ldr	rva, [sv3, rvc]			@ rva, rvc <- next two source data words
	str	rva, [rvb, #0x0C]		@ rva, rvc <- next two source data words
rsmwrf:	@ continue/resume double-word write
	set	rva, #0x10000000		@ rva <- DWPG command bit (write double word)
@	set	rva, #0x20000000		@ rva <- WPG command bit (write word)
	str	rva, [rvb]			@ set command bit (rva) in CR0 (rvb)
	set	rvb, #0
	ldr	rva, =EIC_base
	str	rvb, [rva]
	set	rva, #0x90000000		@ rva <- DWPG and WMS command bits (start to write dble word)
@	set	rva, #0xA0000000		@ rva <- WPG and WMS command bits (start to write word)
	ldr	rvb, =heaptop1
	add	rvb, rvb, #4
	swi	isr_no_irq
	adr	lnk, wrtfl0			@ lnk <- return address for after FLASH command
	set	pc,  rvb			@ jump to FLASH write routine in RAM
wrtfxt:	@ finish up
	set	rva, #0				@ rva <- 0
	str	rva, [rvb, #0x14]		@ clear 1/0 overwriting error bits in FLASH_ER
	@ wait a bit (recovery?)
	set	rva, #0x6000			@ rva <- wait, approx 1 ms
wrtfwt:	subs	rva, rva, #1
	bne	wrtfwt
	ldmfd	sp!, {rva, rvb, sv3, rvc, lnk}	@ restore scheme registers from stack
	set	pc,  lnk			@ return

wrterr:	@ write error other than 1/0
	raw2int	sv1, rva
	ldmfd	sp!, {rva, rvb, sv3, rvc, lnk}	@ restore scheme registers from stack
	ldr	sv4, =flash_
	b	error4

.balign 4

flash_:	.word	0x0400 | symbol_tag	@ number of bytes, symbol tag
	.ascii	"FLSH"
	.balign	4
	
ersfla:	@ erase flash sector that contains page address in sv2
libers:	@ erase on-chip lib flash sector (lib shares on-chip file flash)
	stmfd	sp!, {rva, rvb, rvc, lnk}	@ store scheme registers onto stack
	@ prepare flash sector for erase
	set	rvb, #0x100000			@ rvb <- flash_base = FLASH_CR0
	set	rva, #0x08000000		@ rva <- SER command bit (sector erase)
	str	rva, [rvb]			@ set command bit (r6) in CR0 (rvb)
	bl	pgsctr				@ rva <- sector number (raw int), from page address in sv2
	set	rvb, #1				@ rvb <- 1
	lsl	rva, rvb, rva			@ rva <- bit indicating which sector to erase
	set	rvb, #0x100000			@ rvb <- flash_base = FLASH_CR0
	str	rva, [rvb, #4]			@ specify which sector to erase in FLASH_CR1
rsmerf:	@ continue/resume flash erase
	set	rva, #0x88000000		@ rva <- SER and WMS command bits (start sector erase)
	ldr	rvb, =heaptop1
	add	rvb, rvb, #4
	swi	isr_no_irq
	adr	lnk, ersfxt			@ lnk <- return address for after FLASH command
	set	pc,  rvb			@ jump to FLASH erase routine in RAM
ersfxt:	@ finish up
	swi	run_normal			@ enable interrupts (user mode)
	set	rvb, #0x100000			@ rvb <- flash_base = FLASH_CR0
	ldr	rva, [rvb, #4]
	eq	rva, #0
	streq	rva, [rvb]
	ldr	rva, [rvb]
	tst	rva, #0x48000000
	bicne	rva, rva, #0x40000000		@ rva <- SER command bit (sector erase)
	strne	rva, [rvb]			@ set command bit (r6) in CR0 (rvb)
	setne	rva, #0x08000000		@ rva <- SER command bit (sector erase)
	strne	rva, [rvb]			@ set command bit (r6) in CR0 (rvb)
	bne	rsmerf
	ldmfd	sp!, {rva, rvb, rvc, lnk}	@ restore scheme registers from stack
	set	pc,  lnk			@ return

flsRAM:	@ code to be copied to RAM, at fre+, such that execution is from RAM while
	@ FLASH is being written. i.e. do: (1) ldr lnk, =xyz (2) set pc, fre to initiate
	set	rvb, #0x100000			@ rvb <- flash_base = FLASH_CR0
	str	rva, [rvb]			@ start write/erase operation
	set	rva, #0x100000			@ rva <- timeout to suspend, approx 200 ms
	@ wait loop
	set	rvb, #0x100000			@ rvb <- flash_base = FLASH_CR0
	eq	rva, #0
	subsne	rva, rva, #1			@ rva <- timeout updated, is it zero?
	ldreq	rvb, [rvb]
	tsteq	rvb, #0x10
	orreq	rva, rvb, #0x40000000
	set	rvb, #0x100000			@ rvb <- flash_base = FLASH_CR0
	streq	rva, [rvb]			@	if so,  initiate suspend of operation
	ldr	rvb, [rvb]			@ rvb <- status of flash banks from FLASH_CR0
	tst	rvb, #0x12			@ is flash bank 0 busy? (chk LOCK and BSYA0, 'chckbl' whl bsy)
	subne	pc,  pc, #48			@	if so,  jump to keep waiting
	@ wait a bit if WPG/DWPG/SER is stuck
	set	rva, #0x100000			@ rva <- timeout to suspend, approx 100 ms
	set	rvb, #0x100000			@ rvb <- flash_base = FLASH_CR0
	ldr	rvb, [rvb]
	subs	rva, rva, #1
	tstne	rvb, #0x38000000
	subne	pc,  pc, #24			@	if so,  jump to keep waiting
	@ exit
	set	pc,  lnk			@ return
flsRND: @ end of ram code
	

flashsectors:	@ 4 x 8KB, 1 x 32KB, 3 x 64KB, Bank 0 FLASH sectors of STR711
lib_sectors:	@ lib shares on-chip file flash
.word	0x00000, 0x02000, 0x04000, 0x06000, 0x08000
.word	0x10000, 0x20000, 0x30000, 0x40000, 0x0FFFFFFC


.ltorg	@ dump literal constants here => up to 4K of code before and after this point

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
	set	rvb, #0x41
	str	rvb, [rva, #0x0c]	@ CSR2    <- disabled
	set	rvb, #0x00
	str	rvb, [rva, #0x08]	@ CSR1    <- disabled
	set	rvb, #0x0a
	str	rvb, [rva, #0x10]	@ CLK_DIV <- 48 MHz / 10 = 4.8 MHz
	set	rvb, #0x01
	str	rvb, [rva, #0x08]	@ CSR1    <- enable, master, POL=PHA=0, 8-bit
	set	rvb, #0x03
	str	rvb, [rva, #0x08]	@ CSR1    <- enable, master, POL=PHA=0, 8-bit
	set	pc,  lnk

_func_	
sd_slo:	@ configure spi speed (low), phase, polarity
	@ modifies:	rva, rvb
	ldr	rva, =sd_spi
	set	rvb, #0x41
	str	rvb, [rva, #0x0c]	@ CSR2    <- disabled
	set	rvb, #0x00
	str	rvb, [rva, #0x08]	@ CSR1    <- disabled
	set	rvb, #0x78
	str	rvb, [rva, #0x10]	@ CLK_DIV <- 48 MHz / 120 = 400 KHz
	set	rvb, #0x01
	str	rvb, [rva, #0x08]	@ CSR1    <- enable, master, POL=PHA=0, 8-bit
	set	rvb, #0x03
	str	rvb, [rva, #0x08]	@ CSR1    <- enable, master, POL=PHA=0, 8-bit
	set	pc,  lnk

_func_	
sd_sel:	@ select SD-card subroutine
	@ modifies:	rva, rvb
	ldr	rva, =sd_cs_gpio
	ldr	rvb, [rva, #io_state]	@ rvb <- pin statuses
	bic	rvb, rvb, #sd_cs
	str	rvb, [rva, #io_state]	@ clear CS pin
	set	pc,  lnk
	
_func_	
sd_dsl:	@ de-select SD-card subroutine
	@ modifies:	rva, rvb
	ldr	rva, =sd_cs_gpio
	ldr	rvb, [rva, #io_state]	@ rvb <- pin statuses
	orr	rvb, rvb, #sd_cs
	str	rvb, [rva, #io_state]	@ set CS pin
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
	and	rvb, rvb, #0xff
	lsl	rvb, rvb, #8
	ldr	rva, =sd_spi
	str	rvb, [rva, #spi_thr]	@ sdtx (sdat)
sd_gpw:	@ wait
	ldr	rvb, [rva, #spi_status]	@ ssta
	tst	rvb, #spi_rxrdy		@ sdrr
	beq	sd_gpw
	ldr	rvb, [rva, #spi_rhr]	@ sdrx (sdat)
	lsr	rvb, rvb, #8
	set	pc, lnk

  .endif @ sd_is_on_spi

.endif	@ 	onboard_SDFT
	
@
@ 2- I2C Interrupt routine
@

.ifdef	include_i2c

hwi2cr:	@ write-out additional address registers, if needed
	@ on entry:	sv5 <- i2c[0/1]buffer
	@ on entry:	r6  <- i2c[0/1] base address (also I2CONSET)
	set	rvb, #0			@ r7  <- 0 bytes to send (scheme int)
	tbsti	rvb, sv5, 3		@ store number of bytes to send in i2c buffer[12]
	@ initiate i2c read/write, as master
	swi	run_normal			@ re-enable interrupts
	ldrb	rvb, [rva, #i2c_cr]	@ r7  <- current content of I2C[0/1]_CR
	orr	rvb, rvb, #0x08		@ r7  <- contents orred with start bit
	strb	rvb, [rva, #i2c_cr]	@ initiate bus mastering (write start to I2C[0/1]_CR)
hwi2r0:	@ wait for mcu address and registers to have been transmitted
	swi	run_no_irq			@ disable interrupts
	tbrfi	rvb, sv5, 1		@ r7  <- data ready status from i2cbuffer[4]
	eq	rvb, #f			@ is i2c data ready = #f (i.e. addresses have been transmitted)
	seteq	pc,  lnk		@	if so, jump to continue
	swi	run_normal			@ re-enable interrupts
	b	hwi2r0			@ jump to keep waiting
	
hwi2ni:	@ initiate i2c read/write, as master
	@ on entry:	r6  <- i2c[0/1] base address (also I2CONSET)
	ldrb	rvb, [rva, #i2c_cr]	@ r7  <- current content of I2C[0/1]_CR
	orr	rvb, rvb, #0x08		@ r7  <- contents orred with start bit
	strb	rvb, [rva, #i2c_cr]	@ initiate bus mastering (write start to I2C[0/1]_CR)
	set	pc,  lnk
	
hwi2st:	@ get i2c interrupt status and base address
	ldrb	rvb, [rva, #i2c_stat2]	@ r7  <- I2C Status from SR2
	eq	rvb, #0			@ anything from SR2?
	lslne	rvb, rvb, #8		@	if so,   r7  <- I2C SR2 status, shifted
	ldrbeq	rvb, [rva, #i2c_stat1]	@	if not,  r7  <- I2C Status from SR1
	set	pc,  lnk
	
i2c_hw_branch:
	eq	rvb, #0x94		@ Slave Read/Write -- my address recognzd, EV1-SR1,EVF BSY ADSL,#0x94
	beq	i2c_hw_slv_ini
	eq	rvb, #0x98		@ Slave Read  -- new data received,    EV2 - SR1, EVF BSY BTF, #0x98
	beq	i2c_hw_rs_get
	eq	rvb, #0xB8		@ Slave Write -- master requests byte, EV3 - SR1,EVF TRA BSY BTF,#0xB8
	beq	i2c_hw_ws_put
	tst	rvb, #0x1000		@ Slave Write -- NAK received, Tx done,	  EV3-1	- SR2, AF, #0x10
	bne	i2c_hw_ws_end
	tst	rvb, #0x0800		@ Slave Read  -- STOP or re-START received, EV4	- SR2, STOPF, #0x08
	bne	i2c_rs_end
	eq	rvb, #0x93		@ Master Read/Write -- bus now mastered, EV5-SR1,EVF BSY MSL SB,#0x93
	beq	i2c_hw_mst_bus
	tst	rvb, #0x2000		@ Master Read/Write -- slave ackn. address, EV6	- SR2, ENDAD, #0x20
	bne	i2c_hw_mst_ini
	eq	rvb, #0x9A		@ Master Read -- new byte received, EV7	- SR1, EVF BSY BTF MSL, #0x9A
	beq	i2c_hw_rm_get
	eq	rvb, #0xBA		@ Master Write - slav ok to rcv dat, EV8-SR1,EVF TRA BSY BTF MSL,#0xBA
	beq	i2c_wm_put
	set	pc,  lnk

i2c_hw_slv_ini: @ Slave Read/Write -- my address recognized  (EV1)
	tbrfi	rva, sv2, 0		@ r6  <- channel-busy status
	eq	rva, #f			@ is channel free?
	seteq	rva, #i0		@	if so,  r6  <- 0 (scheme int)
	tbstieq rva, sv2, 0		@	if so,  store 0 (scheme int) as channel-busy
	set	rva, #0			@ r6  <- 0
	tbsti	rva, sv2, 4		@ store 0 as number of bytes sent/received
	b	i2cxit

i2c_hw_rs_get:	
	tbrfi	rvb, sv2, 4		@ r7  <- number of bytes sent
	eq	rvb, #0
	tbstieq rvb, sv2, 2		@	if so,  store 0 as data received so far (clear received data)
	bleq	yldon
	b	i2c_rs_get

i2c_hw_ws_put:	
	tbrfi	rvb, sv2, 4		@ r7  <- number of bytes sent
	eq	rvb, #0
	bne	i2c_ws_put
	bl	gldon
	tbrfi	rva, sv2, 0		@ r6  <- channel-busy status
	eq	rva, #i0		@ was channel free at start of transfer?
	tbstieq rva, sv2, 1		@	if so,  store 0 (scheme int) as data-not-ready/#address-bytes
	ldreq	rva, =eof_char		@	if so,  r6  <- eof-character
	streq	rva, [glv, sv1]		@	if so,  store eof-character as object to send
	seteq	rva, #4			@	if so,  r6  <- 4 (raw int) = number of bytes to send
	tbstieq rva, sv2, 3		@	if so,  store 4 as number of bytes to send
	b	i2c_ws_put

i2c_hw_ws_end:	@ Slave Write -- NAK received, Tx done,	  EV3-1	- SR2, AF, #0x10
	ldrb	rvb, [sv3, #i2c_cr]	@ r7  <- current content of I2C[0/1]_CR
	orr	rvb, rvb, #0x02		@ r7  <- contents orred with stop bit
	strb	rvb, [sv3, #i2c_cr]	@ set stop bit
	ldrb	rvb, [sv3, #i2c_cr]	@ r7  <- current content of I2C[0/1]_CR
	bic	rvb, rvb, #0x02		@ r7  <- contents with cleared stop bit
	strb	rvb, [sv3, #i2c_cr]	@ clear stop bit
	b	i2c_ws_end

i2c_hw_mst_bus:	@ Master Read/Write -- bus now mastered (EV5)
	bl	gldon
	tbrfi	rva, sv2, 0		@ r6  <- address of mcu to send data to (scheme int)
	lsr	rva, rva, #1		@ r6  <- mcu-id as int -- note: ends with 0 (i.e. divide by 2)
	strb	rva, [sv3, #i2c_thr]	@ set address of mcu to send data to
	b	i2cxit

i2c_hw_mst_ini: @ Master Read/Write -- slave aknowledged address (EV6)
	ldrb	rvb, [sv3, #i2c_cr]	@ r7  <- current content of I2C[0/1]_CR
	strb	rvb, [sv3, #i2c_cr]	@ re-store contents of cr (to clear THIS interrupt)
	tbrfi	rvb, sv2, 0		@ r6  <- address of mcu to writ/rd dat to/frm (scheme int{w}/float{r})
	tst	rvb, #0x02		@ is this a write operation?
	beq	i2c_wm_ini
	b	i2c_rm_ini
	
hwi2we:	@ set busy status/stop bit at end of write as master
	@ on entry:	sv2 <- i2c[0/1] buffer address
	@ on entry:	sv3 <- i2c[0/1] base address
	@ on entry:	r7  <- #f
	tbrfi	rva, sv2, 3		@ r6  <- number of data bytes to send (raw int)
	eq	rva, #0			@ were we sendng 0 byts (i.e. rdng as mstr & done writing address byt
	seteq	pc,  lnk
	tbsti	rvb, sv2, 0		@ set busy status to #f (transfer done)
	ldrb	rvb, [sv3, #i2c_cr]	@ r7  <- current content of I2C[0/1]_CR
	orr	rvb, rvb, #0x02		@ r7  <- contents orred with stop bit
	strb	rvb, [sv3, #i2c_cr]	@ initiate stop (write stop to I2C[0/1]_CR)
	set	pc,  lnk
	
i2c_hw_rm_get:
	ldrb	rvb, [sv3, #i2c_cr]	@ r7  <- current content of I2C[0/1]_CR
	tst	rvb, #0x04		@ is ack bit asserted?
	bne	i2c_rm_get		@	if so,  jump to perform normal read
	b	i2c_rm_end		@ jump to perform end of read as master (nack was set on prior byte)

hwi2re:	@ set stop bit if needed at end of read-as-master
	ldrb	rvb, [sv3, #i2c_cr]	@ r7  <- current content of I2C[0/1]_CR
	orr	rvb, rvb, #0x06		@ r7  <- contents orred with stop bit and ack bit (reset nak to ack)
	strb	rvb, [sv3, #i2c_cr]	@ initiate stop (write stop to I2C[0/1]_CR)
	set	pc,  lnk
	
hwi2cs:	@ clear SI
	set	pc,  lnk
	
i2cstp:	@ prepare to end Read as Master transfer
	ldrb	rvb, [sv3, #i2c_cr]	@ r7  <- current content of I2C[0/1]_CR
	bic	rvb, rvb, #0x04		@ r7  <- contents with ack bit cleared
	strb	rvb, [sv3, #i2c_cr]	@ set nak in cr
	set	pc,  lnk

i2putp:	@ Prologue:	write additional address bytes to i2c, from buffer or r12 (prologue)
	set	pc,  lnk
	
i2pute:	@ Epilogue:	set completion status if needed (epilogue)
	tbrfi	rva, sv2, 3		@ r6  <- number of data bytes to send (raw int)
	tbrfi	rvb, sv2, 4		@ r7  <- number of data bytes sent (raw int)
	eq	rva, rvb		@ done sending?
	beq	i2c_wm_end		@	if so,  jump to end transfer
	set	pc,  lnk

.endif
	
.ltorg
