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
	.word	puaisr			@ 6:	uart0
	.word	puaisr			@ 7:	uart1
	.word	i0			@ 8
	.word	pi2isr			@ 9:	i2c0 / i2c1 (if included)
	.word	i0			@ 10
	.word	usbisr			@ 11:	USB (if included)
	.word	ptmisr			@ 12:	timer0
	.word	ptmisr			@ 13:	timer1
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


hwinit:	@ pre-set common values
	set	r0,  #0
	set	r1,  #1
	set	r2,  #2
	set	r3,  #3
	set	r4,  #4
	@ initialization of wait states, oscillator, PLL and main clock
	ldr	r6,  =0xFFFFFF60
	ldr	r7,  =0x480100
	str	r7,  [r6]		@ MC_FMR    -- flash wait states to 1, i.e. 2 cyc for Rd, 3 for Wrt
	ldr	r6,  =0xFFFFFD44
	ldr	r7,  =0x8000
	str	r7,  [r6]		@ WDTC_WDMR -- disable watchdog timer
	ldr	r6,  =PMC_base
	ldr	r7,  =0xFF01
	str	r7,  [r6,  #0x20]	@ PMC_MOR   -- 0x0601 == StartCount=6[*8](0x06)(1.5ms), enable=1=0x01
pllwt0:	ldr	r7,  [r6,  #0x68]
	tst	r7,  #0x01
	beq	pllwt0			@ PMC_SR    -- wait for PMC_MOSCS
	ldr	r7,  =PLL_parms
	str	r7,  [r6,  #0x2c]	@ PMC_PLLR  -- 96 MHz
pllwt1:	ldr	r7,  [r6,  #0x68]
	tst	r7,  #0x04
	beq	pllwt1			@ PMC_SR    -- wait for PMC_LOCK
	str	r4,  [r6,  #0x30]	@ PMC_MCKR  -- Set system clock prescaler to 1/2 (PRES_CLK=0x04)
pllwt2:	ldr	r7,  [r6,  #0x68]
	tst	r7,  #0x08
	beq	pllwt2			@ PMC_SR    -- wait for PMC_MCKRDY
	set	r7,  #0x07
	str	r7,  [r6,  #0x30]	@ PMC_MCKR  -- system clock<-PLL/2 (CSS_PLL_CLCK=0x03 | PRES_CLK=0x04)
pllwt3:	ldr	r7,  [r6,  #0x68]
	tst	r7,  #0x08
	beq	pllwt3			@ PMC_SR    -- wait for PMC_MCKRDY
	ldr	r6,  =0xFFFFFD08
	ldr	r7,  =0xA5000401
	str	r7,  [r6]		@ RSTC_RMR  -- enable reset button (1 ms pulse)
	@ initialization of gpio pins
.ifndef	AT91SAM7X @ AT91SAM7S
	ldr	r6,  =PMC_base
	str	r4,  [r6,  #0x10]	@ PMC_PCER -- Enable clock/power for gpio (PIOA)
.else @ AT91SAM7X
	ldr	r6,  =PMC_base		@							 	<RDC>
	ldr	r7,  =0x0C		@								<RDC>
	str	r7,  [r6,  #0x10]	@ PMC_PCER -- Enable clock/power for gpio (PIOA and PIOB)	<RDC>
.endif
	ldr	r6,  =LEDPINSEL
	ldr	r7,  =ALLLED
	str	r7,  [r6]		@ set gpio function for led
	ldr	r6,  =LEDIO
	str	r7,  [r6,  #io_dir]	@ set led as outputs
	@ initialization of UART0 for 9600 8N1 operation
	ldr	r6,  =uart0_gpio	@							       	<RDC>
	set	r7,  #uart0_pins	@								<RDC>
	str	r7,  [r6,  #0x04]	@ PIOA_PDR -- Disable the GPIO for uart0 pins (bits 5,6)
	str	r7,  [r6,  #0x70]	@ PIOA_ASR -- Select uart0 function (Periph A, bits 5,6)
	str	r0,  [r6,  #0x74]	@ PIOA_BSR -- Deselect peripheral B functions
	ldr	r6,  =PMC_base
	set	r7,  #0x40
	str	r7,  [r6,  #0x10]	@ PMC_PCER -- Enable clock/power for uart0 (bit 6)
	ldr	r6,  =uart0_base
	ldr	r7,  =UART0_DIV
	str	r7,  [r6,  #0x20]	@ US0_BRGR -- Set Baud Rate to 9600 (CLOCK/UART0_DIVx16)
	str	r0,  [r6,  #0x28]	@ US0_TTGR -- disable time guard
	ldr	r7,  =0x08C0
	str	r7,  [r6,  #0x04]	@ US0_MR   -- Set mode to 8N1, 16 x Oversampling
	ldr	r7,  =0x0202
	str	r7,  [r6,  #0x0120]	@ US0_PTCR -- Disable DMA transfers
	str	r1,  [r6,  #0x08]	@ US0_IER  -- Enable RxRDY interrupt
	set	r7,  #0x50
	str	r7,  [r6]		@ US0_CR   -- Enable uart0 RX and TX (bits 4, 6)
	@ initialization of interrupts vector for UART0, UART1, twi (i2c), timer0 and timer1
	ldr	r6,  =AIC_SPU
	ldr	r7,  =spuisr
	str	r7,  [r6]		@ AIC_SPU   <- set spuisr as spurious interrupt handler
	ldr	r6,  =AIC_SVR0
	str	r7,  [r6]		@ AIC_SVR0  <- set spuisr as fiq handler
	str	r7,  [r6,  #0x04]	@ AIC_SVR1  <- set spuisr as sys irq handler
	ldr	r7,  =genisr
	str	r7,  [r6,  #0x18]	@ AIC_SVR6  <- set genisr as uart0  isr
	str	r7,  [r6,  #0x1c]	@ AIC_SVR7  <- set genisr as uart1  isr
	str	r7,  [r6,  #0x24]	@ AIC_SVR9  <- set genisr as twi    isr
	str	r7,  [r6,  #0x30]	@ AIC_SVR12 <- set genisr as timer0 isr
	str	r7,  [r6,  #0x34]	@ AIC_SVR13 <- set genisr as timer1 isr
	@ initialization of mcu-id for variables (normally I2c address if slave enabled)
	ldr	r6,  =I2C0ADR		@ r6  <- I2C0 mcu-address address
	set	r7,  #mcu_id
	str	r7,  [r6]		@ I2C0ADR <- set mcu address
	ldr	r6,  =USB_CONF
	str	r0,  [r6]		@ USB_CONF <- USB device is not yet configured

.ifdef	onboard_SDFT
	
  .ifdef sd_is_on_spi

	@ configure pins and SPI0 for SD-card
	@ clock (power-up) the SPI peripheral
	ldr	r6, =PMC_base
	set	r7, #(1 << 5)
	str	r7, [r6, #0x10]
	@ PIOA_OER  <- set SD CS pin as GPIO output
	ldr	r6, =sd_cs_gpio
	set	r7, #sd_cs
	str	r7, [r6, #0x10]	@ set CS pin as output
	@ PIOA_SODR <- de-select SD (set CS high)
	str	r7, [r6, #0x30]	@ set CS pin
	@ PIOA_PDR <- disable GPIO function (PA.11,12,13,14)
	ldr	r6, =sd_spi_gpio
	set	r7, #(0xf << 11)
	str	r7, [r6, #0x04]
	@ PIOA_ASR <-enable Peripheral A function (SPI) (PA.11,12,13,14)
	str	r7, [r6, #0x70]
	@ low-speed (approx 400 KHz)
	ldr	r6, =sd_spi
	set	r7, #0x81
	str	r7, [r6, #0x00]	@ SPI_CR <- reset SPI
	set	r7, #0x01
	str	r7, [r6, #0x00]	@ SPI_CR <- enable SPI
	set	r7, #0x01
	str	r7, [r6, #0x04]	@ SPI_MR <- enable master mode
	set	r7, #0x7800
	orr	r7, r7, #0x02
	str	r7, [r6, #0x30]	@ SPI_CSR0 <- 48 MHz / 120 = 400 KHz, POL/PHA=0

  .endif @ sd_is_on_spi

.endif	@ onboard_SDFT
	
	@ copy FLASH writing code to RAM
	ldr	r6,  =flsRAM			@ sv1 <- start address of flashing code
	ldr	r7,  =flsRND			@ sv5 <- end address of flashing code
	ldr	r9,  =heaptop1			@ sv3 <- RAM target address
	add	r9,  r9, #4
hwiwt6:	ldr	r10, [r6]			@ rva <- next flashing code instruction
	str	r10, [r9]			@ store it in free RAM
	eq	r6,  r7				@ done copying the flashing code?
	addne	r6,  r6,  #4			@	if not, sv1 <- next flashing code source address
	addne	r9,  r9,  #4			@	if not, sv1 <- next flashing code target address
	bne	hwiwt6				@	if not, jump to keep copying flashing code to RAM

.ifdef	native_usb

	@ initialization of USB device controller -- **** MUST BE LAST ****
	ldr	r6,  =USB_LineCoding
	ldr	r7,  =115200
	str	r7,  [r6]
	set	r7,  #0x00080000
	str	r7,  [r6,  #0x04]
	ldr	r6,  =USB_CHUNK
	str	r0,  [r6]
	ldr	r6,  =USB_ZERO
	str	r0,  [r6]
	ldr	r6,  =USB_CONF
	str	r0,  [r6]
	ldr	r6,  =AIC_SVR11
	ldr	r7,  =genisr
	str	r7,  [r6]		@ set usbisr as USB isr
	ldr	r6,  =PMC_base
	set	r7,  #0x0080
	str	r7,  [r6]		@ PMC_SCER  -- enable USB clock
	set	r7,  #0x0800
	str	r7,  [r6,  #0x10]	@ PMC_PCER  -- enable USB (periph #11 = bit 11)
	ldr	r6,  =usb_base
	ldr	r7,  =0xffff
	str	r7,  [r6,  #0x20]	@ UDP_ICR   -- clear USB interrupts
	@ configure control endpoint
	ldr	r6,  =usb_base
	set	r7,  #0x8000
	str	r7,  [r6,  #0x30]	@ UDP_CSR0  -- r7  <- enable, Control endpoint
	set	r7,  #0x0100
	str	r7,  [r6,  #0x08]	@ UDP_FADDR -- enable transfers on address 0
	ldr	r7,  =0xFF0F
	str	r7,  [r6,  #0x10]	@ UDP_IER   -- enable USB interrupts (0-3)
	str	r0,  [r6,  #0x74]	@ UDP_TXVC  -- enable transceiver

  .ifdef SAM7_P256
	@ OLIMEX SAM_P256 requires USB pullups to be set under software control.
	ldr	r6,  =pioa_base		@							<RDC>
	ldr	r7,  =0x10100		@ PA16 and PA8						<RDC>
	str	r7,  [r6,  #0x00]	@ PIO_PER						<RDC>
	str	r7,  [r6,  #0x10]	@ PIO_OER						<RDC>
	str	r7,  [r6,  #0x64]	@ PIO_PUDR						<RDC>
	str	r7,  [r6,  #0x30]	@ PIO_SODR						<RDC>
	set	r7,  #0x10000		@ PA16							<RDC>
	str	r7,  [r6,  #0x34]	@ PIO_CODR						<RDC>
  .endif @ SAM7_P256

.endif	@ native_usb

	@ end of the hardware initialization
	set	pc,  lnk

	
@------------------------------------------------------------------------------------------------
@ AT91_SAM7
@
@	 0- Spurius interrupt handler (if needed)
@	 1- Initialization from FLASH, writing to and erasing FLASH
@	 2- I2C Interrupt routine
@
@------------------------------------------------------------------------------------------------

@
@ 0- Spurius interrupt handler (if needed)
@

spuisr:	@ spurious interrupt handler
	sub	lnk, lnk, #4			@ Adjust lnk to point to return 
	stmdb	sp!, {rva, rvb, lnk}		@ store lnk_irq on irq stack
	ldr	rva, =int_base
	ldr	rvb, [rva,  #int_status]
	str	rvb, [rva,  #int_iccr]
	set	rvb, #0
	str	rvb, [rva,  #int_clear]	
	ldmia	sp!, {rva, rvb, pc}^		@ return
	
@
@ 1- Initialization from FLASH, writing to and erasing FLASH
@

FlashInitCheck: @ return status of flash init enable/override gpio pin (PA3) in r6
	ldr	rva, =flash_int_gpio		@						<RDC>
	ldr	rvb, [rva, #io_pdsr]
	and	rvb, rvb, #flash_init_pin	@ rvb <- status of boot override pin		<RDC>
	set	rva, rvb			@ rva <- status of boot override pin
	set	pc,  lnk

.ifdef older_version
	
wrtfla:	@ write to flash, sv4=r7 is file descriptor, sv2=r5 is page address
libwrt:	@ write to on-chip lib flash (lib shares on-chip file flash)
	swi	run_no_irq				@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk}	@ store scheme registers onto stack
	set	rvb, #24			@ rvb <- 24 = space for 6 flashing code instruction words
	bl	zmaloc				@ rva <- address of free memory
	bic	fre, fre, #0x03			@ fre <- address of free cell for RAM FLASH code
	stmfd	sp!, {fre, cnt, sv1-sv5,  env, dts, glv} @ store scheme registers onto stack
	bl	cpflcd				@ copy FLASH code to RAM
	@ copy buffer data from file descriptor (sv4) (RAM) to AT91SAM7 FLASH buffer (sv2)
	vcrfi	rvb, sv4, 3			@ rvb <- file data source buffer
	add	rvb, rvb, #4			@ rvb <- RAM start address for data in buffer
	set	sv3, #0				@ sv3 <- 0 = start offset
wrtfl0:	cmp	sv3, #F_PAGE_SIZE		@ done writing to flash buffer?
	ldrmi	sv4, [rvb, sv3]			@	if not, sv4 <- next word from data buffer
	strmi	sv4, [sv2, sv3]			@	if not, store next word into flash buffer
	addmi	sv3, sv3, #4			@	if not, sv3 <- next word offset
	bmi	wrtfl0				@	if not, jump to keep copying data to flash buffer
wrtcmt:	@ commit buffer to FLASH using code in RAM
	lsr	sv1, sv2, #8			@ sv1 <- target FLASH page (assumes 256 bytes page size)
	ldr	rvc, =MC_FSR			@ rvc <- address of FLASH status register
	ldr	rvb, =MC_FCR			@ rvb <- address of FLASH command register
	ldr	rva, =0x5A000001		@ rva <- flash write command (page zero)
	orr	rva, rva, sv1, LSL #8		@ rva <- flash write command for page in sv1
	adr	lnk, wrtfxt			@ lnk <- return address for after FLASH command
	set	pc,  fre			@ jump to FLASH write routine in RAM
wrtfxt:	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ restore scheme registers from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk}	@ restore scheme registers from stack
	orr	fre, fre, #0x02			@ fre <- fre-ptr de-reserved
	swi	run_normal			@ enable interrupts (user mode)
	set	pc,  lnk			@ return

.endif
	
wrtfla:	@ write to flash, sv4=r7 is file descriptor, sv2=r5 is page address
libwrt:	@ write to on-chip lib flash (lib shares on-chip file flash)
	set	rvc, sv3			@ rvc <- sv3, saved
	@ copy buffer data from file descriptor (sv4) (RAM) to AT91SAM7 FLASH buffer (sv2)
	vcrfi	sv3, sv4, 3			@ rvb <- file data source buffer
	set	rvb, #F_PAGE_SIZE		@ rvb <- last source offset
wrtfl0:	ldr	rva, [sv3, rvb]			@ rva <- word from data buffer
	subs	rvb, rvb, #4
	str	rva, [sv2, rvb]			@ store word into flash buffer
	bne	wrtfl0				@	if not, jump to keep copying data to flash buffer
	@ disconnect AIC
	ldr	rva, =int_base
	set	rvb, #2
	str	rvb, [rva, #0x38]
	@ commit buffer to FLASH using code in RAM
	lsr	rvb, sv2, #8			@ sv1 <- target FLASH page (assumes 256 bytes page size)
	ldr	rva, =0x5A000001		@ rva <- flash write command (page zero)
	orr	rva, rva, rvb, LSL #8		@ rva <- flash write command for page in sv1
	ldr	rvb, =heaptop1
	add	rvb, rvb, #4
	swi	isr_no_irq
	adr	lnk, wrtfxt			@ lnk <- return address for after FLASH command
	set	pc,  rvb			@ jump to FLASH write routine in RAM
wrtfxt:	@ finish up
	swi	run_normal			@ enable interrupts (user mode)
	@ reconnect AIC
	ldr	rva, =int_base
	set	rvb, #0
	str	rvb, [rva, #0x38]
	set	sv3, rvc			@ sv3 <- restored
	@ wait a bit (recovery?)
	set	rva, #0x6000			@ rva <- wait, approx 1 ms
wrtfwt:	subs	rva, rva, #1
	bne	wrtfwt
	@ check for errors (it that's possible)
	set	rvb, #0
	mvn	rvb, rvb
	bic	rvb, rvb, #0xff
	ldr	rva, [rvb, #0x68]		@ rva <- status
	tst	rva, #0x01			@ FRDY?
	beq	wrterr
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
	@ copy #xffffffff to AT91SAM7 FLASH buffer (sv2)
	set	rvc, lnk			@ rvc <- lnk, saved
	bl	pgsctr				@ rva <- sector number (raw int), of flash page in sv2
	set	lnk, rvc			@ lnk <- restored
	set	rvc, sv2			@ rvc <- sv2, saved
	ldr	rvb, =flashsectors		@ rvb <- address of flash sector table
	ldr	sv2, [rvb, rva, LSL #2]		@ sv2 <- start address of flash sector
ersfl1:	set	rva, #0				@ sv3 <- 0 = start offset
	mvn	rvb, rva			@ sv4 <- erase flash data = 0xFFFFFFFF
ersfl0:	cmp	rva, #F_PAGE_SIZE		@ done writing to flash buffer?
	strmi	rvb, [sv2, rva]			@	if not, store next word into flash buffer
	addmi	rva, rva, #4			@	if not, sv3 <- next word offset
	bmi	ersfl0				@	if not, jump to keep copying data to flash buffer
	@ commit buffer to FLASH using code in RAM
	lsr	rvb, sv2, #8			@ sv1 <- target FLASH page (assumes 256 bytes page size)
	ldr	rva, =0x5A000001		@ rva <- flash write command (page zero)
	orr	rva, rva, rvb, LSL #8		@ rva <- flash write command for page in sv1
	ldr	rvb, =heaptop1
	add	rvb, rvb, #4
	swi	isr_no_irq
	adr	lnk, ersfxt			@ lnk <- return address for after FLASH command
	set	pc,  rvb			@ jump to FLASH write routine in RAM
ersfxt:	@ finish up or jump to erase next page of sector
	swi	run_normal			@ enable interrupts (user mode)
	add	sv2, sv2, #F_PAGE_SIZE		@ sv2 <- next page address
	ldr	rvb, =0x0FFF
	ands	rvb, rvb, sv2			@ done erasing sector? (4kb = 16 pages of 256 bytes)
	bne	ersfl1
	set	sv2, rvc			@ sv2 <- restored
	set	pc,  lnk			@ return

@flsRAM:	@ code to be copied to RAM, at fre+, such that execution is from RAM while
@	@ FLASH is being written. i.e. do: (1) ldr lnk, =xyz (2) set pc, fre to initiate
@	set	rvb, #0
@	mvn	rvb, rvb
@	bic	rvb, rvb, #0xff
@	str	rva, [rvb, #0x64]		@ perform FLASH write
@	ldr	rva, [rvb, #0x68]		@ get status
@	tst	rva, #0x01			@ FRDY?
@	subeq	pc,  pc,  #16			@	if not, jump back to get status
@	set	pc,  lnk			@ return
@flsRND: @ end of ram code

flsRAM:	@ code to be copied to RAM, at fre+, such that execution is from RAM while
	@ FLASH is being written. i.e. do: (1) ldr lnk, =xyz (2) set pc, fre to initiate
	set	rvb, #0
	mvn	rvb, rvb
	bic	rvb, rvb, #0xff
	str	rva, [rvb, #0x64]		@ perform FLASH write
	@ wait for completion or timeout
	set	rva, #0				@ rva <- 0, for timeout
	set	rvb, #0
	mvn	rvb, rvb
	bic	rvb, rvb, #0xff
	ldr	rvb, [rvb, #0x68]		@ get status
	tst	rvb, #0x01			@ FRDY?
	addeq	rva, rva, #1
	tsteq	rva, #0x800000			@ rva <- timeout, approx 1 sec
	subeq	pc,  pc,  #36			@	if not, jump back to get status
	set	pc,  lnk			@ return
flsRND: @ end of ram code


flashsectors:	@ 64 x 4kB sectors (AT91SAM7 MCU doesn't use sectors though)
lib_sectors:	@ lib shares on-chip file flash
.word	0x000000, 0x001000, 0x002000, 0x003000, 0x004000, 0x005000, 0x006000, 0x007000
.word	0x008000, 0x009000, 0x00A000, 0x00B000, 0x00C000, 0x00D000, 0x00E000, 0x00F000
.word	0x010000, 0x011000, 0x012000, 0x013000, 0x014000, 0x015000, 0x016000, 0x017000
.word	0x018000, 0x019000, 0x01A000, 0x01B000, 0x01C000, 0x01D000, 0x01E000, 0x01F000
.word	0x020000, 0x021000, 0x022000, 0x023000, 0x024000, 0x025000, 0x026000, 0x027000
.word	0x028000, 0x029000, 0x02A000, 0x02B000, 0x02C000, 0x02D000, 0x02E000, 0x02F000
.word	0x030000, 0x031000, 0x032000, 0x033000, 0x034000, 0x035000, 0x036000, 0x037000
.word	0x038000, 0x039000, 0x03A000, 0x03B000, 0x03C000, 0x03D000, 0x03E000, 0x03F000, 0x040000, 0x0FFFFFFC


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
	set	rvb, #0x81
	str	rvb, [rva, #0x00]	@ SPI_CR <- reset SPI
	set	rvb, #0x01
	str	rvb, [rva, #0x00]	@ SPI_CR <- enable SPI
	set	rvb, #0x01
	str	rvb, [rva, #0x04]	@ SPI_MR <- enable master mode
	set	rvb, #0x0300
	orr	rvb, rvb, #0x02
	str	rvb, [rva, #0x30]	@ SPI_CSR0 <- 48 MHz / 3 = 16 MHz, POL/PHA=0
	set	pc,  lnk

_func_	
sd_slo:	@ configure spi speed (low), phase, polarity
	@ modifies:	rva, rvb
	ldr	rva, =sd_spi
	set	rvb, #0x81
	str	rvb, [rva, #0x00]	@ SPI_CR <- reset SPI
	set	rvb, #0x01
	str	rvb, [rva, #0x00]	@ SPI_CR <- enable SPI
	set	rvb, #0x01
	str	rvb, [rva, #0x04]	@ SPI_MR <- enable master mode
	set	rvb, #0x7800
	orr	rvb, rvb, #0x02
	str	rvb, [rva, #0x30]	@ SPI_CSR0 <- 48 MHz / 120 = 400 KHz, POL/PHA=0
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
sd_get:	@ sd-spi get sub-routine
	@ modifies:	rva, rvb
	set	rvb, #0xff
_func_	
sd_put:	@ sd-spi put sub-routine
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
	and	rvb, rvb, #0xff
	set	pc, lnk

  .endif @ sd_is_on_spi

.endif	@ 	onboard_SDFT
	
@
@ 2- I2C hardware and Interrupt routines
@

.ifdef	include_i2c

hwi2cr:	@ write-out additional address registers, if needed
	@ modify interupts, as needed
	@ on entry:	sv5 <- i2c[0/1]buffer
	@ on entry:	r6  <- i2c[0/1] base address (also I2CONSET)
	@ interrupts are disabled throughout
	set	pc,  lnk
	
hwi2ni:	@ initiate i2c read/write, as master
	@ on entry:	r6  <- i2c base address
	ldr	rvb, =0x107		@ r7  <- NACK, TXRDY, RXRDY and TXCOMP
	str	rvb, [rva, #i2c_iclear]	@ disable all TWI interrupts
	set	rvb, #0x05		@ r7  <- TXRDY and TXCOMP
	str	rvb, [rva, #i2c_ienable] @ enable TXRDY and TXCOMP interrupts
	set	rvb, #4			@ r7  <- TWI enable bit
	str	rvb, [rva, #i2c_ctrl]	@ start transfer
	set	pc,  lnk

hwi2st:	@ get i2c interrupt status and base address
	@ on exit:	r6 <- i2c[0/1] base address
	@ on exit:	r7 <- i2c interrupt status
	ldr	rvb, [rva, #i2c_status]	@ r7  <- current status of TWI interface
	ldr	rva, [rva, #i2c_imask]	@ r6  <- TWI enabled Interrupt Mask
	and	rvb, rvb, rva		@ r7  <- asserted TWI interrupts (without potential spurious bits)
	ldr	rva, =i2c0_base		@ r6  <- address of Status Register (restored)
	set	pc,  lnk

hwi2cs:	@ clear SI
	set	pc,  lnk

i2c_hw_branch:	@ process interrupt
	eq	rvb, #0x05		@ Writing or Reading as Master -- bus mastered (txrdy and txcomp set)
	beq	i2c_hw_mst_bus
	tst	rvb, #0x0100		@ Writing or Reading as Master -- NAK received --  re-send byte
	bne	i2cnak
	tst	rvb, #0x04		@ Writing as Master -- slave ok to receive data (txrdy set)
	bne	i2c_wm_put
	tst	rvb, #0x02		@ Reading as Master -- new byte received (rxrdy set)
	bne	i2c_rm_get
	tst	rvb, #0x01		@ Writing or Reading as Master  -- transmission complete (txcomp set)
	bne	i2c_mst_end
	set	pc,  lnk

i2c_hw_mst_bus:	@ Reading or Writing as Master -- bus now mastered
	@ on entry:	sv1 <- i2c[0/1] data offset in glv
	@ on entry:	sv2 <- i2c[0/1] buffer address
	@ on entry:	sv3 <- i2c[0/1] base address
	set	rvb, #0			@ r7  <- 0, number of bytes sent/received so far
	tbsti	rvb, sv2, 4		@ store number of bytes sent/received in i2c buffer
	@ store internal address bytes in TWI_IADR 
	set	rva, #i2c_iadr		@ r6  <- 0, offset to internal address in TWI_IADR
	tbrfi	sv4, sv2, 1		@ sv4 <- number of internal address bytes (scheme int)
	add	sv4, sv4, #0x20		@ sv4 <- additional number of address bytes (scheme int)
i2str0:	eq	sv4, #0x21		@ are we done writing additional address bytes?
	subne	sv4, sv4, #4		@	if not, sv4 <- address byte offst in i2cbuffer[8] (scheme int)
	ldrbne	rvb, [sv2, sv4, LSR #2]	@	if not, r7  <- address byte from i2cbuffer[8+offset]
	strbne	rvb, [sv3, rva]		@ 	if not, store next internal address byte in TWO_IADR
	addne	rva, rva,#1		@	if not, r6  <- offset to next internal address in TWI_IADR
	bne	i2str0			@	if not, jump to store next internal address byte
	@ set TWI_MMR to write/read to/from i2c address with appropriate number of internal address bytes
	tbrfi	rvb, sv2, 0		@ r7  <- address of mcu to wrt/rd dat to/frm (scheme int{w}/float{r})
	tst	rvb, #0x02		@ is this a write operation?
	seteq	rva, #0x0000		@	if so,  r6  <- TWI r/w bit set to write, & address of target
	setne	rva, #0x1000		@	if not, r6  <- TWI r/w bit set to read, & address of target
	lsr	rvb, rvb, #2		@
	orr	rva, rva, rvb, LSL #16	@
	tbrfi	rvb, sv2, 1		@ r7  <- number of internal address bytes (scheme int)
	lsr	rvb, rvb, #2		@ r7  <- number of internal address bytes (raw int)
	orr	rva, rva, rvb, LSL #8	@ r6  <- r/w and #internal address bytes
	str	rva, [sv3, #i2c_mode]	@ set r/w bit, #internal address bytes and target address in TWI MMR
	ldr	rvb, =0x107		@ r7  <- NACK, TXRDY, RXRDY and TXCOMP
	str	rvb, [sv3, #i2c_iclear]	@ deactivate interrupts
	beq	i2strw			@	if so,  jump to start a write
	@ start an i2c read
	tbrfi	rva, sv2,  3		@ r6  <- number of bytes to read
	cmp	rva, #2			@ are we reading just 1 byte?
	setmi	rvb, #1			@	if so,  r7  <- TXCOMP bit
	setpl	rvb, #2			@	if not, r7  <- TWI RXRDY bit
	str	rvb, [sv3, #i2c_ienable] @ enable TWI RXRDY interrupt
	tbrfi	rva, sv2,  3		@ r6  <- number of bytes to send/read
	cmp	rva, #2			@ are we reading just 1 byte?
	setmi	rvb, #3			@	if so,  r7  <- stop and start bits
	setpl	rvb, #1			@	if not, r7  <- start bit
	str	rvb, [sv3, #i2c_ctrl]	@ start transfer
	bl	gldon			@ turn led on
	b	i2cxit			@ exit
i2strw:	@ start an i2c write
	tbrfi	rva, sv2,  3		@ r6  <- number of bytes to send
	cmp	rva, #2			@ are we sending just 1 byte?
	setmi	rvb, #1			@	if so,  r7  <- TWI TXCOMP bit
	setpl	rvb, #4			@	if not, r7  <- TWI TXRDY bit
	orrpl	rvb, rvb, #0x0100	@	if not, r7  <- TXRDY and NAK bits
	str	rvb, [sv3, #i2c_ienable] @ enable TWI TXCOMP OR TXRDY interrupt
	bl	i2putc			@ jump to write 1st byte
	tbrfi	rva, sv2,  3		@ r6  <- number of bytes to send
	cmp	rva, #2			@ are we sending just 1 byte?
	setmi	rvb, #3			@	if so,  r7  <- stop and start bits
	strmi	rvb, [sv3, #i2c_ctrl]	@	if so,  start transfer
	bl	gldon			@ turn led on
	b	i2cxit
	
i2putp:	@ Prologue:	write additional address bytes to i2c, from buffer or r12
	set	pc,  lr

i2pute:	@ Epilogue:	set completion status if needed
	tbrfi	rva, sv2, 3		@ r6  <- number of data bytes to send (raw int)
	tbrfi	rvb, sv2, 4		@ r7  <- number of data bytes sent (raw int)
	eq	rva, rvb		@ done sending?
	beq	i2cstp			@	if so,  jump to end transfer
	set	pc,  lnk

i2cnak:	@ re-send last byte
	tbrfi	rvb, sv2, 4		@ r7  <- number of data bytes sent (raw int)
	sub	rvb, rvb, #1
	tbsti	rvb, sv2, 4		@ r7  <- number of data bytes sent (raw int)
	b	i2c_wm_put
	
i2cstp:	@ NAK received or just 1 byte left to read, set stop bit
	@ note how this is also the bottom of i2pute, above
	ldr	rvb, =0x107		@ r7  <- NACK, TXRDY, RXRDY and TXCOMP
	str	rvb, [sv3, #i2c_iclear]	@ disable TWI interrupts
	set	rvb, #1			@ r7  <- TXCOMP bit
	str	rvb, [sv3, #i2c_ienable] @ enable TWI TXCOMP interrupt
	set	rvb, #2			@ r7  <- stop bit
	str	rvb, [sv3, #i2c_ctrl]	@ set stop transfer
	set	pc,  lnk

i2c_mst_end:	@ txcomp received
	tbrfi	rvb, sv2, 0		@ rvb <- address of mcu to wrt/rd data to/frm (scheme int{w}/float{r})
	tst	rvb, #0x02		@ is this a write operation?
	beq	i2c_wm_end
	b	i2c_rm_end

hwi2we:	@ set busy status/stop bit at end of write as master
	@ on entry:	sv2 <- i2c[0/1] buffer address
	@ on entry:	sv3 <- i2c[0/1] base address
	@ on entry:	r7  <- #f
	tbsti	rvb, sv2, 0		@ set busy status to #f (transfer done)
	ldr	rvb, =0x107		@ r7  <- NACK, TXRDY, RXRDY and TXCOMP
	str	rvb, [sv3, #i2c_iclear]	@ disable TWI interrupts
	set	pc,  lnk
	
hwi2re:	@ set stop bit if needed at end of read-as-master
	@ on entry:	sv3 <- i2c[0/1] base address
	ldr	rvb, =0x107		@ r7  <- NACK, TXRDY, RXRDY and TXCOMP
	str	rvb, [sv3, #i2c_iclear]	@ disable TWI interrupts
	set	pc,  lnk

.endif

.ltorg

