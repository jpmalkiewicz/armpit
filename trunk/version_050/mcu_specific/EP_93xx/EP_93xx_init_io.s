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
	.word	ptmisr			@ 4:	timer0
	.word	ptmisr			@ 5:	timer1
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
	.word	puaisr			@ 23:	uart0
	.word	i0			@ 24
	.word	puaisr			@ 25:	uart1
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
	.word	i0			@ 37
	.word	i0			@ 38
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
	.word	pi2isr			@ 63:	i2c0/1 (if included)


hwinit:	@ pre-set common values
	set	r0,  #0
	@ configure interrupts
	ldr	r7,  =genisr
	ldr	r10, =int_base
	str	r0,  [r10, #0x0c]	@ VIC1IntSelect <- all interrupts are IRQ
	str	r7,  [r10, #0x34]	@ VIC1VectDefAddr <- default ISR
	add	r11, r10, #0x010000
	str	r0,  [r11, #0x0c]	@ VIC2IntSelect <- all interrupts are IRQ
	str	r7,  [r11, #0x34]	@ VIC2VectDefAddr <- default ISR
	@ initialization of UART0 for 9600 8N1 operation
	ldr	r6,  =0x80930000
	set	r7,  #0xaa
	str	r7,  [r6,  #0xc0]	@ sysSWLock <- unlock DeviceCfg register
	set	r7,  #0x00040000	@ enable UART0 (aka uart1)
	orr	r7,  r7, #0x00000800	@ GPIO H pins are GPIO (not IDE) -- for FlashInitCheck Button
.ifdef	hardware_FPU
	@ power-up the Maverick Crunch co-processor
	orr	r7, r7, #0x00800000
.endif	
	str	r7,  [r6,  #0x80]	@ DeviceCfg enable/power-up UART1 (and FPU, if needed)
	ldr	r6,  =uart0_base
	ldr	r7,  =UART0_DIV_L
	str	r7,  [r6,  #0x10]	@ Uart1LinCtrlLow <- 47 (low divisor for 9600 baud)
	ldr	r7,  =UART0_DIV_H
	str	r7,  [r6,  #0x0c]	@ Uart1LinCtrlMid <- 0  (high divisor for 9600 baud)
	set	r7,  #0x60
	str	r7,  [r6,  #0x08]	@ Uart1LinCtrlHigh <- 8, N, 1, no FIFO
	str	r0,  [r6,  #0x1c]	@ Uart1IntIDIntClr <- clear those clearable UART interrupts
	set	r7,  #0x11
	str	r7,  [r6,  #0x14]	@ Uart1Ctrl <- enable uart1 and Rx interrupt
	set	r7,  #0x00800000
	str	r7,  [r10, #0x10]	@ VIC1IntEnable <- bit 23 = UART1 RXINTR1
	set	r7,  #0x00100000
	str	r7,  [r11, #0x10]	@ VIC2IntEnable <- bit 52 overall = INT_UART1
	@ initialization of SD card pins
.ifdef	onboard_SDFT	
  .ifdef sd_is_on_spi
	@ configure spi speed (low), phase, polarity
	ldr	r6,  =sd_spi
	set	r7,  #0x10
	str	r7,  [r6, #0x04]	@ SSPCR1  <- enable SPI
	set	r7,  #40
	str	r7,  [r6, #0x10]	@ SSPCPSR <- 1st prescaler = 40
	set	r7,  #7
	str	r7,  [r6, #0x00]	@ SSPCR0  <- 7.4 MHz/40 = 185KHz, 8-bit, PH/POL=0
	set	r7,  #0x00
	str	r7,  [r6, #0x04]	@ SSPCR1  <- disable SPI
	set	r7,  #0x10
	str	r7,  [r6, #0x04]	@ SSPCR1  <- enable SPI
	@ configure chip-select pin as gpio out, and de-select sd card
	ldr	r6,  =sd_cs_gpio
	and	r7,  r6, #0xff
	cmp	r7,  #0x10
	ldrmi	r7,  [r6, #io_dir]
	ldrpl	r7,  [r6, #io_dir_high]
	orr	r7,  r7, #sd_cs
	strmi	r7,  [r6, #io_dir]	@ sd CS pin set as gpio out
	strpl	r7,  [r6, #io_dir_high]	@ sd CS pin set as gpio out
	ldr	r7,  [r6, #io_state]
	orr	r7,  r7, #sd_cs
	str	r7,  [r6, #io_state]	@ set sd_cs pin to de-select sd card
  .endif  @ sd_is_on_spi
.endif	@ onboard_SDFT
	@ I2C and USB
	ldr	r6,  =I2C0ADR		@ r6  <- I2C0ADR
	set	r7,  #mcu_id
	str	r7,  [r6]		@ I2C0ADR <- set mcu address
	ldr	r6,  =USB_CONF
	str	r0,  [r6]		@ USB_CONF <- USB device is not yet configured
.ifdef	hardware_FPU
	@ set default rounding mode to truncate
	cfmv32sc mvdx0, dspsc		@ mvdx0 <- rounding mode from DSPSC
	cfmvr64l rvb, mvdx0		@ rvb   <- rounding mode
	bic	rvb, rvb, #0x0c00	@ clear rounding mode
	orr	rvb, rvb, #0x0400	@ rounding mode = towards zero (i.e. truncate = default)
	cfmv64lr mvdx0, rvb		@ mvdx0 <- new rounding mode
	cfmvsc32 dspsc, mvdx0		@ set rounding mode in DSPSC
.endif	
	@ end of the hardware initialization
	set	pc,  lnk


@------------------------------------------------------------------------------------------------
@  EP_93xx
@
@	 1- Initialization from FLASH, writing to and erasing FLASH
@	 2- I2C Interrupt routine
@
@------------------------------------------------------------------------------------------------
	
@
@ 1- Initialization from FLASH, writing to and erasing FLASH
@

FlashInitCheck: @ return status of flash init enable/override gpio pin GPIO H pin 4 = BUT button in rva
	ldr	rvb, =ioH_base			@ rvb <- port address
	ldr	rva, [rvb, #io_state]		@ rva <- state of GPIO H pins
	and	rva, rva, #(1 << 4)
	set	pc,  lnk			@ return
	
wrtfla:	@ write to flash, sv2 is page address, sv4 is file descriptor
	swi	run_no_irq			@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk}	@ store scheme registers onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5,  env, dts, glv} @ store scheme registers onto stack
	@ copy buffer data from file descriptor (sv4) (RAM) to FLASH buffer (sv2)
	vcrfi	sv3, sv4, 3			@ sv3 <- buffer address	
	add	sv4, sv2, #F_PAGE_SIZE		@ sv4 <- end target address
wrtfl0:	bl	pgsctr				@ rva <- sector number (raw int), from page address in r5
	adr	rvb, flashsectors		@ rvb <- address of flash sector table
	ldr	sv1, [rvb, rva, LSL #2]		@ sv1 <- address of flash page block start
	@ initiate write-buffer to FLASH
flwrw1:	set	rva, #0xe8			@ rva <- CFI write-buffer command code
	strh	rva, [sv1]			@ initiate write-buffer
	ldrh	rva, [sv1]			@ rva <- FLASH device status
	tst	rva, #0x80			@ is FLASH ready?
	beq	flwrw1				@	if not, jump to keep waiting
	@ set count and transfer data to FLASH write-buffer
	set	rva, #0x1f			@ rva <- 32 bytes to write
	strh	rva, [sv1]			@ set number of bytes to write in CFI controller
	ldmib	sv3!, {fre,cnt,rva,rvb,sv5,env,dts,glv}	@ get next eight source data words
	stmia	sv2,  {fre,cnt,rva,rvb,sv5,env,dts,glv}	@ store data words in FLASH write-buffer
	stmia	sv2!, {fre,cnt,rva,rvb,sv5,env,dts,glv}	@ store data AGAIN (cfi seems to expect 16x2 writes)
	@ commit write-buffer to FLASH
	set	rva, #0xd0			@ rva <- CFI confirm write-buffer command code
	strh	rva, [sv1]			@ confirm write-buffer command
flwrw2:	ldrh	rva, [sv1]			@ rva <- FLASH device status
	tst	rva, #0x80			@ is FLASH ready?
	beq	flwrw2				@	if not, jump to keep waiting
	set	rva, #0x50			@ rva <- CFI Clear Status Register command code
	strh	rva, [sv1]			@ clear the status register
	cmp	sv2, sv4			@ done writing?
	bmi	wrtfl0				@	if not, jump to keep writing data to flash
	set	rva, #0xff			@ rva <- CFI Read Array command code
	strh	rva, [sv1]			@ set FLASH to read array mode
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ restore scheme registers from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk}	@ restore scheme registers from stack
	orr	fre, fre, #0x02			@ fre <- fre-ptr de-reserved
	swi	run_normal			@ enable interrupts (user mode)
	set	pc,  lnk			@ return

ersfla:	@ erase flash sector that contains page address in sv2
	swi	run_no_irq			@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk}	@ store scheme registers onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5,  env, dts, glv} @ store scheme registers onto stack
	@ prepare flash sector for write
	bl	pgsctr				@ rva <- sector number (raw int), from page address in sv2
	adr	rvb, flashsectors		@ rvb <- address of flash sector table
	ldr	sv1, [rvb]			@ sv1 <- start address of whole FLASH (controller)
	ldr	sv2, [rvb, rva, LSL #2]		@ sv2 <- address of flash block start
	@ unlock block to be erased (unlocks all blocks really it seems)
	set	rva, #0x60			@ rva <- CFI unlock block command code
	strh	rva, [sv1]			@ initiate block unlock
	set	rva, #0xd0			@ rva <- CFI confirm unlock command code
	strh	rva, [sv1]			@ confirm block unlock
flrdw0:	ldrh	rva, [sv1]			@ rva <- FLASH device status
	tst	rva, #0x80			@ is FLASH ready?
	beq	flrdw0				@	if not, jump to keep waiting
	set	rva, #0x50			@ rva <- CFI Clear Status Register command code
	strh	rva, [sv1]			@ clear the status register
	@ erase block whose address starts at sv2
	set	rva, #0x20			@ rva <- CFI erase block command code
	strh	rva, [sv2]			@ initiate erase block
	set	rva, #0xd0			@ rva <- CFI confirm erase command code
	strh	rva, [sv2]			@ confirm erase block
flrdwt:	ldrh	rva, [sv1]			@ rva <- FLASH device status
	tst	rva, #0x80			@ is FLASH ready?
	beq	flrdwt				@	if not, jump to keep waiting
	set	rva, #0x50			@ rva <- CFI Clear Status Register command code
	strh	rva, [sv1]			@ clear the status register
	set	rva, #0xff			@ rva <- CFI Read Array command code
	strh	rva, [sv1]			@ set FLASH to read array mode
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ restore scheme registers from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk}	@ restore scheme registers from stack
	orr	fre, fre, #0x02			@ fre <- fre-ptr de-reserved
	swi	run_normal			@ enable interrupts (user mode)
	set	pc,  lnk			@ return


.ifdef CS_E9302
flashsectors:	@ 128 x 128KB FLASH sectors of Intel JS28F128 J3D75 on CS-EP9302 board
.word	0x60000000, 0x60020000,  0x60040000,  0x60060000,  0x60080000,  0x600A0000,  0x600C0000,  0x600E0000
.word	0x60100000, 0x60120000,  0x60140000,  0x60160000,  0x60180000,  0x601A0000,  0x601C0000,  0x601E0000
.word	0x60200000, 0x60220000,  0x60240000,  0x60260000,  0x60280000,  0x602A0000,  0x602C0000,  0x602E0000
.word	0x60300000, 0x60320000,  0x60340000,  0x60360000,  0x60380000,  0x603A0000,  0x603C0000,  0x603E0000
.word	0x60400000, 0x60420000,  0x60440000,  0x60460000,  0x60480000,  0x604A0000,  0x604C0000,  0x604E0000
.word	0x60500000, 0x60520000,  0x60540000,  0x60560000,  0x60580000,  0x605A0000,  0x605C0000,  0x605E0000
.word	0x60600000, 0x60620000,  0x60640000,  0x60660000,  0x60680000,  0x606A0000,  0x606C0000,  0x606E0000
.word	0x60700000, 0x60720000,  0x60740000,  0x60760000,  0x60780000,  0x607A0000,  0x607C0000,  0x607E0000
.word	0x60800000, 0x60820000,  0x60840000,  0x60860000,  0x60880000,  0x608A0000,  0x608C0000,  0x608E0000
.word	0x60900000, 0x60920000,  0x60940000,  0x60960000,  0x60980000,  0x609A0000,  0x609C0000,  0x609E0000
.word	0x60A00000, 0x60a20000,  0x60a40000,  0x60a60000,  0x60a80000,  0x60aA0000,  0x60aC0000,  0x60aE0000
.word	0x60B00000, 0x60b20000,  0x60b40000,  0x60b60000,  0x60b80000,  0x60bA0000,  0x60bC0000,  0x60bE0000
.word	0x60C00000, 0x60c20000,  0x60c40000,  0x60c60000,  0x60c80000,  0x60cA0000,  0x60cC0000,  0x60cE0000
.word	0x60D00000, 0x60d20000,  0x60d40000,  0x60d60000,  0x60d80000,  0x60dA0000,  0x60dC0000,  0x60dE0000
.word	0x60E00000, 0x60e20000,  0x60e40000,  0x60e60000,  0x60e80000,  0x60eA0000,  0x60eC0000,  0x60eE0000
.word	0x60F00000, 0x60f20000,  0x60f40000,  0x60f60000,  0x60f80000,  0x60fA0000,  0x60fC0000,  0x60fE0000
.word	0x61000000
.endif	@ .ifdef CS_E9302
	
@------------------------------------------------------------------------------------------------
@
@ 2- SD card low-level interface
@
@------------------------------------------------------------------------------------------------

.ifdef	onboard_SDFT
	
  .ifdef sd_is_on_spi

_func_	
sd_cfg:	@ configure spi speed (high), phase, polarity
	ldr	rva, =sd_spi
	ldr	rvb, [rva, #0x00]
	eq	rvb, #7
	ldreq	rvb, [rva, #0x10]
	eqeq	rvb, #2
	ldreq	rvb, [rva, #0x04]
	eqeq	rvb, #0x10
	setne	rvb, #0x10
	strne	rvb, [rva, #0x04]	@ SSPCR1  <- enable SPI
	setne	rvb, #2
	strne	rvb, [rva, #0x10]	@ SSPCPSR <- 1st prescaler = 2
	setne	rvb, #7
	strne	rvb, [rva, #0x00]	@ SSPCR0  <- 7.4 MHz/2, 8-bit, PH/POL=0
	setne	rvb, #0x00
	strne	rvb, [rva, #0x04]	@ SSPCR1  <- disable SPI
	setne	rvb, #0x10
	strne	rvb, [rva, #0x04]	@ SSPCR1  <- enable SPI
	set	pc,  lnk

_func_	
sd_slo:	@ configure spi speed (low), phase, polarity
	ldr	rva, =sd_spi
	ldr	rvb, [rva, #0x00]
	eq	rvb, #7
	ldreq	rvb, [rva, #0x10]
	eqeq	rvb, #40
	ldreq	rvb, [rva, #0x04]
	eqeq	rvb, #0x10
	setne	rvb, #0x10
	strne	rvb, [rva, #0x04]	@ SSPCR1  <- enable SPI
	setne	rvb, #40
	strne	rvb, [rva, #0x10]	@ SSPCPSR <- 1st prescaler = 40
	setne	rvb, #7
	strne	rvb, [rva, #0x00]	@ SSPCR0  <- 7.4 MHz/40 = 185KHz, 8-bit, PH/POL=0
	setne	rvb, #0x00
	strne	rvb, [rva, #0x04]	@ SSPCR1  <- disable SPI
	setne	rvb, #0x10
	strne	rvb, [rva, #0x04]	@ SSPCR1  <- enable SPI
	set	pc,  lnk

_func_	
sd_sel:	@ select SD-card subroutine
	ldr	rva, =sd_cs_gpio
	ldr	rvb, [rva, #io_state]
	bic	rvb, rvb, #sd_cs
	str	rvb, [rva, #io_state]	@ clear-pin
	set	pc,  lnk
	
_func_	
sd_dsl:	@ de-select SD-card subroutine
	ldr	rva, =sd_cs_gpio
	ldr	rvb, [rva, #io_state]
	orr	rvb, rvb, #sd_cs
	str	rvb, [rva, #io_state]	@ set-pin
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

  .endif @ sd_is_on_spi


.endif	@ onboard_SDFT

	
@------------------------------------------------------------------------------------------------
@  EP_93xx
@
@ 2- I2C Interrupt routine
@
@------------------------------------------------------------------------------------------------

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
