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
	.word	ptmisr			@ 10:	timer0
	.word	ptmisr			@ 11:	timer1
	.word	i0			@ 12
	.word	i0			@ 13
	.word	i0			@ 14
	.word	puaisr			@ 15:	uart1
	.word	i0			@ 16
	.word	i0			@ 17
	.word	i0			@ 18
	.word	i0			@ 19
	.word	i0			@ 20
	.word	i0			@ 21
	.word	i0			@ 22
	.word	i0			@ 23
	.word	i0			@ 24
	.word	usbisr			@ 25:	USB device (if included)
	.word	i0			@ 26
	.word	pi2isr			@ 27:	i2c0/1 (if included)
	.word	puaisr			@ 28:	uart0
	.word	i0			@ 29
	.word	i0			@ 30
	.word	i0			@ 31


hwinit:	@ configure LED
	ldr	rvb, = io0_base
	set	rva, #0x01
	str	rva, [rvb, #0x08]	@ disable pull-up on GPIO F, pin 0 (GPF0)
	str	rva, [rvb]		@ set output function for GPIO F, pin 0 (GPF0)
	@ initialization of UART0 for 9600 8N1 operation
	ldr	rva, = 0x56000070	@ rva <- GPHCON (uart0 on Port H)
	set	rvb, #0x0c
	str	rvb, [rva, #0x08]	@ disable pull-up on pins GPH 2, 3
	set	rvb, #0xa0
	str	rvb, [rva, #0x00]	@ enable uart0 Tx Rx function for pins GPH 2, 3
	ldr	rva, =uart0_base
	set	rvb, #0x03
	str	rvb, [rva, #0x00]	@ ULCON0 <- 8, N, 1
	set	rvb, #0x00
	str	rvb, [rva, #0x08]	@ UFCON0 <- no FIFO
	ldr	rvb, =UART0_DIV
	str	rvb, [rva, #0x28]	@ UBRDIV0 <- 329 (divisor for 9600 baud)
	set	rvb, #0x45
	str	rvb, [rva, #0x04]	@ UCON0 <- enable Tx and Rx, and error interrupt
	@ initialization of SD card pins
.ifdef	onboard_SDFT	
  .ifdef sd_is_on_spi
	@ configure spi speed (low), phase, polarity
	ldr	rva, =sd_spi
	set	rvb, #63
	str	rvb, [rva, #0x0c]	@ sppre <- 60MHz/2/(63+1) ~= 400KHz
	set	rvb, #0x18
	str	rvb, [rva, #0x00]	@ spcon (control) <- CLKen, master, POL=PHA=0
	@ configure chip-select pin as gpio out, and de-select sd card
	ldr	rva, =sd_cs_gpio
	ldr	rvb, [rva]
	bic	rvb, rvb, #0x0c
	orr	rvb, rvb, #0x04
	str	rvb, [rva]		@ gpio_H <- sd_cs pin configured as gpio output
	ldr	rvb, [rva, #io_state]
	orr	rvb, rvb, #sd_cs
	str	rvb, [rva, #io_state]	@ set sd_cs pin to de-select sd card
	@ configure other spi pins: gpio_G.5,6,7 as SPI (cfg = #b11)
	ldr	rva, =sd_spi_gpio
	ldr	rvb, [rva]
	orr	rvb, rvb, #0xfc00
	str	rvb, [rva]		@ gpio_G <- pins 5,6,7 configured as SPI
  .endif  @ sd_is_on_spi
.endif	@ onboard_SDFT
	@ I2C and USB
	ldr	rva, =I2C0ADR		@ r6  <- I2C0ADR
	set	rvb, #mcu_id
	str	rvb, [rva]		@ I2C0ADR <- set mcu address
	ldr	rva, =USB_CONF
	set	rvb, #0x00
	str	rvb, [rva]		@ USB_CONF <- USB device is not yet configured
	@ initialize interrupts
	ldr	rva, =int_base
	ldr	rvb, =0x07ff
	str	rvb, [rva, #0x18]	@ clear sub-source-pending ints in SUBSRCPND
	set	rvb, #0x00
	mvn	rvb, rvb
	str	rvb, [rva, #0x00]	@ clear source-pending ints in SRCPND
	ldr	rvb, [rva, #0x10]	@ rvb <- asserted interrupts
	str	rvb, [rva, #0x10]	@ clear asserted interrupts in INTPND
	set	rvb, #0x00
	str	rvb, [rva, #0x04]	@ set all interrupts to IRQ in INTMOD
	ldr	rvb, =0x07ba
	str	rvb, [rva, #0x1c]	@ enable UART 0, 2 Rx ints in INTSUBMSK, and uerr int for uart0
.ifdef	native_usb
	@ initialization of USB device controller
	ldr	rva, =0x31000000
	set	rvb, #0
	str	rvb, [rva]
	str	rvb, [rva, #4]
	set	fre, #0
	ldr	rva, =USB_LineCoding
	ldr	rvb, =115200
	str	rvb, [rva]		@ 115200 bauds
	set	rvb, #0x00080000
	str	rvb, [rva,  #0x04]	@ 8 data bits, no parity, 1 stop bit
	ldr	rva, =USB_CHUNK
	str	fre, [rva]		@ zero bytes remaining to send at startup
	ldr	rva, =USB_ZERO
	str	fre, [rva]		@ alternate interface and device/interface status = 0
	ldr	rva, =USB_CONF
	str	fre, [rva]		@ USB device is not yet configured
	@ signal to host that USB device is attached (set GPC15 low)
	ldr	rva, =0x56000020	@ rva <- GPCCON
	set	rvb, #0x8000
	str	rvb, [rva, #0x08]	@ disable pull-up on GPIO C, pin 15 (GPC15)
	set	rvb, #0x40000000
	str	rvb, [rva]		@ set output function for GPIO C, pin 15 (GPC15)
	ldr	rvb, [rva, #0x04]
	bic	rvb, rvb, #0x8000
	str	rvb, [rva, #0x04]	@ set GPC15 low
	
.endif	@ native_usb
	
	@ end of the hardware initialization
	set	pc,  lnk
	


@------------------------------------------------------------------------------------------------
@  S3C24xx
@
@	 1- Initialization from FLASH, writing to and erasing FLASH
@	 2- I2C Interrupt routine
@
@------------------------------------------------------------------------------------------------
	
@
@ 1- Initialization from FLASH, writing to and erasing FLASH
@

FlashInitCheck: @ return status of flash init enable/override gpio pin (GPG3, EINT11, nSS1) in rva
	ldr	rvb, =ioG_base			@ rvb <- address of pin direction control
	ldr	rva, [rvb, #io_state]		@ rva <- status of all pins
	and	rva, rva, #(1 << 3)		@ rva <- status of GPG3 only (return value)
	set	pc,  lnk			@ return
	
wrtfla:	@ write to flash, sv2 is page address, sv4 is file descriptor
	swi	run_no_irq				@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk}	@ store scheme registers onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5,  env, dts, glv} @ store scheme registers onto stack
	@ copy write-flash-code to boot SRAM
	ldr	sv1, =wflRAM			@ sv1 <- start address of flashing code
	ldr	sv5, =wflEND			@ sv5 <- end address of flashing code
	set	sv3, #0x40000000		@ sv3 <- boot SRAM start target address for flashing code
	bl	cpflcd
	@ prepare to copy buffer data from file descriptor (sv4) (RAM) to FLASH buffer (sv2)
	vcrfi	sv3, sv4, 3			@ sv3 <- buffer address	
	add	sv4, sv2, #F_PAGE_SIZE		@ sv4 <- end target address
wrtfl0:	bl	pgsctr				@ rva <- sector number (raw int), from page address in r5
	adr	rvb, flashsectors		@ rvb <- address of flash sector table
	ldr	sv1, [rvb, rva, LSL #2]		@ sv1 <- address of flash page block start
	@ jump to SRAM code
	set	lnk, pc				@ lnk <- return address
	set	pc,  #0x40000000		@ jump to SRAM
	@ more data to write?
	cmp	sv2, sv4			@ done writing?
	bmi	wrtfl0				@	if not, jump to keep writing data to flash
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ restore scheme registers from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk}	@ restore scheme registers from stack
	orr	fre, fre, #0x02			@ fre <- fre-ptr de-reserved
	swi	run_normal			@ enable interrupts (user mode)
	set	pc,  lnk			@ return

wflRAM:	@ code to be copied to S3C24xx boot SRAM such that execution is from SRAM while
	@ writing file FLASH	
	@ initiate write-buffer to FLASH
	set	rva, #0xe8			@ rva <- CFI write-buffer command code
	strh	rva, [sv1]			@ initiate write-buffer
	ldrh	rva, [sv1]			@ rva <- FLASH device status
	tst	rva, #0x80			@ is FLASH ready?
	subeq	pc,  pc, #24			@	if not, jump to keep waiting
	@ set count and transfer data to FLASH write-buffer
	set	rva, #0x1f			@ rva <- 32 bytes to write
	strh	rva, [sv1]			@ set number of bytes to write in CFI controller
	ldmib	sv3!, {fre,cnt,rva,rvb,sv5,env,dts,glv}	@ get next eight source data words
	stmia	sv2,  {fre,cnt,rva,rvb,sv5,env,dts,glv}	@ store data words in FLASH write-buffer
	stmia	sv2!, {fre,cnt,rva,rvb,sv5,env,dts,glv}	@ store data AGAIN (cfi seems to expect 16x2 writes)
	@ commit write-buffer to FLASH
	set	rva, #0xd0			@ rva <- CFI confirm write-buffer command code
	strh	rva, [sv1]			@ confirm write-buffer command
	ldrh	rva, [sv1]			@ rva <- FLASH device status
	tst	rva, #0x80			@ is FLASH ready?
	subeq	pc,  pc,  #16			@	if not, jump to keep waiting
	set	rva, #0x50			@ rva <- CFI Clear Status Register command code
	strh	rva, [sv1]			@ clear the status register
	set	rva, #0xff			@ rva <- CFI Read Array command code
	strh	rva, [sv1]			@ set FLASH to read array mode
	set	pc,  lnk			@ return
wflEND:	@ end of SRAM code
		
ersfla:	@ erase flash sector that contains page address in sv2
	swi	run_no_irq				@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk}	@ store scheme registers onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5,  env, dts, glv} @ store scheme registers onto stack
	@ copy erase-flash-code to boot SRAM
	ldr	sv1, =rflRAM			@ sv1 <- start address of flashing code
	ldr	sv5, =rflEND			@ sv5 <- end address of flashing code
	set	sv3, #0x40000000		@ sv3 <- boot SRAM start target address for flashing code
	bl	cpflcd
	@ prepare flash sector for write
	bl	pgsctr				@ rva <- sector number (raw int), from page address in sv2
	adr	rvb, flashsectors		@ rvb <- address of flash sector table
	ldr	sv1, [rvb]			@ sv1 <- start address of whole FLASH (controller)
	ldr	sv2, [rvb, rva, LSL #2]		@ sv2 <- address of flash block start
	@ jump to SRAM code
	set	lnk, pc				@ lnk <- return address
	set	pc,  #0x40000000		@ jump to SRAM
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ restore scheme registers from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk}	@ restore scheme registers from stack
	orr	fre, fre, #0x02			@ fre <- fre-ptr de-reserved
	swi	run_normal				@ enable interrupts (user mode)
	set	pc,  lnk			@ return

rflRAM:	@ code to be copied to S3C24xx boot SRAM such that execution is from SRAM while
	@ erasing file FLASH
	@ unlock block to be erased (unlocks all blocks really it seems)
	set	rva, #0x60			@ rva <- CFI unlock block command code
	strh	rva, [sv1]			@ initiate block unlock
	set	rva, #0xd0			@ rva <- CFI confirm unlock command code
	strh	rva, [sv1]			@ confirm block unlock
	ldrh	rva, [sv2]			@ rva <- FLASH device status
	tst	rva, #0x80			@ is FLASH ready?
	subeq	pc,  pc,  #16			@	if not, jump to keep waiting
	set	rva, #0x50			@ rva <- CFI Clear Status Register command code
	strh	rva, [sv1]			@ clear the status register
	@ erase block whose address starts at sv2
	set	rva, #0x20			@ rva <- CFI erase block command code
	strh	rva, [sv2]			@ initiate erase block
	set	rva, #0xd0			@ rva <- CFI confirm erase command code
	strh	rva, [sv2]			@ confirm erase block
	ldrh	rva, [sv2]			@ rva <- FLASH device status
	tst	rva, #0x80			@ is FLASH ready?
	subeq	pc,  pc,  #16			@	if not, jump to keep waiting
	set	rva, #0x50			@ rva <- CFI Clear Status Register command code
	strh	rva, [sv1]			@ clear the status register
	set	rva, #0xff			@ rva <- CFI Read Array command code
	strh	rva, [sv1]			@ set FLASH to read array mode
	set	pc,  lnk			@ return
rflEND:	@ end of RAM code

cpflcd:	@ copy FLASH code to RAM
	ldr	rva, [sv1]			@ rva <- next flashing code instruction
	str	rva, [sv3]			@ store it in free RAM
	eq	sv1, sv5			@ done copying the flashing code?
	addne	sv1, sv1, #4			@	if not, sv1 <- next flashing code source address
	addne	sv3, sv3, #4			@	if not, sv1 <- next flashing code target address
	bne	cpflcd				@	if not, jump to keep copying flashing code to RAM
	set	pc,  lnk			@ return


.ifdef TCT_Hammer
flashsectors:	@ 128 x 128KB FLASH sectors of Intel JS28F128 J3D75 on TCT Hammer board
.word	0x00000000, 0x00020000,  0x00040000,  0x00060000,  0x00080000,  0x000A0000,  0x000C0000,  0x000E0000
.word	0x00100000, 0x00120000,  0x00140000,  0x00160000,  0x00180000,  0x001A0000,  0x001C0000,  0x001E0000
.word	0x00200000, 0x00220000,  0x00240000,  0x00260000,  0x00280000,  0x002A0000,  0x002C0000,  0x002E0000
.word	0x00300000, 0x00320000,  0x00340000,  0x00360000,  0x00380000,  0x003A0000,  0x003C0000,  0x003E0000
.word	0x00400000, 0x00420000,  0x00440000,  0x00460000,  0x00480000,  0x004A0000,  0x004C0000,  0x004E0000
.word	0x00500000, 0x00520000,  0x00540000,  0x00560000,  0x00580000,  0x005A0000,  0x005C0000,  0x005E0000
.word	0x00600000, 0x00620000,  0x00640000,  0x00660000,  0x00680000,  0x006A0000,  0x006C0000,  0x006E0000
.word	0x00700000, 0x00720000,  0x00740000,  0x00760000,  0x00780000,  0x007A0000,  0x007C0000,  0x007E0000
.word	0x00800000, 0x00820000,  0x00840000,  0x00860000,  0x00880000,  0x008A0000,  0x008C0000,  0x008E0000
.word	0x00900000, 0x00920000,  0x00940000,  0x00960000,  0x00980000,  0x009A0000,  0x009C0000,  0x009E0000
.word	0x00A00000, 0x00a20000,  0x00a40000,  0x00a60000,  0x00a80000,  0x00aA0000,  0x00aC0000,  0x00aE0000
.word	0x00B00000, 0x00b20000,  0x00b40000,  0x00b60000,  0x00b80000,  0x00bA0000,  0x00bC0000,  0x00bE0000
.word	0x00C00000, 0x00c20000,  0x00c40000,  0x00c60000,  0x00c80000,  0x00cA0000,  0x00cC0000,  0x00cE0000
.word	0x00D00000, 0x00d20000,  0x00d40000,  0x00d60000,  0x00d80000,  0x00dA0000,  0x00dC0000,  0x00dE0000
.word	0x00E00000, 0x00e20000,  0x00e40000,  0x00e60000,  0x00e80000,  0x00eA0000,  0x00eC0000,  0x00eE0000
.word	0x00F00000, 0x00f20000,  0x00f40000,  0x00f60000,  0x00f80000,  0x00fA0000,  0x00fC0000,  0x00fE0000
.word	0x01000000
.endif	@ .ifdef TCT_Hammer
	
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
	set	rvb, #3
	str	rvb, [rva, #0x0c]	@ sppre <- 60MHz/2/(3+1) ~= 6.3MHz
	set	rvb, #0x18
	str	rvb, [rva, #0x00]	@ spcon (control) <- CLKen, master, POL=PHA=0
	set	pc,  lnk

_func_	
sd_slo:	@ configure spi speed (low), phase, polarity
	ldr	rva, =sd_spi
	set	rvb, #63
	str	rvb, [rva, #0x0c]	@ sppre <- 60MHz/2/(63+1) ~= 400KHz
	set	rvb, #0x18
	str	rvb, [rva, #0x00]	@ spcon (control) <- CLKen, master, POL=PHA=0
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
@  S3C24xx
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
