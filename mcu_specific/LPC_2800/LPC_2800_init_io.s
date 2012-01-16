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
	.word	ptmisr			@ 5:	timer0
	.word	ptmisr			@ 6:	timer1
	.word	i0			@ 7
	.word	i0			@ 8
	.word	i0			@ 9
	.word	i0			@ 10
	.word	i0			@ 11
	.word	puaisr			@ 12:	uart0/1
	.word	pi2isr			@ 13:	i2c0/1 (if included)
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
	.word	usbisr			@ 26:	USB (if included)
	.word	usbisr			@ 27:	USB (if included)
	.word	usbisr			@ 28:	USB (if included)
	.word	usbisr			@ 29:	USB (if included)
	.word	i0			@ 30
	.word	i0			@ 31


hwinit:	@ pre-set common values
	set	r0,  #0
	set	r1,  #1
	set	r2,  #2
	set	r3,  #3
	@ set LED port pin to output and turn LED on
	ldr	r6,  =LEDIO
	set	r7,  #0x02
	str	r7,  [r6,  #0x24]	@ Port 2, Mode1S_2 Set Regstr (dir/func) port 2 pin 1 (P2.1) is output
	@ initialize interrupts
	ldr	r6,  =int_base
	add	r6,  r6, #0x0400
	ldr	r7,  =0x1C010001
	str	r7,  [r6,  #0x14]	@ INT_REQ5  <- Timer 0 zero Count interrupt enabled as IRQ
	str	r7,  [r6,  #0x18]	@ INT_REQ6  <- Timer 1 zero Count interrupt enabled as IRQ
	str	r7,  [r6,  #0x30]	@ INT_REQ12 <- UART interrupt enabled as IRQ
	str	r7,  [r6,  #0x34]	@ INT_REQ13 <- i2C interrupt enabled as IRQ
	@ initialize uart
	ldr	r6,  =uart0_base
	str	r1,  [r6,  #0x08]	@ U0FCR        <- Enable UART0, Rx trigger-level = 1 char
	set	r7,  #0x80
	str	r7,  [r6,  #0x0c]	@ U0LCR        <- Enable UART0 divisor latch
	ldr	r7,  =UART0_DIV_L
	str	r7,  [r6]		@ U0DLL        <- UART0 lower byte of divisor for 9600 baud
	ldr	r7,  =UART0_DIV_H
	str	r7,  [r6,  #0x04]	@ U0DLM        <- UART0 upper byte of divisor for 9600 baud
	str	r3,  [r6,  #0x0c]	@ U0LCR        <- Disable UART0 divisor latch and set 8N1 transmission
	str	r1,  [r6,  #0x04]	@ U0IER        <- Enable UART0 RDA interrupt
	@ initialization of mcu-id for variables (normally I2c address if slave enabled)
	ldr	r6,  =I2C0ADR
	set	r7,  #mcu_id
	str	r7,  [r6]		@ I2CADR <- set mcu address
	@ unlock the FLASH
	set	r12, lnk
	bl	unlok
	set	lnk, r12
	ldr	r6,  =USB_CONF
	str	r0,  [r6]

.ifdef	native_usb
	@ USB initialization
	ldr	r8,  =usb_clken
	str	r0, [r8]		@ disable USB Clock
	str	r1, [r8]		@ enable USB Clock
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
	str	r0,  [r6]		@ indicate that USB is not yet configured
	@ see if USB is plugged in (if not, exit USB setup)
	ldr	r6,  =0x800031C0
	ldr	r6,  [r6]
	tst	r6,  #0x01
	seteq	pc,  lnk
	ldr	r6,  =usb_base
	str	r0,  [r6]		@ USB Device Address <- 0
	set	r7,  #0xfc
	str	r7,  [r6,  #0x10]	@ USB Interrupt config <- int on ACK, STALL NYET, sometimes NAK
	str	r1,  [r8]		@ enable USB Clock
	set	r7,  #0x20
	str	r7,  [r6,  #usb_epind]	@ USB EP  INDEX <- select EP 0 SETUP
	set	r7,  #0xff
	str	r7,  [r6,  #0xac]	@ USB Dev Inter clear <- clear all device interrupts
	ldr	r7,  =0xffff
	str	r7,  [r6,  #0xa0]	@ USB EP Inter clear <- clear all endpoint interrupts (EP 0-7, Tx/Rx)
	str	r7,  [r6,  #0x90]	@ USB EP Inter enable <- enable the bus reset interrupt
	ldr	r7,  =0xaa37
	str	r7,  [r6,  #0x7c]	@ unlock USB registers
	set	r7,  #0xa1
	str	r7,  [r6,  #0x8c]	@ USB Dev Inter enable <- enable the bus reset interrupt
	ldr	r6,  =USB_FSHS_MODE
	str	r0,  [r6]		@ indicate that USB is not yet in HS mode
	ldr	r6,  =usb_base
	ldr	r8,  =int_base
	add	r8,  r8, #0x0400
	ldr	r7,  =0x1C010001
	str	r7,  [r8,  #0x68]	@ INT_REQ26 <- USB interrupt enabled as IRQ, priority=1
	str	r7,  [r8,  #0x6c]	@ INT_REQ27 <- USB interrupt enabled as IRQ, priority=1
	str	r7,  [r8,  #0x70]	@ INT_REQ28 <- USB interrupt enabled as IRQ, priority=1
	str	r7,  [r8,  #0x74]	@ INT_REQ29 <- USB interrupt enabled as IRQ, priority=1
	set	r7,  #0x80
	str	r7,  [r6]		@ USB Device Address <- 0, enabled
	set	r7,  #0x89
	str	r7,  [r6,  #0x0c]	@ USB Mode <- clock always on, interrupts enabled, soft connect enabld

.endif	@ native_usb
	
	set	pc,  lnk	


@------------------------------------------------------------------------------------------------
@ lpc28xx
@
@	 1- Initialization from FLASH, writing to and erasing FLASH
@	 2- I2C Interrupt routine
@
@------------------------------------------------------------------------------------------------
	
@
@ 1- Initialization from FLASH, writing to and erasing FLASH
@

FlashInitCheck: @ return status of flash init enable/override gpio pin (P2.0) in r6
	ldr	rvb, =io2_base			@ rvb <- PINS_2
	ldr	rva, [rvb]			@ rva <- values of all PINS_2
	and	rva, rva, #1			@ rva <- status of P2.0 only
	eor	rva, rva, #1			@ rva <- status of P2.0 inverted (return value)
	set	pc,  lnk

wrtfla:	@ write to flash, sv2 = page address, sv4 = file descriptor
	swi	run_no_irq			@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk}	@ store scheme registers onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ store scheme registers onto stack
	@ copy buffer data from file descriptor{sv4} (RAM) to FLASH buffer {sv2}
	vcrfi	sv3, sv4, 3			@ sv3 <- buffer address	
	add	sv5, sv2, #F_PAGE_SIZE		@ sv5 <- end target address
wrtfl0:	add	sv3, sv3, #4			@ sv3 <- address of next source word
	bl	pgsctr				@ rva <- sector number (raw int), from page address in sv2
	ldr	rvb, =flashsectors		@ rvb <- address of flash address table
	ldr	rvb, [rvb, rva,LSL #2]		@ rvb <- start address of target flash block
	@ write lower 2 bytes of word
	ldrh	rvc, [sv3]			@ rvc <- lower half of word to write
	set	rva, #0x40			@ rva <- CFI word program command code
	strh	rva, [sv2]			@ start half word write
	strh	rvc, [sv2]			@ confirm half word write
flwrw1:	@ wait for FLASH device to be ready
	ldrh	rva, [rvb]			@ rva <- FLASH device status
	tst	rva, #0x80			@ is FLASH ready?
	beq	flwrw1				@	if not, jump to keep waiting
	@ write upper two bytes of word
	ldrh	rvc, [sv3, #2]			@ rvc <- upper half word to write
	set	rva, #0x40			@ rva <- CFI word program command code
	strh	rva, [sv2, #2]			@ start half word write
	strh	rvc, [sv2, #2]			@ confirm half word write
flwrw2:	@ wait for FLASH device to be ready
	ldrh	rva, [rvb]			@ rva <- FLASH device status
	tst	rva, #0x80			@ is FLASH ready?
	beq	flwrw2				@	if not, jump to keep waiting
	@ jump to keep writing or finish up
	add	sv2, sv2, #4			@ sv2 <- target address of next word
	cmp	sv2, sv5			@ done writing page?
	bmi	wrtfl0				@	if not, jump to keep writing
	@ Return to FLASH Read Array mode
	set	rva, #0x00ff			@ rva <- CFI Read Array command code
	strh	rva, [rvb]			@ set FLASH to read array mode
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ restore scheme registers from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk}	@ restore scheme registers from stack
	swi	run_normal				@ enable interrupts (user mode)
	set	pc,  lnk			@ return

ersfla:	@ erase flash sector that contains page address in sv2
	swi	run_no_irq				@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk}	@ store scheme registers onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ store scheme registers onto stack
	@ prepare flash sector for write
	bl	pgsctr				@ rva <- sector number (raw int), from page address in sv2
	ldr	rvb, =flashsectors		@ rvb <- address of flash sector table
	ldr	rvb, [rvb, rva, LSL #2]		@ rvb <- address of flash block start
	@ erase block whose address starts at sv2
	set	rva, #0x0020			@ rva <- CFI erase block command code
	strh	rva, [rvb]			@ initiate erase block
	set	rva, #0x00d0			@ rva <- CFI confirm erase command code
	strh	rva, [rvb]			@ confirm erase block
	@ wait for FLASH device to be ready
	adr	rvb, flashsectors		@ rvb <- address of flash sector table
	ldr	rvb, [rvb]			@ rvb <- FLASH start address
flrdwt:	ldrh	rva, [rvb]			@ rva <- FLASH device status
	tst	rva, #0x80			@ is FLASH ready?
	beq	flrdwt				@	if not, jump to keep waiting
	@ Return to FLASH Read Array mode
	set	rva, #0x00ff			@ rva <- CFI Read Array command code
	strh	rva, [rvb]			@ set FLASH to read array mode
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ restore scheme registers from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk}	@ restore scheme registers from stack
	swi	run_normal				@ enable interrupts (user mode)
	set	pc,  lnk			@ return

unlok:	@ unlock all file flash -- called by hwinit
	set	r6,  lr				@ r6  <- lnk, saved
	ldr	r5,  =F_START_PAGE		@ r5  <- start address of file flash
	bl	pgsctr				@ r2  <- sector number (raw int), from page address in r5
	ldr	r3,  =F_END_PAGE		@ r3  <- end address of file flash
	adr	r9,  flashsectors		@ r9  <- address of flash sector table
unlok0:	@ loop over flash blocks to be unlocked
	ldr	r5,  [r9,  r2,  LSL #2]		@ r5  <- start address of flash sector
	@ unlock block that starts at sv2
	ldr	r0,  [r9]			@ r0  <- FLASH start address
	set	r4,  #0x0060			@ r4  <- CFI unlock block command code
	strh	r4,  [r5]			@ initiate block unlock
	set	r4,  #0x00d0			@ r4  <- CFI confirm unlock command code
	strh	r4,  [r5]			@ confirm block unlock
	@ wait for FLASH device to be ready
	set	r4,  #0x0090			@ r4  <- CFI read device ID command code
	strh	r4,  [r0]			@ initiate ID and status read
unlok1:	ldrh	r4,  [r5,  #4]			@ r4  <- block status
	tst	r4,  #0x03			@ is block unlocked?
	bne	unlok1				@	if not, jump to keep waiting
	cmp	r5,  r3				@ done unlocking?
	addmi	r2,  r2, #1			@	if not, r2  <- next sector number
	bmi	unlok0				@	if not, jump to unlock next sector
	@ Return to FLASH Read Array mode and exit
	set	r4,  #0x00ff			@ r4  <- CFI Read Array command code
	strh	r4,  [r0]			@ set FLASH to read array mode
	set	pc,  r6				@ return


.ifdef LPC_H2888
flashsectors:	@ 8 x 8KB + 31 x 64KB FLASH sectors of Intel JS28F160C3-BD70 on LPC-H2888 board
.word	0x20000000, 0x20002000, 0x20004000, 0x20006000, 0x20008000, 0x2000A000, 0x2000C000, 0x2000E000
.word	0x20010000, 0x20020000, 0x20030000, 0x20040000, 0x20050000, 0x20060000, 0x20070000, 0x20080000
.word	0x20090000, 0x200A0000, 0x200B0000, 0x200C0000, 0x200D0000, 0x200E0000, 0x200F0000, 0x20100000
.word	0x20110000, 0x20120000, 0x20130000, 0x20140000, 0x20150000, 0x20160000, 0x20170000, 0x20180000
.word	0x20190000, 0x201A0000, 0x201B0000, 0x201C0000, 0x201D0000, 0x201E0000, 0x201F0000, 0x20200000
.endif


.ltorg

@
@	2- I2C Interrupt routine
@

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
