/*------------------------------------------------------------------------------
@
@  ARMPIT SCHEME Version 070
@
@  ARMPIT SCHEME is distributed under The MIT License.

@  Copyright (c) 2006-2014 Hubert Montas

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
@-----------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
@
@	I2C Interrupt routine hardware-specific sub-components
@
@-----------------------------------------------------------------------------*/


_func_	
hwi2cr:	@ write-out additional address registers, if needed
	@ modify interupts, as needed
	@ on entry:	sv5 <- i2c[0/1]buffer
	@ on entry:	rva <- i2c[0/1] base address (also I2CONSET)
	@ interrupts are disabled throughout
	set	pc,  lnk
	
_func_	
hwi2ni:	@ initiate i2c read/write, as master
	@ on entry:	rva <- i2c base address
	ldr	rvb, =0x107		@ rvb <- NACK, TXRDY, RXRDY and TXCOMP
	str	rvb, [rva, #i2c_iclear]	@ disable all TWI interrupts
	set	rvb, #0x05		@ rvb <- TXRDY and TXCOMP
	str	rvb, [rva, #i2c_ienable] @ enable TXRDY and TXCOMP interrupts
	set	rvb, #4			@ rvb <- TWI enable bit
	str	rvb, [rva, #i2c_ctrl]	@ start transfer
	set	pc,  lnk

_func_	
hwi2st:	@ get i2c interrupt status and base address
	@ on exit:	rva <- i2c[0/1] base address
	@ on exit:	rvb <- i2c interrupt status
	ldr	rvb, [rva, #i2c_status]	@ rvb <- current status of TWI interface
	ldr	rva, [rva, #i2c_imask]	@ rva <- TWI enabled Interrupt Mask
	and	rvb, rvb, rva		@ rvb <- asserted TWI interrupts (without potential spurious bits)
	ldr	rva, =i2c0_base		@ rva <- address of Status Register (restored)
	set	pc,  lnk

_func_	
hwi2cs:	@ clear SI
	set	pc,  lnk

_func_	
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

_func_	
i2c_hw_mst_bus:	@ Reading or Writing as Master -- bus now mastered
	@ on entry:	sv1 <- i2c[0/1] data offset in glv
	@ on entry:	sv2 <- i2c[0/1] buffer address
	@ on entry:	sv3 <- i2c[0/1] base address
	set	rvb, #0			@ rvb <- 0, number of bytes sent/received so far
	tbsti	rvb, sv2, 4		@ store number of bytes sent/received in i2c buffer
	@ store internal address bytes in TWI_IADR 
	set	rva, #i2c_iadr		@ rva <- 0, offset to internal address in TWI_IADR
	tbrfi	sv4, sv2, 1		@ sv4 <- number of internal address bytes (scheme int)
	add	sv4, sv4, #0x20		@ sv4 <- additional number of address bytes (scheme int)
i2str0:	eq	sv4, #0x21		@ are we done writing additional address bytes?
	itTTT	ne
	subne	sv4, sv4, #4		@	if not, sv4 <- adrs byt offset in i2cbfr[8] (scheme int)
	lsrne	rvb, sv4, #2
	ldrbne	rvb, [sv2, rvb]		@	if not, rvb <- address byte from i2cbuffer[8+offset]
	strbne	rvb, [sv3, rva]		@ 	if not, store next internal address byte in TWO_IADR
	it	ne
	addne	rva, rva,#1		@	if not, rva <- offset to next internal address in TWI_IADR
	bne	i2str0			@	if not, jump to str nxt adrs byt
	@ set TWI_MMR to wrt/rd to/from i2c adrs with num of intern adrs bytes
	tbrfi	rvb, sv2, 0		@ r7  <- target mcu adrs, int=wrt/flt=rd
	tst	rvb, #0x02		@ is this a write operation?
	itE	eq
	seteq	rva, #0x0000		@	if so,  rva <- TWI wrt+trgt adrs
	setne	rva, #0x1000		@	if not, rva <- TWI rd+trgt adrs
	lsr	rvb, rvb, #2		@
	orr	rva, rva, rvb, LSL #16	@
	tbrfi	rvb, sv2, 1		@ rvb <- num intern adrs bytes (scm int)
	lsr	rvb, rvb, #2		@ rvb <- num intern adrs bytes (raw int)
	orr	rva, rva, rvb, LSL #8	@ rva <- r/w and #intern adrs bytes
	str	rva, [sv3, #i2c_mode]	@ TWI MMR <- r/w,#int adrs,trgt adrs
	ldr	rvb, =0x107		@ rvb <- NACK, TXRDY, RXRDY and TXCOMP
	str	rvb, [sv3, #i2c_iclear]	@ deactivate interrupts
	beq	i2strw			@	if so,  jump to start a write
	@ start an i2c read
	tbrfi	rva, sv2,  3		@ rva <- number of bytes to read
	cmp	rva, #2			@ are we reading just 1 byte?
	itE	mi
	setmi	rvb, #1			@	if so,  rvb <- TXCOMP bit
	setpl	rvb, #2			@	if not, rvb <- TWI RXRDY bit
	str	rvb, [sv3, #i2c_ienable] @ enable TWI RXRDY interrupt
	tbrfi	rva, sv2,  3		@ rva <- number of bytes to send/read
	cmp	rva, #2			@ are we reading just 1 byte?
	itE	mi
	setmi	rvb, #3			@	if so,  rvb <- stop+start bits
	setpl	rvb, #1			@	if not, rvb <- start bit
	str	rvb, [sv3, #i2c_ctrl]	@ start transfer
	bl	gldon			@ turn led on
	b	i2cxit			@ exit
i2strw:	@ start an i2c write
	tbrfi	rva, sv2,  3		@ rva <- number of bytes to send
	cmp	rva, #2			@ are we sending just 1 byte?
	itEE	mi
	setmi	rvb, #1			@	if so,  rvb <- TWI TXCOMP bit
	setpl	rvb, #4			@	if not, rvb <- TWI TXRDY bit
	orrpl	rvb, rvb, #0x0100	@	if not, rvb <- TXRDY & NAK bits
	str	rvb, [sv3, #i2c_ienable] @ enable TWI TXCOMP OR TXRDY interrupt
	bl	i2putc			@ jump to write 1st byte
	tbrfi	rva, sv2,  3		@ rva <- number of bytes to send
	cmp	rva, #2			@ are we sending just 1 byte?
	itT	mi
	setmi	rvb, #3			@	if so,  rvb <- stop & start bits
	strmi	rvb, [sv3, #i2c_ctrl]	@	if so,  start transfer
	bl	gldon			@ turn led on
	b	i2cxit
	
_func_	
i2putp:	@ Prologue:	write additional adrs bytes to i2c, from buffer or r12
	set	pc,  lnk

_func_	
i2pute:	@ Epilogue:	set completion status if needed
	tbrfi	rva, sv2, 3		@ rva <- num data bytes to snd (raw int)
	tbrfi	rvb, sv2, 4		@ rvb <- num data bytes sent (raw int)
	eq	rva, rvb		@ done sending?
	beq	i2cstp			@	if so,  jump to end transfer
	set	pc,  lnk

i2cnak:	@ re-send last byte
	tbrfi	rvb, sv2, 4		@ rvb <- num data bytes sent (raw int)
	sub	rvb, rvb, #1
	tbsti	rvb, sv2, 4		@ rvb <- num data bytes sent (raw int)
	b	i2c_wm_put
	
_func_	
i2cstp:	@ NAK received or just 1 byte left to read, set stop bit
	@ note how this is also the bottom of i2pute, above
	ldr	rvb, =0x107		@ rvb <- NACK, TXRDY, RXRDY and TXCOMP
	str	rvb, [sv3, #i2c_iclear]	@ disable TWI interrupts
	set	rvb, #1			@ rvb <- TXCOMP bit
	str	rvb, [sv3, #i2c_ienable] @ enable TWI TXCOMP interrupt
	set	rvb, #2			@ rvb <- stop bit
	str	rvb, [sv3, #i2c_ctrl]	@ set stop transfer
	set	pc,  lnk

_func_	
i2c_mst_end:	@ txcomp received
	tbrfi	rvb, sv2, 0		@ rvb <- adrs mcu to wrt/rd dat to/from (scheme int{w}/float{r})
	tst	rvb, #0x02		@ is this a write operation?
	beq	i2c_wm_end
	b	i2c_rm_end

_func_	
hwi2we:	@ set busy status/stop bit at end of write as master
	@ on entry:	sv2 <- i2c[0/1] buffer address
	@ on entry:	sv3 <- i2c[0/1] base address
	@ on entry:	rvb <- #f
	tbsti	rvb, sv2, 0		@ set busy status to #f (transfer done)
	ldr	rvb, =0x107		@ rvb <- NACK, TXRDY, RXRDY and TXCOMP
	str	rvb, [sv3, #i2c_iclear]	@ disable TWI interrupts
	set	pc,  lnk
	
_func_	
hwi2re:	@ set stop bit if needed at end of read-as-master
	@ on entry:	sv3 <- i2c[0/1] base address
	ldr	rvb, =0x107		@ rvb <- NACK, TXRDY, RXRDY and TXCOMP
	str	rvb, [sv3, #i2c_iclear]	@ disable TWI interrupts
	set	pc,  lnk

.ltorg

