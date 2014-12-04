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

/*------------------------------------------------------------------------------
@
@	Targets: LPC2000, LPC1300, LPC1700, LPC4300
@
@-----------------------------------------------------------------------------*/

_func_
hwi2cr:	@ write-out additional address registers, if needed
	@ modify interupts, as needed
	@ on entry:	sv5 <- i2c[0/1]buffer
	@ on entry:	rva <- i2c[0/1] base address (also I2CONSET)
	@ interrupts are disabled throughout
	set	rvb, #0			@ rvb <- 0 bytes to send (scheme int)
	tbsti	rvb, sv5, 3		@ store number of bytes to send in i2c buffer[12]
	@ initiate i2c read/write, as master
	swi	run_normal		@ re-enable interrupts
	set	rvb, #0x20		@ rvb <- i2c START command
	strb	rvb, [rva, #i2c_cset]	@ initiate bus mastering (write start to i2c[0/1]conset)
hwi2r0:	@ wait for mcu address and registers to have been transmitted
	swi	run_no_irq		@ disable interrupts
	tbrfi	rvb, sv5, 1		@ rvb <- data ready status from i2cbuffer[4]
	eq	rvb, #f			@ is i2c data ready = #f (i.e. addresses have been transmitted)
	it	eq
	seteq	pc,  lnk		@	if so, jump to continue
	swi	run_normal		@ re-enable interrupts
	b	hwi2r0			@ jump to keep waiting

_func_
hwi2ni:	@ initiate i2c read/write, as master
	@ on entry:	rva <- i2c[0/1] base address (also I2CONSET)
	set	rvb, #0x20		@ rvb <- i2c START command
	strb	rvb, [rva, #i2c_cset]	@ initiate bus mastering (write start to i2c[0/1]conset)
	set	pc,  lnk

_func_
hwi2st:	@ get i2c interrupt status and base address
	@ on exit:	rva <- i2c[0/1] base address
	@ on exit:	rvb <- i2c interrupt status
	ldrb	rvb, [rva, #i2c_status]	@ r7  <- I2C Status
	set	pc,  lnk

_func_
i2c_hw_branch:	@ process interrupt
	eq	rvb, #0x08		@ Master Read/Write -- bus now mastered		(I2STAT = 0x08)
	beq	i2c_hw_mst_bus
	eq	rvb, #0x18		@ Master Write -- slave has acknowledged adress	(I2STAT = 0x18)
	beq	i2c_wm_ini
	eq	rvb, #0x28		@ Master Write -- slave ok to receive data	(I2STAT = 0x28)
	beq	i2c_wm_put
	eq	rvb, #0x40		@ Master Read  -- slave ackn. adress (set nak?)	(I2STAT = 0x40)
	beq	i2c_rm_ini
	eq	rvb, #i2c_irm_rcv	@ Master Read  -- new byte received (set nak?)	(I2STAT = 0x50)
	beq	i2c_rm_get
	eq	rvb, #0x58		@ Master Read  -- last byte received		(I2STAT = 0x58)
	beq	i2c_rm_end
	eq	rvb, #0x60		@ Slave Read   -- address recognized as mine	(I2STAT = 0x60)
	beq	i2c_rs_ini
	eq	rvb, #i2c_irs_rcv	@ Slave Read   -- new data received		(I2STAT = 0x80)
	beq	i2c_rs_get
	eq	rvb, #0xA0		@ Slave Read   -- STOP or re-START received	(I2STAT = 0xA0)
	beq	i2c_rs_end
	eq	rvb, #0xA8		@ Slave Write  -- address recognized as mine	(I2STAT = 0xA8)
	beq	i2c_ws_ini
	eq	rvb, #0xB8		@ Slave Write  -- master requests byte		(I2STAT = 0xB8)
	beq	i2c_ws_put
	eq	rvb, #0xC0		@ Slave Write  -- NAK received from master/done	(I2STAT = 0xC0)
	beq	i2c_ws_end
	set	pc,  lnk

_func_
i2c_hw_mst_bus:	@ Reading or Writing as Master -- bus now mastered (I2STAT = 0x08)
	tbrfi	rva, sv2, 0		@ rva <- address of mcu to send data to (scheme int)
	lsr	rva, rva, #1		@ rva <- mcu-id as int -- note: ends with 0 (i.e. divide by 2)
	strb	rva, [sv3, #i2c_thr]	@ set address of mcu to send data to
	set	rva, #0x20		@ rva <- bit 5
	strb	rva, [sv3, #i2c_cclear]	@ clear START bit to enable Tx of target address
	b	i2cxit

_func_
hwi2we:	@ set busy status/stop bit at end of write as master
	@ on entry:	sv2 <- i2c[0/1] buffer address
	@ on entry:	sv3 <- i2c[0/1] base address
	@ on entry:	rvb <- #f
	tbrfi	rva, sv2, 3		@ rva <- number of data bytes to send (raw int)
	eq	rva, #0			@ were we sendng 0 byts (i.e. readng as mstr, done wrtng adr byts)?
	itTT	ne
	tbstine rvb, sv2, 0		@	if not, set busy status to #f (transfer done)
	setne	rva, #0x10		@	if not, rva <-  STOP bit used to stop i2c transfer
	strbne	rva, [sv3, #i2c_cset]	@	if not, set  STOP bit to stop i2c transfer
	set	pc,  lnk
	
_func_
hwi2re:	@ set stop bit if needed at end of read-as-master
	set	rva, #0x014		@ rva <- bit4 | bit 2
	strb	rva, [sv3, #i2c_cset]	@ set STOP bit and reset AA to AK
	set	pc,  lnk
	
_func_
hwi2cs:	@ clear SI
	set	rva, #0x08		@ clear SI
	strb	rva, [sv3, #i2c_cclear]
	set	pc,  lnk

_func_
i2cstp:	@ prepare to end Read as Master transfer
	set	rva, #0x04		@ rva <- bit 2
	strb	rva, [sv3, #i2c_cclear]	@ set AA to NAK
	set	pc,  lnk
		
_func_
i2putp:	@ Prologue:	write additional address bytes to i2c, from buffer or r12 (prologue)
	tbrfi	rva, sv2, 1		@ rva <- number of additional address bytes to send (scheme int)
	eq	rva, #i0		@ no more address bytes to send?
	itTT	eq
	tbrfieq rva, sv2, 3		@	if so,  rva <- number of data bytes to send (raw int)
	tbrfieq rvb, sv2, 4		@	if so,  rvb <- number of data bytes sent (raw int)
	eqeq	rva, rvb		@	if so,  are we done sending data?
	beq	i2c_wm_end		@		if so, jump to stop or restart x-fer and exit
	tbrfi	rvb, sv2,  1		@ r7  <- number of address bytes remaining to send (scheme int)
	eq	rvb, #i0		@ done sending address bytes?
	itTTT	ne
	subne	rvb, rvb, #4		@	if not, rvb <- updtd num of addrss byts to snd (scheme int)
	tbstine rvb, sv2, 1		@	if not, str updtd num of addrss byts to snd in i2cbuffer[4]
	addne	rva, sv2, #8		@	if not, rva <- addrss of additionl addrss byts in i2cbuffer
	lsrne	rvb, rvb, #2
	itTTT	ne
	ldrbne	rva, [rva, rvb]		@	if not, rva <- next address byte to send
	strbne	rva, [sv3, #i2c_thr]	@ put next data byte in I2C data register
	lslne	rvb, rvb, #2
	orrne	rvb, rvb, #i0
	bne	i2cxit
	set	pc,  lnk

_func_
i2pute:	@ Epilogue:	set completion status if needed (epilogue)
	set	pc,  lnk


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg


