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
@	I2C routine for communication with PMIC
@
@	Targets:
@
@		OMAP_35xx, OMAP_44xx, AM335x
@
@	Code Entry Points:
@
@		hwi2rst		(helper macro)
@		hwi2wr		(helper macro)
@		hwi2rd		(helper macro)
@		hw_i2c_reset
@		hw_i2c_write
@		hw_i2c_read
@
@-----------------------------------------------------------------------------*/

	/* helper macro for hw_i2c_reset */
.macro	hwi2rst	arg1
	ldr	rva, =\arg1
	bl	hw_i2c_reset
.endm

hw_i2c_reset: /* reset the i2c module */
	@ in:	rva <- base address of i2c module
	@ mods:	rvb
	write16	0x02, rva, #i2c_sysc	@ reset in I2C_SYSC
i2crst:	@ wait for reset completion
	write16	0x8000, rva, #i2c_cset	@ enable i2c in I2C_CON
	read16	rvb,    rva, #i2c_syss	@ rvb <- reset status from I2C_SYSS
	tst	rvb, #1
	beq	i2crst
	@ configure i2c module
	write16	0x01,   rva, #i2c_sysc	@ enable auto-idling in I2C_SYSC
	write16	0x07,   rva, #i2c_psc	@ set prescaler I2C_PSC
	write16	0x08,   rva, #i2c_scll	@ set low clock duration I2C_SCLL
	write16	0x0a,   rva, #i2c_sclh	@ set high clock duration I2C_SCLH
	write16	mcu_id, rva, #i2c_address @ set cpu 0 address in I2C_OA0
  .ifdef i2c_address_1
	add	rvb, rvb, #2
	write16	rvb, rva, #i2c_address_1  @ set cpu 1 address in I2C_OA1
  .endif
	write16	0x8000, rva, #i2c_cset	@ enable i2c in I2C_CON
	set	pc,  lnk

	/* helper macro for hw_i2c_write */
.macro	hwi2wr	arg1, arg2
	ldr	rvc, =\arg1
	set	sv3, \arg2
	bl	hw_i2c_write
.endm

hw_i2c_write: /* write bytes (reg+data) to an i2c slave */
	@ in:	rva <- base address of i2c module
	@ in:	rvc <- address + bytes to wrt, slav-adr=lsB,last-dat=msB
	@ in:	sv3 <- number of reg+data bytes to write (scheme int)
	@ mods:	rvb, rvc, sv3
	@ wait for bus not busy
	read16	rvb, rva, #i2c_status	@ rvb <- current status
	eq	rvb, #0			@ is stat zero (mod fresh out of reset)?
	beq	i2c_wc			@	if so,  jump to continue
	tst	rvb, #(1 << 12)		@ is bus busy?
	bne	hw_i2c_write		@	if so,  jump to keep waiting
	tst	rvb, #(1 << 2)		@ is module command-ready?
	beq	hw_i2c_write		@	if not, jump to keep waiting
	@ check for state errors
	read16	rvb, rva, #i2c_status	@ rvb <- current status
	tst	rvb, #0x08		@ is module RdRdy (a state error)?
	beq	i2c_wc			@	if not, jump to continue
	@ clear Tx and Rx FIFOs
	write16	0x4040, rva, #i2c_buf	@ clear Tx and Rx FIFOs
	wait	1<<24			@ wait a bit
	read16	rvb, rva, #i2c_status	@ rvb <- current status from I2C_STAT
i2c_wc:	@ continue and configure module for transmission as master
	write16	rvb, rva, #i2c_status	@ clear current status bits in I2C_STAT
	read16	rvb, rva, #i2c_sadr	@ rvb <- remote slave address frm I2C_SA
	eor	rvb, rvb, rvc
	tst	rvb, #0xff
	beq	i2c_w6
	@ wait a bit on slave address change
	wait	1<<24
	and	rvb, rvc, #0xff
	write16	rvb, rva, #i2c_sadr	@ set remote slave address in I2C_SA
i2c_w6:	@ continue with same slave address as prior i2c op
	int2raw	rvb, sv3
	write16	rvb, rva, #i2c_cnt	@ set number of bytes to send in I2C_CNT
	dsb
	write16	0x8603, rva, #i2c_cset	@ set master Tx w/Start+Stop in I2C_CON
i2c_w0:	@ loop to write data bytes
	eq	sv3, #i0		@ nothing left to write?
  .ifndef reset_i2c_to_end_transfers
	seteq	pc,  lnk		@	if so,  return
  .else
	beq	hw_i2c_exit		@	if so,  reset and return
  .endif
	read16	rvb, rva, #i2c_status	@ rvb <- current status from I2C_STAT
	tst	rvb, #(1 << 4)		@ is module TxRdy?
	beq	i2c_w0			@	if not, jump to keep waiting
	lsr	rvc, rvc, #8		@ rvc <- remaining data to write
	and	rvb, rvc, #0xff		@ rvb <- byte to send now
	write16	rvb, rva, #i2c_thr	@ send reg or data byte via I2C_DATA
	sub	sv3, sv3, #4		@ sv3 <- updated number of bytes to send
	b	i2c_w0			@ jump to send next byte

	/* helper macro for hw_i2c_read */
.macro	hwi2rd	arg1, arg2
	ldr	rvc, =\arg1
	set	sv3, \arg2
	bl	hw_i2c_read
.endm

hw_i2c_read: /* read bytes from an i2c slave */
	@ Normally performed after hw_i2c_write which sets register to read from
	@ in:	rva <- base address of i2c module
	@ in:	rvc <- slave's i2c address
	@ in:	sv3 <- num data bytes to read = 1 or 2 (scheme int)
	@ out:	rvc <- bytes read
	@ mods:	rvb, rvc, sv3
	@ wait for bus not busy
	read16	rvb, rva, #i2c_status	@ rvb <- current status
	eq	rvb, #0
	beq	i2c_rc
	tst	rvb, #(1 << 2)		@ is module command-ready?
	beq	hw_i2c_read
	tst	rvb, #(1 << 12)
	bne	hw_i2c_read
	read16	rvb, rva, #i2c_status	@ rvb <- current status
	tst	rvb, #0x08		@ is module RdRdy (a state error)?
	beq	i2c_rc			@	if not, jump to continue
	@ clear Tx and Rx FIFOs
	write16	0x4040, rva, #i2c_buf	@ clear Tx and Rx FIFOs
	wait	1<<24			@ wait a bit
	read16	rvb, rva, #i2c_status	@ rvb <- current status
i2c_rc:	@ continue and configure module for reception as master
	write16	rvb, rva, #i2c_status	@ clear current status bits
	read16	rvb, rva, #i2c_sadr	@ rvb <- remote slave address frm I2C_SA
	eor	rvb, rvb, rvc
	tst	rvb, #0xff
	beq	i2c_r6
	@ wait a bit on slave address change
	wait	1<<24
	and	rvb, rvc, #0xff
	write16	rvb, rva, #i2c_sadr	@ set remote slave address in I2C_SA
i2c_r6:	@ continue with same slave address as prior i2c op
	tst	sv3, #1
	beq	i2c_r4
	asrs	rvb, rvc, #16
	seteq	rvb, 0x01
	setne	rvb, 0x02
	write16	rvb, rva, #i2c_cnt	@ set number of bytes to send in I2C_CNT
	dsb
	write16	0x8601, rva, #i2c_cset	@ set master Tx with Start in I2C_CON
i2c_r0:	@ write destination register
	r16wfbt	rva, #i2c_status, 4, 1	@ wait for module Tx rdy
	asr	rvc, rvc, #8		@ rvc <- remaining data to write
	and	rvb, rvc, #0xff		@ rvb <- byte to send now
	write16	rvb, rva, #i2c_thr	@ send reg or data byte via I2C_DATA
	tst	rvc, #(0xff << 24)
	bicne	rvc, rvc, #(0xff << 24)
	bne	i2c_r0
	r16wfbt	rva, #i2c_status, 2, 1	@ wait for module ready
i2c_r4:	@ wait for module ready
	int2raw	rvb, sv3
	write16	rvb, rva, #i2c_cnt	@ set number of bytes to read in I2C_CNT
	dsb
	write16	0x8403, rva, #i2c_cset	@ set master Rx w/Start+Stop in I2C_CON
	set	rvc, 0			@ rvc <- initial result
i2c_r2:	@ loop to read data bytes
	tst	sv3, #0x1c		@ nothing left to read?
  .ifndef reset_i2c_to_end_transfers
	seteq	pc,  lnk		@	if so,  return
  .else
	beq	hw_i2c_exit		@	if so,  reset and return
  .endif
	r16wfbt	rva, #i2c_status, 3, 1	@ wait for module RxRdy
	read16	rvb, rva, #i2c_rhr	@ rvb <- received data byte frm I2C_DATA
	and	rvb, rvb, #0xff		@ rvb <- received byte, trimmed
	tst	sv3, #(1 << 16)
	lslne	rvb, rvb, #8
	orr	rvc, rvc, rvb		@	if so,  rvc <- updated result
	sub	sv3, sv3, #4		@ sv3 <- updated num of bytes to receive
	eor	sv3, sv3, #(1 << 16)
	write16	1<<3, rva, #i2c_status	@ clear RxRdy bit in I2C_STAT
	b	i2c_r2			@ jump to get next byte

.ifdef reset_i2c_to_end_transfers

	/* flushing buffers doesn't work so end transfers with module reset */
hw_i2c_exit: @ wait a bit, then reset i2c and return
	dsb
	isb
	wait	1<<24
	b	hw_i2c_reset		@ reset and return 

.endif



