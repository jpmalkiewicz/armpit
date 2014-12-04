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

	/* STR7, STR9 */

.ifndef cortex

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
	eq	rvb, #0x94		@ Slave Read/Write -- my adrs recognzd, EV1-SR1, EVF BSY ADSL,#0x94
	beq	i2c_hw_slv_ini
	eq	rvb, #0x98		@ Slave Read  -- new data received,	EV2-SR1, EVF BSY BTF,#0x98
	beq	i2c_hw_rs_get
	eq	rvb, #0xB8		@ Slave Write -- master requests byte, EV3-SR1,EVF TRA BSY BTF,#0xB8
	beq	i2c_hw_ws_put
	tst	rvb, #0x1000		@ Slave Write -- NAK received, Tx done,	  EV3-1	- SR2, AF, #0x10
	bne	i2c_hw_ws_end
	tst	rvb, #0x0800		@ Slave Read  -- STOP or re-START received, EV4	- SR2, STOPF, #0x08
	bne	i2c_rs_end
	eq	rvb, #0x93		@ Master Read/Write -- bus now mstrd, EV5- SR1, EVF BSY MSL SB, #0x93
	beq	i2c_hw_mst_bus
	tst	rvb, #0x2000		@ Master Read/Write -- slave ackn. address, EV6	- SR2, ENDAD, #0x20
	bne	i2c_hw_mst_ini
	eq	rvb, #0x9A		@ Master Read -- new byte recvd, EV7 - SR1, EVF BSY BTF MSL, #0x9A
	beq	i2c_hw_rm_get
	eq	rvb, #0xBA		@ Master Write - slv ok to rx dat, EV8-SR1,EVF TRA BSY BTF MSL,#0xBA
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
	tbrfi	rvb, sv2, 0		@ r6  <- addrss of mcu to wrt/rd data to/from (scm int{w}/float{r})
	tst	rvb, #0x02		@ is this a write operation?
	beq	i2c_wm_ini
	b	i2c_rm_ini
	
hwi2we:	@ set busy status/stop bit at end of write as master
	@ on entry:	sv2 <- i2c[0/1] buffer address
	@ on entry:	sv3 <- i2c[0/1] base address
	@ on entry:	r7  <- #f
	tbrfi	rva, sv2, 3		@ r6  <- number of data bytes to send (raw int)
	eq	rva, #0			@ sending 0 bytes? (rdng as mastr & done writng addrss byt)
	seteq	pc,  lnk		@	if so,  return
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
	

	/* STM32F1 */

.ifdef cortex

_func_
hwi2cr:	@ write-out additional address registers, if needed
	@ on entry:	sv5 <- i2c[0/1]buffer
	@ on entry:	rva <- i2c[0/1] base address (also I2CONSET)
	@ modifies:	rvb
	set	rvb, #0			@ rvb <- 0 bytes to send (scheme int)
	tbsti	rvb, sv5, 3		@ store num byts to snd in i2c bfr[12]
	@ initiate i2c read/write, as master
	set	rvb, #0
	strh	rvb, [rva, #i2c_stat1]	@ clear SR1 clear-able error bits
	swi	run_normal		@ re-enable interrupts
	ldrh	rvb, [rva, #i2c_cr1]	@ rvb <- current content of I2C[0/1]_CR
	orr	rvb, rvb, #0x100	@ rvb <- contents orred with start bit
	strh	rvb, [rva, #i2c_cr1]	@ init bus mstrng, wrt strt to I2C0/1_CR
hwi2r0:	@ wait for mcu address and registers to have been transmitted
	swi	run_no_irq			@ disable interrupts
	tbrfi	rvb, sv5, 1		@ rvb <- data rdy stat frm i2cbuffer[4]
	eq	rvb, #f			@ is i2c dat rdy=#f (adrs hav been Txd)?
	it	eq
	seteq	pc,  lnk		@	if so, jump to continue
	swi	run_normal		@ re-enable interrupts
	b	hwi2r0			@ jump to keep waiting

_func_
hwi2ni:	@ initiate i2c read/write, as master
	@ possibly as a re-start condition during read (after writing adrs byts)
	@ on entry:	rva <- i2c[0/1] base address (also I2CONSET)
	@ modifies:	rvb
	set	rvb, #0
	strh	rvb, [rva, #i2c_stat1]	@ clear SR1 clear-able error bits
	ldrh	rvb, [rva, #i2c_cr1]	@ rvb <- current content of I2C[0/1]_CR
	orr	rvb, rvb, #0x100	@ rvb <- contents orred with start bit
	strh	rvb, [rva, #i2c_cr1]	@ init bus mstrng, wrt strt to I2C0/1_CR
	ldrh	rvb, [rva, #i2c_cr2]	@ rvb <- current content of I2C[0/1]_CR2
	orr	rvb, rvb, #0x0400	@ rvb <- strt gnrtng Tx ints, eg.restart
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
	biceq	rvb, rvb, #0x20		@	if so,  rvb <- clr bit 5, slave
	orrne	rvb, rvb, #0x20		@	if not, rvb <- set bit 5, master
	@ get rid of BTF	
	bic	rvb, rvb, #0x04
	set	pc,  lnk

_func_
i2c_hw_branch:	@ process interrupt
	eq	rvb, #0x02		@ Slave Rd/Wrt my adrs rcgnzd, EV1 ADDR
	beq	i2c_hw_slv_ini
	eq	rvb, #0x40		@ Slave Rd  -- new data rcvd, EV2 RxNE
	beq	i2c_hw_rs_get
	eq	rvb, #0x80		@ Slave Wrt -- mstr reqsts byte EV3 TxE
	beq	i2c_hw_ws_put
	tst	rvb, #0x0400		@ Slave Wrt -- NAK rcvd Tx done EV3-1 AF
	bne	i2c_hw_ws_end
	tst	rvb, #0x0010		@ Slave Rd  -- STOP rcvd, EV4 STOPF
	bne	i2c_hw_rs_end
	tst	rvb, #0x01		@ Mstr Rd/Wrt - bus now mstrd EV5 SB,MSL
	bne	i2c_hw_mst_bus
	eq	rvb, #0x21		@ Mstr Rd/Wrt - bus now mstrd EV5 SB,MSL
	beq	i2c_hw_mst_bus
	tst	rvb, #0x02		@ Mstr Rd/Wrt - slave ackn adrs EV6 ADDR
	bne	i2c_hw_mst_ini
	eq	rvb, #0x60		@ Mstr Rd -- new byte rcvd EV7 RxNE,MSL
	beq	i2c_hw_rm_get
	eq	rvb, #0xA0		@ Mstr Wrt slv ok to rcv dat EV8 TxE,MSL
	beq	i2c_wm_put
	set	pc,  lnk
	
_func_
i2c_hw_slv_ini: @ Slave Read/Write -- my address recognized  (EV1)
	tbrfi	rva, sv2, 0		@ r6  <- channel-busy status
	eq	rva, #f			@ is channel free?
	itT	eq
	seteq	rva, #i0		@	if so,  rva <- 0 (schm int)
	tbstieq rva, sv2, 0		@	if so,  store 0 as channel-busy
	set	rva, #0			@ r6  <- 0
	tbsti	rva, sv2, 4		@ store 0 as num bytes sent/received
	b	i2cxit

_func_
i2c_hw_rs_get:	
	tbrfi	rvb, sv2, 4		@ r7  <- number of bytes sent
	eq	rvb, #0
	itT	eq
	tbstieq rvb, sv2, 2		@	if so,  store 0 dat rcvd so far
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
	tbstieq rva, sv2, 1		@	if so,  store 0 dat-not-rdy/adrs
	ldreq	rva, =eof_char		@	if so,  rva <- eof-character
	streq	rva, [glv, sv1]		@	if so,  store eof-char to snd
	seteq	rva, #4			@	if so,  rva <- 4=num byts to snd
	it	eq
	tbstieq rva, sv2, 3		@	if so,  store 4=num byts to snd
	b	i2c_ws_put

_func_
i2c_hw_ws_end:	@ Slave Write -- NAK received, Tx done,	  EV3-1	- SR2, AF, #0x10
	ldrh	rvb, [sv3, #i2c_stat1]	@ rvb <- current cntnt of I2C[0/1]_STAT1
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
	tbrfi	rva, sv2, 0		@ rva <- adrs of mcu to snd dat to (int)
	lsr	rva, rva, #1		@ rva <- mcu-id as int -- ends with 0
	strb	rva, [sv3, #i2c_thr]	@ set address of mcu to send data to
	@ wait for target to be addressed (avoids getting a TxE int before then)
	@ a bit risky if remote device doesn't exist we get jammed inside int!!!
	set	rvb, #0x1000000
i2c_hw_mst_bwt:
	subs	rvb, rvb, #1
	beq	i2cxit
	ldrh	rva, [sv3, #i2c_stat1]
	tst	rva, #0x02
	beq	i2c_hw_mst_bwt
_func_
i2c_hw_mst_ini: @ Master Read/Write -- slave aknowledged address (EV6)
	ldrh	rvb, [sv3, #i2c_stat2]	@ rvb <- I2C Status from SR2 (clear int)
	tbrfi	rvb, sv2, 0		@ rva <- adr of mcu to wrt/rd dat to/frm
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
	ldrh	rvb, [sv3, #i2c_stat1]	@ rvb <- current content of I2C0/1_STAT1
	tst	rvb, #0x84
	beq	hwi2ww
	tbrfi	rva, sv2, 3		@ r6  <- num data byts to send (raw int)
	eq	rva, #0			@ were we sendng 0 byts (rd as mstr/don)
	beq	hwi2wv
	set	rvb, #f			@ rvb <- #f
	tbsti	rvb, sv2, 0		@ set busy status to #f (transfer done)
	ldrh	rvb, [sv3, #i2c_cr1]	@ rvb <- current content of I2C[0/1]_CR1
	orr	rvb, rvb, #0x0200	@ rvb <- contents orred with stop bit
	strh	rvb, [sv3, #i2c_cr1]	@ initiate stop (wrt stop to I2C0/1_CR1)
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
	bne	i2c_rm_get		@	if so,  jump to normal read
	b	i2c_rm_end		@ jmp to end rd as mstr (nack prior byt)

_func_
hwi2re:	@ set stop bit if needed at end of read-as-master
	ldrh	rvb, [sv3, #i2c_cr1]	@ rvb <- current content of I2C[0/1]_CR1
	orr	rvb, rvb, #0x0600	@ rvb <- contents ord w/stop + ack bit
	strh	rvb, [sv3, #i2c_cr1]	@ initiate stop (write stop to I2Cn_CR1)

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
i2putp:	@ Prologue:	write addtnl adrs byts to i2c, from bfr/r12 (prologue)
	@ check if address bytes need sending
	@ if not, return via lnk unless #bytes to write is zero
	@		-> if so, jump to i2pute o2 i2c_wm_end
	@ if so, write them out and subtract count, then skip i2putc
	@		-> jump to i2pute
	@ with link set to i2cxit
	tbrfi	rva, sv2, 1		@ rva <- num addtnl adrs byts to snd
	eq	rva, #i0		@ no more address bytes to send?
	itTT	eq
	tbrfieq rva, sv2, 3		@	if so,  rva <- num byts to send
	tbrfieq rvb, sv2, 4		@	if so,  rvb <- num byts sent raw
	eqeq	rva, rvb		@	if so,  done sending data?
	beq	i2c_wm_end		@		if so, stop or restart
	tbrfi	rvb, sv2,  1		@ rvb <- num adrs bytes remaining to snd
	eq	rvb, #i0		@ done sending address bytes?
	it	eq	
	seteq	pc,  lnk		@	if so,  return
	and	rvb, rvb, #0x03
	eq	rvb, #i0
	bne	i2cxit	
	tbrfi	rvb, sv2,  1		@ rvb <- num adrs byts remnng to snd
	sub	rvb, rvb, #4		@ rvb <- updtd num adrs byts to snd
	tbsti	rvb, sv2, 1		@ stor updtd num adrs byts to snd
	add	rva, sv2, #8		@ rva <- adrs of addtl adrs byts in bfr
	lsr	rvb, rvb, #2
	ldrb	rva, [rva, rvb]		@ rva <- next address byte to send
	strb	rva, [sv3, #i2c_thr]	@ put next data byte in I2C data reg
	b	i2cxit

_func_
i2pute:	@ Epilogue:	set completion status if needed (epilogue)
	tbrfi	rva, sv2, 3		@ rva <- num data bytes to snd (raw int)
	tbrfi	rvb, sv2, 4		@ rvb <- num data bytes sent (raw int)
	eq	rva, rvb		@ done sending?
	beq	i2c_wm_end		@	if so,  jump to end transfer
	set	pc,  lnk

.endif

.ltorg


