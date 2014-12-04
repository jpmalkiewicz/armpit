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

		configure CPO

------------------------------------------------------------------------------*/

_func_
cpocfg:	/* configure cpo */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- [*not set on LPC4300*]
	@ ret:	via lnk
  .ifdef cpo_default_io
	@ copy coprocessor code to coprocessor memory
	@ then start coprocessor
	@
	@ Note: total code size (M4 + M0) is less than 64KB (32KB on LPC4357)
	@
	ldr	rva, =sys_config+0x404	@ rva <- M0APPMEMMAP
	ldr	rvc, =RAM_bank_4	@ rvc <- address of RAM bank 4
	str	rvc, [rva]		@ M0APPMEMMAP <- set M0 code start adrs
	ldr	rva, =_enddata		@ M0 code start (after M4) frm link file
	ldr	cnt, =(boot_ram_size-1)	@ cnt <- mask to end code copy (32/64KB)
cpocpy:	@ code copy loop
	ldr	rvb, [rva]
	str	rvb, [rvc]
	add	rva, rva, #4
	add	rvc, rvc, #4
	tst	rva, cnt
	beq	cpocpd
	ldr	rvb, =RAM_bank_5	@ rvc <- RAM bank 5 (end of bank 4)
	eq	rvb, rvc
	bne	cpocpy
cpocpd:	@ start the coprocessor (M0)
	ldr	rva, =0x40053104
	set	rvb, 0
	str	rvb, [rva]
  .endif @ .ifdef cpo_default_io
	@ return
	set	pc,  lnk

/*------------------------------------------------------------------------------

		response to CPO interrupts 

------------------------------------------------------------------------------*/

_func_
cpohwrdywt: @ wait on message completion from coprocessor
	ldr	rvc, =cpo_tx_status	@ rva <- M4TXEVENT register address
	ldr	rva, [rvc]		@ rva <- M4TXEVENT contents
	eq	rva, #0			@ has coprocessor cleared last event?
	bne	cpohwrdywt		@	if not, jump back to wait
	set	pc,  lnk

_func_
cpohwsig: @ signal the coprocessor (eg. for write)
	dsb
	isb
	sev				@ send event to M0 core
	set	pc,  lnk

_func_
cpohwclri: @ clear CPO interrupt on normal isr exit (M0TXEVENT)
	set	rvb, 0
	ldr	rva, =cpo_rxint_clear	@ rva <- M0TXEVENT register address
	str	rvb, [rva]		@ clear the M0 TX event (ack interrupt)
	set	pc,  lnk

_func_
cpohwbrki: @ clear CPO interrupt on break/ctrl-c (M0TXEVENT)
	set	rvc, 0
	ldr	rva, =cpo_rxint_clear	@ rva <- M0TXEVENT register address
	str	rvc, [rva]		@ clear the M0 TX event (ack interrupt)
	set	pc,  lnk

/*------------------------------------------------------------------------------

		initiate CPO character write from scheme (port function)

------------------------------------------------------------------------------*/

_func_
cpohwrc: @ initiate cpo write, re-enable ints and return
	@ modifies:	rva, rvc
	@ returns via:	lnk

	@ wait on message completion from coprocessor
	ldr	rvc, =cpo_tx_status	@ rva <- M4TXEVENT register address
	ldr	rva, [rvc]		@ rva <- M4TXEVENT contents
	eq	rva, #0			@ has coprocessor cleared last event?
	bne	cpohwrc			@	if not, jump back to wait
	ldr	rva, =cpo_tx_msg
	set	rvc, 1			@ rvc <- 1, new message = write
	str	rvc, [rva]		@ set message in Tx-MSG
	@ signal the write to coprocessor
	dsb
	isb
	sev				@ send event to M0 core
chwrc1:	@ wait for acknowledge
	ldr	rva, =cpo_rx_msg
	ldr	rvc, [rva]		@ set message in Tx-MSG
	eq	rvc, #0x81
	bne	chwrc1
	@ clear tx-msg (handshake) (prevent M0 isr re-entry on interrupt jitter)
	ldr	rva, =cpo_tx_msg
	set	rvc, 0			@ rvc <- 0, new message = do nothing
	str	rvc, [rva]		@ set message in Tx-MSG
	@ return
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk



