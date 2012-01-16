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


usbhwDeviceStatus: @ return device status in rvb
	ldr	rva, =usb_base
	ldrb	rvb, [rva, #0x0a]
	set	pc,  lnk

usbhwReset:
	ldr	rva, =usb_base
	ldrb	rvb, [rva, #0x01]
	tst	rvb, #0x10
	seteq	rvb, #0			@ rvb <- 0
	setne	rvb, #1
	ldr	rva, =USB_FSHS_MODE
	str	rvb, [rva]		@ indicate that USB is not yet in HS mode
	set	rvb, #0			@ rvb <- 0
	ldr	rva, =USB_SETUP_BUFFER
	str	rvb, [rva]		@ rvb <- reqtyp(8), request(8), val(16) or 0xFF if Status Phase	
	set	pc,  lnk

usbhwRemoteWakeUp:	
	set	pc,  lnk

usbhwEndpointStatus: @ get status of EP whose interrupt is in sv2 into sv3
	ldr	rva, =usb_base
	set	sv2, sv1			@ sv2 <- Endpoint Interrupt Status (shifted)
	set	sv3, sv2
	tst	sv2, #1				@ EP0 (control) interrupt?
	seteq	pc,  lnk			@	if not, return with int in sv2, sv3
	set	rvb, #0
	strb	rvb, [rva, #usb_index_reg]
ushw_0:	ldrb	rvb, [rva, #usb_index_reg]
	eq	rvb, #0
	bne	ushw_0
	ldrh	sv3, [rva, #usb_ctl_stat]
	tst	sv3, #0x10
	setne	sv2, #0
	setne	rvb, #0x80
	strhne	rvb, [rva, #usb_ctl_stat]	@ set SERVICED_SETUP_END in EP0_CSR
	setne	pc,  lnk
	tst	sv3, #0x04			@ responding to sent_stall?
	setne	sv2, #0
	strhne	sv2, [rva, #usb_ctl_stat]
	setne	pc,  lnk
	tst	sv3, #0x11			@ is either OUT_PKT_RDY or SETUP_END set?
	seteq	sv2, #2				@	if not, sv2 <- control In indicator
	setne	sv2, #1				@	if so,  sv2 <- control out indicator
	mvn	sv3, sv3
	set	pc,  lnk
	
usbhwClearRxBk:
	set	pc,  lnk

usbStall: @ stall endpoint 1 (phys 1, log 0, aka 0x80) -- i.e. Control IN
	set	env, #UsbControlOutEP		@ env <- Control OUT EndPoint
	ldr	rva, =usb_base
	strb	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_q:	ldrb	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_q
	set	rvb, #0x68
	strh	rvb, [rva, #usb_ctl_stat]	@ set SERVICED_OUT_PKT_RDY & SEND_STALL	
	b	usbEPx

rdEP:	@ read from endpoint in env to buffer in dts with count in cnt
	ldr	rva, =usb_base
	strb	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_7:	ldrb	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_7
	ldrh	cnt, [rva, #usb_rcvd_cnt]	@ cnt <- byte count from OUT_FIFO_CNT1_REG
	add	rva, rva, #0x20			@ rva <- address of base of EP FIFOs
	set	rvb, #0
usbSEZ:	
	sub	rvc, cnt, rvb
	cmp	rvc, #4
	bmi	usbSEX
	cmp	rvb, cnt
	bpl	usbSEX
	ldr	rvc, [rva, env, lsl #2]		@ rvc <- data byte from EPn_FIFO
	str	rvc, [dts, rvb]
	add	rvb, rvb, #4
	b	usbSEZ
usbSEX:
	eq	rvb, cnt
	ldrbne	rvc, [rva, env, lsl #2]		@ store it in EPn_FIFO
	strbne	rvc, [dts, rvb]			@ rvc <- data from buffer
	addne	rvb, rvb, #1
	bne	usbSEX
	cmp	env, #2
	bmi	usbhwrdEP0x
	ldr	rva, =usb_base
	set	rvb, #0
	strh	rvb, [rva, #0x16]		@ clear OUT_PKT_RDY bit in OUT_CSR1_REG
	set	pc,  lnk
usbhwrdEP0x: @ EP0 special processing
	@ assumes payload is always 8-byte or less for control EP (i.e. sets dataend)
	ldr	rva, =usb_base
	set	rvb, #0x48
	strh	rvb, [rva, #usb_ctl_stat]	@ set SERVICED_OUTPKT_RDY bit in EP0_CSR
	set	pc,  lnk

wrtEPU:	@ clear EP rcv interrupt then write data to Bulk In Endpoint
	@ (write cnt bytes starting at dts to enpoint in env)
	b	wrtEP
	
wrtEP:	@ write data to Control In Endpoint (write cnt bytes starting at dts to enpoint in env)
	@ write data to Bulk In Endpoint (write cnt bytes starting at dts to enpoint in env)
	ldr	rva, =usb_base
	strb	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_8:	ldrb	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_8
	eq	env, #0
	seteq	rvb, #0
	setne	rvb, #1
ushw_t:	ldrh	rvc, [rva, #usb_ctl_stat]
	tst	rvc, rvb
	bne	ushw_t	
	add	rva, rva, #0x20			@ rva <- address of base of EP FIFOs
	set	rvb, #0
ushw_9:	
	sub	rvc, cnt, rvb
	cmp	rvc, #4
	bmi	ushw_a
	cmp	rvb, cnt
	bpl	ushw_a
	ldr	rvc, [dts, rvb]			@ rvc <- data from buffer
	str	rvc, [rva, env, lsl #2]		@ store it in EPn_FIFO
	add	rvb, rvb, #4
	b	ushw_9
ushw_a:
	eq	rvb, cnt
	ldrbne	rvc, [dts, rvb]			@ rvc <- data from buffer
	strbne	rvc, [rva, env, lsl #2]		@ store it in EPn_FIFO
	addne	rvb, rvb, #1
	bne	ushw_a
	cmp	env, #2
	bmi	usbhwrtEP0x
	ldr	rva, =usb_base
	set	rvb, #1
	strh	rvb, [rva, #usb_ctl_stat]	@ set IN_PKT_RDY bit in TXCSR
	set	pc,  lnk
usbhwrtEP0x: @ EP0 special processing
	set	rvb, #0
	ldr	rva, =USB_CHUNK
	str	rvb, [rva]		@ cnt <- how many bytes will remain to be sent
	ldr	rva, =usb_base
	set	rvb, #0x0A			@ rvb <- IN_PKT_RDY
	strh	rvb, [rva, #usb_ctl_stat]	@ set IN_PKT_RDY bit in EP0_CSR
	set	pc,  lnk
	
usbhwCleariTX: @ clear the txcomp interrupt
	set	pc,  lnk

usbhwwrtEPA:
	b	wrtEP
	
usbhwixx:
	b	usbEPx

usbhwCIi:  @ Control IN interrupt response
	ldr	rva, =USB_SETUP_BUFFER
	ldr	rva, [rva]
	eq	rva, #0xff
	beq	usbEPx
	ldr	rva, =usb_base
	strb	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_b:	ldrb	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_b
	add	rva, rva, #0x20			@ rva <- address of base of EP FIFOs
	set	rvb, #0
ushw_c:	
	sub	rvc, cnt, rvb
	cmp	rvc, #4
	bmi	ushw_d
	cmp	rvb, cnt
	bpl	ushw_d
	ldr	rvc, [dts, rvb]			@ rvc <- data from buffer
	str	rvc, [rva, env, lsl #2]		@ store it in EPn_FIFO
	add	rvb, rvb, #4
	b	ushw_c
ushw_d:	
	eq	rvb, cnt
	ldrbne	rvc, [dts, rvb]			@ rvc <- data from buffer
	strbne	rvc, [rva, env, lsl #2]		@ store it in EPn_FIFO
	addne	rvb, rvb, #1
	bne	ushw_d
	ldr	rva, =USB_CHUNK
	ldr	rvb, [rva]		@ cnt <- how many bytes remain to be sent
	ldr	rva, =usb_base
	eq	rvb, #0
	setne	rvb, #0x02			@	if not, rvb <- IN_PKT_RDY
	strhne	rvb, [rva, #usb_ctl_stat]	@ set IN_PKT_RDY bit in EP0_CSR
	setne	pc,  lnk
	set	rvb, #0x0a
	strh	rvb, [rva, #usb_ctl_stat]
	b	usbSOx
	set	pc,  lnk
	
usbhwSetup: @ see also rdEP, here, env = 0
	ldr	rva, =usb_base
	strb	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_1:	ldrb	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_1
	ldrh	cnt, [rva, #usb_rcvd_cnt]	@ cnt <- byte count from OUT_FIFO_CNT1_REG
	add	rva, rva, #0x20			@ rva <- address of base of EP FIFOs
	set	rvb, #0
ushw_2:	cmp	rvb, cnt
	bpl	ushw_3
	ldr	rvc, [rva, env, lsl #2]		@ rvc <- data byte from EPn_FIFO
	str	rvc, [dts, rvb]
	add	rvb, rvb, #4
	b	ushw_2
ushw_3:	ldr	rva, =usb_base
	set	rvb, #0x40
	strh	rvb, [rva, #usb_ctl_stat]	@ set SERVICED_OUT_PKT_RDY to clear OUT_PKT_RDY bit in EP0_CSR
	ldr	rva, =USB_SETUP_BUFFER
	ldr	rvb, [rva]		@ rvb <- reqtyp(8), request(8), val(16)
	tst	rvb, #0x80		@ is direction from device to host (i.e. bit 7 set in reqtyp) ?
	setne	pc,  lnk		@	if so,  return
	ldr	cnt, [rva, #4]		@ cnt <- index(16), length(16)
	lsr	cnt, cnt, #16		@ cnt <- length of data to transfer (in bytes)
	eq	cnt, #0
	seteq	pc,  lnk		@ may need to set data-end too here(?) -> 0-length packet reading
	ldr	rva, =usb_base
jadrwt:	ldrh	rvc, [rva, #0x02]
	tst	rvc, #1
	beq	jadrwt
	ldrh	rvc, [rva, #usb_ctl_stat]	@ set SERVICED_OUT_PKT_RDY to clear OUT_PKT_RDY bit in EP0
	b	usbRQS

usbhwStatusOut:	@ Control OUT Interrupt, Status OUT Phase
	set	env, #UsbControlOutEP		@ env <- Control OUT EndPoint	
	ldr	rva, =usb_base
	strb	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_e:	ldrb	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_e
	set	rvb, #0x80			@ rvb <- SERVICED_SETUP_END
	strh	rvb, [rva, #usb_ctl_stat]	@ set SERVICED_SETUP_END in EP0_CSR
	b	usbEPx

usbhwUnstallEP:	@ Unstall the EndPoint in sv5, jump to Status IN
	set	pc,  lnk

usbhwStallEP: @ Stall the EndPoint in r5
	set	pc,  lnk

usbhwDGD: @ 9.4.3 Get Descriptor of Device Standard request	
	ldr	rva, =usb_base
	strb	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_f:	ldrb	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_f
	add	rva, rva, #0x20			@ rva <- address of base of EP FIFOs
	set	rvb, #0
ushw_g:	
	sub	rvc, cnt, rvb
	cmp	rvc, #4
	bmi	ushw_h
	cmp	rvb, cnt
	bpl	ushw_h
	ldr	rvc, [dts, rvb]			@ rvc <- data from buffer
	str	rvc, [rva, env, lsl #2]		@ store it in EPn_FIFO
	add	rvb, rvb, #4
	b	ushw_g
ushw_h:
	eq	rvb, cnt
	ldrbne	rvc, [dts, rvb]			@ rvc <- data from buffer
	strbne	rvc, [rva, env, lsl #2]		@ store it in EPn_FIFO
	addne	rvb, rvb, #1
	bne	ushw_h
	ldr	rva, =usb_base
	set	rvb, #0x02
	strh	rvb, [rva, #usb_ctl_stat]	@ set IN_PKT_RDY bit in EP0_CSR
	b	usbEPx

usbSOx:	@ Prepare setup buffer for Status OUT Phase 
	ldr	rva, =USB_SETUP_BUFFER
	ldr	rvb, =0xFF
	str	rvb, [rva]
	b	usbEPx

usbhwEGS: @ Get Status of Endpoint in sv5 into rvb
	ldr	rva, =usb_base
	set	rvb, #1
	set	pc,  lnk

usbhwSetAddress: @ Set Device to Address in sv5
	@ USB Status IN  exit -- write null packet to   EP 1 (phys, aka 0x80)
	ldr	rva, =USB_SETUP_BUFFER		@ rva <- address of setup buffer
	ldr	sv5, [rva]			@ sv5 <- reqtyp(8), request(8), val(16)
	lsr	sv5, sv5, #16			@ sv5 <- address = val(16)	
	ldr	rva, =usb_base
	set	env, #UsbControlInEP		@ env <- Control IN EndPoint
	strb	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_j:	ldrb	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_j
	set	rvb, #0x08			@	if so,  rvb <- IN_PKT_RDY and DATA_END
	strh	rvb, [rva, #usb_ctl_stat]	@ set IN_PKT_RDY bit in EP0_CSR
iadrwt:	ldrh	rvb, [rva, #0x02]
	tst	rvb, #1
	beq	iadrwt
	set	env, #UsbControlInEP		@ env <- Control IN EndPoint
	strb	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_k:	ldrb	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_k
	ldrh	rvc, [rva, #usb_ctl_stat]	@ set IN_PKT_RDY bit in EP0_CSR
	strb	sv5, [rva, #usb_daddr]		@ set address
	b	usbEPx

usbhwSIX:
	ldr	rva, =USB_SETUP_BUFFER
	ldr	rvb, =0xFF
	str	rvb, [rva]
	set	rvb, #0
	ldr	rva, =USB_CHUNK
	str	rvb, [rva]		@ cnt <- how many bytes will remain to be sent
	ldr	rva, =usb_base
	set	env, #UsbControlInEP		@ env <- Control IN EndPoint
	strb	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_m:	ldrb	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_m
	set	rvb, #0x08			@ rvb <- IN_PKT_RDY
	strh	rvb, [rva, #usb_ctl_stat]	@ set IN_PKT_RDY bit in EP0_CSR
	ldr	rva, =usb_base
kadrwt:	ldrh	rvb, [rva, #0x02]
	tst	rvb, #1
	beq	kadrwt	
	ldrh	rvc, [rva, #usb_ctl_stat]	@ set IN_PKT_RDY bit in EP0_CSR
	set	pc,  lnk

usbhwDeconfigure: @ Deconfigure the device
	@ clear the readbuffer
	ldr	rva, =BUFFER_START
	vcrfi	rva, rva, READ_BF_offset
	set	rvb, #i0
	vcsti	rva, 0, rvb
	@ set uart to generate Rx interrupts
	ldr	rva, =uart0_base
	set	rvb, #0x01
	str	rvb, [rva, #0x04]	@ UCON0 <- enable Tx and Rx, and error interrupt
	@ set default i/o port to uart
	ldr	rvb, =vuart0
	vcsti	glv, 4, rvb			@ default input/output port model
	set	pc,  lnk
	
usbhwConfigure: @ Configure the device
	@ stop uart from generating Rx interrupts (they cause noise on shared READBUFFER)
	ldr	rva, =uart0_base
	set	rvb, #0
	str	rvb, [rva, #0x04]		@ UCON0 <- disable Tx and Rx, and error Interrupt
	@ clear the readbuffer
	ldr	rva, =BUFFER_START
	vcrfi	rva, rva, READ_BF_offset
	set	rvb, #i0
	vcsti	rva, 0, rvb
	@ configure USB
	ldr	rva, =usb_base
	set	env, #UsbBulkOutEP
	strb	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_x:	ldrb	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_x
	set	rvb, #0
	strh	rvb, [rva, #0x12]		@ set direction to OUT in IN_CSR2_REG
	strh	rvb, [rva, #0x16]		@ set direction to OUT in IN_CSR2_REG
	ldr	rvb, =USB_FSHS_MODE
	ldr	rvb, [rvb]
	eq	rvb, #0
	seteq	rvb, #64
	setne	rvb, #512
	strh	rvb, [rva, #0x10]		@ set packet size to 64 bytes in MAXP_REG
	strh	rvb, [rva, #0x14]		@ set packet size to 64 bytes in MAXP_REG
	set	env, #UsbBulkInEP
	strb	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_y:	ldrb	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_y
	set	rvb, #0x2000
	strh	rvb, [rva, #0x10]		@ set direction to IN in IN_CSR2_REG
	ldr	rvb, =USB_FSHS_MODE
	ldr	rvb, [rvb]
	eq	rvb, #0
	seteq	rvb, #64
	setne	rvb, #512
	strh	rvb, [rva, #0x10]		@ set packet size to 64 bytes in MAXP_REG
	strh	rvb, [rva, #0x14]		@ set packet size to 64 bytes in MAXP_REG
	@ set default i/o port to usb
	ldr	rvb, =vusb
	vcsti	glv, 4, rvb			@ default input/output port model
	set	pc,  lnk

usbhwr:	@ write writebuffer to USB Bulk IN endpoint 5 (phys 5, log 2, aka 0x82) (eg. section 9.14)
	@ on entry:	sv5 <- USB Write Buffer
	@ on entry:	rvb <- number of bytes to send
	@ modifies:	sv3, rva, rvb, rvc
	@ write data packet to send
	ldr	rva, =usb_base			@ rva <- USB base register
	lsl	rvb, rvb, #2
	orr	sv3, rvb, #i0
	set	rvc, #UsbBulkInEP
	strb	rvc, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_w:	ldrb	rvb, [rva, #usb_index_reg]
	eq	rvb, rvc
	bne	ushw_w
	add	rva, rva, #0x20
	add	rva, rva, rvc, lsl #2		@ rva <- USB FIFO address
	add	sv3, sv3, #32
	set	rvc, #i0
	add	rvc, rvc, #32
usbwr1:	eq	rvc, sv3
	ldrbne	rvb, [sv5, rvc, LSR #2]		@ rvb <- next data byte
	strbne	rvb, [rva]			@ write data to Transmit register
	addne	rvc, rvc, #4			@ rvc <- updated offset to data source address
	bne	usbwr1
	ldr	rva, =usb_base
	set	rvb, #1
	strh	rvb, [rva, #usb_ctl_stat]	@ set IN_PKT_RDY bit in TXCSR
	b	usbwcn				@ return


