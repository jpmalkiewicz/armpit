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

.ltorg
	
usbhwDeviceStatus: @ return device status in rvb
	set	rvb, sv1			@ rvb <- Device Interrupt Status (reset, resume, suspend)
	set	pc,  lnk

usbhwReset:
	ldr	rva, =USB_SETUP_BUFFER
	set	rvb, #0
	str	rvb, [rva]		@ rvb <- reqtyp(8), request(8), val(16) or 0xFF if Status Phase	
	ldr	rva, =usb_base
	set	rvb, #0x80			@ rvb <- bit to enable USB at address 0
	str	rvb, [rva, #usb_daddr]
	set	pc,  lnk

usbhwRemoteWakeUp: @ suspend/wakeup
	set	pc,  lnk

usbhwEndpointStatus: @ get status of EP whose interrupt is in sv2 into sv3
	ldr	rva, =usb_base
	set	sv2, sv1			@ sv2 <- Endpoint Interrupt Status
	set	sv3, sv2
	tst	sv2, #1				@ EP0 (control) interrupt?
	seteq	pc,  lnk			@	if not, return with int in sv2, sv3
	set	rvb, #0
	str	rvb, [rva, #usb_index_reg]
ushw_0:	ldr	rvb, [rva, #usb_index_reg]
	eq	rvb, #0
	bne	ushw_0
	ldr	sv3, [rva, #usb_ctl_stat]
	tst	sv3, #0x11			@ is either OUT_PKT_RDY or SETUP_END set?
	seteq	sv2, #2				@	if not, sv2 <- control In indicator
	setne	sv2, #1				@	if so,  sv2 <- control out indicator
	mvn	sv3, sv3
	set	pc,  lnk
	ldr	rva, =USB_SETUP_BUFFER
	ldr	rvb, [rva]		@ rvb <- reqtyp(8), request(8), val(16) or 0xFF if Status Phase
	ldr	rva, =0x2021
	eq	rva, rvb
	biceq	sv3, sv3, #0x110
	set	pc,  lnk
	
usbhwClearRxBk:
	set	pc,  lnk

rdEP:	@ read from endpoint in env to buffer in dts with count in cnt
	ldr	rva, =usb_base
	str	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_7:	ldr	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_7
	ldr	cnt, [rva, #usb_rcvd_cnt]	@ cnt <- byte count from OUT_FIFO_CNT1_REG
	add	rva, rva, #0x80			@ rva <- address of base of EP FIFOs
	set	rvb, #0
usbSEZ:	cmp	rvb, cnt
	bpl	usbSEX
	ldr	rvc, [rva, env, lsl #2]		@ rvc <- data byte from EPn_FIFO
	strb	rvc, [dts, rvb]
	add	rvb, rvb, #1
	b	usbSEZ
usbSEX:
	cmp	env, #2
	bmi	usbhwrdEP0x
	ldr	rva, =usb_base
	set	rvb, #0
	str	rvb, [rva, #0x50]		@ clear OUT_PKT_RDY bit in OUT_CSR1_REG
	set	pc,  lnk
usbhwrdEP0x: @ EP0 special processing	
	ldr	rva, =usb_base
	set	rvb, #0x40			@ rvb <- SERVICED_OUT_PKT_RDY
	eq	cnt, #0				@ zero bytes to read?
	orreq	rvb, rvb, #0x08			@	if so,  rvb <- SERVICED_OUT_PKT_RDY and DATA_END
	str	rvb, [rva, #usb_ctl_stat]	@ set SERVICED_OUTPKT_RDY bit in EP0_CSR
	set	pc,  lnk
	
usbStall: @ stall endpoint 1 (phys 1, log 0, aka 0x80) -- i.e. Control IN
	set	env, #UsbControlOutEP		@ env <- Control OUT EndPoint
	ldr	rva, =usb_base
	str	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_q:	ldr	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_q
	set	rvb, #0x68
	str	rvb, [rva, #usb_ctl_stat]	@ set SERVICED_OUT_PKT_RDY & SEND_STALL	
	b	usbEPx

wrtEPU:	@ clear EP rcv interrupt then write data to Bulk In Endpoint
	@ (write cnt bytes starting at dts to enpoint in env)
	ldr	rva, =usb_base	
	str	sv1, [rva, #usb_iclear_ep]	@ clear USB EP interrupt register
	b	wrtEP
	
	
wrtEP:	@ write data to Control In Endpoint (write cnt bytes starting at dts to enpoint in env)
	@ write data to Bulk In Endpoint (write cnt bytes starting at dts to enpoint in env)
	ldr	rva, =usb_base
	str	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_8:	ldr	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_8
	eq	env, #0
	seteq	rvb, #0
	setne	rvb, #1
ushw_t:	ldr	rvc, [rva, #usb_ctl_stat]
	tst	rvc, rvb
	bne	ushw_t	
	add	rva, rva, #0x80			@ rva <- address of base of EP FIFOs
	set	rvb, #0
ushw_9:	cmp	rvb, cnt
	bpl	ushw_a
	ldrb	rvc, [dts, rvb]			@ rvc <- data from buffer
	str	rvc, [rva, env, lsl #2]		@ store it in EPn_FIFO
	add	rvb, rvb, #1
	b	ushw_9
ushw_a:	
	cmp	env, #2
	bmi	usbhwrtEP0x
	ldr	rva, =usb_base
	set	rvb, #1
	str	rvb, [rva, #0x44]		@ set IN_PKT_RDY bit in IN_CSR1_REG
	set	pc,  lnk
usbhwrtEP0x: @ EP0 special processing
	set	rvb, #0
	ldr	rva, =USB_CHUNK
	str	rvb, [rva]		@ cnt <- how many bytes will remain to be sent
	ldr	rva, =usb_base
	set	rvb, #0x0A			@ rvb <- IN_PKT_RDY
	str	rvb, [rva, #usb_ctl_stat]	@ set IN_PKT_RDY bit in EP0_CSR
	set	pc,  lnk
	
usbhwCleariTX: @ clear the txcomp interrupt
	set	pc,  lnk

usbhwwrtEPA:
	b	wrtEP
	
usbhwixx:
	b	usbEPx

usbhwCIi:  @ Control IN interrupt response
	ldr	rva, =usb_base
	str	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_b:	ldr	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_b
	str	sv1, [rva, #usb_iclear_ep]	@ clear USB EP interrupt register
	add	rva, rva, #0x80			@ rva <- address of base of EP FIFOs
	set	rvb, #0
ushw_c:	cmp	rvb, cnt
	bpl	ushw_d
	ldrb	rvc, [dts, rvb]			@ rvc <- data from buffer
	str	rvc, [rva, env, lsl #2]		@ store it in EPn_FIFO
	add	rvb, rvb, #1
	b	ushw_c
ushw_d:	
	ldr	rva, =USB_CHUNK
	ldr	rvb, [rva]		@ cnt <- how many bytes remain to be sent
	ldr	rva, =usb_base
	eq	rvb, #0
	setne	rvb, #0x02			@	if not, rvb <- IN_PKT_RDY
	strne	rvb, [rva, #usb_ctl_stat]	@ set IN_PKT_RDY bit in EP0_CSR
	setne	pc,  lnk
	eq	rvb, cnt
	setne	rvb, #0x0A			@	if not, rvb <- IN_PKT_RDY
	strne	rvb, [rva, #usb_ctl_stat]	@ set IN_PKT_RDY bit in EP0_CSR
	set	pc,  lnk
	
usbhwSetup: @ see also rdEP, here, env = 0
	ldr	rvb, [dts]		@ rvb <- reqtyp(8), request(8), val(16) or 0xFF if Status Phase
	ldr	rva, =0x2021
	eq	rva, rvb
	ldreq	cnt, [dts, #4]		@ cnt <- index(16), length(16)
	lsreq	cnt, cnt, #16		@ cnt <- length of data to transfer (in bytes)
	beq	usbRQS
	ldr	rva, =usb_base
	str	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_1:	ldr	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_1
	ldr	cnt, [rva, #usb_rcvd_cnt]	@ cnt <- byte count from OUT_FIFO_CNT1_REG
	add	rva, rva, #0x80			@ rva <- address of base of EP FIFOs
	set	rvb, #0
ushw_2:	cmp	rvb, cnt
	bpl	ushw_3
	ldr	rvc, [rva, env, lsl #2]		@ rvc <- data byte from EPn_FIFO
	strb	rvc, [dts, rvb]
	add	rvb, rvb, #1
	b	ushw_2
ushw_3:	ldr	rva, =usb_base
	set	rvb, #0x40
	str	rvb, [rva, #usb_ctl_stat]	@ set SERVICED_OUT_PKT_RDY to clear OUT_PKT_RDY bit in EP0_CSR
	set	pc,  lnk
	
usbhwStatusOut:	@ Control OUT Interrupt, Status OUT Phase
	set	env, #UsbControlOutEP		@ env <- Control OUT EndPoint	
	ldr	rva, =usb_base
	str	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_e:	ldr	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_e
	set	rvb, #0x80			@ rvb <- SERVICED_SETUP_END
	str	rvb, [rva, #usb_ctl_stat]	@ set SERVICED_SETUP_END in EP0_CSR
	ldr	dts, =USB_DATA			@ dts <- buffer
	set	cnt, #0				@ cnt <- 0 bytes to read
	bl	rdEP				@ read 0 bytes from EP
	b	usbEPx

usbhwUnstallEP:	@ Unstall the EndPoint in sv5, jump to Status IN
	set	pc,  lnk

usbhwStallEP: @ Stall the EndPoint in r5
	set	pc,  lnk

usbhwDGD: @ 9.4.3 Get Descriptor of Device Standard request	
	ldr	rva, =usb_base
	str	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_f:	ldr	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_f
	add	rva, rva, #0x80			@ rva <- address of base of EP FIFOs
	set	rvb, #0
ushw_g:	cmp	rvb, cnt
	bpl	ushw_h
	ldrb	rvc, [dts, rvb]			@ rvc <- data from buffer
	str	rvc, [rva, env, lsl #2]		@ store it in EPn_FIFO
	add	rvb, rvb, #1
	b	ushw_g
ushw_h:	
	ldr	rva, =usb_base
	set	rvb, #0x02
	str	rvb, [rva, #usb_ctl_stat]	@ set IN_PKT_RDY bit in EP0_CSR
	b	usbSOx

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
	orr	rvb, sv5, #0x80			@ rvb <- address ored with Device Enable (0x100)
	ldr	rva, =usb_base
	str	rvb, [rva, #usb_daddr]		@ set address	
	set	env, #UsbControlInEP		@ env <- Control IN EndPoint
	str	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
	set	rvb, #0x08			@	if so,  rvb <- IN_PKT_RDY and DATA_END
	str	rvb, [rva, #usb_ctl_stat]	@ set IN_PKT_RDY bit in EP0_CSR
	b	usbEPx
	
usbhwDeconfigure: @ Deconfigure the device
	@ clear the readbuffer
	ldr	rva, =BUFFER_START
	vcrfi	rva, rva, READ_BF_offset
	set	rvb, #i0
	vcsti	rva, 0, rvb
	@ set uart to generate Rx interrupts
	ldr	rva, =uart0_base
	set	rvb, #0x45
	str	rvb, [rva, #0x04]		@ UCON0 <- enable Tx and Rx, and error interrupt
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
	str	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_x:	ldr	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_x
	set	rvb, #0
	str	rvb, [rva, #0x48]		@ set direction to OUT in IN_CSR2_REG
	set	rvb, #0x08
	str	rvb, [rva, #0x40]		@ set packet size to 64 bytes in MAXP_REG
	set	env, #UsbBulkInEP
	str	env, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_y:	ldr	rvb, [rva, #usb_index_reg]
	eq	rvb, env
	bne	ushw_y
	set	rvb, #0x20
	str	rvb, [rva, #0x48]		@ set direction to IN in IN_CSR2_REG
	set	rvb, #0x08
	str	rvb, [rva, #0x40]		@ set packet size to 64 bytes in MAXP_REG
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
	str	rvc, [rva, #usb_index_reg]	@ select endpoint in INDEX_REG
ushw_w:	ldr	rvb, [rva, #usb_index_reg]
	eq	rvb, rvc
	bne	ushw_w
	add	rva, rva, #0x80
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
	str	rvb, [rva, #0x44]		@ set IN_PKT_RDY bit in IN_CSR1_REG
	b	usbwcn				@ return


	
