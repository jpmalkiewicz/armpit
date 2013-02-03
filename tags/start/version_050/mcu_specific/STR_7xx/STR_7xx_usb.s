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
	set	rvb, sv1			@ rvb <- Device Interrupt Status
	set	pc,  lnk

usbhwReset:
	ldr	rva, =usb_base
	ldr	rvb, [rva]
	eor	rvb, rvb, #0x3000
	eor	rvb, rvb, #0x0030
	bic	rvb, rvb, #0xC000
	bic	rvb, rvb, #0x00C0
	orr	rvb, rvb, #0x0200
	str	rvb, [rva]			@ USB_EP0R <- enable enpoint 0 as control EP
	set	rvb, #0x80			@ rvb <- bit to enable USB at address 0
	str	rvb, [rva, #usb_daddr]
	set	pc,  lnk

usbhwRemoteWakeUp: @ suspend/wakeup
	ldr	rva, =usb_base
	ldr	rvb, [rva, #0x40]		@ rvb <- contents of USB_CNTR
	eor	rvb, rvb, #0x08			@ rvb <- contents of USB_CNTR with FSUSP bit toggled
	str	rvb, [rva, #0x40]
	set	pc,  lnk

usbhwEndpointStatus: @ get status of EP into sv2 and sv3 (sv1 is device interrupt from usb_istr)
	ldr	rva, =usb_base
	and	rvb, sv1, #0x0F			@ rvb <- endpoint part of istr
	ldr	sv3, [rva, rvb, LSL #2]		@ sv3 <- content of USB_EPnR (Endpoint Status)
	eq	rvb, #0				@ is interrupt for EP0?
	seteq	rvb, #0x010000			@	if so,  rvb <- indicator bit for EP0
	eq	rvb, #1				@ is interrupt for EP1?
	seteq	rvb, #0x040000			@	if so,  rvb <- indicator bit for EP1
	eq	rvb, #2				@ is interrupt for EP2?
	seteq	rvb, #0x100000			@	if so,  rvb <- indicator bit for EP2
	eq	rvb, #3				@ is interrupt for EP3?
	seteq	rvb, #0x400000			@	if so,  rvb <- indicator bit for EP3
	tst	sv1, #0x10			@ is this an IN transfer?
	lsleq	rvb, rvb, #1			@	if so,  adjust indicator bit
	set	sv2, rvb
	set	pc,  lnk

usbhwClearRxBk:
	bic	sv1, sv1, #usb_itxendp		@ exclude Txendpkt bit from interrupt clearing
	ldr	rva, =usb_base
	str	sv1, [rva, #usb_iclear_dv]	@ clear USB interrupt register (DEV_INT_CLR = disr)
	set	pc,  lnk

usbStall: @ stall endpoint 1 (phys 1, log 0, aka 0x80) -- i.e. Control IN
	ldr	rva, =usb_base
	ldr	rvb, [rva]			@ rvb <- USB_EP0R
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00C0
	eor	rvb, rvb, #0x0010
	orr	rvb, rvb, #0x8000
	str	rvb, [rva]			@ USB_EPnR <- EP data now VALID (transmit when needed)
	b	usbEPx

usbhwCleariTX: @ clear the txcomp interrupt
	ldr	env, =UsbBulkInEP
	ldr	rva, =usb_base
	ldr	rvb, [rva, env, LSL #2]		@ rvb <- USB_EPnR
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00F0
	orr	rvb, rvb, #0x8000
	str	rvb, [rva, env, LSL #2]		@ USB_EPnR <- Clear CTR-TX interrupt on EP in env
	set	pc,  lnk

usbhwwrtEPA:
	and	env, env, #0x0F
	ldr	rva, =0xC0008000		@ rva <- usb hardware buffer start
	add	rva, rva, env, LSL #4		@ rva <- start of packet buffer table for EP in env
	str	cnt, [rva, #4]			@ store number of byte to send in USB_COUNTn_TX
	ldr	rvb, [rva, #0]			@ rvb <- offset to USB_ADRn_TX data / 2
	ldr	rva, =0xC0008000		@ rva <- usb hardware buffer start
	add	rva, rva, rvb, LSL #1		@ rva <- address of start of Tx packet memory for EP in env
	set	rvb, #0
wrtEPL:	cmp	rvb, cnt
	bpl	wrtEPM
	ldrh	rvc, [dts, rvb]
	str	rvc, [rva, rvb, LSL #1]
	add	rvb, rvb, #2
	b	wrtEPL
wrtEPM:	ldr	rva, =usb_base
	ldr	rvb, [rva, env, LSL #2]		@ rvb <- USB_EPnR
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00C0
	eor	rvb, rvb, #0x0030
	orr	rvb, rvb, #0x8000
	orr	rvb, rvb, #0x0080
	str	rvb, [rva, env, LSL #2]		@ USB_EPnR <- EP data now VALID (transmit when needed)
	set	pc,  lnk
	
usbhwixx:
	b	usbEPx
	
usbhwCIi:  @ Control IN interrupt response
	b	wrtEPU

usbhwSetup: @ Control OUT Interrupt, Setup Phase
	b	rdEP

usbhwStatusOut:	@ Control OUT Interrupt, Status OUT Phase
	set	env, #UsbControlOutEP		@ env <- Control OUT EndPoint
	ldr	dts, =USB_DATA			@ dts <- buffer
	set	cnt, #0				@ cnt <- 0 bytes to read
	bl	rdEP				@ read 0 bytes from EP
	b	usbEPx

usbhwUnstallEP:	@ Unstall the EndPoint in sv5, jump to Status IN
	and	rvb, sv5, #0x0F			@ rvb <- logical endpoint
	ldr	rva, =usb_base
	ldr	rvb, [rva, rvb, LSL #2]		@ rvb <- USB_EPnR
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00C0
	tst	sv5, #0x80
	eorne	rvb, rvb, #0x0020
	eoreq	rvb, rvb, #0x0300
	orr	rvb, rvb, #0x8000
	and	sv5, sv5, #0x0F			@ sv5 <- logical endpoint
	str	rvb, [rva, sv5, LSL #2]		@ USB_EPnR <- EP data now VALID (transmit when needed)
	set	pc,  lnk

usbhwStallEP: @ Stall the EndPoint in r5
	and	rvb, sv5, #0x0F			@ rvb <- logical endpoint
	ldr	rva, =usb_base
	ldr	rvb, [rva, rvb, LSL #2]		@ rvb <- USB_EPnR
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00C0
	tst	sv5, #0x80
	eorne	rvb, rvb, #0x0010
	eoreq	rvb, rvb, #0x0100
	orr	rvb, rvb, #0x8000
	and	sv5, sv5, #0x0F			@ rvb <- logical endpoint
	str	rvb, [rva, sv5, LSL #2]		@ USB_EPnR <- EP data now VALID (transmit when needed)
	set	pc,  lnk

usbhwDGD: @ 9.4.3 Get Descriptor of Device Standard request	
	bl	wrtEP
	b	usbSOx

usbSOx:	@ Prepare setup buffer for Status OUT Phase 
	ldr	rva, =USB_SETUP_BUFFER
	ldr	rvb, =0xFF
	str	rvb, [rva]
	b	usbEPx

usbhwEGS: @ Get Status of Endpoint in sv5 into rvb
	and	rvb, sv5, #0x0F			@ rvb <- logical endpoint
	ldr	rva, =usb_base
	ldr	env, [rva, rvb, LSL #2]
	tst	rvb, #0x80
	lsreq	env, env, #8
	and	rvb, env, #0x30
	eq	rvb, #0x30
	seteq	rvb, #0				@	if so,  rvb <- 0, not enabled
	setne	rvb, #1				@	if not, rvb <- 1, enabled
	set	pc,  lnk

usbhwSetAddress: @ Set Device to Address in sv5
	@ USB Status IN  exit -- write null packet to   EP 1 (phys, aka 0x80)
	@ disable the correct transfer interrupt mask (CTRM) in USB_CNTR
	ldr	rva, =usb_base
	ldr	rvb, [rva, #0x40]
	bic	rvb, rvb, #0x9C00
	str	rvb, [rva, #0x40]	
	set	env, #UsbControlInEP		@ env <- Control IN EndPoint
	ldr	dts, =USB_DATA			@ dts <- buffer
	set	cnt, #0x00			@ cnt <- 0 bytes to send
	bl	wrtEP				@ write 0 bytes to EP
hwsta0:	@ wait for tx to be complete (CTR_TX in USB_EP0R) then clear CTR_TX
	ldr	rvb, [rva]			@ rvb <- contents of USB_EP0R
	tst	rvb, #0x80			@ is CTR_TX set?
	beq	hwsta0				@	if not, jump to keep waiting
	ldr	rvb, [rva]			@ rvb <- contents of USB_EP0R
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00F0
	orr	rvb, rvb, #0x8000
	str	rvb, [rva]			@ USB_EPnR <- Clear CTR-TX interrupt on EP in env
	ldr	rvb, [rva]			@ rvb <- contents of USB_EP0R
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00C0
	eor	rvb, rvb, #0x0030
	orr	rvb, rvb, #0x8000
	str	rvb, [rva]			@ USB_EPnR <- EP data now VALID (transmit when needed)
	@ re-enable the correct transfer interrupt mask (CTRM) in USB_CNTR
	ldr	rva, =usb_base
	ldr	rvb, [rva, #0x40]
	orr	rvb, rvb, #0x9C00
	str	rvb, [rva, #0x40]
	ldr	rva, =USB_SETUP_BUFFER		@ rva <- address of setup buffer
	ldr	sv5, [rva]			@ sv5 <- reqtyp(8), request(8), val(16)
	lsr	sv5, sv5, #16			@ sv5 <- address = val(16)
	orr	rvb, sv5, #0x80			@ rvb <- address ored with Device Enable (0x80)
	ldr	rva, =usb_base
	str	rvb, [rva, #0x4C]		@ set address
	b	usbEPx
	
usbhwDeconfigure: @ Deconfigure the device
	@ clear the readbuffer
	ldr	rva, =BUFFER_START
	vcrfi	rva, rva, READ_BF_offset
	set	rvb, #i0
	vcsti	rva, 0, rvb
	@ set uart to generate Rx interrupts
	ldr	rva, =uart0_base
	set	rvb, #1
	str	rvb, [rva, #0x10]		@ UART0_IE <- enable RxBufNotEmpty Interrupt
	@ set default i/o port to uart
	ldr	rvb, =vuart0
	vcsti	glv, 4, rvb			@ default input/output port model
	set	pc,  lnk
	
usbhwConfigure: @ Configure the device
	@ stop uart from generating Rx interrupts (they cause noise on shared READBUFFER)
	ldr	rva, =uart0_base
	set	rvb, #0
	str	rvb, [rva, #0x10]		@ UART0_IE <- disable RxBufNotEmpty Interrupt
	@ clear the readbuffer
	ldr	rva, =BUFFER_START
	vcrfi	rva, rva, READ_BF_offset
	set	rvb, #i0
	vcsti	rva, 0, rvb
	@ configure USB
	ldr	rva, =usb_base
	ldr	rvb, =0x0621
	str	rvb, [rva, #0x04]		@ enable EP1 -- Interrupt IN
	ldr	rvb, =0x3002
	str	rvb, [rva, #0x08]		@ enable EP2 -- Bulk OUT
	ldr	rvb, =0x0023
	str	rvb, [rva, #0x0C]		@ enable EP3 -- Bulk IN
	@ set default i/o port to usb
	ldr	rvb, =vusb
	vcsti	glv, 4, rvb			@ default input/output port model
	set	pc,  lnk

wrtEP:	@ write data to Control In Endpoint (write cnt bytes starting at dts to enpoint in env)
wrtEPU:	@ write data to Control In Endpoint (write cnt bytes starting at dts to enpoint in env)
	and	env, env, #0x0F
	ldr	rva, =usb_base
	ldr	rvb, [rva, env, LSL #2]		@ rvb <- USB_EPnR
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00F0
	orr	rvb, rvb, #0x8000
	str	rvb, [rva, env, LSL #2]		@ USB_EPnR <- Clear CTR-TX interrupt on EP in env
	ldr	rva, =0xC0008000		@ rva <- usb hardware buffer start
	add	rva, rva, env, LSL #4		@ rva <- start of packet buffer table for EP in env
wrtEPW:	set	rvb, #0
	str	rvb, [rva, #4]			@ store number of byte to send in USB_COUNTn_TX
	ldr	rvb, [rva, #4]			@ store number of byte to send in USB_COUNTn_TX
	eq	rvb, #0
	bne	wrtEPW
	str	cnt, [rva, #4]			@ store number of byte to send in USB_COUNTn_TX
	ldr	rvb, [rva, #0]			@ rvb <- offset to USB_ADRn_TX data / 2
	ldr	rva, =0xC0008000		@ rva <- usb hardware buffer start
	add	rva, rva, rvb, LSL #1		@ rva <- address of start of Tx packet memory for EP in env
	set	rvb, #0
wrtEPX:	cmp	rvb, cnt
	bpl	wrtEPY
	ldrh	rvc, [dts, rvb]
	str	rvc, [rva, rvb, LSL #1]
	add	rvb, rvb, #2
	b	wrtEPX
wrtEPY:	ldr	rva, =usb_base
	ldr	rvb, [rva, env, LSL #2]		@ rvb <- USB_EPnR
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00C0
	eor	rvb, rvb, #0x0030
	orr	rvb, rvb, #0x8000
	orr	rvb, rvb, #0x0080
	str	rvb, [rva, env, LSL #2]		@ USB_EPnR <- EP data now VALID (transmit when needed)
	set	pc,  lnk
	
rdEP:	@ read from endpoint in env to buffer in dts with count in cnt
	and	env, env, #0x0F
	ldr	rva, =usb_base
	ldr	rvb, [rva, env, LSL #2]		@ rvb <- USB_EPnR
	bic	rvb, rvb, #0xF000
	bic	rvb, rvb, #0x00F0
	orr	rvb, rvb, #0x0080
	str	rvb, [rva, env, LSL #2]		@ USB_EP0R <- Clear CTR-RX interrupt on EP0
	ldr	rva, =0xC0008000		@ rva <- usb hardware buffer start
	add	rva, rva, env, LSL #4		@ rva <- start of packet buffer table for EP0
	ldr	cnt, [rva, #12]			@ cnt <- number of bytes received (from USB_COUNTn_RX)
	bic	cnt, cnt, #0xFC00		@ cnt <- number of bytes received
	ldr	rvb, [rva, #8]			@ rvb <- offset to USB_ADRn_RX data / 2
	ldr	rva, =0xC0008000		@ rva <- usb hardware buffer start
	add	rva, rva, rvb, LSL #1		@ rva <- address of start of Rx packet memory for EP in env
	set	rvb, #0
usbSEZ:	cmp	rvb, cnt
	bpl	usbSEX
	ldr	rvc, [rva, rvb, LSL #1]
	strh	rvc, [dts, rvb]
	add	rvb, rvb, #2
	b	usbSEZ
usbSEX:	ldr	rva, =usb_base
	ldr	rvb, [rva, env, LSL #2]		@ rvb <- USB_EPnR
	bic	rvb, rvb, #0xC000
	bic	rvb, rvb, #0x00F0
	eor	rvb, rvb, #0x3000
	orr	rvb, rvb, #0x0080
	orr	rvb, rvb, #0x8000
	str	rvb, [rva, env, LSL #2]		@ USB_EP0R <- EP data now VALID (receive when needed)
	set	pc,  lnk

usbhwr:	@ write writebuffer to USB Bulk IN endpoint 5 (phys 5, log 2, aka 0x82) (eg. section 9.14)
	@ on entry:	sv5 <- USB Write Buffer
	@ on entry:	rvb <- number of bytes to send
	@ called with interrupts disabled
	@ write data packet to send
	@ modifies:	sv3, rva, rvb, rvc
	lsl	rvb, rvb, #2
	orr	sv3, rvb, #i0			@ sv3 <- number of bytes to send (scheme int)
	ldr	rva, =0xC0008000		@ rva <- usb hardware buffer start
	lsr	rvb, sv3, #2			@ rvb <- number of bytes to send (raw int)
	str	rvb, [rva, #0x34]		@ store number of byte to send in USB_COUNT3_TX
	ldr	rvb, [rva, #0x30]		@ rvb <- offset to USB_ADR3_TX data / 2
	add	rva, rva, rvb, LSL #1		@ rva <- address of start of Tx packet memory for EP
	set	rvc, #i0
usbwr1:	cmp	rvc, sv3
	bpl	usbwr2
	set	rvb, #8
	add	rvb, rvb, rvc, LSR #2
	ldrh	rvb, [sv5, rvb]
	str	rvb, [rva, rvc, LSR #1]
	add	rvc, rvc, #8
	b	usbwr1
usbwr2:	ldr	rva, =usb_base
	ldr	rvb, [rva, #0x0C]		@ rvb <- contents of USB_EP3R
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00C0
	eor	rvb, rvb, #0x0030
	orr	rvb, rvb, #0x8000
	orr	rvb, rvb, #0x0080
	str	rvb, [rva, #0x0C]		@ USB_EP3R <- EP data now VALID (transmit when needed)
	b	usbwcn				@ return

