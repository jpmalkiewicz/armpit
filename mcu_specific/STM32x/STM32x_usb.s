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

.ifndef connectivity_ln		@ USB Device Functions (STM32F103)
	
_func_
usbhwDeviceStatus: @ return device status in rvb
	set	rvb, sv1			@ rvb <- Device Interrupt Status
	set	pc,  lnk

_func_
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

_func_
usbhwRemoteWakeUp: @ suspend/wakeup
	ldr	rva, =usb_base
	ldr	rvb, [rva, #0x40]		@ rvb <- contents of USB_CNTR
	eor	rvb, rvb, #0x08			@ rvb <- contents of USB_CNTR with FSUSP bit toggled
	str	rvb, [rva, #0x40]
	set	pc,  lnk

_func_
usbhwEndpointStatus: @ get status of EP into sv2 and sv3 (sv1 is device interrupt from usb_istr)
	ldr	rva, =usb_base
	and	rvb, sv1, #0x0F			@ rvb <- endpoint part of istr
	ldr	sv3, [rva, rvb, LSL #2]		@ sv3 <- content of USB_EPnR (Endpoint Status)
	eq	rvb, #0				@ is interrupt for EP0?
	it	eq
	seteq	rvb, #0x010000			@	if so,  rvb <- indicator bit for EP0
	eq	rvb, #1				@ is interrupt for EP1?
	it	eq
	seteq	rvb, #0x040000			@	if so,  rvb <- indicator bit for EP1
	eq	rvb, #2				@ is interrupt for EP2?
	it	eq
	seteq	rvb, #0x100000			@	if so,  rvb <- indicator bit for EP2
	eq	rvb, #3				@ is interrupt for EP3?
	it	eq
	seteq	rvb, #0x400000			@	if so,  rvb <- indicator bit for EP3
	tst	sv1, #0x10			@ is this an IN transfer?
	it	eq
	lsleq	rvb, rvb, #1			@	if so,  adjust indicator bit
	set	sv2, rvb
	set	pc,  lnk

_func_
usbhwClearRxBk:
	bic	sv1, sv1, #usb_itxendp		@ exclude Txendpkt bit from interrupt clearing
	ldr	rva, =usb_base
	str	sv1, [rva, #usb_iclear_dv]	@ clear USB interrupt register (DEV_INT_CLR = disr)
	set	pc,  lnk

_func_
usbStall: @ stall endpoint 1 (phys 1, log 0, aka 0x80) -- i.e. Control IN
	ldr	rva, =usb_base
	ldr	rvb, [rva]			@ rvb <- USB_EP0R
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00C0
	eor	rvb, rvb, #0x0010
	orr	rvb, rvb, #0x8000
	str	rvb, [rva]			@ USB_EPnR <- EP data now VALID (transmit when needed)
	b	usbEPx

_func_
usbhwCleariTX: @ clear the txcomp interrupt
	ldr	env, =UsbBulkInEP
	ldr	rva, =usb_base
	ldr	rvb, [rva, env, LSL #2]		@ rvb <- USB_EPnR
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00F0
	orr	rvb, rvb, #0x8000
	str	rvb, [rva, env, LSL #2]		@ USB_EPnR <- Clear CTR-TX interrupt on EP in env
	set	pc,  lnk

_func_
usbhwwrtEPA:
	and	env, env, #0x0F
	ldr	rva, =0x40006000		@ rva <- usb hardware buffer start
	add	rva, rva, env, LSL #4		@ rva <- start of packet buffer table for EP in env
	str	cnt, [rva, #4]			@ store number of byte to send in USB_COUNTn_TX
	ldr	rvb, [rva, #0]			@ rvb <- offset to USB_ADRn_TX data / 2
	ldr	rva, =0x40006000		@ rva <- usb hardware buffer start
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
	
_func_
usbhwixx:
	b	usbEPx
	
_func_
usbhwCIi:  @ Control IN interrupt response
	b	wrtEPU

_func_
usbhwSetup: @ Control OUT Interrupt, Setup Phase
	b	rdEP

_func_
usbhwStatusOut:	@ Control OUT Interrupt, Status OUT Phase
	set	env, #UsbControlOutEP		@ env <- Control OUT EndPoint
	ldr	dts, =USB_DATA			@ dts <- buffer
	set	cnt, #0				@ cnt <- 0 bytes to read
	bl	rdEP				@ read 0 bytes from EP
	b	usbEPx

_func_
usbhwUnstallEP:	@ Unstall the EndPoint in sv5, jump to Status IN
	and	rvb, sv5, #0x0F			@ rvb <- logical endpoint
	ldr	rva, =usb_base
	ldr	rvb, [rva, rvb, LSL #2]		@ rvb <- USB_EPnR
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00C0
	tst	sv5, #0x80
	itE	ne
	eorne	rvb, rvb, #0x0020
	eoreq	rvb, rvb, #0x0300
	orr	rvb, rvb, #0x8000
	and	sv5, sv5, #0x0F			@ sv5 <- logical endpoint
	str	rvb, [rva, sv5, LSL #2]		@ USB_EPnR <- EP data now VALID (transmit when needed)
	set	pc,  lnk

_func_
usbhwStallEP: @ Stall the EndPoint in r5
	and	rvb, sv5, #0x0F			@ rvb <- logical endpoint
	ldr	rva, =usb_base
	ldr	rvb, [rva, rvb, LSL #2]		@ rvb <- USB_EPnR
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00C0
	tst	sv5, #0x80
	itE	ne
	eorne	rvb, rvb, #0x0010
	eoreq	rvb, rvb, #0x0100
	orr	rvb, rvb, #0x8000
	and	sv5, sv5, #0x0F			@ rvb <- logical endpoint
	str	rvb, [rva, sv5, LSL #2]		@ USB_EPnR <- EP data now VALID (transmit when needed)
	set	pc,  lnk

_func_
usbhwDGD: @ 9.4.3 Get Descriptor of Device Standard request	
	bl	wrtEP
	b	usbSOx

_func_
usbSOx:	@ Prepare setup buffer for Status OUT Phase 
	ldr	rva, =USB_SETUP_BUFFER
	ldr	rvb, =0xFF
	str	rvb, [rva]
	b	usbEPx

_func_
usbhwEGS: @ Get Status of Endpoint in sv5 into rvb
	and	rvb, sv5, #0x0F			@ rvb <- logical endpoint
	ldr	rva, =usb_base
	ldr	env, [rva, rvb, LSL #2]
	tst	rvb, #0x80
	it	eq
	lsreq	env, env, #8
	and	rvb, env, #0x30
	eq	rvb, #0x30
	itE	eq
	seteq	rvb, #0				@	if so,  rvb <- 0, not enabled
	setne	rvb, #1				@	if not, rvb <- 1, enabled
	set	pc,  lnk

_func_
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

_func_
usbhwDeconfigure: @ Deconfigure the device
	@ clear the readbuffer
	ldr	rva, =BUFFER_START
	vcrfi	rva, rva, READ_BF_offset
	set	rvb, #i0
	vcsti	rva, 0, rvb
	@ set uart to generate Rx interrupts
	ldr	rva, =uart0_base
	ldr	rvb, =0x202c
	str	rvb, [rva, #0x0c]	@ USART_CR1   <- USART, Tx and Rx enabled at 8N1, with Rx interrupt
	@ set default i/o port to uart
	ldr	rvb, =vuart0
	vcsti	glv, 4, rvb			@ default input/output port model	
	set	pc,  lnk
	
_func_
usbhwConfigure: @ Configure the device
	@ stop uart from generating Rx interrupts (they cause noise on shared READBUFFER)
	ldr	rva, =uart0_base
	ldr	rvb, =0x200c
	str	rvb, [rva, #0x0c]	@ USART_CR1   <- USART, Tx and Rx enabled at 8N1, without Rx interrupt
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

_func_
wrtEP:	@ write data to Control In Endpoint (write cnt bytes starting at dts to enpoint in env)
wrtEPU:	@ write data to Control In Endpoint (write cnt bytes starting at dts to enpoint in env)
	and	env, env, #0x0F
	ldr	rva, =usb_base
	ldr	rvb, [rva, env, LSL #2]		@ rvb <- USB_EPnR
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00F0
	orr	rvb, rvb, #0x8000
	str	rvb, [rva, env, LSL #2]		@ USB_EPnR <- Clear CTR-TX interrupt on EP in env
	ldr	rva, =0x40006000		@ rva <- usb hardware buffer start
	add	rva, rva, env, LSL #4		@ rva <- start of packet buffer table for EP in env
wrtEPW:	set	rvb, #0
	str	rvb, [rva, #4]			@ store number of byte to send in USB_COUNTn_TX
	ldr	rvb, [rva, #4]			@ store number of byte to send in USB_COUNTn_TX
	eq	rvb, #0
	bne	wrtEPW
	str	cnt, [rva, #4]			@ store number of byte to send in USB_COUNTn_TX
	ldr	rvb, [rva, #0]			@ rvb <- offset to USB_ADRn_TX data / 2
	ldr	rva, =0x40006000		@ rva <- usb hardware buffer start
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
	
_func_
rdEP:	@ read from endpoint in env to buffer in dts with count in cnt
	and	env, env, #0x0F
	ldr	rva, =usb_base
	ldr	rvb, [rva, env, LSL #2]		@ rvb <- USB_EPnR
	bic	rvb, rvb, #0xF000
	bic	rvb, rvb, #0x00F0
	orr	rvb, rvb, #0x0080
	str	rvb, [rva, env, LSL #2]		@ USB_EP0R <- Clear CTR-RX interrupt on EP0
	ldr	rva, =0x40006000		@ rva <- usb hardware buffer start
	add	rva, rva, env, LSL #4		@ rva <- start of packet buffer table for EP0
	ldr	cnt, [rva, #12]			@ cnt <- number of bytes received (from USB_COUNTn_RX)
	bic	cnt, cnt, #0xFC00		@ cnt <- number of bytes received
	ldr	rvb, [rva, #8]			@ rvb <- offset to USB_ADRn_RX data / 2
	ldr	rva, =0x40006000		@ rva <- usb hardware buffer start
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

_func_
usbhwr:	@ write writebuffer to USB Bulk IN endpoint 5 (phys 5, log 2, aka 0x82) (eg. section 9.14)
	@ on entry:	sv5 <- USB Write Buffer
	@ on entry:	rvb <- number of bytes to send
	@ called with interrupts disabled
	@ write data packet to send
	@ modifies:	sv3, rva, rvb, rvc
	lsl	rvb, rvb, #2
	orr	sv3, rvb, #i0			@ sv3 <- number of bytes to send (scheme int)
	ldr	rva, =0x40006000		@ rva <- usb hardware buffer start
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
	lsr	rvc, rvc, #1
	str	rvb, [rva, rvc]
	lsl	rvc, rvc, #1
	orr	rvc, rvc, #i0
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


.else	@ USB OTG FS Device Functions (STM32F107)

@
@  Connectivity Line USB OTG FS is not functional in this version of Armpit Scheme
@
@  Code below is under development (construction site).
@
	
_func_
usbhwDeviceStatus: @ return device status in rvb
	set	rvb, sv1			@ rvb <- Device Interrupt Status
	set	pc,  lnk

_func_
usbhwReset:
	ldr	rva, =usb_base
@<-|||	set	rvb, #64
	set	rvb, #0x400000
	orr	rvb, rvb, #0x40
	str	rvb, [rva, #0x24]		@ OTG_FS_GRXFSIZ  <- 64 words Rx FIFO
@<-|||	set	rvb, #0x400000
@<-|||	orr	rvb, rvb, #0x40
	add	rvb, rvb, #0x40
	str	rvb, [rva, #0x28]		@ OTG_FS_DIEPTXF0 <- 64 words EP0 Tx FIFO, start addr. = 64

	add	rva, rva, #0x0100
	add	rvb, rvb, #0x40
	str	rvb, [rva, #0x04]		@ OTG_FS_DIEPTXF1 <- 64 words FIFO 1, start addr. = 128
	add	rvb, rvb, #0x40
	str	rvb, [rva, #0x08]		@ OTG_FS_DIEPTXF2 <- 64 words FIFO 2, start addr. = 192
	add	rvb, rvb, #0x40
	str	rvb, [rva, #0x0c]		@ OTG_FS_DIEPTXF3 <- 64 words FIFO 3, start addr. = 256
	
	sub	rva, rva, #0x0100
	set	rvb, #0x0420
@	set	rvb, #0x0430
	str	rvb, [rva, #0x10]		@ OTG_FS_GRSTCTL <- flush all Tx Fifos
ushw_y:	ldr	rvb, [rva, #0x10]
	tst	rvb, #(1 << 31)
	beq	ushw_y
	set	rvb, #0x0010
	
	str	rvb, [rva, #0x10]		@ OTG_FS_GRSTCTL <- flush Rx Fifo and all Tx Fifos
	
ushw_x:	ldr	rvb, [rva, #0x10]
	tst	rvb, #(1 << 31)
	beq	ushw_x
	tst	rvb, #0x10
	bne	ushw_x
@	ldr	rvb, [rva, #0x20]		@ pop the Rx Fifo
@	ldr	rvb, [rva, #0x20]		@ pop the Rx Fifo
	
	add	rva, rva, #0x0800
@@	add	rva, rva, #0x0700

	ldr	rvb, [rva]
	orr	rvb, rvb, #0x03
	str	rvb, [rva]			@ OTG_FS_DCFG     <- device mode at full speed, address 0

	set	rvb, #0
	str	rvb, [rva, #0x34]		@ OTG_FS_DIEPEMPMSK <- mask TxFIfo empty interrupts
		
	set	rvb, #0x10000
	orr	rvb, rvb, #0x01
	str	rvb, [rva, #0x1c]		@ OTG_FS_DAINTMSK <- unmask EP0 IN/OUT interrupts
@ <-*	
@	
	set	rvb, #0x09
@<-$	
@<-&&	
@	set	rvb, #0x01
	str	rvb, [rva, #0x14]		@ OTG_FS_DOEPMSK  <- unmask EP0 OUT setup and Rx interrupts
@ <-!	
	set	rvb, #0x01
@ <-&&	set	rvb, #0x11
@	set	rvb, #0x10	@ <-$
@ <-*	set	rvb, #0x09
	str	rvb, [rva, #0x10]		@ OTG_FS_DIEPMSK  <- unmask EP0 IN  Tx interrupt

@@	set	rvb, #0x60000000
@@	orr	rvb, rvb, #0x08
@@	ldr	rvb, =#((3 << 29) | (1 << 19) | 8)
@	set	rvb, #0x03

@	orr	rvb, rvb, #(1 << 19)
	
@	str	rvb, [rva, #0x0300]		@ OTG_FS_DOEPCTL0  <- EP0 OUT 8 byte packet size
	
@@@	set	rvb, #(3 << 29)
	set	rvb, #(1 << 29)
	orr	rvb, rvb, #(1 << 19)
	orr	rvb, rvb, #8
	
	str	rvb, [rva, #0x0310]		@ OTG_FS_DOEPTSIZ0 <- EP0 OUT up to 3 setup pakets

@@	set	rvb, #0x08
@@	str	rvb, [rva, #0x0110]		@ OTG_FS_DIEPTSIZ0 <- 8 bytes per transfer
@@	set	rvb, #0x80000000
@@	orr	rvb, rvb, #0x03
	set	rvb, #0x03
@	orr	rvb, rvb, #(1 << 19)
	orr	rvb, rvb, #(1 << 31)
@	orr	rvb, rvb, #(1 << 26)
	orr	rvb, rvb, #(1 << 27)		@ nak until loaded
	
	str	rvb, [rva, #0x0100]		@ OTG_FS_DIEPCTL0  <- EP0 IN 8 byte packet size

	
@	add	rva, rva, #0x0b10
@	set	rvb, #(1 << 19)
@	orr	rvb, rvb, #(1 << 29)
@	orr	rvb, rvb, #8
@@	add	rva, rva, env, LSL #5
@	str	rvb, [rva]			@ OTG_FS_DOEPTSIZx <- number of bytes to read for this 1 packet
	
@	sub	rva, rva, #0x08
@	ldr	rvb, [rva]			@ rvb <- OTG_FS_DIEPINTx
@	orr	rvb, rvb, #1
@	str	rvb, [rva]			@ OTG_FS_DIEPINTx  <- clear Tx interrupt on EP
	
@	sub	rva, rva, #0x08
@	ldr	rvb, [rva]			@ rvb <- OTG_FS_DOEPCTLx
	set	rvb, #3
	orr	rvb, rvb, #(1 << 31)
	orr	rvb, rvb, #(1 << 26)		@ rvb <- clear NAK generation
	str	rvb, [rva, #0x0300]		@ OTG_FS_DOEPCTLx  <- enable EP for reception

	
@	ldr	rvb, [rva]
@	eor	rvb, rvb, #0x3000
@	eor	rvb, rvb, #0x0030
@	bic	rvb, rvb, #0xC000
@	bic	rvb, rvb, #0x00C0
@	orr	rvb, rvb, #0x0200
@	str	rvb, [rva]			@ USB_EP0R <- enable enpoint 0 as control EP
@	set	rvb, #0x80			@ rvb <- bit to enable USB at address 0
@	str	rvb, [rva, #usb_daddr]
	set	pc,  lnk

_func_
usbhwRemoteWakeUp: @ suspend/wakeup
@	ldr	rva, =usb_base
@	ldr	rvb, [rva, #0x40]		@ rvb <- contents of USB_CNTR
@	eor	rvb, rvb, #0x08			@ rvb <- contents of USB_CNTR with FSUSP bit toggled
@	str	rvb, [rva, #0x40]
	set	pc,  lnk

_func_
usbhwEndpointStatus: @ get status of EP into sv2 and sv3 (sv1 is device interrupt from usb_istr)
	@ on entry:	sv1 <- OTG_FS_GINTSTS
	@ on exit:	sv2 <- OTG_FS_DAINT
	@ on exit:	sv3 <- OTG_FS_DIEPINTx / DOEPINTx
	@ side-effect:	clears EP interrupt
	@ modifies:	rva, rvb, sv2, sv3


	tst	sv1, #0x1000
	bne	usbDSi
	
@	tst	sv1, #0x0800
@	bne	usbDSi
	
	ldr	rva, =usb_base

	add	rva, rva, #0x0800
	ldr	sv2, [rva, #0x18]		@ sv2 <- OTG_FS_DAINT (EP interrupts)

	ldr	rvb, [rva, #0x1c]		@ rvb <- OTG_FS_DAINTMSK (allowed EP interrupts)
	and	sv2, sv2, rvb

	eq	sv2, #0
	beq	usbhwEPstd
	
@	it	eq
@	seteq	sv2, #usbCO_ibit

	ldr	rvb, [rva, #0x08]		@ rvb <- OTG_FS_DSTS (must be read on DAINT interrupt)

	add	rva, rva, #0x0100
	tst	sv2, #usbCO_ibit		@ is interrupt for Control OUT EP ?
	it	ne
	addne	rva, rva, #0x0200
	bne	usbhwEPstc
	tst	sv2, #usbBO_ibit		@ is interrupt for Bulk Out EP ?
	it	ne
	addne	rva, rva, #0x0240
	bne	usbhwEPstc
	tst	sv2, #usbBI_ibit		@ is interrupt for Bulk IN EP ?
	it	ne
	addne	rva, rva, #0x60
usbhwEPstc:
	ldr	sv3, [rva, #0x08]		@ sv3 <- OTG_FS_DIEPINTx / DOEPINTx
@	tst	sv3, #0xff0000			@ is this an IN endpoint
	tst	sv2, #0xff0000			@ is this an IN endpoint
	
@	it	eq
@	biceq	sv3, sv3, #0x01			@	if so,  sv3 <- don't clear Tx	
	str	sv3, [rva, #0x08]		@ clear EP interrupt (except Tx)

@	set	rvb, #0
@	str	rvb, [rva, #0x08]		@ clear EP interrupt (except Tx)

@	it	ne
@	eorne	sv3, sv3, #usbCO_setupbit
	
@ <-*
@<-&
	
@ <-$	
@<-%%	bic	sv3, sv3, #usbCO_setupbit	@ setup is processed below (other case)
	
	@ may need to invert (eor bit 3) setup bit in sv3 in case of usbCO_ibit ????
	
	set	pc,  lnk

usbhwEPstd:

@	ldr	rva, =usb_base
@	ldr	rvb, [rva, #0x1c]		@ rvb <- OTG_FS_GRXSTSR (peek Rx FIFO)
	
@@@ <--------
@	set	pc,  lnk
@@@ <--------
	ldr	rva, =usb_base
	ldr	rvb, [rva, #0x1c]		@ rvb <- OTG_FS_GRXSTSR (peek Rx FIFO)
@	ldr	rvb, [rva, #0x20]		@ 
	tst	rvb, #0x0f
@	itEE	eq
	itT	ne
@	seteq	sv2, #usbCO_ibit
	setne	sv2, #usbBO_ibit		@
	setne	pc,  lnk

@@@ <--------
@@@	set	sv2, #usbCO_ibit
@@@ <--------

	add	rva, rva, #0x0b00
	ldr	sv2, [rva, #0x08]
	str	sv2, [rva, #0x08]
	
.ifdef	debug_usb
	set	sv3, rvb
	ldr	rva, =RAMTOP
	ldr	rvb, [rva]
	add	rvb, rvb, #12		@ rvb <- next free byte
	str	rvb, [rva]
	sub	rva, rvb, #12		@ rva <- current free byte
	set	rvb, #'Y
	str	rvb, [rva]
	str	sv2, [rva, #4]
	str	sv3, [rva, #8]
	set	rvb, sv3
.endif

	lsr	rvb, rvb, #17
	and	rvb, rvb, #0x0f
	eq	rvb, #0x06
	beq	usbhwEPstdsetup
@	itE	eq
@	seteq	sv3, #usbCO_setupbit
@	setne	sv3, #0

@	it	ne
@<-&	setne	sv2, #usbCI_ibit		@
@	ldr	rvb, [rva, #0x20]		@ 
@	ldr	rvb, [rva, #0x20]		@ 

@	orr	sv3, sv3, #0xaa00		@ for debug (as viewed through ocd)
	
@@@ <--------
@@@	set	sv3, #usbCO_setupbit
@@@ <--------

@	set	pc,  lnk
	set	env, #UsbControlOutEP	@ env <- Control OUT EndPoint
	ldr	dts, =USB_SETUP_BUFFER	@ dts <- Setup
	bl	rdEP

@ doing this (rather than rdEP, above) does not re-enable Control-out EP and so gives issues
@	ldr	rva, =usb_base
@@%%%	ldr	rvb, [rva, #0x1c]		@ rvb <- OTG_FS_GRXSTSR (peek Rx FIFO)
@	ldr	rvb, [rva, #0x20]		@ 

	b	usbEPx

usbhwEPstdsetup:

@	b	usbCO8		@ <- %%%

@ ---- code below is not executed, should it? (i.e. branch above would be removed)
		
	set	rvb, #0			@ rvb <- 0
	ldr	rva, =USB_CHUNK		@ rva <- address of chunk in RAM
	str	rvb, [rva]		@ set remaining bytes to send to zero	
	set	env, #UsbControlOutEP	@ env <- Control OUT EndPoint
	ldr	dts, =USB_SETUP_BUFFER	@ dts <- Setup
	bl	rdEP

	b	usbEPx

	@ wait for setup completee
	ldr	rva, =usb_base
usb_23:	ldr	rvb, [rva, #0x1c]		@ rvb <- OTG_FS_GRXSTSR (peek Rx FIFO)
	lsr	rvb, rvb, #17
	and	rvb, rvb, #0x0f
	eq	rvb, #0x04
	bne	usb_23
	ldr	rvb, [rva, #0x20]		@ rvb <- OTG_FS_GRXSTSR (peek Rx FIFO)
	add	rva, rva, #0x0b00
	ldr	sv2, [rva, #0x08]
	str	sv2, [rva, #0x08]
	
	b	usbzig
	
	b	usbEPx
	
_func_
usbhwClearRxBk:
	bic	sv1, sv1, #usb_itxendp		@ exclude Txendpkt bit from interrupt clearing
	ldr	rva, =usb_base
	str	sv1, [rva, #usb_iclear_dv]	@ clear USB interrupt register (DEV_INT_CLR = disr)
	set	pc,  lnk

_func_
usbStall: @ stall endpoint 1 (phys 1, log 0, aka 0x80) -- i.e. Control IN
	ldr	rva, =usb_base
	ldr	rvb, [rva, #0x0900]		@ rvb <- USB_OTG_FS_DIEPCTL0
	orr	rvb, rvb, #(1 << 21)
	str	rvb, [rva, #0x0900]		@ send stall
	b	usbEPx

_func_
usbhwCleariTX: @ clear the txcomp interrupt for bulk in EP (EP3)
@@	ldr	env, =UsbBulkInEP
	ldr	rva, =usb_base
	add	rva, rva, #0x0900
	ldr	rvb, [rva, #0x68]		@ rvb <- USB_OTG_FS_DIEPINT3
	bic	rvb, rvb, #0xF7
	orr	rvb, rvb, #0x01
	str	rvb, [rva, #0x68]		@ USB_OTG_FS_DIEPINT3 <- Clear CTR-TX interrupt
	set	pc,  lnk

_func_
usbhwixx:
	b	usbEPx
	
_func_
usbhwCIi:  @ Control IN interrupt response

	ldr	rva, =USB_CHUNK
	ldr	rvb, [rva]		@ rvb <- how many bytes will remain to be sent
	eq	rvb, #0
	itTTT	eq
	ldreq	rva, =usb_base
	addeq	rva, rva, #0x0800
	seteq	rvb, #0
	streq	rvb, [rva, #0x34]
	
	b	wrtEPU

_func_
usbhwSetup: @ Control OUT Interrupt, Setup Phase
@<-&
@ <- %%%%
@	b	rdEP
	set	pc,  lnk		@ setup packet read in hwstatus above

_func_
usbhwStatusOut:	@ Control OUT Interrupt, Status OUT Phase
.ifdef	debug_usb
	ldr	rva, =RAMTOP
	ldr	rvb, [rva]
	add	rvb, rvb, #16		@ rvb <- next free byte
	str	rvb, [rva]
	sub	rva, rvb, #16		@ rva <- current free byte
	set	rvb, #'e
	str	rvb, [rva]
	ldr	rvb, =usb_base
	ldr	rvb, [rvb, #0x1c]		@ rvb <- OTG_FS_GRXSTSP (pop Rx FIFO)
	str	rvb, [rva, #4]
	ldr	rvb, =usb_base
	add	rvb, rvb, #0x0b00
	ldr	rvb, [rvb]
	str	rvb, [rva, #8]
	ldr	rvb, =usb_base
	add	rvb, rvb, #0x0b00
	ldr	rvb, [rvb, #8]
	str	rvb, [rva, #12]
.endif
	set	env, #UsbControlOutEP		@ env <- Control OUT EndPoint
	ldr	dts, =USB_DATA			@ dts <- buffer
	set	cnt, #0				@ cnt <- 0 bytes to read
@<-&	
	bl	rdEP				@ read 0 bytes from EP
	b	usbEPx

_func_
usbhwUnstallEP:	@ Unstall the EndPoint in sv5, jump to Status IN
	and	rvb, sv5, #0x0F			@ rvb <- logical endpoint
	ldr	rva, =usb_base
	tst	sv5, #0x80
	itE	eq
	addeq	rva, rva, #0x0b00
	addne	rva, rva, #0x0900
	add	rva, rva, rvb, LSL #5
	ldr	rvb, [rva]			@ rvb <- USB_OTG_FS_DIEPCTLx / DOEPCTLx
	bic	rvb, rvb, #(1 << 21)
	str	rvb, [rva]			@ unstall EP
	
@	bic	rvb, rvb, #0xF100
@	bic	rvb, rvb, #0x00C0
@	tst	sv5, #0x80
@	itE	ne
@	eorne	rvb, rvb, #0x0020
@	eoreq	rvb, rvb, #0x0300
@	orr	rvb, rvb, #0x8000
@	and	sv5, sv5, #0x0F			@ sv5 <- logical endpoint
@	str	rvb, [rva, sv5, LSL #2]		@ USB_EPnR <- EP data now VALID (transmit when needed)
	set	pc,  lnk

_func_
usbhwStallEP: @ Stall the EndPoint in sv5
	and	rvb, sv5, #0x0F			@ rvb <- logical endpoint
	ldr	rva, =usb_base
	tst	sv5, #0x80
	itE	eq
	addeq	rva, rva, #0x0b00
	addne	rva, rva, #0x0900
	add	rva, rva, rvb, LSL #5
	ldr	rvb, [rva]			@ rvb <- USB_OTG_FS_DIEPCTLx / DOEPCTLx
	orr	rvb, rvb, #(1 << 21)
	str	rvb, [rva]			@ stall EP
	
@	and	rvb, sv5, #0x0F			@ rvb <- logical endpoint
@	ldr	rva, =usb_base
@	ldr	rvb, [rva, rvb, LSL #2]		@ rvb <- USB_EPnR
@	bic	rvb, rvb, #0xF100
@	bic	rvb, rvb, #0x00C0
@	tst	sv5, #0x80
@	itE	ne
@	eorne	rvb, rvb, #0x0010
@	eoreq	rvb, rvb, #0x0100
@	orr	rvb, rvb, #0x8000
@	and	sv5, sv5, #0x0F			@ rvb <- logical endpoint
@	str	rvb, [rva, sv5, LSL #2]		@ USB_EPnR <- EP data now VALID (transmit when needed)
	set	pc,  lnk

_func_
usbhwDGD: @ 9.4.3 Get Descriptor of Device Standard request	
	bl	wrtEP
	b	usbSOx

_func_
usbSOx:	@ Prepare setup buffer for Status OUT Phase 
	ldr	rva, =USB_SETUP_BUFFER
	ldr	rvb, =0xFF
	str	rvb, [rva]
	b	usbEPx

_func_
usbhwEGS: @ Get stall Status of Endpoint in sv5 into rvb
	and	rvb, sv5, #0x0F			@ rvb <- logical endpoint
	ldr	rva, =usb_base
	tst	sv5, #0x80
	itE	eq
	addeq	rva, rva, #0x0b00
	addne	rva, rva, #0x0900
	add	rva, rva, rvb, LSL #5
	ldr	rvb, [rva]			@ rvb <- USB_OTG_FS_DIEPCTLx / DOEPCTLx
	tst	rvb, #(1 << 21)			@ is EP non-stalled
	itE	ne
	setne	rvb, #0				@	if not, rvb <- 0, stalled
	seteq	rvb, #1				@	if so,  rvb <- 1, not stalled
	set	pc,  lnk

_func_
usbhwSetAddress: @ Set Device to Address in sv5
	@ USB Status IN  exit -- write null packet to   EP 1 (phys, aka 0x80)
	@ disable the correct transfer interrupt mask (CTRM) in USB_CNTR
@	ldr	rva, =usb_base
@@	ldr	rvb, [rva, #0x40]
@@	bic	rvb, rvb, #0x9C00
@@	str	rvb, [rva, #0x40]	
@	set	env, #UsbControlInEP		@ env <- Control IN EndPoint
@	ldr	dts, =USB_DATA			@ dts <- buffer
@	set	cnt, #0x00			@ cnt <- 0 bytes to send
@	bl	wrtEP				@ write 0 bytes to EP
@hwsta0:	@ wait for tx to be complete (CTR_TX in USB_EP0R) then clear CTR_TX
@	ldr	rvb, [rva]			@ rvb <- contents of USB_EP0R
@	tst	rvb, #0x80			@ is CTR_TX set?
@	beq	hwsta0				@	if not, jump to keep waiting
@	ldr	rvb, [rva]			@ rvb <- contents of USB_EP0R
@	bic	rvb, rvb, #0xF100
@	bic	rvb, rvb, #0x00F0
@	orr	rvb, rvb, #0x8000
@	str	rvb, [rva]			@ USB_EPnR <- Clear CTR-TX interrupt on EP in env
@	ldr	rvb, [rva]			@ rvb <- contents of USB_EP0R
@	bic	rvb, rvb, #0xF100
@	bic	rvb, rvb, #0x00C0
@	eor	rvb, rvb, #0x0030
@	orr	rvb, rvb, #0x8000
@	str	rvb, [rva]			@ USB_EPnR <- EP data now VALID (transmit when needed)
	@ re-enable the correct transfer interrupt mask (CTRM) in USB_CNTR
@	ldr	rva, =usb_base
@	ldr	rvb, [rva, #0x40]
@	orr	rvb, rvb, #0x9C00
@	str	rvb, [rva, #0x40]
	
	ldr	rva, =USB_SETUP_BUFFER		@ rva <- address of setup buffer
	ldr	sv5, [rva]			@ sv5 <- reqtyp(8), request(8), val(16)
	lsr	sv5, sv5, #16			@ sv5 <- address = val(16)

	lsl	rvb, sv5, #4
	orr	rvb, rvb, #0x03
	ldr	rva, =usb_base
	str	rvb, [rva, #usb_daddr]		@ set address
	
@@@ send status IN  <- ****************
	
@	b	usbEPx
	b	usbSIx

_func_
usbhwDeconfigure: @ Deconfigure the device
	@ clear the readbuffer
	ldr	rva, =BUFFER_START
	vcrfi	rva, rva, READ_BF_offset
	set	rvb, #i0
	vcsti	rva, 0, rvb
	@ set uart to generate Rx interrupts
	ldr	rva, =uart0_base
	ldr	rvb, =0x202c
	str	rvb, [rva, #0x0c]		@ USART_CR1   <- USART, Tx-Rx enabled, 8N1, Rx interrupt
	@ set default i/o port to uart
	ldr	rvb, =vuart0
	vcsti	glv, 4, rvb			@ default input/output port model	
	set	pc,  lnk
	
_func_
usbhwConfigure: @ Configure the device
	@ stop uart from generating Rx interrupts (they cause noise on shared READBUFFER)
	ldr	rva, =uart0_base
	ldr	rvb, =0x200c
	str	rvb, [rva, #0x0c]		@ USART_CR1   <- USART, Tx-Rx enabled, 8N1, no Rx interrupt
	@ clear the readbuffer
	ldr	rva, =BUFFER_START
	vcrfi	rva, rva, READ_BF_offset
	set	rvb, #i0
	vcsti	rva, 0, rvb
	@ configure USB
	ldr	rva, =usb_base

	add	rva, rva, #0x0900
@	set	rvb, #8
@	str	rvb, [rva, #0x30]		@ OTG_FS_DIEPTSIZ1 <- EP1 packet size = 8 bytes
	ldr	rvb, = #((1 << 22) | (3 << 18) | (1 << 15) | 8)
	str	rvb, [rva, #0x20]		@ OTG_FS_DIEPCTL1  <- EP1 FIFO 1, Interrupt IN, active, 8 bytes
@	set	rvb, #64
@	str	rvb, [rva, #0x70]		@ OTG_FS_DIEPTSIZ3 <- EP3 packet size = 64 bytes
	ldr	rvb, = #((3 << 22) | (2 << 18) | (1 << 15) | 64)
	str	rvb, [rva, #0x60]		@ OTG_FS_DIEPCTL3  <- EP3 FIFO 3, Bulk IN, active, 64 bytes
	add	rva, rva, #0x0200
@	set	rvb, #64
@	str	rvb, [rva, #0x50]		@ OTG_FS_DIEPTSIZ2 <- EP2 packet size = 64 bytes
	ldr	rvb, = #((2 << 18) | (1 << 15) | 64)
	str	rvb, [rva, #0x40]		@ OTG_FS_DOEPCTL2  <- EP2 Bulk OUT, active, 64 bytes
	
	ldr	rva, =usb_base
	add	rva, rva, #0x0800
	ldr	rvb, [rva, #0x1c]		@ rvb <- OTG_FS_DAINTMSK
	orr	rvb, rvb, #0x040000
	orr	rvb, rvb, #0x0a
	str	rvb, [rva, #0x1c]		@ OTG_FS_DAINTMSK <- unmask interrupts for EP 1,3 IN, 2 OUT

@	ldr	rvb, =0x0621
@	str	rvb, [rva, #0x04]		@ enable EP1 -- Interrupt IN
@	ldr	rvb, =0x3002
@	str	rvb, [rva, #0x08]		@ enable EP2 -- Bulk OUT
@	ldr	rvb, =0x0023
@	str	rvb, [rva, #0x0C]		@ enable EP3 -- Bulk IN
	@ set default i/o port to usb
	ldr	rvb, =vusb
	vcsti	glv, 4, rvb			@ default input/output port model	
	set	pc,  lnk

_func_
usbhwwrtEPA:
	and	env, env, #0x0F

	ldr	rva, =usb_base

	add	rva, rva, #0x0910
	set	rvb, #(1 << 19)
	orr	rvb, rvb, cnt
	add	rva, rva, env, LSL #5
	str	rvb, [rva]			@ OTG_FS_DIEPTSIZx <- number of bytes to send for this 1 packet
	
	sub	rva, rva, #0x08
@	ldr	rvb, [rva]			@ rvb <- OTG_FS_DIEPINTx
@	orr	rvb, rvb, #1
@	str	rvb, [rva]			@ OTG_FS_DIEPINTx  <- clear Tx interrupt on EP
	
	sub	rva, rva, #0x08
	ldr	rvb, [rva]			@ rvb <- OTG_FS_DIEPCTLx
	orr	rvb, rvb, #(1 << 31)
	str	rvb, [rva]			@ OTG_FS_DIEPCTLx  <- enable EP for transmission
	
	ldr	rva, =usb_base
	add	rva, rva, #0x1000
	add	rva, rva, env, LSL #12		@ rva <- address of FIFO
	set	rvb, #0
wrtEPM:	cmp	rvb, cnt
	bpl	wrtEPL
@	ldrh	rvc, [dts, rvb]
@	str	rvc, [rva, rvb, LSL #1]
@	add	rvb, rvb, #2
	ldr	rvc, [dts, rvb]
	str	rvc, [rva]
	add	rvb, rvb, #4
	b	wrtEPM
wrtEPL:	
	set	pc,  lnk
	
@	ldr	rva, =0x40006000		@ rva <- usb hardware buffer start
@	add	rva, rva, env, LSL #4		@ rva <- start of packet buffer table for EP in env
@	str	cnt, [rva, #4]			@ store number of byte to send in USB_COUNTn_TX
@	ldr	rvb, [rva, #0]			@ rvb <- offset to USB_ADRn_TX data / 2
@	ldr	rva, =0x40006000		@ rva <- usb hardware buffer start
@	add	rva, rva, rvb, LSL #1		@ rva <- address of start of Tx packet memory for EP in env
@	set	rvb, #0
@wrtEPL:	cmp	rvb, cnt
@	bpl	wrtEPM
@	ldrh	rvc, [dts, rvb]
@	str	rvc, [rva, rvb, LSL #1]
@	add	rvb, rvb, #2
@	b	wrtEPL
@wrtEPM:	ldr	rva, =usb_base
@	ldr	rvb, [rva, env, LSL #2]		@ rvb <- USB_EPnR
@	bic	rvb, rvb, #0xF100
@	bic	rvb, rvb, #0x00C0
@	eor	rvb, rvb, #0x0030
@	orr	rvb, rvb, #0x8000
@	orr	rvb, rvb, #0x0080
@	str	rvb, [rva, env, LSL #2]		@ USB_EPnR <- EP data now VALID (transmit when needed)
@	set	pc,  lnk
	
_func_
wrtEP:	@ write data to Control In Endpoint (write cnt bytes starting at dts to enpoint in env)
wrtEPU:	@ write data to Control In Endpoint (write cnt bytes starting at dts to enpoint in env)

@@@ ****** turn some LED ON / toggle HERE **** ????
	
	and	env, env, #0x0F
	
@	ldr	rva, =usb_base
@	add	rva, rva, #0x1000
@	add	rva, rva, env, LSL #12		@ rva <- address of FIFO
@	set	rvb, #0
@wrtEPX:	cmp	rvb, cnt
@	bpl	wrtEPY
@@	ldrh	rvc, [dts, rvb]
@@	str	rvc, [rva, rvb, LSL #1]
@@	add	rvb, rvb, #2
@@	ldr	rvc, [dts, rvb]
@@	str	rvc, [rva]
@@	add	rvb, rvb, #4
@	ldr	rvc, [dts, rvb]
@	str	rvc, [rva, rvb]
@	add	rvb, rvb, #4
@	b	wrtEPX
@wrtEPY:
	
	ldr	rva, =usb_base

	add	rva, rva, #0x0910
	add	rva, rva, env, LSL #5
	set	rvb, #(1 << 19)
	orr	rvb, rvb, cnt
	str	rvb, [rva]			@ OTG_FS_DIEPTSIZx <- number of bytes to send for this 1 packet
	
	sub	rva, rva, #0x08
	ldr	rvb, [rva]			@ rvb <- OTG_FS_DIEPINTx
	orr	rvb, rvb, #1
	str	rvb, [rva]			@ OTG_FS_DIEPINTx  <- clear Tx interrupt on EP
	
	sub	rva, rva, #0x08
	ldr	rvb, [rva]			@ rvb <- OTG_FS_DIEPCTLx
	orr	rvb, rvb, #(1 << 31)		@ rvb <- enable EP bit
	orr	rvb, rvb, #(1 << 26)		@ rvb <- clear NAK generation
	str	rvb, [rva]			@ OTG_FS_DIEPCTLx  <- enable EP for transmission

		
@	ldr	rvb, [rva, env, LSL #2]		@ rvb <- USB_EPnR
@	bic	rvb, rvb, #0xF100
@	bic	rvb, rvb, #0x00F0
@	orr	rvb, rvb, #0x8000
@	str	rvb, [rva, env, LSL #2]		@ USB_EPnR <- Clear CTR-TX interrupt on EP in env
@	ldr	rva, =0x40006000		@ rva <- usb hardware buffer start
@	add	rva, rva, env, LSL #4		@ rva <- start of packet buffer table for EP in env
@wrtEPW:	set	rvb, #0
@	str	rvb, [rva, #4]			@ store number of byte to send in USB_COUNTn_TX
@	ldr	rvb, [rva, #4]			@ store number of byte to send in USB_COUNTn_TX
@	eq	rvb, #0
@	bne	wrtEPW
@	str	cnt, [rva, #4]			@ store number of byte to send in USB_COUNTn_TX
	
@	ldr	rvb, [rva, #0]			@ rvb <- offset to USB_ADRn_TX data / 2
@	ldr	rva, =0x40006000		@ rva <- usb hardware buffer start
	
	ldr	rva, =usb_base
	add	rva, rva, #0x1000
	add	rva, rva, env, LSL #12		@ rva <- address of FIFO
	set	rvb, #0
wrtEPX:	cmp	rvb, cnt
	bpl	wrtEPY
@	ldrh	rvc, [dts, rvb]
@	str	rvc, [rva, rvb, LSL #1]
@	add	rvb, rvb, #2
	ldr	rvc, [dts, rvb]
	str	rvc, [rva]
	add	rvb, rvb, #4
	b	wrtEPX
wrtEPY:
	
	eq	cnt, #0
	it	eq
	seteq	pc,  lnk
	eq	env, #0
	it	ne
	setne	pc,  lnk

	ldr	rva, =USB_CHUNK
	ldr	rvb, [rva]		@ rvb <- how many bytes will remain to be sent
	eq	rvb, #0
	itTTT	ne
	ldrne	rva, =usb_base
	addne	rva, rva, #0x0800
	setne	rvb, #1
	strne	rvb, [rva, #0x34]
	
@	ldr	rva, =usb_base
@	ldr	rvb, [rva, env, LSL #2]		@ rvb <- USB_EPnR
@	bic	rvb, rvb, #0xF100
@	bic	rvb, rvb, #0x00C0
@	eor	rvb, rvb, #0x0030
@	orr	rvb, rvb, #0x8000
@	orr	rvb, rvb, #0x0080
@	str	rvb, [rva, env, LSL #2]		@ USB_EPnR <- EP data now VALID (transmit when needed)
	set	pc,  lnk
	
_func_
rdEP:	@ read from endpoint in env to buffer in dts with count in cnt
	and	env, env, #0x0F
	ldr	rva, =usb_base

	ldr	rvb, [rva, #0x20]		@ rvb <- OTG_FS_GRXSTSP (pop Rx FIFO)
	lsr	cnt, rvb, #4
	and	cnt, cnt, #0xff			@ cnt <- received byte count (up to 256 bytes)
	
@	add	rva, rva, #0x0b10
@	set	rvb, #(1 << 19)
@	orr	rvb, rvb, cnt
@	add	rva, rva, env, LSL #5
@	str	rvb, [rva]			@ OTG_FS_DOEPTSIZx <- number of bytes to read for this 1 packet
	
@	sub	rva, rva, #0x08
@@	ldr	rvb, [rva]			@ rvb <- OTG_FS_DIEPINTx
@@	orr	rvb, rvb, #1
@@	str	rvb, [rva]			@ OTG_FS_DIEPINTx  <- clear Tx interrupt on EP
	
@	sub	rva, rva, #0x08
@	ldr	rvb, [rva]			@ rvb <- OTG_FS_DOEPCTLx
@	orr	rvb, rvb, #(1 << 31)
@	orr	rvb, rvb, #(1 << 26)		@ rvb <- clear NAK generation
@	str	rvb, [rva]			@ OTG_FS_DOEPCTLx  <- enable EP for reception

	ldr	rva, =usb_base
@c	add	rva, rva, #0x1000
@c	add	rva, rva, env, LSL #12		@ rva <- address of FIFO
	set	rvb, #0
usbSEZ:	cmp	rvb, cnt
	bpl	usbSEX
@	ldr	rvc, [rva, rvb, LSL #1]
@	strh	rvc, [dts, rvb]
@	add	rvb, rvb, #2
@c	ldr	rvc, [rva]
	ldr	rvc, [rva, #0x20]
	str	rvc, [dts, rvb]		@ NOT STRH !!!!!!!!
	add	rvb, rvb, #4
	b	usbSEZ
usbSEX:

	ldr	rva, =usb_base
	eq	env, #0
@	it	eq
@	ldreq	rvb, [rva, #0x20]		@ rvb <- OTG_FS_GRXSTSP (pop Rx FIFO) -- clear

	it	ne
	setne	pc,  lnk

	
@	ldr	rvb, [rva, #0x20]		@ rvb <- OTG_FS_GRXSTSP (pop Rx FIFO) -- clear

usbSEB:	ldr	rvb, [rva, #0x14]
	tst	rvb, #0x10
	it	ne
	ldrne	rvb, [rva, #0x20]
	bne	usbSEB
	
	add	rva, rva, #0x0b10
	set	rvb, #(1 << 19)
	orr	rvb, rvb, #(1 << 29)
	orr	rvb, rvb, #8
@	add	rva, rva, env, LSL #5
	str	rvb, [rva]			@ OTG_FS_DOEPTSIZx <- number of bytes to read for this 1 packet
	
	sub	rva, rva, #0x08
@	ldr	rvb, [rva]			@ rvb <- OTG_FS_DIEPINTx
@	orr	rvb, rvb, #1
@	str	rvb, [rva]			@ OTG_FS_DIEPINTx  <- clear Tx interrupt on EP
	
	sub	rva, rva, #0x08
@	ldr	rvb, [rva]			@ rvb <- OTG_FS_DOEPCTLx
	set	rvb, #3
	orr	rvb, rvb, #(1 << 31)
	orr	rvb, rvb, #(1 << 26)		@ rvb <- clear NAK generation
	str	rvb, [rva]			@ OTG_FS_DOEPCTLx  <- enable EP for reception
	
	set	pc,  lnk
	
@	ldr	rva, =usb_base
@	ldr	rvb, [rva, env, LSL #2]		@ rvb <- USB_EPnR
@	bic	rvb, rvb, #0xF000
@	bic	rvb, rvb, #0x00F0
@	orr	rvb, rvb, #0x0080
@	str	rvb, [rva, env, LSL #2]		@ USB_EP0R <- Clear CTR-RX interrupt on EP0
@	ldr	rva, =0x40006000		@ rva <- usb hardware buffer start
@	add	rva, rva, env, LSL #4		@ rva <- start of packet buffer table for EP0
@	ldr	cnt, [rva, #12]			@ cnt <- number of bytes received (from USB_COUNTn_RX)
@	bic	cnt, cnt, #0xFC00		@ cnt <- number of bytes received
@	ldr	rvb, [rva, #8]			@ rvb <- offset to USB_ADRn_RX data / 2
@	ldr	rva, =0x40006000		@ rva <- usb hardware buffer start
@	add	rva, rva, rvb, LSL #1		@ rva <- address of start of Rx packet memory for EP in env
@	set	rvb, #0
@usbSEZ:	cmp	rvb, cnt
@	bpl	usbSEX
@	ldr	rvc, [rva, rvb, LSL #1]
@	strh	rvc, [dts, rvb]
@	add	rvb, rvb, #2
@	b	usbSEZ
@usbSEX:	ldr	rva, =usb_base
@	ldr	rvb, [rva, env, LSL #2]		@ rvb <- USB_EPnR
@	bic	rvb, rvb, #0xC000
@	bic	rvb, rvb, #0x00F0
@	eor	rvb, rvb, #0x3000
@	orr	rvb, rvb, #0x0080
@	orr	rvb, rvb, #0x8000
@	str	rvb, [rva, env, LSL #2]		@ USB_EP0R <- EP data now VALID (receive when needed)
@	set	pc,  lnk

_func_
usbhwr:	@ write writebuffer to USB Bulk IN endpoint (EP 3)
	@ on entry:	sv5 <- USB Write Buffer
	@ on entry:	rvb <- number of bytes to send
	@ called with interrupts disabled
	@ write data packet to send
	@ modifies:	sv3, rva, rvb, rvc
	lsl	rvb, rvb, #2
	orr	sv3, rvb, #i0			@ sv3 <- number of bytes to send (scheme int)
	
	ldr	rva, =usb_base

	add	rva, rva, #0x0970
	set	rvb, #(1 << 19)
	orr	rvb, rvb, sv3, LSR #2
	str	rvb, [rva]			@ OTG_FS_DIEPTSIZ3 <- number of bytes to send for this 1 packet
	
	sub	rva, rva, #0x08
@	ldr	rvb, [rva, env]			@ rvb <- OTG_FS_DIEPINT3
@	orr	rvb, rvb, #1
@	str	rvb, [rva, env, LSL #5]		@ OTG_FS_DIEPINTx  <- clear Tx interrupt on EP
	
	sub	rva, rva, #0x08
	ldr	rvb, [rva, env]			@ rvb <- OTG_FS_DIEPCTL3
	orr	rvb, rvb, #(1 << 31)
	str	rvb, [rva, env]			@ OTG_FS_DIEPCTL3  <- enable EP for transmission

	ldr	rva, =usb_base
	add	rva, rva, #0x4000
@	add	rva, rva, cnt, LSL #12		@ rva <- address of FIFO
	set	rvb, #0
usbwr1:	cmp	rvb, sv3, LSR #2
	bpl	usbwr2
@	ldrh	rvc, [dts, rvb]
@	str	rvc, [rva, rvb, LSL #1]
@	add	rvb, rvb, #2
	add	rvb, rvb, #8
	ldr	rvc, [sv5, rvb]
	str	rvc, [rva]
@	add	rvb, rvb, #4
	sub	rvb, rvb, #4
	b	usbwr1
usbwr2:
	b	usbwcn				@ return
	
@	ldr	rva, =0x40006000		@ rva <- usb hardware buffer start
@	lsr	rvb, sv3, #2			@ rvb <- number of bytes to send (raw int)
@	str	rvb, [rva, #0x34]		@ store number of byte to send in USB_COUNT3_TX
@	ldr	rvb, [rva, #0x30]		@ rvb <- offset to USB_ADR3_TX data / 2
@	add	rva, rva, rvb, LSL #1		@ rva <- address of start of Tx packet memory for EP
@	set	rvc, #i0
@usbwr1:	cmp	rvc, sv3
@	bpl	usbwr2
@	set	rvb, #8
@	add	rvb, rvb, rvc, LSR #2
@	ldrh	rvb, [sv5, rvb]
@	lsr	rvc, rvc, #1
@	str	rvb, [rva, rvc]
@	lsl	rvc, rvc, #1
@	orr	rvc, rvc, #i0
@	add	rvc, rvc, #8
@	b	usbwr1
@usbwr2:	ldr	rva, =usb_base
@	ldr	rvb, [rva, #0x0C]		@ rvb <- contents of USB_EP3R
@	bic	rvb, rvb, #0xF100
@	bic	rvb, rvb, #0x00C0
@	eor	rvb, rvb, #0x0030
@	orr	rvb, rvb, #0x8000
@	orr	rvb, rvb, #0x0080
@	str	rvb, [rva, #0x0C]		@ USB_EP3R <- EP data now VALID (transmit when needed)
@	b	usbwcn				@ return

.endif

	