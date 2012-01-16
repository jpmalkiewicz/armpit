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


_func_
usbhwDeviceStatus: @ return device status in rvb
	ldr	rva, =usb_base
	str	sv1, [rva, #usb_iclear_dv]	@ clear USB interrupt
	set	sv4, lnk
	ldr	rvb, =0xFE0500			@ rvb <- Get Device Status command
	bl	wrtcmd				@ get the device status
	ldr	rvb, =0xFE0200			@ rvb <- Read Device Status command
	bl	rdcmd				@ read the device status to rvb
	set	pc,  sv4

_func_
usbhwReset:
	set	pc,  lnk

_func_
usbhwRemoteWakeUp:	
	set	sv4, lnk
	ldr	rvb, =0xFE0500		@ rvb <- Get/Set Device Status command
	bl	wrtcmd			@ get ready to set the device status
	ldr	rvb, =0x010100		@ rvb <- Set Status command
	bl	wrtcmd			@ set the device status to 0x01 (remote wakeup)
	set	pc,  sv4
	
_func_
usbhwEndpointStatus: @ return endpoint status in sv3
	ldr	rva, =usb_base
	ldr	sv2, [rva, #usb_istat_ep]	@ sv2 <- Endpoint Interrupt Status (eisr)
	str	sv2, [rva, #usb_iclear_ep]	@ clear the interrupt
usbhw0:	ldr	rvb, [rva, #usb_istat_dv]	@ rvb <- Device Interrupt Status
	tst	rvb, #usb_icd_full		@ is command data ready (i.e. rvb has CDFULL=0x20) ?
	beq	usbhw0				@	if not, jump to wait for it
	ldr	sv3, [rva, #usb_cmd_data]	@ sv3 <- command data (EP status)
	set	pc,  lnk

_func_
usbhwCIi: @ Control IN interrupt response
	b	wrtEPU

_func_
usbhwSetup: @ Control OUT Interrupt, Setup Phase
	b	rdEP

_func_
usbhwStatusOut:	@ Control OUT Interrupt, Status OUT Phase
	set	env,  #UsbControlOutEP	@ env <- Control OUT EndPoint
	ldr	dts, =USB_DATA		@ dts <- buffer
	set	cnt, #0			@ cnt <- 0 bytes to read
	bl	rdEP			@ read 0 bytes from EP
	b	usbEPx

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
usbhwClearRxBk:
	set	pc,  lnk

_func_
usbhwwrtEPA:
	b	wrtEP			@ write data from dts to host, return to point of call
	
_func_
usbhwixx:
	b	usbEPx
	
_func_
usbhwCleariTX: @ clear the txendpkt interrupt
	and	env, sv1, #usb_itxendp
	ldr	rva, =usb_base
	str	env, [rva, #usb_iclear_dv]	@ clear USB interrupt register (DEV_INT_CLR = disr)
	set	pc,  lnk

_func_
usbhwEGS: @ Get Status of Endpoint in sv5 into rvb
	set	sv4, lnk
	bl	usbhwEPSet			@ rvb <- EP formated for command writing (physical endpoint)
	bic	env, rvb, #0x500		@ env <- physical endpoint, shifted, saved
	bl	wrtcmd				@ select endpoint
	orr	rvb, env, #0x200		@ rvb <- command to read status
	bl	rdcmd				@ rvb <- cmd/data (read status)
	tst	rvb, #0x02			@ is the selected endpoint stalled?
	itE	eq
	seteq	rvb, #0				@	if not, rvb <- 0, not stalled
	setne	rvb, #1				@	if so,  rvb <- 1, stalled
	set	pc,  sv4

_func_
usbhwSetAddress: @ Set Device to Address in sv5
	ldr	rvb, =0xD00500			@ rvb <- Set Address command
	bl	wrtcmd				@ execute Set Address command
	ldr	rva, =USB_SETUP_BUFFER		@ rva <- address of setup buffer
	ldr	rvb, [rva]			@ rvb <- reqtyp(8), request(8), val(16)
	lsr	rvb, rvb, #16			@ rvb <- address = val(16)
	orr	rvb, rvb, #0x80			@ rvb <- address ored with Device Enable (0x80)
	lsl	rvb, rvb, #16			@ rvb <- address/enable shifted
	orr	rvb, rvb, #0x0100		@ rvb <- command to set address (part 2)
	bl	wrtcmd				@ Set the address
	b	usbSIx				@ jump to Status IN Phase and exit
	
_func_
usbhwDeconfigure: @ Deconfigure the device
	set	sv4, lnk
	ldr	rvb, =0xD80500			@ rvb <- command to configure the device (0xD8)
	bl	wrtcmd				@ set device configuration status ...
	ldr	rvb, =0x100			@ rvb <- data for command = 0
	bl	wrtcmd				@ ... to 0
	@ clear the readbuffer
	ldr	rva, =BUFFER_START
	vcrfi	rva, rva, READ_BF_offset
	set	rvb, #i0
	vcsti	rva, 0, rvb
	@ set uart to generate Rx interrupts
	ldr	rva, =uart0_base
	set	rvb, #1
	str	rvb, [rva, #uart_ier]	@ U0IER <- disable UART0 RDA interrupt
	@ set default i/o port to uart
	ldr	rvb, =vuart0
	vcsti	glv, 4, rvb		@ default input/output port model
	set	pc,  sv4
	
_func_
usbhwConfigure: @ Configure the device
	set	sv4, lnk
	@ stop uart from generating Rx interrupts (they cause noise on shared READBUFFER)
	ldr	rva, =uart0_base
	set	rvb, #0
	str	rvb, [rva, #uart_ier]	@ U0IER <- disable UART0 RDA interrupt
	@ clear the readbuffer
	ldr	rva, =BUFFER_START
	vcrfi	rva, rva, READ_BF_offset
	set	rvb, #i0
	vcsti	rva, 0, rvb
	@ Realize the Interrupt In Endpoint (phys 3, log 1, aka 0x81)
	set	sv5, #3
	bl	usbhwReEP
	ldr	rvb, =0x430500			@ rvb <- command to set status of EP 3 (phys)
	bl	wrtcmd				@ set endpoint status ...
	ldr	rvb, =0x100			@ rvb <- data for command = 0
	bl	wrtcmd				@ ... to 0
	@ Realize the BULK OUT Endpoint (phys 4, log 2, aka 0x02)
	set	sv5, #4
	bl	usbhwReEP
	ldr	rvb, =0x440500			@ rvb <- command to set status of EP 0xA (phys)
	bl	wrtcmd				@ set endpoint status ...
	ldr	rvb, =0x100			@ rvb <- data for command = 0
	bl	wrtcmd				@ ... to 0
	@ Realize the BULK IN Endpoint (phys 5, log 2, aka 0x82)
	set	sv5, #5
	bl	usbhwReEP
	ldr	rvb, =0x450500			@ rvb <- command to set status of EP 0xA (phys)
	bl	wrtcmd				@ set endpoint status ...
	ldr	rvb, =0x100			@ rvb <- data for command = 0
	bl	wrtcmd				@ ... to 0
	@ configure device
	ldr	rvb, =0xD80500			@ rvb <- command to configure the device (0xD8)
	bl	wrtcmd				@ set device configuration status ...
	ldr	rvb, =0x010100			@ rvb <- data for command = 1
	bl	wrtcmd				@ ... to 1
	@ set default i/o port to usb
	ldr	rvb, =vusb
	vcsti	glv, 4, rvb			@ default input/output port model
	set	pc,  sv4


usbhwReEP: @ realize endpoint in sv5 (raw int)
	@ modifies:	rva, rvb
	set	rvb, #1
	lsl	rvb, rvb, sv5
	ldr	rva, =usb_base
	ldr	rva, [rva, #usb_reep]		@ rva <- current content of ReEP register
	orr	rvb, rva, rvb			@ bit for EP 3
	ldr	rva, =usb_base
	str	rvb, [rva, #usb_reep]		@ OR endpoint with existing value of realized register 
	str	sv5, [rva, #usb_epind]		@ Load endpoint index Reg with physical endpoint no
	cmp	sv5, #4
	itE	mi
	setmi	rvb, #0x08			@ rvb <-  8 bytes = max packet size for EP 3
	setpl	rvb, #0x64			@ rvb <- 64 bytes = max packet size for EP 4 and 5
	str	rvb, [rva, #usb_maxpsize]	@ load the max packet size Register
usbhw1:	ldr	rvb, [rva, #usb_istat_dv]	@ check whether the EP Reallized bit is set
	tst	rvb, #0x0100			@ is EP realized (= 0x100)?
	beq	usbhw1				@	if not, jump to wait for this
	set	rvb, #0x0100			@ rvb <- EP Reallized bit = 0x100
	str	rvb, [rva, #usb_iclear_dv]	@ Clear the EP Realized bit
	set	pc,  lnk

_func_
usbhwUnstallEP:	@ Unstall the EndPoint in sv5
	set	sv4, lnk
	bl	usbhwEPSet		@ rvb <- EP formated for command writing (physical endpoint)
	orr	rvb, rvb, #0x400000	@ rvb <- full command to set status of EP
	bl	wrtcmd			@ set status of EP ...
	ldr	rvb, =0x000100
	bl	wrtcmd			@ ... to 0 = not stalled	
	set	pc,  sv4

_func_
usbhwStallEP: @ Stall EP in sv5
	set	sv4, lnk
	bl	usbhwEPSet		@ rvb <- EP formated for command writing (physical endpoint)
	orr	rvb, rvb, #0x400000	@ rvb <- full command to set status of EP
	bl	wrtcmd			@ set status of EP ...
	ldr	rvb, =0x010100
	bl	wrtcmd			@ ... to 1 = stalled
	set	pc,  sv4

_func_
usbStall: @ stall endpoint 1 (phys 1, log 0, aka 0x80)
	ldr	rvb, =0x410500		@ rvb <- set endpoint 1 status command
	bl	wrtcmd			@ USBHwCmdWrite(CMD_EP_SET_STATUS==0x40 | idx==1 ...
	ldr	rvb, =0x010100
	bl	wrtcmd			@ ... , EP_ST == 1);
	b	usbEPx

_func_
rdEP:	@ (eg. section 9.13) uses rva, rvb, env, dts, cnt, returns cnt = count
	@ env <- EPNum, dts <- buffer
	@ set read_enable bit, and endpoint to use in control register
	and	rvb, env, #0x0F		@ rvb <- logical endpoint number
	lsl	rvb, rvb, #2		@ rvb <- logical endpoint number, shifted
	orr	rvb, rvb, #0x01		@ rvb <- log ep number shifted ored with read_enable = 0x01
	ldr	rva, =usb_base
	str	rvb, [rva, #usb_ctrl]	@ enable reading from endpoint buffer
	@ wait for packet ready
rdEP_0:	ldr	cnt, [rva, #usb_rxplen]	@ cnt <- contents of Receive Length register
	tst	cnt, #0x800		@ is the received packet ready (0x800) ?
	beq	rdEP_0			@	if not, jump to keep waiting
	@ verify that packet is valid
	tst	cnt, #0x400		@ is packet valid ?
	itT	eq
	seteq	cnt, #-1		@	if not, set count to -1
	seteq	pc,  lnk		@	if not, return (with count{cnt} = -1)
	@ get count from packet
	ldr	rvb, =0x3FF		@ rvb <- mask to get number of bytes
	and	cnt, cnt, rvb		@ cnt <- number of bytes read
	@ read data
rdEP_1:	ldr	rvb, [rva, #usb_ctrl]
	tst	rvb, #0x01		@ is read_enable still asserted ?
	beq	rdEP_2			@	if not, exit read loop
	ldr	rvb, [rva, #usb_rxdata]	@ rvb <- word of data read
	str	rvb, [dts]		@ store it in buffer
	add	dts, dts, #4		@ dts <- updated data storage address
	b	rdEP_1
rdEP_2:	set	dts, lnk		@ dts <- saved lnk
	@ send select endpoint to protocol engine
	set	sv5, env
	bl	usbhwEPSet		@ rvb <- EP formated for command writing (physical endpoint)
	bl	wrtcmd			@ select the endpoint
	@ issue clear buffer command (0xF2)
	ldr	rvb, =0xF20500		@ rvb <- clear buffer command = 0x00F20500
	bl	wrtcmd			@ clear the endpoint's receive buffer
	set	pc,  dts		@ return
	
_func_
wrtEP:	@ (eg. section 9.14) uses rva, rvb, env, dts, cnt
	@ env <- EPNum, dts <- buffer, cnt <- cnt
	@ set write_enable bit, and endpoint to use in control register
	and	rvb, env, #0x0F		@ rvb <- logical endpoint number
	lsl	rvb, rvb, #2		@ rvb <- logical endpoint number, shifted
	orr	rvb, rvb, #0x02		@ rvb <- log ep number shifted ored with write_enable = 0x02
	ldr	rva, =usb_base		@ rva <- USB base register
	str	rvb, [rva, #usb_ctrl]	@ enable writing to endpoint buffer
	str	cnt, [rva, #usb_txplen]	@ set the number of bytes to be sent
	@ write data packet to send
wrtEP1:	ldr	rvb, [rva, #usb_ctrl]
	tst	rvb, #0x02		@ is write_enable still asserted ?
	beq	wrtEP2			@	if not, exit write loop
	ldr	rvb, [dts]		@ rvb <- next data word
	str	rvb, [rva, #usb_txdata]	@ write data to Transmit register
	add	dts, dts, #4		@ dts <- updated data source address
	b	wrtEP1
wrtEP2:	set	dts, lnk		@ dts <- saved lnk
	@ send select endpoint command to protocol engine
	set	sv5, env
	bl	usbhwEPSet		@ rvb <- EP formated for command writing (physical endpoint)
	bl	wrtcmd			@ select the endpoint
	@ issue validate buffer command (0xFA)
	ldr	rvb, =0xFA0500		@ rvb <- validate buffer command = 0x00FA0500
	bl	wrtcmd			@ validate the endpoint's transmit buffer
	set	pc,  dts		@ return

_func_
wrtEPU:	@ (eg. section 9.14)
	@ env <- EPNum, dts <- buffer, cnt <- cnt
	set	sv4, lnk
	bl	wrtEP
	bic	sv1, sv1, #usb_itxendp		@ exclude Txendpkt bit from interrupt clearing
	ldr	rva, =usb_base
	str	sv1, [rva, #usb_iclear_dv]	@ clear USB interrupt
	set	pc,  sv4

_func_
usbhwr:	@ write writebuffer to USB Bulk IN endpoint 5 (phys 5, log 2, aka 0x82) (eg. section 9.14)
	@ on entry:	sv5 <- USB Write Buffer
	@ on entry:	rvb <- number of bytes to send
	@ set write_enable bit, and endpoint to use in control register, set length of packet to send
	@ modifies:	sv3, sv5, rva, rvb
	ldr	rva, =usb_base
	set	sv3, #0x0A			@ rvb <- 1010 = log ep 2 shftd by 2 ored with WR_EN = 0x02
	str	sv3, [rva, #usb_ctrl]		@ enable writing to endpoint buffer
	str	rvb, [rva, #usb_txplen]		@ set the number of bytes to be sent
	@ write data packet to send
	add	sv5, sv5, #8
usbwr1:	ldr	rvb, [rva, #usb_ctrl]
	tst	rvb, #0x02			@ is write_enable still asserted ?
	beq	usbwr2				@	if not, exit write loop
	ldr	rvb, [sv5]			@ rvb <- next data word
	str	rvb, [rva, #usb_txdata]		@ write data to Transmit register
	add	sv5, sv5, #4			@ sv5 <- updated data source address
	b	usbwr1
usbwr2:	@ send select endpoint command to protocol engine
	set	rvb, #usb_icc_empty		@ rvb <- CCEMTY bit
	orr	rvb, rvb, #usb_icd_full		@ rvb <- CCEMTY and CDFULL bits
	str	rvb, [rva, #usb_iclear_dv]
	ldr	rvb, =0x050500			@ rvb <- select physical endpoint 5 command
	str	rvb, [rva, #usb_cmd_code]	@ 
usbwr3:	ldr	rvb, [rva, #usb_istat_dv]	@ rvb <- Device Interrupt Status
	tst	rvb, #usb_icc_empty		@ has command been processed (i.e. rvb has CCEMTY) ?
	beq	usbwr3				@	if not, jump to wait for it
	@ issue validate buffer command (0xFA)
	set	rvb, #usb_icc_empty		@ rvb <- CCEMTY bit
	orr	rvb, rvb, #usb_icd_full		@ rvb <- CCEMTY and CDFULL bits
	str	rvb, [rva, #usb_iclear_dv]
	ldr	rvb, =0xFA0500			@ rvb <- validate buffer command = 0x00FA0500
	str	rvb, [rva, #usb_cmd_code]	@ 
usbwr4:	ldr	rvb, [rva, #usb_istat_dv]	@ rvb <- Device Interrupt Status
	tst	rvb, #usb_icc_empty		@ has command been processed (i.e. rvb has CCEMTY) ?
	beq	usbwr4				@	if not, jump to wait for it
	set	rvb, #usb_icc_empty		@ rvb <- CCEMTY
	str	rvb, [rva, #usb_iclear_dv]	@ clear CCEMTY bit
	b	usbwcn				@ return
	

_func_
usbhwEPSet: @ get EP into proper format for writing cmd to engine	
	@ on entry:	sv5 <- EP
	@ on exit:	rvb <- EP formated for command writing
	@ modifies:	rvb
	@ returns via:	lnk
	and	rvb, sv5, #0x0F			@ rvb <- EP logical number
	lsl	rvb, rvb, #1			@ rvb <- EP physical number (if even)
	tst	sv5, #0x80			@ is this an IN enpoint (odd) ?
	it	ne
	addne	rvb, rvb, #1			@	if so,  rvb <- EP physical index
	lsl	rvb, rvb, #16			@ rvb <- command shifted
	orr	rvb, rvb, #0x0500		@ rvb <- full command to set status of EP
	set	pc,  lnk

_func_
wrtcmd:	@ write command/data from rvb to USB protocol engine (uses sv5, rva, rvb)
	@ modifies:	sv5, rva, rvb
	ldr	rva, =usb_base
	set	sv5, #usb_icc_empty		@ sv5 <- CCEMTY bit
	orr	sv5, sv5, #usb_icd_full		@ sv5 <- CCEMTY and CDFULL bits
	str	sv5, [rva, #usb_iclear_dv]	@ Clear both CCEMTY & CDFULL bits
	str	rvb, [rva, #usb_cmd_code]	@ 
wrtcm0:	ldr	rvb, [rva, #usb_istat_dv]	@ rvb <- Device Interrupt Status
	tst	rvb, #usb_icc_empty		@ has command been processed (i.e. rvb has CCEMTY) ?
	beq	wrtcm0				@	if not, jump to wait for it
	set	rvb, #usb_icc_empty		@ rvb <- CCEMTY
	str	rvb, [rva, #usb_iclear_dv]	@ clear CCEMTY bit
	set	pc,  lnk			@ return

_func_
rdcmd:	@ read command data, cmd in rvb, result in rvb (uses sv5, rva, rvb)
	@ always follows a wrtcmd (never used alone)
	@ modifies:	sv5, rva, rvb
	ldr	rva, =usb_base
	str	rvb, [rva, #usb_cmd_code]	@ CMD_CODE -> protocol engine
rdcmd1:	ldr	rvb, [rva, #usb_istat_dv]	@ rvb <- Device Interrupt Status
	tst	rvb, #usb_icd_full		@ is command data ready (i.e. rvb has CDFULL) ?
	beq	rdcmd1				@	if not, jump to wait for it
	ldr	rvb, [rva, #usb_cmd_data]	@ rvb <- command data
	set	sv5, #usb_icd_full		@ sv5 <- CDFULL
	str	sv5, [rva, #usb_iclear_dv]	@ clear the CDFULL bit
	set	pc,  lnk			@ return

