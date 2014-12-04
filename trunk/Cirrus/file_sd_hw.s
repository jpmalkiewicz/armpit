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
@	SD card low-level interface
@
@-----------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
@
@	SPI Interface
@
@	Code Entry Points:
@
@		sd_cfg		configure hardware
@		sd_fst		set high speed
@		sd_slo		set slow speed
@
@-----------------------------------------------------------------------------*/

.ifdef sd_is_on_spi

_func_
sd_cfg:	@ configure spi power and pins for SD card
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<- 0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- VIC1
	@ in:	dts	<- VIC2
	@ ret:	via lnk (through sd_slo)
	@ configure chip-select pin as gpio out, and de-select sd card
	set	rva, sd_cs_gpio
	and	rvb, rva, #0xff
	cmp	rvb, #0x10
	bpl	sd_cfh
	@ cfg for lo port
	rgcpbt	rva, #io_dir, sd_cs_pin, 1	@ sd CS pin set as gpio out
	b	sd_cfc
sd_cfh:	@ cfg for high port
	rgcpbt	rva, #io_dir_high, sd_cs_pin, 1 @ sd CS pin set as gpio out
sd_cfc:	@ continue
	rgcpbt	rva, #io_state, sd_cs_pin, 1	@ set sd_cs pin = de-select card
	@ jump to set speed, polarity, phase
	b	sd_slo

_func_	
sd_fst:	@ configure spi speed (high), phase, polarity
	read	rvb, sd_spi, #0x00	@ rvb <- SSPCR0
	eq	rvb, #7			@ SPI mode already configured?
	ldreq	rvb, [rva, #0x10]	@	if so,  rvb <- SSPCPSR
	eqeq	rvb, #2			@ 	if so,  speed already fast?
	ldreq	rvb, [rva, #0x04]	@	if so,  rvb <- SSPCR1
	eqeq	rvb, #0x10		@ 	if so,  SPI already enabled?
	seteq	pc,  lnk		@	if so,  return
	write	0x10, rva, #0x04	@ SSPCR1  <- enable SPI
	write	2,    rva, #0x10	@ SSPCPSR <- 1st prescaler = 2
	write	7,    rva, #0x00	@ SSPCR0  <- 7.4 MHz/2=3.7MHz,8bt,PHPO=0
	write	0x00, rva, #0x04	@ SSPCR1  <- disable SPI
	write	0x10, rva, #0x04	@ SSPCR1  <- enable SPI
	@ return
	set	pc,  lnk

_func_	
sd_slo:	@ configure spi speed (low), phase, polarity
	read	rvb, sd_spi, #0x00	@ rvb <- SSPCR0
	eq	rvb, #7			@ SPI mode already configured?
	ldreq	rvb, [rva, #0x10]	@	if so,  rvb <- SSPCPSR
	eqeq	rvb, #40		@ 	if so,  speed already fast?
	ldreq	rvb, [rva, #0x04]	@	if so,  rvb <- SSPCR1
	eqeq	rvb, #0x10		@ 	if so,  SPI already enabled?
	seteq	pc,  lnk		@	if so,  return
	write	0x10, rva, #0x04	@ SSPCR1  <- enable SPI
	write	40,   rva, #0x10	@ SSPCPSR <- 1st prescaler = 40
	write	7,    rva, #0x00	@ SSPCR0  <- 7.4MHz/40=185KHz,8bt,PHPO=0
	write	0x00, rva, #0x04	@ SSPCR1  <- disable SPI
	write	0x10, rva, #0x04	@ SSPCR1  <- enable SPI
	@ return
	set	pc,  lnk

.endif @ sd_is_on_spi


.ltorg



