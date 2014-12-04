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
	@ ret:	via lnk (through sd_slo)
	@ cfg CS pin = gpio out, de-sel sd card, cfg gpio_G.5,6,7 = SPI (#b11)
	rgcpbf	sd_cs_gpio, #0x00, 2, 4, 1	@ gpio_H <- sd_cs pin = gpio out
	rgcpbt	rva, #io_state, sd_cs_pin, 1	@ set sd_cs pin = de-select card
	rgrmw	sd_spi_gpio, #0x00, 0xfc00	@ gpio_G <- pins 5,6,7 = SPI
	@ jump to set speed, polarity, phase
	b	sd_slo

_func_	
sd_fst:	@ configure spi speed (high), phase, polarity
	write	3,    sd_spi, #0x0c	@ sppre <- 60MHz/2/(3+1) ~= 6.3MHz
	write	0x18, rva,    #0x00	@ spcon (ctrl) <- CLKen,master,POL=PHA=0
	set	pc,  lnk

_func_	
sd_slo:	@ configure spi speed (low), phase, polarity
	write	63,   sd_spi, #0x0c	@ sppre <- 60MHz/2/(63+1) ~= 400KHz
	write	0x18, rva,    #0x00	@ spcon (ctrl) <- CLKen,master,POL=PHA=0
	set	pc,  lnk

.endif @ sd_is_on_spi


.ltorg



