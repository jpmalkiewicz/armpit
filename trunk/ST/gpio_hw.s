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

_func_
piocfg:	/* configure GPIO for LED(s), button(s) */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	STR7	env	<- rcc_base = RCC base adrs
	@ in:	STM32x	env	<- rcc_base = RCC base adrs
	@ in:	STM32F4	env	<- rcc_base = RCC base adrs
	@ in:	STR9	env	<- sys_ctrl = scu register base (0x5C002000)
	@ ret:	via lnk
  .ifdef STR_7xx
	@ Make ALLLED open drain outputs <- set LED bits of PC0,1,2 to 0,0,1
	mask	rvc, 0, 1, rled_pin, yled_pin, gled_pin	@ rvc <- led pin mask
	rgrmw	LEDPINSEL, #0x00, rvc, xxx	@ IOPORTn PC0 <- open drain out
	rgrmw	      rva, #0x04, rvc, xxx	@ IOPORTn PC1 <- open drain out
	rgrmw	      rva, #0x08, rvc		@ IOPORTn PC2 <- open drain out
  .endif
  .ifdef STR_9xx
	@ configure LED pin (P0.0 although no LED is on board)
	mask	rvb, 0, 1, rled_pin,yled_pin,gled_pin	@ rvb <- led pin mask
	write	rvb, env, #0x44			@ SCU_GPIOOUT0  <- gpio out func
	write	rvb, env, #0x84			@ SCU_GPIOTYPE0 <- open-collect
	write	rvb, ioport0_base, #io_dir	@ GPIO0_DIR     <- out dir
  .endif
  .ifdef STM32x
	@ initialize LED gpio pins
	@ limitation: all LED pins need to be either in CRL (< 8) or CRH (> 7)
	rgrmw	env, #24, LEDBUT_APB2P		@ RCC_APB2ENR <- enab GPIO clk
	mask	rvc, 2, 0xf, rled_pin, yled_pin, gled_pin	@ rvc <- clr-msk
	mask	cnt, 2, led_pinmode, rled_pin,yled_pin,gled_pin	@ cnt <- set-msk
	rgrmw	LEDPINSEL, #((rled_pin/8)<<2), rvc, cnt	@ GPIOn_CRL/H <- LEDmode
  .endif
  .ifdef STM32F4
	@ initialize user button and LED gpio pins
	rgrmw	env, #0x30, LEDBUT_AHB1P	@ RCC_AHB1ENR <- enab GPIO clk
	mask	rvc, 1, 0b11, rled_pin, yled_pin, gled_pin	@ rvc <- clr-msk
	mask	cnt, 1, 0b01, rled_pin, yled_pin, gled_pin	@ cnt <- set-msk
	rgrmw	LEDPINSEL, #0x00, rvc, cnt	@ GPIOn_CR   <- LED pins out dir
  .endif
	@ return
	set	pc,  lnk

	/* check if boot-override button is pressed */
  .ifdef STR_9xx
FlashInitCheck:
	write	(1<<BOOTOVERRID_BUT), sys_ctrl, #0x64	@ SCU_GPIOIN0 <- P0.3 in
	read	rvc, BOOTOVERRID_PRT, #io_set		@ rvc <- port data vals
	write	0, sys_ctrl, #0x64			@ SCU_GPIOIN0 <- disconn
	and	rva, rvc, #(1<<BOOTOVERRID_BUT)		@ rva <- stat of button
	set	pc,  lnk
  .endif



