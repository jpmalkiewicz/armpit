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
memcfg:	/* configure external RAM (if not done in startup.s) */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- sys_ctrl, clock selection, enabling, ...
	@ ret:	via lnk
  .ifdef LPC_H2214
	@ Samsung K6R4016V1D-TC10
	@ Note: make sure bus turnaround is compatible with flash
	write	0x0F000924,0xE002C014,#0 @ PINSEL2 <- 
					@ F=A23:1 are address lines (not A0)
					@ 9=P3.27:26->WE,CS1
					@ 2=P2.31:0-D31:0,P3.31:28-BLS0:3,
					@   P1.1:0-OE,CS0
					@ 4=P1.31:26-debug,P1.25:16-gpio not trc
	@ R/W bus wait (IDCY) set to 25ns (2x16.67ns) rather than 16x16.67ns
	@ as tDF = 25ns on Micron Flash (Chip deselected to High-Z data lines)
	@ (meanwhile, on K6R4016, tDH and tHz = 5ns, but still set to 2x16.67ns
	@ to account for bank switching)
	write	0x20000401,0xFFE00000,#4 @ BCFG1 <- bank 1, SRAM,K6R4016V1D-TC10
					@ R/W10ns, 0x81000000->0x81FFFFFF
					@ 10/b29:28->MW=32bit,000/b15:11->WST2=0
					@ (wrt=3cyc),1/b10->RBLE=1,
					@ 000/b9:5  ->WST1=0 (read=3 cycles),
					@ 0001/b3:0->IDCY=2 (R/W wait)
  .endif
  .ifdef LPC_H2294
	@ Samsung K6R4016V1D-UI10
	@ Note: make sure bus turnaround is compatible with flash
	write	0x0F000924,0xE002C014,#0 @ PINSEL2 <- 
					@ F = A23:1 are address lines (not A0)
					@ 9 = P3.27:26->WE,CS1
					@ 2 = P2.31:0->D31:0,  P3.31:28->BLS0:3,
					@     P1.1:0->OE,CS0
					@ 4 = P1.31:26->debug, P1.25:16->gpio
	@ R/W bus wait (IDCY) set to 20ns (2x16.67ns) rather than 16x16.67ns
	@ as tEHQZ = 20ns on INTEL Flash (Chip deselected to High-Z data lines)
	@ (meanwhile, on K6R4016, tDH and tHz = 5ns, but still set to 2x16.67ns
	@  for bank switching)
	write	0x20000401,0xFFE00000,#4 @ BCFG1 <- bank 1,SRAM, K6R4016V1D-UI10
					@ R/W10ns,0x81000000-0x81FFFFFF
					@ 10/b29:28->MW=32bit,000/b15:11->WST2=0
					@ (wrt=3cyc),1/b10->RBLE=1,
					@ 000/b9:5  ->WST1=0 (read=3 cyc),
					@ 0001/b3:0->IDCY=2 (R/W wait)
  .endif
  .ifdef LPC2478_STK
	@ Samsung K4S561632C-TC/L75
	@ see NXP AN10771.pdf for config
	@ also ARM infocenter PL176 for ordering of RASCAS vs SDRAM MODE setting
	set	rvc, 0x55555555
	set	rvb, 0x00fcfcc0
	bic	rvb, rvc, rvb
	write	rvb, PINSEL0,  #0x14	@ pinsel5 <- memory bus alternate func
	write	rvc, rva,      #0x18	@ pinsel6 <- memory bus alternate func
	write	rvc, rva,      #0x1c	@ pinsel7 <- memory bus alternate func
	bic	rvb, rvc, #0xC0000000
	write	rvb, rva,      #0x20	@ pinsel8 <- memory bus alternate func
	write	0x040000, rva, #0x24	@ pinsel9 <- memory bus alternate func
	lsl	rvc, rvc, #0x01
	set	rvb, 0x00fcfcc0
	bic	rvb, rvc, rvb
	write	rvb, rva,      #0x54	@ pinmode5 <- disable pull-u/d resistor
	write	rvc, rva,      #0x58	@ pinmode6 <- disable pull-u/d resistor
	write	rvc, rva,      #0x5c	@ pinmode7 <- disable pull-u/d resistor
	bic	rvb, rvc, #0xC0000000
	write	rvb, rva,      #0x60	@ pinmode8 <- disable pull-u/d resistor
	write	0x080000, rva, #0x64	@ pinmode9 <- disable pull-u/d resistor
	@ initialization of external memory (LPC2478)
	write	sv1, 0xFFE08000, #0x00	@ EMCCONTROL <- 1 -- Enable EMC
	write	sv1, rva,        #0x28	@ CONFIG <- 1, Cmd delayed w/EMCCLKDELAY
	write	sv1, rva,        #0x30	@ tRP   <- 20/13.9 + 1 = 2, prech time
	write	sv3, rva,        #0x34	@ tRAS  <- 45/13.9 + 1 = 4, row act time
	write	fre, rva,        #0x38	@ tSREX <- 1, self-rfrsh exit tim (tXSR)
	write	fre, rva,        #0x3C	@ tAPR  <- 1, last data out to active
	write	sv3, rva,        #0x40	@ tDAL  <- 1+20/13.9=3, last din to act
	write	fre, rva,        #0x44	@ tWR   <- 1, wrt rcvr tim tDPL,RWL,RDL
	write	sv4, rva,        #0x48	@ tRC   <- 65/13.9+1=5, row cycle time
	write	sv4, rva,        #0x4C	@ tRFC  <- 66/13.9+1=5, aut-rfrsh/to-act
	write	fre, rva,        #0x50	@ tXSR  <- 1, exit self-refresh to activ
	write	sv1, rva,        #0x54	@ tRRD  <- 15/13.9+1=2, Bank A to B act
	write	sv1, rva,        #0x58	@ tMRD  <- 2, load mode reg to act tRSA
	@ Initialize SDRAM chip (JEDEC)
	write	0x0183, rva, #0x20	@ CONTROL <- 0x0183,8:7<-11=NOP,run,rfrs
	wait	0x080000		@ wait a bit
	rgcpbt	rva, #0x20, 7, 0	@ CONTROL,SDRAMINI <- 2, 8:7<-10, PALL
	write	sv1, rva, #0x24		@ REFRESH <- 1 (every 16 clk in prchrg)
	wait	0x080000		@ wait a bit
	@ keep going
	write	35,     rva, #0x24	@ REFRESH <-  64ms/8192=7.81us->(78125/13.9+1)>>4
	write	0x0202, rva, #0x104	@ RASCAS <- 3,2,9:8,1:0<-10-3AHB CLK lat
	write	0x4680, rva, #0x100	@ CONFIG <- 0x0004680,13row,9col,32bit
	read	rvb,    rva, #0x20
	eor	rvb, rvb, #0x0180
	write	rvb,    rva, #0x20	@ CONTROL, SDRAMINI <- 1, 8:7<-01,MODE
	set	rvb, 0xA0044000		@ rvb <- Code for seq burst 4 wrds,CAS-2
	read	rvb,    rvb, #0x00	@ set mode into RAM mode register
	write	fre,    rva, #0x20	@ CONTROL <- 0x0000, NORMAL mode command
	rgcpbt	rva, #0x100, 19, 1	@ CONFIG, BUF_EN <- 1, bit 19, r/w bfrs
	wait	0x080000		@ wait a bit
  .endif
  .ifdef LPC4357_Xplorer
  	@ Micron MT48LC8M32B2B5-6
	@ cfg EMC address function for P1_n, P2_n and P6_n pins
	set	glv, SCU_SFSP1_n	@ glv  <- SCU_SFSP1_0
	set	cnt, SCU_SFSP2_n	@ cnt  <- SCU_SFSP2_0
	set	rva, SCU_SFSP6_n	@ rva  <- SCU_SFSP6_0
	write	0xf1, rva, #0x20	@ P6_8 <- A14/BA1 	, mode 1
	write	0xf2, glv, #0x00	@ P1_0 <- A5 		, mode 2
	write	rvb,  glv, #0x04	@ P1_1 <- A6 		, mode 2
	write	rvb,  glv, #0x08	@ P1_2 <- A7 		, mode 2
	write	rvb,  cnt, #0x00	@ P2_0 <- A13/BA0 	, mode 2
	write	rvb,  cnt, #0x08	@ P2_2 <- A11 		, mode 2
	write	rvb,  cnt, #0x18	@ P2_6 <- A10 		, mode 2
	add	cnt, cnt,  #0x1c	@ cnt  <- SCU_SFSP2_7
	set	env, 0xf3		@ env  <- mode 3
	set	rvc, 0x1c
emclp0:	subs	rvc, rvc, #4
	write	env, cnt, rvc		@ P2_7:13 <- A0:4,A8,A9	, mode 3
	bne	emclp0
	@ cfg EMC control function for P1_6, P6_n, PD_0 and PE_13 pins
	set	cnt, SCU_SFSPD_n	@ cnt   <- SCU_SFSPD_0
	set	dts, SCU_SFSPE_n	@ dts   <- SCU_SFSPE_0
	write	env, glv, #0x18		@ P1_6  <- WE		, mode 3
	write	env, rva, #0x10		@ P6_4  <- CAS		, mode 3
	write	env, rva, #0x14		@ P6_5  <- RAS		, mode 3
	write	env, rva, #0x24		@ P6_9  <- DYCS0	, mode 3
	write	env, rva, #0x28		@ P6_10 <- DQMOUT1	, mode 3
	write	env, rva, #0x2c		@ P6_11 <- CKEOUT0	, mode 3
	write	env, rva, #0x30		@ P6_12 <- DQMOUT0	, mode 3
	write	rvb, cnt, #0x00		@ PD_0  <- DQMOUT2	, mode 2
	write	env, dts, #0x34		@ PE_13 <- DQMOUT3	, mode 3
	@ cfg EMC data function on P1_n, P5_n, PD_n and PE_n pins
	add	glv, glv, #0x1c		@ glv <- SCU_SFSP1_7
	set	rva, SCU_SFSP5_n	@ rva <- SCU_SFSP5_0
	add	cnt, cnt, #0x08		@ cnt <- SCU_SFSPD_2
	add	dts, dts, #0x14		@ dts <- SCU_SFSPE_5
	set	rvc, 0x20
emclp2:	subs	rvc, rvc, #4
	write	env, glv, rvc		@ P1_07:14 <- D00:07	, mode 3
	write	rvb, rva, rvc		@ P5_00:07 <- D08:15	, mode 2
	write	rvb, cnt, rvc		@ PD_02:09 <- D16:23	, mode 2
	write	env, dts, rvc		@ PE_05:12 <- D24:31	, mode 3
	bne	emclp2
    .ifndef run_at_120mhz
	@ when M4 runs at 204 MHz, run EMC/SDRAM at 102 MHz (max is 120 MHz)
	write	0x21, 0x40051478, #0	@ CLK_M4_EMCDIV_CFG <- div 2, enab
	set	rvc,  sys_config+0x12c	@ rvc <- CREG6
	rgcpbt	rvc, #0, 16, 1		@ CREG6 <- EMC_CLK is EMC_CLK_DIV/2
	rgwfbt	rva, #0x04, 0, 1	@ wait for clock enabled
    .endif
	@ cfg CLK pins to EMC (enable input for 16-32 bit data bus)
	write	0xf5,   SCU_SFSCLKn, #0	@ CLK0 <- EMC_CLK0 and CLK1, combined
	write	0xf0,   rva, #0x08	@ CLK2 <- EMC_CLK3
	write	rvb,    rva, #0x0c	@ CLK3 <- EMC_CLK2
	write	0x7777, rva, #0x100	@ EMCDELAYCLK <- 3.5 ns
	@ configure the EMC
	write	sv1, emc_base, #0x00	@ CONTROL <- enabled, norml map/mode
	write	fre, rva,      #0x08	@ CONFIG  <- little endian (default)
	set	rvb, (1<<14)|(1<<12)|(2<<9)|(1<<7)
	add	rvc, rva, #0x0100	@ rvc <- DYNAMICCONFIG0
	write	rvb, rvc,      #0x00	@ DYNAMICCONFIG0 <- 32bit,4bnk,12ro,9col
    .ifndef run_at_120mhz
	write	0x202, rvc, #0x04	@ DYNAMICRASACS0 <- RAS=2, CAS=2
	write	sv1,   rva, #0x28	@ DYNAMICREADCONFIG <- cmd delay 1/2 CLK
	write	sv1,   rva, #0x30	@ DYNAMICRP   <- 18ns
	write	sv4,   rva, #0x34	@ DYNAMICRAS  <- 42ns
	write	6,     rva, #0x38	@ DYNAMICSREX <- 70ns
	write	rvb,   rva, #0x3c	@ DYNAMICAPR  <- 70ns
	write	sv4,   rva, #0x40	@ DYNAMICDAL  <- 4xTck (CAS 2)
	write	sv1,   rva, #0x44	@ DYNAMICWR   <- 1xTck + 6ns
	write	sv5,   rva, #0x48	@ DYNAMICRC   <- 60ns
	write	sv5,   rva, #0x4c	@ DYNAMICRFC  <- 60ns
	write	rvb,   rva, #0x50	@ DYNAMICXSR  <- 70ns
	write	sv1,   rva, #0x54	@ DYNAMICRRD  <- 12ns
	write	sv1,   rva, #0x58	@ DYNAMICMRD  <- 2xTck
    .else
	write	0x303, rvc, #0x04	@ DYNAMICRASACS0 <- RAS=3, CAS=3
	write	sv1,   rva, #0x28	@ DYNAMICREADCONFIG <- cmd delay 1/2 CLK
	write	sv2,   rva, #0x30	@ DYNAMICRP   <- 18ns
	write	sv4,   rva, #0x34	@ DYNAMICRAS  <- 42ns
	write	8,     rva, #0x38	@ DYNAMICSREX <- 70ns
	write	rvb,   rva, #0x3c	@ DYNAMICAPR  <- 70ns
	write	sv5,   rva, #0x40	@ DYNAMICDAL  <- 5xTck (CAS 3)
	write	sv1,   rva, #0x44	@ DYNAMICWR   <- 1xTck + 6ns
	write	rvb,   rva, #0x50	@ DYNAMICXSR  <- 70ns
	write	7,     rva, #0x48	@ DYNAMICRC   <- 60ns
	write	rvb,   rva, #0x4c	@ DYNAMICRFC  <- 60ns
	write	sv1,   rva, #0x54	@ DYNAMICRRD  <- 12ns
	write	sv1,   rva, #0x58	@ DYNAMICMRD  <- 2xTck
    .endif
	@ initialize SDRAM
	wait	0x4000			@ cnt <- countdown for > 100us
	write	0x183, rva, #0x20	@ DYNAMICCONTROL <- NOP command
	wait	0x8000			@ countdown for > 200us
	write	0x103, rva, #0x20	@ DYNAMICCONTROL <- PALL command
    .ifndef run_at_120mhz
	write	7,     rva, #0x24	@ DYNAMICREFRESH <- ~70ns x 16 = 1us/row
	wait	0x8000			@ countdown for > 200us
	write	99,    rva, #0x24	@ DYNAMICREFRESH <- ~64ms (15.6us/row)
	write	0x83,  rva, #0x20	@ DYNAMICCONTROL <- MODE command
	set	rvb, (0x28<<24)|(0x22<<11) @ rvb <- CAS=2, BL=4, seq burst
    .else
	write	8,     rva, #0x24	@ DYNAMICREFRESH <- ~70ns x 16 = 1us/row
	wait	0x8000			@ countdown for > 200us
	write	117,   rva, #0x24	@ DYNAMICREFRESH <- ~64ms (15.6us/row)
	write	0x83,  rva, #0x20	@ DYNAMICCONTROL <- MODE command
	set	rvb, (0x28<<24)|(0x32<<11) @ rvb <- CAS=3, BL=4, seq burst
    .endif
	read	rvb, rvb, #0x00		@ set mode into SDRAM
	write	fre, rva, #0x20		@ DYNAMICCONTROL <- NORMAL command
	rgcpbt	rvc, #0x00, 19, 1	@ DYNAMICCONFIG0 <- enable buffers
    .ifdef run_in_SDRAM
	@  copy code to SDRAM
	set	rva, 0x28000000
	set	rvc, 0
ccprlp:	@ code copy loop (32KB)
	read	rvb, rvc, #0
	write	rvb, rva, rvc
	add	rvc, rvc, #4
	eq	rvc, #0x008000
	bne	ccprlp
	@ remap SDRAM to address 0x00000000
	write	0x28000000, sys_config, #0x100	@ M4MEMMAP <- SDRAM remap to 0x0
    .endif
  .endif @ LPC4357_Xplorer
	@ return
	set	pc,  lnk


