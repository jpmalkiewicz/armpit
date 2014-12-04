/*------------------------------------------------------------------------------
@
@ lpc_add_chksum.c
@
@-------------------------------------------------------------------------------
@
@ Distributed under The MIT License.

@  Copyright (c) 2013-2014 Hubert Montas

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
//------------------------------------------------------------------------------
//
// lpc_add_chksum.c
//
// Adds a checksum in interrupt-vector[5] to indicate valid code on LPC2000
//
// useful when uploading to LPC2478 via openocd
//
//------------------------------------------------------------------------------
//
// compile with:	 gcc -o lpc_add_chksum lpc_add_chksum.c
//
// usage:	         lpc_add_chksum nameofbinfile.bin
//
// output (overwritten): nameofbinfile.bin
//
//------------------------------------------------------------------------------

#include <stdio.h>
#include <string.h>
#define max_fname_size	100
#define code_array_size	0x10000

int main(int argc, char *argv[])
{
   int	i, idx, itmp;
   char	infname[max_fname_size], outfname[max_fname_size];
   unsigned char code[code_array_size];
   FILE	* infile, * outfile;
   unsigned int * icode = (unsigned int *)code;
   unsigned int chksum;

   // clear the code array
   for (idx = 0 ; idx < code_array_size ; idx++) code[idx] = 0;

   // quick check for input arguments
   if (argc < 2)
   {
      printf("\n\t%s ERROR: no input file specified.\n\n", argv[0]);
      return(-2);
   }

   // Open the main input file (.bin) and read the code.
   strcpy(infname,  argv[1]);
   infile = fopen(infname, "rb");
   if (!infile)
   {
      printf("\n\tERROR: input file %s not found.\n\n", infname);
      return(-1);
   }   
   idx = 0;
   while( 1 )
   {
      itmp = fgetc(infile);
      if (feof(infile)) break;
      if (idx == code_array_size)
      {
         printf("\n\tERROR: input file is larger than %d bytes.\n\n", code_array_size);
         return(-5);
      }
      code[idx++] = itmp;
   }
   fclose(infile);

   // compute checksum and store it in code array
   chksum = 0;
   for (i = 0 ; i < 8 ; i++) if (i != 5) chksum += icode[i];
   icode[5] = -chksum;
   printf("checksum = 0x%x\n", icode[5]);

   // Open the output file and write the code + checksum
   strcpy(outfname, infname);
   outfile = fopen(outfname, "wb");
   if (!outfile)
   {
      printf("\n\tERROR: cannot open output file %s.\n\n", outfname);
      return(-2);
   }
   i = 0;
   while (i < idx) fputc(code[i++], outfile);
   fclose(outfile);
   return( 0 );
}


