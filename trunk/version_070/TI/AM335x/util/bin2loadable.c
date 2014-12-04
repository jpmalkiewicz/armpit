/*------------------------------------------------------------------------------
@
@ bin2loadable.c            --   OMAP AM335x version
@
@-------------------------------------------------------------------------------
@
@ Distributed under The MIT License.

@  Copyright (c) 2012-2014 Hubert Montas

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
// bin2loadable.c
//
// adapted from signGP.c
//
// Read a machine code file assembled for OMAP and convert it to a
// "signed image" suitable for loading onto the MCU.
//
// The "signed image" is the original pre-pended with the size of the image
// and the load address (0x402F0400).
//
//------------------------------------------------------------------------------
//
// compile with:	gcc -o bin2loadable bin2loadable.c
//
// usage:               bin2loadable nameofbinfile.bin
//
// output:		nameofbinfile.bin.MLO
//
//------------------------------------------------------------------------------

#include <stdio.h>
#include <string.h>
#define max_fname_size	100
#define	header_size	8
#define	max_code_size	111616		// 109KB

int main(int argc, char *argv[])
{
   int	i, idx, itmp;
   char	infname[max_fname_size], outfname[max_fname_size];
   char	code[max_code_size+header_size] = {0,0,0,0,0x00,0x04,0x2F,0x40};
   char outfsuffix[] = ".MLO";
   FILE	* infile, * outfile;

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
   idx = header_size;
   while( 1 )
   {
      itmp = fgetc(infile);
      if (feof(infile)) break;
      if (idx == (max_code_size+header_size))
      {
         printf("\n\tERROR: input file is larger than %d bytes.\n\n", max_code_size);
         return(-5);
      }
      code[idx++] = itmp;
   }
   fclose(infile);
   
   // add code length to header
   itmp = idx;
   idx  = idx - header_size;
   for (i=0 ; i < 4; i++)
   {
     code[i] = idx & 0xff;
     idx = idx >> 8;
   }
   
   // Open the output file and write the header + code + padding
   strcpy(outfname, infname);
   strcat(outfname, outfsuffix);
   outfile = fopen(outfname, "wb");
   if (!outfile)
   {
      printf("\n\tERROR: cannot open output file %s.\n\n", outfname);
      return(-2);
   }
   idx = 0;
   while (idx < itmp) fputc(code[idx++], outfile);
   fclose(outfile);
   return( 0 );
}


