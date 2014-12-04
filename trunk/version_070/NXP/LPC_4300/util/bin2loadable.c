/*------------------------------------------------------------------------------
@
@ bin2loadable.c            --   LPC43xx version
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
// Adds a DFU-download header to a LPC4300 bin file.
// The input file size is, at maximum, 63.5 KB (65024 bytes)
// so that the code+header will fit in at most 64 KB on
// the target (eg. SPIFI flash).
//
// The output file (.dfu) consists of header + code + padding,
// where the size of code + padding is the multiple of 512
// bytes that matches the size information in the header.
// The header also specifies that no encryption is used.
//
// This code also appends a coprocessor binary (if specified) to the end of
// the main binary, before pre-pending the header.
//
//------------------------------------------------------------------------------
//
// compile with:	gcc -o bin2loadable bin2loadable.c
//
// usage (no cpo):      bin2loadable nameofbinfile.bin
//
// usage (with cpo):    bin2loadable nameofbinfile.bin coprocessorcode.bin
//
// output:		nameofbinfile.bin.dfu
//
//------------------------------------------------------------------------------

#include <stdio.h>
#include <string.h>
#define max_fname_size	100
#define	header_size	16
#define	max_code_size	65024

int main(int argc, char *argv[])
{
   int	idx, itmp;
   char	infname[max_fname_size], cpofname[max_fname_size], outfname[max_fname_size];
   char	code[max_code_size+header_size] = {0x1a,63,127,0,0,0,0,0,0,0,0,0,255,255,255,255};
   char outfsuffix[] = ".dfu";
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
   
   // check if there is a coprocessor code file, if so, open and read it
  if (argc > 2)
  {
    strcpy(cpofname,  argv[2]);
    infile = fopen(cpofname, "rb");
    if (!infile)
    {
      printf("\n\tERROR: input file %s not found.\n\n", cpofname);
      return(-1);
    }
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
  }

   // update image size in header and clear padding area in code array
   code[2] = 1 + ((idx - 1 - header_size) >> 9);
   itmp    = 512 * code[2] + header_size;
   while (idx < itmp) code[idx++] = 0;

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


