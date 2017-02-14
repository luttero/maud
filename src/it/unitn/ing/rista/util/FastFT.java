/*
 * @(#)FastFT.java created 9/09/2001 Casalino
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with the author.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.rista.util;

/**
 * The FastFT is a class to perform fast fourier transform
 *
 * @version $Revision: 1.3 $, $Date: 2006/01/19 14:45:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class FastFT {

  public FastFT() {
  }

/* FFT library
 * Library of in-place fast fourier transforms
 * Forward and inverse complex transforms
 * and real forward transform
 * Optimized for RISC processors with many registers
 * Version 1.1 John Green NUWC New London CT	January 96
 * Version 1.1 renamed as fftlib from fftbig
 * Version 1.1 removed (float *)((char *) ptr) optimization
 * Version 1.0  John Green NUWC New London CT	December 95
 * (John Green) green_jt@vsdec.nl.nuwc.navy.mil
 * green_jt@vsdec.nl.nuwc.navy.mil
 */

  final static int MAXMROOT = 9;	// 2^(MAXMROOT-1) = # of elements in BRcnt

// Bit reversed counter
  final static char[] BRcnt = {
    0, 128, 64, 192, 32, 160, 96, 224, 16, 144, 80, 208,
    48, 176, 112, 240, 8, 136, 72, 200, 40, 168, 104, 232,
    24, 152, 88, 216, 56, 184, 120, 248, 4, 132, 68, 196,
    36, 164, 100, 228, 20, 148, 84, 212, 52, 180, 116, 244,
    12, 140, 76, 204, 44, 172, 108, 236, 28, 156, 92, 220,
    60, 188, 124, 252, 2, 130, 66, 194, 34, 162, 98, 226,
    18, 146, 82, 210, 50, 178, 114, 242, 10, 138, 74, 202,
    42, 170, 106, 234, 26, 154, 90, 218, 58, 186, 122, 250,
    6, 134, 70, 198, 38, 166, 102, 230, 22, 150, 86, 214,
    54, 182, 118, 246, 14, 142, 78, 206, 46, 174, 110, 238,
    30, 158, 94, 222, 62, 190, 126, 254, 1, 129, 65, 193,
    33, 161, 97, 225, 17, 145, 81, 209, 49, 177, 113, 241,
    9, 137, 73, 201, 41, 169, 105, 233, 25, 153, 89, 217,
    57, 185, 121, 249, 5, 133, 69, 197, 37, 165, 101, 229,
    21, 149, 85, 213, 53, 181, 117, 245, 13, 141, 77, 205,
    45, 173, 109, 237, 29, 157, 93, 221, 61, 189, 125, 253,
    3, 131, 67, 195, 35, 163, 99, 227, 19, 147, 83, 211,
    51, 179, 115, 243, 11, 139, 75, 203, 43, 171, 107, 235,
    27, 155, 91, 219, 59, 187, 123, 251, 7, 135, 71, 199,
    39, 167, 103, 231, 23, 151, 87, 215, 55, 183, 119, 247,
    15, 143, 79, 207, 47, 175, 111, 239, 31, 159, 95, 223,
    63, 191, 127, 255};

  // Table of powers of 2
  final static int[] Ntbl = {1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048,
                             4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288, 1048576};

/* Compute cosine table and check size for complex ffts
 * INPUTS
 * fftN = size of fft
 * OUTPUTS
 * fftMptr = log2 of fft size
 * Utbl = cosine table with fftN/4 + 1 entries (angles = 0 to pi/2 inclusive)
 * RETURNS
 * 1 if fftN is invalid, 0 otherwise
 */
  public boolean FFTInit(int[] fftMptr, int fftN, double[] Utbl) {
    boolean ErrVal = true;
    fftMptr[0] = (int) (Math.log(fftN) / Math.log(2.0) + 0.5);
    if ((fftMptr[0] >= 3) & (fftMptr[0] <= 19) & (int) (Math.pow(2, fftMptr[0]) + .5) == fftN)
      for (int i1 = 0; i1 <= fftN / 4; i1++)
        Utbl[i1] = Math.cos((Math.PI * 2.0 * i1) / fftN);
    else
      ErrVal = false;
    return ErrVal;
  }

/* Compute cosine table and check size for a real input fft
 * INPUTS
 * fftN = size of fft
 * OUTPUTS
 * fftMptr = log2 of fft size
 * Utbl = cosine table with fftN/4 + 1 entries (angles = 0 to pi/2 inclusive)
 * RETURNS
 * 1 if fftN is invalid, 0 otherwise
 */
  public boolean rFFTInit(int[] fftMptr, int fftN, double[] Utbl) {
    boolean ErrVal = true;
    fftMptr[0] = (int) (Math.log(fftN) / Math.log(2.0) + 0.5);
    if ((fftMptr[0] >= 4) & (fftMptr[0] <= 20) & (int) (Math.pow(2, fftMptr[0]) + .5) == fftN)
      for (int i1 = 0; i1 <= fftN / 4; i1++)
        Utbl[i1] = Math.cos((Math.PI * 2.0 * i1) / fftN);
    else
      ErrVal = false;
    return ErrVal;
  }


/* Compute complex fft on the rows of the input array
 * INPUTS
 * M[0] = log2 of fft size
 * ioptr = input data array
 * Utbl = cosine table
 * Rows = number of rows in ioptr array (use Rows of 1 if ioptr is a 1 dimensional array)
 * OUTPUTS
 * ioptr = output data array
 */

  public void ffts(double[] ioptr, int[] M, int Rows, double[] Utbl) {

    int Flyinc, FlyOffsetA, FlyOffsetAIm, FlyOffsetB, FlyOffsetBIm,
            NsameU1, NsameU2, NsameU4, diffUcnt, LoopCnt;

    double scale, fly0r, fly0i, fly1r, fly1i, fly2r, fly2i, fly3r, fly3i,
            fly4r, fly4i, fly5r, fly5i, fly6r, fly6i, fly7r, fly7i,
            U0r = 0.0, U0i = 0.0, U1r = 0.0, U1i = 0.0, U2r = 0.0, U2i = 0.0,
            U3r = 0.0, U3i = 0.0, t0r, t0i, t1r, t1i;

    int fly0P, fly1P, fly2P, fly3P,
            U0rP = 0, U0iP = 0, U1rP = 0, U1iP = 0, U2rP = 0, U2iP = 0, IOP = 0;
    int U3offset = 0, stage, NdiffU, LoopN;

    final int BRshift = MAXMROOT - (M[0] >> 1);
    final int Nrems2 = Ntbl[(M[0] - (M[0] >> 1) + 1)];
    final int Nroot_1_ColInc = (Ntbl[M[0] - 1] - Ntbl[M[0] - (M[0] >> 1)]) * 2;

    scale = 2.0;
    int ioptrIndex = 0;

    for (; Rows > 0; Rows--) {

      FlyOffsetA = Ntbl[M[0]] * (2 / 2);
      FlyOffsetAIm = FlyOffsetA + 1;
      FlyOffsetB = FlyOffsetA + 2;
      FlyOffsetBIm = FlyOffsetB + 1;

// BitrevR2 ** bit reverse and first radix 2 stage

      for (stage = 0; stage < Ntbl[M[0] - (M[0] >> 1)] * 2; stage += Ntbl[M[0] >> 1] * 2) {
        for (LoopN = (Ntbl[(M[0] >> 1) - 1] - 1); LoopN >= 0; LoopN--) {
          LoopCnt = (Ntbl[(M[0] >> 1) - 1] - 1);
          fly0P = ioptrIndex + Nroot_1_ColInc + ((int) BRcnt[LoopN] >> BRshift) * (2 * 2) + stage;
          IOP = ioptrIndex + (LoopN << (M[0] + 1) / 2) * 2 + stage;
          fly1P = IOP + ((int) BRcnt[LoopCnt] >> BRshift) * (2 * 2);
          fly0r = ioptr[fly0P];
          fly0i = ioptr[fly0P + 1];
          fly1r = ioptr[fly0P + FlyOffsetA];
          fly1i = ioptr[fly0P + FlyOffsetAIm];
          for (; LoopCnt > LoopN;) {
            fly2r = ioptr[fly0P + 2];
            fly2i = ioptr[fly0P + (2 + 1)];
            fly3r = ioptr[fly0P + FlyOffsetB];
            fly3i = ioptr[fly0P + FlyOffsetBIm];
            fly4r = ioptr[fly1P];
            fly4i = ioptr[fly1P + 1];
            fly5r = ioptr[fly1P + FlyOffsetA];
            fly5i = ioptr[fly1P + FlyOffsetAIm];
            fly6r = ioptr[fly1P + 2];
            fly6i = ioptr[fly1P + (2 + 1)];
            fly7r = ioptr[fly1P + FlyOffsetB];
            fly7i = ioptr[fly1P + FlyOffsetBIm];

            t0r = fly0r + fly1r;
            t0i = fly0i + fly1i;
            fly1r = fly0r - fly1r;
            fly1i = fly0i - fly1i;
            t1r = fly2r + fly3r;
            t1i = fly2i + fly3i;
            fly3r = fly2r - fly3r;
            fly3i = fly2i - fly3i;
            fly0r = fly4r + fly5r;
            fly0i = fly4i + fly5i;
            fly5r = fly4r - fly5r;
            fly5i = fly4i - fly5i;
            fly2r = fly6r + fly7r;
            fly2i = fly6i + fly7i;
            fly7r = fly6r - fly7r;
            fly7i = fly6i - fly7i;

            ioptr[fly1P] = t0r;       // why scale is missing here?
            ioptr[fly1P + 1] = t0i;
            ioptr[fly1P + 2] = fly1r;
            ioptr[fly1P + (2 + 1)] = fly1i;
            ioptr[fly1P + FlyOffsetA] = t1r;
            ioptr[fly1P + FlyOffsetAIm] = t1i;
            ioptr[fly1P + FlyOffsetB] = fly3r;
            ioptr[fly1P + FlyOffsetBIm] = fly3i;
            ioptr[fly0P] = fly0r;
            ioptr[fly0P + 1] = fly0i;
            ioptr[fly0P + 2] = fly5r;
            ioptr[fly0P + (2 + 1)] = fly5i;
            ioptr[fly0P + FlyOffsetA] = fly2r;
            ioptr[fly0P + FlyOffsetAIm] = fly2i;
            ioptr[fly0P + FlyOffsetB] = fly7r;
            ioptr[fly0P + FlyOffsetBIm] = fly7i;

            fly0P -= Nrems2;
            fly0r = ioptr[fly0P];
            fly0i = ioptr[fly0P + 1];
            fly1r = ioptr[fly0P + FlyOffsetA];
            fly1i = ioptr[fly0P + FlyOffsetAIm];
            LoopCnt -= 1;
            fly1P = (IOP + ((int) BRcnt[LoopCnt] >> BRshift) * (2 * 2));
          }
          fly2r = ioptr[fly0P + 2];
          fly2i = ioptr[fly0P + (2 + 1)];
          fly3r = ioptr[fly0P + FlyOffsetB];
          fly3i = ioptr[fly0P + FlyOffsetBIm];

          t0r = fly0r + fly1r;
          t0i = fly0i + fly1i;
          fly1r = fly0r - fly1r;
          fly1i = fly0i - fly1i;
          t1r = fly2r + fly3r;
          t1i = fly2i + fly3i;
          fly3r = fly2r - fly3r;
          fly3i = fly2i - fly3i;

          ioptr[fly0P] = t0r;
          ioptr[fly0P + 1] = t0i;
          ioptr[fly0P + 2] = fly1r;
          ioptr[fly0P + 3] = fly1i;
          ioptr[fly0P + FlyOffsetA] = t1r;
          ioptr[fly0P + FlyOffsetAIm] = t1i;
          ioptr[fly0P + FlyOffsetB] = fly3r;
          ioptr[fly0P + FlyOffsetBIm] = fly3i;

        }
      }



// **** FFTC  **************

      NdiffU = 2;
      Flyinc = NdiffU;

      NsameU4 = Ntbl[M[0]] / 4;
      LoopN = Ntbl[M[0] - 3];

      stage = ((M[0] - 1) / 3);
      if ((M[0] - 1 - (stage * 3)) != 0) {
        FlyOffsetA = Flyinc << 2;
        FlyOffsetB = FlyOffsetA << 1;
        FlyOffsetAIm = FlyOffsetA + 1;
        FlyOffsetBIm = FlyOffsetB + 1;
        if ((M[0] - 1 - (stage * 3)) == 1) {
          // 1 radix 2 stage

          IOP = ioptrIndex;
          fly0P = IOP;
          fly1P = (IOP + Flyinc);
          fly2P = (fly1P + Flyinc);
          fly3P = (fly2P + Flyinc);

          /* Butterflys		*/
          /*
          t0	-	-	t0
          t1	-	-	t1
          f2	-  1-	f5
          f1	- -i-	f7
          f4	-	-	f4
          f0	-	-	f0
          f6	-  1-	f2
          f3	- -i-	f1
          */

          for (LoopCnt = LoopN; LoopCnt > 0; LoopCnt--) {
            t0r = ioptr[fly0P];
            t0i = ioptr[fly0P + 1];
            t1r = ioptr[fly1P];
            t1i = ioptr[fly1P + 1];
            fly2r = ioptr[fly2P];
            fly2i = ioptr[fly2P + 1];
            fly1r = ioptr[fly3P];
            fly1i = ioptr[fly3P + 1];
            fly4r = ioptr[fly0P + FlyOffsetA];
            fly4i = ioptr[fly0P + FlyOffsetAIm];
            fly0r = ioptr[fly1P + FlyOffsetA];
            fly0i = ioptr[fly1P + FlyOffsetAIm];
            fly6r = ioptr[fly2P + FlyOffsetA];
            fly6i = ioptr[fly2P + FlyOffsetAIm];
            fly3r = ioptr[fly3P + FlyOffsetA];
            fly3i = ioptr[fly3P + FlyOffsetAIm];

            fly5r = t0r - fly2r;
            fly5i = t0i - fly2i;
            t0r = t0r + fly2r;
            t0i = t0i + fly2i;

            fly7r = t1r - fly1i;
            fly7i = t1i + fly1r;
            t1r = t1r + fly1i;
            t1i = t1i - fly1r;

            fly2r = fly4r - fly6r;
            fly2i = fly4i - fly6i;
            fly4r = fly4r + fly6r;
            fly4i = fly4i + fly6i;

            fly1r = fly0r - fly3i;
            fly1i = fly0i + fly3r;
            fly0r = fly0r + fly3i;
            fly0i = fly0i - fly3r;

            ioptr[fly2P] = fly5r;
            ioptr[fly2P + 1] = fly5i;
            ioptr[fly0P] = t0r;
            ioptr[fly0P + 1] = t0i;
            ioptr[fly3P] = fly7r;
            ioptr[fly3P + 1] = fly7i;
            ioptr[fly1P] = t1r;
            ioptr[fly1P + 1] = t1i;
            ioptr[fly2P + FlyOffsetA] = fly2r;
            ioptr[fly2P + FlyOffsetAIm] = fly2i;
            ioptr[fly0P + FlyOffsetA] = fly4r;
            ioptr[fly0P + FlyOffsetAIm] = fly4i;
            ioptr[fly3P + FlyOffsetA] = fly1r;
            ioptr[fly3P + FlyOffsetAIm] = fly1i;
            ioptr[fly1P + FlyOffsetA] = fly0r;
            ioptr[fly1P + FlyOffsetAIm] = fly0i;

            fly0P = (fly0P + FlyOffsetB);
            fly1P = (fly1P + FlyOffsetB);
            fly2P = (fly2P + FlyOffsetB);
            fly3P = (fly3P + FlyOffsetB);
          }

          NsameU4 >>= 1;
          LoopN >>= 1;
          NdiffU <<= 1;
          Flyinc = Flyinc << 1;
        } else {
          // 1 radix 4 stage
          IOP = ioptrIndex;

          U3r = 0.7071067811865475244008443621; /* sqrt(0.5);	*/
          U3i = U3r;
          fly0P = IOP;
          fly1P = (IOP + Flyinc);
          fly2P = (fly1P + Flyinc);
          fly3P = (fly2P + Flyinc);

          /* Butterflys		*/
          /*
          t0	-	-	t0	-	-	t0
          t1	-	-	t1	-	-	t1
          f2	-  1-	f5	-	-	f5
          f1	- -i-	f7	-	-	f7
          f4	-	-	f4	-  1-	f6
          f0	-	-	f0	-U3	-	f3
          f6	-  1-	f2	- -i-	f4
          f3	- -i-	f1	-U3a-	f2
          */

          for (LoopCnt = LoopN; LoopCnt > 0; LoopCnt--) {
            t0r = ioptr[fly0P];
            t0i = ioptr[fly0P + 1];
            t1r = ioptr[fly1P];
            t1i = ioptr[fly1P + 1];
            fly2r = ioptr[fly2P];
            fly2i = ioptr[fly2P + 1];
            fly1r = ioptr[fly3P];
            fly1i = ioptr[fly3P + 1];
            fly4r = ioptr[fly0P + FlyOffsetA];
            fly4i = ioptr[fly0P + FlyOffsetAIm];
            fly0r = ioptr[fly1P + FlyOffsetA];
            fly0i = ioptr[fly1P + FlyOffsetAIm];
            fly6r = ioptr[fly2P + FlyOffsetA];
            fly6i = ioptr[fly2P + FlyOffsetAIm];
            fly3r = ioptr[fly3P + FlyOffsetA];
            fly3i = ioptr[fly3P + FlyOffsetAIm];

            fly5r = t0r - fly2r;
            fly5i = t0i - fly2i;
            t0r = t0r + fly2r;
            t0i = t0i + fly2i;

            fly7r = t1r - fly1i;
            fly7i = t1i + fly1r;
            t1r = t1r + fly1i;
            t1i = t1i - fly1r;

            fly2r = fly4r - fly6r;
            fly2i = fly4i - fly6i;
            fly4r = fly4r + fly6r;
            fly4i = fly4i + fly6i;

            fly1r = fly0r - fly3i;
            fly1i = fly0i + fly3r;
            fly0r = fly0r + fly3i;
            fly0i = fly0i - fly3r;


            fly6r = t0r - fly4r;
            fly6i = t0i - fly4i;
            t0r = t0r + fly4r;
            t0i = t0i + fly4i;

            fly3r = fly5r - fly2i;
            fly3i = fly5i + fly2r;
            fly5r = fly5r + fly2i;
            fly5i = fly5i - fly2r;

            fly4r = t1r - U3r * fly0r;
            fly4r = fly4r - U3i * fly0i;
            fly4i = t1i + U3i * fly0r;
            fly4i = fly4i - U3r * fly0i;
            t1r = scale * t1r - fly4r;
            t1i = scale * t1i - fly4i;

            fly2r = fly7r + U3i * fly1r;
            fly2r = fly2r - U3r * fly1i;
            fly2i = fly7i + U3r * fly1r;
            fly2i = fly2i + U3i * fly1i;
            fly7r = scale * fly7r - fly2r;
            fly7i = scale * fly7i - fly2i;

            ioptr[fly0P + FlyOffsetA] = fly6r;
            ioptr[fly0P + FlyOffsetAIm] = fly6i;
            ioptr[fly0P] = t0r;
            ioptr[fly0P + 1] = t0i;
            ioptr[fly2P + FlyOffsetA] = fly3r;
            ioptr[fly2P + FlyOffsetAIm] = fly3i;
            ioptr[fly2P] = fly5r;
            ioptr[fly2P + 1] = fly5i;
            ioptr[fly1P + FlyOffsetA] = fly4r;
            ioptr[fly1P + FlyOffsetAIm] = fly4i;
            ioptr[fly1P] = t1r;
            ioptr[fly1P + 1] = t1i;
            ioptr[fly3P + FlyOffsetA] = fly2r;
            ioptr[fly3P + FlyOffsetAIm] = fly2i;
            ioptr[fly3P] = fly7r;
            ioptr[fly3P + 1] = fly7i;

            fly0P = (fly0P + FlyOffsetB);
            fly1P = (fly1P + FlyOffsetB);
            fly2P = (fly2P + FlyOffsetB);
            fly3P = (fly3P + FlyOffsetB);

          }

          NsameU4 >>= 2;
          LoopN >>= 2;
          NdiffU <<= 2;
          Flyinc = Flyinc << 2;
        }
      }

      NsameU2 = NsameU4 >> 1;
      NsameU1 = NsameU2 >> 1;
      Flyinc <<= 1;
      FlyOffsetA = Flyinc << 2;
      FlyOffsetB = FlyOffsetA << 1;
      FlyOffsetAIm = FlyOffsetA + 1;
      FlyOffsetBIm = FlyOffsetB + 1;
      LoopN >>= 1;
      /*   ****** RADIX 8 Stages	*/
      for (stage = stage << 1; stage > 0; stage--) {

        /* an fft stage is done in two parts to ease use of the single quadrant cos table	*/

        /*	fftcalc1(iobuf, Utbl, N, NdiffU, LoopN);	*/
        if (!(MoreMath.odd(stage))) {
          U0rP = 0;
          U0iP = Ntbl[M[0] - 2];
          U1rP = U0rP;
          U1iP = U0iP;
          U2rP = U0rP;
          U2iP = U0iP;
          U3offset = (Ntbl[M[0]]) / 8;

          IOP = ioptrIndex;

          U0r = Utbl[U0rP];
          U0i = Utbl[U0iP];
          U1r = Utbl[U1rP];
          U1i = Utbl[U1iP];
          U2r = Utbl[U2rP];
          U2i = Utbl[U2iP];
          U3r = Utbl[U2rP + U3offset];
          U3i = Utbl[U2iP - U3offset];
        }

        fly0P = IOP;
        fly1P = (IOP + Flyinc);
        fly2P = (fly1P + Flyinc);
        fly3P = (fly2P + Flyinc);

        for (diffUcnt = (NdiffU) >> 1; diffUcnt != 0; diffUcnt--) {

          /* Butterflys		*/
          /*
          f0	-	-	t0	-	-	t0	-	-	t0
          f1	-U0	-	t1	-	-	t1	-	-	t1
          f2	-	-	f2	-U1	-	f5	-	-	f3
          f3	-U0	-	f1	-U1a-	f7	-	-	f7
          f4	-	-	f4	-	-	f4	-U2	-	f6
          f5	-U0	-	f0	-	-	f0	-U3	-	f4
          f6	-	-	f6	-U1	-	f2	-U2a-	f2
          f7	-U0	-	f3	-U1a-	f1	-U3a-	f5
          */

          fly0r = ioptr[IOP];
          fly0i = ioptr[IOP + 1];
          fly1r = ioptr[fly1P];
          fly1i = ioptr[fly1P + 1];

          for (LoopCnt = LoopN - 1; LoopCnt > 0; LoopCnt--) {

            fly2r = ioptr[fly2P];
            fly2i = ioptr[fly2P + 1];
            fly3r = ioptr[fly3P];
            fly3i = ioptr[fly3P + 1];
            fly4r = ioptr[fly0P + FlyOffsetA];
            fly4i = ioptr[fly0P + FlyOffsetAIm];
            fly5r = ioptr[fly1P + FlyOffsetA];
            fly5i = ioptr[fly1P + FlyOffsetAIm];
            fly6r = ioptr[fly2P + FlyOffsetA];
            fly6i = ioptr[fly2P + FlyOffsetAIm];
            fly7r = ioptr[fly3P + FlyOffsetA];
            fly7i = ioptr[fly3P + FlyOffsetAIm];

            t1r = fly0r - U0r * fly1r;
            t1r = t1r - U0i * fly1i;
            t1i = fly0i + U0i * fly1r;
            t1i = t1i - U0r * fly1i;
            t0r = scale * fly0r - t1r;
            t0i = scale * fly0i - t1i;

            fly1r = fly2r - U0r * fly3r;
            fly1r = fly1r - U0i * fly3i;
            fly1i = fly2i + U0i * fly3r;
            fly1i = fly1i - U0r * fly3i;
            fly2r = scale * fly2r - fly1r;
            fly2i = scale * fly2i - fly1i;

            fly0r = fly4r - U0r * fly5r;
            fly0r = fly0r - U0i * fly5i;
            fly0i = fly4i + U0i * fly5r;
            fly0i = fly0i - U0r * fly5i;
            fly4r = scale * fly4r - fly0r;
            fly4i = scale * fly4i - fly0i;

            fly3r = fly6r - U0r * fly7r;
            fly3r = fly3r - U0i * fly7i;
            fly3i = fly6i + U0i * fly7r;
            fly3i = fly3i - U0r * fly7i;
            fly6r = scale * fly6r - fly3r;
            fly6i = scale * fly6i - fly3i;


            fly5r = t0r - U1r * fly2r;
            fly5r = fly5r - U1i * fly2i;
            fly5i = t0i + U1i * fly2r;
            fly5i = fly5i - U1r * fly2i;
            t0r = scale * t0r - fly5r;
            t0i = scale * t0i - fly5i;

            fly7r = t1r + U1i * fly1r;
            fly7r = fly7r - U1r * fly1i;
            fly7i = t1i + U1r * fly1r;
            fly7i = fly7i + U1i * fly1i;
            t1r = scale * t1r - fly7r;
            t1i = scale * t1i - fly7i;

            fly2r = fly4r - U1r * fly6r;
            fly2r = fly2r - U1i * fly6i;
            fly2i = fly4i + U1i * fly6r;
            fly2i = fly2i - U1r * fly6i;
            fly4r = scale * fly4r - fly2r;
            fly4i = scale * fly4i - fly2i;

            fly1r = fly0r + U1i * fly3r;
            fly1r = fly1r - U1r * fly3i;
            fly1i = fly0i + U1r * fly3r;
            fly1i = fly1i + U1i * fly3i;
            fly0r = scale * fly0r - fly1r;
            fly0i = scale * fly0i - fly1i;

            fly6r = t0r - U2r * fly4r;
            fly6r = fly6r - U2i * fly4i;
            fly6i = t0i + U2i * fly4r;
            fly6i = fly6i - U2r * fly4i;
            t0r = scale * t0r - fly6r;
            t0i = scale * t0i - fly6i;

            fly3r = fly5r - U2i * fly2r;
            fly3r = fly3r + U2r * fly2i;
            fly3i = fly5i - U2r * fly2r;
            fly3i = fly3i - U2i * fly2i;
            fly2r = scale * fly5r - fly3r;
            fly2i = scale * fly5i - fly3i;

            fly4r = t1r - U3r * fly0r;
            fly4r = fly4r - U3i * fly0i;
            fly4i = t1i + U3i * fly0r;
            fly4i = fly4i - U3r * fly0i;
            t1r = scale * t1r - fly4r;
            t1i = scale * t1i - fly4i;

            fly5r = fly7r + U3i * fly1r;
            fly5r = fly5r - U3r * fly1i;
            fly5i = fly7i + U3r * fly1r;
            fly5i = fly5i + U3i * fly1i;
            fly7r = scale * fly7r - fly5r;
            fly7i = scale * fly7i - fly5i;

            ioptr[fly0P + FlyOffsetA] = fly6r;
            ioptr[fly0P + FlyOffsetAIm] = fly6i;
            ioptr[fly0P] = t0r;
            ioptr[fly0P + 1] = t0i;
            ioptr[fly2P] = fly3r;
            ioptr[fly2P + 1] = fly3i;
            ioptr[fly2P + FlyOffsetA] = fly2r;
            ioptr[fly2P + FlyOffsetAIm] = fly2i;

            fly0r = ioptr[fly0P + FlyOffsetB];
            fly0i = ioptr[fly0P + FlyOffsetBIm];

            ioptr[fly1P + FlyOffsetA] = fly4r;
            ioptr[fly1P + FlyOffsetAIm] = fly4i;
            ioptr[fly1P] = t1r;
            ioptr[fly1P + 1] = t1i;

            fly1r = ioptr[fly1P + FlyOffsetB];
            fly1i = ioptr[fly1P + FlyOffsetBIm];

            ioptr[fly3P + FlyOffsetA] = fly5r;
            ioptr[fly3P + FlyOffsetAIm] = fly5i;
            ioptr[fly3P] = fly7r;
            ioptr[fly3P + 1] = fly7i;

            fly0P = (fly0P + FlyOffsetB);
            fly1P = (fly1P + FlyOffsetB);
            fly2P = (fly2P + FlyOffsetB);
            fly3P = (fly3P + FlyOffsetB);
          }
          fly2r = ioptr[fly2P];
          fly2i = ioptr[fly2P + 1];
          fly3r = ioptr[fly3P];
          fly3i = ioptr[fly3P + 1];
          fly4r = ioptr[fly0P + FlyOffsetA];
          fly4i = ioptr[fly0P + FlyOffsetAIm];
          fly5r = ioptr[fly1P + FlyOffsetA];
          fly5i = ioptr[fly1P + FlyOffsetAIm];
          fly6r = ioptr[fly2P + FlyOffsetA];
          fly6i = ioptr[fly2P + FlyOffsetAIm];
          fly7r = ioptr[fly3P + FlyOffsetA];
          fly7i = ioptr[fly3P + FlyOffsetAIm];

          t1r = fly0r - U0r * fly1r;
          t1r = t1r - U0i * fly1i;
          t1i = fly0i + U0i * fly1r;
          t1i = t1i - U0r * fly1i;
          t0r = scale * fly0r - t1r;
          t0i = scale * fly0i - t1i;

          fly1r = fly2r - U0r * fly3r;
          fly1r = fly1r - U0i * fly3i;
          fly1i = fly2i + U0i * fly3r;
          fly1i = fly1i - U0r * fly3i;
          fly2r = scale * fly2r - fly1r;
          fly2i = scale * fly2i - fly1i;

          fly0r = fly4r - U0r * fly5r;
          fly0r = fly0r - U0i * fly5i;
          fly0i = fly4i + U0i * fly5r;
          fly0i = fly0i - U0r * fly5i;
          fly4r = scale * fly4r - fly0r;
          fly4i = scale * fly4i - fly0i;

          fly3r = fly6r - U0r * fly7r;
          fly3r = fly3r - U0i * fly7i;
          fly3i = fly6i + U0i * fly7r;
          fly3i = fly3i - U0r * fly7i;
          fly6r = scale * fly6r - fly3r;
          fly6i = scale * fly6i - fly3i;

          fly5r = t0r - U1r * fly2r;
          fly5r = fly5r - U1i * fly2i;
          fly5i = t0i + U1i * fly2r;
          fly5i = fly5i - U1r * fly2i;
          t0r = scale * t0r - fly5r;
          t0i = scale * t0i - fly5i;

          fly7r = t1r + U1i * fly1r;
          fly7r = fly7r - U1r * fly1i;
          fly7i = t1i + U1r * fly1r;
          fly7i = fly7i + U1i * fly1i;
          t1r = scale * t1r - fly7r;
          t1i = scale * t1i - fly7i;

          fly2r = fly4r - U1r * fly6r;
          fly2r = fly2r - U1i * fly6i;
          fly2i = fly4i + U1i * fly6r;
          fly2i = fly2i - U1r * fly6i;
          fly4r = scale * fly4r - fly2r;
          fly4i = scale * fly4i - fly2i;

          fly1r = fly0r + U1i * fly3r;
          fly1r = fly1r - U1r * fly3i;
          fly1i = fly0i + U1r * fly3r;
          fly1i = fly1i + U1i * fly3i;
          fly0r = scale * fly0r - fly1r;
          fly0i = scale * fly0i - fly1i;

          U0i = Utbl[U0iP = (U0iP - NsameU4)];
          U0r = Utbl[U0rP = (U0rP + NsameU4)];
          U1r = Utbl[U1rP = (U1rP + NsameU2)];
          U1i = Utbl[U1iP = (U1iP - NsameU2)];
          if (MoreMath.odd(stage))
            U0r = -U0r;

          fly6r = t0r - U2r * fly4r;
          fly6r = fly6r - U2i * fly4i;
          fly6i = t0i + U2i * fly4r;
          fly6i = fly6i - U2r * fly4i;
          t0r = scale * t0r - fly6r;
          t0i = scale * t0i - fly6i;

          fly3r = fly5r - U2i * fly2r;
          fly3r = fly3r + U2r * fly2i;
          fly3i = fly5i - U2r * fly2r;
          fly3i = fly3i - U2i * fly2i;
          fly2r = scale * fly5r - fly3r;
          fly2i = scale * fly5i - fly3i;

          fly4r = t1r - U3r * fly0r;
          fly4r = fly4r - U3i * fly0i;
          fly4i = t1i + U3i * fly0r;
          fly4i = fly4i - U3r * fly0i;
          t1r = scale * t1r - fly4r;
          t1i = scale * t1i - fly4i;

          fly5r = fly7r + U3i * fly1r;
          fly5r = fly5r - U3r * fly1i;
          fly5i = fly7i + U3r * fly1r;
          fly5i = fly5i + U3i * fly1i;
          fly7r = scale * fly7r - fly5r;
          fly7i = scale * fly7i - fly5i;

          ioptr[fly0P + FlyOffsetA] = fly6r;
          ioptr[fly0P + FlyOffsetAIm] = fly6i;
          ioptr[fly0P] = t0r;
          ioptr[fly0P + 1] = t0i;

          U2r = Utbl[U2rP = (U2rP + NsameU1)];
          U2i = Utbl[U2iP = (U2iP - NsameU1)];

          ioptr[fly2P] = fly3r;
          ioptr[fly2P + 1] = fly3i;
          ioptr[fly2P + FlyOffsetA] = fly2r;
          ioptr[fly2P + FlyOffsetAIm] = fly2i;
          ioptr[fly1P + FlyOffsetA] = fly4r;
          ioptr[fly1P + FlyOffsetAIm] = fly4i;
          ioptr[fly1P] = t1r;
          ioptr[fly1P + 1] = t1i;

          U3r = Utbl[U2rP + U3offset];
          U3i = Utbl[U2iP - U3offset];

          ioptr[fly3P + FlyOffsetA] = fly5r;
          ioptr[fly3P + FlyOffsetAIm] = fly5i;
          ioptr[fly3P] = fly7r;
          ioptr[fly3P + 1] = fly7i;

          IOP = IOP + 2;
          fly0P = IOP;
          fly1P = (IOP + Flyinc);
          fly2P = (fly1P + Flyinc);
          fly3P = (fly2P + Flyinc);
        }
        NsameU4 = -NsameU4;

        if (MoreMath.odd(stage)) {
          LoopN >>= 3;
          NsameU1 >>= 3;
          NsameU2 >>= 3;
          NsameU4 >>= 3;
          NdiffU <<= 3;
          Flyinc <<= 3;
          FlyOffsetA <<= 3;
          FlyOffsetB <<= 3;
          FlyOffsetAIm = FlyOffsetA + 1;
          FlyOffsetBIm = FlyOffsetB + 1;
        }
      }
      ioptrIndex += 2 * Ntbl[M[0]];
    }
  }

/* Compute inverse complex fft on the rows of the input array
 * INPUTS
 * M[0] = log2 of fft size
 * ioptr = input data array
 * Utbl = cosine table
 * Rows = number of rows in ioptr array (use Rows of 1 if ioptr is a 1 dimensional array)
 * OUTPUTS
 * ioptr = output data array
 */
  public void iffts(double[] ioptr, int[] M, int Rows, double[] Utbl) {

    int Flyinc, FlyOffsetA, FlyOffsetAIm, FlyOffsetB, FlyOffsetBIm,
            NsameU1, NsameU2, NsameU4, diffUcnt, LoopCnt;

    double scale, fly0r, fly0i, fly1r, fly1i, fly2r, fly2i, fly3r, fly3i,
            fly4r, fly4i, fly5r, fly5i, fly6r, fly6i, fly7r, fly7i,
            U0r = 0.0, U0i = 0.0, U1r = 0.0, U1i = 0.0, U2r = 0.0, U2i = 0.0,
            U3r = 0.0, U3i = 0.0, t0r, t0i, t1r, t1i;

    int fly0P, fly1P, fly2P, fly3P,
            U0rP = 0, U0iP = 0, U1rP = 0, U1iP = 0, U2rP = 0, U2iP = 0, IOP = 0;
    int U3offset = 0, stage, NdiffU, LoopN;

    final int BRshift = MAXMROOT - (M[0] >> 1);
    final int Nrems2 = Ntbl[M[0] - (M[0] >> 1) + 1];
    final int Nroot_1_ColInc = (Ntbl[M[0] - 1] - Ntbl[M[0] - (M[0] >> 1)]) * 2;

    int ioptrIndex = 0;

    for (; Rows > 0; Rows--) {

      FlyOffsetA = Ntbl[M[0]] * 2 / 2;
      FlyOffsetAIm = FlyOffsetA + 1;
      FlyOffsetB = FlyOffsetA + 2;
      FlyOffsetBIm = FlyOffsetB + 1;

// BitrevR2 ** bit reverse and first radix 2 stage

      scale = 1. / Ntbl[M[0]];
      for (stage = 0; stage < Ntbl[M[0] - (M[0] >> 1)] * 2; stage += Ntbl[M[0] >> 1] * 2) {
        for (LoopN = (Ntbl[(M[0] >> 1) - 1] - 1); LoopN >= 0; LoopN--) {
          LoopCnt = (Ntbl[(M[0] >> 1) - 1] - 1);
          fly0P = ioptrIndex + Nroot_1_ColInc + ((int) BRcnt[LoopN] >> BRshift) * (2 * 2) + stage;
          IOP = ioptrIndex + (LoopN << (M[0] + 1) / 2) * 2 + stage;
          fly1P = IOP + ((int) BRcnt[LoopCnt] >> BRshift) * (2 * 2);
          fly0r = ioptr[fly0P];
          fly0i = ioptr[fly0P + 1];
          fly1r = ioptr[fly0P + FlyOffsetA];
          fly1i = ioptr[fly0P + FlyOffsetAIm];
          for (; LoopCnt > LoopN;) {
            fly2r = ioptr[fly0P + 2];
            fly2i = ioptr[fly0P + (2 + 1)];
            fly3r = ioptr[fly0P + FlyOffsetB];
            fly3i = ioptr[fly0P + FlyOffsetBIm];
            fly4r = ioptr[fly1P];
            fly4i = ioptr[fly1P + 1];
            fly5r = ioptr[fly1P + FlyOffsetA];
            fly5i = ioptr[fly1P + FlyOffsetAIm];
            fly6r = ioptr[fly1P + 2];
            fly6i = ioptr[fly1P + (2 + 1)];
            fly7r = ioptr[fly1P + FlyOffsetB];
            fly7i = ioptr[fly1P + FlyOffsetBIm];

            t0r = fly0r + fly1r;
            t0i = fly0i + fly1i;
            fly1r = fly0r - fly1r;
            fly1i = fly0i - fly1i;
            t1r = fly2r + fly3r;
            t1i = fly2i + fly3i;
            fly3r = fly2r - fly3r;
            fly3i = fly2i - fly3i;
            fly0r = fly4r + fly5r;
            fly0i = fly4i + fly5i;
            fly5r = fly4r - fly5r;
            fly5i = fly4i - fly5i;
            fly2r = fly6r + fly7r;
            fly2i = fly6i + fly7i;
            fly7r = fly6r - fly7r;
            fly7i = fly6i - fly7i;

            ioptr[fly1P] = scale * t0r;
            ioptr[fly1P + 1] = scale * t0i;
            ioptr[fly1P + 2] = scale * fly1r;
            ioptr[fly1P + (2 + 1)] = scale * fly1i;
            ioptr[fly1P + FlyOffsetA] = scale * t1r;
            ioptr[fly1P + FlyOffsetAIm] = scale * t1i;
            ioptr[fly1P + FlyOffsetB] = scale * fly3r;
            ioptr[fly1P + FlyOffsetBIm] = scale * fly3i;
            ioptr[fly0P] = scale * fly0r;
            ioptr[fly0P + 1] = scale * fly0i;
            ioptr[fly0P + 2] = scale * fly5r;
            ioptr[fly0P + (2 + 1)] = scale * fly5i;
            ioptr[fly0P + FlyOffsetA] = scale * fly2r;
            ioptr[fly0P + FlyOffsetAIm] = scale * fly2i;
            ioptr[fly0P + FlyOffsetB] = scale * fly7r;
            ioptr[fly0P + FlyOffsetBIm] = scale * fly7i;

            fly0P -= Nrems2;
            fly0r = ioptr[fly0P];
            fly0i = ioptr[fly0P + 1];
            fly1r = ioptr[fly0P + FlyOffsetA];
            fly1i = ioptr[fly0P + FlyOffsetAIm];
            LoopCnt -= 1;
            fly1P = (IOP + ((int) BRcnt[LoopCnt] >> BRshift) * (2 * 2));
          }
          fly2r = ioptr[fly0P + 2];
          fly2i = ioptr[fly0P + (2 + 1)];
          fly3r = ioptr[fly0P + FlyOffsetB];
          fly3i = ioptr[fly0P + FlyOffsetBIm];

          t0r = fly0r + fly1r;
          t0i = fly0i + fly1i;
          fly1r = fly0r - fly1r;
          fly1i = fly0i - fly1i;
          t1r = fly2r + fly3r;
          t1i = fly2i + fly3i;
          fly3r = fly2r - fly3r;
          fly3i = fly2i - fly3i;

          ioptr[fly0P] = scale * t0r;
          ioptr[fly0P + 1] = scale * t0i;
          ioptr[fly0P + 2] = scale * fly1r;
          ioptr[fly0P + (2 + 1)] = scale * fly1i;
          ioptr[fly0P + FlyOffsetA] = scale * t1r;
          ioptr[fly0P + FlyOffsetAIm] = scale * t1i;
          ioptr[fly0P + FlyOffsetB] = scale * fly3r;
          ioptr[fly0P + FlyOffsetBIm] = scale * fly3i;

        }
      }

      /**** FFTC  **************/

      scale = 2.0;

      NdiffU = 2;
      Flyinc = (NdiffU);

      NsameU4 = Ntbl[M[0]] / 4;
      LoopN = Ntbl[M[0] - 3];

      stage = ((M[0] - 1) / 3);
      if ((M[0] - 1 - (stage * 3)) != 0) {
        FlyOffsetA = Flyinc << 2;
        FlyOffsetB = FlyOffsetA << 1;
        FlyOffsetAIm = FlyOffsetA + 1;
        FlyOffsetBIm = FlyOffsetB + 1;
        if ((M[0] - 1 - (stage * 3)) == 1) {
          /* 1 radix 2 stage */

          IOP = ioptrIndex;
          fly0P = IOP;
          fly1P = (IOP + Flyinc);
          fly2P = (fly1P + Flyinc);
          fly3P = (fly2P + Flyinc);

          /* Butterflys		*/
          /*
          t0	-	-	t0
          t1	-	-	t1
          f2	-  1-	f5
          f1	- -i-	f7
          f4	-	-	f4
          f0	-	-	f0
          f6	-  1-	f2
          f3	- -i-	f1
          */

          for (LoopCnt = LoopN; LoopCnt > 0; LoopCnt--) {
            t0r = ioptr[fly0P];
            t0i = ioptr[fly0P + 1];
            t1r = ioptr[fly1P];
            t1i = ioptr[fly1P + 1];
            fly2r = ioptr[fly2P];
            fly2i = ioptr[fly2P + 1];
            fly1r = ioptr[fly3P];
            fly1i = ioptr[fly3P + 1];
            fly4r = ioptr[fly0P + FlyOffsetA];
            fly4i = ioptr[fly0P + FlyOffsetAIm];
            fly0r = ioptr[fly1P + FlyOffsetA];
            fly0i = ioptr[fly1P + FlyOffsetAIm];
            fly6r = ioptr[fly2P + FlyOffsetA];
            fly6i = ioptr[fly2P + FlyOffsetAIm];
            fly3r = ioptr[fly3P + FlyOffsetA];
            fly3i = ioptr[fly3P + FlyOffsetAIm];

            fly5r = t0r - fly2r;
            fly5i = t0i - fly2i;
            t0r = t0r + fly2r;
            t0i = t0i + fly2i;

            fly7r = t1r + fly1i;
            fly7i = t1i - fly1r;
            t1r = t1r - fly1i;
            t1i = t1i + fly1r;

            fly2r = fly4r - fly6r;
            fly2i = fly4i - fly6i;
            fly4r = fly4r + fly6r;
            fly4i = fly4i + fly6i;

            fly1r = fly0r + fly3i;
            fly1i = fly0i - fly3r;
            fly0r = fly0r - fly3i;
            fly0i = fly0i + fly3r;

            ioptr[fly2P] = fly5r;
            ioptr[fly2P + 1] = fly5i;
            ioptr[fly0P] = t0r;
            ioptr[fly0P + 1] = t0i;
            ioptr[fly3P] = fly7r;
            ioptr[fly3P + 1] = fly7i;
            ioptr[fly1P] = t1r;
            ioptr[fly1P + 1] = t1i;
            ioptr[fly2P + FlyOffsetA] = fly2r;
            ioptr[fly2P + FlyOffsetAIm] = fly2i;
            ioptr[fly0P + FlyOffsetA] = fly4r;
            ioptr[fly0P + FlyOffsetAIm] = fly4i;
            ioptr[fly3P + FlyOffsetA] = fly1r;
            ioptr[fly3P + FlyOffsetAIm] = fly1i;
            ioptr[fly1P + FlyOffsetA] = fly0r;
            ioptr[fly1P + FlyOffsetAIm] = fly0i;

            fly0P = (fly0P + FlyOffsetB);
            fly1P = (fly1P + FlyOffsetB);
            fly2P = (fly2P + FlyOffsetB);
            fly3P = (fly3P + FlyOffsetB);
          }

          NsameU4 >>= 1;
          LoopN >>= 1;
          NdiffU <<= 1;
          Flyinc = Flyinc << 1;
        } else {
          /* 1 radix 4 stage */
          IOP = ioptrIndex;

          U3r = 0.7071067811865475244008443621; /* sqrt(0.5);	*/
          U3i = U3r;
          fly0P = IOP;
          fly1P = (IOP + Flyinc);
          fly2P = (fly1P + Flyinc);
          fly3P = (fly2P + Flyinc);

          /* Butterflys		*/
          /*
          t0	-	-	t0	-	-	t0
          t1	-	-	t1	-	-	t1
          f2	-  1-	f5	-	-	f5
          f1	- -i-	f7	-	-	f7
          f4	-	-	f4	-  1-	f6
          f0	-	-	f0	-U3	-	f3
          f6	-  1-	f2	- -i-	f4
          f3	- -i-	f1	-U3a-	f2
          */

          for (LoopCnt = LoopN; LoopCnt > 0; LoopCnt--) {
            t0r = ioptr[fly0P];
            t0i = ioptr[fly0P + 1];
            t1r = ioptr[fly1P];
            t1i = ioptr[fly1P + 1];
            fly2r = ioptr[fly2P];
            fly2i = ioptr[fly2P + 1];
            fly1r = ioptr[fly3P];
            fly1i = ioptr[fly3P + 1];
            fly4r = ioptr[fly0P + FlyOffsetA];
            fly4i = ioptr[fly0P + FlyOffsetAIm];
            fly0r = ioptr[fly1P + FlyOffsetA];
            fly0i = ioptr[fly1P + FlyOffsetAIm];
            fly6r = ioptr[fly2P + FlyOffsetA];
            fly6i = ioptr[fly2P + FlyOffsetAIm];
            fly3r = ioptr[fly3P + FlyOffsetA];
            fly3i = ioptr[fly3P + FlyOffsetAIm];

            fly5r = t0r - fly2r;
            fly5i = t0i - fly2i;
            t0r = t0r + fly2r;
            t0i = t0i + fly2i;

            fly7r = t1r + fly1i;
            fly7i = t1i - fly1r;
            t1r = t1r - fly1i;
            t1i = t1i + fly1r;

            fly2r = fly4r - fly6r;
            fly2i = fly4i - fly6i;
            fly4r = fly4r + fly6r;
            fly4i = fly4i + fly6i;

            fly1r = fly0r + fly3i;
            fly1i = fly0i - fly3r;
            fly0r = fly0r - fly3i;
            fly0i = fly0i + fly3r;

            fly6r = t0r - fly4r;
            fly6i = t0i - fly4i;
            t0r = t0r + fly4r;
            t0i = t0i + fly4i;

            fly3r = fly5r + fly2i;
            fly3i = fly5i - fly2r;
            fly5r = fly5r - fly2i;
            fly5i = fly5i + fly2r;

            fly4r = t1r - U3r * fly0r;
            fly4r = fly4r + U3i * fly0i;
            fly4i = t1i - U3i * fly0r;
            fly4i = fly4i - U3r * fly0i;
            t1r = scale * t1r - fly4r;
            t1i = scale * t1i - fly4i;

            fly2r = fly7r + U3i * fly1r;
            fly2r = fly2r + U3r * fly1i;
            fly2i = fly7i - U3r * fly1r;
            fly2i = fly2i + U3i * fly1i;
            fly7r = scale * fly7r - fly2r;
            fly7i = scale * fly7i - fly2i;

            ioptr[fly0P + FlyOffsetA] = fly6r;
            ioptr[fly0P + FlyOffsetAIm] = fly6i;
            ioptr[fly0P] = t0r;
            ioptr[fly0P + 1] = t0i;
            ioptr[fly2P + FlyOffsetA] = fly3r;
            ioptr[fly2P + FlyOffsetAIm] = fly3i;
            ioptr[fly2P] = fly5r;
            ioptr[fly2P + 1] = fly5i;
            ioptr[fly1P + FlyOffsetA] = fly4r;
            ioptr[fly1P + FlyOffsetAIm] = fly4i;
            ioptr[fly1P] = t1r;
            ioptr[fly1P + 1] = t1i;
            ioptr[fly3P + FlyOffsetA] = fly2r;
            ioptr[fly3P + FlyOffsetAIm] = fly2i;
            ioptr[fly3P] = fly7r;
            ioptr[fly3P + 1] = fly7i;

            fly0P = (fly0P + FlyOffsetB);
            fly1P = (fly1P + FlyOffsetB);
            fly2P = (fly2P + FlyOffsetB);
            fly3P = (fly3P + FlyOffsetB);

          }

          NsameU4 >>= 2;
          LoopN >>= 2;
          NdiffU <<= 2;
          Flyinc = Flyinc << 2;
        }
      }

      NsameU2 = NsameU4 >> 1;
      NsameU1 = NsameU2 >> 1;
      Flyinc <<= 1;
      FlyOffsetA = Flyinc << 2;
      FlyOffsetB = FlyOffsetA << 1;
      FlyOffsetAIm = FlyOffsetA + 1;
      FlyOffsetBIm = FlyOffsetB + 1;
      LoopN >>= 1;

      /*   ****** RADIX 8 Stages	*/
      for (stage = stage << 1; stage > 0; stage--) {

        /* an fft stage is done in two parts to ease use of the single quadrant cos table	*/

        /*	fftcalc1(iobuf, Utbl, N, NdiffU, LoopN);	*/
        if (!(MoreMath.odd(stage))) {
          U0rP = 0;
          U0iP = Ntbl[M[0] - 2];
          U1rP = U0rP;
          U1iP = U0iP;
          U2rP = U0rP;
          U2iP = U0iP;
          U3offset = (Ntbl[M[0]]) / 8;

          IOP = ioptrIndex;

          U0r = Utbl[U0rP];
          U0i = Utbl[U0iP];
          U1r = Utbl[U1rP];
          U1i = Utbl[U1iP];
          U2r = Utbl[U2rP];
          U2i = Utbl[U2iP];
          U3r = Utbl[U2rP + U3offset];
          U3i = Utbl[U2iP - U3offset];
        }

        fly0P = IOP;
        fly1P = (IOP + Flyinc);
        fly2P = (fly1P + Flyinc);
        fly3P = (fly2P + Flyinc);

        for (diffUcnt = (NdiffU) >> 1; diffUcnt > 0; diffUcnt--) {

          /* Butterflys		*/
          /*
          f0	-	-	t0	-	-	t0	-	-	t0
          f1	-U0	-	t1	-	-	t1	-	-	t1
          f2	-	-	f2	-U1	-	f5	-	-	f3
          f3	-U0	-	f1	-U1a-	f7	-	-	f7
          f4	-	-	f4	-	-	f4	-U2	-	f6
          f5	-U0	-	f0	-	-	f0	-U3	-	f4
          f6	-	-	f6	-U1	-	f2	-U2a-	f2
          f7	-U0	-	f3	-U1a-	f1	-U3a-	f5
          */

          fly0r = ioptr[IOP];
          fly0i = ioptr[IOP + 1];
          fly1r = ioptr[fly1P];
          fly1i = ioptr[fly1P + 1];

          for (LoopCnt = LoopN - 1; LoopCnt > 0; LoopCnt--) {

            fly2r = ioptr[fly2P];
            fly2i = ioptr[fly2P + 1];
            fly3r = ioptr[fly3P];
            fly3i = ioptr[fly3P + 1];
            fly4r = ioptr[fly0P + FlyOffsetA];
            fly4i = ioptr[fly0P + FlyOffsetAIm];
            fly5r = ioptr[fly1P + FlyOffsetA];
            fly5i = ioptr[fly1P + FlyOffsetAIm];
            fly6r = ioptr[fly2P + FlyOffsetA];
            fly6i = ioptr[fly2P + FlyOffsetAIm];
            fly7r = ioptr[fly3P + FlyOffsetA];
            fly7i = ioptr[fly3P + FlyOffsetAIm];

            t1r = fly0r - U0r * fly1r;
            t1r = t1r + U0i * fly1i;
            t1i = fly0i - U0i * fly1r;
            t1i = t1i - U0r * fly1i;
            t0r = scale * fly0r - t1r;
            t0i = scale * fly0i - t1i;

            fly1r = fly2r - U0r * fly3r;
            fly1r = fly1r + U0i * fly3i;
            fly1i = fly2i - U0i * fly3r;
            fly1i = fly1i - U0r * fly3i;
            fly2r = scale * fly2r - fly1r;
            fly2i = scale * fly2i - fly1i;

            fly0r = fly4r - U0r * fly5r;
            fly0r = fly0r + U0i * fly5i;
            fly0i = fly4i - U0i * fly5r;
            fly0i = fly0i - U0r * fly5i;
            fly4r = scale * fly4r - fly0r;
            fly4i = scale * fly4i - fly0i;

            fly3r = fly6r - U0r * fly7r;
            fly3r = fly3r + U0i * fly7i;
            fly3i = fly6i - U0i * fly7r;
            fly3i = fly3i - U0r * fly7i;
            fly6r = scale * fly6r - fly3r;
            fly6i = scale * fly6i - fly3i;


            fly5r = t0r - U1r * fly2r;
            fly5r = fly5r + U1i * fly2i;
            fly5i = t0i - U1i * fly2r;
            fly5i = fly5i - U1r * fly2i;
            t0r = scale * t0r - fly5r;
            t0i = scale * t0i - fly5i;

            fly7r = t1r + U1i * fly1r;
            fly7r = fly7r + U1r * fly1i;
            fly7i = t1i - U1r * fly1r;
            fly7i = fly7i + U1i * fly1i;
            t1r = scale * t1r - fly7r;
            t1i = scale * t1i - fly7i;

            fly2r = fly4r - U1r * fly6r;
            fly2r = fly2r + U1i * fly6i;
            fly2i = fly4i - U1i * fly6r;
            fly2i = fly2i - U1r * fly6i;
            fly4r = scale * fly4r - fly2r;
            fly4i = scale * fly4i - fly2i;

            fly1r = fly0r + U1i * fly3r;
            fly1r = fly1r + U1r * fly3i;
            fly1i = fly0i - U1r * fly3r;
            fly1i = fly1i + U1i * fly3i;
            fly0r = scale * fly0r - fly1r;
            fly0i = scale * fly0i - fly1i;

            fly6r = t0r - U2r * fly4r;
            fly6r = fly6r + U2i * fly4i;
            fly6i = t0i - U2i * fly4r;
            fly6i = fly6i - U2r * fly4i;
            t0r = scale * t0r - fly6r;
            t0i = scale * t0i - fly6i;

            fly3r = fly5r - U2i * fly2r;
            fly3r = fly3r - U2r * fly2i;
            fly3i = fly5i + U2r * fly2r;
            fly3i = fly3i - U2i * fly2i;
            fly2r = scale * fly5r - fly3r;
            fly2i = scale * fly5i - fly3i;

            fly4r = t1r - U3r * fly0r;
            fly4r = fly4r + U3i * fly0i;
            fly4i = t1i - U3i * fly0r;
            fly4i = fly4i - U3r * fly0i;
            t1r = scale * t1r - fly4r;
            t1i = scale * t1i - fly4i;

            fly5r = fly7r + U3i * fly1r;
            fly5r = fly5r + U3r * fly1i;
            fly5i = fly7i - U3r * fly1r;
            fly5i = fly5i + U3i * fly1i;
            fly7r = scale * fly7r - fly5r;
            fly7i = scale * fly7i - fly5i;

            ioptr[fly0P + FlyOffsetA] = fly6r;
            ioptr[fly0P + FlyOffsetAIm] = fly6i;
            ioptr[fly0P] = t0r;
            ioptr[fly0P + 1] = t0i;
            ioptr[fly2P] = fly3r;
            ioptr[fly2P + 1] = fly3i;
            ioptr[fly2P + FlyOffsetA] = fly2r;
            ioptr[fly2P + FlyOffsetAIm] = fly2i;

            fly0r = ioptr[fly0P + FlyOffsetB];
            fly0i = ioptr[fly0P + FlyOffsetBIm];

            ioptr[fly1P + FlyOffsetA] = fly4r;
            ioptr[fly1P + FlyOffsetAIm] = fly4i;
            ioptr[fly1P] = t1r;
            ioptr[fly1P + 1] = t1i;

            fly1r = ioptr[fly1P + FlyOffsetB];
            fly1i = ioptr[fly1P + FlyOffsetBIm];

            ioptr[fly3P + FlyOffsetA] = fly5r;
            ioptr[fly3P + FlyOffsetAIm] = fly5i;
            ioptr[fly3P] = fly7r;
            ioptr[fly3P + 1] = fly7i;

            fly0P = (fly0P + FlyOffsetB);
            fly1P = (fly1P + FlyOffsetB);
            fly2P = (fly2P + FlyOffsetB);
            fly3P = (fly3P + FlyOffsetB);

          }
          fly2r = ioptr[fly2P];
          fly2i = ioptr[fly2P + 1];
          fly3r = ioptr[fly3P];
          fly3i = ioptr[fly3P + 1];
          fly4r = ioptr[fly0P + FlyOffsetA];
          fly4i = ioptr[fly0P + FlyOffsetAIm];
          fly5r = ioptr[fly1P + FlyOffsetA];
          fly5i = ioptr[fly1P + FlyOffsetAIm];
          fly6r = ioptr[fly2P + FlyOffsetA];
          fly6i = ioptr[fly2P + FlyOffsetAIm];
          fly7r = ioptr[fly3P + FlyOffsetA];
          fly7i = ioptr[fly3P + FlyOffsetAIm];

          t1r = fly0r - U0r * fly1r;
          t1r = t1r + U0i * fly1i;
          t1i = fly0i - U0i * fly1r;
          t1i = t1i - U0r * fly1i;
          t0r = scale * fly0r - t1r;
          t0i = scale * fly0i - t1i;

          fly1r = fly2r - U0r * fly3r;
          fly1r = fly1r + U0i * fly3i;
          fly1i = fly2i - U0i * fly3r;
          fly1i = fly1i - U0r * fly3i;
          fly2r = scale * fly2r - fly1r;
          fly2i = scale * fly2i - fly1i;

          fly0r = fly4r - U0r * fly5r;
          fly0r = fly0r + U0i * fly5i;
          fly0i = fly4i - U0i * fly5r;
          fly0i = fly0i - U0r * fly5i;
          fly4r = scale * fly4r - fly0r;
          fly4i = scale * fly4i - fly0i;

          fly3r = fly6r - U0r * fly7r;
          fly3r = fly3r + U0i * fly7i;
          fly3i = fly6i - U0i * fly7r;
          fly3i = fly3i - U0r * fly7i;
          fly6r = scale * fly6r - fly3r;
          fly6i = scale * fly6i - fly3i;

          fly5r = t0r - U1r * fly2r;
          fly5r = fly5r + U1i * fly2i;
          fly5i = t0i - U1i * fly2r;
          fly5i = fly5i - U1r * fly2i;
          t0r = scale * t0r - fly5r;
          t0i = scale * t0i - fly5i;

          fly7r = t1r + U1i * fly1r;
          fly7r = fly7r + U1r * fly1i;
          fly7i = t1i - U1r * fly1r;
          fly7i = fly7i + U1i * fly1i;
          t1r = scale * t1r - fly7r;
          t1i = scale * t1i - fly7i;

          fly2r = fly4r - U1r * fly6r;
          fly2r = fly2r + U1i * fly6i;
          fly2i = fly4i - U1i * fly6r;
          fly2i = fly2i - U1r * fly6i;
          fly4r = scale * fly4r - fly2r;
          fly4i = scale * fly4i - fly2i;

          fly1r = fly0r + U1i * fly3r;
          fly1r = fly1r + U1r * fly3i;
          fly1i = fly0i - U1r * fly3r;
          fly1i = fly1i + U1i * fly3i;
          fly0r = scale * fly0r - fly1r;
          fly0i = scale * fly0i - fly1i;

          U0i = Utbl[U0iP = (U0iP - NsameU4)];
          U0r = Utbl[U0rP = (U0rP + NsameU4)];
          U1r = Utbl[U1rP = (U1rP + NsameU2)];
          U1i = Utbl[U1iP = (U1iP - NsameU2)];
          if (MoreMath.odd(stage))
            U0r = -U0r;

          fly6r = t0r - U2r * fly4r;
          fly6r = fly6r + U2i * fly4i;
          fly6i = t0i - U2i * fly4r;
          fly6i = fly6i - U2r * fly4i;
          t0r = scale * t0r - fly6r;
          t0i = scale * t0i - fly6i;

          fly3r = fly5r - U2i * fly2r;
          fly3r = fly3r - U2r * fly2i;
          fly3i = fly5i + U2r * fly2r;
          fly3i = fly3i - U2i * fly2i;
          fly2r = scale * fly5r - fly3r;
          fly2i = scale * fly5i - fly3i;

          fly4r = t1r - U3r * fly0r;
          fly4r = fly4r + U3i * fly0i;
          fly4i = t1i - U3i * fly0r;
          fly4i = fly4i - U3r * fly0i;
          t1r = scale * t1r - fly4r;
          t1i = scale * t1i - fly4i;

          fly5r = fly7r + U3i * fly1r;
          fly5r = fly5r + U3r * fly1i;
          fly5i = fly7i - U3r * fly1r;
          fly5i = fly5i + U3i * fly1i;
          fly7r = scale * fly7r - fly5r;
          fly7i = scale * fly7i - fly5i;

          ioptr[fly0P + FlyOffsetA] = fly6r;
          ioptr[fly0P + FlyOffsetAIm] = fly6i;
          ioptr[fly0P] = t0r;
          ioptr[fly0P + 1] = t0i;

          U2r = Utbl[U2rP = (U2rP + NsameU1)];
          U2i = Utbl[U2iP = (U2iP - NsameU1)];

          ioptr[fly2P] = fly3r;
          ioptr[fly2P + 1] = fly3i;
          ioptr[fly2P + FlyOffsetA] = fly2r;
          ioptr[fly2P + FlyOffsetAIm] = fly2i;
          ioptr[fly1P + FlyOffsetA] = fly4r;
          ioptr[fly1P + FlyOffsetAIm] = fly4i;
          ioptr[fly1P] = t1r;
          ioptr[fly1P + 1] = t1i;

          U3r = Utbl[U2rP + U3offset];
          U3i = Utbl[U2iP - U3offset];

          ioptr[fly3P + FlyOffsetA] = fly5r;
          ioptr[fly3P + FlyOffsetAIm] = fly5i;
          ioptr[fly3P] = fly7r;
          ioptr[fly3P + 1] = fly7i;

          IOP = IOP + 2;
          fly0P = IOP;
          fly1P = (IOP + Flyinc);
          fly2P = (fly1P + Flyinc);
          fly3P = (fly2P + Flyinc);
        }
        NsameU4 = -NsameU4;

        if (MoreMath.odd(stage)) {
          LoopN >>= 3;
          NsameU1 >>= 3;
          NsameU2 >>= 3;
          NsameU4 >>= 3;
          NdiffU <<= 3;
          Flyinc = Flyinc << 3;
          FlyOffsetA <<= 3;
          FlyOffsetB <<= 3;
          FlyOffsetAIm = FlyOffsetA + 1;
          FlyOffsetBIm = FlyOffsetB + 1;
        }
      }
      ioptrIndex += 2 * Ntbl[M[0]];
    }
  }

  /* rffts
   * compute multiple real input FFTs on 'Rows' consecutively stored arrays
   * ioptr = pointer to the data in and out
   * M[0] = log2 of fft size
   * Rows = number of FFTs to compute (use Rows of 1 if ioptr is a 1 dimensional array)
   * Utbl = Pointer to cosine table
   */

  public void rffts(double[] ioptr, int[] M, int Rows, double[] Utbl) {

    int Flyinc, FlyOffsetA, FlyOffsetAIm, FlyOffsetB, FlyOffsetBIm,
            NsameU1, NsameU2, NsameU4, diffUcnt, LoopCnt;
    double scale, fly0r, fly0i, fly1r, fly1i, fly2r, fly2i, fly3r, fly3i,
            fly4r, fly4i, fly5r, fly5i, fly6r, fly6i, fly7r, fly7i,
            U0r = 0.0, U0i = 0.0, U1r = 0.0, U1i = 0.0, U2r = 0.0, U2i = 0.0, U3r = 0.0, U3i = 0.0,
            t0r, t0i, t1r, t1i;

    int fly0P, fly1P, fly2P, fly3P,
            U0rP = 0, U0iP = 0, U1rP = 0, U1iP = 0, U2rP = 0, U2iP = 0, IOP = 0;
    int U3offset = 0;

    int stage;
    int NdiffU;
    int LoopN;

    int ioptrIndex = 0;

    final int BRshift = MAXMROOT - ((M[0] - 1) >> 1);		/* for RFFT */
    final int Nrems2 = Ntbl[(M[0] - 1) - ((M[0] - 1) >> 1) + 1];		/* for RFFT */
    final int Nroot_1_ColInc = (Ntbl[(M[0] - 1) - 1] - Ntbl[(M[0] - 1) - ((M[0] - 1) >> 1)]) * 2; /* for RFFT */

    for (; Rows > 0; Rows--) {

      M[0] = M[0] - 1;		/* for RFFT */

      FlyOffsetA = Ntbl[M[0]] * 2 / 2;
      FlyOffsetAIm = FlyOffsetA + 1;
      FlyOffsetB = FlyOffsetA + 2;
      FlyOffsetBIm = FlyOffsetB + 1;

      /* BitrevR2 ** bit reverse shuffle and first radix 2 stage ******/

      scale = 0.5;
      for (stage = 0; stage < Ntbl[M[0] - (M[0] >> 1)] * 2; stage += Ntbl[M[0] >> 1] * 2) {
        for (LoopN = (Ntbl[(M[0] >> 1) - 1] - 1); LoopN >= 0; LoopN--) {
          LoopCnt = (Ntbl[(M[0] >> 1) - 1] - 1);
          fly0P = ioptrIndex + Nroot_1_ColInc + ((int) BRcnt[LoopN] >> BRshift) * (2 * 2) + stage;
          IOP = ioptrIndex + (LoopN << (M[0] + 1) / 2) * 2 + stage;
          fly1P = IOP + ((int) BRcnt[LoopCnt] >> BRshift) * (2 * 2);
          fly0r = ioptr[fly0P];
          fly0i = ioptr[fly0P + 1];
          fly1r = ioptr[fly0P + FlyOffsetA];
          fly1i = ioptr[fly0P + FlyOffsetAIm];
          for (; LoopCnt > LoopN;) {
            fly2r = ioptr[fly0P + 2];
            fly2i = ioptr[fly0P + (2 + 1)];
            fly3r = ioptr[fly0P + FlyOffsetB];
            fly3i = ioptr[fly0P + FlyOffsetBIm];
            fly4r = ioptr[fly1P];
            fly4i = ioptr[fly1P + 1];
            fly5r = ioptr[fly1P + FlyOffsetA];
            fly5i = ioptr[fly1P + FlyOffsetAIm];
            fly6r = ioptr[fly1P + 2];
            fly6i = ioptr[fly1P + (2 + 1)];
            fly7r = ioptr[fly1P + FlyOffsetB];
            fly7i = ioptr[fly1P + FlyOffsetBIm];

            t0r = fly0r + fly1r;
            t0i = fly0i + fly1i;
            fly1r = fly0r - fly1r;
            fly1i = fly0i - fly1i;
            t1r = fly2r + fly3r;
            t1i = fly2i + fly3i;
            fly3r = fly2r - fly3r;
            fly3i = fly2i - fly3i;
            fly0r = fly4r + fly5r;
            fly0i = fly4i + fly5i;
            fly5r = fly4r - fly5r;
            fly5i = fly4i - fly5i;
            fly2r = fly6r + fly7r;
            fly2i = fly6i + fly7i;
            fly7r = fly6r - fly7r;
            fly7i = fly6i - fly7i;

            ioptr[fly1P] = scale * t0r;
            ioptr[fly1P + 1] = scale * t0i;
            ioptr[fly1P + 2] = scale * fly1r;
            ioptr[fly1P + (2 + 1)] = scale * fly1i;
            ioptr[fly1P + FlyOffsetA] = scale * t1r;
            ioptr[fly1P + FlyOffsetAIm] = scale * t1i;
            ioptr[fly1P + FlyOffsetB] = scale * fly3r;
            ioptr[fly1P + FlyOffsetBIm] = scale * fly3i;
            ioptr[fly0P] = scale * fly0r;
            ioptr[fly0P + 1] = scale * fly0i;
            ioptr[fly0P + 2] = scale * fly5r;
            ioptr[fly0P + (2 + 1)] = scale * fly5i;
            ioptr[fly0P + FlyOffsetA] = scale * fly2r;
            ioptr[fly0P + FlyOffsetAIm] = scale * fly2i;
            ioptr[fly0P + FlyOffsetB] = scale * fly7r;
            ioptr[fly0P + FlyOffsetBIm] = scale * fly7i;

            fly0P -= Nrems2;
            fly0r = ioptr[fly0P];
            fly0i = ioptr[fly0P + 1];
            fly1r = ioptr[fly0P + FlyOffsetA];
            fly1i = ioptr[fly0P + FlyOffsetAIm];
            LoopCnt -= 1;
            fly1P = (IOP + ((int) BRcnt[LoopCnt] >> BRshift) * (2 * 2));
          }
          fly2r = ioptr[fly0P + 2];
          fly2i = ioptr[fly0P + (2 + 1)];
          fly3r = ioptr[fly0P + FlyOffsetB];
          fly3i = ioptr[fly0P + FlyOffsetBIm];

          t0r = fly0r + fly1r;
          t0i = fly0i + fly1i;
          fly1r = fly0r - fly1r;
          fly1i = fly0i - fly1i;
          t1r = fly2r + fly3r;
          t1i = fly2i + fly3i;
          fly3r = fly2r - fly3r;
          fly3i = fly2i - fly3i;

          ioptr[fly0P] = scale * t0r;
          ioptr[fly0P + 1] = scale * t0i;
          ioptr[fly0P + 2] = scale * fly1r;
          ioptr[fly0P + (2 + 1)] = scale * fly1i;
          ioptr[fly0P + FlyOffsetA] = scale * t1r;
          ioptr[fly0P + FlyOffsetAIm] = scale * t1i;
          ioptr[fly0P + FlyOffsetB] = scale * fly3r;
          ioptr[fly0P + FlyOffsetBIm] = scale * fly3i;

        }
      }



      /**** FFTC  **************/

      scale = 2.0;

      NdiffU = 2;
      Flyinc = (NdiffU);

      NsameU4 = Ntbl[M[0] + 1] / 4;	/* for RFFT */
      LoopN = Ntbl[M[0] - 3];

      stage = ((M[0] - 1) / 3);
      if ((M[0] - 1 - (stage * 3)) != 0) {
        FlyOffsetA = Flyinc << 2;
        FlyOffsetB = FlyOffsetA << 1;
        FlyOffsetAIm = FlyOffsetA + 1;
        FlyOffsetBIm = FlyOffsetB + 1;
        if ((M[0] - 1 - (stage * 3)) == 1) {
          /* 1 radix 2 stage */

          IOP = ioptrIndex;
          fly0P = IOP;
          fly1P = (IOP + Flyinc);
          fly2P = (fly1P + Flyinc);
          fly3P = (fly2P + Flyinc);

          /* Butterflys		*/
          /*
          t0	-	-	t0
          t1	-	-	t1
          f2	-  1-	f5
          f1	- -i-	f7
          f4	-	-	f4
          f0	-	-	f0
          f6	-  1-	f2
          f3	- -i-	f1
          */

          for (LoopCnt = LoopN; LoopCnt > 0; LoopCnt--) {
            t0r = ioptr[fly0P];
            t0i = ioptr[fly0P + 1];
            t1r = ioptr[fly1P];
            t1i = ioptr[fly1P + 1];
            fly2r = ioptr[fly2P];
            fly2i = ioptr[fly2P + 1];
            fly1r = ioptr[fly3P];
            fly1i = ioptr[fly3P + 1];
            fly4r = ioptr[fly0P + FlyOffsetA];
            fly4i = ioptr[fly0P + FlyOffsetAIm];
            fly0r = ioptr[fly1P + FlyOffsetA];
            fly0i = ioptr[fly1P + FlyOffsetAIm];
            fly6r = ioptr[fly2P + FlyOffsetA];
            fly6i = ioptr[fly2P + FlyOffsetAIm];
            fly3r = ioptr[fly3P + FlyOffsetA];
            fly3i = ioptr[fly3P + FlyOffsetAIm];

            fly5r = t0r - fly2r;
            fly5i = t0i - fly2i;
            t0r = t0r + fly2r;
            t0i = t0i + fly2i;

            fly7r = t1r - fly1i;
            fly7i = t1i + fly1r;
            t1r = t1r + fly1i;
            t1i = t1i - fly1r;

            fly2r = fly4r - fly6r;
            fly2i = fly4i - fly6i;
            fly4r = fly4r + fly6r;
            fly4i = fly4i + fly6i;

            fly1r = fly0r - fly3i;
            fly1i = fly0i + fly3r;
            fly0r = fly0r + fly3i;
            fly0i = fly0i - fly3r;

            ioptr[fly2P] = fly5r;
            ioptr[fly2P + 1] = fly5i;
            ioptr[fly0P] = t0r;
            ioptr[fly0P + 1] = t0i;
            ioptr[fly3P] = fly7r;
            ioptr[fly3P + 1] = fly7i;
            ioptr[fly1P] = t1r;
            ioptr[fly1P + 1] = t1i;
            ioptr[fly2P + FlyOffsetA] = fly2r;
            ioptr[fly2P + FlyOffsetAIm] = fly2i;
            ioptr[fly0P + FlyOffsetA] = fly4r;
            ioptr[fly0P + FlyOffsetAIm] = fly4i;
            ioptr[fly3P + FlyOffsetA] = fly1r;
            ioptr[fly3P + FlyOffsetAIm] = fly1i;
            ioptr[fly1P + FlyOffsetA] = fly0r;
            ioptr[fly1P + FlyOffsetAIm] = fly0i;

            fly0P = (fly0P + FlyOffsetB);
            fly1P = (fly1P + FlyOffsetB);
            fly2P = (fly2P + FlyOffsetB);
            fly3P = (fly3P + FlyOffsetB);
          }

          NsameU4 >>= 1;
          LoopN >>= 1;
          NdiffU <<= 1;
          Flyinc = Flyinc << 1;
        } else {
          /* 1 radix 4 stage */
          IOP = ioptrIndex;

          U3r = 0.7071067811865475244008443621; /* sqrt(0.5);	*/
          U3i = U3r;
          fly0P = IOP;
          fly1P = (IOP + Flyinc);
          fly2P = (fly1P + Flyinc);
          fly3P = (fly2P + Flyinc);

          /* Butterflys		*/
          /*
          t0	-	-	t0	-	-	t0
          t1	-	-	t1	-	-	t1
          f2	-  1-	f5	-	-	f5
          f1	- -i-	f7	-	-	f7
          f4	-	-	f4	-  1-	f6
          f0	-	-	f0	-U3	-	f3
          f6	-  1-	f2	- -i-	f4
          f3	- -i-	f1	-U3a-	f2
          */

          for (LoopCnt = LoopN; LoopCnt > 0; LoopCnt--) {
            t0r = ioptr[fly0P];
            t0i = ioptr[fly0P + 1];
            t1r = ioptr[fly1P];
            t1i = ioptr[fly1P + 1];
            fly2r = ioptr[fly2P];
            fly2i = ioptr[fly2P + 1];
            fly1r = ioptr[fly3P];
            fly1i = ioptr[fly3P + 1];
            fly4r = ioptr[fly0P + FlyOffsetA];
            fly4i = ioptr[fly0P + FlyOffsetAIm];
            fly0r = ioptr[fly1P + FlyOffsetA];
            fly0i = ioptr[fly1P + FlyOffsetAIm];
            fly6r = ioptr[fly2P + FlyOffsetA];
            fly6i = ioptr[fly2P + FlyOffsetAIm];
            fly3r = ioptr[fly3P + FlyOffsetA];
            fly3i = ioptr[fly3P + FlyOffsetAIm];

            fly5r = t0r - fly2r;
            fly5i = t0i - fly2i;
            t0r = t0r + fly2r;
            t0i = t0i + fly2i;

            fly7r = t1r - fly1i;
            fly7i = t1i + fly1r;
            t1r = t1r + fly1i;
            t1i = t1i - fly1r;

            fly2r = fly4r - fly6r;
            fly2i = fly4i - fly6i;
            fly4r = fly4r + fly6r;
            fly4i = fly4i + fly6i;

            fly1r = fly0r - fly3i;
            fly1i = fly0i + fly3r;
            fly0r = fly0r + fly3i;
            fly0i = fly0i - fly3r;

            fly6r = t0r - fly4r;
            fly6i = t0i - fly4i;
            t0r = t0r + fly4r;
            t0i = t0i + fly4i;

            fly3r = fly5r - fly2i;
            fly3i = fly5i + fly2r;
            fly5r = fly5r + fly2i;
            fly5i = fly5i - fly2r;

            fly4r = t1r - U3r * fly0r;
            fly4r = fly4r - U3i * fly0i;
            fly4i = t1i + U3i * fly0r;
            fly4i = fly4i - U3r * fly0i;
            t1r = scale * t1r - fly4r;
            t1i = scale * t1i - fly4i;

            fly2r = fly7r + U3i * fly1r;
            fly2r = fly2r - U3r * fly1i;
            fly2i = fly7i + U3r * fly1r;
            fly2i = fly2i + U3i * fly1i;
            fly7r = scale * fly7r - fly2r;
            fly7i = scale * fly7i - fly2i;

            ioptr[fly0P + FlyOffsetA] = fly6r;
            ioptr[fly0P + FlyOffsetAIm] = fly6i;
            ioptr[fly0P] = t0r;
            ioptr[fly0P + 1] = t0i;
            ioptr[fly2P + FlyOffsetA] = fly3r;
            ioptr[fly2P + FlyOffsetAIm] = fly3i;
            ioptr[fly2P] = fly5r;
            ioptr[fly2P + 1] = fly5i;
            ioptr[fly1P + FlyOffsetA] = fly4r;
            ioptr[fly1P + FlyOffsetAIm] = fly4i;
            ioptr[fly1P] = t1r;
            ioptr[fly1P + 1] = t1i;
            ioptr[fly3P + FlyOffsetA] = fly2r;
            ioptr[fly3P + FlyOffsetAIm] = fly2i;
            ioptr[fly3P] = fly7r;
            ioptr[fly3P + 1] = fly7i;

            fly0P = (fly0P + FlyOffsetB);
            fly1P = (fly1P + FlyOffsetB);
            fly2P = (fly2P + FlyOffsetB);
            fly3P = (fly3P + FlyOffsetB);

          }

          NsameU4 >>= 2;
          LoopN >>= 2;
          NdiffU <<= 2;
          Flyinc = Flyinc << 2;
        }
      }

      NsameU2 = NsameU4 >> 1;
      NsameU1 = NsameU2 >> 1;
      Flyinc <<= 1;
      FlyOffsetA = Flyinc << 2;
      FlyOffsetB = FlyOffsetA << 1;
      FlyOffsetAIm = FlyOffsetA + 1;
      FlyOffsetBIm = FlyOffsetB + 1;
      LoopN >>= 1;

      /*   ****** RADIX 8 Stages	*/
      for (stage = stage << 1; stage > 0; stage--) {

        /* an fft stage is done in two parts to ease use of the single quadrant cos table	*/

        /*	fftcalc1(iobuf, Utbl, N, NdiffU, LoopN);	*/
        if (!(MoreMath.odd(stage))) {
          U0rP = 0;
          U0iP = Ntbl[M[0] - 1];	/* for RFFT */
          U1rP = U0rP;
          U1iP = U0iP;
          U2rP = U0rP;
          U2iP = U0iP;
          U3offset = (Ntbl[M[0] + 1]) / 8;	/* for RFFT */

          IOP = ioptrIndex;

          U0r = Utbl[U0rP];
          U0i = Utbl[U0iP];
          U1r = Utbl[U1rP];
          U1i = Utbl[U1iP];
          U2r = Utbl[U2rP];
          U2i = Utbl[U2iP];
          U3r = Utbl[U2rP + U3offset];
          U3i = Utbl[U2iP - U3offset];
        }

        fly0P = IOP;
        fly1P = (IOP + Flyinc);
        fly2P = (fly1P + Flyinc);
        fly3P = (fly2P + Flyinc);

        for (diffUcnt = (NdiffU) >> 1; diffUcnt > 0; diffUcnt--) {

          /* Butterflys		*/
          /*
          f0	-	-	t0	-	-	t0	-	-	t0
          f1	-U0	-	t1	-	-	t1	-	-	t1
          f2	-	-	f2	-U1	-	f5	-	-	f3
          f3	-U0	-	f1	-U1a-	f7	-	-	f7
          f4	-	-	f4	-	-	f4	-U2	-	f6
          f5	-U0	-	f0	-	-	f0	-U3	-	f4
          f6	-	-	f6	-U1	-	f2	-U2a-	f2
          f7	-U0	-	f3	-U1a-	f1	-U3a-	f5
          */

          fly0r = ioptr[IOP];
          fly0i = ioptr[IOP + 1];
          fly1r = ioptr[fly1P];
          fly1i = ioptr[fly1P + 1];

          for (LoopCnt = LoopN - 1; LoopCnt > 0; LoopCnt--) {

            fly2r = ioptr[fly2P];
            fly2i = ioptr[fly2P + 1];
            fly3r = ioptr[fly3P];
            fly3i = ioptr[fly3P + 1];
            fly4r = ioptr[fly0P + FlyOffsetA];
            fly4i = ioptr[fly0P + FlyOffsetAIm];
            fly5r = ioptr[fly1P + FlyOffsetA];
            fly5i = ioptr[fly1P + FlyOffsetAIm];
            fly6r = ioptr[fly2P + FlyOffsetA];
            fly6i = ioptr[fly2P + FlyOffsetAIm];
            fly7r = ioptr[fly3P + FlyOffsetA];
            fly7i = ioptr[fly3P + FlyOffsetAIm];

            t1r = fly0r - U0r * fly1r;
            t1r = t1r - U0i * fly1i;
            t1i = fly0i + U0i * fly1r;
            t1i = t1i - U0r * fly1i;
            t0r = scale * fly0r - t1r;
            t0i = scale * fly0i - t1i;

            fly1r = fly2r - U0r * fly3r;
            fly1r = fly1r - U0i * fly3i;
            fly1i = fly2i + U0i * fly3r;
            fly1i = fly1i - U0r * fly3i;
            fly2r = scale * fly2r - fly1r;
            fly2i = scale * fly2i - fly1i;

            fly0r = fly4r - U0r * fly5r;
            fly0r = fly0r - U0i * fly5i;
            fly0i = fly4i + U0i * fly5r;
            fly0i = fly0i - U0r * fly5i;
            fly4r = scale * fly4r - fly0r;
            fly4i = scale * fly4i - fly0i;

            fly3r = fly6r - U0r * fly7r;
            fly3r = fly3r - U0i * fly7i;
            fly3i = fly6i + U0i * fly7r;
            fly3i = fly3i - U0r * fly7i;
            fly6r = scale * fly6r - fly3r;
            fly6i = scale * fly6i - fly3i;


            fly5r = t0r - U1r * fly2r;
            fly5r = fly5r - U1i * fly2i;
            fly5i = t0i + U1i * fly2r;
            fly5i = fly5i - U1r * fly2i;
            t0r = scale * t0r - fly5r;
            t0i = scale * t0i - fly5i;

            fly7r = t1r + U1i * fly1r;
            fly7r = fly7r - U1r * fly1i;
            fly7i = t1i + U1r * fly1r;
            fly7i = fly7i + U1i * fly1i;
            t1r = scale * t1r - fly7r;
            t1i = scale * t1i - fly7i;

            fly2r = fly4r - U1r * fly6r;
            fly2r = fly2r - U1i * fly6i;
            fly2i = fly4i + U1i * fly6r;
            fly2i = fly2i - U1r * fly6i;
            fly4r = scale * fly4r - fly2r;
            fly4i = scale * fly4i - fly2i;

            fly1r = fly0r + U1i * fly3r;
            fly1r = fly1r - U1r * fly3i;
            fly1i = fly0i + U1r * fly3r;
            fly1i = fly1i + U1i * fly3i;
            fly0r = scale * fly0r - fly1r;
            fly0i = scale * fly0i - fly1i;

            fly6r = t0r - U2r * fly4r;
            fly6r = fly6r - U2i * fly4i;
            fly6i = t0i + U2i * fly4r;
            fly6i = fly6i - U2r * fly4i;
            t0r = scale * t0r - fly6r;
            t0i = scale * t0i - fly6i;

            fly3r = fly5r - U2i * fly2r;
            fly3r = fly3r + U2r * fly2i;
            fly3i = fly5i - U2r * fly2r;
            fly3i = fly3i - U2i * fly2i;
            fly2r = scale * fly5r - fly3r;
            fly2i = scale * fly5i - fly3i;

            fly4r = t1r - U3r * fly0r;
            fly4r = fly4r - U3i * fly0i;
            fly4i = t1i + U3i * fly0r;
            fly4i = fly4i - U3r * fly0i;
            t1r = scale * t1r - fly4r;
            t1i = scale * t1i - fly4i;

            fly5r = fly7r + U3i * fly1r;
            fly5r = fly5r - U3r * fly1i;
            fly5i = fly7i + U3r * fly1r;
            fly5i = fly5i + U3i * fly1i;
            fly7r = scale * fly7r - fly5r;
            fly7i = scale * fly7i - fly5i;

            ioptr[fly0P + FlyOffsetA] = fly6r;
            ioptr[fly0P + FlyOffsetAIm] = fly6i;
            ioptr[fly0P] = t0r;
            ioptr[fly0P + 1] = t0i;
            ioptr[fly2P] = fly3r;
            ioptr[fly2P + 1] = fly3i;
            ioptr[fly2P + FlyOffsetA] = fly2r;
            ioptr[fly2P + FlyOffsetAIm] = fly2i;

            fly0r = ioptr[fly0P + FlyOffsetB];
            fly0i = ioptr[fly0P + FlyOffsetBIm];

            ioptr[fly1P + FlyOffsetA] = fly4r;
            ioptr[fly1P + FlyOffsetAIm] = fly4i;
            ioptr[fly1P] = t1r;
            ioptr[fly1P + 1] = t1i;

            fly1r = ioptr[fly1P + FlyOffsetB];
            fly1i = ioptr[fly1P + FlyOffsetBIm];

            ioptr[fly3P + FlyOffsetA] = fly5r;
            ioptr[fly3P + FlyOffsetAIm] = fly5i;
            ioptr[fly3P] = fly7r;
            ioptr[fly3P + 1] = fly7i;

            fly0P = (fly0P + FlyOffsetB);
            fly1P = (fly1P + FlyOffsetB);
            fly2P = (fly2P + FlyOffsetB);
            fly3P = (fly3P + FlyOffsetB);
          }

          fly2r = ioptr[fly2P];
          fly2i = ioptr[fly2P + 1];
          fly3r = ioptr[fly3P];
          fly3i = ioptr[fly3P + 1];
          fly4r = ioptr[fly0P + FlyOffsetA];
          fly4i = ioptr[fly0P + FlyOffsetAIm];
          fly5r = ioptr[fly1P + FlyOffsetA];
          fly5i = ioptr[fly1P + FlyOffsetAIm];
          fly6r = ioptr[fly2P + FlyOffsetA];
          fly6i = ioptr[fly2P + FlyOffsetAIm];
          fly7r = ioptr[fly3P + FlyOffsetA];
          fly7i = ioptr[fly3P + FlyOffsetAIm];

          t1r = fly0r - U0r * fly1r;
          t1r = t1r - U0i * fly1i;
          t1i = fly0i + U0i * fly1r;
          t1i = t1i - U0r * fly1i;
          t0r = scale * fly0r - t1r;
          t0i = scale * fly0i - t1i;

          fly1r = fly2r - U0r * fly3r;
          fly1r = fly1r - U0i * fly3i;
          fly1i = fly2i + U0i * fly3r;
          fly1i = fly1i - U0r * fly3i;
          fly2r = scale * fly2r - fly1r;
          fly2i = scale * fly2i - fly1i;

          fly0r = fly4r - U0r * fly5r;
          fly0r = fly0r - U0i * fly5i;
          fly0i = fly4i + U0i * fly5r;
          fly0i = fly0i - U0r * fly5i;
          fly4r = scale * fly4r - fly0r;
          fly4i = scale * fly4i - fly0i;

          fly3r = fly6r - U0r * fly7r;
          fly3r = fly3r - U0i * fly7i;
          fly3i = fly6i + U0i * fly7r;
          fly3i = fly3i - U0r * fly7i;
          fly6r = scale * fly6r - fly3r;
          fly6i = scale * fly6i - fly3i;


          fly5r = t0r - U1r * fly2r;
          fly5r = fly5r - U1i * fly2i;
          fly5i = t0i + U1i * fly2r;
          fly5i = fly5i - U1r * fly2i;
          t0r = scale * t0r - fly5r;
          t0i = scale * t0i - fly5i;

          fly7r = t1r + U1i * fly1r;
          fly7r = fly7r - U1r * fly1i;
          fly7i = t1i + U1r * fly1r;
          fly7i = fly7i + U1i * fly1i;
          t1r = scale * t1r - fly7r;
          t1i = scale * t1i - fly7i;

          fly2r = fly4r - U1r * fly6r;
          fly2r = fly2r - U1i * fly6i;
          fly2i = fly4i + U1i * fly6r;
          fly2i = fly2i - U1r * fly6i;
          fly4r = scale * fly4r - fly2r;
          fly4i = scale * fly4i - fly2i;

          fly1r = fly0r + U1i * fly3r;
          fly1r = fly1r - U1r * fly3i;
          fly1i = fly0i + U1r * fly3r;
          fly1i = fly1i + U1i * fly3i;
          fly0r = scale * fly0r - fly1r;
          fly0i = scale * fly0i - fly1i;
//			System.out.println(NsameU4);
//			System.out.println(U0iP);
          U0i = Utbl[U0iP = (U0iP - NsameU4)];
//			System.out.println(U0iP);
//			System.out.println(U0i);
          U0r = Utbl[U0rP = (U0rP + NsameU4)];
          U1r = Utbl[U1rP = (U1rP + NsameU2)];
          U1i = Utbl[U1iP = (U1iP - NsameU2)];
          if (MoreMath.odd(stage))
            U0r = -U0r;

          fly6r = t0r - U2r * fly4r;
          fly6r = fly6r - U2i * fly4i;
          fly6i = t0i + U2i * fly4r;
          fly6i = fly6i - U2r * fly4i;
          t0r = scale * t0r - fly6r;
          t0i = scale * t0i - fly6i;

          fly3r = fly5r - U2i * fly2r;
          fly3r = fly3r + U2r * fly2i;
          fly3i = fly5i - U2r * fly2r;
          fly3i = fly3i - U2i * fly2i;
          fly2r = scale * fly5r - fly3r;
          fly2i = scale * fly5i - fly3i;

          fly4r = t1r - U3r * fly0r;
          fly4r = fly4r - U3i * fly0i;
          fly4i = t1i + U3i * fly0r;
          fly4i = fly4i - U3r * fly0i;
          t1r = scale * t1r - fly4r;
          t1i = scale * t1i - fly4i;

          fly5r = fly7r + U3i * fly1r;
          fly5r = fly5r - U3r * fly1i;
          fly5i = fly7i + U3r * fly1r;
          fly5i = fly5i + U3i * fly1i;
          fly7r = scale * fly7r - fly5r;
          fly7i = scale * fly7i - fly5i;

          ioptr[fly0P + FlyOffsetA] = fly6r;
          ioptr[fly0P + FlyOffsetAIm] = fly6i;
          ioptr[fly0P] = t0r;
          ioptr[fly0P + 1] = t0i;

          U2r = Utbl[U2rP = (U2rP + NsameU1)];
          U2i = Utbl[U2iP = (U2iP - NsameU1)];

          ioptr[fly2P] = fly3r;
          ioptr[fly2P + 1] = fly3i;
          ioptr[fly2P + FlyOffsetA] = fly2r;
          ioptr[fly2P + FlyOffsetAIm] = fly2i;
          ioptr[fly1P + FlyOffsetA] = fly4r;
          ioptr[fly1P + FlyOffsetAIm] = fly4i;
          ioptr[fly1P] = t1r;
          ioptr[fly1P + 1] = t1i;

          U3r = Utbl[U2rP + U3offset];
          U3i = Utbl[U2iP - U3offset];

          ioptr[fly3P + FlyOffsetA] = fly5r;
          ioptr[fly3P + FlyOffsetAIm] = fly5i;
          ioptr[fly3P] = fly7r;
          ioptr[fly3P + 1] = fly7i;

          IOP = IOP + 2;
          fly0P = IOP;
          fly1P = (IOP + Flyinc);
          fly2P = (fly1P + Flyinc);
          fly3P = (fly2P + Flyinc);
        }
        NsameU4 = -NsameU4;

        if (MoreMath.odd(stage)) {
          LoopN >>= 3;
          NsameU1 >>= 3;
          NsameU2 >>= 3;
          NsameU4 >>= 3;
          NdiffU <<= 3;
          Flyinc = Flyinc << 3;
          FlyOffsetA <<= 3;
          FlyOffsetB <<= 3;
          FlyOffsetAIm = FlyOffsetA + 1;
          FlyOffsetBIm = FlyOffsetB + 1;
        }
      }

      /*	Finish RFFT		*/
      M[0] = M[0] + 1;

      FlyOffsetA = Ntbl[M[0]] * 2 / 4;
      FlyOffsetAIm = Ntbl[M[0]] * 2 / 4 + 1;

      IOP = ioptrIndex;

      fly0P = (IOP + Ntbl[M[0]] * 2 / 4);
      fly1P = (IOP + Ntbl[M[0]] * 2 / 8);

      U0rP = Ntbl[M[0] - 3];

      U0r = Utbl[U0rP];

      fly0r = ioptr[IOP];
      fly0i = ioptr[IOP + 1];
      fly1r = ioptr[fly0P];
      fly1i = ioptr[fly0P + 1];
      fly2r = ioptr[fly1P];
      fly2i = ioptr[fly1P + 1];
      fly3r = ioptr[fly1P + FlyOffsetA];
      fly3i = ioptr[fly1P + FlyOffsetAIm];

      t0r = scale * fly0r + scale * fly0i;
      t0i = 0.0;
      t1r = scale * fly1r;
      t1i = -scale * fly1i;

      fly0r = fly2r + fly3r;
      fly0i = fly2i - fly3i;
      fly1r = fly2i + fly3i;
      fly1i = fly3r - fly2r;

      fly2r = fly0r + U0r * fly1r;
      fly2r = fly2r + U0r * fly1i;
      fly2i = fly0i - U0r * fly1r;
      fly2i = fly2i + U0r * fly1i;
      fly3r = scale * fly0r - fly2r;
      fly3i = fly2i - scale * fly0i;

      ioptr[IOP] = t0r;
      ioptr[IOP + 1] = t0i;
      ioptr[fly0P] = t1r;
      ioptr[fly0P + 1] = t1i;
      ioptr[fly1P] = fly2r;
      ioptr[fly1P + 1] = fly2i;
      ioptr[fly1P + FlyOffsetA] = fly3r;
      ioptr[fly1P + FlyOffsetAIm] = fly3i;

      U0rP = 1;
      U0iP = Ntbl[M[0] - 2] - 1;

      U0r = Utbl[U0rP];
      U0i = Utbl[U0iP];

      fly0P = (IOP + 2);
      fly1P = (IOP + (Ntbl[M[0] - 2] - 1) * 2);

      /* Butterflys */
      /*
  f0	-	t0	-	-	f2
  f1	-	t1	-U0	-	f3
  f2	-	f0	-	-	t0
  f3	-	f1	-U0a-	t1
      */

      for (diffUcnt = Ntbl[M[0] - 3] - 1; diffUcnt > 0; diffUcnt--) {

        fly0r = ioptr[fly0P];
        fly0i = ioptr[fly0P + 1];
        fly1r = ioptr[fly1P + FlyOffsetA];
        fly1i = ioptr[fly1P + FlyOffsetAIm];
        fly2r = ioptr[fly1P];
        fly2i = ioptr[fly1P + 1];
        fly3r = ioptr[fly0P + FlyOffsetA];
        fly3i = ioptr[fly0P + FlyOffsetAIm];

        t0r = fly0r + fly1r;
        t0i = fly0i - fly1i;
        t1r = fly0i + fly1i;
        t1i = fly1r - fly0r;

        fly0r = fly2r + fly3r;
        fly0i = fly2i - fly3i;
        fly1r = fly2i + fly3i;
        fly1i = fly3r - fly2r;

        fly2r = t0r + U0r * t1r;
        fly2r = fly2r + U0i * t1i;
        fly2i = t0i - U0i * t1r;
        fly2i = fly2i + U0r * t1i;
        fly3r = scale * t0r - fly2r;
        fly3i = fly2i - scale * t0i;

        t0r = fly0r + U0i * fly1r;
        t0r = t0r + U0r * fly1i;
        t0i = fly0i - U0r * fly1r;
        t0i = t0i + U0i * fly1i;
        t1r = scale * fly0r - t0r;
        t1i = t0i - scale * fly0i;

        ioptr[fly0P] = fly2r;
        ioptr[fly0P + 1] = fly2i;
        ioptr[fly1P + FlyOffsetA] = fly3r;
        ioptr[fly1P + FlyOffsetAIm] = fly3i;

        U0r = Utbl[U0rP = (U0rP + 1)];
        U0i = Utbl[U0iP = (U0iP - 1)];

        ioptr[fly1P] = t0r;
        ioptr[fly1P + 1] = t0i;
        ioptr[fly0P + FlyOffsetA] = t1r;
        ioptr[fly0P + FlyOffsetAIm] = t1i;

        fly0P += 2;
        fly1P -= 2;
      }

      ioptrIndex += Ntbl[M[0]];
    }
  }

  /* irffts
   * compute multiple real input FFTs on 'Rows' consecutively stored arrays
   * ioptr = pointer to the data in and out
   * M[0] = log2 of fft size
   * Rows = number of FFTs to compute (use Rows of 1 if ioptr is a 1 dimensional array)
   * Utbl = Pointer to cosine table
   */

  public void irffts(double[] ioptr, int[] M, int Rows, double[] Utbl) {

    int Flyinc, FlyOffsetA, FlyOffsetAIm, FlyOffsetB, FlyOffsetBIm,
            NsameU1, NsameU2, NsameU4, diffUcnt, LoopCnt;
    double scale, fly0r, fly0i, fly1r, fly1i, fly2r, fly2i, fly3r, fly3i,
            fly4r, fly4i, fly5r, fly5i, fly6r, fly6i, fly7r, fly7i,
            U0r = 0.0, U0i = 0.0, U1r = 0.0, U1i = 0.0, U2r = 0.0, U2i = 0.0, U3r = 0.0, U3i = 0.0,
            t0r, t0i, t1r, t1i;

    int fly0P, fly1P, fly2P, fly3P,
            U0rP = 0, U0iP = 0, U1rP = 0, U1iP = 0, U2rP = 0, U2iP = 0, IOP = 0;
    int U3offset = 0;

    int stage;
    int NdiffU;
    int LoopN;

    int ioptrIndex = 0;

    final int BRshift = MAXMROOT - ((M[0] - 1) >> 1);		/* for RFFT */
    final int Nrems2 = Ntbl[(M[0] - 1) - ((M[0] - 1) >> 1) + 1];		/* for RFFT */
    final int Nroot_1_ColInc = (Ntbl[(M[0] - 1) - 1] - Ntbl[(M[0] - 1) - ((M[0] - 1) >> 1)]) * 2; /* for RFFT */

    for (; Rows > 0; Rows--) {

      M[0] = M[0] - 1;		/* for RFFT */

      FlyOffsetA = Ntbl[M[0]] * 2 / 2;
      FlyOffsetAIm = FlyOffsetA + 1;
      FlyOffsetB = FlyOffsetA + 2;
      FlyOffsetBIm = FlyOffsetB + 1;

      /* BitrevR2 ** bit reverse shuffle and first radix 2 stage ******/

      scale = 1. / Ntbl[M[0]];
      for (stage = 0; stage < Ntbl[M[0] - (M[0] >> 1)] * 2; stage += Ntbl[M[0] >> 1] * 2) {
        for (LoopN = (Ntbl[(M[0] >> 1) - 1] - 1); LoopN >= 0; LoopN--) {
          LoopCnt = (Ntbl[(M[0] >> 1) - 1] - 1);
          fly0P = ioptrIndex + Nroot_1_ColInc + ((int) BRcnt[LoopN] >> BRshift) * (2 * 2) + stage;
          IOP = ioptrIndex + (LoopN << (M[0] + 1) / 2) * 2 + stage;
          fly1P = IOP + ((int) BRcnt[LoopCnt] >> BRshift) * (2 * 2);
          fly0r = ioptr[fly0P];
          fly0i = ioptr[fly0P + 1];
          fly1r = ioptr[fly0P + FlyOffsetA];
          fly1i = ioptr[fly0P + FlyOffsetAIm];
          for (; LoopCnt > LoopN;) {
            fly2r = ioptr[fly0P + 2];
            fly2i = ioptr[fly0P + (2 + 1)];
            fly3r = ioptr[fly0P + FlyOffsetB];
            fly3i = ioptr[fly0P + FlyOffsetBIm];
            fly4r = ioptr[fly1P];
            fly4i = ioptr[fly1P + 1];
            fly5r = ioptr[fly1P + FlyOffsetA];
            fly5i = ioptr[fly1P + FlyOffsetAIm];
            fly6r = ioptr[fly1P + 2];
            fly6i = ioptr[fly1P + (2 + 1)];
            fly7r = ioptr[fly1P + FlyOffsetB];
            fly7i = ioptr[fly1P + FlyOffsetBIm];

            t0r = fly0r + fly1r;
            t0i = fly0i + fly1i;
            fly1r = fly0r - fly1r;
            fly1i = fly0i - fly1i;
            t1r = fly2r + fly3r;
            t1i = fly2i + fly3i;
            fly3r = fly2r - fly3r;
            fly3i = fly2i - fly3i;
            fly0r = fly4r + fly5r;
            fly0i = fly4i + fly5i;
            fly5r = fly4r - fly5r;
            fly5i = fly4i - fly5i;
            fly2r = fly6r + fly7r;
            fly2i = fly6i + fly7i;
            fly7r = fly6r - fly7r;
            fly7i = fly6i - fly7i;

            ioptr[fly1P] = scale * t0r;
            ioptr[fly1P + 1] = scale * t0i;
            ioptr[fly1P + 2] = scale * fly1r;
            ioptr[fly1P + (2 + 1)] = scale * fly1i;
            ioptr[fly1P + FlyOffsetA] = scale * t1r;
            ioptr[fly1P + FlyOffsetAIm] = scale * t1i;
            ioptr[fly1P + FlyOffsetB] = scale * fly3r;
            ioptr[fly1P + FlyOffsetBIm] = scale * fly3i;
            ioptr[fly0P] = scale * fly0r;
            ioptr[fly0P + 1] = scale * fly0i;
            ioptr[fly0P + 2] = scale * fly5r;
            ioptr[fly0P + (2 + 1)] = scale * fly5i;
            ioptr[fly0P + FlyOffsetA] = scale * fly2r;
            ioptr[fly0P + FlyOffsetAIm] = scale * fly2i;
            ioptr[fly0P + FlyOffsetB] = scale * fly7r;
            ioptr[fly0P + FlyOffsetBIm] = scale * fly7i;

            fly0P -= Nrems2;
            fly0r = ioptr[fly0P];
            fly0i = ioptr[fly0P + 1];
            fly1r = ioptr[fly0P + FlyOffsetA];
            fly1i = ioptr[fly0P + FlyOffsetAIm];
            LoopCnt -= 1;
            fly1P = (IOP + ((int) BRcnt[LoopCnt] >> BRshift) * (2 * 2));
          }
          fly2r = ioptr[fly0P + 2];
          fly2i = ioptr[fly0P + (2 + 1)];
          fly3r = ioptr[fly0P + FlyOffsetB];
          fly3i = ioptr[fly0P + FlyOffsetBIm];

          t0r = fly0r + fly1r;
          t0i = fly0i + fly1i;
          fly1r = fly0r - fly1r;
          fly1i = fly0i - fly1i;
          t1r = fly2r + fly3r;
          t1i = fly2i + fly3i;
          fly3r = fly2r - fly3r;
          fly3i = fly2i - fly3i;

          ioptr[fly0P] = scale * t0r;
          ioptr[fly0P + 1] = scale * t0i;
          ioptr[fly0P + 2] = scale * fly1r;
          ioptr[fly0P + (2 + 1)] = scale * fly1i;
          ioptr[fly0P + FlyOffsetA] = scale * t1r;
          ioptr[fly0P + FlyOffsetAIm] = scale * t1i;
          ioptr[fly0P + FlyOffsetB] = scale * fly3r;
          ioptr[fly0P + FlyOffsetBIm] = scale * fly3i;

        }
      }



      /**** FFTC  **************/

      scale = 2.0;

      NdiffU = 2;
      Flyinc = (NdiffU);

      NsameU4 = Ntbl[M[0] + 1] / 4;	/* for RFFT */
      LoopN = Ntbl[M[0] - 3];

      stage = ((M[0] - 1) / 3);
      if ((M[0] - 1 - (stage * 3)) != 0) {
        FlyOffsetA = Flyinc << 2;
        FlyOffsetB = FlyOffsetA << 1;
        FlyOffsetAIm = FlyOffsetA + 1;
        FlyOffsetBIm = FlyOffsetB + 1;
        if ((M[0] - 1 - (stage * 3)) == 1) {
          /* 1 radix 2 stage */

          IOP = ioptrIndex;
          fly0P = IOP;
          fly1P = (IOP + Flyinc);
          fly2P = (fly1P + Flyinc);
          fly3P = (fly2P + Flyinc);

          /* Butterflys		*/
          /*
          t0	-	-	t0
          t1	-	-	t1
          f2	-  1-	f5
          f1	- -i-	f7
          f4	-	-	f4
          f0	-	-	f0
          f6	-  1-	f2
          f3	- -i-	f1
          */

          for (LoopCnt = LoopN; LoopCnt > 0; LoopCnt--) {
            t0r = ioptr[fly0P];
            t0i = ioptr[fly0P + 1];
            t1r = ioptr[fly1P];
            t1i = ioptr[fly1P + 1];
            fly2r = ioptr[fly2P];
            fly2i = ioptr[fly2P + 1];
            fly1r = ioptr[fly3P];
            fly1i = ioptr[fly3P + 1];
            fly4r = ioptr[fly0P + FlyOffsetA];
            fly4i = ioptr[fly0P + FlyOffsetAIm];
            fly0r = ioptr[fly1P + FlyOffsetA];
            fly0i = ioptr[fly1P + FlyOffsetAIm];
            fly6r = ioptr[fly2P + FlyOffsetA];
            fly6i = ioptr[fly2P + FlyOffsetAIm];
            fly3r = ioptr[fly3P + FlyOffsetA];
            fly3i = ioptr[fly3P + FlyOffsetAIm];

            fly5r = t0r - fly2r;
            fly5i = t0i - fly2i;
            t0r = t0r + fly2r;
            t0i = t0i + fly2i;

            fly7r = t1r - fly1i;
            fly7i = t1i + fly1r;
            t1r = t1r + fly1i;
            t1i = t1i - fly1r;

            fly2r = fly4r - fly6r;
            fly2i = fly4i - fly6i;
            fly4r = fly4r + fly6r;
            fly4i = fly4i + fly6i;

            fly1r = fly0r - fly3i;
            fly1i = fly0i + fly3r;
            fly0r = fly0r + fly3i;
            fly0i = fly0i - fly3r;

            ioptr[fly2P] = fly5r;
            ioptr[fly2P + 1] = fly5i;
            ioptr[fly0P] = t0r;
            ioptr[fly0P + 1] = t0i;
            ioptr[fly3P] = fly7r;
            ioptr[fly3P + 1] = fly7i;
            ioptr[fly1P] = t1r;
            ioptr[fly1P + 1] = t1i;
            ioptr[fly2P + FlyOffsetA] = fly2r;
            ioptr[fly2P + FlyOffsetAIm] = fly2i;
            ioptr[fly0P + FlyOffsetA] = fly4r;
            ioptr[fly0P + FlyOffsetAIm] = fly4i;
            ioptr[fly3P + FlyOffsetA] = fly1r;
            ioptr[fly3P + FlyOffsetAIm] = fly1i;
            ioptr[fly1P + FlyOffsetA] = fly0r;
            ioptr[fly1P + FlyOffsetAIm] = fly0i;

            fly0P = (fly0P + FlyOffsetB);
            fly1P = (fly1P + FlyOffsetB);
            fly2P = (fly2P + FlyOffsetB);
            fly3P = (fly3P + FlyOffsetB);
          }

          NsameU4 >>= 1;
          LoopN >>= 1;
          NdiffU <<= 1;
          Flyinc = Flyinc << 1;
        } else {
          /* 1 radix 4 stage */
          IOP = ioptrIndex;

          U3r = 0.7071067811865475244008443621; /* sqrt(0.5);	*/
          U3i = U3r;
          fly0P = IOP;
          fly1P = (IOP + Flyinc);
          fly2P = (fly1P + Flyinc);
          fly3P = (fly2P + Flyinc);

          /* Butterflys		*/
          /*
          t0	-	-	t0	-	-	t0
          t1	-	-	t1	-	-	t1
          f2	-  1-	f5	-	-	f5
          f1	- -i-	f7	-	-	f7
          f4	-	-	f4	-  1-	f6
          f0	-	-	f0	-U3	-	f3
          f6	-  1-	f2	- -i-	f4
          f3	- -i-	f1	-U3a-	f2
          */

          for (LoopCnt = LoopN; LoopCnt > 0; LoopCnt--) {
            t0r = ioptr[fly0P];
            t0i = ioptr[fly0P + 1];
            t1r = ioptr[fly1P];
            t1i = ioptr[fly1P + 1];
            fly2r = ioptr[fly2P];
            fly2i = ioptr[fly2P + 1];
            fly1r = ioptr[fly3P];
            fly1i = ioptr[fly3P + 1];
            fly4r = ioptr[fly0P + FlyOffsetA];
            fly4i = ioptr[fly0P + FlyOffsetAIm];
            fly0r = ioptr[fly1P + FlyOffsetA];
            fly0i = ioptr[fly1P + FlyOffsetAIm];
            fly6r = ioptr[fly2P + FlyOffsetA];
            fly6i = ioptr[fly2P + FlyOffsetAIm];
            fly3r = ioptr[fly3P + FlyOffsetA];
            fly3i = ioptr[fly3P + FlyOffsetAIm];

            fly5r = t0r - fly2r;
            fly5i = t0i - fly2i;
            t0r = t0r + fly2r;
            t0i = t0i + fly2i;

            fly7r = t1r - fly1i;
            fly7i = t1i + fly1r;
            t1r = t1r + fly1i;
            t1i = t1i - fly1r;

            fly2r = fly4r - fly6r;
            fly2i = fly4i - fly6i;
            fly4r = fly4r + fly6r;
            fly4i = fly4i + fly6i;

            fly1r = fly0r - fly3i;
            fly1i = fly0i + fly3r;
            fly0r = fly0r + fly3i;
            fly0i = fly0i - fly3r;

            fly6r = t0r - fly4r;
            fly6i = t0i - fly4i;
            t0r = t0r + fly4r;
            t0i = t0i + fly4i;

            fly3r = fly5r - fly2i;
            fly3i = fly5i + fly2r;
            fly5r = fly5r + fly2i;
            fly5i = fly5i - fly2r;

            fly4r = t1r - U3r * fly0r;
            fly4r = fly4r - U3i * fly0i;
            fly4i = t1i + U3i * fly0r;
            fly4i = fly4i - U3r * fly0i;
            t1r = scale * t1r - fly4r;
            t1i = scale * t1i - fly4i;

            fly2r = fly7r + U3i * fly1r;
            fly2r = fly2r - U3r * fly1i;
            fly2i = fly7i + U3r * fly1r;
            fly2i = fly2i + U3i * fly1i;
            fly7r = scale * fly7r - fly2r;
            fly7i = scale * fly7i - fly2i;

            ioptr[fly0P + FlyOffsetA] = fly6r;
            ioptr[fly0P + FlyOffsetAIm] = fly6i;
            ioptr[fly0P] = t0r;
            ioptr[fly0P + 1] = t0i;
            ioptr[fly2P + FlyOffsetA] = fly3r;
            ioptr[fly2P + FlyOffsetAIm] = fly3i;
            ioptr[fly2P] = fly5r;
            ioptr[fly2P + 1] = fly5i;
            ioptr[fly1P + FlyOffsetA] = fly4r;
            ioptr[fly1P + FlyOffsetAIm] = fly4i;
            ioptr[fly1P] = t1r;
            ioptr[fly1P + 1] = t1i;
            ioptr[fly3P + FlyOffsetA] = fly2r;
            ioptr[fly3P + FlyOffsetAIm] = fly2i;
            ioptr[fly3P] = fly7r;
            ioptr[fly3P + 1] = fly7i;

            fly0P = (fly0P + FlyOffsetB);
            fly1P = (fly1P + FlyOffsetB);
            fly2P = (fly2P + FlyOffsetB);
            fly3P = (fly3P + FlyOffsetB);

          }

          NsameU4 >>= 2;
          LoopN >>= 2;
          NdiffU <<= 2;
          Flyinc = Flyinc << 2;
        }
      }

      NsameU2 = NsameU4 >> 1;
      NsameU1 = NsameU2 >> 1;
      Flyinc <<= 1;
      FlyOffsetA = Flyinc << 2;
      FlyOffsetB = FlyOffsetA << 1;
      FlyOffsetAIm = FlyOffsetA + 1;
      FlyOffsetBIm = FlyOffsetB + 1;
      LoopN >>= 1;

      /*   ****** RADIX 8 Stages	*/
      for (stage = stage << 1; stage > 0; stage--) {

        /* an fft stage is done in two parts to ease use of the single quadrant cos table	*/

        /*	fftcalc1(iobuf, Utbl, N, NdiffU, LoopN);	*/
        if (!(MoreMath.odd(stage))) {
          U0rP = 0;
          U0iP = Ntbl[M[0] - 1];	/* for RFFT */
          U1rP = U0rP;
          U1iP = U0iP;
          U2rP = U0rP;
          U2iP = U0iP;
          U3offset = (Ntbl[M[0] + 1]) / 8;	/* for RFFT */

          IOP = ioptrIndex;

          U0r = Utbl[U0rP];
          U0i = Utbl[U0iP];
          U1r = Utbl[U1rP];
          U1i = Utbl[U1iP];
          U2r = Utbl[U2rP];
          U2i = Utbl[U2iP];
          U3r = Utbl[U2rP + U3offset];
          U3i = Utbl[U2iP - U3offset];
        }

        fly0P = IOP;
        fly1P = (IOP + Flyinc);
        fly2P = (fly1P + Flyinc);
        fly3P = (fly2P + Flyinc);

        for (diffUcnt = (NdiffU) >> 1; diffUcnt > 0; diffUcnt--) {

          /* Butterflys		*/
          /*
          f0	-	-	t0	-	-	t0	-	-	t0
          f1	-U0	-	t1	-	-	t1	-	-	t1
          f2	-	-	f2	-U1	-	f5	-	-	f3
          f3	-U0	-	f1	-U1a-	f7	-	-	f7
          f4	-	-	f4	-	-	f4	-U2	-	f6
          f5	-U0	-	f0	-	-	f0	-U3	-	f4
          f6	-	-	f6	-U1	-	f2	-U2a-	f2
          f7	-U0	-	f3	-U1a-	f1	-U3a-	f5
          */

          fly0r = ioptr[IOP];
          fly0i = ioptr[IOP + 1];
          fly1r = ioptr[fly1P];
          fly1i = ioptr[fly1P + 1];

          for (LoopCnt = LoopN - 1; LoopCnt > 0; LoopCnt--) {

            fly2r = ioptr[fly2P];
            fly2i = ioptr[fly2P + 1];
            fly3r = ioptr[fly3P];
            fly3i = ioptr[fly3P + 1];
            fly4r = ioptr[fly0P + FlyOffsetA];
            fly4i = ioptr[fly0P + FlyOffsetAIm];
            fly5r = ioptr[fly1P + FlyOffsetA];
            fly5i = ioptr[fly1P + FlyOffsetAIm];
            fly6r = ioptr[fly2P + FlyOffsetA];
            fly6i = ioptr[fly2P + FlyOffsetAIm];
            fly7r = ioptr[fly3P + FlyOffsetA];
            fly7i = ioptr[fly3P + FlyOffsetAIm];

            t1r = fly0r - U0r * fly1r;
            t1r = t1r - U0i * fly1i;
            t1i = fly0i + U0i * fly1r;
            t1i = t1i - U0r * fly1i;
            t0r = scale * fly0r - t1r;
            t0i = scale * fly0i - t1i;

            fly1r = fly2r - U0r * fly3r;
            fly1r = fly1r - U0i * fly3i;
            fly1i = fly2i + U0i * fly3r;
            fly1i = fly1i - U0r * fly3i;
            fly2r = scale * fly2r - fly1r;
            fly2i = scale * fly2i - fly1i;

            fly0r = fly4r - U0r * fly5r;
            fly0r = fly0r - U0i * fly5i;
            fly0i = fly4i + U0i * fly5r;
            fly0i = fly0i - U0r * fly5i;
            fly4r = scale * fly4r - fly0r;
            fly4i = scale * fly4i - fly0i;

            fly3r = fly6r - U0r * fly7r;
            fly3r = fly3r - U0i * fly7i;
            fly3i = fly6i + U0i * fly7r;
            fly3i = fly3i - U0r * fly7i;
            fly6r = scale * fly6r - fly3r;
            fly6i = scale * fly6i - fly3i;


            fly5r = t0r - U1r * fly2r;
            fly5r = fly5r - U1i * fly2i;
            fly5i = t0i + U1i * fly2r;
            fly5i = fly5i - U1r * fly2i;
            t0r = scale * t0r - fly5r;
            t0i = scale * t0i - fly5i;

            fly7r = t1r + U1i * fly1r;
            fly7r = fly7r - U1r * fly1i;
            fly7i = t1i + U1r * fly1r;
            fly7i = fly7i + U1i * fly1i;
            t1r = scale * t1r - fly7r;
            t1i = scale * t1i - fly7i;

            fly2r = fly4r - U1r * fly6r;
            fly2r = fly2r - U1i * fly6i;
            fly2i = fly4i + U1i * fly6r;
            fly2i = fly2i - U1r * fly6i;
            fly4r = scale * fly4r - fly2r;
            fly4i = scale * fly4i - fly2i;

            fly1r = fly0r + U1i * fly3r;
            fly1r = fly1r - U1r * fly3i;
            fly1i = fly0i + U1r * fly3r;
            fly1i = fly1i + U1i * fly3i;
            fly0r = scale * fly0r - fly1r;
            fly0i = scale * fly0i - fly1i;

            fly6r = t0r - U2r * fly4r;
            fly6r = fly6r - U2i * fly4i;
            fly6i = t0i + U2i * fly4r;
            fly6i = fly6i - U2r * fly4i;
            t0r = scale * t0r - fly6r;
            t0i = scale * t0i - fly6i;

            fly3r = fly5r - U2i * fly2r;
            fly3r = fly3r + U2r * fly2i;
            fly3i = fly5i - U2r * fly2r;
            fly3i = fly3i - U2i * fly2i;
            fly2r = scale * fly5r - fly3r;
            fly2i = scale * fly5i - fly3i;

            fly4r = t1r - U3r * fly0r;
            fly4r = fly4r - U3i * fly0i;
            fly4i = t1i + U3i * fly0r;
            fly4i = fly4i - U3r * fly0i;
            t1r = scale * t1r - fly4r;
            t1i = scale * t1i - fly4i;

            fly5r = fly7r + U3i * fly1r;
            fly5r = fly5r - U3r * fly1i;
            fly5i = fly7i + U3r * fly1r;
            fly5i = fly5i + U3i * fly1i;
            fly7r = scale * fly7r - fly5r;
            fly7i = scale * fly7i - fly5i;

            ioptr[fly0P + FlyOffsetA] = fly6r;
            ioptr[fly0P + FlyOffsetAIm] = fly6i;
            ioptr[fly0P] = t0r;
            ioptr[fly0P + 1] = t0i;
            ioptr[fly2P] = fly3r;
            ioptr[fly2P + 1] = fly3i;
            ioptr[fly2P + FlyOffsetA] = fly2r;
            ioptr[fly2P + FlyOffsetAIm] = fly2i;

            fly0r = ioptr[fly0P + FlyOffsetB];
            fly0i = ioptr[fly0P + FlyOffsetBIm];

            ioptr[fly1P + FlyOffsetA] = fly4r;
            ioptr[fly1P + FlyOffsetAIm] = fly4i;
            ioptr[fly1P] = t1r;
            ioptr[fly1P + 1] = t1i;

            fly1r = ioptr[fly1P + FlyOffsetB];
            fly1i = ioptr[fly1P + FlyOffsetBIm];

            ioptr[fly3P + FlyOffsetA] = fly5r;
            ioptr[fly3P + FlyOffsetAIm] = fly5i;
            ioptr[fly3P] = fly7r;
            ioptr[fly3P + 1] = fly7i;

            fly0P = (fly0P + FlyOffsetB);
            fly1P = (fly1P + FlyOffsetB);
            fly2P = (fly2P + FlyOffsetB);
            fly3P = (fly3P + FlyOffsetB);
          }

          fly2r = ioptr[fly2P];
          fly2i = ioptr[fly2P + 1];
          fly3r = ioptr[fly3P];
          fly3i = ioptr[fly3P + 1];
          fly4r = ioptr[fly0P + FlyOffsetA];
          fly4i = ioptr[fly0P + FlyOffsetAIm];
          fly5r = ioptr[fly1P + FlyOffsetA];
          fly5i = ioptr[fly1P + FlyOffsetAIm];
          fly6r = ioptr[fly2P + FlyOffsetA];
          fly6i = ioptr[fly2P + FlyOffsetAIm];
          fly7r = ioptr[fly3P + FlyOffsetA];
          fly7i = ioptr[fly3P + FlyOffsetAIm];

          t1r = fly0r - U0r * fly1r;
          t1r = t1r - U0i * fly1i;
          t1i = fly0i + U0i * fly1r;
          t1i = t1i - U0r * fly1i;
          t0r = scale * fly0r - t1r;
          t0i = scale * fly0i - t1i;

          fly1r = fly2r - U0r * fly3r;
          fly1r = fly1r - U0i * fly3i;
          fly1i = fly2i + U0i * fly3r;
          fly1i = fly1i - U0r * fly3i;
          fly2r = scale * fly2r - fly1r;
          fly2i = scale * fly2i - fly1i;

          fly0r = fly4r - U0r * fly5r;
          fly0r = fly0r - U0i * fly5i;
          fly0i = fly4i + U0i * fly5r;
          fly0i = fly0i - U0r * fly5i;
          fly4r = scale * fly4r - fly0r;
          fly4i = scale * fly4i - fly0i;

          fly3r = fly6r - U0r * fly7r;
          fly3r = fly3r - U0i * fly7i;
          fly3i = fly6i + U0i * fly7r;
          fly3i = fly3i - U0r * fly7i;
          fly6r = scale * fly6r - fly3r;
          fly6i = scale * fly6i - fly3i;


          fly5r = t0r - U1r * fly2r;
          fly5r = fly5r - U1i * fly2i;
          fly5i = t0i + U1i * fly2r;
          fly5i = fly5i - U1r * fly2i;
          t0r = scale * t0r - fly5r;
          t0i = scale * t0i - fly5i;

          fly7r = t1r + U1i * fly1r;
          fly7r = fly7r - U1r * fly1i;
          fly7i = t1i + U1r * fly1r;
          fly7i = fly7i + U1i * fly1i;
          t1r = scale * t1r - fly7r;
          t1i = scale * t1i - fly7i;

          fly2r = fly4r - U1r * fly6r;
          fly2r = fly2r - U1i * fly6i;
          fly2i = fly4i + U1i * fly6r;
          fly2i = fly2i - U1r * fly6i;
          fly4r = scale * fly4r - fly2r;
          fly4i = scale * fly4i - fly2i;

          fly1r = fly0r + U1i * fly3r;
          fly1r = fly1r - U1r * fly3i;
          fly1i = fly0i + U1r * fly3r;
          fly1i = fly1i + U1i * fly3i;
          fly0r = scale * fly0r - fly1r;
          fly0i = scale * fly0i - fly1i;
//			System.out.println(NsameU4);
//			System.out.println(U0iP);
          U0i = Utbl[U0iP = (U0iP - NsameU4)];
//			System.out.println(U0iP);
//			System.out.println(U0i);
          U0r = Utbl[U0rP = (U0rP + NsameU4)];
          U1r = Utbl[U1rP = (U1rP + NsameU2)];
          U1i = Utbl[U1iP = (U1iP - NsameU2)];
          if (MoreMath.odd(stage))
            U0r = -U0r;

          fly6r = t0r - U2r * fly4r;
          fly6r = fly6r - U2i * fly4i;
          fly6i = t0i + U2i * fly4r;
          fly6i = fly6i - U2r * fly4i;
          t0r = scale * t0r - fly6r;
          t0i = scale * t0i - fly6i;

          fly3r = fly5r - U2i * fly2r;
          fly3r = fly3r + U2r * fly2i;
          fly3i = fly5i - U2r * fly2r;
          fly3i = fly3i - U2i * fly2i;
          fly2r = scale * fly5r - fly3r;
          fly2i = scale * fly5i - fly3i;

          fly4r = t1r - U3r * fly0r;
          fly4r = fly4r - U3i * fly0i;
          fly4i = t1i + U3i * fly0r;
          fly4i = fly4i - U3r * fly0i;
          t1r = scale * t1r - fly4r;
          t1i = scale * t1i - fly4i;

          fly5r = fly7r + U3i * fly1r;
          fly5r = fly5r - U3r * fly1i;
          fly5i = fly7i + U3r * fly1r;
          fly5i = fly5i + U3i * fly1i;
          fly7r = scale * fly7r - fly5r;
          fly7i = scale * fly7i - fly5i;

          ioptr[fly0P + FlyOffsetA] = fly6r;
          ioptr[fly0P + FlyOffsetAIm] = fly6i;
          ioptr[fly0P] = t0r;
          ioptr[fly0P + 1] = t0i;

          U2r = Utbl[U2rP = (U2rP + NsameU1)];
          U2i = Utbl[U2iP = (U2iP - NsameU1)];

          ioptr[fly2P] = fly3r;
          ioptr[fly2P + 1] = fly3i;
          ioptr[fly2P + FlyOffsetA] = fly2r;
          ioptr[fly2P + FlyOffsetAIm] = fly2i;
          ioptr[fly1P + FlyOffsetA] = fly4r;
          ioptr[fly1P + FlyOffsetAIm] = fly4i;
          ioptr[fly1P] = t1r;
          ioptr[fly1P + 1] = t1i;

          U3r = Utbl[U2rP + U3offset];
          U3i = Utbl[U2iP - U3offset];

          ioptr[fly3P + FlyOffsetA] = fly5r;
          ioptr[fly3P + FlyOffsetAIm] = fly5i;
          ioptr[fly3P] = fly7r;
          ioptr[fly3P + 1] = fly7i;

          IOP = IOP + 2;
          fly0P = IOP;
          fly1P = (IOP + Flyinc);
          fly2P = (fly1P + Flyinc);
          fly3P = (fly2P + Flyinc);
        }
        NsameU4 = -NsameU4;

        if (MoreMath.odd(stage)) {
          LoopN >>= 3;
          NsameU1 >>= 3;
          NsameU2 >>= 3;
          NsameU4 >>= 3;
          NdiffU <<= 3;
          Flyinc = Flyinc << 3;
          FlyOffsetA <<= 3;
          FlyOffsetB <<= 3;
          FlyOffsetAIm = FlyOffsetA + 1;
          FlyOffsetBIm = FlyOffsetB + 1;
        }
      }

      /*	Finish RFFT		*/
      M[0] = M[0] + 1;

      FlyOffsetA = Ntbl[M[0]] * 2 / 4;
      FlyOffsetAIm = Ntbl[M[0]] * 2 / 4 + 1;

      IOP = ioptrIndex;

      fly0P = (IOP + Ntbl[M[0]] * 2 / 4);
      fly1P = (IOP + Ntbl[M[0]] * 2 / 8);

      U0rP = Ntbl[M[0] - 3];

      U0r = Utbl[U0rP];

      fly0r = ioptr[IOP];
      fly0i = ioptr[IOP + 1];
      fly1r = ioptr[fly0P];
      fly1i = ioptr[fly0P + 1];
      fly2r = ioptr[fly1P];
      fly2i = ioptr[fly1P + 1];
      fly3r = ioptr[fly1P + FlyOffsetA];
      fly3i = ioptr[fly1P + FlyOffsetAIm];

      t0r = scale * fly0r + scale * fly0i;
      t0i = 0.0;
      t1r = scale * fly1r;
      t1i = -scale * fly1i;

      fly0r = fly2r + fly3r;
      fly0i = fly2i - fly3i;
      fly1r = fly2i + fly3i;
      fly1i = fly3r - fly2r;

      fly2r = fly0r + U0r * fly1r;
      fly2r = fly2r + U0r * fly1i;
      fly2i = fly0i - U0r * fly1r;
      fly2i = fly2i + U0r * fly1i;
      fly3r = scale * fly0r - fly2r;
      fly3i = fly2i - scale * fly0i;

      ioptr[IOP] = t0r;
      ioptr[IOP + 1] = t0i;
      ioptr[fly0P] = t1r;
      ioptr[fly0P + 1] = t1i;
      ioptr[fly1P] = fly2r;
      ioptr[fly1P + 1] = fly2i;
      ioptr[fly1P + FlyOffsetA] = fly3r;
      ioptr[fly1P + FlyOffsetAIm] = fly3i;

      U0rP = 1;
      U0iP = Ntbl[M[0] - 2] - 1;

      U0r = Utbl[U0rP];
      U0i = Utbl[U0iP];

      fly0P = (IOP + 2);
      fly1P = (IOP + (Ntbl[M[0] - 2] - 1) * 2);

      /* Butterflys */
      /*
  f0	-	t0	-	-	f2
  f1	-	t1	-U0	-	f3
  f2	-	f0	-	-	t0
  f3	-	f1	-U0a-	t1
      */

      for (diffUcnt = Ntbl[M[0] - 3] - 1; diffUcnt > 0; diffUcnt--) {

        fly0r = ioptr[fly0P];
        fly0i = ioptr[fly0P + 1];
        fly1r = ioptr[fly1P + FlyOffsetA];
        fly1i = ioptr[fly1P + FlyOffsetAIm];
        fly2r = ioptr[fly1P];
        fly2i = ioptr[fly1P + 1];
        fly3r = ioptr[fly0P + FlyOffsetA];
        fly3i = ioptr[fly0P + FlyOffsetAIm];

        t0r = fly0r + fly1r;
        t0i = fly0i - fly1i;
        t1r = fly0i + fly1i;
        t1i = fly1r - fly0r;

        fly0r = fly2r + fly3r;
        fly0i = fly2i - fly3i;
        fly1r = fly2i + fly3i;
        fly1i = fly3r - fly2r;

        fly2r = t0r + U0r * t1r;
        fly2r = fly2r + U0i * t1i;
        fly2i = t0i - U0i * t1r;
        fly2i = fly2i + U0r * t1i;
        fly3r = scale * t0r - fly2r;
        fly3i = fly2i - scale * t0i;

        t0r = fly0r + U0i * fly1r;
        t0r = t0r + U0r * fly1i;
        t0i = fly0i - U0r * fly1r;
        t0i = t0i + U0i * fly1i;
        t1r = scale * fly0r - t0r;
        t1i = t0i - scale * fly0i;

        ioptr[fly0P] = fly2r;
        ioptr[fly0P + 1] = fly2i;
        ioptr[fly1P + FlyOffsetA] = fly3r;
        ioptr[fly1P + FlyOffsetAIm] = fly3i;

        U0r = Utbl[U0rP = (U0rP + 1)];
        U0i = Utbl[U0iP = (U0iP - 1)];

        ioptr[fly1P] = t0r;
        ioptr[fly1P + 1] = t0i;
        ioptr[fly0P + FlyOffsetA] = t1r;
        ioptr[fly0P + FlyOffsetAIm] = t1i;

        fly0P += 2;
        fly1P -= 2;
      }

      ioptrIndex += Ntbl[M[0]];
    }
  }

}
