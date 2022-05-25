/*
 * @(#)FFT.java created 8/09/2001 Casalino
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
 * The FFT is a class to perform fast fourier transform
 *
 * @version $Revision: 1.2 $, $Date: 1970/01/04 19:14:50 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class FFT {

  public FFT() {
  }

/*
** FFT and FHT routines
**  Copyright 1988, 1993; Ron Mayer
**
**  fht(fz,n);
**      Does a hartley transform of "n" points in the array "fz".
**  fft(n,real,imag)
**      Does a fourier transform of "n" points of the "real" and
**      "imag" arrays.
**  ifft(n,real,imag)
**      Does an inverse fourier transform of "n" points of the "real"
**      and "imag" arrays.
**  realfft(n,real)
**      Does a real-valued fourier transform of "n" points of the
**      "real" and "imag" arrays.  The real part of the transform ends
**      up in the first half of the array and the imaginary part of the
**      transform ends up in the second half of the array.
**  realifft(n,real)
**      The inverse of the realfft() routine above.
**
**
** NOTE: This routine uses at least 2 patented algorithms, and may be
**       under the restrictions of a bunch of different organizations.
**       Although I wrote it completely myself; it is kind of a derivative
**       of a routine I once authored and released under the GPL, so it
**       may fall under the free software foundation's restrictions;
**       it was worked on as a Stanford Univ project, so they claim
**       some rights to it; it was further optimized at work here, so
**       I think this company claims parts of it.  The patents are
**       held by R. Bracewell (the FHT algorithm) and O. Buneman (the
**       trig generator), both at Stanford Univ.
**       If it were up to me, I'd say go do whatever you want with it;
**       but it would be polite to give credit to the following people
**       if you use this anywhere:
**           Euler     - probable inventor of the fourier transform.
**           Gauss     - probable inventor of the FFT.
**           Hartley   - probable inventor of the hartley transform.
**           Buneman   - for a really cool trig generator
**           Mayer(me) - for authoring this particular version and
**                       including all the optimizations in one package.
**       Thanks,
**       Ron Mayer; mayer@acuson.com
**
*/



  static final String fht_version = "Brcwl-Hrtly-Ron-dbld";

  static final double SQRT2_2 = 0.70710678118654752440084436210484;
  static final double SQRT2 = 2 * SQRT2_2;

  public static void fht(double[] fz, int n) {
    double a,b;
    double c1,s1,s2,c2,s3,c3,s4,c4;
    double f0,g0,f1,g1,f2,g2,f3,g3;
    int i,k,k1,k2,k3,k4,kx;
    int fi, fn, gi, ifz = 0;
    int t_lam = 0;
    double[] sinwrk = new double[16],
            coswrk = new double[16],
            halsec = new double[16],
            costab = new double[16],
            sintab = new double[16];

    for (int ij = 0; ij < 16; ij++) {
      sinwrk[ij] = static_sinwrk[ij];
      coswrk[ij] = static_coswrk[ij];
      halsec[ij] = static_halsec[ij];
      costab[ij] = static_costab[ij];
      sintab[ij] = static_sintab[ij];
    }

    for (k1 = 1, k2 = 0; k1 < n; k1++) {
      for (k = n >> 1; !(((k2 ^= k) & k) != 0); k >>= 1) ;
      if (k1 > k2) {
        a = fz[k1];
        fz[k1] = fz[k2];
        fz[k2] = a;
      }
    }
    for (k = 0; (1 << k) < n; k++) ;
    k &= 1;
    if (k == 0) {
      for (fi = ifz, fn = ifz + n; fi < fn; fi += 4) {
        f1 = fz[fi] - fz[fi + 1];
        f0 = fz[fi] + fz[fi + 1];
        f3 = fz[fi + 2] - fz[fi + 3];
        f2 = fz[fi + 2] + fz[fi + 3];
        fz[fi + 2] = (f0 - f2);
        fz[fi + 0] = (f0 + f2);
        fz[fi + 3] = (f1 - f3);
        fz[fi + 1] = (f1 + f3);
      }
    } else {
      for (fi = ifz, fn = ifz + n, gi = fi + 1; fi < fn; fi += 8, gi += 8) {
//	     double s1,c1,s2,c2,s3,c3,s4,c4,g0,f0,f1,g1,f2,g2,f3,g3;
        c1 = fz[fi + 0] - fz[gi + 0];
        s1 = fz[fi + 0] + fz[gi + 0];
        c2 = fz[fi + 2] - fz[gi + 2];
        s2 = fz[fi + 2] + fz[gi + 2];
        c3 = fz[fi + 4] - fz[gi + 4];
        s3 = fz[fi + 4] + fz[gi + 4];
        c4 = fz[fi + 6] - fz[gi + 6];
        s4 = fz[fi + 6] + fz[gi + 6];
        f1 = (s1 - s2);
        f0 = (s1 + s2);
        g1 = (c1 - c2);
        g0 = (c1 + c2);
        f3 = (s3 - s4);
        f2 = (s3 + s4);
        g3 = SQRT2 * c4;
        g2 = SQRT2 * c3;
        fz[fi + 4] = (f0 - f2);
        fz[fi + 0] = (f0 + f2);
        fz[fi + 6] = (f1 - f3);
        fz[fi + 2] = (f1 + f3);
        fz[gi + 4] = (g0 - g2);
        fz[gi + 0] = (g0 + g2);
        fz[gi + 6] = (g1 - g3);
        fz[gi + 2] = (g1 + g3);
      }
    }
    if (n < 16) return;

    do {
//     double s1,c1;
      k += 2;
      k1 = 1 << k;
      k2 = k1 << 1;
      k4 = k2 << 1;
      k3 = k2 + k1;
      kx = k1 >> 1;
      fi = ifz;
      gi = fi + kx;
      fn = ifz + n;
      do {
//	     double g0,f0,f1,g1,f2,g2,f3,g3;
        f1 = fz[fi + 0] - fz[fi + k1];
        f0 = fz[fi + 0] + fz[fi + k1];
        f3 = fz[fi + k2] - fz[fi + k3];
        f2 = fz[fi + k2] + fz[fi + k3];
        fz[fi + k2] = (f0 - f2);
        fz[fi + 0] = (f0 + f2);
        fz[fi + k3] = (f1 - f3);
        fz[fi + k1] = (f1 + f3);
        g1 = fz[gi + 0] - fz[gi + k1];
        g0 = fz[gi + 0] + fz[gi + k1];
        g3 = SQRT2 * fz[k3 + gi];
        g2 = SQRT2 * fz[k2 + gi];
        fz[gi + k2] = (g0 - g2);
        fz[gi + 0] = (g0 + g2);
        fz[gi + k3] = (g1 - g3);
        fz[gi + k1] = (g1 + g3);
        gi += k4;
        fi += k4;
      } while (fi < fn);
//     TRIG_INIT(k,c1,s1);
      {
        for (int ij = 2; ij <= k; ij++) {
          coswrk[ij] = costab[ij];
          sinwrk[ij] = sintab[ij];
        }
        t_lam = 0;
        c1 = 1;
        s1 = 0;
      }
// end trig_init
      for (i = 1; i < kx; i++) {
//	 double c2,s2;
//         TRIG_NEXT(k,c1,s1);
        {
          int ij,jk;
          t_lam++;
          for (ij = 0; !(((1 << ij) & t_lam) != 0); ij++) ;
          ij = k - ij;
          s1 = sinwrk[ij];
          c1 = coswrk[ij];
          if (ij > 1) {
            for (jk = k - ij + 2; ((1 << jk) & t_lam) != 0; jk++) ;
            jk = k - jk;
            sinwrk[ij] = halsec[ij] * (sinwrk[ij - 1] + sinwrk[jk]);
            coswrk[ij] = halsec[ij] * (coswrk[ij - 1] + coswrk[jk]);
          }
        }
// end trig_next
        c2 = c1 * c1 - s1 * s1;
        s2 = 2 * (c1 * s1);
        fn = ifz + n;
        fi = ifz + i;
        gi = ifz + k1 - i;
        do {
//		 double a,b,g0,f0,f1,g1,f2,g2,f3,g3;
          b = s2 * fz[k1 + fi] - c2 * fz[k1 + gi];
          a = c2 * fz[k1 + fi] + s2 * fz[k1 + gi];
          f1 = fz[0 + fi] - a;
          f0 = fz[0 + fi] + a;
          g1 = fz[0 + gi] - b;
          g0 = fz[0 + gi] + b;
          b = s2 * fz[k3 + fi] - c2 * fz[k3 + gi];
          a = c2 * fz[k3 + fi] + s2 * fz[k3 + gi];
          f3 = fz[k2 + fi] - a;
          f2 = fz[k2 + fi] + a;
          g3 = fz[k2 + gi] - b;
          g2 = fz[k2 + gi] + b;
          b = s1 * f2 - c1 * g3;
          a = c1 * f2 + s1 * g3;
          fz[k2 + fi] = f0 - a;
          fz[0 + fi] = f0 + a;
          fz[k3 + gi] = g1 - b;
          fz[k1 + gi] = g1 + b;
          b = c1 * g2 - s1 * f3;
          a = s1 * g2 + c1 * f3;
          fz[k2 + gi] = g0 - a;
          fz[0 + gi] = g0 + a;
          fz[k3 + fi] = f1 - b;
          fz[k1 + fi] = f1 + b;
          gi += k4;
          fi += k4;
        } while (fi < fn);
      }
//     TRIG_RESET(k,c1,s1);
      for (int ij = 0; ij < 16; ij++) {
        sinwrk[ij] = static_sinwrk[ij];
        coswrk[ij] = static_coswrk[ij];
        halsec[ij] = static_halsec[ij];
        costab[ij] = static_costab[ij];
        sintab[ij] = static_sintab[ij];
      }
    } while (k4 < n);
  }


  public static void ifft(int n, double[] real, double[] imag) {
    double a,b,c,d;
    double q,r,s,t;
    int i,j,k;
    fht(real, n);
    fht(imag, n);
    for (i = 1, j = n - 1, k = n / 2; i < k; i++, j--) {
      a = real[i];
      b = real[j];
      q = a + b;
      r = a - b;
      c = imag[i];
      d = imag[j];
      s = c + d;
      t = c - d;
      imag[i] = (s + r) * 0.5;
      imag[j] = (s - r) * 0.5;
      real[i] = (q - t) * 0.5;
      real[j] = (q + t) * 0.5;
    }
  }

  public static void realfft(int n, double[] real) {
    double a,b,c,d;
    int i,j,k;
    fht(real, n);
    for (i = 1, j = n - 1, k = n / 2; i < k; i++, j--) {
      a = real[i];
      b = real[j];
      real[j] = (a - b) * 0.5;
      real[i] = (a + b) * 0.5;
    }
  }

  public static void fft(int n, double[] real, double[] imag) {
    double a,b,c,d;
    double q,r,s,t;
    int i,j,k;
    for (i = 1, j = n - 1, k = n / 2; i < k; i++, j--) {
      a = real[i];
      b = real[j];
      q = a + b;
      r = a - b;
      c = imag[i];
      d = imag[j];
      s = c + d;
      t = c - d;
      real[i] = (q + t) * .5;
      real[j] = (q - t) * .5;
      imag[i] = (s - r) * .5;
      imag[j] = (s + r) * .5;
    }
    fht(real, n);
    fht(imag, n);
  }

  public static void realifft(int n, double[] real) {
    double a,b,c,d;
    int i,j,k;
    for (i = 1, j = n - 1, k = n / 2; i < k; i++, j--) {
      a = real[i];
      b = real[j];
      real[j] = (a - b);
      real[i] = (a + b);
    }
    fht(real, n);
  }

/* >#define double double
>
> double real[140001];
> double imag[140001];
>
>main(argc,argv)
>int argc;
>char **argv;
>{
> int num=0,lop,i,k,j;
> long cycles;
> double ssq=0;
> double scale;
>int status;
> if (argc>1) num=atoi(argv[1]);
> if (num==0) num=256;
> for (i=0;i<num;i++)
>    {
>     real[i]=i;
>     imag[i]=0;
>    }
> scale = 1.0/num;
> lop = 2+256*256/num;
> printf("real-FFT-mayer            (%-6d %6d):",lop,num);
> cycles=clock();
> for (k=0;k<lop;k++)
>     {
>      realfft(num, real);
>      realifft(num, real);
>      for (j=0;j<num*2;j++)  {real[j]*=scale;}
>     }
> printf("%10d CPU us ;",clock());
>
> for (ssq=0,i=0;i<num;i++)
>    ssq+=(real[i]-i)*(real[i]-i);
> printf("ssq errors %#.2g\n",ssq);
>
>} */

/*
>** Please only distribute this with it's associated FHT routine.
>** This algorithm is apparently patented(!) and the code copyrighted.
>** See the comment with the fht routine for more info.
>**   -Thanks,
>**   Ron Mayer
>*/


//#define FHT_SWAP(a,b,t) {(t)=(a);(a)=(b);(b)=(t);}
//#define TRIG_VARS	int t_lam=0;
/*#define TRIG_INIT(k,c,s)					 \
     {								 \
      int i;							 \
      for (i=2 ; i<=k ; i++)					 \
          {coswrk[i]=costab[i];sinwrk[i]=sintab[i];}		 \
      t_lam = 0;					         \
      c = 1;							 \
      s = 0;							 \
     }
#define TRIG_NEXT(k,c,s)					 \
     {								 \
	 int i,j;	                                         \
         (t_lam)++;	  					 \
         for (i=0 ; !((1<<i)&t_lam) ; i++);			 \
         i = k-i;						 \
         s = sinwrk[i];						 \
         c = coswrk[i];						 \
         if (i>1)   						 \
            {	    						 \
             for (j=k-i+2 ; (1<<j)&t_lam ; j++);		 \
             j	       = k - j;					 \
             sinwrk[i] = halsec[i] * (sinwrk[i-1] + sinwrk[j]);  \
             coswrk[i] = halsec[i] * (coswrk[i-1] + coswrk[j]);  \
            }                                                    \
     }
#define TRIG_RESET(k,c,s) */

  static double[] static_halsec =
          {
            0,
            0,
            .54119610014619698439972320536638942006107206337801,
            .50979557910415916894193980398784391368261849190893,
            .50241928618815570551167011928012092247859337193963,
            .50060299823519630134550410676638239611758632599591,
            .50015063602065098821477101271097658495974913010340,
            .50003765191554772296778139077905492847503165398345,
            .50000941253588775676512870469186533538523133757983,
            .50000235310628608051401267171204408939326297376426,
            .50000058827484117879868526730916804925780637276181,
            .50000014706860214875463798283871198206179118093251,
            .50000003676714377807315864400643020315103490883972,
            .50000000919178552207366560348853455333939112569380,
            .50000000229794635411562887767906868558991922348920,
            .50000000057448658687873302235147272458812263401372
          };
  static double[] static_costab =
          {
            .00000000000000000000000000000000000000000000000000,
            .70710678118654752440084436210484903928483593768847,
            .92387953251128675612818318939678828682241662586364,
            .98078528040323044912618223613423903697393373089333,
            .99518472667219688624483695310947992157547486872985,
            .99879545620517239271477160475910069444320361470461,
            .99969881869620422011576564966617219685006108125772,
            .99992470183914454092164649119638322435060646880221,
            .99998117528260114265699043772856771617391725094433,
            .99999529380957617151158012570011989955298763362218,
            .99999882345170190992902571017152601904826792288976,
            .99999970586288221916022821773876567711626389934930,
            .99999992646571785114473148070738785694820115568892,
            .99999998161642929380834691540290971450507605124278,
            .99999999540410731289097193313960614895889430318945,
            .99999999885102682756267330779455410840053741619428
          };
  static double[] static_sintab =
          {
            1.0000000000000000000000000000000000000000000000000,
            .70710678118654752440084436210484903928483593768846,
            .38268343236508977172845998403039886676134456248561,
            .19509032201612826784828486847702224092769161775195,
            .09801714032956060199419556388864184586113667316749,
            .04906767432741801425495497694268265831474536302574,
            .02454122852291228803173452945928292506546611923944,
            .01227153828571992607940826195100321214037231959176,
            .00613588464915447535964023459037258091705788631738,
            .00306795676296597627014536549091984251894461021344,
            .00153398018628476561230369715026407907995486457522,
            .00076699031874270452693856835794857664314091945205,
            .00038349518757139558907246168118138126339502603495,
            .00019174759731070330743990956198900093346887403385,
            .00009587379909597734587051721097647635118706561284,
            .00004793689960306688454900399049465887274686668768
          };
  static double[] static_coswrk =
          {
            .00000000000000000000000000000000000000000000000000,
            .70710678118654752440084436210484903928483593768847,
            .92387953251128675612818318939678828682241662586364,
            .98078528040323044912618223613423903697393373089333,
            .99518472667219688624483695310947992157547486872985,
            .99879545620517239271477160475910069444320361470461,
            .99969881869620422011576564966617219685006108125772,
            .99992470183914454092164649119638322435060646880221,
            .99998117528260114265699043772856771617391725094433,
            .99999529380957617151158012570011989955298763362218,
            .99999882345170190992902571017152601904826792288976,
            .99999970586288221916022821773876567711626389934930,
            .99999992646571785114473148070738785694820115568892,
            .99999998161642929380834691540290971450507605124278,
            .99999999540410731289097193313960614895889430318945,
            .99999999885102682756267330779455410840053741619428
          };
  static double[] static_sinwrk =
          {
            1.0000000000000000000000000000000000000000000000000,
            .70710678118654752440084436210484903928483593768846,
            .38268343236508977172845998403039886676134456248561,
            .19509032201612826784828486847702224092769161775195,
            .09801714032956060199419556388864184586113667316749,
            .04906767432741801425495497694268265831474536302574,
            .02454122852291228803173452945928292506546611923944,
            .01227153828571992607940826195100321214037231959176,
            .00613588464915447535964023459037258091705788631738,
            .00306795676296597627014536549091984251894461021344,
            .00153398018628476561230369715026407907995486457522,
            .00076699031874270452693856835794857664314091945205,
            .00038349518757139558907246168118138126339502603495,
            .00019174759731070330743990956198900093346887403385,
            .00009587379909597734587051721097647635118706561284,
            .00004793689960306688454900399049465887274686668768
          };

}
