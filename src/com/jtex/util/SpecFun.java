package com.jtex.util;

//package ca.dal.hcd;

public class SpecFun
{

    public static double lgamma(double y)
    {
	if (Double.isNaN(y))
	    return y;

	// Initialized data 

	final double XBIG = 2.55e305;
	final double EPS = 2.22e-16;
	final double FRTBIG = 2.25e76;
	final double d1 = -0.5772156649015328605195174;
	final double[] p1 = { 4.945235359296727046734888,
		201.8112620856775083915565, 2290.838373831346393026739,
		11319.67205903380828685045, 28557.24635671635335736389,
		38484.96228443793359990269, 26377.48787624195437963534,
		7225.813979700288197698961 };
	final double[] q1 = { 67.48212550303777196073036,
		1113.332393857199323513008, 7738.757056935398733233834,
		27639.87074403340708898585, 54993.10206226157329794414,
		61611.22180066002127833352, 36351.27591501940507276287,
		8785.536302431013170870835 };
	final double d2 = 0.4227843350984671393993777;
	final double[] p2 = { 4.974607845568932035012064,
		542.4138599891070494101986, 15506.93864978364947665077,
		184793.2904445632425417223, 1088204.76946882876749847,
		3338152.967987029735917223, 5106661.678927352456275255,
		3074109.054850539556250927 };
	final double[] q2 = { 183.0328399370592604055942,
		7765.049321445005871323047, 133190.3827966074194402448,
		1136705.821321969608938755, 5267964.117437946917577538,
		13467014.54311101692290052, 17827365.30353274213975932,
		9533095.591844353613395747 };
	final double d4 = 1.791759469228055000094023;
	final double[] p4 = { 14745.02166059939948905062,
		2426813.369486704502836312, 121475557.4045093227939592,
		2663432449.630976949898078, 29403789566.34553899906876,
		170266573776.5398868392998, 492612579337.743088758812,
		560625185622.3951465078242 };
	final double[] q4 = { 2690.530175870899333379843,
		639388.5654300092398984238, 41355999.30241388052042842,
		1120872109.61614794137657, 14886137286.78813811542398,
		101680358627.2438228077304, 341747634550.7377132798597,
		446315818741.9713286462081 };
	final double[] c = { -0.001910444077728, 8.4171387781295e-4,
		-5.952379913043012e-4, 7.93650793500350248e-4,
		-0.002777777777777681622553, 0.08333333333333333331554247,
		0.0057083835261 };
	final double PNT68 = 0.6796875;
	final double SQRTPI = 0.9189385332046727417803297;

	// Local variables 
	double xden, corr, xnum;
	double xm1, xm2, xm4, res, ysq;

// ---------------------------------------------------------------------- 
//
// This routine calculates the LOG(GAMMA) function for a positive real 
//   argument X.  Computation is based on an algorithm outlined in 
//   references 1 and 2.  The program uses rational functions that 
//   theoretically approximate LOG(GAMMA) to at least 18 significant 
//   decimal digits.  The approximation for X > 12 is from reference 
//   3, while approximations for X < 12.0 are similar to those in 
//   reference 1, but are unpublished.  The accuracy achieved depends 
//   on the arithmetic system, the compiler, the intrinsic functions, 
//   and proper selection of the machine-dependent constants. 
//
//
// ********************************************************************* 
// ********************************************************************* 
//
// Explanation of machine-dependent constants 
//
// beta   - radix for the floating-point representation 
// maxexp - the smallest positive power of beta that overflows 
// XBIG   - largest argument for which LN(GAMMA(X)) is representable 
//          in the machine, i.e., the solution to the equation 
//                  LN(GAMMA(XBIG)) = beta**maxexp 
// XINF   - largest machine representable floating-point number; 
//          approximately beta**maxexp. 
// EPS    - The smallest positive floating-point number such that 
//          1.0+EPS > 1.0 
// FRTBIG - Rough estimate of the fourth root of XBIG 
//
//
//     Approximate values for some important machines are: 
//
//                           beta      maxexp         XBIG 
//
// CRAY-1        (S.P.)        2        8191       9.62E+2461 
// Cyber 180/855 
//   under NOS   (S.P.)        2        1070       1.72E+319 
// IEEE (IBM/XT, 
//   SUN, etc.)  (S.P.)        2         128       4.08E+36 
// IEEE (IBM/XT, 
//   SUN, etc.)  (D.P.)        2        1024       2.55D+305 
// IBM 3033      (D.P.)       16          63       4.29D+73 
// VAX D-Format  (D.P.)        2         127       2.05D+36 
// VAX G-Format  (D.P.)        2        1023       1.28D+305 
//
//
//                           XINF        EPS        FRTBIG 
//
// CRAY-1        (S.P.)   5.45E+2465   7.11E-15    3.13E+615 
// Cyber 180/855 
//   under NOS   (S.P.)   1.26E+322    3.55E-15    6.44E+79 
// IEEE (IBM/XT, 
//   SUN, etc.)  (S.P.)   3.40E+38     1.19E-7     1.42E+9 
// IEEE (IBM/XT, 
//   SUN, etc.)  (D.P.)   1.79D+308    2.22D-16    2.25D+76 
// IBM 3033      (D.P.)   7.23D+75     2.22D-16    2.56D+18 
// VAX D-Format  (D.P.)   1.70D+38     1.39D-17    1.20D+9 
// VAX G-Format  (D.P.)   8.98D+307    1.11D-16    1.89D+76 
//
// ************************************************************** 
// ************************************************************** 
//
// Error returns 
//
//  The program returns the value XINF for X <= 0.0 or when
//     overflow would occur.  The computation is believed to 
//     be free of underflow and overflow. 
//
//
// References: 
//
//  1) W. J. Cody and K. E. Hillstrom, 'Chebyshev Approximations for 
//     the Natural Logarithm of the Gamma Function,' Math. Comp. 21, 
//     1967, pp. 198-203. 
//
//  2) K. E. Hillstrom, ANL/AMD Program ANLC366S, DGAMMA/DLGAMA, May, 
//     1969. 
//
//  3) Hart, Et. Al., Computer Approximations, Wiley and sons, New 
//     York, 1968. 
//
//
//  Authors: W. J. Cody and L. Stoltz 
//           Argonne National Laboratory 
//
//  Latest modification: June 16, 1988 
//
//  Translated to C from FORTRAN and adapted for Java
//  by Michael Kiefte, Dalhousie University, June 4, 2002.
//
// ---------------------------------------------------------------------- 

	if (y > 0.0 && y <= XBIG) {
	    if (y <= EPS) {
		res = -Math.log(y);
	    } else if (y <= 1.5) {
// ---------------------------------------------------------------------- 
//  EPS < X <= 1.5 
// ---------------------------------------------------------------------- 
		if (y < PNT68) {
		    corr = -Math.log(y);
		    xm1 = y;
		} else {
		    corr = 0.0;
		    xm1 = y - 1.0;
		}
		if (y <= 0.5 || y >= PNT68) {
		    xden = 1.0;
		    xnum = 0.0;
		    for (int i = 0; i < 8; ++i) {
			xnum = xnum * xm1 + p1[i];
			xden = xden * xm1 + q1[i];
		    }
		    res = corr + xm1 * (d1 + xm1 * (xnum / xden));
		} else {
		    xm2 = y - 1.0;
		    xden = 1.0;
		    xnum = 0.0;
		    for (int i = 0; i < 8; ++i) {
			xnum = xnum * xm2 + p2[i];
			xden = xden * xm2 + q2[i];
		    }
		    res = corr + xm2 * (d2 + xm2 * (xnum / xden));
		}
	    } else if (y <= 4.0) {
// ---------------------------------------------------------------------- 
//  1.5 < X <= 4.0 
// ---------------------------------------------------------------------- 
		xm2 = y - 2.0;
		xden = 1.0;
		xnum = 0.0;
		for (int i = 0; i < 8; ++i) {
		    xnum = xnum * xm2 + p2[i];
		    xden = xden * xm2 + q2[i];
		}
		res = xm2 * (d2 + xm2 * (xnum / xden));
	    } else if (y <= 12.0) {
// ---------------------------------------------------------------------- 
//  4.0 < X <= 12.0 
// ---------------------------------------------------------------------- 
		xm4 = y - 4.0;
		xden = -1.0;
		xnum = 0.0;
		for (int i = 0; i < 8; ++i) {
		    xnum = xnum * xm4 + p4[i];
		    xden = xden * xm4 + q4[i];
		}
		res = d4 + xm4 * (xnum / xden);
	    } else {
// ---------------------------------------------------------------------- 
//  Evaluate for argument >= 12.0, 
// ---------------------------------------------------------------------- 
		res = 0.0;
		if (y <= FRTBIG) {
		    res = c[6];
		    ysq = y * y;
		    for (int i = 0; i < 6; ++i) {
			res = res / ysq + c[i];
		    }
		}
		res /= y;
		corr = Math.log(y);
		res = res + SQRTPI - 0.5 * corr;
		res += y * (corr - 1.0);
	    }
	} else {
// ---------------------------------------------------------------------- 
//  Return for bad arguments 
// ---------------------------------------------------------------------- 
	    res = Double.POSITIVE_INFINITY;
	}
// ---------------------------------------------------------------------- 
//  Final adjustments and return 
// ---------------------------------------------------------------------- 
	return res;
    } // dlgama 

    public static double daw(double x)
    {
	if (Double.isNaN(x))
	    return x;

	// Initialized data 

	final double[] p1 = { -2.6902039878870478241e-12,
		4.18572065374337710778e-10, -1.34848304455939419963e-8,
		9.28264872583444852976e-7, -1.23877783329049120592e-5,
		4.07205792429155826266e-4, -0.00284388121441008500446,
		0.0470139022887204722217, -0.138868086253931995101,
		1.00000000000000000004 };
	final double[] q1 = { 1.71257170854690554214e-10,
		1.19266846372297253797e-8, 4.32287827678631772231e-7,
		1.03867633767414421898e-5, 1.7891096528424624934e-4,
		0.00226061077235076703171, 0.0207422774641447644725,
		0.132212955897210128811, 0.527798580412734677256, 1. };
	final double[] p2 = { -1.7095380470085549493,
		-37.9258977271042880786, 26.1935631268825992835,
		12.5808703738951251885, -22.7571829525075891337,
		4.56604250725163310122, -7.3308008989640287075,
		46.5842087940015295573, -17.3717177843672791149,
		0.500260183622027967838 };
	final double[] q2 = { 1.82180093313514478378, 1100.67081034515532891,
		-7.08465686676573000364, 453.642111102577727153,
		40.6209742218935689922, 302.890110610122663923,
		170.641269745236227356, 951.190923960381458747,
		0.206522691539642105009 };
	final double[] p3 = { -4.55169503255094815112,
		-18.6647123338493852582, -7.36315669126830526754,
		-66.8407240337696756838, 48.450726508149145213,
		26.9790586735467649969, -33.5044149820592449072,
		7.50964459838919612289, -1.48432341823343965307,
		0.499999810924858824981 };
	final double[] q3 = { 44.7820908025971749852, 99.8607198039452081913,
		14.0238373126149385228, 3488.17758822286353588,
		-9.18871385293215873406, 1240.18500009917163023,
		-68.8024952504512254535, -2.3125157538514514307,
		0.250041492369922381761 };
	final double[] p4 = { -8.11753647558432685797,
		-38.404388247745445343, -22.3787669028751886675,
		-28.8301992467056105854, -5.99085540418222002197,
		-11.3867365736066102577, -6.5282872752698074159,
		-4.50002293000355585708, -2.50000000088955834952,
		0.5000000000000004884 };
	final double[] q4 = { 269.382300417238816428, 50.4198958742465752861,
		61.1539671480115846173, 208.210246935564547889,
		19.7325365692316183531, -12.2097010558934838708,
		-6.99732735041547247161, -2.49999970104184464568,
		0.749999999999027092188 };
	final double XSMALL = 1.05e-8;
	final double XLARGE = 9.49e7;
	final double XMAX = 2.24e307;

	// Local variables 
	double frac, sump, sumq;
	double y, w2;

// ---------------------------------------------------------------------- 
//
// This function program evaluates Dawson's integral, 
//
//                       2  / x   2 
//                     -x   |    t 
//             F(x) = e     |   e    dt 
//                          | 
//                          / 0 
//
//   for a real argument x. 
//
//   The calling sequence for this function is 
//
//                   Y=DAW(X) 
//
//   The main computation uses rational Chebyshev approximations 
//   published in Math. Comp. 24, 171-178 (1970) by Cody, Paciorek 
//   and Thacher.  This transportable program is patterned after the 
//   machine-dependent FUNPACK program DDAW(X), but cannot match that 
//   version for efficiency or accuracy.  This version uses rational 
//   approximations that are theoretically accurate to about 19 
//   significant decimal digits.  The accuracy achieved depends on the 
//   arithmetic system, the compiler, the intrinsic functions, and 
//   proper selection of the machine-dependent constants. 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Explanation of machine-dependent constants 
//
//   XINF   = largest positive machine number 
//   XMIN   = the smallest positive machine number. 
//   EPS    = smallest positive number such that 1+eps > 1. 
//            Approximately  beta**(-p), where beta is the machine 
//            radix and p is the number of significant base-beta 
//            digits in a floating-point number. 
//   XMAX   = absolute argument beyond which DAW(X) underflows. 
//            XMAX = min(0.5/xmin, xinf). 
//   XSMALL = absolute argument below DAW(X)  may be represented 
//            by X.  We recommend XSMALL = sqrt(eps). 
//   XLARGE = argument beyond which DAW(X) may be represented by 
//            1/(2x).  We recommend XLARGE = 1/sqrt(eps). 
//
//     Approximate values for some important machines are 
//
//                        beta  p     eps     xmin       xinf 
//
//  CDC 7600      (S.P.)    2  48  7.11E-15  3.14E-294  1.26E+322 
//  CRAY-1        (S.P.)    2  48  7.11E-15  4.58E-2467 5.45E+2465 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (S.P.)    2  24  1.19E-07  1.18E-38   3.40E+38 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (D.P.)    2  53  1.11D-16  2.23E-308  1.79D+308 
//  IBM 3033      (D.P.)   16  14  1.11D-16  5.40D-79   7.23D+75 
//  VAX 11/780    (S.P.)    2  24  5.96E-08  2.94E-39   1.70E+38 
//                (D.P.)    2  56  1.39D-17  2.94D-39   1.70D+38 
//   (G Format)   (D.P.)    2  53  1.11D-16  5.57D-309  8.98D+307 
//
//                         XSMALL     XLARGE     XMAX 
//
//  CDC 7600      (S.P.)  5.96E-08   1.68E+07  1.59E+293 
//  CRAY-1        (S.P.)  5.96E-08   1.68E+07  5.65E+2465 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (S.P.)  2.44E-04   4.10E+03  4.25E+37 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (D.P.)  1.05E-08   9.49E+07  2.24E+307 
//  IBM 3033      (D.P.)  3.73D-09   2.68E+08  7.23E+75 
//  VAX 11/780    (S.P.)  2.44E-04   4.10E+03  1.70E+38 
//                (D.P.)  3.73E-09   2.68E+08  1.70E+38 
//   (G Format)   (D.P.)  1.05E-08   9.49E+07  8.98E+307 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Error Returns 
//
//  The program returns 0.0 for |X| > XMAX. 
//
//  Author: W. J. Cody 
//          Mathematics and Computer Science Division 
//          Argonne National Laboratory 
//          Argonne, IL 60439 
//
//  Latest modification: June 15, 1988 
//
//  Translated to C from FORTRAN and adapted for Java
//  by Michael Kiefte, Dalhousie University, June 4, 2002.
//
// ---------------------------------------------------------------------- 

	if (Math.abs(x) > XLARGE) {
	    if (Math.abs(x) <= XMAX) {
		return 0.5 / x;
	    } else {
		return 0.0;
	    }
	} else if (Math.abs(x) < XSMALL) {
	    return x;
	} else {
	    y = x * x;
	    if (y < 6.25) {
// ---------------------------------------------------------------------- 
//  ABS(X) < 2.5 
// ---------------------------------------------------------------------- 
		sump = p1[0];
		sumq = q1[0];
		for (int i = 1; i < 10; ++i) {
		    sump = sump * y + p1[i];
		    sumq = sumq * y + q1[i];
		}
		return x * sump / sumq;
	    } else if (y < 12.25) {
// ---------------------------------------------------------------------- 
//  2.5 <= ABS(X) < 3.5 
// ---------------------------------------------------------------------- 
		frac = 0.0;
		for (int i = 0; i < 9; ++i) {
		    frac = q2[i] / (p2[i] + y + frac);
		}
		return (p2[9] + frac) / x;
	    } else if (y < 25.0) {
// ---------------------------------------------------------------------- 
//  3.5 <= ABS(X) < 5.0 
// --------------------------------------------------------------------- 
		frac = 0.0;
		for (int i = 0; i < 9; ++i) {
		    frac = q3[i] / (p3[i] + y + frac);
		}
		return (p3[9] + frac) / x;
	    } else {
// ---------------------------------------------------------------------- 
//  5.0 <= ABS(X) <= XLARGE 
// ------------------------------------------------------------------ 
		w2 = 1.0 / x / x;
		frac = 0.0;
		for (int i = 0; i < 9; ++i) {
		    frac = q4[i] / (p4[i] + y + frac);
		}
		frac = p4[9] + frac;
		return (0.5 + 0.5 * w2 * frac) / x;
	    }
	}
    } // daw 

    private static double calcei(double x, int flag)
    {
	if (Double.isNaN(x))
	    return x;

	// Initialized data 

	final double EXP40 = 235385266837019985.41;
	final double X01 = 381.5;
	final double X11 = 1024.0;
	final double X02 = -5.1182968633365538008e-5;
	final double X0 = 0.37250741078136663466;
	final double XMAX = 716.351;
	final double XBIG = 701.84;
	final double[] a = { 116.69552669734461083368,
		2150.0672908092918123209, 15924.175980637303639884,
		89904.972007457256553251, 150260.59476436982420737,
		-148151.02102575750838086, 5.019678518543984379102 };
	final double[] b = { 40.205465640027706061433,
		750.43163907103936624165, 8125.8035174768735759855,
		52440.529172056355429883, 184340.70063353677359298,
		256664.93484897117319268 };
	final double[] c = { 0.3828573121022477169108,
		11.07326627786831743809, 72.46689782858597021199,
		170.0632978311516129328, 169.8106763764238382705,
		76.33628843705946890896, 14.87967702840464066613,
		0.9999989642347613068437, 1.737331760720576030932e-8 };
	final double[] d = { 0.08258160008564488034698,
		4.34483633550928208336, 46.62179610356861756812,
		177.5728186717289799677, 295.3136335677908517423,
		234.2573504717625153053, 90.21658450529372642314,
		15.87964570758947927903, 1.0 };
	final double[] e = { 132.76881505637444622987,
		35846.198743996904308695, 172833.75773777593926828,
		261814.54937205639647381, 175032.73087497081314708,
		59346.841538837119172356, 10816.852399095915622498,
		1061.1777263550331766871, 52.199632588522572481039,
		0.99999999999999999087819 };
	final double[] f = { 39147.856245556345627078,
		259897.62083608489777411, 559037.5621002286400338,
		546168.42050691155735758, 278581.34710520842139357,
		79231.787945279043698718, 12842.808586627297365998,
		1163.5769915320848035459, 54.199632588522559414924, 1.0 };
	final double[] plg = { -24.562334077563243311, 236.42701335621505212,
		-549.89956895857911039, 356.87548468071500413 };
	final double[] qlg = { -35.553900764052419184, 194.00230218539473193,
		-334.42903192607538956, 178.43774234035750207 };
	final double[] p = { -12.96370260247483002859,
		-1283.1220659262000678155, -14287.072500197005777376,
		-1429984.1572091610380064, -313986.6086424726586205,
		-353778096.944311334848, 319843542.35237738511048,
		-25301823984.599019348858, 12177698136.19959467758,
		-208290406668.0249712094 };
	final double[] q = { 76.88671875, -5564.8470543369082846819,
		194184.69440759880361415, -4264843.4812177161405483,
		64698830.956576428587653, -701085687.74215954065376,
		5422961798.4472955011862, -28986272696.554495342658,
		98900934262.481749439886, -89673749185.755048616855 };
	final double[] r = { -2.645677793077147237806,
		-2.378372882815725244124, -24.2110695698065351155,
		10.52976392459015155422, 19.45603779539281810439,
		-30.15761863840593359165, 11.20011024227297451523,
		-3.988850730390541057912, 9.565134591978630774217,
		0.9981193787537396413219 };
	final double[] s = { 1.598517957704779356479e-4,
		4.64418593258328694265, 369.7412299772985940785,
		-8.791401054875438925029, 760.8194509086645763123,
		28.52397548119248700147, 473.1097187816050252967,
		-236.9210235636181001661, 1.24988482271244789144 };
	final double[] p1 = { -1.647721172463463140042,
		-18.60092121726437582253, -10.00641913989284829961,
		-21.05740799548040450394, -0.9134835699998742552432,
		-33.23612579343962284333, 24.95487730402059440626,
		26.52575818452799819855, -1.845086232391278674524,
		0.9999933106160568739091 };
	final double[] q1 = { 97.9240359921729029684,
		64.03800405352415551324, 59.94932325667407355255,
		253.8819315630708031713, 44.29413178337928401161,
		1192.832423968601006985, 199.1004470817742470726,
		-10.93556195391091143924, 1.001533852045342697818 };
	final double[] p2 = { 175.33880126546597239, -223.12767077763240955,
		-18.1949664929868906455, -27.979852862430538934,
		-7.63147701620253630855, -15.2856623636929636839,
		-7.06810977895029358836, -5.00006640413131002475,
		-3.00000000320981265753, 1.00000000000000485503 };
	final double[] q2 = { 39784.597716741472084, 3.97277109100414518365,
		137.790390235747998793, 117.179220502086455287,
		70.4831847180424675988, -12.0187763547154743238,
		-7.99243595776339741065, -2.99999894040324959612,
		1.99999999999048104167 };

	// Local variables 
	double frac, sump, sumq;
	double t, w, y, ei, ysq, xmx0;
	double[] px = new double[10];
	double[] qx = new double[10];

// ---------------------------------------------------------------------- 
//
// This packet computes the exponential integrals Ei(x), 
//  E1(x), and  exp(-x)*Ei(x)  for real arguments  x  where 
//
//           integral (from t=-infinity to t=x) (exp(t)/t),  x > 0, 
//  Ei(x) = 
//          -integral (from t=-x to t=infinity) (exp(t)/t),  x < 0, 
//
//  and where the first integral is a principal value integral. 
//  The packet contains three function type subprograms: EI, EONE, 
//  and EXPEI;  and one subroutine type subprogram: CALCEI.  The 
//  calling statements for the primary entries are 
//
//                 Y = EI(X),            where  X != 0, 
//
//                 Y = EONE(X),          where  X > 0, 
//  and 
//                 Y = EXPEI(X),         where  X != 0, 
//
//  and where the entry points correspond to the functions Ei(x), 
//  E1(x), and exp(-x)*Ei(x), respectively.  The routine CALCEI 
//  is intended for internal packet use only, all computations within 
//  the packet being concentrated in this routine.  The function 
//  subprograms invoke CALCEI with the statement 
//         RESULT = CALCEI(ARG,INT) 
//  where the parameter usage is as follows 
//
//     Function                  Parameters for CALCEI 
//       Call               ARG            RESULT          INT 
//
//      EI(X)              X != 0          Ei(X)            1 
//      EONE(X)            X > 0          -Ei(-X)           2 
//      EXPEI(X)           X != 0          exp(-X)*Ei(X)    3 
//
//  The main computation involves evaluation of rational Chebyshev 
//  approximations published in Math. Comp. 22, 641-649 (1968), and 
//  Math. Comp. 23, 289-303 (1969) by Cody and Thacher.  This 
//  transportable program is patterned after the machine-dependent 
//  FUNPACK packet  NATSEI,  but cannot match that version for 
//  efficiency or accuracy.  This version uses rational functions 
//  that theoretically approximate the exponential integrals to 
//  at least 18 significant decimal digits.  The accuracy achieved 
//  depends on the arithmetic system, the compiler, the intrinsic 
//  functions, and proper selection of the machine-dependent 
//  constants. 
//
//
// ******************************************************************* 
// ******************************************************************* 
//
// Explanation of machine-dependent constants 
//
//   beta = radix for the floating-point system. 
//   minexp = smallest representable power of beta. 
//   maxexp = smallest power of beta that overflows. 
//   XBIG = largest argument acceptable to EONE; solution to 
//          equation: 
//                     exp(-x)/x * (1 + 1/x) = beta ** minexp. 
//   XINF = largest positive machine number; approximately 
//                     beta ** maxexp 
//   XMAX = largest argument acceptable to EI; solution to 
//          equation:  exp(x)/x * (1 + 1/x) = beta ** maxexp. 
//
//     Approximate values for some important machines are: 
//
//                           beta      minexp      maxexp 
//
//  CRAY-1        (S.P.)       2       -8193        8191 
//  Cyber 180/185 
//    under NOS   (S.P.)       2        -975        1070 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (S.P.)       2        -126         128 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (D.P.)       2       -1022        1024 
//  IBM 3033      (D.P.)      16         -65          63 
//  VAX D-Format  (D.P.)       2        -128         127 
//  VAX G-Format  (D.P.)       2       -1024        1023 
//
//                           XBIG       XINF       XMAX 
//
//  CRAY-1        (S.P.)    5670.31  5.45E+2465   5686.21 
//  Cyber 180/185 
//    under NOS   (S.P.)     669.31  1.26E+322     748.28 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (S.P.)      82.93  3.40E+38       93.24 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (D.P.)     701.84  1.79D+308     716.35 
//  IBM 3033      (D.P.)     175.05  7.23D+75      179.85 
//  VAX D-Format  (D.P.)      84.30  1.70D+38       92.54 
//  VAX G-Format  (D.P.)     703.22  8.98D+307     715.66 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Error returns 
//
//  The following table shows the types of error that may be 
//  encountered in this routine and the function value supplied 
//  in each case. 
//
//       Error       Argument       Function values for 
//                    Range       EI      EXPEI     EONE 
//
//     UNDERFLOW  (-)X > XBIG      0        -         0 
//     OVERFLOW      X >= XMAX    XINF      -         - 
//     ILLEGAL X      X = 0       -XINF    -XINF     XINF 
//     ILLEGAL X      X < 0        -        -     USE ABS(X) 
//
//  Author: W. J. Cody 
//          Mathematics abd Computer Science Division 
//          Argonne National Laboratory 
//          Argonne, IL 60439 
//
//  Latest modification: September 9, 1988 
//
//  Translated to C from FORTRAN and adapted for Java
//  by Michael Kiefte, Dalhousie University, June 5, 2002.
//
// ---------------------------------------------------------------------- 

	if (x == 0.0) {
	    ei = Double.NEGATIVE_INFINITY;
	    if (flag == 2) {
		ei = -ei;
	    }
	} else if (x < 0.0 || flag == 2) {
// ---------------------------------------------------------------------- 
// Calculate EI for negative argument or for E1. 
// ---------------------------------------------------------------------- 
	    y = Math.abs(x);
	    if (y <= 1.0) {
		sump = a[6] * y + a[0];
		sumq = y + b[0];
		for (int i = 1; i < 6; ++i) {
		    sump = sump * y + a[i];
		    sumq = sumq * y + b[i];
		}
		ei = Math.log(y) - sump / sumq;
		if (flag == 3) {
		    ei *= Math.exp(y);
		}
	    } else if (y <= 4.0) {
		w = 1.0 / y;
		sump = c[0];
		sumq = d[0];
		for (int i = 1; i < 9; ++i) {
		    sump = sump * w + c[i];
		    sumq = sumq * w + d[i];
		}
		ei = -sump / sumq;
		if (flag != 3) {
		    ei *= Math.exp(-y);
		}
	    } else {
		if (y > XBIG && flag < 3) {
		    ei = 0.0;
		} else {
		    w = 1.0 / y;
		    sump = e[0];
		    sumq = f[0];
		    for (int i = 1; i < 10; ++i) {
			sump = sump * w + e[i];
			sumq = sumq * w + f[i];
		    }
		    ei = -w * (1.0 - w * sump / sumq);
		    if (flag != 3) {
			ei *= Math.exp(-y);
		    }
		}
	    }
	    if (flag == 2) {
		ei = -ei;
	    }
	} else if (x < 6.0) {
// ---------------------------------------------------------------------- 
//  To improve conditioning, rational approximations are expressed 
//    in terms of Chebyshev polynomials for 0 <= X < 6, and in 
//    continued fraction form for larger X. 
// ---------------------------------------------------------------------- 
	    t = 2.0 * x;
	    t = t / 3.0 - 2.0;
	    px[0] = qx[0] = 0.0;
	    px[1] = p[0];
	    qx[1] = q[0];
	    for (int i = 1; i < 9; ++i) {
		px[i + 1] = t * px[i] - px[i - 1] + p[i];
		qx[i + 1] = t * qx[i] - qx[i - 1] + q[i];
	    }
	    sump = 0.5 * t * px[9] - px[8] + p[9];
	    sumq = 0.5 * t * qx[9] - qx[8] + q[9];
	    frac = sump / sumq;
	    xmx0 = x - X01 / X11 - X02;
	    if (Math.abs(xmx0) >= 0.037) {
		ei = Math.log(x / X0) + xmx0 * frac;
		if (flag == 3) {
		    ei = Math.exp(-x) * ei;
		}
	    } else {
// ---------------------------------------------------------------------- 
// Special approximation to  ln(X/X0)  for X close to X0 
// ---------------------------------------------------------------------- 
		y = xmx0 / (x + X0);
		ysq = y * y;
		sump = plg[0];
		sumq = ysq + qlg[0];
		for (int i = 1; i < 4; ++i) {
		    sump = sump * ysq + plg[i];
		    sumq = sumq * ysq + qlg[i];
		}
		ei = (sump / (sumq * (x + X0)) + frac) * xmx0;
		if (flag == 3) {
		    ei *= Math.exp(-x);
		}
	    }
	} else if (x < 12.0) {
	    frac = 0.0;
	    for (int i = 0; i < 9; ++i) {
		frac = s[i] / (r[i] + x + frac);
	    }
	    ei = (r[9] + frac) / x;
	    if (flag != 3) {
		ei *= Math.exp(x);
	    }
	} else if (x <= 24.0) {
	    frac = 0.0;
	    for (int i = 0; i < 9; ++i) {
		frac = q1[i] / (p1[i] + x + frac);
	    }
	    ei = (p1[9] + frac) / x;
	    if (flag != 3) {
		ei *= Math.exp(x);
	    }
	} else {
	    if (x >= XMAX && flag < 3) {
		ei = Double.POSITIVE_INFINITY;
	    } else {
		y = 1.0 / x;
		frac = 0.0;
		for (int i = 0; i < 9; ++i) {
		    frac = q2[i] / (p2[i] + x + frac);
		}
		frac = p2[9] + frac;
		ei = y + y * y * frac;
		if (flag != 3) {
		    if (x <= XMAX - 24.0) {
			ei *= Math.exp(x);
		    } else {
// ---------------------------------------------------------------------- 
// Calculation reformulated to avoid premature overflow 
// ---------------------------------------------------------------------- 
			ei = ei * Math.exp(x - 40.0) * EXP40;
		    }
		}
	    }
	}
	return ei;
    } // calcei 

    public static double ei(double x)
    {
// -------------------------------------------------------------------- 
//
// This function program computes approximate values for the 
//   exponential integral  Ei(x), where  x  is real. 
//
//  Author: W. J. Cody 
//
//  Latest modification: January 12, 1988 
//
// -------------------------------------------------------------------- 

	return calcei(x, 1);
    } // ei 

    public static double expei(double x)
    {
// -------------------------------------------------------------------- 
//
// This function program computes approximate values for the 
//   function  exp(-x) * Ei(x), where  Ei(x)  is the exponential 
//   integral, and  x  is real. 
//
//  Author: W. J. Cody 
//
//  Latest modification: January 12, 1988 
//
// -------------------------------------------------------------------- 

	return calcei(x, 3);
    } // expei 

    public static double eone(double x)
    {
// -------------------------------------------------------------------- 
//
// This function program computes approximate values for the 
//   exponential integral E1(x), where  x  is real. 
//
//  Author: W. J. Cody 
//
//  Latest modification: January 12, 1988 
//
// -------------------------------------------------------------------- 

	return calcei(x, 2);
    } // eone 

    private static double calerf(double x, int jint)
    {
	if (Double.isNaN(x))
	    return x;

	double result;

	// Initialized data 

	final double XNEG = -26.628;
	final double XSMALL = 1.11e-16;
	final double XBIG = 26.543;
	final double XHUGE = 6.71e7;
	final double XMAX = 2.53e307;
	final double[] a = { 3.1611237438705656, 113.864154151050156,
		377.485237685302021, 3209.37758913846947,
		0.185777706184603153 };
	final double[] b = { 23.6012909523441209, 244.024637934444173,
		1282.61652607737228, 2844.23683343917062 };
	final double[] c = { 0.564188496988670089, 8.88314979438837594,
		66.1191906371416295, 298.635138197400131, 881.95222124176909,
		1712.04761263407058, 2051.07837782607147, 1230.33935479799725,
		2.15311535474403846e-8 };
	final double[] d = { 15.7449261107098347, 117.693950891312499,
		537.181101862009858, 1621.38957456669019, 3290.79923573345963,
		4362.61909014324716, 3439.36767414372164, 1230.33935480374942 };
	final double[] p = { 0.305326634961232344, 0.360344899949804439,
		0.125781726111229246, 0.0160837851487422766,
		6.58749161529837803e-4, 0.0163153871373020978 };
	final double[] q = { 2.56852019228982242, 1.87295284992346047,
		0.527905102951428412, 0.0605183413124413191,
		0.00233520497626869185 };
	final double SQRPI = 0.56418958354775628695;
	final double THRESH = 0.46875;

	// Local variables 
	double xden, xnum;
	double y, del, ysq;

// ------------------------------------------------------------------ 
//
// This packet evaluates  erf(x),  erfc(x),  and  exp(xx)*erfc(x) 
//   for a real argument  x.  It contains three FUNCTION type 
//   subprograms: ERF, ERFC, and ERFCX (or DERF, DERFC, and DERFCX), 
//   and one SUBROUTINE type subprogram, CALERF.  The calling 
//   statements for the primary entries are: 
//
//                   Y=ERF(X)
//
//                   Y=ERFC(X)
//   and 
//                   Y=ERFCX(X)
//
//   The routine  CALERF  is intended for internal packet use only, 
//   all computations within the packet being concentrated in this 
//   routine.  The function subprograms invoke  CALERF  with the 
//   statement 
//
//          RESULT = CALERF(ARG,JINT) 
//
//   where the parameter usage is as follows 
//
//      Function                     Parameters for CALERF 
//       call              ARG                  Result          JINT 
//
//     ERF(ARG)      ANY REAL ARGUMENT         ERF(ARG)          0 
//     ERFC(ARG)     ABS(ARG) < XBIG          ERFC(ARG)          1 
//     ERFCX(ARG)    XNEG < ARG < XMAX       ERFCX(ARG)          2 
//
//   The main computation evaluates near-minimax approximations 
//   from "Rational Chebyshev approximations for the error function" 
//   by W. J. Cody, Math. Comp., 1969, PP. 631-638.  This 
//   transportable program uses rational functions that theoretically 
//   approximate  erf(x)  and  erfc(x)  to at least 18 significant 
//   decimal digits.  The accuracy achieved depends on the arithmetic 
//   system, the compiler, the intrinsic functions, and proper 
//   selection of the machine-dependent constants. 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Explanation of machine-dependent constants 
//
//   XMIN   = the smallest positive floating-point number. 
//   XINF   = the largest positive finite floating-point number. 
//   XNEG   = the largest negative argument acceptable to ERFCX; 
//            the negative of the solution to the equation 
//            2*exp(xx) = XINF. 
//   XSMALL = argument below which erf(x) may be represented by 
//            2x/sqrt(pi)  and above which  xx  will not underflow. 
//            A conservative value is the largest machine number X 
//            such that   1.0 + X = 1.0   to machine precision. 
//   XBIG   = largest argument acceptable to ERFC;  solution to 
//            the equation:  W(x) * (1-0.5/x**2) = XMIN,  where 
//            W(x) = exp(-xx)/[x*sqrt(pi)]. 
//   XHUGE  = argument above which  1.0 - 1/(2xx) = 1.0  to 
//            machine precision.  A conservative value is 
//            1/[2*sqrt(XSMALL)] 
//   XMAX   = largest acceptable argument to ERFCX; the minimum 
//            of XINF and 1/[sqrt(pi)*XMIN]. 
//
//   Approximate values for some important machines are: 
//
//                          XMIN       XINF        XNEG     XSMALL 
//
//  CDC 7600      (S.P.)  3.13E-294   1.26E+322   -27.220  7.11E-15 
//  CRAY-1        (S.P.)  4.58E-2467  5.45E+2465  -75.345  7.11E-15 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (S.P.)  1.18E-38    3.40E+38     -9.382  5.96E-8 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (D.P.)  2.23D-308   1.79D+308   -26.628  1.11D-16 
//  IBM 195       (D.P.)  5.40D-79    7.23E+75    -13.190  1.39D-17 
//  UNIVAC 1108   (D.P.)  2.78D-309   8.98D+307   -26.615  1.73D-18 
//  VAX D-Format  (D.P.)  2.94D-39    1.70D+38     -9.345  1.39D-17 
//  VAX G-Format  (D.P.)  5.56D-309   8.98D+307   -26.615  1.11D-16 
//
//
//                          XBIG       XHUGE       XMAX 
//
//  CDC 7600      (S.P.)  25.922      8.39E+6     1.80X+293 
//  CRAY-1        (S.P.)  75.326      8.39E+6     5.45E+2465 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (S.P.)   9.194      2.90E+3     4.79E+37 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (D.P.)  26.543      6.71D+7     2.53D+307 
//  IBM 195       (D.P.)  13.306      1.90D+8     7.23E+75 
//  UNIVAC 1108   (D.P.)  26.582      5.37D+8     8.98D+307 
//  VAX D-Format  (D.P.)   9.269      1.90D+8     1.70D+38 
//  VAX G-Format  (D.P.)  26.569      6.71D+7     8.98D+307 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Error returns 
//
//  The program returns  ERFC = 0      for  ARG >= XBIG; 
//
//                       ERFCX = XINF  for  ARG < XNEG; 
//      and 
//                       ERFCX = 0     for  ARG >= XMAX. 
//
//
//  Author: W. J. Cody 
//          Mathematics and Computer Science Division 
//          Argonne National Laboratory 
//          Argonne, IL 60439 
//
//  Latest modification: March 19, 1990 
//
//  Translated to C from FORTRAN and adapted for Java
//  by Michael Kiefte, Dalhousie University, June 5, 2002.
//
// ------------------------------------------------------------------ 
	y = Math.abs(x);
	if (y <= THRESH) {
// ------------------------------------------------------------------ 
//  Evaluate  erf  for  |X| <= 0.46875 
// ------------------------------------------------------------------ 
	    ysq = 0.0;
	    if (y > XSMALL) {
		ysq = y * y;
	    }
	    xnum = a[4] * ysq;
	    xden = ysq;
	    for (int i = 0; i < 3; ++i) {
		xnum = (xnum + a[i]) * ysq;
		xden = (xden + b[i]) * ysq;
	    }
	    result = x * (xnum + a[3]) / (xden + b[3]);
	    if (jint != 0) {
		result = 1.0 - result;
	    }
	    if (jint == 2) {
		result *= Math.exp(ysq);
	    }
	    return result;
// ------------------------------------------------------------------ 
//  Evaluate  erfc  for 0.46875 <= |X| <= 4.0 
// ------------------------------------------------------------------ 
	} else if (y <= 4.0) {
	    xnum = c[8] * y;
	    xden = y;
	    for (int i = 0; i < 7; ++i) {
		xnum = (xnum + c[i]) * y;
		xden = (xden + d[i]) * y;
	    }
	    result = (xnum + c[7]) / (xden + d[7]);
	    if (jint != 2) {
		ysq = (double) ((int) (y * 16.0)) / 16.0;
		del = (y - ysq) * (y + ysq);
		result = Math.exp(-ysq * ysq) * Math.exp(-del) * result;
	    }
// ------------------------------------------------------------------ 
//  Evaluate  erfc  for |X| > 4.0 
// ------------------------------------------------------------------ 
	} else {
	    result = 0.0;
	    if (jint == 2 && y >= XHUGE && y < XMAX) {
		result = SQRPI / y;
	    } else if (y < XBIG || jint == 2 && y < XMAX) {
		ysq = 1.0 / (y * y);
		xnum = p[5] * ysq;
		xden = ysq;
		for (int i = 0; i < 4; ++i) {
		    xnum = (xnum + p[i]) * ysq;
		    xden = (xden + q[i]) * ysq;
		}
		result = ysq * (xnum + p[4]) / (xden + q[4]);
		result = (SQRPI - result) / y;
		if (jint != 2) {
		    ysq = (double) ((int) (y * 16.0)) / 16.0;
		    del = (y - ysq) * (y + ysq);
		    result = Math.exp(-ysq * ysq) * Math.exp(-del) * result;
		}
	    }
	}
// ------------------------------------------------------------------ 
//  Fix up for negative argument, erf, etc. 
// ------------------------------------------------------------------ 
	if (jint == 0) {
	    result = 1.0 - result;
	    if (x < 0.0) {
		result = -result;
	    }
	} else if (jint == 1) {
	    if (x < 0.0) {
		result = 2.0 - result;
	    }
	} else {
	    if (x < 0.0) {
		if (x < XNEG) {
		    result = Double.POSITIVE_INFINITY;
		} else {
		    ysq = (double) ((int) (x * 16.0)) / 16.0;
		    del = (x - ysq) * (x + ysq);
		    y = Math.exp(ysq * ysq) * Math.exp(del);
		    result = 2.0 * y - result;
		}
	    }
	}
	return result;
    } // calerf 

    public static double erf(double x)
    {
// -------------------------------------------------------------------- 
//
// This subprogram computes approximate values for erf(x). 
//   (see comments heading CALERF). 
//
//   Author/date: W. J. Cody, January 8, 1985 
//
// -------------------------------------------------------------------- 

	return calerf(x, 0);
    } // erf 

    public static double erfc(double x)
    {
// -------------------------------------------------------------------- 
//
// This subprogram computes approximate values for erfc(x). 
//   (see comments heading CALERF). 
//
//   Author/date: W. J. Cody, January 8, 1985 
//
// -------------------------------------------------------------------- 

	return calerf(x, 1);
    } // erfc 

    public static double erfcx(double x)
    {
// ------------------------------------------------------------------ 
//
// This subprogram computes approximate values for exp(xx) * erfc(x). 
//   (see comments heading CALERF). 
//
//   Author/date: W. J. Cody, March 30, 1987 
//
// ------------------------------------------------------------------ 

	return calerf(x, 2);
    } // erfcx 

    public static double gamma(double x)
    {
	if (Double.isNaN(x))
	    return x;

	// Initialized data 

	final double EPS = 2.22e-16;
	final double[] p = { -1.71618513886549492533811,
		24.7656508055759199108314, -379.804256470945635097577,
		629.331155312818442661052, 866.966202790413211295064,
		-31451.2729688483675254357, -36144.4134186911729807069,
		66456.1438202405440627855 };
	final double[] q = { -30.8402300119738975254353,
		315.350626979604161529144, -1015.15636749021914166146,
		-3107.77167157231109440444, 22538.1184209801510330112,
		4755.84627752788110767815, -134659.959864969306392456,
		-115132.259675553483497211 };
	final double[] c = { -0.001910444077728, 8.4171387781295e-4,
		-5.952379913043012e-4, 7.93650793500350248e-4,
		-0.002777777777777681622553, 0.08333333333333333331554247,
		0.0057083835261 };
	final double SQRTPI = 0.9189385332046727417803297;
	final double XBIG = 171.624;
	final double XMININ = 2.23e-308;

	// Local variables 
	double fact, xden, xnum;
	int n;
	double y, z, y1;
	boolean parity;
	double res, sum, ysq;

// ---------------------------------------------------------------------- 
//
// This routine calculates the GAMMA function for a real argument X. 
//   Computation is based on an algorithm outlined in reference 1. 
//   The program uses rational functions that approximate the GAMMA 
//   function to at least 20 significant decimal digits.  Coefficients 
//   for the approximation over the interval (1, 2) are unpublished. 
//   Those for the approximation for X >= 12 are from reference 2. 
//   The accuracy achieved depends on the arithmetic system, the 
//   compiler, the intrinsic functions, and proper selection of the 
//   machine-dependent constants. 
//
//
// ******************************************************************* 
// ******************************************************************* 
//
// Explanation of machine-dependent constants 
//
// beta   - radix for the floating-point representation 
// maxexp - the smallest positive power of beta that overflows 
// XBIG   - the largest argument for which GAMMA(X) is representable 
//          in the machine, i.e., the solution to the equation 
//                  GAMMA(XBIG) = beta**maxexp 
// XINF   - the largest machine representable floating-point number; 
//          approximately beta**maxexp 
// EPS    - the smallest positive floating-point number such that 
//          1.0+EPS > 1.0 
// XMININ - the smallest positive floating-point number such that 
//          1/XMININ is machine representable 
//
//     Approximate values for some important machines are: 
//
//                            beta       maxexp        XBIG 
//
// CRAY-1         (S.P.)        2         8191        966.961 
// Cyber 180/855 
//   under NOS    (S.P.)        2         1070        177.803 
// IEEE (IBM/XT, 
//   SUN, etc.)   (S.P.)        2          128        35.040 
// IEEE (IBM/XT, 
//   SUN, etc.)   (D.P.)        2         1024        171.624 
// IBM 3033       (D.P.)       16           63        57.574 
// VAX D-Format   (D.P.)        2          127        34.844 
// VAX G-Format   (D.P.)        2         1023        171.489 
//
//                            XINF         EPS        XMININ 
//
// CRAY-1         (S.P.)   5.45E+2465   7.11E-15    1.84E-2466 
// Cyber 180/855 
//   under NOS    (S.P.)   1.26E+322    3.55E-15    3.14E-294 
// IEEE (IBM/XT, 
//   SUN, etc.)   (S.P.)   3.40E+38     1.19E-7     1.18E-38 
// IEEE (IBM/XT, 
//   SUN, etc.)   (D.P.)   1.79D+308    2.22D-16    2.23D-308 
// IBM 3033       (D.P.)   7.23D+75     2.22D-16    1.39D-76 
// VAX D-Format   (D.P.)   1.70D+38     1.39D-17    5.88D-39 
// VAX G-Format   (D.P.)   8.98D+307    1.11D-16    1.12D-308 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Error returns 
//
//  The program returns the value XINF for singularities or 
//     when overflow would occur.  The computation is believed 
//     to be free of underflow and overflow. 
//
//
// References: "An Overview of Software Development for Special 
//              Functions", W. J. Cody, Lecture Notes in Mathematics, 
//              506, Numerical Analysis Dundee, 1975, G. A. Watson 
//              (ed.), Springer Verlag, Berlin, 1976. 
//
//              Computer Approximations, Hart, Et. Al., Wiley and 
//              sons, New York, 1968. 
//
//  Latest modification: October 12, 1989 
//
//  Authors: W. J. Cody and L. Stoltz 
//           Applied Mathematics Division 
//           Argonne National Laboratory 
//           Argonne, IL 60439 
//
//  Translated to C from FORTRAN and adapted for Java
//  by Michael Kiefte, Dalhousie University, June 5, 2002.
//
// ---------------------------------------------------------------------- 
	parity = false;
	fact = 1.0;
	n = 0;
	y = x;
	if (y <= 0.0) {
// ---------------------------------------------------------------------- 
//  Argument is negative 
// ---------------------------------------------------------------------- 
	    y = -x;
	    y1 = (double) ((int) y);
	    res = y - y1;
	    if (res != 0.0) {
		if (y1 != (double) ((int) (y1 * 0.5)) * 2.0) {
		    parity = true;
		}
		fact = -Math.PI / Math.sin(Math.PI * res);
		y += 1.0;
	    } else {
		return Double.POSITIVE_INFINITY;
	    }
	}
// ---------------------------------------------------------------------- 
//  Argument is positive 
// ---------------------------------------------------------------------- 
	if (y < EPS) {
// ---------------------------------------------------------------------- 
//  Argument < EPS 
// ---------------------------------------------------------------------- 
	    if (y >= XMININ) {
		res = 1.0 / y;
	    } else {
		return Double.POSITIVE_INFINITY;
	    }
	} else if (y < 12.0) {
	    y1 = y;
	    if (y < 1.0) {
// ---------------------------------------------------------------------- 
//  0.0 < argument < 1.0 
// ---------------------------------------------------------------------- 
		z = y;
		y += 1.0;
	    } else {
// ---------------------------------------------------------------------- 
//  1.0 < argument < 12.0, reduce argument if necessary 
// ---------------------------------------------------------------------- 
		n = (int) y - 1;
		y -= (double) n;
		z = y - 1.0;
	    }
// ---------------------------------------------------------------------- 
//  Evaluate approximation for 1.0 < argument < 2.0 
// ---------------------------------------------------------------------- 
	    xnum = 0.0;
	    xden = 1.0;
	    for (int i = 0; i < 8; ++i) {
		xnum = (xnum + p[i]) * z;
		xden = xden * z + q[i];
	    }
	    res = xnum / xden + 1.0;
	    if (y1 < y) {
// ---------------------------------------------------------------------- 
//  Adjust result for case  0.0 < argument < 1.0 
// ---------------------------------------------------------------------- 
		res /= y1;
	    } else if (y1 > y) {
// ---------------------------------------------------------------------- 
//  Adjust result for case  2.0 < argument < 12.0 
// ---------------------------------------------------------------------- 
		for (int i = 1; i <= n; ++i) {
		    res *= y;
		    y += 1.0;
		}
	    }
	} else {
// ---------------------------------------------------------------------- 
//  Evaluate for argument >= 12.0, 
// ---------------------------------------------------------------------- 
	    if (y <= XBIG) {
		ysq = y * y;
		sum = c[6];
		for (int i = 0; i < 6; ++i) {
		    sum = sum / ysq + c[i];
		}
		sum = sum / y - y + SQRTPI;
		sum += (y - 0.5) * Math.log(y);
		res = Math.exp(sum);
	    } else {
		return Double.POSITIVE_INFINITY;
	    }
	}
// ---------------------------------------------------------------------- 
//  Final adjustments and return 
// ---------------------------------------------------------------------- 
	if (parity) {
	    res = -res;
	}
	if (fact != 1.0) {
	    res = fact / res;
	}
	return res;
    } // gamma 

    private static double calci0(double arg, int jint)
    {
	    double result = 0.0;

	    // Initialized data 

	    final double[] p = { -5.24878666279456998e-18,
		    -1.5982226675653184646e-14, -2.6843448573468483278e-11,
		    -3.0517226450451067446e-8, -2.5172644670688975051e-5,
		    -0.015453977791786851041, -7.093534744921054919,
		    -2412.5195876041896775, -595456.26019847898221,
		    -103130667.08737980747, -11912746104.985237192,
		    -849251012471.14157499, -32940087627407.749166,
		    -550503696730184.27753, -2233558263947437.5249 };
	    final double[] q = { -3727.7560179962773046, 6515850.6418655165707,
		    -6562656074.0833869295, 3760418870409.2954661,
		    -970879461795940.19126 };
	    final double[] pp = { -0.3984375, 2.9205384596336793945,
		    -2.4708469169133954315, 0.47914889422856814203,
		    -0.003738499192606896915, -0.002680152035332863531,
		    9.9168777670983678974e-5, -2.187712818903272673e-6 };
	    final double[] qq = { -31.4466902751354915, 85.5395632580129296,
		    -60.228002066743340583, 13.982595353892851542,
		    -1.1151759188741312645, 0.032547697594819615062,
		    -5.5194330231005480228e-4 };
	    final double EXP40 = 235385266837019985.4;
	    final double REC15 = 0.066666666666666666666;
	    final double XSMALL = 5.55e-17;
	    final double XMAX = 713.986;

	    // Local variables 
	    double sump, sumq, a, b;
	    double x, xx;

// -------------------------------------------------------------------- 
//
// This packet computes modified Bessel functions of the first kind 
//   and order zero, I0(X) and EXP(-ABS(X))*I0(X), for real 
//   arguments X.  It contains two function type subprograms, BESI0 
//   and BESEI0, and 1.0 subroutine type subprogram, CALCI0. 
//   The calling statements for the primary entries are 
//
//                   Y=BESI0(X) 
//   and 
//                   Y=BESEI0(X) 
//
//   where the entry points correspond to the functions I0(X) and 
//   EXP(-ABS(X))*I0(X), respectively.  The routine CALCI0 is 
//   intended for internal packet use only, all computations within 
//   the packet being concentrated in this routine.  The function 
//   subprograms invoke CALCI0 with the statement 
//          CALL CALCI0(ARG,RESULT,JINT) 
//   where the parameter usage is as follows 
//
//      Function                     Parameters for CALCI0 
//       Call              ARG                  RESULT          JINT 
//
//     BESI0(ARG)    ABS(ARG) <= XMAX        I0(ARG)           1 
//     BESEI0(ARG)    any real ARG        EXP(-ABS(ARG))*I0(ARG) 2 
//
//   The main computation evaluates slightly modified forms of 
//   minimax approximations generated by Blair and Edwards, Chalk 
//   River (Atomic Energy of Canada Limited) Report AECL-4928, 
//   October, 1974.  This transportable program is patterned after 
//   the machine-dependent FUNPACK packet NATSI0, but cannot match 
//   that version for efficiency or accuracy.  This version uses 
//   rational functions that theoretically approximate I-SUB-0(X) 
//   to at least 18 significant decimal digits.  The accuracy 
//   achieved depends on the arithmetic system, the compiler, the 
//   intrinsic functions, and proper selection of the machine- 
//   dependent constants. 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Explanation of machine-dependent constants 
//
//   beta   = Radix for the floating-point system 
//   maxexp = Smallest power of beta that overflows 
//   XSMALL = Positive argument such that 1.0 - X = 1.0 to 
//            machine precision for all ABS(X) <= XSMALL. 
//   XINF =   Largest positive machine number; approximately 
//            beta**maxexp 
//   XMAX =   Largest argument acceptable to BESI0;  Solution to 
//            equation: 
//               W(X) * (1+1/(8*X)+9/(128*X**2) = beta**maxexp 
//            where  W(X) = EXP(X)/SQRT(2*PI*X) 
//
//
//     Approximate values for some important machines are: 
//
//                          beta       maxexp       XSMALL 
//
// CRAY-1        (S.P.)       2         8191       3.55E-15 
// Cyber 180/855 
//   under NOS   (S.P.)       2         1070       3.55E-15 
// IEEE (IBM/XT, 
//   SUN, etc.)  (S.P.)       2          128       2.98E-8 
// IEEE (IBM/XT, 
//   SUN, etc.)  (D.P.)       2         1024       5.55D-17 
// IBM 3033      (D.P.)      16           63       6.95D-18 
// VAX           (S.P.)       2          127       2.98E-8 
// VAX D-Format  (D.P.)       2          127       6.95D-18 
// VAX G-Format  (D.P.)       2         1023       5.55D-17 
//
//
//                               XINF          XMAX 
//
// CRAY-1        (S.P.)       5.45E+2465     5682.810 
// Cyber 180/855 
//   under NOS   (S.P.)       1.26E+322       745.893 
// IEEE (IBM/XT, 
//   SUN, etc.)  (S.P.)       3.40E+38         91.900 
// IEEE (IBM/XT, 
//   SUN, etc.)  (D.P.)       1.79D+308       713.986 
// IBM 3033      (D.P.)       7.23D+75        178.182 
// VAX           (S.P.)       1.70D+38         91.203 
// VAX D-Format  (D.P.)       1.70D+38         91.203 
// VAX G-Format  (D.P.)       8.98D+307       713.293 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Error returns 
//
//  The program returns XINF for BESI0 for ABS(ARG) > XMAX. 
//
//
//  Intrinsic functions required are: 
//
//     ABS, SQRT, EXP 
//
//
//  Authors: W. J. Cody and L. Stoltz 
//           Mathematics and Computer Science Division 
//           Argonne National Laboratory 
//           Argonne, IL 60439 
//
//  Latest modification: June 7, 1988 
//
//  Translated to C from FORTRAN and adapted for Java
//  by Michael Kiefte, Dalhousie University, June 5, 2002.
//
// -------------------------------------------------------------------- 
	x = Math.abs(arg);
	if (x < XSMALL) {
	    result = 1.0;
	} else if (x < 15.0) {
// -------------------------------------------------------------------- 
//  XSMALL <=  ABS(ARG)  < 15.0 
// -------------------------------------------------------------------- 
	    xx = x * x;
	    sump = p[0];
	    for (int i = 1; i < 15; ++i) {
		sump = sump * xx + p[i];
	    }
	    xx -= 225.0;
	    sumq = ((((xx + q[0]) * xx + q[1]) * xx + q[2]) * xx + q[3])
		    * xx + q[4];
	    result = sump / sumq;
	    if (jint == 2) {
		result *= Math.exp(-x);
	    }
	} else if (x >= 15.0) {
	    if (jint == 1 && x > XMAX) {
		result = Double.POSITIVE_INFINITY;
	    } else {
// -------------------------------------------------------------------- 
//  15.0  <=  ABS(ARG) 
// -------------------------------------------------------------------- 
		xx = 1.0 / x - REC15;
		sump = ((((((pp[0] * xx + pp[1]) * xx + pp[2]) * xx + pp[3])
		    * xx + pp[4]) * xx + pp[5]) * xx + pp[6]) * xx + pp[7];
		sumq = ((((((xx + qq[0]) * xx + qq[1]) * xx + qq[2])
		    * xx + qq[3]) * xx + qq[4]) * xx + qq[5]) * xx + qq[6];
		result = sump / sumq;
		if (jint == 2) {
		    result = (result - pp[0]) / Math.sqrt(x);
		} else {
// -------------------------------------------------------------------- 
//  Calculation reformulated to avoid premature overflow 
// -------------------------------------------------------------------- 
		    if (x <= XMAX - 15.0) {
			a = Math.exp(x);
			b = 1.0;
		    } else {
			a = Math.exp(x - 40.0);
			b = EXP40;
		    }
		    result = (result * a - pp[0] * a) / Math.sqrt(x) * b;
		}
	    }
	}
// -------------------------------------------------------------------- 
//  Return for ABS(ARG) < XSMALL 
// -------------------------------------------------------------------- 
	return result;
    } // calci0 

    public static double besi0(double x)
    {
// -------------------------------------------------------------------- 
//
// This long precision subprogram computes approximate values for 
//   modified Bessel functions of the first kind of order zero for 
//   arguments ABS(ARG) <= XMAX  (see comments heading CALCI0). 
//
// -------------------------------------------------------------------- 

	return calci0(x, 1);
    } // besi0 

    public static double besei0(double x)
    {
// -------------------------------------------------------------------- 
//
// This function program computes approximate values for the 
//   modified Bessel function of the first kind of order zero 
//   multiplied by EXP(-ABS(X)), where EXP is the 
//   exponential function, ABS is the absolute value, and X 
//   is any argument. 
//
// -------------------------------------------------------------------- 
	return calci0(x, 2);
    } // besei0 

    private static double calci1(double arg, int jint)
    {
	double result;

	// Initialized data 

	final double XMAX = 713.987;
	final double[] p = { -1.970529180253513993e-19,
		-6.524551558315190291e-16, -1.1928788903603238754e-12,
		-1.4831904935994647675e-9, -1.3466829827635152875e-6,
		-9.1746443287817501309e-4, -0.47207090827310162436,
		-182.25946631657315931, -51894.09198230801754,
		-10588550.724769347106, -1482826760.6612366099,
		-133574376822.75493024, -6987677964801.009007,
		-177320378407915.9132, -1457718027814346.3643 };
	final double[] q = { -4007.6864679904189921, 7481058.0356655069138,
		-8005951899.8619764991, 4854471425827.3622913,
		-1321816830732144.2305 };
	final double[] pp = { -0.0604371590561376, 0.45748122901933459,
		-0.42843766903304806403, 0.097356000150886612134,
		-0.0032457723974465568321, -3.6395264712121795296e-4,
		1.6258661867440836395e-5, -3.6347578404608223492e-7 };
	final double[] qq = { -3.880658672155659345, 3.2593714889036996297,
		-0.85017476463217924408, 0.074212010813186530069,
		-0.0022835624489492512649, 3.7510433111922824643e-5 };
	final double PBAR = 0.3984375;
	final double EXP40 = 235385266837019985.4;
	final double REC15 = 0.066666666666666666666;
	final double XSMALL = 5.55e-17;

	// Local variables 
	double sump, sumq, a, b;
	double x, xx;

// -------------------------------------------------------------------- 
//
// This packet computes modified Bessel functioons of the first kind 
//   and order one I1(X) and EXP(-ABS(X))*I1(X), for real 
//   arguments X.  It contains two function type subprograms, BESI1 
//   and BESEI1, and one subroutine type subprogram, CALCI1. 
//   The calling statements for the primary entries are 
//
//                   Y=BESI1(X) 
//   and 
//                   Y=BESEI1(X) 
//
//   where the entry points correspond to the functions I1(X) and 
//   EXP(-ABS(X))*I1(X), respectively.  The routine CALCI1 is 
//   intended for internal packet use only, all computations within 
//   the packet being concentrated in this routine.  The function 
//   subprograms invoke CALCI1 with the statement 
//          RESULT = CALCI1(ARG,JINT) 
//   where the parameter usage is as follows 
//
//      Function                     Parameters for CALCI1 
//       Call              ARG                  RESULT          JINT 
//
//     BESI1(ARG)    ABS(ARG) <= XMAX          I1(ARG)           1 
//     BESEI1(ARG)    any real ARG        EXP(-ABS(ARG))*I1(ARG) 2 
//
//   The main computation evaluates slightly modified forms of 
//   minimax approximations generated by Blair and Edwards, Chalk 
//   River (Atomic Energy of Canada Limited) Report AECL-4928, 
//   October, 1974.  This transportable program is patterned after 
//   the machine-dependent FUNPACK packet NATSI1, but cannot match 
//   that version for efficiency or accuracy.  This version uses 
//   rational functions that theoretically approximate I-SUB-1(X) 
//   to at least 18 significant decimal digits.  The accuracy 
//   achieved depends on the arithmetic system, the compiler, the 
//   intrinsic functions, and proper selection of the machine- 
//   dependent constants. 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Explanation of machine-dependent constants 
//
//   beta   = Radix for the floating-point system 
//   maxexp = Smallest power of beta that overflows 
//   XSMALL = Positive argument such that 1.0 - X = 1.0 to 
//            machine precision for all ABS(X) <= XSMALL. 
//   XINF =   Largest positive machine number; approximately 
//            beta**maxexp 
//   XMAX =   Largest argument acceptable to BESI1;  Solution to 
//            equation: 
//               EXP(X) * (1-3/(8*X)) / SQRT(2*PI*X) = beta**maxexp 
//
//
//     Approximate values for some important machines are: 
//
//                          beta       maxexp       XSMALL 
//
// CRAY-1        (S.P.)       2         8191       3.55E-15 
// Cyber 180/855 
//   under NOS   (S.P.)       2         1070       3.55E-15 
// IEEE (IBM/XT, 
//   SUN, etc.)  (S.P.)       2          128       2.98E-8 
// IEEE (IBM/XT, 
//   SUN, etc.)  (D.P.)       2         1024       5.55D-17 
// IBM 3033      (D.P.)      16           63       6.95D-18 
// VAX           (S.P.)       2          127       2.98E-8 
// VAX D-Format  (D.P.)       2          127       6.95D-18 
// VAX G-Format  (D.P.)       2         1023       5.55D-17 
//
//
//                               XINF          XMAX 
//
// CRAY-1        (S.P.)       5.45E+2465     5682.810 
// Cyber 180/855 
//   under NOS   (S.P.)       1.26E+322       745.894 
// IEEE (IBM/XT, 
//   SUN, etc.)  (S.P.)       3.40E+38         91.906 
// IEEE (IBM/XT, 
//   SUN, etc.)  (D.P.)       1.79D+308       713.987 
// IBM 3033      (D.P.)       7.23D+75        178.185 
// VAX           (S.P.)       1.70D+38         91.209 
// VAX D-Format  (D.P.)       1.70D+38         91.209 
// VAX G-Format  (D.P.)       8.98D+307       713.293 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Error returns 
//
//  The program returns the value XINF for ABS(ARG) > XMAX. 
//
//
//  Authors: W. J. Cody and L. Stoltz 
//           Mathematics and Computer Science Division 
//           Argonne National Laboratory 
//           Argonne, IL  60439 
//
//  Latest modification: May 31, 1989 
//
//  Translated to C from FORTRAN and adapted for Java
//  by Michael Kiefte, Dalhousie University, June 10, 2002.
//
// -------------------------------------------------------------------- 
	x = Math.abs(arg);
	if (x < XSMALL) {
// -------------------------------------------------------------------- 
//  Return for ABS(ARG) < XSMALL 
// -------------------------------------------------------------------- 
	    result = 0.5 * x;
	} else if (x < 15.0) {
// -------------------------------------------------------------------- 
//  XSMALL <= ABS(ARG) < 15.0 
// -------------------------------------------------------------------- 
	    xx = x * x;
	    sump = p[0];
	    for (int j = 1; j < 15; ++j) {
		sump = sump * xx + p[j];
	    }
	    xx -= 225.0;
	    sumq = ((((xx + q[0]) * xx + q[1]) * xx + q[2]) * xx + q[3])
		* xx + q[4];
	    result = sump / sumq * x;
	    if (jint == 2) {
		result *= Math.exp(-x);
	    }
	} else if (jint == 1 && x > XMAX) {
	    result = Double.POSITIVE_INFINITY;
	} else {
// -------------------------------------------------------------------- 
//  15.0 <= ABS(ARG) 
// -------------------------------------------------------------------- 
	    xx = 1.0 / x - REC15;
	    sump = ((((((pp[0] * xx + pp[1]) * xx + pp[2]) * xx + pp[3]) * xx + 
		    pp[4]) * xx + pp[5]) * xx + pp[6]) * xx + pp[7];
	    sumq = (((((xx + qq[0]) * xx + qq[1]) * xx + qq[2]) * xx + qq[3]) * 
		    xx + qq[4]) * xx + qq[5];
	    result = sump / sumq;
	    if (jint != 1) {
		result = (result + PBAR) / Math.sqrt(x);
	    } else {
// -------------------------------------------------------------------- 
//  Calculation reformulated to avoid premature overflow 
// -------------------------------------------------------------------- 
		if (x > XMAX - 15.0) {
		    a = Math.exp(x - 40.0);
		    b = EXP40;
		} else {
		    a = Math.exp(x);
		    b = 1.0;
		}
		result = (result * a + PBAR * a) / Math.sqrt(x) * b;
// -------------------------------------------------------------------- 
//  Error return for ABS(ARG) > XMAX 
// -------------------------------------------------------------------- 
	    }
	}
	if (arg < 0.0) {
	    result = -result;
	}
	return result;
    } // calci1 

    public static double besi1(double x)
    {
// -------------------------------------------------------------------- 
//
// This long precision subprogram computes approximate values for 
//   modified Bessel functions of the first kind of order 1.0 for 
//   arguments ABS(ARG) <= XMAX  (see comments heading CALCI1). 
//
// -------------------------------------------------------------------- 
	return calci1(x, 1);
    } // besi1 

    public static double besei1(double x)
    {
// -------------------------------------------------------------------- 
//
// This function program computes approximate values for the 
//   modified Bessel function of the first kind of order 1.0 
//   multiplied by EXP(-ABS(X)), where EXP is the 
//   exponential function, ABS is the absolute value, and X 
//   is any argument. 
//
// -------------------------------------------------------------------- 
	return calci1(x, 2);
    } // besei1 

    private static double caljy0(double arg, int jint)
    {
	double result;

	// Initialized data 

	final double CONS = -0.11593151565841244881;
	final double PI2 = 0.63661977236758134308;
	final double TWOPI = 6.2831853071795864769;
	final double TWOPI1 = 6.28125;
	final double TWOPI2 = 0.0019353071795864769253;
	final double XMAX = 2.68e8;
	final double XSMALL = 3.72e-9;
	final double XJ0 = 2.4048255576957727686;
	final double XJ1 = 5.5200781102863106496;
	final double XY0 = 0.89357696627916752158;
	final double XY1 = 3.9576784193148578684;
	final double XY2 = 7.0860510603017726976;
	final double XJ01 = 616.0;
	final double XJ02 = -0.0014244423042272313784;
	final double XJ11 = 1413.0;
	final double XJ12 = 5.468602863106495966e-4;
	final double XY01 = 228.0;
	final double XY02 = 0.0029519662791675215849;
	final double XY11 = 1013.0;
	final double XY12 = 6.4716931485786837568e-4;
	final double XY21 = 1814.0;
	final double XY22 = 1.1356030177269762362e-4;
	final double[] plg = { -24.562334077563243311, 236.42701335621505212,
		-549.89956895857911039, 356.87548468071500413 };
	final double[] qlg = { -35.553900764052419184, 194.00230218539473193,
		-334.42903192607538956, 178.43774234035750207 };
	final double[] pj0 = { 6630299.7904833794242, -621407004.23540120665,
		27282507878.605942706, -412986685009.90866786,
		-0.12117036164593528341, 103.44222815443188943,
		-36629.814655107086448 };
	final double[] qj0 = { 456126.962242199382, 139850973.72263433271,
		26328198300.859648632, 2388378799633.2290397,
		936.14022392337710626 };
	final double[] pj1 = { 4417.6707025325087628, 11725.046279757103576,
		10341.910641583726701, -7287.9702464464618998,
		-12254.078161378989535, -1831.9397969392084011,
		48.591703355916499363, 743.21196680624245801 };
	final double[] qj1 = { 333.07310774649071172, -2945.8766545509337327,
		18680.990008359188352, -84055.062591169562211,
		245991.02262586308984, -357834.78026152301072,
		-25.258076240801555057 };
	final double[] py0 = { 10102.53294802090759, -2128754.8474401797963,
		204222743.57376619816, -8371625545.1260504098,
		107235387820.03176831, -18.402381979244993524 };
	final double[] qy0 = { 664.75986689240190091, 238893.93209447253406,
		55662956.624278251596, 8161718777.7290363573,
		588738657389.97033405 };
	final double[] py1 = { -14566.86583266363592, 4690528.861167863151,
		-695904393.94619619534, 43600098638.603061642,
		-551074352067.22644429, -22213976967566.192242,
		17.427031242901594547 };
	final double[] qy1 = { 830.30857612070288823, 406699.82352539552018,
		139602027.70986831075, 34015103849.971240096,
		5426682441941.234755, 433861465807072.64428 };
	final double[] py2 = { 21363.534169313901632, -10085539.923498211426,
		2195882717.0518100757, -193630512667.72083678,
		-128299123640.88687306, 670166418691732.37784,
		-8072872690515021.0443, -17.439661319197499338 };
	final double[] qy2 = { 879.03362168128450017, 539247.3920976805703,
		247272194.75672302327, 86926121104.209825246,
		22598377924042.897629, 3927242556964030.9819,
		345637246288464575.19 };
	final double[] p0 = { 3480.6486443249270347, 21170.523380864944322,
		41345.386639580765797, 22779.090197304684302,
		0.88961548424210455236, 153.76201909008354296 };
	final double[] q0 = { 3502.8735138235608207, 21215.35056188011573,
		41370.41249551041664, 22779.090197304684318,
		157.11159858080893649 } ;
	final double[] p1 = { -22.300261666214198472, -111.83429920482737611,
		-185.919536443429938, -89.226600200800094098,
		-0.0088033303048680751817, -1.2441026745835638459 };
	final double[] q1 = { 1488.7231232283756582, 7264.2780169211018836,
		11951.131543434613647, 5710.5024128512061905,
		90.593769594993125859 };
	final double P17 = 0.1716;

	// Local variables 
	double xden, resj, prod, down, xnum;
	double w, z, r0, r1, ax, up, xy, wsq, zsq;

// --------------------------------------------------------------------- 
//
// This packet computes zero-order Bessel functions of the first and 
//   second kind (J0 and Y0), for real arguments X, where 0 < X <= XMAX 
//   for Y0, and |X| <= XMAX for J0.  It contains two function-type 
//   subprograms,  BESJ0  and  BESY0,  and one subroutine-type 
//   subprogram,  CALJY0.  The calling statements for the primary 
//   entries are: 
//
//           Y = BESJ0(X) 
//   and 
//           Y = BESY0(X), 
//
//   where the entry points correspond to the functions J0(X) and Y0(X), 
//   respectively.  The routine  CALJY0  is intended for internal packet 
//   use only, all computations within the packet being concentrated in 
//   this one routine.  The function subprograms invoke  CALJY0  with 
//   the statement 
//           RESULT = CALJY0(ARG,JINT), 
//   where the parameter usage is as follows: 
//
//      Function                  Parameters for CALJY0 
//       call              ARG             RESULT          JINT 
//
//     BESJ0(ARG)     |ARG| <= XMAX         J0(ARG)          0 
//     BESY0(ARG)   0 < ARG <= XMAX         Y0(ARG)          1 
//
//   The main computation uses unpublished minimax rational 
//   approximations for X <= 8.0, and an approximation from the 
//   book  Computer Approximations  by Hart, et. al., Wiley and Sons, 
//   New York, 1968, for arguments larger than 8.0   Part of this 
//   transportable packet is patterned after the machine-dependent 
//   FUNPACK program BESJ0(X), but cannot match that version for 
//   efficiency or accuracy.  This version uses rational functions 
//   that are theoretically accurate to at least 18 significant decimal 
//   digits for X <= 8, and at least 18 decimal places for X > 8.  The 
//   accuracy achieved depends on the arithmetic system, the compiler, 
//   the intrinsic functions, and proper selection of the machine- 
//   dependent constants. 
//
// ******************************************************************* 
//
// Explanation of machine-dependent constants 
//
//   XINF   = largest positive machine number 
//   XMAX   = largest acceptable argument.  The functions AINT, SIN 
//            and COS must perform properly for  ABS(X) <= XMAX. 
//            We recommend that XMAX be a small int multiple of 
//            Math.sqrt(1/eps), where eps is the smallest positive number 
//            such that  1+eps > 1. 
//   XSMALL = positive argument such that  1.0-(X/2)**2 = 1.0 
//            to machine precision for all  ABS(X) <= XSMALL. 
//            We recommend that  XSMALL < Math.sqrt(eps)/beta, where beta 
//            is the floating-point radix (usually 2 or 16). 
//
//     Approximate values for some important machines are 
//
//                          eps      XMAX     XSMALL      XINF 
//
//  CDC 7600      (S.P.)  7.11E-15  1.34E+08  2.98E-08  1.26E+322 
//  CRAY-1        (S.P.)  7.11E-15  1.34E+08  2.98E-08  5.45E+2465 
//  IBM PC (8087) (S.P.)  5.96E-08  8.19E+03  1.22E-04  3.40E+38 
//  IBM PC (8087) (D.P.)  1.11D-16  2.68D+08  3.72D-09  1.79D+308 
//  IBM 195       (D.P.)  2.22D-16  6.87D+09  9.09D-13  7.23D+75 
//  UNIVAC 1108   (D.P.)  1.73D-18  4.30D+09  2.33D-10  8.98D+307 
//  VAX 11/780    (D.P.)  1.39D-17  1.07D+09  9.31D-10  1.70D+38 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Error Returns 
//
//  The program returns the value 0.0 for  X > XMAX, and returns 
//    -XINF when BESLY0 is called with a negative or 0.0 argument. 
//
//
//  Latest modification: June 2, 1989 
//
//  Author: W. J. Cody 
//          Mathematics and Computer Science Division 
//          Argonne National Laboratory 
//          Argonne, IL 60439 
//
//  Translated to C from FORTRAN and adapted for Java
//  by Michael Kiefte, Dalhousie University, June 10, 2002.
//
// -------------------------------------------------------------------- 

// ------------------------------------------------------------------- 
//  Check for error conditions 
// ------------------------------------------------------------------- 
	ax = Math.abs(arg);
	if (jint == 1 && arg <= 0.0) {
	    result = Double.NEGATIVE_INFINITY;
	    return result;
	} else if (ax > XMAX) {
	    result = 0.0;
	    return result;
	} else if (ax > 8.0) {
// ------------------------------------------------------------------- 
//  Calculate J0 or Y0 for |ARG|  >  8.0 
// ------------------------------------------------------------------- 
	    z = 8.0 / ax;
	    w = ax / TWOPI;
	    w = (double) ((int) w) + 0.125;
	    w = ax - w * TWOPI1 - w * TWOPI2;
	    zsq = z * z;
	    xnum = p0[4] * zsq + p0[5];
	    xden = zsq + q0[4];
	    up = p1[4] * zsq + p1[5];
	    down = zsq + q1[4];
	    for (int i = 0; i < 4; ++i) {
		xnum = xnum * zsq + p0[i];
		xden = xden * zsq + q0[i];
		up = up * zsq + p1[i];
		down = down * zsq + q1[i];
	    }
	    r0 = xnum / xden;
	    r1 = up / down;
	    if (jint == 0) {
		result = Math.sqrt(PI2 / ax) * (r0 * Math.cos(w) - z * r1
			* Math.sin(w));
	    } else {
		result = Math.sqrt(PI2 / ax) * (r0 * Math.sin(w) + z * r1
			* Math.cos(w));
	    }
	    return result;
	} else if (ax <= XSMALL) {
	    if (jint == 0) {
		result = 1.0;
	    } else {
		result = PI2 * (Math.log(ax) + CONS);
	    }
	    return result;
	} else {
// ------------------------------------------------------------------- 
//  Calculate J0 for appropriate interval, preserving 
//     accuracy near the 0.0 of J0 
// ------------------------------------------------------------------- 
	    zsq = ax * ax;
	    if (ax <= 4.0) {
		xnum = (pj0[4] * zsq + pj0[5]) * zsq + pj0[6];
		xden = zsq + qj0[4];
		for (int i = 0; i < 4; ++i) {
		    xnum = xnum * zsq + pj0[i];
		    xden = xden * zsq + qj0[i];
		}
		prod = (ax - XJ01 / 256.0 - XJ02) * (ax + XJ0);
	    } else {
		wsq = 1.0 - zsq / 64.0;
		xnum = pj1[6] * wsq + pj1[7];
		xden = wsq + qj1[6];
		for (int i = 0; i < 6; ++i) {
		    xnum = xnum * wsq + pj1[i];
		    xden = xden * wsq + qj1[i];
		}
		prod = (ax + XJ1) * (ax - XJ11 / 256.0 - XJ12);
	    }
	    result = prod * xnum / xden;
	    if (jint == 0) {
		return result;
	    }
// ------------------------------------------------------------------- 
//  Calculate Y0.  First find  RESJ = pi/2 ln(x/xn) J0(x), 
//    where xn is a zero of Y0 
// ------------------------------------------------------------------- 
	    if (ax <= 3.0) {
		up = ax - XY01 / 256.0 - XY02;
		xy = XY0;
	    } else if (ax <= 5.5) {
		up = ax - XY11 / 256.0 - XY12;
		xy = XY1;
	    } else {
		up = ax - XY21 / 256.0 - XY22;
		xy = XY2;
	    }
	    down = ax + xy;
	    if (Math.abs(up) < P17 * down) {
		w = up / down;
		wsq = w * w;
		xnum = plg[0];
		xden = wsq + qlg[0];
		for (int i = 1; i < 4; ++i) {
		    xnum = xnum * wsq + plg[i];
		    xden = xden * wsq + qlg[i];
		}
		resj = PI2 * result * w * xnum / xden;
	    } else {
		resj = PI2 * result * Math.log(ax / xy);
	    }
// ------------------------------------------------------------------- 
//  Now calculate Y0 for appropriate interval, preserving 
//     accuracy near the zero of Y0 
// ------------------------------------------------------------------- 
	    if (ax <= 3.0) {
		xnum = py0[5] * zsq + py0[0];
		xden = zsq + qy0[0];
		for (int i = 1; i < 5; ++i) {
		    xnum = xnum * zsq + py0[i];
		    xden = xden * zsq + qy0[i];
		}
	    } else if (ax <= 5.5) {
		xnum = py1[6] * zsq + py1[0];
		xden = zsq + qy1[0];
		for (int i = 1; i < 6; ++i) {
		    xnum = xnum * zsq + py1[i];
		    xden = xden * zsq + qy1[i];
		}
	    } else {
		xnum = py2[7] * zsq + py2[0];
		xden = zsq + qy2[0];
		for (int i = 1; i < 7; ++i) {
		    xnum = xnum * zsq + py2[i];
		    xden = xden * zsq + qy2[i];
		}
	    }
	    result = resj + up * down * xnum / xden;
	    return result;
	}
    } // caljy0 

    public static double besj0(double x)
    {
// -------------------------------------------------------------------- 
//
// This subprogram computes approximate values for Bessel functions 
//   of the first kind of order zero for arguments  |X| <= XMAX 
//   (see comments heading CALJY0). 
//
// -------------------------------------------------------------------- 
	return caljy0(x, 0);
    } // besj0 

    public static double besy0(double x)
    {
// -------------------------------------------------------------------- 
//
// This subprogram computes approximate values for Bessel functions 
//   of the second kind of order zero for arguments 0 < X <= XMAX 
//   (see comments heading CALJY0). 
//
// -------------------------------------------------------------------- 
	return caljy0(x, 1);
    } // besy0 

    private static double caljy1(double arg, int jint)
    {
	double result;

	// Initialized data 

	final double TWOPI2 = 0.0019353071795864769253;
	final double RTPI2 = 0.79788456080286535588;
	final double XMAX = 2.68e+8;
	final double XSMALL = 3.72e-9;
	final double XINF = 1.79e308;
	final double XJ0 = 3.8317059702075123156;
	final double XJ1 = 7.0155866698156187535;
	final double XY0 = 2.1971413260310170351;
	final double XY1 = 5.4296810407941351328;
	final double XJ01 = 981.0;
	final double XJ02 = -3.2527979248768438556e-4;
	final double XJ11 = 1796.0;
	final double XJ12 = -3.833018438124646295e-5;
	final double XY01 = 562.0;
	final double XY02 = 0.001828826031017035149;
	final double XY11 = 1390.0;
	final double XY12 = -6.4592058648672279948e-6;
	final double[] plg = { -24.562334077563243311, 236.42701335621505212,
		-549.89956895857911039, 356.87548468071500413 };
	final double[] qlg = { -35.553900764052419184, 194.00230218539473193,
		-334.42903192607538956, 178.43774234035750207 };
	final double[] pj0 = { 980629.04098958257677, -115486967.64841276794,
		6678104126.1492395835, -142585098013.66645672,
		-4461.579298277507613, 10.650724020080236441,
		-0.010767857011487300348 };
	final double[] qj0 = { 591176.14494174794095, 202283751.40097033958,
		42091902282.580133541, 4186860446082.017529,
		1074.2272239517380498 };
	final double[] pj1 = { 4.617919185275825228, -7132.9006872560947377,
		4503965.8105749078904, -1443771771.8363239107,
		235692853972.17157313, -16324168293282.543629,
		113570227199794.68624, 1005189971711528.5432 };
	final double[] qj1 = { 1126712.506502913805, 648725028.99596389593,
		276227772862.44082666, 84899346165481.429307,
		17128800897135812.012, 1725390588844768119.4,
		1388.6978985861357615 };
	final double[] py0 = { 221579.5322228026082, -59157479.997408395984,
		7214454821.4502560419, -375959744978.19597599,
		5470861171652.5426053, 40535726612579.544093,
		-317.14424660046133456 };
	final double[] qy0 = { 820.79908168393867438, 381364.70753052572164,
		122504351.2218296322, 27800352738.690585613,
		4127228620040.6461981, 307378739210792.86084 };
	final double[] py1 = { 1915380.6858264202986, -1195796191.2070617006,
		374536739624.38488783, -59530713129741.981618,
		4068627528980474.4814, -23638408497043134.724,
		-5680809457472420457.7, 11514276357909013326.0,
		-1233.7180442012953128 };
	final double[] qy1 = { 1285.5164849321609336, 1045374.8201934079734,
		635503180.87088919566, 302217668529.60403645,
		111870100658569.71027, 30837179548112881.95,
		5696819882285717891.1, 533218443133161856970.0 };
	final double[] p0 = { -109824.05543459346727, -1523529.3511811373833,
		-6603373.2483649391093, -9942246.5050776411957,
		-4435757.8167941278571, -1611.6166443246101165 };
	final double[] q0 = { -107263.85991103820119, -1511809.5066341608816,
		-6585339.4797230870728, -9934124.389934585659,
		-4435757.8167941278568, -1455.0094401904961825 };
	final double[] p1 = { 1706.3754290207680021, 18494.262873223866797,
		66178.836581270835179, 85145.160675335701966,
		33220.913409857223519, 35.265133846636032186 };
	final double[] q1 = { 37890.229745772202641, 400294.43582266975117,
		1419460.6696037208929, 1819458.0422439972989,
		708712.81941028743574, 863.83677696049909675 };
	final double PI2 = 0.63661977236758134308;
	final double P17 = 0.1716;
	final double TWOPI = 6.2831853071795864769;
	final double TWOPI1 = 6.28125;

	// Local variables 
	double xden, resj, prod, down, xnum;
	double w, z, r0, r1, ax, up, xy, wsq, zsq;

// --------------------------------------------------------------------- 
//
// This packet computes first-order Bessel functions of the first and 
//   second kind (J1 and Y1), for real arguments X, where 0 < X <= XMAX 
//   for Y1, and |X| <= XMAX for J1.  It contains two function-type 
//   subprograms,  BESJ1  and  BESY1,  and one subroutine-type 
//   subprogram,  CALJY1.  The calling statements for the primary 
//   entries are: 
//
//           Y = BESJ1(X) 
//   and 
//           Y = BESY1(X), 
//
//   where the entry points correspond to the functions J1(X) and Y1(X), 
//   respectively.  The routine  CALJY1  is intended for internal packet 
//   use only, all computations within the packet being concentrated in 
//   this one routine.  The function subprograms invoke  CALJY1  with 
//   the statement 
//           RESULT = CALJY1(ARG,JINT), 
//   where the parameter usage is as follows: 
//
//      Function                  Parameters for CALJY1 
//       call              ARG             RESULT          JINT 
//
//     BESJ1(ARG)     |ARG| <= XMAX         J1(ARG)          0 
//     BESY1(ARG)   0 < ARG <= XMAX         Y1(ARG)          1 
//
//   The main computation uses unpublished minimax rational 
//   approximations for X <= 8.0, and an approximation from the 
//   book  Computer Approximations  by Hart, et. al., Wiley and Sons, 
//   New York, 1968, for arguments larger than 8.0   Part of this 
//   transportable packet is patterned after the machine-dependent 
//   FUNPACK program BESJ1(X), but cannot match that version for 
//   efficiency or accuracy.  This version uses rational functions 
//   that are theoretically accurate to at least 18 significant decimal 
//   digits for X <= 8, and at least 18 decimal places for X > 8.  The 
//   accuracy achieved depends on the arithmetic system, the compiler, 
//   the intrinsic functions, and proper selection of the machine- 
//   dependent constants. 
//
// ******************************************************************* 
//
// Explanation of machine-dependent constants 
//
//   XINF   = largest positive machine number 
//   XMAX   = largest acceptable argument.  The functions AINT, SIN 
//            and COS must perform properly for  ABS(X) <= XMAX. 
//            We recommend that XMAX be a small int multiple of 
//            Math.sqrt(1/eps), where eps is the smallest positive number 
//            such that  1+eps > 1. 
//   XSMALL = positive argument such that  1.0-(1/2)(X/2)**2 = 1.0 
//            to machine precision for all  ABS(X) <= XSMALL. 
//            We recommend that  XSMALL < Math.sqrt(eps)/beta, where beta 
//            is the floating-point radix (usually 2 or 16). 
//
//     Approximate values for some important machines are 
//
//                          eps      XMAX     XSMALL      XINF 
//
//  CDC 7600      (S.P.)  7.11E-15  1.34E+08  2.98E-08  1.26E+322 
//  CRAY-1        (S.P.)  7.11E-15  1.34E+08  2.98E-08  5.45E+2465 
//  IBM PC (8087) (S.P.)  5.96E-08  8.19E+03  1.22E-04  3.40E+38 
//  IBM PC (8087) (D.P.)  1.11D-16  2.68D+08  3.72D-09  1.79D+308 
//  IBM 195       (D.P.)  2.22D-16  6.87D+09  9.09D-13  7.23D+75 
//  UNIVAC 1108   (D.P.)  1.73D-18  4.30D+09  2.33D-10  8.98D+307 
//  VAX 11/780    (D.P.)  1.39D-17  1.07D+09  9.31D-10  1.70D+38 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Error Returns 
//
//  The program returns the value 0.0 for  X > XMAX, and returns 
//    -XINF when BESLY1 is called with a negative or 0.0 argument. 
//
//
//  Author: W. J. Cody 
//          Mathematics and Computer Science Division 
//          Argonne National Laboratory 
//          Argonne, IL 60439 
//
//  Latest modification: November 10, 1987 
//
//  Translated to C from FORTRAN and adapted for Java
//  by Michael Kiefte, Dalhousie University, June 10, 2002.
//
// -------------------------------------------------------------------- 

// ------------------------------------------------------------------- 
//  Check for error conditions 
// ------------------------------------------------------------------- 
	ax = Math.abs(arg);
	if (jint == 1 && (arg <= 0.0 || arg < 0.5 && ax * XINF < PI2)) {
	    result = Double.NEGATIVE_INFINITY;
	    return result;
	} else if (ax > XMAX) {
	    result = 0.0;
	    return result;
	} else if (ax > 8.0) {
// ------------------------------------------------------------------- 
//  Calculate J1 or Y1 for |ARG|  >  8.0 
// ------------------------------------------------------------------- 
	    z = 8.0 / ax;
	    w = (double) ((int) (ax / TWOPI)) + 0.375;
	    w = ax - w * TWOPI1 - w * TWOPI2;
	    zsq = z * z;
	    xnum = p0[5];
	    xden = zsq + q0[5];
	    up = p1[5];
	    down = zsq + q1[5];
	    for (int i = 0; i < 5; ++i) {
		xnum = xnum * zsq + p0[i];
		xden = xden * zsq + q0[i];
		up = up * zsq + p1[i];
		down = down * zsq + q1[i];
	    }
	    r0 = xnum / xden;
	    r1 = up / down;
	    if (jint == 0) {
		result = RTPI2 / Math.sqrt(ax) * (r0 * Math.cos(w)
			- z * r1 * Math.sin(w));
	    } else {
		result = RTPI2 / Math.sqrt(ax) * (r0 * Math.sin(w)
			+ z * r1 * Math.cos(w));
	    }
	    if (jint == 0 && arg < 0.0) {
		result = -result;
	    }
	    return result;
	} else if (ax <= XSMALL) {
	    if (jint == 0) {
		result = arg * 0.5;
	    } else {
		result = -PI2 / ax;
	    }
	    return result;
	} else {
// ------------------------------------------------------------------- 
//  Calculate J1 for appropriate interval, preserving 
//     accuracy near the zero of J1 
// ------------------------------------------------------------------- 
	    zsq = ax * ax;
	    if (ax <= 4.0) {
		xnum = (pj0[6] * zsq + pj0[5]) * zsq + pj0[4];
		xden = zsq + qj0[4];
		for (int i = 0; i < 4; ++i) {
		    xnum = xnum * zsq + pj0[i];
		    xden = xden * zsq + qj0[i];
		}
		prod = arg * (ax - XJ01 / 256.0 - XJ02) * (ax + XJ0);
	    } else {
		xnum = pj1[0];
		xden = (zsq + qj1[6]) * zsq + qj1[0];
		for (int i = 1; i < 6; ++i) {
		    xnum = xnum * zsq + pj1[i];
		    xden = xden * zsq + qj1[i];
		}
		xnum = xnum * (ax - 8.0) * (ax + 8.0) + pj1[6];
		xnum = xnum * (ax - 4.0) * (ax + 4.0) + pj1[7];
		prod = arg * (ax - XJ11 / 256.0 - XJ12) * (ax + XJ1);
	    }
	    result = prod * (xnum / xden);
	    if (jint == 0) {
		return result;
	    }
// ------------------------------------------------------------------- 
//  Calculate Y1.  First find  RESJ = pi/2 ln(x/xn) J1(x), 
//    where xn is a zero of Y1 
// ------------------------------------------------------------------- 
	    if (ax <= 4.0) {
		up = ax - XY01 / 256.0 - XY02;
		xy = XY0;
	    } else {
		up = ax - XY11 / 256.0 - XY12;
		xy = XY1;
	    }
	    down = ax + xy;
	    if (Math.abs(up) < P17 * down) {
		w = up / down;
		wsq = w * w;
		xnum = plg[0];
		xden = wsq + qlg[0];
		for (int i = 1; i < 4; ++i) {
		    xnum = xnum * wsq + plg[i];
		    xden = xden * wsq + qlg[i];
		}
		resj = PI2 * result * w * xnum / xden;
	    } else {
		resj = PI2 * result * Math.log(ax / xy);
	    }
// ------------------------------------------------------------------- 
//  Now calculate Y1 for appropriate interval, preserving 
//     accuracy near the zero of Y1 
// ------------------------------------------------------------------- 
	    if (ax <= 4.0) {
		xnum = py0[6] * zsq + py0[0];
		xden = zsq + qy0[0];
		for (int i = 1; i < 6; ++i) {
		    xnum = xnum * zsq + py0[i];
		    xden = xden * zsq + qy0[i];
		}
	    } else {
		xnum = py1[8] * zsq + py1[0];
		xden = zsq + qy1[0];
		for (int i = 1; i < 8; ++i) {
		    xnum = xnum * zsq + py1[i];
		    xden = xden * zsq + qy1[i];
		}
	    }
	    result = resj + up * down / ax * xnum / xden;
	    return result;
	}
    } // caljy1 

    public static double besj1(double x)
    {
// -------------------------------------------------------------------- 
//
// This subprogram computes approximate values for Bessel functions 
//   of the first kind of order 0.0 for arguments  |X| <= XMAX 
//   (see comments heading CALJY1). 
//
// -------------------------------------------------------------------- 
	return caljy1(x, 0);
    } // besj1 

    public static double besy1(double x)
    {
// -------------------------------------------------------------------- 
//
// This subprogram computes approximate values for Bessel functions 
//   of the second kind of order 0.0 for arguments 0 < X <= XMAX 
//   (see comments heading CALJY1). 
//
// -------------------------------------------------------------------- 
	return caljy1(x, 1);
    } // besy1 

    private static double calck0(double arg, int jint)
    {
	double result;

	// Initialized data 

	final double[] pp = { 113.94980557384778174, 3683.258995734026794,
		31075.408980684392399, 105770.68948034021957,
		173988.67902565686251, 150976.46353289914539,
		71557.062783764037541, 18321.525870183537725,
		2344.4738764199315021, 116.00249425076035558 };
	final double[] qq = { 200.13443064949242491, 4432.9628889746408858,
		31474.655750295278825, 97418.829762268075784,
		151446.44673520157801, 126898.39587977598727,
		58824.616785857027752, 14847.228371802360957,
		1882.1890840982713696, 92.556599177304839811 };
	final double XSMALL = 1.11e-16;
	final double XMAX = 705.342;
	final double[] p = { 5.85992214128261e-4, 0.1316605256498957185,
		11.999463724910714109, 468.50901201934832188,
		5916.9059852270512312, 2470.8152720399552679 };
	final double[] q = { -249.94418972832303646, 21312.71430384912038 };
	final double[] f = { -1.64144528372990641, -296.01657892958843866,
		-17733.784684952985886, -403203.40761145482298 };
	final double[] g = { -250.6497244587799273, 29865.713163054025489,
		-1612813.6304458193998 };

	// Local variables 
	double temp, sumf, sumg, sump, sumq;
	double x, xx;

// -------------------------------------------------------------------- 
//
// This packet computes modified Bessel functions of the second kind 
//   and order zero, K0(X) and EXP(X)*K0(X), for real 
//   arguments X.  It contains two function type subprograms, BESK0 
//   and BESEK0, and one subroutine type subprogram, CALCK0. 
//   the calling statements for the primary entries are 
//
//                   Y=BESK0(X) 
//   and 
//                   Y=BESEK0(X) 
//
//   where the entry points correspond to the functions K0(X) and 
//   EXP(X)*K0(X), respectively.  The routine CALCK0 is 
//   intended for internal packet use only, all computations within 
//   the packet being concentrated in this routine.  The function 
//   subprograms invoke CALCK0 with the statement 
//          RESULT = CALCK0(ARG,JINT) 
//   where the parameter usage is as follows 
//
//      Function                     Parameters for CALCK0 
//       Call              ARG                  RESULT          JINT 
//
//     BESK0(ARG)   0 < ARG <= XMAX            K0(ARG)           1 
//     BESEK0(ARG)     0 < ARG              EXP(ARG)*K0(ARG)     2 
//
//   The main computation evaluates slightly modified forms of near 
//   minimax rational approximations generated by Russon and Blair, 
//   Chalk River (Atomic Energy of Canada Limited) Report AECL-3461, 
//   1969.  This transportable program is patterned after the 
//   machine-dependent FUNPACK packet NATSK0, but cannot match that 
//   version for efficiency or accuracy.  This version uses rational 
//   functions that theoretically approximate K-SUB-0(X) to at 
//   least 18 significant decimal digits.  The accuracy achieved 
//   depends on the arithmetic system, the compiler, the intrinsic 
//   functions, and proper selection of the machine-dependent 
//   constants. 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Explanation of machine-dependent constants 
//
//   beta   = Radix for the floating-point system 
//   minexp = Smallest representable power of beta 
//   maxexp = Smallest power of beta that overflows 
//   XSMALL = Argument below which BESK0 and BESEK0 may 
//            each be represented by a constant and a Math.log. 
//            largest X such that  1.0 + X = 1.0  to machine 
//            precision. 
//   XINF   = Largest positive machine number; approximately 
//            beta**maxexp 
//   XMAX   = Largest argument acceptable to BESK0;  Solution to 
//            equation: 
//               W(X) * (1-1/8X+9/128X**2) = beta**minexp 
//            where  W(X) = EXP(-X)*SQRT(PI/2X) 
//
//
//     Approximate values for some important machines are: 
//
//
//                           beta       minexp       maxexp 
//
//  CRAY-1        (S.P.)       2        -8193         8191 
//  Cyber 180/185 
//    under NOS   (S.P.)       2         -975         1070 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (S.P.)       2         -126          128 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (D.P.)       2        -1022         1024 
//  IBM 3033      (D.P.)      16          -65           63 
//  VAX D-Format  (D.P.)       2         -128          127 
//  VAX G-Format  (D.P.)       2        -1024         1023 
//
//
//                          XSMALL       XINF         XMAX 
//
// CRAY-1        (S.P.)    3.55E-15   5.45E+2465    5674.858 
// Cyber 180/855 
//   under NOS   (S.P.)    1.77E-15   1.26E+322      672.788 
// IEEE (IBM/XT, 
//   SUN, etc.)  (S.P.)    5.95E-8    3.40E+38        85.337 
// IEEE (IBM/XT, 
//   SUN, etc.)  (D.P.)    1.11D-16   1.79D+308      705.342 
// IBM 3033      (D.P.)    1.11D-16   7.23D+75       177.852 
// VAX D-Format  (D.P.)    6.95D-18   1.70D+38        86.715 
// VAX G-Format  (D.P.)    5.55D-17   8.98D+307      706.728 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Error returns 
//
//  The program returns the value XINF for ARG <= 0.0, and the 
//  BESK0 entry returns the value 0.0 for ARG > XMAX. 
//
//
//  Latest modification: March 19, 1990 
//
//  Authors: W. J. Cody and Laura Stoltz 
//           Mathematics and Computer Science Division 
//           Argonne National Laboratory 
//           Argonne, IL 60439 
//
//  Translated to C from FORTRAN and adapted for Java
//  by Michael Kiefte, Dalhousie University, June 15, 2002.
//
// -------------------------------------------------------------------- 
	x = arg;
	if (x > 0.0) {
	    if (x <= 1.0) {
// -------------------------------------------------------------------- 
//     0.0 <  ARG  <= 1.0 
// -------------------------------------------------------------------- 
		temp = Math.log(x);
		if (x < XSMALL) {
// -------------------------------------------------------------------- 
//     Return for small ARG 
// -------------------------------------------------------------------- 
		    result = p[5] / q[1] - temp;
		} else {
		    xx = x * x;
		    sump = ((((p[0] * xx + p[1]) * xx + p[2]) * xx + p[3])
			    * xx + p[4]) * xx + p[5];
		    sumq = (xx + q[0]) * xx + q[1];
		    sumf = ((f[0] * xx + f[1]) * xx + f[2]) * xx + f[3];
		    sumg = ((xx + g[0]) * xx + g[1]) * xx + g[2];
		    result = sump / sumq - xx * sumf * temp / sumg - temp;
		    if (jint == 2) {
			result *= Math.exp(x);
		    }
		}
	    } else if (jint == 1 && x > XMAX) {
// -------------------------------------------------------------------- 
//     Error return for ARG > XMAX 
// -------------------------------------------------------------------- 
		result = 0.0;
	    } else {
// -------------------------------------------------------------------- 
//     1.0 < ARG 
// -------------------------------------------------------------------- 
		xx = 1.0 / x;
		sump = pp[0];
		for (int i = 1; i < 10; ++i) {
		    sump = sump * xx + pp[i];
		}
		sumq = xx;
		for (int i = 0; i < 9; ++i) {
		    sumq = (sumq + qq[i]) * xx;
		}
		sumq += qq[9];
		result = sump / sumq / Math.sqrt(x);
		if (jint == 1) {
		    result *= Math.exp(-x);
		}
	    }
	} else {
// -------------------------------------------------------------------- 
//     Error return for ARG <= 0.0 
// -------------------------------------------------------------------- 
	    result = Double.POSITIVE_INFINITY;
	}
// -------------------------------------------------------------------- 
//     Update error counts, etc. 
// -------------------------------------------------------------------- 
	return result;
    } // calck0 

    public static double besk0(double x)
    {
// -------------------------------------------------------------------- 
//
// This function program computes approximate values for the 
//   modified Bessel function of the second kind of order zero 
//   for arguments 0.0 < ARG <= XMAX (see comments heading 
//   CALCK0). 
//
//  Authors: W. J. Cody and Laura Stoltz 
//
//  Latest Modification: January 19, 1988 
//
// -------------------------------------------------------------------- 
	return calck0(x, 1);
    } // besk0 

    public static double besek0(double x)
    {
// -------------------------------------------------------------------- 
//
// This function program computes approximate values for the 
//   modified Bessel function of the second kind of order zero 
//   multiplied by the Exponential function, for arguments 
//   0.0 < ARG. 
//
//  Authors: W. J. Cody and Laura Stoltz 
//
//  Latest Modification: January 19, 1988 
//
// -------------------------------------------------------------------- 
	return calck0(x, 2);
    } // besek0 

    private static double calck1(double x, int jint)
    {
	double result;

	// Initialized data 

	final double[] g = { -305.07151578787595807, 43117.653211351080007,
		-2706232.2985570842656 };
	final double[] pp = { 0.064257745859173138767,
		7.558458463117603081, 131.82609918569941308,
		810.94256146537402173, 2312.374220916887155,
		3454.0675585544584407, 2859.0657697910288226,
		1331.948643318322199, 341.2295348680131291,
		44.137176114230414036, 2.2196792496874548962 };
	final double[] qq = { 36.001069306861518855,
		330.31020088765390854, 1208.2692316002348638,
		2118.100048717194381, 1944.8440788918006154,
		969.29165726802648634, 259.51223655579051357,
		34.552228452758912848, 1.7710478032601086579 };
	final double XLEAST = 2.23e-308;
	final double XSMALL = 1.11e-16;
	final double XMAX = 705.343;
	final double[] p = { 0.4812707045687844231, 99.991373567429309922,
		7188.5382604084798576, 177333.2403514701563,
		719389.20065420586101 } ;
	final double[] q = { -281.43915754538725829, 37264.298672067697862,
		-2214937.4878243304548 };
	final double[] f = { -0.2279559082695500239, -53.103913335180275253,
		-4505.1623763436087023, -147580.69205414222471,
		-1353116.1492785421328 };

	// Local variables 
	double sumf, sumg, sump, sumq;
	double xx;

// -------------------------------------------------------------------- 
//
// This packet computes modified Bessel functions of the second kind 
//   and order one,  K1(X)  and  EXP(X)*K1(X), for real arguments X. 
//   It contains two function type subprograms, BESK1  and  BESEK1, 
//   and one subroutine type subprogram, CALCK1.  The calling 
//   statements for the primary entries are 
//
//                   Y=BESK1(X) 
//   and 
//                   Y=BESEK1(X) 
//
//   where the entry points correspond to the functions K1(X) and 
//   EXP(X)*K1(X), respectively.  The routine CALCK1 is intended 
//   for internal packet use only, all computations within the 
//   packet being concentrated in this routine.  The function 
//   subprograms invoke CALCK1 with the statement 
//          RESULT = CALCK1(ARG,JINT) 
//   where the parameter usage is as follows 
//
//      Function                      Parameters for CALCK1 
//        Call             ARG                  RESULT          JINT 
//
//     BESK1(ARG)  XLEAST < ARG < XMAX          K1(ARG)          1 
//     BESEK1(ARG)     XLEAST < ARG          EXP(ARG)*K1(ARG)    2 
//
//   The main computation evaluates slightly modified forms of near 
//   minimax rational approximations generated by Russon and Blair, 
//   Chalk River (Atomic Energy of Canada Limited) Report AECL-3461, 
//   1969.  This transportable program is patterned after the 
//   machine-dependent FUNPACK packet NATSK1, but cannot match that 
//   version for efficiency or accuracy.  This version uses rational 
//   functions that theoretically approximate K-SUB-1(X) to at 
//   least 18 significant decimal digits.  The accuracy achieved 
//   depends on the arithmetic system, the compiler, the intrinsic 
//   functions, and proper selection of the machine-dependent 
//   constants. 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Explanation of machine-dependent constants 
//
//   beta   = Radix for the floating-point system 
//   minexp = Smallest representable power of beta 
//   maxexp = Smallest power of beta that overflows 
//   XLEAST = Smallest acceptable argument, i.e., smallest machine 
//            number X such that 1/X is machine representable. 
//   XSMALL = Argument below which BESK1(X) and BESEK1(X) may 
//            each be represented by 1/X.  A safe value is the 
//            largest X such that  1.0 + X = 1.0  to machine 
//            precision. 
//   XINF   = Largest positive machine number; approximately 
//            beta**maxexp 
//   XMAX   = Largest argument acceptable to BESK1;  Solution to 
//            equation: 
//               W(X) * (1+3/8X-15/128X**2) = beta**minexp 
//            where  W(X) = EXP(-X)*SQRT(PI/2X) 
//
//
//     Approximate values for some important machines are: 
//
//                           beta       minexp       maxexp 
//
//  CRAY-1        (S.P.)       2        -8193         8191 
//  Cyber 180/185 
//    under NOS   (S.P.)       2         -975         1070 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (S.P.)       2         -126          128 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (D.P.)       2        -1022         1024 
//  IBM 3033      (D.P.)      16          -65           63 
//  VAX D-Format  (D.P.)       2         -128          127 
//  VAX G-Format  (D.P.)       2        -1024         1023 
//
//
//                         XLEAST     XSMALL      XINF       XMAX 
//
// CRAY-1                1.84E-2466  3.55E-15  5.45E+2465  5674.858 
// Cyber 180/855 
//   under NOS   (S.P.)  3.14E-294   1.77E-15  1.26E+322    672.789 
// IEEE (IBM/XT, 
//   SUN, etc.)  (S.P.)  1.18E-38    5.95E-8   3.40E+38      85.343 
// IEEE (IBM/XT, 
//   SUN, etc.)  (D.P.)  2.23D-308   1.11D-16  1.79D+308    705.343 
// IBM 3033      (D.P.)  1.39D-76    1.11D-16  7.23D+75     177.855 
// VAX D-Format  (D.P.)  5.88D-39    6.95D-18  1.70D+38      86.721 
// VAX G-Format  (D.P.)  1.12D-308   5.55D-17  8.98D+307    706.728 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Error returns 
//
//  The program returns the value XINF for ARG <= 0.0 and the 
//   BESK1 entry returns the value 0.0 for ARG > XMAX. 
//
//
//  Intrinsic functions required are: 
//
//     LOG, SQRT, EXP 
//
//
//  Authors: W. J. Cody and Laura Stoltz 
//           Mathematics and Computer Science Division 
//           Argonne National Laboratory 
//           Argonne, IL 60439 
//
//  Latest modification: January 28, 1988 
//
//  Translated to C from FORTRAN and adapted for Java
//  by Michael Kiefte, Dalhousie University, June 15, 2002.
//
// -------------------------------------------------------------------- 

	if (x < XLEAST) {
// -------------------------------------------------------------------- 
//  Error return for  ARG  < XLEAST 
// -------------------------------------------------------------------- 
	    result = Double.POSITIVE_INFINITY;
	} else if (x <= 1.0) {
// -------------------------------------------------------------------- 
//  XLEAST <=  ARG  <= 1.0 
// -------------------------------------------------------------------- 
	    if (x < XSMALL) {
// -------------------------------------------------------------------- 
//  Return for small ARG 
// -------------------------------------------------------------------- 
		result = 1.0 / x;
	    } else {
		xx = x * x;
		sump = ((((p[0] * xx + p[1]) * xx + p[2]) * xx + p[3])
			* xx + p[4]) * xx + q[2];
		sumq = ((xx + q[0]) * xx + q[1]) * xx + q[2];
		sumf = (((f[0] * xx + f[1]) * xx + f[2]) * xx + f[3])
			* xx + f[4];
		sumg = ((xx + g[0]) * xx + g[1]) * xx + g[2];
		result = (xx * Math.log(x) * sumf / sumg + sump / sumq) / x;
		if (jint == 2) {
		    result *= Math.exp(x);
		}
	    }
	} else if (jint == 1 && x > XMAX) {
// -------------------------------------------------------------------- 
//  Error return for  ARG  > XMAX 
// -------------------------------------------------------------------- 
	    result = 0.0;
	} else {
// -------------------------------------------------------------------- 
//  1.0 <  ARG 
// -------------------------------------------------------------------- 
	    xx = 1.0 / x;
	    sump = pp[0];
	    for (int i = 1; i < 11; ++i) {
		sump = sump * xx + pp[i];
	    }
	    sumq = xx;
	    for (int i = 0; i < 8; ++i) {
		sumq = (sumq + qq[i]) * xx;
	    }
	    sumq += qq[8];
	    result = sump / sumq / Math.sqrt(x);
	    if (jint == 1) {
		result *= Math.exp(-x);
	    }
	}
	return result;
    } // calck1 

    public static double besk1(double x)
    {
// -------------------------------------------------------------------- 
//
// This function program computes approximate values for the 
//   modified Bessel function of the second kind of order 1.0 
//   for arguments  XLEAST <= ARG <= XMAX. 
//
// -------------------------------------------------------------------- 
	return calck1(x, 1);
    } // besk1 

    public static double besek1(double x)
    {
// -------------------------------------------------------------------- 
//
// This function program computes approximate values for the 
//   modified Bessel function of the second kind of order 1.0 
//   multiplied by the exponential function, for arguments 
//   XLEAST <= ARG <= XMAX. 
//
// -------------------------------------------------------------------- 
	return calck1(x, 2);
    } // besek1 

    public static double psi(double x)
    {
	// Initialized data 

	final double XMAX1 = 4.50e+15;
	final double XSMALL = 5.80e-09;
	final double XLARGE = 2.71e+14;
	final double X01 = 187.0;
	final double X01D = 128.0;
	final double X02 = 6.9464496836234126266e-4;
	final double[] p1 = { 0.004510468124576293416,
		5.4932855833000385356, 376.46693175929276856,
		7952.5490849151998065, 71451.59581895193321,
		306559.76301987365674, 636069.97788964458797,
		580413.12783537569993, 165856.95029761022321 };
	final double[] q1 = { 96.141654774222358525, 2628.771579058119333,
		29862.49702225027792, 162065.66091533671639,
		434878.80712768329037, 542563.84537269993733,
		242421.85002017985252, 6.4155223783576225996e-8 };
	final double[] p2 = { -2.7103228277757834192, -15.166271776896121383,
		-19.784554148719218667, -8.8100958828312219821,
		-1.4479614616899842986, -0.073689600332394549911,
		-6.5135387732718171306e-21 };
	final double[] q2 = { 44.992760373789365846,
		202.40955312679931159, 247.36979003315290057,
		107.42543875702278326, 17.463965060678569906,
		0.88427520398873480342 };
	final double PIOV4 = 0.78539816339744830962;
	final double XMIN1 = 2.23e-308;

	// Local variables 
	int n;
	double w, z, upper;
	int nq;
	double den, aug, sgn;

// ---------------------------------------------------------------------- 
//
// This function program evaluates the logarithmic derivative of the 
//   gamma function, 
//
//      psi(x) = d/dx (gamma(x)) / gamma(x) = d/dx (ln gamma(x)) 
//
//   for real x, where either 
//
//          -XMAX1 < x < -XMIN (x not a negative int), or 
//            XMIN < x. 
//
//   The calling sequence for this function is 
//
//                  Y = PSI(X) 
//
//   The main computation uses rational Chebyshev approximations 
//   published in Math. Comp. 27, 123-127 (1973) by Cody, Strecok and 
//   Thacher.  This transportable program is patterned after the 
//   machine-dependent FUNPACK program PSI(X), but cannot match that 
//   version for efficiency or accuracy.  This version uses rational 
//   approximations that are theoretically accurate to 20 significant 
//   decimal digits.  The accuracy achieved depends on the arithmetic 
//   system, the compiler, the intrinsic functions, and proper selection 
//   of the machine-dependent constants. 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Explanation of machine-dependent constants 
//
//   XINF   = largest positive machine number 
//   XMAX1  = beta ** (p-1), where beta is the radix for the 
//            floating-point system, and p is the number of base-beta 
//            digits in the floating-point significand.  This is an 
//            upper bound on non-integral floating-point numbers, and 
//            the negative of the lower bound on acceptable negative 
//            arguments for PSI.  If rounding is necessary, round this 
//            value down. 
//   XMIN1  = the smallest in magnitude acceptable argument.  We 
//            recommend XMIN1 = MAX(1/XINF,xmin) rounded up, where 
//            xmin is the smallest positive floating-point number. 
//   XSMALL = absolute argument below which  PI*COTAN(PI*X)  may be 
//            represented by 1/X.  We recommend XSMALL < sqrt(3 eps)/pi, 
//            where eps is the smallest positive number such that 
//            1+eps > 1. 
//   XLARGE = argument beyond which PSI(X) may be represented by 
//            LOG(X).  The solution to the equation 
//               x*ln(x) = beta ** p 
//            is a safe value. 
//
//     Approximate values for some important machines are 
//
//                        beta  p     eps     xmin       XINF 
//
//  CDC 7600      (S.P.)    2  48  7.11E-15  3.13E-294  1.26E+322 
//  CRAY-1        (S.P.)    2  48  7.11E-15  4.58E-2467 5.45E+2465 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (S.P.)    2  24  1.19E-07  1.18E-38   3.40E+38 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (D.P.)    2  53  1.11D-16  2.23E-308  1.79D+308 
//  IBM 3033      (D.P.)   16  14  1.11D-16  5.40D-79   7.23D+75 
//  SUN 3/160     (D.P.)    2  53  1.11D-16  2.23D-308  1.79D+308 
//  VAX 11/780    (S.P.)    2  24  5.96E-08  2.94E-39   1.70E+38 
//                (D.P.)    2  56  1.39D-17  2.94D-39   1.70D+38 
//   (G Format)   (D.P.)    2  53  1.11D-16  5.57D-309  8.98D+307 
//
//                         XMIN1      XMAX1     XSMALL    XLARGE 
//
//  CDC 7600      (S.P.)  3.13E-294  1.40E+14  4.64E-08  9.42E+12 
//  CRAY-1        (S.P.)  1.84E-2466 1.40E+14  4.64E-08  9.42E+12 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (S.P.)  1.18E-38   8.38E+06  1.90E-04  1.20E+06 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (D.P.)  2.23D-308  4.50D+15  5.80D-09  2.71D+14 
//  IBM 3033      (D.P.)  1.39D-76   4.50D+15  5.80D-09  2.05D+15 
//  SUN 3/160     (D.P.)  2.23D-308  4.50D+15  5.80D-09  2.71D+14 
//  VAX 11/780    (S.P.)  5.89E-39   8.38E+06  1.35E-04  1.20E+06 
//                (D.P.)  5.89D-39   3.60D+16  2.05D-09  2.05D+15 
//   (G Format)   (D.P.)  1.12D-308  4.50D+15  5.80D-09  2.71D+14 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Error Returns 
//
//  The program returns XINF for  X < -XMAX1, for X 0.0 or a negative 
//    int, or when X lies in (-XMIN1, 0), and returns -XINF 
//    when X lies in (0, XMIN1). 
//
//  Author: W. J. Cody 
//          Mathematics and Computer Science Division 
//          Argonne National Laboratory 
//          Argonne, IL 60439 
//
//  Latest modification: June 8, 1988 
//
//  Translated to C from FORTRAN and adapted for Java
//  by Michael Kiefte, Dalhousie University, June 16, 2002.
//
// -------------------------------------------------------------------- 

	w = Math.abs(x);
	aug = 0.0;
// ---------------------------------------------------------------------- 
//  Check for valid arguments, then branch to appropriate algorithm 
// ---------------------------------------------------------------------- 
	if (-x >= XMAX1 || w < XMIN1) {
// ---------------------------------------------------------------------- 
//  Error return 
// ---------------------------------------------------------------------- 
	    return x > 0.0 ? Double.NEGATIVE_INFINITY :
		    Double.POSITIVE_INFINITY;
	} else if (x < 0.5) {
// ---------------------------------------------------------------------- 
//  X < 0.5, use reflection formula: psi(1-x) = psi(x) + pi * cot(pi*x) 
//     Use 1/X for PI*COTAN(PI*X)  when  XMIN1 < |X| <= XSMALL. 
// ---------------------------------------------------------------------- 
	    if (w <= XSMALL) {
		aug = -1.0 / x;
	    } else {
// ---------------------------------------------------------------------- 
//  Argument reduction for cot 
// ---------------------------------------------------------------------- 
		if (x < 0.0) {
		    sgn = PIOV4;
		} else {
		    sgn = -PIOV4;
		}
		w -= (double) ((int) w);
		nq = (int) (w * 4.0);
		w = 4.0 * (w - (double) nq * 0.25);
// ---------------------------------------------------------------------- 
//  W is now related to the fractional part of  4.0 * X. 
//     Adjust argument to correspond to values in the first 
//     quadrant and determine the sign. 
// ---------------------------------------------------------------------- 
		n = nq >> 1; /* n = nq / 2 */
		if (n << 1 != nq) { /* if (n + n != nq) */
		    w = 1.0 - w;
		}
		z = PIOV4 * w;
		if ((n & 1) != 0) { /* if (n % 2 != 0) */
		    sgn = -sgn;
		}
// ---------------------------------------------------------------------- 
//  determine the value for  -pi * cotan(pi*x) 
// ---------------------------------------------------------------------- 
		n = (nq + 1) >> 1; /* n = (nq + 1) / 2 */
		if ((n & 1) == 0) { /* if (n % 2 == 0) */
// ---------------------------------------------------------------------- 
//  Check for singularity 
// ---------------------------------------------------------------------- 
		    if (z == 0.0) {
// ---------------------------------------------------------------------- 
//  Error return 
// ---------------------------------------------------------------------- 
			return x > 0.0 ? Double.NEGATIVE_INFINITY :
				Double.POSITIVE_INFINITY;
		    }
		    aug = sgn * (4.0 / Math.tan(z));
		} else {
		    aug = sgn * (4.0 * Math.tan(z));
		}
	    }
	    x = 1.0 - x;
	}
	if (x > 3.0) {
// ---------------------------------------------------------------------- 
//  3.0 < X 
// ---------------------------------------------------------------------- 
	    if (x < XLARGE) {
		w = 1.0 / (x * x);
		den = w;
		upper = p2[0] * w;
		for (int i = 1; i <= 5; ++i) {
		    den = (den + q2[i - 1]) * w;
		    upper = (upper + p2[i]) * w;
		}
		aug = (upper + p2[6]) / (den + q2[5]) - 0.5 / x + aug;
	    }
	    return aug + Math.log(x);
	} else {
// ---------------------------------------------------------------------- 
//  0.5 <= X <= 3.0 
// ---------------------------------------------------------------------- 
	    den = x;
	    upper = p1[0] * x;
	    for (int i = 1; i <= 7; ++i) {
		den = (den + q1[i - 1]) * x;
		upper = (upper + p1[i]) * x;
	    }
	    den = (upper + p1[8]) / (den + q1[7]);
	    x = x - X01 / X01D - X02;
	    return den * x + aug;
	}
    } // psi 

    public static int ribesl(double x, double alpha, int ize, double[] b)
    {
	int ncalc;
	int nb = b.length;

	// Initialized data 

	final double one = 1.0;
	final double ensig = 1e16;
	final double rtnsig = 1e-4;
	final double enmten = 8.9e-308;
	final double two = 2.0;
	final double zero = 0.0;
	final double half = 0.5;
	final double const__ = 1.585;
	final int nsig = 16;
	final double xlarge = 1e4;
	final double exparg = 709.0;
	final double enten = 1e308;

	// System generated locals 
	int i__1, i__2;
	double d__1, d__2, d__3;

	// Local variables 
	int nend, magx;
	double pold;
	int nbmx;
	double test;
	int k, l, n;
	double p, empal, halfx, tempa, tempb, tempc, psave, plast, 
		tover, emp2al, em, en;
	double psavel;
	int nstart;
	double sum;

// ------------------------------------------------------------------- 
//
//  This routine calculates Bessel functions I SUB(N+ALPHA) (X) 
//  for non-negative argument X, and non-negative order N+ALPHA, 
//  with or without exponential scaling. 
//
//
// Explanation of variables in the calling sequence 
//
// X     - Working precision non-negative real argument for which 
//         I's or exponentially scaled I's (I*EXP(-X)) 
//         are to be calculated.  If I's are to be calculated, 
//         X must be less than EXPARG (see below). 
// ALPHA - Working precision fractional part of order for which 
//         I's or exponentially scaled I's (I*EXP(-X)) are 
//         to be calculated.  0 <= ALPHA < 1.0. 
// NB    - Integer number of functions to be calculated, NB > 0. 
//         The first function calculated is of order ALPHA, and the 
//         last is of order (NB - 1 + ALPHA). 
// IZE   - Integer type.  IZE = 1 if unscaled I's are to calculated, 
//         and 2 if exponentially scaled I's are to be calculated. 
// B     - Working precision output vector of length NB.  If the routine 
//         terminates normally (NCALC=NB), the vector B contains the 
//         functions I(ALPHA,X) through I(NB-1+ALPHA,X), or the 
//         corresponding exponentially scaled functions. 
// NCALC - Integer output variable indicating possible errors. 
//         Before using the vector B, the user should check that 
//         NCALC=NB, i.e., all orders have been calculated to 
//         the desired accuracy.  See error returns below. 
//
//
// ******************************************************************* 
// ******************************************************************* 
//
// Explanation of machine-dependent constants 
//
//   beta   = Radix for the floating-point system 
//   minexp = Smallest representable power of beta 
//   maxexp = Smallest power of beta that overflows 
//   it     = Number of bits in the mantissa of a working precision 
//            variable 
//   NSIG   = Decimal significance desired.  Should be set to 
//            INT(LOG10(2)*it+1).  Setting NSIG lower will result 
//            in decreased accuracy while setting NSIG higher will 
//            increase CPU time without increasing accuracy.  The 
//            truncation error is limited to a relative error of 
//            T=0.5*10**(-NSIG). 
//   ENTEN  = 10.0 ** K, where K is the largest int such that 
//            ENTEN is machine-representable in working precision 
//   ENSIG  = 10.0 ** NSIG 
//   RTNSIG = 10.0 ** (-K) for the smallest int K such that 
//            K >= NSIG/4 
//   ENMTEN = Smallest ABS(X) such that X/4 does not underflow 
//   XLARGE = Upper limit on the magnitude of X when IZE=2.  Bear 
//            in mind that if ABS(X)=N, then at least N iterations 
//            of the backward recursion will be executed.  The value 
//            of 10.0 ** 4 is used on every machine. 
//   EXPARG = Largest working precision argument that the library 
//            EXP routine can handle and upper limit on the 
//            magnitude of X when IZE=1; approximately 
//            LOG(beta**maxexp) 
//
//
//     Approximate values for some important machines are: 
//
//                        beta       minexp      maxexp       it 
//
//  CRAY-1        (S.P.)    2        -8193        8191        48 
//  Cyber 180/855 
//    under NOS   (S.P.)    2         -975        1070        48 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (S.P.)    2         -126         128        24 
//  IEEE (IBM/XT, 
//    SUN, etc.)  (D.P.)    2        -1022        1024        53 
//  IBM 3033      (D.P.)   16          -65          63        14 
//  VAX           (S.P.)    2         -128         127        24 
//  VAX D-Format  (D.P.)    2         -128         127        56 
//  VAX G-Format  (D.P.)    2        -1024        1023        53 
//
//
//                        NSIG       ENTEN       ENSIG      RTNSIG 
//
// CRAY-1        (S.P.)    15       1.0E+2465   1.0E+15     1.0E-4 
// Cyber 180/855 
//   under NOS   (S.P.)    15       1.0E+322    1.0E+15     1.0E-4 
// IEEE (IBM/XT, 
//   SUN, etc.)  (S.P.)     8       1.0E+38     1.0E+8      1.0E-2 
// IEEE (IBM/XT, 
//   SUN, etc.)  (D.P.)    16       1.0D+308    1.0D+16     1.0D-4 
// IBM 3033      (D.P.)     5       1.0D+75     1.0D+5      1.0D-2 
// VAX           (S.P.)     8       1.0E+38     1.0E+8      1.0E-2 
// VAX D-Format  (D.P.)    17       1.0D+38     1.0D+17     1.0D-5 
// VAX G-Format  (D.P.)    16       1.0D+307    1.0D+16     1.0D-4 
//
//
//                         ENMTEN      XLARGE   EXPARG 
//
// CRAY-1        (S.P.)   1.84E-2466   1.0E+4    5677 
// Cyber 180/855 
//   under NOS   (S.P.)   1.25E-293    1.0E+4     741 
// IEEE (IBM/XT, 
//   SUN, etc.)  (S.P.)   4.70E-38     1.0E+4      88 
// IEEE (IBM/XT, 
//   SUN, etc.)  (D.P.)   8.90D-308    1.0D+4     709 
// IBM 3033      (D.P.)   2.16D-78     1.0D+4     174 
// VAX           (S.P.)   1.17E-38     1.0E+4      88 
// VAX D-Format  (D.P.)   1.17D-38     1.0D+4      88 
// VAX G-Format  (D.P.)   2.22D-308    1.0D+4     709 
//
// ******************************************************************* 
// ******************************************************************* 
//
// Error returns 
//
//  In case of an error,  NCALC != NB, and not all I's are 
//  calculated to the desired accuracy. 
//
//  NCALC < 0:  An argument is out of range. For example, 
//     NB <= 0, IZE is not 1 or 2, or IZE=1 and ABS(X) >= EXPARG. 
//     In this case, the B-vector is not calculated, and NCALC is 
//     set to MIN0(NB,0)-1 so that NCALC != NB. 
//
//  NB > NCALC > 0: Not all requested function values could 
//     be calculated accurately.  This usually occurs because NB is 
//     much larger than ABS(X).  In this case, B(N) is calculated 
//     to the desired accuracy for N <= NCALC, but precision 
//     is lost for NCALC < N <= NB.  If B(N) does not vanish 
//     for N > NCALC (because it is too small to be represented), 
//     and B(N)/B(NCALC) = 10**(-K), then only the first NSIG-K 
//     significant figures of B(N) can be trusted. 
//
//
// Intrinsic functions required are: 
//
//     DBLE, EXP, DGAMMA, GAMMA, INT, MAX, MIN, REAL, SQRT 
//
//
// Acknowledgement 
//
//  This program is based on a program written by David J. 
//  Sookne (2) that computes values of the Bessel functions J or 
//  I of real argument and int order.  Modifications include 
//  the restriction of the computation to the I Bessel function 
//  of non-negative real argument, the extension of the computation 
//  to arbitrary positive order, the inclusion of optional 
//  exponential scaling, and the elimination of most underflow. 
//  An earlier version was published in (3). 
//
// References: "A Note on Backward Recurrence Algorithms," Olver, 
//              F. W. J., and Sookne, D. J., Math. Comp. 26, 1972, 
//              pp 941-947. 
//
//             "Bessel Functions of Real Argument and Integer Order," 
//              Sookne, D. J., NBS Jour. of Res. B. 77B, 1973, pp 
//              125-132. 
//
//             "ALGORITHM 597, Sequence of Modified Bessel Functions 
//              of the First Kind," Cody, W. J., Trans. Math. Soft., 
//              1983, pp. 242-245. 
//
//  Latest modification: May 30, 1989 
//
//  Modified by: W. J. Cody and L. Stoltz 
//               Applied Mathematics Division 
//               Argonne National Laboratory 
//               Argonne, IL  60439 
//
//  Translated to C from FORTRAN and adapted for Java
//  by Michael Kiefte, Dalhousie University, June 16, 2002.
//
// -------------------------------------------------------------------- 

// ------------------------------------------------------------------- 
// Check for X, NB, OR IZE out of range. 
// ------------------------------------------------------------------- 
	if (nb > 0 && x >= zero && alpha >= zero && alpha < one && (ize == 1 
		&& x <= exparg || ize == 2 && x <= xlarge)) {
// ------------------------------------------------------------------- 
// Use 2-term ascending series for small X 
// ------------------------------------------------------------------- 
	    ncalc = nb;
	    magx = (int) (x);
	    if (x >= rtnsig) {
// ------------------------------------------------------------------- 
// Initialize the forward sweep, the P-sequence of Olver 
// ------------------------------------------------------------------- 
		nbmx = nb - magx;
		n = magx + 1;
		i__1 = n + n;
		en = (double) i__1 + (alpha + alpha);
		plast = one;
		p = en / x;
// ------------------------------------------------------------------- 
// Calculate general significance test 
// ------------------------------------------------------------------- 
		test = ensig + ensig;
		if (magx << 1 > nsig * 5) {
		    test = Math.sqrt(test * p);
		} else {
		    test /= Math.pow(const__, (double) magx);
		}
L120:
		for (;;) {
		    if (nbmx >= 3) {
// ------------------------------------------------------------------- 
// Calculate P-sequence until N = NB-1.  Check for possible overflow. 
// ------------------------------------------------------------------- 
			tover = enten / ensig;
			nstart = magx + 2;
			nend = nb - 1;
			i__1 = nend;
			for (k = nstart; k <= i__1; ++k) {
			    n = k;
			    en += two;
			    pold = plast;
			    plast = p;
			    p = en * plast / x + pold;
			    if (p > tover) {
// ------------------------------------------------------------------- 
// To avoid overflow, divide P-sequence by TOVER.  Calculate 
// P-sequence until ABS(P) > 1. 
// ------------------------------------------------------------------- 
				tover = enten;
				p /= tover;
				plast /= tover;
				psave = p;
				psavel = plast;
				nstart = n + 1;
				do {
				    ++n;
				    en += two;
				    pold = plast;
				    plast = p;
				    p = en * plast / x + pold;
				} while (p <= one);
				tempb = en / x;
// ------------------------------------------------------------------- 
// Calculate backward test, and find NCALC, the highest N 
// such that the test is passed. 
// ------------------------------------------------------------------- 
				test = pold * plast / ensig;
				test *= half - half / (tempb * tempb);
				p = plast * tover;
				--n;
				en -= two;
				nend = Math.min(nb,n);
				i__2 = nend;
L90:
				for (;;) {
				    for (l = nstart; l <= i__2; ++l) {
					ncalc = l;
					pold = psavel;
					psavel = psave;
					psave = en * psavel / x + pold;
					if (psave * psavel > test) {
					    break L90;
					}
				    }
				    ncalc = nend + 1;
				    break;
				}
				--(ncalc);
				break L120;
			    }
			}
			n = nend;
			i__1 = n + n;
			en = (double) i__1 + (alpha + alpha);
// ------------------------------------------------------------------- 
// Calculate special significance test for NBMX > 2. 
// ------------------------------------------------------------------- 
// Computing MAX 
			d__1 = test;
			d__2 = Math.sqrt(plast * ensig) * Math.sqrt(p + p);
			test = Math.max(d__1, d__2);
		    }
// ------------------------------------------------------------------- 
// Calculate P-sequence until significance test passed. 
// ------------------------------------------------------------------- 
		    do {
			++n;
			en += two;
			pold = plast;
			plast = p;
			p = en * plast / x + pold;
		    } while (p < test);
		    break;
		}
// ------------------------------------------------------------------- 
// Initialize the backward recursion and the normalization sum. 
// ------------------------------------------------------------------- 
		++n;
		en += two;
		tempb = zero;
		tempa = one / p;
		em = (double) n - one;
		empal = em + alpha;
		emp2al = em - one + (alpha + alpha);
		sum = tempa * empal * emp2al / em;
		nend = n - nb;
L230:
		for (;;) {
L220:
		    for (;;) {
			if (nend < 0) {
// ------------------------------------------------------------------- 
// N < NB, so store B(N) and set higher orders to zero. 
// ------------------------------------------------------------------- 
			    b[n - 1] = tempa;
			    nend = -nend;
			    i__1 = nend;
			    for (l = 1; l <= i__1; ++l) {
				b[n + l - 1] = zero;
			    }
			} else {
			    if (nend > 0) {
// ------------------------------------------------------------------- 
// Recur backward via difference equation, calculating (but 
// not storing) B(N), until N = NB. 
// ------------------------------------------------------------------- 
				i__1 = nend;
				for (l = 1; l <= i__1; ++l) {
				    --n;
				    en -= two;
				    tempc = tempb;
				    tempb = tempa;
				    tempa = en * tempb / x + tempc;
				    if (Double.isInfinite(tempa))
System.out.println("en = " + en + "; tempb = " + tempb + "; x = "
	+ x + "; tempc = " + tempc);
				    em -= one;
				    emp2al -= one;
				    if (n == 1) {
					break;
				    }
				    if (n == 2) {
					emp2al = one;
				    }
				    empal -= one;
				    sum = (sum + tempa * empal) * emp2al / em;
				}
			    }
// ------------------------------------------------------------------- 
// Store B(NB) 
// ------------------------------------------------------------------- 
			    b[n - 1] = tempa;
			    if (nb <= 1) {
				sum = sum + sum + tempa;
				break L230;
			    }
// ------------------------------------------------------------------- 
// Calculate and Store B(NB-1) 
// ------------------------------------------------------------------- 
			    --n;
			    en -= two;
			    b[n - 1] = en * tempa / x + tempb;
			    if (n == 1) {
				break L220;
			    }
			    em -= one;
			    emp2al -= one;
			    if (n == 2) {
				emp2al = one;
			    }
			    empal -= one;
			    sum = (sum + b[n - 1] * empal) * emp2al / em;
			}
			nend = n - 2;
			if (nend > 0) {
// ------------------------------------------------------------------- 
// Calculate via difference equation and store B(N), until N = 2. 
// ------------------------------------------------------------------- 
			    i__1 = nend;
			    for (l = 1; l <= i__1; ++l) {
				--n;
				en -= two;
				b[n - 1] = en * b[n] / x + b[n + 1];
				em -= one;
				emp2al -= one;
				if (n == 2) {
				    emp2al = one;
				}
				empal -= one;
				sum = (sum + b[n - 1] * empal) * emp2al / em;
			    }
			}
// ------------------------------------------------------------------- 
// Calculate B(1) 
// ------------------------------------------------------------------- 
			b[0] = two * empal * b[1] / x + b[2];
			break;
		    }
		    sum = sum + sum + b[0];
		    break;
		}
// ------------------------------------------------------------------- 
// Normalize.  Divide all B(N) by sum. 
// ------------------------------------------------------------------- 
		if (alpha != zero) {
		    d__1 = one + alpha;
		    d__2 = x * half;
		    d__3 = -(alpha);
		    sum = sum * gamma(d__1) * Math.pow(d__2, d__3);
		}
		if (ize == 1) {
		    sum *= Math.exp(-(x));
		}
		tempa = enmten;
		if (sum > one) {
		    tempa *= sum;
		}
		i__1 = nb;
		for (n = 1; n <= i__1; ++n) {
		    if (b[n - 1] < tempa) {
			b[n - 1] = zero;
		    }
		    b[n - 1] /= sum;
		}
		return ncalc;
// ------------------------------------------------------------------- 
// Two-term ascending series for small X. 
// ------------------------------------------------------------------- 
	    } else {
		tempa = one;
		empal = one + alpha;
		halfx = zero;
		if (x > enmten) {
		    halfx = half * x;
		}
		if (alpha != zero) {
		    tempa = Math.pow(halfx, alpha) / gamma(empal);
		}
		if (ize == 2) {
		    tempa *= Math.exp(-(x));
		}
		tempb = zero;
		if (x + one > one) {
		    tempb = halfx * halfx;
		}
		b[0] = tempa + tempa * tempb / empal;
		if (x != zero && b[0] == zero) {
		    ncalc = 0;
		}
		if (nb > 1) {
		    if (x == zero) {
			i__1 = nb;
			for (n = 2; n <= i__1; ++n) {
			    b[n - 1] = zero;
			}
		    } else {
// ------------------------------------------------------------------- 
// Calculate higher-order functions. 
// ------------------------------------------------------------------- 
			tempc = halfx;
			tover = (enmten + enmten) / x;
			if (tempb != zero) {
			    tover = enmten / tempb;
			}
			i__1 = nb;
			for (n = 2; n <= i__1; ++n) {
			    tempa /= empal;
			    empal += one;
			    tempa *= tempc;
			    if (tempa <= tover * empal) {
				tempa = zero;
			    }
			    b[n - 1] = tempa + tempa * tempb / empal;
			    if (b[n - 1] == zero && ncalc > n) {
				ncalc = n - 1;
			    }
			}
		    }
		}
	    }
	} else {
	    ncalc = Math.min(nb,0) - 1;
	}
	return ncalc;
    } // ribesl 
}
