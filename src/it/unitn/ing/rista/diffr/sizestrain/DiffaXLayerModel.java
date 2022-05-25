/*
 * @(#)DiffaXLayerModel.java created Jun 30, 2009 Caen
 *
 * Copyright (c) 2009 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.sizestrain;

import it.unitn.ing.rista.util.Misc;

/**
 * The DiffaXLayerModel is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jun 30, 2009 11:14:24 AM $
 * @since JDK1.1
 */
public class DiffaXLayerModel {
	
	static int MAX_L = 20, MAX_A = 200, MAX_TA = 20, MAX_SP = 20001, SADSIZE = 256;
	static int XP_MAX = 5000, RCSV_MAX = 1022, MAX_NAM = 31, MAX_BIN = 10;
	static int FFACT_SIZE = 201, N_SIGMAS = 7;
	static double inf_width = 1.0E4;
	static int ip = 5, op = 6, df = 2, sf = 4, dp = 10, sy = 11, sp = 12, sk = 13, sa = 14;
	static int scrtch = 3;
	static int CLIP = 14;
	static int UNKNOWN = -1;
    // define some useful numerical constants
	static double[] C_ZERO = {0, 0}, C_ONE = {1, 0};
	static double ZERO = 0.0, QUARTER = 0.25, HALF = 0.5,
				  ONE = 1.0, TWO = 2.0, THREE = 3.0, FOUR = 4.0,
			      FIVE = 5.0, SIX = 6.0, EIGHT = 8.0, TEN = 10.0,
			          TWELVE = 12.0, TWENTY = 20.0, FIFTY = 50.0,
	        HUNDRED = 100.0, ONE_EIGHTY = 180.0;
	static double eps1 = 1.0E-1, eps2 = 1.0E-2, eps3 = 1.0E-3,
			      eps4 = 1.0E-4, eps5 = 1.0E-5, eps6 = 1.0E-6,
			      eps7 = 1.0E-7, eps8 = 1.0E-8, eps9 = 1.0E-9,
	          eps10 = 1.0E-10, eps14 = 1.0E-14;
	static int EIGHTBITS = 256, FIFTEENBITS = 32768, SIXTEENBITS = 65536;
	
// Common Block Declarations

    String a_name[][] = new String[MAX_A][MAX_L];
    String[] atom_l = new String[MAX_TA];
	String pnt_grp, sfname, cfname;
    boolean[] one_b = new boolean[MAX_L], one_occup = new boolean[MAX_L];
	boolean[][] bs_zero = new boolean[MAX_L][MAX_L], there = new boolean[MAX_L][MAX_L];
    boolean only_real, same_bs, all_bs_zero, rot_only, cfile,
	    dodatdump, dosymdump, intp_f, trim_origin, recrsv, xplcit,
	    rndm, inf_thick, has_l_mirror, h_mirror, k_mirror,
	    hk_mirror, check_sym, same_rz, any_sharp, same_layer,
	    finite_width;
    int[] l_seq = new int[XP_MAX], pow = new int[MAX_BIN], l_n_atoms = new int[MAX_L];
	int[] l_symmetry = new int[MAX_L], l_actual = new int[MAX_L], e_sf = new int[MAX_TA];
    int[][] a_type = new int[MAX_A][MAX_L], a_number = new int[MAX_A][MAX_L];

    int symgrpno, no_trials, h_bnd, k_bnd, cntrl, max_pow,
	    l_cnt, full_brd, full_shrp, sadblock, loglin, bitdepth,
	    rad_type, n_layers, n_actual, blurring, n_atoms, maxsad;

    int none, centro, gauss, lorenz, ps_vgt, pv_gss, pv_lrn,
	    x_ray, neutrn, electn;

    double[][][] l_r = new double[3][MAX_L][MAX_L], a_pos = new double[3][MAX_A][MAX_L]; 
    double[][] a_b = new double[MAX_A][MAX_L], a_occup = new double[MAX_A][MAX_L], r_b11 = new double[MAX_L][MAX_L], 
	           r_b22 = new double[MAX_L][MAX_L], r_b33 = new double[MAX_L][MAX_L], r_b12 = new double[MAX_L][MAX_L], 
	           r_b23 = new double[MAX_L][MAX_L], r_b31 = new double[MAX_L][MAX_L], hx_ky = new double[MAX_A][MAX_L], 
	           detune = new double[MAX_L][MAX_L], x_sf = new double[9][MAX_TA], l_alpha = new double[MAX_L][MAX_L];
	double[] l_g = new double[MAX_L], high_atom = new double[MAX_L], low_atom = new double[MAX_L], 
	         spec = new double[MAX_SP], brd_spc = new double[MAX_SP], formfactor = new double[FFACT_SIZE];

    double a_b11, a_b22, a_b33, a_b12, a_b23, a_b31;

    double tolerance, max_var, max_angle, l_bnd, l_rz, pi, pi2,
	    rad2deg, deg2rad, scaleint, brightness, lambda, th2_min,
	    th2_max, d_theta, h_start, k_start, h_end, k_end,
	    cell_a, cell_b, cell_c, cell_gamma, pv_u, pv_v,
	    pv_w, pv_gamma, fwhm, mltplcty, bnds_wt, theta1, theta2, a0,
	     b0, c0, d0, ab0, bc0, ca0, tiny_inty, fatswalla_hk;

    double ffact_scale, wa, wb, ffhkcnst, ffwdth;

    double[][][] mat = new double[2][MAX_L][MAX_L], mat1 = new double[2][MAX_L][MAX_L],
        l_phi = new double[2][MAX_L][MAX_L];
	double[] wavefn;


  double delta = .001;


// Table of constant values 

/*  static int c2 = 2;
  static int c1 = 1;
  static int c3 = 3;
  static int c4 = 4;
  static static Complex c_b82 = new Complex(-1.,-0.);
  static int c0 = 0;
  static double c_b157 = 0.;
  static int c20 = 20;
  static int c12 = 12;
  static double c_b592 = 4e6;
  static int c256 = 256;*/

// *********************************************************************** 
//                                                                      * 
//     Copyright 1987-2002 Michael M. J. Treacy and Michael W. Deem     * 
//                                                                      * 
// *********************************************************************** 
// *********************************************************************** 
// *******************      Source file DIFFaX.f       ******************* 
// *********************************************************************** 
// *********************************************************************** 
// ******************** version 1.812, 3rd July, 2005 ******************** 
// *********************************************************************** 
// *********************************************************************** 
// This program calculates the powder diffraction spectrum of a crystal * 
// formed from layers which stack coherently, but not deterministically.* 
// The algorithm used is described in detail in "A General Recursion    * 
// Method for Calculating Diffracted Intensities From Crystals          * 
// Containing Planar Faults", by M.M.J. Treacy, M.W. Deem and           * 
// J.M. Newsam, Proceedings of the Royal Society, London, A (1991) 433, * 
// pp 499 - 520.                                                        * 
//                                                                      * 
// Source code written by Michael M. J. Treacy and Michael W. Deem      * 
//                                                                      * 
// HISTORY:                                                             * 
// 6/87-5/88; MMJT: Original versions were named 'betaPXD' and 'FauPXD'.* 
// They were 'hardwired' for simulating PXD patterns from zeolite beta, * 
// and the faujasite/'Breck's structure 6' zeolite family.              * 
//                                                                      * 
// 5-8/88; MWD: Completely rewritten in generalized form for Cray and   * 
// VAX, and named 'PDS'.                                                * 
//                                                                      * 
// 8/88-11/89; MMJT: Control file option added. Improved handling of    * 
// sharp peaks. Symmetry testing added. Layer 'stacking uncertainty'    * 
// factors added. Selected area electron diffraction option added.      * 
// Explicit layer sequencing option. Optimization of layer form factor  * 
// calculations. Renamed 'DIFFaX' for, (D)iffraction (I)ntensities      * 
// (F)rom (Fa)ulted (X)tals.                                            * 
//                                                                      * 
// 12/89 - 3/90; MMJT: Finite crystal thickness now accepted under      * 
//              'RECURSIVE' option. Self-consistency check of atomic    * 
//               coordinates in data file. (v1.76)                      * 
//                                                                      * 
// 4/90 - 12/90; MMJT: Minor bug fixes. Streamlined 'data.sfc' file.    * 
//               (v1.761 and v1.762)                                    * 
//                                                                      * 
// 1/91; MMJT: Eliminated the use of scratch file while reading data.   * 
//             GETLNE now handles multiple, nested, comments (v1.763)   * 
//                                                                      * 
// 5/91; MMJT: Eliminated bug in default tolerance parameter. Added     * 
//             average crystal composition printout to dump file.       * 
//             (v1.764)                                                 * 
//                                                                      * 
// 8/91; MMJT: Replaced the LU decomposition routines CLUDCM and CLUBKS * 
//             (from "Numerical Recipes") with the faster linpack       * 
//             routines, CGEFA and CGESL (v1.765)                       * 
//                                                                      * 
// 8/91; MMJT: Improved sharp peak detection - use peak widths rather   * 
//             than the more complicated phase coherence argument in    * 
//             subroutine SHARP (v1.766)                                * 
//                                                                      * 
// 4/92; MMJT: Fixed bug in INTEN2 where last layer in an explicit      * 
//             sequence was inadvertently assigned a scattering factor  * 
//             of C_ONE. Improved error checking for explicit layers so * 
//             that when alpha(j,i) = 0, an error is issued if j        * 
//             follows i. XP_MAX increased to 5000. GETLNE now checks   * 
//             that data lines do not exceed maximum length. (v1.767)   * 
//                                                                      * 
// 12/94; MMJT:Reinstated the use of the scratch file that had been     * 
//             eliminated in v1.763. The Cray, and Microsoft fortran    * 
//             compiler for PC, adhere to the FORTRAN77 standard and    * 
//             do not allow unformatted reads/writes from/to strings.   * 
//                                                                      * 
// 1/95; MMJT: Finessed the diffraction symmetry detection routines.    * 
//             Introduced the subroutine THRESH. (v1.769)               * 
//                                                                      * 
// 2/95; MMJT: Fixed glitches in THRESH, TST_MIR and TST_ROT that were  * 
//             introduced in the 1/95 fix. (still v1.769)               * 
//                                                                      * 
// 3/95; MMJT: Implemented Debye-Scherrer type broadening due to finite * 
//             lateral layer widths. Added CHWDTH, RDWDTH. Modified the * 
//             way the powder pattern is written to the array spec, so  * 
//             that the low angle intensity begins at spec(1). AtomSite     *
//             names are now case insensitive. The layer Bij factors    * 
//             were reordered - B23, B31 become B13, B23.               * 
//             GET_G modified to handle singularities better. (v1.80)   * 
//                                                                      * 
// 7/95; MMJT: Fixed rare zero integration range bug in GLQ16.          * 
//             Fixed "fatsWalla" bug in GET_MAT.  (v1.801)              * 
//                                                                      * 
// 5/96; MMJT: Fixed a bug in the LL() function in INTEGR() which was   * 
//             introduced by a "cosmetic" change in v1.801.  (v1.802)   * 
//                                                                      * 
// 10/96; MMJT:Changed eps3 to eps5 in CHWDTH function so that the      * 
//             broadening tails extend further.  (v1.803)               * 
//                                                                      * 
// 7/97; MMJT: Added subroutines NXTARG and RDNMBR. These allow data    * 
//             to be entered as fractions (ie 1/3). Improved robustness * 
//             of the "fatswalla" interlayer uncertainty code. (v1.804) * 
//                                                                      * 
// 6/98; MMJT: Fixed bug in PV() that was introduced in v1.80. (v1.805) * 
//                                                                      * 
// 3/00; MMJT: Now allow 16-bit deep SADPs. (v1.806)                    * 
//                                                                      * 
// 8/00; MMJT: RDSTAK changed so that if a stacking probability is zero,* 
//              the rest of the line is ignored.        (v1.807)        * 
//                                                                      * 
// 4/02; MMJT: Removed calls to iidnnt in WRTSADP.      (v1.808)        * 
//                                                                      * 
// 2/03; MMJT: Halved the value of ffhkcnst in GETSPC. The half-width   * 
//             was being used instead of the FWHM, which made the shape * 
//             broadening twice as large as it should be. (v1.809)      * 
//                                                                      * 
// 3/04; MMJT: Fixed a minor printing bug in TST_MIR      (v1.810)      * 
//                                                                      * 
// 1/05; MMJT: Fixed some f77 compiler compatibility bugs.  (v1.811)    * 
//                                                                      * 
// 7/05; MMJT: Fixed a bug in EQUALB that caused DIFFaX to ignore the   * 
//             sign of the Fats-Waller Bij terms.  (v1.812)             * 
//                                                                      * 
// *********************************************************************** 
// ***************************** Legal Note ****************************** 
// *********************************************************************** 
//                                                                      * 
// * * * * * * * * * *  DISCLAIMER OF WARRANTIES: * * * * * * * * * * * * 
//                                                                      * 
// The authors make no warranties whatsoever, express or implied, with  * 
// respect to the DIFFaX software or any of its parts, nor do they      * 
// warrant that the DIFFaX software, or any of its parts, will be       * 
// error-free, will operate without interruption, or will be compatible * 
// with any software or hardware possessed by the user.                 * 
//                                                                      * 
// * * * * * * * * * *  LIMITATION OF LIABILITY:  * * * * * * * * * * * * 
//                                                                      * 
// The authors will not be liable for any special, incidental, or       * 
// consequential damages, even if informed of the possibility of such   * 
// damages in advance.                                                  * 
//                                                                      * 
// *********************************************************************** 
// ************************** DIFFaX file i/o. *************************** 
// *********************************************************************** 
//                                                                      * 
// * * * * OPTIONAL CONTROLFILE FOR AUTOMATIC RUNNING OF DIFFaX * * * * * 
//                                                                      * 
// DIFFaX first searches the current directory for a control file named * 
// 'control.dif'. If it finds this file it opens it on unit 'cntrl'     * 
// and this becomes the default input unit. Structure filenames,        * 
// and the various parameters (which would normally be requested        * 
// interactively) obviously must be in the correct sequence. The data   * 
// read from 'control' is echoed on the default output device (ie. the  * 
// screen, unit 'op') so the user can check that the responses          * 
// are properly synchronized. If 'control.dif' does not exist, the      * 
// default input device is the keyboard (unit number 'ip'), and the     * 
// user is expected to answer the prompts. DIFFaX will loop through the * 
// contents of 'control', and thus can be used to rerun DIFFaX on fresh * 
// data files, without quitting. Under direction from a control file,   * 
// normal termination of DIFFaX occurs when a filename 'END' is         * 
// encountered. Interactively, DIFFaX will end normally when the user   * 
// chooses not to return to the function menu.                          * 
// The name of the control file is stored in the global character       * 
// variable 'cfname', and is assigned in 'main'.                        * 
//                                                                      * 
//                                                                      * 
// * * * * * * * * * * *  STRUCTURE INPUT FILE  * * * * * * * * * * * * * 
//                                                                      * 
// The structure input file is opened on unit 'df'. It can have any     * 
// name except 'END' (case insensitive). For clarity it may be best to  * 
// keep it short (less than 8 characters) and (optionally) with '.dat'  * 
// appended. Output files use the input name up to the first blank      * 
// (' ') or period ('.') as their root name. Thus, if 'beta.dat'        * 
// (or 'beta') is the data input file name, then 'beta.spc' etc... will * 
// be the form of the output file names.                                * 
//                                                                      * 
//                                                                      * 
// * * * * * * * STRUCTURE FACTOR PARAMETER INPUT FILE  * * * * * * * * * 
//                                                                      * 
// The structure factor parameter file, 'data.sfc' is opened on unit    * 
// 'sf'. If a file of name 'data.sfc' is not found, DIFFaX will abort.  * 
// The name of the structure factor parameter file is stored in the     * 
// global character variable 'sfname', and is assigned in 'main'.       * 
//                                                                      * 
//                                                                      * 
// * * * * * * * * * * *  SPECTRUM OUTPUT FILE  * * * * * * * * * * * * * 
//                                                                      * 
// Spectra are output as text files on unit 'sp'. Each record contains  * 
//        2theta     intensity     (instrumentally broadened intensity) * 
// in tab-delimited format. 'Instrumentally broadened intensity' is     * 
// output only if the pseudo-Voigt, Gaussian or Lorentzian options were * 
// requested. Spectra output file names are in the form 'rootname.spc', * 
// or alternatively, if that name is already taken, as 'rootname.spc#', * 
// where #=1,2,3 etc...                                                 * 
//                                                                      * 
//                                                                      * 
// * * * * * * * * * STREAK INTENSITIES OUTPUT FILE * * * * * * * * * * * 
//                                                                      * 
// Streak calculations are output on unit 'sk'. Streak output file      * 
// names are in the form 'rootname.str', or alternatively, if that name * 
// is already taken, as 'rootname.str#', where #=1,2,3 etc...           * 
//                                                                      * 
//                                                                      * 
// * * *   SELECTED AREA DIFFRACTION PATTERN (SADP) OUTPUT FILE   * * * * 
//                                                                      * 
// Selected area diffraction pattern data is saved in binary format in  * 
// a file named 'rootname.sadp' which is output on unit 'sad'. If that  * 
// name is already taken, the alternative name 'rootname.sadp#' is      * 
// used, where #=1,2,3 etc...                                           * 
//                                                                      * 
//                                                                      * 
// * * * * * *  OPTIONAL DUMP FILE OF STRUCTURAL PARAMETERS * * * * * * * 
//                                                                      * 
// If the user requests a dump of the structure data file (as DIFFaX    * 
// read it!) a dumpfile named 'rootname.dmp' is output on unit 'dp'. If * 
// that name is already taken, the alternative name 'rootname.dmp#' is  * 
// used, where #=1,2,3 etc...This is valuable for debugging the input   * 
// data file.                                                           * 
//                                                                      * 
//                                                                      * 
// * * * * * * *  OPTIONAL DUMP FILE OF INTENSITIES FOUND   * * * * * * * 
// * * * * * * * WHEN EVALUATING DIFFRACTION POINT SYMMETRY * * * * * * * 
//                                                                      * 
// The user may also output the history of the intensity values found   * 
// when DIFFaX attempts to establish the point group symmetry of the    * 
// diffraction output. This is useful when debugging the datafile. The  * 
// intensity data is saved in a file named 'rootname.sym' which is      * 
// output on unit 'sy'. If that name is already taken, the alternative  * 
// name 'rootname.sym#' is used, where #=1,2,3 etc...                   * 
//                                                                      * 
// *********************************************************************** 
// ******************************* DIFFaX ******************************** 
// *********************************************************************** 

//  
// Title: DIFFaX 
// Authors: MWD and MMJT 
// Date: 23 Oct 1988 
// Description: This is the main program. First, important global 
// constants, such as PI, are defined. The name of the control file 
// is assigned to cfname, and then FNDCTL searches for this file in the 
// current directory. If found, the control file is opened and it 
// becomes the default input device. If not found, then the keyboard is 
// the standard input device. The user's data file, and the atomic 
// scattering factor data file (whose name is contained in 'sfname') 
// are then searched for in the current directory (GETFIL), and opened. 
// The user's data file is then read (RDFILE). The standard scattering 
// factor data file 'sfname' is then searched for data on the atom 
// types specified by the user (SFC). The layer existence 
// probabilities are calculated (GET_G). If the user data file 
// requested EXPLICIT, RANDOM stacking, then DIFFaX computes a random 
// layer sequence consistent with the stacking probabilities (GETLAY). 
// Reciprocal lattice constants related to the unit cell are then 
// calculated (SPHCST). If the user requested (either interactively, 
// or through the control file) a dump of what DIFFaX read from the 
// user's data file, then an annotated dump is generated (DUMP). DETUN 
// then delicately adjusts the probability data so as to avoid zero 
// determinants at the sharp peaks. The user is then asked if a dump 
// of DIFFaX's symmetry evaluations is required, and then searches the 
// data looking for simple opportunities to speed up the calculation 
// (OPTIMZ). The user is then asked if he wants to calculate the 
// intensity at a point (POINT), along a streak (GOSTRK), integrated 
// within a defined interval (GOINTR), a powder pattern (GOSPEC) or 
// a selected area diffraction pattern (GOSADP). If running 
// interactively, the user can return to any of these menu options, 
// except if GOSPEC was chosen, where DIFFaX will finish. If a control 
// file is being used, then DIFFaX will return to the beginning if 
// GOSPEC was chosen. If a new data file name is read then DIFFaX will 
// run again. If the control file reads 'End' (case insensitive) as the 
// new file name, then DIFFaX will finish. 
// Note: The file names contained in 'cfname' and 'sfname', and the name 
// 'End' are reserved names, and cannot be used by the user as data file 
// names. 

//      COMMON VARIABLES: 
//            uses:  rndm, cntrl, CFile, SymGrpNo 

//        modifies:  PI, PI2, DEG2RAD, RAD2DEG, DoDatdump, 
//                   DoSymDump, cfname, sfname 
//  

	// What type of intensity output does the user want? (operation)
	// 0 POINT, 1 STREAK, 2 INTEGRATE, 3 POWDER PATTERN, 4 SADP	

public void DIFFAaX(int operation) {
    // Local variables 
    int i;
    boolean[] ok = new boolean[1];
    boolean ending;
//    char infile[31];
//    char outfile[31];

// set up transcendental constants. Use FORTRAN library definition of PI. 
    pi = Math.atan(ONE) * FOUR;
    pi2 = pi * TWO;
    deg2rad = pi / ONE_EIGHTY;
    rad2deg = ONE_EIGHTY / pi;

// Set up the global "all is well" flags. 
    ending = false;
    ok[0] = true;

// Fly the flag 
//    salute();

// check input files, and if okay, open them. 
//      cfname = 'control.dif' 
//      sfname = 'data.sfc' 
//    ok = fndctl();
//    if (ok) {
//	getfil(infile, &ok, &ending, (ftnlen)31);
//    }
//    if (! ok || ending) {
//  999 if(CFile) close(unit = cntrl) 
//	if (ok && ending) {
//        write(op,100) 'DIFFaX ended normally.' 
//	} else {
//        write(op,100) 'DIFFaX was terminated abnormally.' 
//	}
//	return ;
//    }

// Handle request for a dump file of the data DIFFaX reads in. 
//    dodatdump = false;
//   99 write(op,100) 'Enter 1 for DUMP' 
//      read(cntrl,*,err=99,end=999) i 
//      if(CFile) write(op,101) i 
//      if(i.eq.1) DoDatdump = .true. 

// Let's read the data, and process it. 
//    ok = rdfile_(infile, (ftnlen)31);
    if (ok[0]) 
		ok[0] = sfc();
    if (ok[0])
		ok[0] = get_g();
    if (ok[0] && rndm)
		ok[0] = getlay();
    if (ok[0])
		sphcst();
//    if (ok[0] && dodatdump)
//		dump(infile, ok);
    if (ok[0])
		detun();
    if (!ok[0]) {
        System.out.println("DIFFaX was terminated abnormally.");
		return;
    }

// Handle request for a file containing the symmetry evaluation data 
    dosymdump = false;
//    if (symgrpno != 11) {
//   98   write(op,100) 'Enter 1 for a dump of the symmetry evaluations' 
//        read(cntrl,*,err=98,end=999) i 
//        if(CFile) write(op,101) i 
//        if(i.eq.1) DoSymDump = .true. 
//    }

// See if there are any optimizations we can do 
    if (ok[0])
		optimz(ok);

// What type of intensity output does the user want? 
//   10 if(ok) then 
//   97   write(op,100) 'Enter function number:' 
//        write(op,100) 
//     |      '0 POINT, 1 STREAK, 2 INTEGRATE, 3 POWDER PATTERN, 4 SADP' 
//        read(cntrl,*,err=97,end=999) i 
//        if(i.lt.0.or.i.gt.4) goto 97 
//        if(CFile) write(op,101) i 
//      end if 

// Do what the user asked for. 
    if (ok[0])
		switch (operation) {
			case 0:
				point(ok);
				break;
			case 1:
				gostrk(ok);
				break;
			case 2:
				gointr(ok);
				break;
			case 3:
				gospec(ok);
				break;
			case 4:
				gosadp(ok);
				break;
			default:
			{
				System.out.println("Unknown function type.");
				return;
			}
		}


// If we are here, then all must have gone well. 
	ending = true;

	if (ok[0] && ending)
        System.out.println("DIFFaX ended normally.");
	else
        System.out.println("DIFFaX was terminated abnormally.");
}

//  
// Title: block data 
// Author: MMJT 
// Date: 23 Oct 1988 
// Description: Sets up some global constants which help make the 
// code easier to read. 

//      COMMON VARIABLES: 

//        modifies:  CENTRO, ELECTN, NEUTRN, NONE, GAUSS, 
//                   LORENZ, PS_VGT, PV_GSS, PV_LRN, X_RAY 
//  



// The common block 'consts' also occurs in the file 'DIFFaXj.inc' 

//  
// Title: AGLQ16 
// Author: MWD 
// Date: 18 Aug 1988 
// Description:  This routine does adaptive 16-point Gauss-Legendre 
// quadrature in an interval (h,k,a) to (h,k,b) of reciprocal space. 
// ok is returned as .TRUE. if GLQ16 hasn't blown its stack. The 
// integrated result is returned in AGLQ16. 

//      ARGUMENTS: 
//            h   -  reciprocal lattice vector h-component. (input). 
//            k   -  reciprocal lattice vector k-component. (input). 
//            a   -  l-value of the lower bound of reciprocal 
//                   lattice integration region. (input). 
//            b   -  l-value of the upper bound of reciprocal 
//                   lattice integration region. (input). 
//            ok  -  boolean flag indicating all went well. (output). 

//      AGLQ16 returns the adaptively integrated value. 
//  

double aglq16(int h, int k, double a, double b, boolean[] ok)
{
    // System generated locals 
    double ret_val;

    // Local variables
	int maxstk = 200;
    double epsilon2, epsilon = FIVE * eps4;
    int n;
    double x, d1 = 0, d2 = 0, d3;
    int n2;
    double[] stk = new double[maxstk];
	double sum;
    int stp;
    double sum1, sum2, sum3;
    int first;

    ret_val = ZERO;
// initalize stack; top is at highest index 
    stp = maxstk;
// get first integration 
    sum3 = glq16(h, k, a, b, ok);
    if (!ok[0]) {
		System.out.println("GLQ16 returned an error to AGLQ16.");
		System.out.println("at: h = " + h + ", k = " + k + ", l1 = " + d1 + ", l2 = " + d2);
		return ret_val;
    }
    n2 = 2;
    n = 0;
    d1 = a;
    d3 = b;
    sum = ZERO;
    first = 0;
    while(stp != maxstk || first == 0) {
	if (first == 0)
	    first = 1;
	d2 = (d1 + d3) * HALF;
	n += 2;
	sum1 = glq16(h, k, d1, d2, ok);
	if (!ok[0]) {
		System.out.println("GLQ16 returned an error to AGLQ16.");
		System.out.println("at: h = " + h + ", k = " + k + ", l1 = " + d1 + ", l2 = " + d2);
	    return ret_val;
	}
	sum2 = glq16(h, k, d2, d3, ok);
	if (!ok[0]) {
		System.out.println("GLQ16 returned an error to AGLQ16.");
		System.out.println("at: h = " + h + ", k = " + k + ", l1 = " + d1 + ", l2 = " + d2);
	    return ret_val;
	}
	x = sum1 + sum2;
// determine figure of merit 
// Computing MAX 
	epsilon2 = Math.max(epsilon, epsilon * Math.abs(x));
	if (Math.abs(x - sum3) > epsilon2) {
// the area of these two panels is not accurately known 
// check for stack overflow 
	    if (stp < 3) {
		  System.out.println("Stack overflow in AGLQ16.");
		  return ret_val;
	    }
// push right one onto stack 
	    stk[stp - 1] = sum2;
	    stk[stp - 2] = d2;
	    stk[stp - 3] = d3;
	    stp += -3;
	    d3 = d2;
	    sum3 = sum1;
	} else {
// this panel has been accurately integrated; save its area 
	    sum += x;
// get next panel 
// check for stack underflow--happens when no panels left to pop off 
//          if(stp.eq.maxstk) goto 30 
	    if (stp != maxstk) {
		d3 = stk[stp];
		d1 = stk[stp + 1];
		sum3 = stk[stp + 2];
		stp += 3;
	    }
	    if (n == n2) {
			n2 *= 2;
	    }
	}
    }
    return sum;
} 


//  
// Title: APPR_F 
// Author: MMJT 
// Date: 11 Feb 1989 
// Description: This subroutine returns polynomial approximations of f 
//  at 16 points, for use in GLQ16. The f's are returned in a MAX_L x 16 
//  array. The order of the polynomial is n-1, where n is input. The 16 
//  points in reciprocal space for which f's are needed are given by 
//  (h,k,ag_l(i)), where h, k and ag_l are input. 
//  The ll are the n sampling points, whose f's must be calculated 
//  by direct calls to GET_F, and from which the interpolations are 
//  calculated. list contains the indices of those n points. There is no 
//  need to interpolate for these values since we know them already! 

//      ARGUMENTS: 
//            f      -  Array of layer form factors. (output). 
//            h      -  reciprocal lattice vector h-component. (input). 
//            k      -  reciprocal lattice vector k-component. (input). 
//            ll     -  an array of n l-values to be used in the 
//                      interpolation. (input). 
//            ag_l   -  an array of 16 l_values at which interpolated 
//                      form factors are required. (input). 
//            n      -  the order of the polynomial approximation. 
//                                                              (input). 
//            list   -  A list of the indices of the n ll points 
//                      entered. Interpolation is not needed at these 
//                      input values. (input). 
//            ok     -  boolean flag indicating all went well. (output). 

//      COMMON VARIABLES: 
//            uses:     a0, b0, c0, d0, n_layers 
//  

void appr_f(double[][][] f, int h, int k, double[] ll, double[] ag_l, 
			int n, int[] list, boolean[] ok)
{
    // System generated locals 
    int i1, i2, i3, i4;
    double Q2;

    // Local variables 
	int max_poly = 10;
    int i, j, m, p;
    double[][] fa = new double[2][n_layers];
	double[][][] ff = new double[2][n_layers][n];
    double [][] fft = new double[2][n_layers];
    double[] f_ans = new double[2];
    boolean know_f;
    double[] f_error = new double[2];

// statement function 
// Q2 is the value of 1/d**2 at hkl 

// sample GET_F n times 

    for (i = 0; i < n; i++) {
	    Q2 = h * h * a0 + k * k * b0 + ll[i] * ll[i] * c0 + h * k * d0;
      get_f(fft, Q2, ll[i]);
      for (j = 0; j < n_layers; j++) {
        ff[0][j][i] = fft[0][j];
        ff[1][j][i] = fft[1][j];
      }
    }

// store the n sampled f values for each layer i in fa. 
// call POLINT for each layer in turn 
// do this 16 times. 
    for (m = 0; m < 16; m++) {
// check to see that we haven't just calculated f at l(m) 
	know_f = false;
	for (i = 0; i < n; i++) {
	    if (m == list[i]) {
//		p = i;
		know_f = true;
	    }
	}
// if we have, then simply copy it 
	if (know_f) {
	    for (i = 0; i < n_layers; i++) {
		  f[0][i][m] = ff[0][i][m];
		  f[1][i][m] = ff[1][i][m];
	    }
	} else {
// else, use polynomial interpolation. 
	    for (i = 0; i < n_layers; i++) {
		  for (j = 0; j < n; j++) {
			fa[0][j] = ff[0][i][j];
			fa[1][j] = ff[1][i][j];
		  }
		  polint(ll, fa, n, ag_l[m], f_ans, f_error, ok);
			if (!ok[0]) {
				System.out.println("POLINT returned an error to APPR_F.");
				return;
			}
			f[0][i][m] = f_ans[0];
			f[1][i][m] = f_ans[1];
	    }
	}
    }

    return;
} 



//  
// Title: ATOMS 
// Authors: MMJT 
// Date: 18 Feb 90 
// Description: This routine lists the legal atom names accepted by 
// DIFFaX. These names are taken from the file 'sfname'. 

//      ARGUMENTS: 
//           No arguments are used. 

//      COMMON VARIABLES: 
//            uses:  sfname 

//        modifies:  No COMMON variables are modified. 
//  

/* void atoms_()
{
    // Local variables 

// sfname has been opened once already, 
// no need for elaborate error checking 
//      open(unit = sf, file = sfname, status = 'old', err = 999) 

//      write(op,200) 'Legal atom types are:' 

// skip first few lines which contain no data, until we come to Hydrogen 
//    1 read(sf,'(a)') line 
//        if(line(1:4).ne.'H   ') goto 1 
//      backspace(unit = sf, err = 500) 

// initialize atomline 
//      atomline = ' ' 
// write 10 atom names to a line 
//   10 i = 0 
//   20 read(sf, '(a)', end = 100) line 
//        atom_name = line(1:4) 
//        write(atomline(i*7+1:i*7+7),'(3a)') '''', atom_name, ''' ' 
//        i = i + 1 
//        if(mod(i,10).eq.0) then 
//          write(op,210) atomline 
//          goto 10 
//        endif 
//        goto 20 
//  100 continue 

//      close(unit = sf, err = 600) 

//  999 return 
//  500 write(op,220) 'Unable to backspace scattering factor file ''', 
//     |    sfname(1:LENGTH(sfname)), '''.' 
    return;
//  600 write(op,220) 'Unable to close scattering factor file ''', 
//     |    sfname(1:LENGTH(sfname)), '''.' 
//      return 
//  200 format(1x, a) 
//  210 format(2x, a) 
//  220 format(1x, 'ERROR in ATOMS: ', 3a) 
} // atoms_ */



//  
// Title: BINPOW 
// Author: MMJT 
// Date: 18 Mar 1990 
// Description:  This function breaks down the number 'n' into its 
// binary representation. The result is stored in the global array 'pow'. 
// This is used for efficiently multiplying a square matrix by itself 
// n times if the RECURSIVE option was chosen for a finite number of 
// layers. 

// n must be such that n <= RCSV_MAX+1 <= 2**(MAX_BIN+1) - 1 

//      ARGUMENTS: 
//            n   -  number to binarize. (input). 

//      COMMON VARIABLES: 
//            uses:  max_pow 

//        modifies:  max_pow, pow 

// BINPOW returns boolean TRUE if no problems were encountered. 
//  

boolean binpow(int n)
{
    // Local variables 
    int i, j;
    int itmp;

    itmp = n;
    max_pow = 0;
    i = MAX_BIN + 1;

    while(i > 1) {
		--i;
		j = (int) (itmp / Math.pow(2, i - 1));
// j should be either 1 or zero if n was within bounds 
	if (j == 1) {
	    pow[i - 1] = 1;
	    itmp -= Math.pow(2, i - 1);
	    if (max_pow == 0) 
			max_pow = i;
		else if (j == 0)
			pow[i] = 0;
		else {
			System.out.println("ERROR in BINPOW: Invalid exponent " + n);
			System.out.println("Maximum No. of layers allowed = " + RCSV_MAX);
			System.out.println("Maximum supported by DIFFaX is " + (Math.pow(2, MAX_BIN+1)-1));
			return false;
		}
	}
    }

    return true;
} 



//  
// Title: BOUNDS 
// Authors: MMJT 
// Date: 24 Feb 1990 
// Description: This function translates the value x so that it lies 
// within the range 0 to 1. 

//      ARGUMENTS: 
//                  x  -  real number to convert. (input). 

//      COMMON VARIABLES: 

//                  No COMMON variables are used 

//      BOUNDS returns the translated value of x. x is not modified. 
//  

double bounds(double x)
{
    double y;

    y = x - (int) x + ONE;
    y -= (int) y;

// allow for rounding error 
    if (ONE - y < eps5) 
	  y = 0.;
    return y;
}


//  
// Title: CHK_SYM 
// Author: MMJT 
// Date: 15 Aug 1989; 21 Jan 1995 
// Checks the user's assertions in the data file about the symmetry of 
// the diffraction pattern. The symmetry is used to define the smallest 
// possible volume in reciprocal space over which integration can 
// occur and be representative of the whole pattern. CHK_SYM gives the 
// user a crude 'goodness of fit', or tolerance, within which a random 
// sampling of intensities fit the asserted symmetry. CHK_SYM does 
// not override the user's judgment if the fit is poor, unless the cell 
// dimensions or angle are inconsistent with the requested symmetry. The 
// intention of CHK_SYM is to alert the user that the data does not 
// conform to the expected symmetry. Useful for debugging datafiles. 

//      ARGUMENTS: 
//            ok  -  boolean flag indicating all went well. (output). 

//      COMMON VARIABLES: 
//            uses:  cell_a, cell_b, cell_gamma, max_angle, SymGrpNo, 
//                   pnt_grp, tolerance, PI, PI2, PS_VGT, RAD2DEG 

//        modifies:  max_var, h_mirror, k_mirror, check_sym, SymGrpNo 
//  

	static String symError = "ERROR in CHK_SYM";

void chk_sym(boolean[] ok)
{
    // Local variables 
    boolean eq_sides;
    double tmp;
    boolean diad;
    int idum;
    boolean cell90, triad, cell120, tetrad;

// reinitialize random numbers in RAN3 
    idum = -1;

    diad = false;
    triad = false;
    tetrad = false;
    cell90 = Math.abs(cell_gamma - HALF*pi) < HALF*pi*eps6;
    cell120 = Math.abs(cell_gamma - pi2/THREE) < pi2*eps6/THREE;

// sample reciprocal space to get an idea of the sort of intensities 
// that are out there. 
// 360 degrees, no symmetry (-1) 
// there is nothing to check. The center of symmetry is a given. 
    if (symgrpno == 1) {
		max_var = ZERO;
		symmetryOutput900();
		return;
    }

// 180 degrees, rotation only (2/M, 1st setting) 
    if (symgrpno == 2) {
	diad = tst_rot(2, idum, ok);
	if (!ok[0]) {
		System.out.println(symError);
	    return;
	}
		symmetryOutput900();
		return;
    }

// 180 degrees, vertical mirror (2/M, 2nd setting) 
    if (symgrpno == 3) {
	h_mirror = tst_mir(1, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
		tmp = max_var;
	max_var = ZERO;
	k_mirror = tst_mir(2, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
		max_var = Math.max(tmp, max_var);
		symmetryOutput900();
		return;
    }

// 90 degrees, vertical mirrors (MMM) 
    if (symgrpno == 4) {
	if (! cell90) {
	    check_sym = false;
	    symgrpno = get_sym(ok);
		symmetryOutput910(ok);
		return;
	}
	diad = tst_rot(2, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
		tmp = max_var;
	max_var = ZERO;
	h_mirror = tst_mir(1, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
		tmp = Math.max(tmp, max_var);
	max_var = ZERO;
	k_mirror = tst_mir(2, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
		max_var = Math.max(tmp, max_var);
		symmetryOutput900();
		return;
    }

// the following point groups require equi-sided cells 
    if (symgrpno > 4) {
	eq_sides = Math.abs(cell_a - cell_b) <= HALF*eps6*(cell_a + cell_b);
	if (! eq_sides) {
	    check_sym = false;
	    max_var = ZERO;
	    symgrpno = get_sym(ok);
		symmetryOutput920(ok);
		return;
	}

// 120 degrees, rotation only (-3) 
	if (symgrpno == 5) {
	    if (! cell120) {
		check_sym = false;
		symgrpno = get_sym(ok);
			symmetryOutput910(ok);
			return;
	    }
	    triad = tst_rot(3, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
	    tmp = max_var;
	    max_var = ZERO;
	    h_mirror = tst_mir(1, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
	    tmp = Math.max(tmp, max_var);
	    max_var = ZERO;
	    hk_mirror = tst_mir(3, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
	    max_var = Math.max(tmp, max_var);
		symmetryOutput900();
		return;
	}

// 60 degrees, vertical mirrors (-3M) 
	if (symgrpno == 6) {
	    if (! cell120) {
		check_sym = false;
		symgrpno = get_sym(ok);
			symmetryOutput910(ok);
			return;
	    }
	    triad = tst_rot(3, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
	    tmp = max_var;
	    max_var = ZERO;
	    h_mirror = tst_mir(1, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
	    tmp = Math.max(tmp, max_var);
	    max_var = ZERO;
	    hk_mirror = tst_mir(3, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
	    max_var = Math.max(tmp, max_var);
		symmetryOutput900();
		return;
	}

// 90 degrees, rotation (4/M) 
	if (symgrpno == 7) {
	    if (! cell90) {
		check_sym = false;
		symgrpno = get_sym(ok);
			symmetryOutput910(ok);
			return;
	    }
	    tetrad = tst_rot(4, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
		symmetryOutput900();
		return;
	}

// 45 degrees, vertical mirrors (4/MMM) 
	if (symgrpno == 8) {
	    if (! cell90) {
		check_sym = false;
		symgrpno = get_sym(ok);
			symmetryOutput910(ok);
			return;
	    }
	    tetrad = tst_rot(4, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
	    tmp = max_var;
	    max_var = ZERO;
	    h_mirror = tst_mir(1, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
	    if (! h_mirror) {
		tmp = Math.max(tmp, max_var);
		max_var = ZERO;
		hk_mirror = tst_mir(3, idum, ok);
			if (!ok[0]) {
				System.out.println(symError);
				return;
			}
	    }
	    max_var = Math.max(tmp, max_var);
		symmetryOutput900();
		return;
	}

// 60 degrees, rotation (6/M) 
	if (symgrpno == 9) {
	    if (! cell120) {
		check_sym = false;
		symgrpno = get_sym(ok);
			symmetryOutput910(ok);
			return;
	    }
	    diad = tst_rot(2, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
	    tmp = max_var;
	    max_var = ZERO;
	    triad = tst_rot(3, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
	    max_var = Math.max(tmp, max_var);
		symmetryOutput900();
		return;
	}

// 30 degrees, vertical mirrors (6/MMM) 
	if (symgrpno == 10) {
	    if (! cell120) {
		check_sym = false;
		symgrpno = get_sym(ok);
			symmetryOutput910(ok);
			return;
	    }
	    diad = tst_rot(2, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
	    tmp = max_var;
	    max_var = ZERO;
	    triad = tst_rot(3, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
	    tmp = Math.max(tmp, max_var);
	    max_var = ZERO;
	    h_mirror = tst_mir(1, idum, ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
	    if (! h_mirror) {
		tmp = Math.max(tmp, max_var);
		max_var = ZERO;
		hk_mirror = tst_mir(3, idum, ok);
			if (!ok[0]) {
				System.out.println(symError);
				return;
			}
	    }
	    max_var = Math.max(tmp, max_var);
	}
    }
} 

	// The user's guess is inconsistent with cell_gamma. 
	// Override the user. 
	
	void symmetryOutput910(boolean[] ok) {
		System.out.println("The cell angle of " + cell_gamma * rad2deg + " degrees,");
		System.out.println("   is inconsistent with point group symmetry " + pnt_grp);
	    System.out.println("Re-evaluating diffraction symmetry");
		// reset check_sym flag, since we are now evaluating from scratch 
		check_sym = false; 
		symgrpno = get_sym(ok);
		if (!ok[0]) {
			System.out.println(symError);
			return;
		}
		symmetryOutput900();
		
		return;
	}
	
	// The user's guess is inconsistent with cell dimensions. 
	// Override the user. 

	void symmetryOutput920(boolean[] ok) {
		System.out.println("The cell a and b dimensions, " + cell_a + " Angstroms by " + cell_b + " Angstroms");
		System.out.println("   are inconsistent with point group symmetry " + pnt_grp);
	    System.out.println("Re-evaluating diffraction symmetry");
	// reset check_sym flag, since we are now evaluating from scratch 
				   check_sym = false; 
				   max_var = ZERO;
				   symgrpno = get_sym(ok);
				   if (!ok[0]) {
				   System.out.println(symError);
				   return;
				   }
		symmetryOutput900();
	
		return;
	}

	void symmetryOutput900() {
		System.out.println("The diffraction data fits the point group symmetry " + pnt_grp);
		if(max_var > eps6 && max_var <= eps1)
			System.out.println("  with a tolerance of one part in " + (int)(ONE / max_var));
			else if(max_var > eps1)
				System.out.println("  with a tolerance of one part in " + ONE / max_var);
				else
					System.out.println("  with a tolerance better than one part in a million.");
		return;
	}

//  
// Title: CHOICE 
// Author: MWD 
// Date: 18 Aug 1988 (modified by MMJT 6 August 1989) 
// Description: This function compares a string with an array of keys, 
// and returns the index of the key that matches the first characters 
// of the string. CHOICE returns -1 if an error is found. 

//      ARGUMENTS: 
//            flag  -  Character string to be matched. (input). 
//            list  -  Array of character strings. (input). 
//            n     -  Length of the array 'list'. (input). 
//  

/*int choice_(char *flag, char *list, int *n, ftnlen flag_len, ftnlen
	list_len)
{
    // System generated locals 
    int ret_val;

    // Local variables 
    int i, j1, j2;
    boolean loopon;

//      character*(*) flag, list(n) 

    // Parameter adjustments 
    list -= 80;

    // Function Body 
    i = 1;
    j1 = length_(flag, flag_len);

    loopon = true;
    while(loopon) {
	j2 = length_(list + i * 80, (ftnlen)80);
// see if the string contained in list(i) is identical to that in flag 
	if (j1 == j2 && i_indx(flag, list + i * 80, flag_len, j2) == 1) {
	    loopon = false;
	} else {
	    ++i;
	    if (i > *n) {
		loopon = false;
	    }
	}
    }

    if (i > *n) {
	i = -1;
    }
    ret_val = i;

    return ret_val;
} // choice_ */


//  
// Title: CHWDTH 
// Author: MMJT 
// Date: 6 Mar 1995; 31st Oct 1996 
// Description:  This routine adds shape broadening caused by the finite 
// lateral width of the layers. This routine does not add the broadening 
// caused by finite width in the stacking direction. That is handled in 
// a different manner by the routine INTEN2 and associated routines. 
// The broadening handled here is added phenomenobooleanly by applying a 
// Lorentzian profile to the computed intensity at each peak in the h-k 
// plane. If we are on the 00l axis the shape broadening is not 
// symmetrical. For a Lorentzian shape broadening intensity profile in 
// the h-k plane, the curvature of the Ewald sphere ensures a sharp 
// onset of intensity with a slowly decaying tail at higher l values. For 
// a symmetrical disk, the integrated intensity decays logarithmically. 
// In the event that the crystal width is different in the h, and h- 
// perpendicular directions, the disk of confusion becomes elongated 
// into a streak perpendicular to l, and the tail becomes more 
// Lorentzian-like. This is modelled in a phenomenoboolean fashion, by 
// mixing Lorentzian and logarithmic terms in a manner that depends 
// on the ratio Wa/Wb. The costly logarithm function is avoided by 
// using its derivative. 
// When off the 00l axis, the broadening is modelled as a symmetric 
// Lorentzian whose half-width depends on the angle that the Ewald 
// sphere intercepts the disk of confusion (controlled by l). If 
// the lateral dimensions are not equal, then the half width is also 
// dependent on h and k. The Lorentzian is pre-computed in OPTIMZ to gain 
// computational speed. 
// This routine is called by GETSPC. 

//      ARGUMENTS: 
//            h       -  Reciprocal space index. (input). 
//            k       -  Reciprocal space index. (input). 
//            l0      -  Lower bound of the l reciprocal space index 
//                       that is being integrated over. (input). 
//            l1      -  Lower bound of the l reciprocal space index 
//                       that is being integrated over. (input). 
//            x       -  The integrated intensity value along the 
//                       line defined by h,k,l0,l1. (input). 
//            m       -  The current index of the array 'spec' 
//                       corresponding to h,k,l0,l1. (input). 
//          max_indx  -  Maximum array value in spec that will be 
//                       accessed. (input). 

//      COMMON VARIABLES: 
//            uses:      spec, brd_spec, FFACT_SIZE, formfactor, d_theta 
//                       ffact_scale, ffhkcnst, ffwdth 

//        modifies:      spec, brd_spec 
//  

void chwdth(int h, int k, double l0,
	double l1, double x, int m, int max_indx)
{
    // Local variables 
    int i;
    double l;
    int n, p;
    double dx, xx, avg, tmp, d_hk;
    double n_hw;
    int indx;
    double norm, scale, h_wdth;

// indx indexes into the arrays spec and brd_spec 
// n indexes into the array formfactor 
// p is the index of the centroid of the array formfactor 
// h_wdth contains the effective half-width of the size broadening 
// ffhkcnst depends only on h and k, and was computed in GETSPC 
// scale contains the calibration term for the formfactor array 
// d_hk is the average radius in reciprocal Angstroms that the 
// Ewald sphere of radius theta+d_theta intercepts the 00l plane 
// brd_spc is used for temporary storage. 
// We are only accessing half of the symmetrical formfactor array 

// statement functions 
// S is the value of 1/d**2 at hkl 

// *********************************************************************** 
// special case if we are on the l axis 
// Don't bother if the intensity is negligible in the first place 
    if (h == 0 && k == 0 && x > tiny_inty * TEN) {
	l = (l0 + l1) * HALF;
	d_hk = l * TWO * d_theta / (lambda * cell_c * ffwdth * ffwdth);
	norm = ZERO;
	indx = 0;
	xx = wa / wb;
	if (xx > ONE) {
	    xx = ONE / xx;
	}
	dx = ONE;
	while(dx >= eps5) {
	    ++indx;
	    tmp = ONE / (ONE + indx*d_hk);
// balance streak, versus disk of confusion (phenomenoboolean treatment) 
// xx=1 means disk, xx=0 means streak. Intensity falls off more 
// rapidly for a streak 
	    dx = ((ONE - xx) * Math.sqrt(indx) * tmp + xx) * tmp;
	    if (m + indx - 1 <= max_indx)
		brd_spc[m + indx - 2] = dx;
	    norm += dx;
// eps5 is reasonable. However, it may be worth experimenting more. 
	}

	norm = x / norm;
	for (i = 0; i < indx; i++)
	    if (m + i <= max_indx)
		spec[m + i - 1] += norm * brd_spc[m + i - 1];

// We were on the 00l axis, we can exit now 
	return;
    }

// *********************************************************************** 
// We are not on the l-axis. Broadening is handled differently. 
// scale relates the formfactor array indices to the spec array indices. 
// h_wdth and ffact_scale should never be zero. In case they are, 
// make scale large enough so that n.ge.p-1 and the loop below is 
// exited early 
    double d1 = HALF * (l0 + l1);
    h_wdth = ffhkcnst / Math.sqrt(h * h * a0 + k * k * b0 + d1 * d1 * c0 + h * k * d0);
    if (h_wdth > ZERO && ffact_scale > ZERO)
	scale = d_theta / (ffact_scale * h_wdth);
    else
	scale = FFACT_SIZE;

    p = FFACT_SIZE/2 + 1;
    norm = ONE;
    brd_spc[m - 1] = ONE;
    indx = 0;
    n = p - 2;
    while (n < p - 1) {
	++indx;
	n_hw = scale * indx;
	n = (int) n_hw;
//        if(n.ge.p-1) goto 50 
	if (n < p - 1) {
// linear interpolation of the pre-computed pseudo-Lorentzian 
	    xx = n_hw - n;
	    avg = (ONE - xx) * formfactor[p + n - 1] + xx * formfactor[p + n];
	    if (m + indx <= max_indx)
		brd_spc[m + indx - 1] = avg;
	    if (m - indx > 0)
		brd_spc[m - indx - 1] = avg;
// intensity x is being redistributed. We will need to normalize later 
	    norm += avg * TWO;
	}
    }

    norm = x / norm;
    spec[m - 1] += norm * brd_spc[m - 1];
    for (i = 0; i < indx - 1; i++) {
	if (m + i <= max_indx)
	    spec[m + i - 1] += norm * brd_spc[m + i - 1];
	if (m - i > 0)
	    spec[m - i - 1] += norm * brd_spc[m - i - 1];
    }

    return;
} 



// *********************************************************************** 
// ***************************LINPACK ROUTINES**************************** 
// *********************************************************************** 

// The following are the standard Linpack routines for solving complex 
// simultaneous equations. They were found to reduce DIFFaX run time by 
// significant amount (30% in one case) compared with the Numerical 
// Recipes routines LUDCMP and LUBKSB. The only changes are 

//                         complex -> complex*16 
//                         real    -> real*8 
//                         real()  -> dble() 
//                         aimag   -> dimag 

// *********************************************************************** 
//  
// Title: CGESL (LINPACK ROUTINE) 
// Author: cleve moler, university of new mexico, argonne national lab. 
// Date: linpack. this version dated 08/14/78 
// Description: 
//     CGESL solves the complex system 
//     a * x = b  or  ctrans(a) * x = b 
//     using the factors computed by cgeco or CGEFA. 

//     on entry 

//        a       complex(lda, n) 
//                the output from cgeco or CGEFA. 

//        lda     int 
//                the leading dimension of the array  a . 

//        n       int 
//                the order of the matrix  a . 

//        ipvt    int(n) 
//                the pivot vector from cgeco or CGEFA. 

//        b       complex(n) 
//                the right hand side vector. 

//        job     int 
//                = 0         to solve  a*x = b , 
//                = nonzero   to solve  ctrans(a)*x = b  where 
//                            ctrans(a)  is the conjugate transpose. 

//     on return 

//        b       the solution vector  x . 

//     error condition 

//        a division by zero will occur if the input factor contains a 
//        zero on the diagonal.  technically this indicates singularity 
//        but it is often caused by improper arguments or improper 
//        setting of lda .  it will not occur if the subroutines are 
//        called correctly and if cgeco has set rcond .gt. 0.0 
//        or CGEFA has set info .eq. 0 . 

//     to compute  inverse(a) * c  where  c  is a matrix 
//     with  p  columns 
//           call cgeco(a,lda,n,ipvt,rcond,z) 
//           if (rcond is too small) go to ... 
//           do 10 j = 1, p 
//              call CGESL(a,lda,n,ipvt,c(1,j),0) 
//        10 continue 

//     subroutines and functions 

//     blas CAXPY,CDOTC 
//     fortran conjg 

//  

void cgesl(double[][][] a, int lda, int n, int[] ipvt, double[] b, int job)
{
    // System generated locals 
    double[] z1, z2, z3;

    // Local variables 
    int k, l;
    double[] t;
    int kb, nm1;

    // Function Body 
    nm1 = n - 1;
    if (job == 0) {

//        job = 0 , solve  a * x = b 
//        first solve  l*y = b 

	if (nm1 >= 1) {
	    for (k = 1; k <= nm1; k++) {
		l = ipvt[k-1];
		t[0] = b[0][l], t[1] = b[1][l];
		if (l != k) {
		    b[0][l-1] = b[0][k-1], b[1][l-1] = b[1][k-1];
		    b[0][k-1] = t[0], b[1][k-1] = t[1];
		}
		caxpy(n-k, t, a[][k][k-1], 1, b[][k], 1);
	    }
	}

//        now solve  u*x = y 

	for (kb = 1; kb <= n; kb++) {
	    k = n + 1 - kb;
	    z_div(t, b[][k-1], a[k-1][k-1]);
	    b[0][k-1] = t[0], b[1][k-1] = t[1];
	    t[0] = -t[0], t[1] = -t[1];
	    caxpy(k-1, t, a[][0][k-1], 1, b[][0], 1);
	}
	return;
    }

//        job = nonzero, solve  ctrans(a) * x = b 
//        first solve  ctrans(u)*y = b 

    i1 = *n;
    for (k = 1; k <= i1; ++k) {
	i2 = k - 1;
	cdotc_(&z1, &i2, &a[k * a_dim1 + 1], &c1, &b[1], &c1);
	t[0] = z1[0], t[1] = z1[1];
	i2 = k;
	i3 = k;
	z2[0] = b[0][i3] - t[0], z2[1] = b[1][i3] - t[1];
	d_cnjg(&z3, &a[k + k * a_dim1]);
	z_div(&z1, &z2, &z3);
	b[0][i2] = z1[0], b[1][i2] = z1[1];
// L60: 
    }

//        now solve ctrans(l)*x = y 

    if (nm1 >= 1) {
	i1 = nm1;
	for (kb = 1; kb <= i1; ++kb) {
	    k = *n - kb;
	    i2 = k;
	    i3 = k;
	    i4 = *n - k;
	    cdotc_(&z2, &i4, &a[k + 1 + k * a_dim1], &c1, &b[k + 1], &
		    c1);
	    z1[0] = b[0][i3] + z2[0], z1[1] = b[1][i3] + z2[1];
	    b[0][i2] = z1[0], b[1][i2] = z1[1];
	    l = ipvt[k];
	    if (l != k) {
		i2 = l;
		t[0] = b[0][i2], t[1] = b[1][i2];
		i2 = l;
		i3 = k;
		b[0][i2] = b[0][i3], b[1][i2] = b[1][i3];
		i2 = k;
		b[0][i2] = t[0], b[1][i2] = t[1];
	    }
// L80: 
	}
    }
    return 0;
} // cgesl_ */

    /*
     * dgefa factors a double precision matrix by gaussian elimination.
     * 
     * dgefa is usually called by dgeco, but it can be called directly with a
     * saving in time if rcond is not needed. (time for dgefa) .
     * 
     * on entry
     * 
     * a double precision[n][lda] the matrix to be factored.
     * 
     * lda integer the leading dimension of the array a .
     * 
     * n integer the order of the matrix a .
     * 
     * on return
     * 
     * a an upper triangular matrix and the multipliers which were used to
     * obtain it. u where l is a product of permutation and unit lower
     * triangular matrices and u is upper triangular.
     * 
     * ipvt integer[n] an integer vector of pivot indices.
     * 
     * info integer = 0 normal value. = k if u[k][k] .eq. 0.0 . this is not an
     * error condition for this subroutine, but it does indicate that dgesl or
     * dgedi will divide by zero if called. use rcond in dgeco for a reliable
     * indication of singularity.
     * 
     * linpack. this version dated 08/14/78. cleve moler, university of new
     * mexico, argonne national lab.
     * 
     * functions
     * 
     * blas daxpy,dscal,idamax
     */
    final int dgefa(final double a[][], final int n, int ipvt[]) {
        double[] col_k, col_j;
        double t;
        int j, k, kp1, l, nm1;
        int info;
		
        // gaussian elimination with partial pivoting
		
        info = 0;
        nm1 = n - 1;
        if (nm1 >= 0) {
            for (k = 0; k < nm1; k++) {
                col_k = a[k];
                kp1 = k + 1;
				
                // find l = pivot index
				
                l = idamax(n - k, col_k, k, 1) + k;
                ipvt[k] = l;
				
                // zero pivot implies this column already triangularized
				
                if (col_k[l] != 0) {
					
                    // interchange if necessary
					
                    if (l != k) {
                        t = col_k[l];
                        col_k[l] = col_k[k];
                        col_k[k] = t;
                    }
					
                    // compute multipliers
					
                    t = -1.0 / col_k[k];
                    dscal(n - (kp1), t, col_k, kp1, 1);
					
                    // row elimination with column indexing
					
                    for (j = kp1; j < n; j++) {
                        col_j = a[j];
                        t = col_j[l];
                        if (l != k) {
                            col_j[l] = col_j[k];
                            col_j[k] = t;
                        }
                        daxpy(n - (kp1), t, col_k, kp1, 1, col_j, kp1, 1);
                    }
                } else {
                    info = k;
                }
            }
        }
        ipvt[n - 1] = n - 1;
        if (a[(n - 1)][(n - 1)] == 0)
            info = n - 1;
		
        return info;
    }
	
    /*
     * dgesl solves the double precision system x = b or trans(a) * x = b using
     * the factors computed by dgeco or dgefa.
     * 
     * on entry
     * 
     * a double precision[n][lda] the output from dgeco or dgefa.
     * 
     * lda integer the leading dimension of the array a .
     * 
     * n integer the order of the matrix a .
     * 
     * ipvt integer[n] the pivot vector from dgeco or dgefa.
     * 
     * b double precision[n] the right hand side vector.
     * 
     * job integer x = b , x = b where trans(a) is the transpose.
     * 
     * on return
     * 
     * b the solution vector x .
     * 
     * error condition
     * 
     * a division by zero will occur if the input factor contains a zero on the
     * diagonal. technically this indicates singularity but it is often caused
     * by improper arguments or improper setting of lda . it will not occur if
     * the subroutines are called correctly and if dgeco has set rcond .gt. 0.0
     * or dgefa has set info .eq. 0 .
     * 
     * c where c is a matrix with p columns dgeco(a,lda,n,ipvt,rcond,z) if
     * (!rcond is too small){ for (j=0,j
     * <p,j++) dgesl(a,lda,n,ipvt,c[j][0],0); }
     * 
     * linpack. this version dated 08/14/78 . cleve moler, university of new
     * mexico, argonne national lab.
     * 
     * functions
     * 
     * blas daxpy,ddot
     */
    final void dgesl(final double a[][], final int n, final int ipvt[], double b[], final int job) {
        double t;
        int k, kb, l, nm1, kp1;
		
        nm1 = n - 1;
        if (job == 0) {
			
            // job = 0 , solve a * x = b. first solve l*y = b
			
            if (nm1 >= 1) {
                for (k = 0; k < nm1; k++) {
                    l = ipvt[k];
                    t = b[l];
                    if (l != k) {
                        b[l] = b[k];
                        b[k] = t;
                    }
                    kp1 = k + 1;
                    daxpy(n - (kp1), t, a[k], kp1, 1, b, kp1, 1);
                }
            }
			
            // now solve u*x = y
			
            for (kb = 0; kb < n; kb++) {
                k = n - (kb + 1);
                b[k] /= a[k][k];
                t = -b[k];
                daxpy(k, t, a[k], 0, 1, b, 0, 1);
            }
        } else {
			
            // job = nonzero, solve trans(a) * x = b. first solve trans(u)*y =
            // b
			
            for (k = 0; k < n; k++) {
                t = ddot(k, a[k], 0, 1, b, 0, 1);
                b[k] = (b[k] - t) / a[k][k];
            }
			
            // now solve trans(l)*x = y
			
            if (nm1 >= 1) {
                for (kb = 1; kb < nm1; kb++) {
                    k = n - (kb + 1);
                    kp1 = k + 1;
                    b[k] += ddot(n - (kp1), a[k], kp1, 1, b, kp1, 1);
                    l = ipvt[k];
                    if (l != k) {
                        t = b[l];
                        b[l] = b[k];
                        b[k] = t;
                    }
                }
            }
        }
    }
	
    /*
     * constant times a vector plus a vector. jack dongarra, linpack, 3/11/78.
     */
    final void daxpy(
					 final int n,
					 final double da,
					 final double dx[],
					 final int dx_off,
					 final int incx,
					 double dy[],
					 final int dy_off,
					 final int incy) {
        int i, ix, iy;
		
        if ((n > 0) && (da != 0)) {
            if (incx != 1 || incy != 1) {
				
                // code for unequal increments or equal increments not equal to
                // 1
				
                ix = 0;
                iy = 0;
                if (incx < 0)
                    ix = (-n + 1) * incx;
                if (incy < 0)
                    iy = (-n + 1) * incy;
                for (i = 0; i < n; i++) {
                    dy[iy + dy_off] += da * dx[ix + dx_off];
                    ix += incx;
                    iy += incy;
                }
                return;
            } else {
				
                // code for both increments equal to 1
				
                for (i = 0; i < n; i++)
                    dy[i + dy_off] += da * dx[i + dx_off];
            }
        }
    }
	
    /*
     * forms the dot product of two vectors. jack dongarra, linpack, 3/11/78.
     * mcj: declared some parameters as final
     */
    final double ddot(
					  final int n,
					  final double dx[],
					  final int dx_off,
					  final int incx,
					  final double dy[],
					  final int dy_off,
					  final int incy) {
        double dtemp;
        int i, ix, iy;
		
        dtemp = 0;
		
        if (n > 0) {
            if (incx != 1 || incy != 1) {
                // code for unequal increments or equal increments not equal to
                // 1
				
                ix = 0;
                iy = 0;
                if (incx < 0)
                    ix = (-n + 1) * incx;
                if (incy < 0)
                    iy = (-n + 1) * incy;
                for (i = 0; i < n; i++) {
                    dtemp += dx[ix + dx_off] * dy[iy + dy_off];
                    ix += incx;
                    iy += incy;
                }
            } else {
                // code for both increments equal to 1
                for (i = 0; i < n; i++)
                    dtemp += dx[i + dx_off] * dy[i + dy_off];
            }
        }
        return (dtemp);
    }
	
    /*
     * scales a vector by a constant. jack dongarra, linpack, 3/11/78.
     * mcj: declared some parameters as final
     */
    final void dscal(final int n, final double da, double dx[], final int dx_off, final int incx) {
        int i, nincx;
		
        if (n > 0) {
            if (incx != 1) {
				
                // code for increment not equal to 1
				
                nincx = n * incx;
                for (i = 0; i < nincx; i += incx)
                    dx[i + dx_off] *= da;
            } else {
				
                // code for increment equal to 1
				
                for (i = 0; i < n; i++)
                    dx[i + dx_off] *= da;
            }
        }
    }
	
    /*
     * finds the index of element having max. absolute value. jack dongarra,
     * linpack, 3/11/78.
     */
    final int idamax(final int n, final double dx[], final int dx_off, final int incx) {
        double dmax, dtemp;
        int i, ix, itemp = 0;
		
        if (n < 1) {
            itemp = -1;
        } else if (n == 1) {
            itemp = 0;
        } else if (incx != 1) {
			
            // code for increment not equal to 1
			
            dmax = abs(dx[dx_off]);
            ix = 1 + incx;
            for (i = 1; i < n; i++) {
                dtemp = abs(dx[ix + dx_off]);
                if (dtemp > dmax) {
                    itemp = i;
                    dmax = dtemp;
                }
                ix += incx;
            }
        } else {
			
            // code for increment equal to 1
			
            itemp = 0;
            dmax = abs(dx[dx_off]);
            for (i = 1; i < n; i++) {
                dtemp = abs(dx[i + dx_off]);
                if (dtemp > dmax) {
                    itemp = i;
                    dmax = dtemp;
                }
            }
        }
        return (itemp);
    }
	
    /*
     * estimate unit roundoff in quantities of size x.
     * 
     * this program should function properly on all systems satisfying the
     * following two assumptions, 1. the base used in representing dfloating
     * point numbers is not a power of three. 2. the quantity a in statement 10
     * is represented to the accuracy used in dfloating point variables that
     * are stored in memory. the statement number 10 and the go to 10 are
     * intended to force optimizing compilers to generate code satisfying
     * assumption 2. under these assumptions, it should be true that, a is not
     * exactly equal to four-thirds, b has a zero for its last bit or digit, c
     * is not exactly equal to one, eps measures the separation of 1.0 from the
     * next larger dfloating point number. the developers of eispack would
     * appreciate being informed about any systems where these assumptions do
     * not hold.
     * 
     * **************************************************************** this
     * routine is one of the auxiliary routines used by eispack iii to avoid
     * machine dependencies.
     * ****************************************************************
     * 
     * this version dated 4/6/83.
     */
    final double epslon(final double x) {
        final double a = 4.0e0 / 3.0e0;
        double b, c, eps=0;
        
        while (eps == 0) {
            b = a - 1.0;
            c = b + b + b;
            eps = abs(c - 1.0);
        }
        return (eps * abs(x));
    }
	
    /*
     * purpose: multiply matrix m times vector x and add the result to vector y.
     * 
     * parameters:
     * 
     * n1 integer, number of elements in vector y, and number of rows in matrix
     * m
     * 
     * y double [n1], vector of length n1 to which is added x
     * 
     * n2 integer, number of elements in vector x, and number of columns in
     * matrix m
     * 
     * ldm integer, leading dimension of array m
     * 
     * x double [n2], vector of length n2
     * 
     * m double [ldm][n2], matrix of n1 rows and n2 columns
     */
    final void dmxpy(final int n1, double y[], final int n2, final double x[], final double m[][]) {
        int j, i;
		
        // cleanup odd vector
        for (j = 0; j < n2; j++) {
            for (i = 0; i < n1; i++) {
                y[i] += x[j] * m[j][i];
            }
        }
    }
	
	//  
// Title: CAXPY (LINPACK ROUTINE) 
// Author: jack dongarra 
// Date: linpack, 3/11/78 
// Description: constant times a vector plus a vector. 
//  

int caxpy_(int *n, double[] ca, double[] *cx,
	int *incx, double[] *cy, int *incy)
{
    // System generated locals 
    int i1, i2, i3, i4;
    double d1, d2;
    double[] z1, z2;

    // Local variables 
    int i, ix, iy;




    // Parameter adjustments 
    --cy;
    --cx;

    // Function Body 
    if (*n <= 0) {
	return 0;
    }
    if ((d1 = ca[0], Math.abs(d1)) + (d2 = d_imag(ca), Math.abs(d2)) == 0.f) {
	return 0;
    }
    if (*incx != 1 || *incy != 1) {

//        code for unequal increments or equal increments 
//          not equal to 1 

	ix = 1;
	iy = 1;
	if (*incx < 0) {
	    ix = (-(*n) + 1) * *incx + 1;
	}
	if (*incy < 0) {
	    iy = (-(*n) + 1) * *incy + 1;
	}
	i1 = *n;
	for (i = 0; i < i1; i++) {
	    i2 = iy;
	    i3 = iy;
	    i4 = ix;
	    z2[0] = ca[0] * cx[0][i4] - ca[1] * cx[1][i4], z2[1] = ca[0] *
		     cx[1][i4] + ca[1] * cx[0][i4];
	    z1[0] = cy[0][i3] + z2[0], z1[1] = cy[1][i3] + z2[1];
	    cyv[i2] = z1[0], cy[1][i2] = z1[1];
	    ix += *incx;
	    iy += *incy;
// L10: 
	}
	return 0;

//        code for both increments equal to 1 

    }
    i1 = *n;
    for (i = 0; i < i1; i++) {
	i2 = i;
	i3 = i;
	i4 = i;
	z2[0] = ca[0] * cx[0][i4] - ca[1] * cx[1][i4], z2[1] = ca[0] * cx[1][
		i4] + ca[1] * cx[0][i4];
	z1[0] = cy[0][i3] + z2[0], z1[1] = cy[1][i3] + z2[1];
	cy[0][i2] = z1[0], cy[1][i2] = z1[1];
// L30: 
    }
    return 0;
} // caxpy_ 

//  
// Title: CDOTC (LINPACK ROUTINE) 
// Author: jack dongarra 
// Date: linpack,  3/11/78. 
// Description: 
//     forms the dot product of two vectors, conjugating the first 
//     vector. 
//  

 void cdotc_(double[] * ret_val, int *n,
	double[] *cx, int *incx, double[] *cy, int *incy)
{
    // System generated locals 
    int i1, i2;
    double[] z1, z2, z3;

    // Local variables 
    int i, ix, iy;
    double[] ctemp;

    // Parameter adjustments 
    --cy;
    --cx;

    // Function Body 
    ctemp[0] = 0.f, ctemp[1] = 0.f;
     ret_val[0] = 0.f,  ret_val[1] = 0.f;
    if (*n <= 0) {
	return ;
    }
    if (*incx != 1 || *incy != 1) {

//        code for unequal increments or equal increments 
//          not equal to 1 

	ix = 1;
	iy = 1;
	if (*incx < 0) {
	    ix = (-(*n) + 1) * *incx + 1;
	}
	if (*incy < 0) {
	    iy = (-(*n) + 1) * *incy + 1;
	}
	i1 = *n;
	for (i = 0; i < i1; i++) {
	    d_cnjg(&z3, &cx[ix]);
	    i2 = iy;
	    z2[0] = z3[0] * cy[0][i2] - z3[1] * cy[1][i2], z2[1] =
		    z3[0] * cy[1][i2] + z3[1] * cy[0][i2];
	    z1[0] = ctemp[0] + z2[0], z1[1] = ctemp[1] + z2[1];
	    ctemp[0] = z1[0], ctemp[1] = z1[1];
	    ix += *incx;
	    iy += *incy;
// L10: 
	}
	 ret_val[0] = ctemp[0],  ret_val[1] = ctemp[1];
	return ;

//        code for both increments equal to 1 

    }
    i1 = *n;
    for (i = 0; i < i1; i++) {
	d_cnjg(&z3, &cx[i]);
	i2 = i;
	z2[0] = z3[0] * cy[0][i2] - z3[1] * cy[1][i2], z2[1] = z3[0] *
		cy[1][i2] + z3[1] * cy[0][i2];
	z1[0] = ctemp[0] + z2[0], z1[1] = ctemp[1] + z2[1];
	ctemp[0] = z1[0], ctemp[1] = z1[1];
// L30: 
    }
     ret_val[0] = ctemp[0],  ret_val[1] = ctemp[1];
    return ;
} // cdotc_

//  
// Title: CGEFA (LINPACK ROUTINE) 
// Author: cleve moler, university of new mexico, argonne national lab. 
// Date: linpack. this version dated 08/14/78 
// Description: 

//     CGEFA factors a complex matrix by gaussian elimination. 

//     CGEFA is usually called by cgeco, but it can be called 
//     directly with a saving in time if  rcond  is not needed. 
//     (time for cgeco) = (1 + 9/n)*(time for CGEFA) . 

//     on entry 

//        a       complex(lda, n) 
//                the matrix to be factored. 

//        lda     int 
//                the leading dimension of the array  a . 

//        n       int 
//                the order of the matrix  a . 

//     on return 

//        a       an upper triangular matrix and the multipliers 
//                which were used to obtain it. 
//                the factorization can be written  a = l*u  where 
//                l  is a product of permutation and unit lower 
//                triangular matrices and  u  is upper triangular. 

//        ipvt    int(n) 
//                an int vector of pivot indices. 

//        info    int 
//                = 0  normal value. 
//                = k  if  u(k,k) .eq. 0.0 .  this is not an error 
//                     condition for this subroutine, but it does 
//                     indicate that CGESL or cgedi will divide by zero 
//                     if called.  use  rcond  in cgeco for a reliable 
//                     indication of singularity. 

//     subroutines and functions 

//     blas CAXPY,CSCAL,ICAMAX 
//     fortran abs,aimag,real 
//  

 int cgefa_(double[] *a, int *lda, int *n,
	int *ipvt, int *info)
{
    // System generated locals 
    int a_dim1, a_offset, i1, i2, i3, i4;
    double d1, d2;
    double[] z1;

    // Local variables 
    int j, k, l;
    double[] t;
    int kp1, nm1;

//     gaussian elimination with partial pivoting 

    // Parameter adjustments 
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --ipvt;

    // Function Body 
    *info = 0;
    nm1 = *n - 1;
    if (nm1 >= 1) {
	i1 = nm1;
	for (k = 1; k <= i1; ++k) {
	    kp1 = k + 1;

//        find l = pivot index 

	    i2 = *n - k + 1;
	    l = icamax_(&i2, &a[k + k * a_dim1], &c1) + k - 1;
	    ipvt[k] = l;

//        zero pivot implies this column already triangularized 

	    i2 = l + k * a_dim1;
	    if ((d1 = a[0][i2], Math.abs(d1)) + (d2 = d_imag(&a[l + k *
		    a_dim1]), Math.abs(d2)) != 0.f) {

//           interchange if necessary 

		if (l != k) {
		    i2 = l + k * a_dim1;
		    t[0] = a[0][i2], t[1] = a[1][i2];
		    i2 = l + k * a_dim1;
		    i3 = k + k * a_dim1;
		    a[0][i2] = a[0][i3], a[1][i2] = a[1][i3];
		    i2 = k + k * a_dim1;
		    a[0][i2] = t[0], a[1][i2] = t[1];
		}

//           compute multipliers 

		z_div(&z1, &c_b82, &a[k + k * a_dim1]);
		t[0] = z1[0], t[1] = z1[1];
		i2 = *n - k;
		cscal_(&i2, &t, &a[k + 1 + k * a_dim1], &c1);

//           row elimination with column indexing 

		i2 = *n;
		for (j = kp1; j <= i2; ++j) {
		    i3 = l + j * a_dim1;
		    t[0] = a[0][i3], t[1] = a[1][i3];
		    if (l != k) {
			i3 = l + j * a_dim1;
			i4 = k + j * a_dim1;
			a[0][i3] = a[0][i4], a[1][i3] = a[1][i4];
			i3 = k + j * a_dim1;
			a[0][i3] = t[0], a[1][i3] = t[1];
		    }
		    i3 = *n - k;
		    caxpy_(&i3, &t, &a[k + 1 + k * a_dim1], &c1, &a[k + 1
			    + j * a_dim1], &c1);
// L30: 
		}
	    } else {
		*info = k;
	    }
// L60: 
	}
    }
    ipvt[*n] = *n;
    i1 = *n + *n * a_dim1;
    if ((d1 = a[0][i1], Math.abs(d1)) + (d2 = d_imag(&a[*n + *n * a_dim1]),
	    Math.abs(d2)) == 0.f) {
	*info = *n;
    }
    return 0;
} // cgefa_

//  
// Title: CSCAL (LINPACK ROUTINE) 
// Author: jack dongarra 
// Date: linpack,  3/11/78. 
// Description: scales a vector by a constant. 
//  

int cscal_(int *n, double[] *ca, double[] *cx,
	int *incx)
{
    // System generated locals 
    int i1, i2, i3, i4;
    double[] z1;

    // Local variables 
    int i, nincx;

    // Parameter adjustments 
    --cx;

    // Function Body 
    if (*n <= 0) {
	return 0;
    }
    if (incx != 1) {

//        code for increment not equal to 1 

    nincx = *n * *incx;
    i1 = nincx;
    i2 = *incx;
    for (i = 1; i2 < 0 ? i >= i1 : i <= i1; i += i2) {
	i3 = i;
	i4 = i;
	z1[0] = ca[0] * cx[0][i4] - ca[1] * cx[1][i4], z1[1] = ca[0] * cx[1][
		i4] + ca[1] * cx[0][i4];
	cx[0][i3] = z1[0], cx[1][i3] = z1[1];
// L10: 
    }
    return 0;
    }

//        code for increment equal to 1 

    i2 = *n;
    for (i = 1; i <= i2; ++i) {
	i1 = i;
	i3 = i;
	z1[0] = ca[0] * cx[0][i3] - ca[1] * cx[1][i3], z1[1] = ca[0] * cx[1][
		i3] + ca[1] * cx[0][i3];
	cx[0][i1] = z1[0], cx[1][i1] = z1[1];
// L30: 
    }
    return 0;
} // cscal_

//  
// Title: ICAMAX (LINPACK ROUTINE) 
// Author: jack dongarra 
// Date: linpack, 3/11/78 
// Description: 
//     finds the index of element having max. absolute value. 
//  

int icamax_(int *n, double[] *cx, int *incx)
{
    // System generated locals 
    int ret_val, i1, i2;
    double d1, d2;

    // Local variables 
    int i, ix;
    double smax;

// statement function 

    // Parameter adjustments 
    --cx;

    // Function Body 
    ret_val = 0;
    if (*n < 1) {
	return ret_val;
    }
    ret_val = 1;
    if (*n == 1) {
	return ret_val;
    }
    if (*incx != 1) {

//        code for increment not equal to 1 

	ix = 1;
	smax = (d1 = cx[0][1], Math.abs(d1)) + (d2 = d_imag(&cx[1]), Math.abs(d2)
		);
	ix += *incx;
	i1 = *n;
	for (i = 2; i <= i1; ++i) {
	    i2 = ix;
	    if ((d1 = cx[0][i2], Math.abs(d1)) + (d2 = d_imag(&cx[ix]), Math.abs(
		    d2)) > smax) {
		ret_val = i;
		i2 = ix;
		smax = (d1 = cx[0][i2], Math.abs(d1)) + (d2 = d_imag(&cx[ix]
			), Math.abs(d2));
		ix += *incx;
	    }
// L10: 
	}
	return ret_val;

//        code for increment equal to 1 

    }
    smax = (d1 = cx[0][1], Math.abs(d1)) + (d2 = d_imag(&cx[1]), Math.abs(d2));
    i1 = *n;
    for (i = 2; i <= i1; ++i) {
	i2 = i;
	if ((d1 = cx[0][i2], Math.abs(d1)) + (d2 = d_imag(&cx[i]), Math.abs(
		d2)) > smax) {
	    ret_val = i;
	    i2 = i;
	    smax = (d1 = cx[0][i2], Math.abs(d1)) + (d2 = d_imag(&cx[i]),
		    Math.abs(d2));
	}
// L30: 
    }
    return ret_val;
} // icamax_


//  
// Title: CNTARG 
// Author: MMJT 
// Date: 24 Feb 1990 
// Description: Counts the number of arguments in the character 
// string 'line', and returns the answer in CNTARG. Legal separators 
// are, blanks, tabs, and commas. 

//      ARGUMENTS: 
//            line  -  Line of characters to be parsed. (input). 

//      CNTARG returns the number of int arguments in the line. 
//      Returns -1 if an error occurred. 
//  

/*int cntarg_(char *line, ftnlen line_len)
{
    // System generated locals 
    int ret_val, i1;

    // Local variables 
    int i, j;
    boolean in_arg;
    int arg_cnt, lin_len;

    in_arg = false;
    arg_cnt = 0;
    ret_val = -1;

    lin_len = i_len(line, line_len);
    i1 = lin_len;
    for (i = 0; i < i1; i++) {
	j = *(unsigned char *)&line[i - 1];
	if (j == 9 || j == 32 || j == 44) {
	    in_arg = false;
	} else {
	    if (! in_arg) {
		in_arg = true;
		++arg_cnt;
	    }
	}
// L10: 
    }

// There cannot be more than (lin_len+1)/2 arguments 
    if (arg_cnt <= (lin_len + 1) / 2) {
	ret_val = arg_cnt;
    }

    return ret_val;
} // cntarg_ */


//  
// Title: DETUN 
// Author: MMJT 
// Date: 27 Jan 1989 
// Description: This subroutine detunes the sharp peaks so that 
// they can be integrated. In effect, it modifies the stacking 
// probability matrix such that the determinant will never be 
// exactly zero. 

//      ARGUMENTS: 
//            No input arguments. 

//      COMMON VARIABLES: 
//            uses:    n_layers 

//        modifies:    detune 
//  

void detun()
{
    // System generated locals 
    int i1, i2;

    // Local variables 
    int i, j;

// A value of delta = 0.001 is found to be optimum.
// If preferred, user can specify 'delta' interactively 
// using the following three lines. 
//   30 write(op,400) 'Enter detune parameter' 
//      read(cntrl,*,err=30,end=999) delta 
//      if(CFile) write(op,401) delta 

    for (i = 0; i < n_layers; i++)
	for (j = 0; j < n_layers; j++)
	    detune[j][i] = 1. - Math.abs(delta);

    return;
//  999 stop 'ERROR: Bad delta value. DIFFaX aborted.' 
//  400 format(1x, a) 
//  401 format(1x, g12.5) 
}



//  
// Title: DUMP 
// Author: MWD and MMJT 
// Date: 18 Aug 1988; 15 Mar 1995 
// Description: This subroutine prints out the data read in from 
// the data file. It reassures the user that the data was read in 
// correctly. 

//      ARGUMENTS: 
//            infile  -  The name of the input data file. (input). 
//            ok      -  boolean flag indicating all went well. 
//                                                      (output). 

//      COMMON VARIABLES: 
//            uses:      CFile, GAUSS, LORENZ, NEUTRN, PI2, PS_VGT, 
//                       RAD2DEG, SymGrpNo, X_RAY, a_B, a_name, 
//                       a_number, a_occup, a_pos, a_type, blurring, 
//                       cell_a, cell_b, cell_c, cell_gamma, cntrl 
//                       d_theta, e_sf, FWHM, inf_thick, l_actual 
//                       l_alpha, l_cnt, l_g, l_n_atoms, l_r, l_symmetry 
//                       lambda, l_seq, n_actual, n_atoms, n_layers 
//                       pnt_grp, pv_gamma, pv_u, pv_v, pv_w, r_B11 
//                       r_B12, r_B22, r_B23, r_B31, r_B33, rad_type 
//                       recrsv, rndm, th2_max, th2_min 
//                       tolerance, xplcit 

//        modifies:      no COMMON variables are modified 
//  

/*int dump_(char *infile, boolean *ok, ftnlen infile_len)
{
    // System generated locals 
    int i1, i2;

    // Local variables 
    double atom_cnt[20];
    int i, j, n, i2, num_types, tot_types, print_width;
    double norm;
    char list[80*5];
    int type;
    double cum_atom_cnt[20], scale;
    boolean loopon;
    char dmpfile[31];

    getfnm_(infile, dmpfile, "dmp", ok, infile_len, (ftnlen)31, (ftnlen)3);
    if (! (*ok)) {
//        write(op,200) 'DUMP aborted' 
	return 0;
//        goto 999 
    }
//      if(dp.ne.op) open(unit = dp , file = dmpfile, status = 'new') 

    i2 = 0;
//   99 write(op,200) 'Enter 1 for full atomic position dump' 
//      read(cntrl,*,err=99,end=9999) i2 
//      if(CFile) write(op,'(1x,i3)') i2 
//      write(op,300) 'Writing data dump to file ''', 
//     |               dmpfile(1:LENGTH(dmpfile)),'''. . .' 
// sundry details about layers 
//      write(dp,125) 'Number of layers = ', n_layers 
//      write(dp,125) 'Number of unique layers = ', n_actual 
//      write(dp,125) 'Number of different atom types = ', n_atoms 
// cell dimensions 
//      write(dp,140) 
//     |    ' cell_a,      cell_b,      cell_c,      cell_gamma ', 
//     |      cell_a, cell_b, cell_c, RAD2DEG * cell_gamma 
// in-plane layer widths 
    if (finite_width) {
	if (wa < 1e4) {
//          write(dp,126) 'width along a', Wa 
//          write(dp,128) Wa/cell_a 
	} else {
//          write(dp,127) 'a' 
	}
	if (wb < 1e4) {
//          write(dp,126) 'width along b', Wb 
//          write(dp,128) Wb/cell_b 
	} else {
//          write(dp,127) 'b' 
	}
    } else {
//        write(dp,200) 
//     |    'Layers are to be treated as having infinite lateral width.' 
    }
// radiation type 
    s_copy(list, "X-RAY ", (ftnlen)80, (ftnlen)6);
    s_copy(list + 80, "NEUTRON ", (ftnlen)80, (ftnlen)8);
    s_copy(list + 160, "ELECTRON ", (ftnlen)80, (ftnlen)9);
//      write(dp,100) 'radiation type = ', list(rad_type+1) 
// wavelength 
//      write(dp,170) 'Wavelength lambda = ', lambda 
// symmetry 
//      write(dp,100) 
//     |'Diffraction point group symmetry specified = ',pnt_grp 
//      if(SymGrpNo.eq.UNKNOWN) write(dp,175) 
//     |'Tolerance on intensities in symmetry evaluation = +/-', 
//     |                    tolerance * HUNDRED, ' %' 
// instrumental broadening to simulate 
    s_copy(list, "NONE", (ftnlen)80, (ftnlen)4);
    s_copy(list + 80, "GAUSSIAN", (ftnlen)80, (ftnlen)8);
    s_copy(list + 160, "LORENTZIAN", (ftnlen)80, (ftnlen)10);
    s_copy(list + 240, "PSEUDO-VOIGT", (ftnlen)80, (ftnlen)12);
    i = blurring + 1;
// see if it's a pure Gaussian pseudo-Voigt 
    if (blurring == pv_gss) {
	i = 2;
    }
// see if it's a pure Lorentzian pseudo-Voigt 
    if (blurring == pv_lrn) {
	i = 3;
    }
//      write(dp,100) 
//     |  'Instrumental broadening to be simulated = ', list(i) 
//      if(blurring.eq.GAUSS .or. blurring.eq.LORENZ) write(dp,170) 
//     |'Full width half-maximum = ', FWHM 
    if (blurring == ps_vgt) {
//        write(dp,200) 
//     |'Pseudo-Voigt parameters:   u,       v,       w,     gamma' 
//        write(dp,180) pv_u, pv_v, pv_w, pv_gamma 
    }
    if (blurring == pv_gss) {
//        write(dp,200) 'Gaussian parameters:      u,       v,       w' 
//        write(dp,185) pv_u, pv_v, pv_w 
    }
    if (blurring == pv_lrn) {
//        write(dp,200) 'Lorentzian parameters:    u,       v,       w' 
//        write(dp,185) pv_u, pv_v, pv_w 
    }
// are we to trim out the origin? 
    if (blurring != none) {
	if (trim_origin) {
//          write(dp,200) '  Intensity near origin will be ignored' 
	} else {
//          write(dp,200) '  Intensity near origin will be included' 
	}
    }

// equivalent layers 
//      write(dp,200) ' ' 
    i1 = n_layers;
    for (i = 0; i < i1; i++) {
//        write(dp,110) 'LAYER ',i, 
//     |            ' is equivalent to fundamental LAYER ',l_actual(i) 
//        write(dp,170) '   Existence probability = ', l_g(i) 
// L10: 
    }

    for (j = 1; j <= 20; ++j) {
	cum_atom_cnt[j - 1] = 0.;
// L11: 
    }

// layer structure 
//      write(dp,200) ' ' 
    i1 = n_actual;
    for (i = 0; i < i1; i++) {
//        write(dp,130) 'fundamental LAYER ',i 
	s_copy(list, "NONE ", (ftnlen)80, (ftnlen)5);
	s_copy(list + 80, "CENTROSYMMETRIC ", (ftnlen)80, (ftnlen)16);
//        write(dp,100) 'symmetry = ', list(l_symmetry(i)+1) 
// write out the layer composition 
//        write(dp,200) 'LAYER composition is:' 
	for (j = 1; j <= 20; ++j) {
	    atom_cnt[j - 1] = 0.;
// L14: 
	}
	num_types = 0;
	scale = 1.;
	if (l_symmetry[i - 1] == centro) {
	    scale = 2.;
	}
	i2 = l_n_atoms[i - 1];
	for (j = 0; j < i2; j++) {
	    type = a_type[j + i * 200 - 201];
	    if (type > num_types) {
		num_types = type;
	    }
	    atom_cnt[type - 1] += a_occup[j + i * 200 - 201]
		    * scale;
// L15: 
	}
	if (num_types > tot_types) {
	    tot_types = num_types;
	}
// accumulate weighted atom count 
	for (j = 1; j <= 20; ++j) {
	    cum_atom_cnt[j - 1] += atom_cnt[j - 1] * l_g[i -
		    1];
// L16: 
	}
//        do 17 j = 1, num_types 
//          write(dp,280) atom_l(j), ':', atom_cnt(j) 
//   17   continue 
//        write(dp,200) ' ' 

// do we want all the details about each atom? 
//        if(i2.EQ.1) then 
// yes, go for it. 
//        do 20 j = 1, l_n_atoms(i) 
//            write(dp,200) ' ' 
//          write(dp,120) 'ATOM number', a_number(j,i), ' in layer',i, 
//     |     ' is ''', a_name(j,i),'''' 
//          write(dp,121) 
//          write(dp,122) a_pos(1,j,i), a_pos(2,j,i), a_pos(3,j,i), 
//     |      a_B(j,i), a_occup(j,i) 
//          if(rad_type.eq.X_RAY) then 
//            write(dp,140) 'X-ray scattering factor data Ai, Bi', 
//     |                         (x_sf(n,a_type(j,i)),n=1,9) 
//          else if(rad_type.eq.NEUTRN) then 
//            write(dp,140) 'Neutron scattering factor data', 
//     |                         n_sf(a_type(j,i)) 
//          else if(rad_type.eq.ELECTN) then 
//            write(dp,140)'Electron scattering factor data Ai, Bi and Z', 
//     |               (x_sf(n,a_type(j,i)),n=1,9), e_sf(a_type(j,i)) 
//          else 
//            write(op,200) 'ERROR: Illegal radiation type in DUMP' 
//          endif 
//   20   continue 
//        write(dp,200) ' ' 
//      end if 
// L30: 
    }

// Print out average composition 
    if (! xplcit) {
	norm = 0.;
	i1 = num_types;
	for (j = 1; j <= i1; ++j) {
	    norm += cum_atom_cnt[j - 1];
// L25: 
	}
//        write(dp,200) ' ' 
//        write(dp,200) 'Average crystal composition is:' 
//        do 26 j = 1, num_types 
//          write(dp,281) atom_l(j), ':', cum_atom_cnt(j) / norm 
//   26   continue 
//        write(dp,200) ' ' 
    }

// stacking details 
//      write(dp,200) ' ' 
//      write(dp,200) 'STACKING' 
//      if(recrsv) then 
//        write(dp,210) 'RECURSIVELY' 
//        if(inf_thick) then 
//          write(dp,220) 'INFINITE' 
//        else 
//          write(dp,230) l_cnt 
//        endif 
//      else if(xplcit) then 
    if (xplcit) {
//        if(rndm) then 
//          write(dp,240) 
//        else 
//          write(dp,250) 
//        endif 
//        write(dp,260) 'Sequence for ', l_cnt, ' layers is:' 
	print_width = 25;
	j = 1;
	n = print_width;
	loopon = true;
	while(loopon) {
	    if (n > l_cnt) {
		n = l_cnt;
	    }
//        write(dp,270) (l_seq(i), i = j, n) 
	    if (n < l_cnt) {
		j += print_width;
		n += print_width;
		loopon = true;
	    } else {
		loopon = false;
	    }
	}
    }

// stacking transition data 
//      write(dp,200) ' ' 
//      write(dp,200) ' ' 
//      write(dp,200) 'TRANSITIONS' 
//      write(dp,200) 
//     |      'Layer stacking probabilities and stacking vectors.' 
//      write(dp,200) 
//     |'  i  j |   alpha_ij      Rx_ij        Ry_ij        Rz_ij' 
//      write(dp,200) 
//     |'  -----|-------------------------------------------------' 
//      do 40 i = 1, n_layers 
//        write(dp,200) 
//     |'       |' 
//        do 50 j = 1, n_layers 
//          write(dp,150) i, j, l_alpha(j,i), 
//     |                   l_r(1,j,i), l_r(2,j,i), l_r(3,j,i) 
//   50   continue 
//   40 continue 

//      write(dp,200) ' ' 
//      write(dp,200) 
//     |      'Anisotropic Layer stacking ''uncertainty'' factors.' 
// MMJT: 3/15/95. Ordering of B23 and B31 swapped. B31 -> C13 
//      write(dp,200) 
//     |'  i  j |     C11      C22      C33      C12      C13      C23' 
//      write(dp,200) 
//     |'  -----|-------------------------------------------------------' 
//      do 60 i = 1, n_layers 
//        write(dp,200) 
//     |'       |' 
//        do 70 j = 1, n_layers 
// MMJT: 3/15/95. Ordering of B23 and B31 swapped 
//          write(dp,151) i, j, r_B11(j,i), r_B22(j,i), r_B33(j,i), 
//     |                        r_B12(j,i), r_B31(j,i), r_B23(j,i) 
//   70   continue 
//   60 continue 

//      if(dp.ne.op) close(unit = dp) 
//  999 return 
    return 0;
// 9999 ok = .false. 
//      write(op,200) 'DUMP aborted' 
//      return 
//  100 format(1x, 2a) 
//  110 format(1x, a, i4, a, i4) 
//  120 format(1x, a, i3, a, i2, 3a) 
//  121 format(4x,'x_rel',8x,'y_rel',8x,'z_rel',10x,'DW',10x,'Occ') 
//  122 format(1x, 5g13.5) 
//  125 format(1x, a40, i4) 
//  126 format(1x, 'layer characteristic ', a, ' = ', f9.2, ' Angstroms') 
//  127 format(1x, 'layer characteristic ', a, ' = INFINITY Angstroms') 
//  128 format(1x, '   which is equivalent to ', f9.2, ' unit cells') 
//  130 format(1x, a, i4) 
//  140 format(1x, a, / 5g13.5, / 4g13.5, i4) 
//  150 format(1x, 2i3, ' |', 4(1x, g12.5)) 
//  151 format(1x, 2i3, ' |', 6(1x, f8.3)) 
//  170 format(1x, a, g12.5) 
//  175 format(1x, a, g12.5, a) 
//  180 format(21x, 4(2x, f7.3)) 
//  185 format(21x, 3(2x, f7.3)) 
//  200 format(1x, a) 
//  210 format(1x, 'Stacking is to be treated ', a, ' by DIFFaX.') 
//  220 format(1x, 'Number of layers along the fault direction is ', a) 
//  230 format(1x, 'There are ',i5 ,' layers along the fault direction') 
//  240 format(1x, 'Sequencing is defined RANDOMLY by DIFFaX.') 
//  250 format(1x, 'Sequencing is defined EXPLICITLY by the user.') 
//  260 format(1x, a, i4, a) 
//  270 format(1x, 30i3) 
//  280 format(23x, 2a, f7.2) 
//  281 format(23x, 2a, f7.4) 
//  300 format(1x, 3a) 
} // dump_ */



//  
// Title: EQUALB 
// Author: MMJT 
// Date: 13 Mar 1990; 21 July 1997; 3 July 2005 
// Description:  This routine determines if all of the stacking 
// uncertainty parameters are identical. There are six arrays to be 
// tested, namely r_B11, r_B22, r_B33, r_B12, r_B23 and r_B31. These are 
// passed one at a time from OPTIMZ as r_B. The average value of the 
// r_B parameters is returned in a_B. 
// The test fails if the user changes the sign, but not the amplitude, of 
// some of the B11, B22 or B33. EQUALB returns false, and the calculation 
// is then done the hard way. 

//      ARGUMENTS: 
//            r_B  -  Array of stacking uncertainty parameters. (input). 
//            av_B  -  Average of r_B. (output). 

//      COMMON VARIABLES: 
//            uses the array 'there'. n_layers 
//  

boolean equalb_(double *r_b, double *av_b)
{
    // System generated locals 
    int i1, i2;
    double d1;
    boolean ret_val;

    // Local variables 
    int i, j, m;
    double error;

    // Parameter adjustments 
    r_b -= 21;

    // Function Body 
    *av_b = 0.;
    m = 0;
    i1 = n_layers;
    for (i = 0; i < i1; i++) {
	i2 = n_layers;
	for (j = 0; j < i2; j++) {
// Examine only those transitions that actually occur 
	    if (there[j + i]) {
		++m;
		*av_b += r_b[j + i * 20];
	    }
// L20: 
	}
// L10: 
    }

// Take average 
    if (m != 0) {
	*av_b /= (double) m;
    }

    error = 0.;
// find absolute deviation of coefficients 
    i1 = n_layers;
    for (i = 0; i < i1; i++) {
	i2 = n_layers;
	for (j = 0; j < i2; j++) {
	    if (there[j + i]) {
		error += (d1 = r_b[j + i * 20] - *av_b, Math.abs(d1));
	    }
// L40: 
	}
// L30: 
    }

    ret_val = Math.abs(error) <= (d1 = *av_b * .001, Math.abs(d1));

    return ret_val;
} // equalb_ 



//  
// Title: FNDCTL 
// Author: MMJT 
// Date: 6 June 1989 
// Description: Checks whether or not there is a control file, and 
// sets the i/o unit specifier 'cntrl' to 99. 

//      ARGUMENTS: 
//           No arguments are used. All data is in 'COMMON'. 

//      COMMON VARIABLES: 
//            uses:   cfname, CFile 

//        modifies:   cntrl 

//      FNDCTL returns boolean .true. if a control file was found in the 
//      current directory. 
//  

/*boolean fndctl(void)
{
    // System generated locals 
    boolean ret_val;

    // Local variables 

// See if control file is present 
//      inquire(file = cfname, exist = CFile) 
// get data from ControlFile 
//      if(CFile) then 
//        cntrl = 99 
//        open(unit = cntrl, file = cfname, status = 'old', 
//     |      err = 999, iostat = io_err) 
//      else 
// get data from user entries to the screen 
//        cntrl = ip 
//      endif 
    ret_val = true;

    return ret_val;
//  999 write(op,100) 
//     |'A control file named ''', cfname(1:LENGTH(cfname)), '''' 
//      write(op,101) 'exists, but there were problems opening it.' 
//      write(op,102) 'iostat = ', io_err 
//      FNDCTL = .false. 
//      return 
//  100 format(1x, 3a) 
//  101 format(1x, a) 
//  102 format(1x, a, i5) 
} // fndctl */



//  
// Title: GETFIL 
// Author: MMJT 
// Date: 5 June 1989 
// Description: Checks for the presence of the structure data file and 
// the structure factor data file in the current directory. 

//      ARGUMENTS: 
//            fname   -  The name of the input data file. (output). 
//            ok      -  boolean flag indicating all went well. 
//                                                      (output). 
//            ending  -  returns boolean .true. if 'end' was read 
//                       from the control file. (output). 

//      COMMON VARIABLES: 
//            uses:       cntrl, sfname, CFile 

//        modifies:       no COMMON variables are modified 
//  

/* int getfil(char *fname, boolean *ok, boolean *ending,
	ftnlen fname_len)
{
    // Local variables 
    int i;
    char list[80*1];
    char tmp_nam[31];
    int unit_no;

    unit_no = cntrl;
// get file name from ControlFile 
//      write(op,104) 'Enter name of structure file.' 
// use GETLNE to strip off any leading blanks etc... 
//      call GETLNE(unit_no, line, *993) 
//      write(fname,'(a)') line(1:len(fname)) 
//      if(CFile) write(op,104) fname 
// check that the string does not say 'end'. First convert to upper case 
// we must be sure that the string 'end' is not part of a legitimate 
// file name. Use CHOICE for this. 
    s_copy(tmp_nam, fname, (ftnlen)31, fname_len);
    touppr_(fname, fname_len);
    s_copy(list, "END ", (ftnlen)80, (ftnlen)4);
    i = choice_(fname, list, &c1, fname_len, (ftnlen)80);
    if (i == 1) {
	*ending = true;
    } else {
	s_copy(fname, tmp_nam, fname_len, (ftnlen)31);

// Open structure data file. 
//      write(op,100) 'Looking for structure data file ''', 
//     |            fname(1:LENGTH(fname)),'''' 
//      open(unit = df, file = fname, status = 'old', 
//     |            err = 991, iostat = io_err) 
//      write(op,100) 'Opening structure file ''', 
//     |       fname(1:LENGTH(fname)),'''' 

// Open scattering factor data file. 
//      write(op,100) 
//     | 'Looking for scattering factor data file ''', 
//     |            sfname(1:LENGTH(sfname)),'''' 
//      open(unit = sf, file = sfname, status = 'old', 
//     |            err = 992, iostat = io_err) 
//      write(op,100) 'Opening scattering factor data file ''', 
//     |       sfname(1:LENGTH(sfname)),'''' 

//      goto 999 
//  991 write(op,100) 
//     |'Problems opening structure data file ''', 
//     |            fname(1:LENGTH(fname)),'''' 
//      write(op,103) 'IOSTAT = ',io_err 
//      ok = .false. 
//      goto 999 
//  992 write(op,100) 
//     |'Problems opening scattering factor file ''', 
//     |            sfname(1:LENGTH(sfname)),'''' 
//      write(op,103) 'IOSTAT = ',io_err 
//      ok = .false. 
//      goto 999 
//  993 write(op,104) 'ERROR reading filename in GETFIL' 
//      ok = .false. 
    }
    return 0;
//  100 format(1x, 3a) 
//  103 format(1x, a, i5) 
//  104 format(1x, a) 
} // getfil */



//  
// Title: GETFNM 
// Author: MMJT 
// Date: 13 Oct 1988 
// Description: This subroutine makes sure that we do not write over 
// an existing file, and creates a suitable filename (name2). name2 
// equals 'name1.append' append is supplied by the calling routine. 
// If 'name1.append' is the name of an existing file, then append is 
// set to 'append#' where #=1,2. 
// CLIP is the maximum length of the appendage. Thus if append 
// = 'fred', and CLIP is 3, then the appendages become 'fre', 
// 'fr1', 'fr2', ...'f10', 'f11' ... up to 'f99'. 
// If CLIP is greater than 10, there is no restriction. 
// NOTE: It is up to the user to ensure that filename lengths are 
// compatible with the operating system that DIFFaX is being run on. 
// 'CLIP' and 'MAX_NAM' are set in 'DIFFaXj.par' 

//      ARGUMENTS: 
//            name1   -  The name of the input data file. (input). 
//            name2   -  A derivative filename. (output). 
//            append  -  A token to be appended to name2. (input). 
//            ok      -  boolean flag signalling all went well. 
//                                                      (output). 
//  

/* int getfnm_(char *name1, char *name2, char *append, boolean *
	ok, ftnlen name1_len, ftnlen name2_len, ftnlen append_len)
{
    // Local variables 
    int i;
    char appnd2[31];
    int applen;
    int myclip;
    boolean fexist;

    *ok = true;
    fexist = true;
    s_copy(appnd2, append, (ftnlen)31, append_len);
    applen = length_(append, append_len);
    myclip = 14;
    if (myclip <= 0) {
//        write(op,302) 'Illegal CLIP value ', CLIP 
//        write(op,300) 'Default CLIP value of 3 is being assumed' 
	myclip = 3;
    }

    i = 0;
//   10 call NAMER(name1,name2,appnd2) 
//      inquire(file = name2, exist = fexist) 
//      if(fexist) then 
//        i = i + 1 
//        if(i.lt.10) then 
//          if(myclip.gt.10) then 
//            write(appnd2(applen+1:applen+1),200) i 
//          else 
//            write(appnd2(myclip:myclip),200) i 
//          endif 
//        else if(i.ge.10 .and. i.lt.100) then 
//          if(myclip.gt.10) then 
//            write(appnd2(applen+1:applen+2),201) i 
//          else if(myclip.gt.1) then 
//            write(appnd2(myclip-1:myclip),201) i 
//          else 
//            ok = .false. 
//            goto 999 
//          endif 
//        else if(i.ge.100 .and. i.lt.1000) then 
//          if(myclip.gt.10) then 
//            write(appnd2(applen+1:applen+3),202) i 
//          else if(myclip.gt.2) then 
//            write(appnd2(myclip-2:myclip),202) i 
//          else 
//            ok = .false. 
//            goto 999 
//          endif 
//        else 
//          ok = .false. 
//          goto 999 
//        endif 
//        goto 10 
//      endif 

    return 0;
//  999 write(op,300) 'No new files can be created. . .' 
//      write(op,300) '      . . . some old files should be deleted.' 
//      write(op,301) 
//     |      'Last file created was ''',name2(1:LENGTH(name2)),'''' 
//      return 
//  200 format(i1) 
//  201 format(i2) 
//  202 format(i3) 
//  300 format(1x, a) 
//  301 format(1x, 3a) 
//  302 format(1x, a, i5) 
} // getfnm_ */


//  
// Title: GETLAY 
// Author: MMJT 
// Date: 4 Oct 1989 
// Description: This function generates a random sequence of layers 
// weighted by the stacking probabilities. Needed only when the 
// 'EXPLICIT' and 'RANDOM' options were specified in the 'STACKING' 
// description. 

//      ARGUMENTS: 
//           No arguments are used. All data is in 'COMMON'. 

//      COMMON VARIABLES: 
//            uses:      l_cnt, l_g, n_layers, l_seq, l_alpha 

//        modifies:      l_seq 

//      GETLAY returns boolean .true. if all went well. 
//  

boolean getlay()
{
    // System generated locals 
    boolean ret_val;

    // Local variables 
    int i, j;
    double x, sum;
    int idum;
    boolean okay;

    ret_val = false;
    okay = true;

// initialize random numbers in RAN3 
    idum = -1;

//      write(op,100) 
//     |      'Generating a random sequence of ', l_cnt, ' layers.' 

// Get the first layer. Even though some stacking transition 
// probabilities to and from the layer are non-zero, the layer itself 
// may not actually exist anywhere in the crystal! Must use the 
// existence probabilities, l_g. 
    x = ran3_(&idum);
    if (x == 1.) {
	x += -1e-7;
    }
    sum = 0.;
    i = 1;
    while(i <= n_layers && x > sum) {
	sum += l_g[i - 1];
	if (x > sum) {
	    ++i;
//        if(i.gt.n_layers) then 
// if all goes well, we should not reach here. 
//        messge = 'GETLAY could not generate the first layer.$' 
//        goto 999 
//         return 
//        end if 
	}
    }
    l_seq[0] = i;

// Now generate the remaining l_cnt-1 layers 
    j = 2;
    while(j <= l_cnt) {
	x = ran3_(&idum);
	if (x == 1.) {
	    x += -1e-7;
	}
	sum = 0.;
	i = 1;
	while(i <= n_layers && x > sum) {
	    sum += l_alpha[i + l_seq[j - 2]]
		    ;
	    if (x > sum) {
		++i;
//        if(i.gt.n_layers) then 
// if all goes well, we should not reach here. 
//        write(messge,101) 'GETLAY could not generate layer ', j, '.$' 
//        goto 999 
//        return 
//        end if 
	    }
	}
	l_seq[j - 1] = i;
	++j;
    }

    ret_val = true;
    return ret_val;
//  999 write(op,102) messge(1:index(messge,'$')-1) 
//      return 
//  100 format(1x, a, i4, a) 
//  101 format(a, i4, a) 
//  102 format(1x, 'ERROR: ', a) 
} // getlay_ 



//  
// Title: GETLNE 
// Author: MWD  & MMJT 
// Date: MWD: Original version 18 Aug 1988; MMJT: This version 9 Apr 1992 
// Description: This function returns a non-blank, left-justified line, 
// stripped of any comments, from unit 'unit_no'. Any error generates 
// a return to the passed linenumber, presumably an error-handling 
// routine in the calling procedure. Multiple comments, and nested 
// braces on one line can be handled. 

//      ARGUMENTS: 
//            unit_no  -  Logical unit number that data is to 
//                        be read from. (input). 
//            line     -  Line of data read as a character string. 
//                                                          (output). 
//            *        -  The label of a line in the calling routine 
//                        to return to if an error occurs. (input). 
//  

//      subroutine GETLNE(unit_no, line, *) 
//      include 'DIFFaXj.par' 
//      int*4 unit_no 
//      character*(*) line 

//      character*1 tab, blank 
// nominal line length is 200. Leave generous room for overflow. 
//      character*250 rawline 
//      int*4 i, j, n, offset, lin_len 
//      int*4 l_br, r_br, first_left, last_right 

//      tab = char(9) 
//      blank = char(32) 
//      lin_len = len(line) 

//    1 read(unit_no, '(a)', err = 200, end = 200) rawline 
//      line = rawline 

// First check if user has gone beyond lin_len characters 
//      do 2 i = len(rawline), lin_len + 1, -1 
//        if(rawline(i:i).ne.blank) goto 998 
//    2 continue 

// strip out all tabs. Replace with blanks. 
//    3 n = index(line,tab) 
//      if(n.eq.0) goto 4 
//        write(line(n:n),'(a)') blank 
//        goto 3 
//    4 continue 

// Search for matching braces. Keep looping until all braces are found 
//   10   first_left = 0 
//        last_right = 0 
//        l_br = 0 
//        r_br = 0 
//        i = 0 
//   20   i = i + 1 
//          if(i.gt.lin_len) goto 30 
//          if(line(i:i).eq.'{') then 
//            if(first_left.eq.0) first_left = i 
//            l_br = l_br + 1 
//          else if(line(i:i).eq.'}') then 
//            last_right = i 
//            r_br = r_br + 1 
//          endif 
//          if(r_br.gt.l_br) goto 999 
//          if(l_br.eq.0 .or. l_br.gt.r_br) goto 20 
//   30   continue 
//        if(r_br.ne.l_br) goto 999 

// if no more comments, then left justify the line 
//        if(l_br.eq.0) goto 50 
// else, remove comments, and shunt data to the left 
//        offset = last_right - first_left 
//        i = first_left 
//        do 60 j = last_right + 1, lin_len 
//          line(i:i) = line(j:j) 
//          i = i + 1 
//   60   continue 
// blank out the end of the modified string 
//        do 70 i = lin_len-offset, lin_len 
//          line(i:i) = blank 
//   70   continue 
// repeat until no more braces are found 
//      goto 10 

// left adjust line 
//   50 do 80 i = 1, lin_len 
//        if((line(i:i).ne.blank)) then 
//          do 90 j = 1, lin_len-i+1 
//            line(j:j) = line(i+j-1:i+j-1) 
//   90     continue 
//          do 100 j = lin_len-i+2,lin_len 
//            line(j:j) = blank 
//  100     continue 
//          goto 110 
//        endif 
//   80 continue 
// If, after all this, the line is empty, repeat with next next line. 
//  110 if(line(1:1).eq.blank) goto 1 

//      return 

//  998 write(op,401) 'ERROR: Data line exceeds ',lin_len, ' characters.' 
//      return 1 
//  999 write(op,400) 'ERROR: Unbalanced braces in data file.' 
//  200 return 1 

//  400 format(1x, a) 
//  401 format(1x, a, i3, a) 
//      end 

//  
// Title: GETSAD 
// Author: MMJT 
// Date: 29 Oct 1989; 16th April 1999 
// Description: This subroutine generates the selected area diffraction 
// pattern (SADP). GETSAD returns .true. if all went well. The pattern 
// is stored in the linear array 'spec'. 'spec' is read by WRTSAD 
// to write the diffraction pattern image to disk as a binary file. 

//      ARGUMENTS: 
//            FN      -  Function name passed by reference. The 
//                       choice is between GLQ16 (non-adaptive 
//                       Gauss-Legendre integration), and AGLQ16 
//                       (adaptive Gauss-Legendre integration). (input). 
//            view    -  Choice of beam direction (input). 
//                              1  =  normal to the plane k = 0 
//                              2  =  normal to the plane h = 0 
//                              3  =  normal to the plane h = k 
//                              4  =  normal to the plane h = -k 
//            l_upper -  Upper limit of l. (input). 
//            hk_lim  -  Upper limit of h (or k). (output). 
//            infile  -  The name of the input data file. (input). 
//            ok      -  boolean flag indicating all went well. 
//                                                      (output). 

//      COMMON VARIABLES: 
//            uses:      a0, b0, c0, d0, lambda, has_l_mirror, sadblock, 
//                       spec, loglin, brightness, X_RAY, rad_type 

//        modifies:      scaleint 
//  

int getsad_(D_fp fn, int *view, double *l_upper,
	int *hk_lim, char *infile, boolean *ok, ftnlen infile_len)
{
    // System generated locals 
    int i1, i2;
    double d1, d2, d3, d4, d5;

    // Local variables 
    int h, i, j, k;
    double l;
    int n;
    double x;
    int info_step;
    double q2, dl;
    int cnt;
    int info;
    double high1, high2;
    int origin;
    double s_value, l_lower;

// statement functions 
// S is the value of 1/d**2 at hkl 
// ANGLE is the Bragg angle (in radians) of the h,k,l plane 
// W4 is the X-ray polarization factor 

// Computing 2nd power 
    d1 = lambda;
    q2 = 4. / (d1 * d1);

// check angles are meaningful 
    s_value = c0 * c0 * a0 + c0 * c0 * b0 + *
	    l_upper * *l_upper * c0 + c0 * c0 * d0;
    if (s_value <= 0.) {
//        write(op,101) 
//     |     'ERROR: Illegal value in GETSAD: 1/d**2 = ',S_value 
	*ok = false;
//        goto 990 
	return 0;
    }
    if (s_value > q2) {
//        write(op,100) 
	*l_upper = 2. / (lambda * Math.sqrt(c0)) - 1e-10;
//        write(op,101) 'Upper bound reduced to ', l_upper 
	s_value = c0 * c0 * a0 + c0 * c0 * b0 + *
		l_upper * *l_upper * c0 + c0 * c0 *
		d0;
    }

// Set h and k limit 
    if (*view == 1) {
	*hk_lim = (int) (*l_upper * Math.sqrt(c0 / a0));
    } else if (*view == 2) {
	*hk_lim = (int) (*l_upper * Math.sqrt(c0 / b0));
    } else if (*view == 3) {
	*hk_lim = (int) (*l_upper * Math.sqrt(c0 / (a0 +
		b0 + d0)));
    } else if (*view == 4) {
	*hk_lim = (int) (*l_upper * Math.sqrt(c0 / (a0 +
		b0 - d0)));
    }

// Get l increment 
    dl = *l_upper / 128.;
// reset l_upper so that our integration window, of width dl, 
// straddles l = 0. 
    *l_upper -= dl * .5;

// Get diffraction pattern. 
// First check scan limits, and make sure we are not going to overflow 
// the array 'spec' which will be used to hold the scan information 
// prior to writing to file in WRTSAD. 
    if (has_l_mirror) {
	l_lower = 0. - dl * .5;
	sadblock = 128;
	if (*hk_lim + 1 << 7 > 20001) {
	    *hk_lim = 155;
	}
    } else {
	l_lower = -(*l_upper);
	sadblock = 256;
	if (*hk_lim + 1 << 8 > 20001) {
	    *hk_lim = 77;
	}
    }
    d1 = *l_upper / (dl * 20.);
    info_step = i_dnnt(&d1);
    if (info_step <= 0) {
	info_step = 1;
    }

    cnt = 0;
    i1 = *hk_lim;
    for (i = 0; i <= i1; ++i) {
// set h and k 
	if (*view == 1) {
	    h = i;
	    k = 0;
	} else if (*view == 2) {
	    h = 0;
	    k = i;
	} else if (*view == 3) {
	    h = i;
	    k = i;
	} else if (*view == 4) {
	    h = i;
	    k = -i;
	} else {
//          write(op,105) 'ERROR: Illegal view value in GETSAD', view 
//          goto 990 
	    return 0;
	}

// write progress to screen 
//        write(op,102) h, k, infile(1:LENGTH(infile)) 

	xyphse_(&h, &k);
	pre_mat(&h, &k);

// The following piece of monkey business is here because if there is 
// no mirror then the l-loop only calculates SADSIZE-1 values. If there 
// is a mirror then the l-loop makes SADSIZE/2 calculations. We wish 
// the final binary data file to be in a square array SADSIZE x SADSIZE, 
// so we pad out the first value with a zero. 

	if (! has_l_mirror) {
	    ++cnt;
	    spec[cnt - 1] = 0.;
	}

	info = 0;
	d1 = *l_upper - 1e-10;
	d2 = dl;
	for (l = l_lower; d2 < 0 ? l >= d1 : l <= d1; l += d2) {
// If we are at the origin, don't integrate, we'll probably overflow. 
	    if (i == 0 && (d3 = l + dl, Math.abs(d3)) <= dl + 1e-10) {
		x = 0.;
		origin = cnt + 1;
	    } else /* if(complicated condition) */ {
		d3 = l + dl;
		if (h * h * a0 + k * k * b0 + d3 *
			d3 * c0 + h * k * d0 > q2) {
		    x = 0.;
		} else {
		    d3 = l + dl;
		    x = (*fn)(&h, &k, &l, &d3, ok);
		    if (! (*ok)) {
			return 0;
		    }
//            goto 999 
		    if (rad_type == x_ray) {
			d3 = l + dl * .5;
			d4 = Math.asin(.5 * lambda * Math.sqrt(h * h *
				a0 + k * k * b0 + d3 *
				d3 * c0 + h * k * d0));
// Computing 2nd power 
			d5 = Math.cos(2. * d4);
			x *= .5 * (1. + d5 * d5);
		    }
		}
	    }
	    ++cnt;
// make sure we do not overflow 
	    if (cnt > 20001) {
		*ok = false;
		return 0;
//          goto 998 
	    }
	    spec[cnt - 1] = x;
	    if (info % info_step == 0) {
		if (loglin == 0) {
		    if (x + 1. > 0.) {
// write out Math.log(1+x), since x may get to be quite small 
//                write(op,103) 'l = ',l,' Math.log(intensity) = ',Math.log(ONE+x) 
		    } else {
//                write(op,103) 'l = ', l, ' Math.log(intensity) = ', ZERO 
		    }
		} else {
//              write(op,103) 'l = ', l, ' intensity = ', x 
		}
	    }
	    ++info;
// L10: 
	}
    }
// check cnt 
    if (cnt < 2) {
	*ok = false;
	return 0;
//      goto 980 
    }

// patch the intensity at the origin to put a well-defined peak there 
    if (has_l_mirror) {
// origin = 1, make origin slightly bigger than proceeding value 
	spec[origin - 1] = spec[origin] * 1.0001;
    } else {
// make it slightly larger than the biggest adjacent value 
// Computing MAX 
	d2 = spec[origin - 2], d1 = spec[origin];
	spec[origin - 1] = max(d2,d1) * 1.0001;
    }

// we need to find the second highest peak intensity so as to scale the 
// data. The highest intensity should be at the origin, which we discard. 
    high1 = 0.;
    high2 = 0.;
    i1 = *hk_lim;
    for (i = 0; i <= i1; ++i) {
	i2 = sadblock - 1;
	for (j = 0; j < i2; j++) {
	    n = i * sadblock + j;
// check scaling type. If logarithmic, make sure values are always +ve 
	    if (loglin == 0) {
		if (spec[n - 1] + 1. > 0.) {
		    spec[n - 1] = Math.log(spec[n - 1] + 1.);
		} else {
		    spec[n - 1] = 0.;
		}
	    }
// check if origin is the first value. If so, preset high value. 
	    if (n == 1 && origin == 1) {
		high1 = spec[origin - 1];
	    } else {
		x = spec[n - 1];
		if (j == 1) {
		    if (x > spec[n]) {
			if (x > high1) {
			    high2 = high1;
			    high1 = x;
			} else if (x > high2) {
			    high2 = x;
			}
		    }
		} else {
		    if (x > spec[n - 2] && x > spec[n]) {
			if (x > high1) {
			    high2 = high1;
			    high1 = x;
			} else if (x > high2) {
			    high2 = x;
			}
		    }
		}
	    }
// L30: 
	}
    }

    if (loglin != 0 && high2 <= 0.) {
//        write(op,101) 
//     |  'ERROR in intensity scaling in GETSAD. ''scale factor'' = ', 
//     |                                                      high2 
	*ok = false;
	return 0;
//        goto 990 
    }

    if (loglin == 0 && high1 <= 0.) {
//        write(op,101) 
//     |  'ERROR in intensity scaling in GETSAD. ''scale factor'' = ', 
//     |                                                      high1 
	*ok = false;
	return 0;
//        goto 990 
    }

// If logarithmic, scale to the brightest peak 
// If linear, scale to the 2nd brightest peak 
// Note: Intensity scaling can be modified by the user-defined 
// brightness value 
    if (loglin == 0) {
	scaleint = brightness * (maxsad - 1.) /
		high1;
    } else {
	scaleint = brightness * (maxsad - 1.) /
		high2;
    }

    return 0;
//  980 write(op,105) 
//     |     'Error in GETSAD: loop counter is too small. cnt = ', cnt 
//      ok = .false. 
//      return 
//  998 write(op,104) 'ERROR in GETSAD: spectrum array overflow at h = ', 
//     |                                    h,', k = ',k,', l = ',l 
//      ok = .false. 
//      return 
//  999 write(op,104) 'ERROR in GETSAD at h = ',h,', k = ',k,', l = ',l 
//      return 
//  100 format(1x, 'Upper bound exceeds 180 degrees!') 
//  101 format(1x, a, g12.5) 
//  102 format(1x, 'h = ', i3, ' k = ', i3, 10x, '''', a, '''') 
//  103 format(1x, a, f10.5, a, g12.5) 
//  104 format(1x, 2(a, i3), a, f10.5) 
//  105 format(1x, a, i3) 
} // getsad_ 



//  
// Title: GETSPC 
// Authors: MWD and MMJT 
// Date: 17 Mar 1989; Last tweaked on 7 Mar 1995 
// Description: This subroutine calculates the spectrum. 

//      ARGUMENTS: 
//            FN      -  Function name passed by reference. The 
//                       choice is between GLQ16 (non-adaptive 
//                       Gauss-Legendre), and AGLQ16 
//                       (adaptive Gauss-Legendre). (input). 
//            infile  -  The name of the input data file. (input). 

//      COMMON VARIABLES: 
//            uses:      CFile, ELECTN, NEUTRN, SymGrpNo, X_RAY, a0 
//                       any_sharp, b0, bnds_wt, c0, cntrl, d0, d_theta 
//                       full_brd, lambda, mltplcty, rad_type, rot_only, 
//                       spec, th2_max, th2_min, theta1, theta2 

//        modifies:      full_shrp 

//      GETSPC returns boolean .true. if all went well. 
//  

boolean getspc_(D_fp fn, char *infile, ftnlen infile_len)
{
    // System generated locals 
    int i1, i2;
    double d1, d2, d3, d4, d5;
    boolean ret_val;

    // Local variables 
    int max_indx;
    boolean on_bndry;
    double[] f = new double[2][20];
    int h, i, k;
    double l;
    int m;
    double q, x, l1, l0, l00;
    boolean ok;
    double d_l, tmp, tmp2, tmp3, fact;
    boolean shrp;
    double hk_th, l_max, theta;
    double min_th;
    boolean l_axis;
    double max_th;
    boolean loopon;
    int h_lower, h_upper, k_lower, k_upper;

// statement functions 
// S is the value of 1/d**2 at hkl 
// LL is the maximum allowable l value for a given h,k and theta 
// ANGLE is the Bragg angle (in radians) of the h,k,l plane 
// HKANGL is the angle between the vector (h_val,k_val,0) and (1,0,0) 
// These factors are for powder diffraction patterns 
// W1 is the polarization factor for weighting x-ray intensities 
// it also incorporates the Lorentz factor 
// W2 is the neutron weighting factor--the Lorentz factor 
// W3 is the electron weighting factor--the Lorentz factor 

    ret_val = false;
    ok = true;

// Make sure we are within bounds. If not, adjust. 
    min_th = th2_min * .5;
    max_th = th2_max * .5;
    max_indx = (int) ((max_th - min_th) / d_theta + 1);
    if (max_indx > 20001) {
	d_theta = (max_th - min_th) / 20000;
	max_indx = (int) ((max_th - min_th) / d_theta +
		1);
//        write(op,300) '' 
//        write(op,250) 'd_theta is too small and has been adjusted to ', 
//     |                   TWO*d_theta*RAD2DEG 
    }

// 1234 write(op,300) 
//     | 'Enter 1 for adaptive quadrature over all l values' 
//      write(op,300) 'on rows with "sharp" spots' 
//      read(cntrl,*,err=1234) full_shrp 
//      if(CFile) write(op,400) full_shrp 
// zero out spectra 
    for (i = 1; i <= 20001; ++i) {
	spec[i - 1] = 0.;
// L10: 
    }
// See if there is a chance of any sharp peaks. 
// If so, an appropriate step along l is found, and any_sharp is .true. 
    d_l = l_step(&ok);
    if (! ok) {
	return ret_val;
    }
//      goto 999 
// If no sharp peaks were unambiguously detected, override user. 
    if (d_l == 0.) {
	full_shrp = 1;
    }
// determine extreme values of h 
    q = Math.sin(max_th) * 2. / lambda;
    fact = 2. / lambda;
    fact *= fact;
// h_upper is the largest value that the index h can have, 
// consistent with the value of Q. (When cell_gamma is not 90 degrees, 
// k is not necessarily zero at this extreme). 
// In case the incredible happens, immune system to rescue. 
    tmp3 = a0 * 4. * b0 - d0 * d0;
    if (tmp3 <= 0.) {
	return ret_val;
    }
//      goto 990 
    tmp3 = q * 2. * Math.sqrt(1. / tmp3);
    h_upper = (int) (tmp3 * Math.sqrt(b0));
    h_lower = -h_upper;
    k_upper = (int) (tmp3 * Math.sqrt(a0));
    k_lower = -k_upper;
// scan along h-axis from h_lower to h_upper 
    i1 = h_upper;
    for (h = h_lower; h <= i1; ++h) {
// determine limits along k for a given h 
	i2 = k_upper;
	for (k = k_lower; k <= i2; ++k) {
// if out of bounds, cycle 
	    if (h * h * a0 + k * k * b0 + c_b157 *
		    c_b157 * c0 + h * k * d0 <= q * q) {
		l_axis = h == 0 && k == 0;
		hk_th = theta1;
		if (! l_axis) {
		    d1 = (double) k;
		    d2 = (double) h;
		    hk_th = atan2(d1 * Math.sqrt(a0 * b0 -
			    d0 * d0 * .25), d2 *
			    a0 + d1 * d0 * .5);
		}
// see if in wedge to be scanned 
		if ((theta1 - hk_th) * (theta2 - hk_th)
			<= .001 || symgrpno == 1) {
// if rotational symmetry only, do not take points on upper wedge plane 
		    if (! rot_only || theta2 - hk_th >
			    .001 || symgrpno == 1) {
			if (symgrpno != 11 || l_axis) {
//            write(op,200) 'Integrating along l at ',h,k, 
//     |            '''',infile(1:LENGTH(infile)),'''' 
			    on_bndry = (d1 = hk_th - theta1,
				    Math.abs(d1)) <= .001 || (d2 = hk_th -
				    theta2, Math.abs(d2)) <= .001;
// set up the phases in the structure factors and stacking vectors 
// which depend on h and k only 
			    xyphse_(&h, &k);
			    pre_mat(&h, &k);
// assign a corrected shape-broadening half-width 
			    if (finite_width) {
				tmp2 = (h + k * Math.cos(pi -
					cell_gamma)) / (
					wa * Math.sin(pi -
					cell_gamma));
				tmp3 = k / wb;
				ffhkcnst = lambda * .25 *
					Math.sqrt(a0 * tmp2 * tmp2 +
					b0 * tmp3 * tmp3);
			    }
// get starting value of l 
			    if (l_axis) {
				tmp = min(d_theta,max_th);
				if (tmp < min_th) {
				    tmp = min_th;
				}
// Computing 2nd power 
				d1 = Math.sin(tmp);
				l1 = Math.sqrt((fact * (d1 * d1) - h * h *
					a0 - k * k * b0 -
					h * k * d0) / c0);
				shrp = any_sharp;
			    } else {
				tmp = Math.asin(.5 * lambda * Math.sqrt(h *
					h * a0 + k * k *
					b0 + c_b157 * c_b157 *
					c0 + h * k * d0));
				if (tmp < min_th) {
// Computing 2nd power 
				    d1 = Math.sin(min_th);
				    l1 = Math.sqrt((fact * (d1 * d1) - h *
					    h * a0 - k * k *
					    b0 - h * k *
					    d0) / c0);
				    tmp = Math.asin(.5 * lambda * Math.sqrt(
					    h * h * a0 + k * k *
					    b0 + l1 * l1 *
					    c0 + h * k *
					    d0));
				} else {
				    l1 = 0.;
				}
				if (any_sharp &&
					full_shrp != 1) {
				    shrp = sharp_(&h, &k, &d_l);
				    if (! ok) {
					return ret_val;
				    }
//                goto 999 
				} else {
				    shrp = any_sharp;
				}
			    }
// m indexes into the array spec 
			    m = (int) ((tmp - min_th) /
				    d_theta) + 1;
			    if (! shrp || full_shrp == 1) {
// broad streak or full adaptive integration over sharp spots 
//              if(full_shrp.eq.1 .or. full_brd.eq.1) 
//     |                write(op,300) 'Full adaptive integration' 
// integrate each d_theta's range of reciprocal space 
				d1 = max_th - 1e-10;
				d2 = d_theta;
				for (theta = tmp; d2 < 0 ? theta >= d1 :
					theta <= d1; theta += d2) {
				    l0 = l1;
// Computing MIN 
				    d3 = d_theta, d4 =
					    max_th - theta;
				    tmp2 = min(d3,d4);
				    d3 = theta + tmp2;
// Computing 2nd power 
				    d4 = Math.sin(d3);
				    l1 = Math.sqrt((fact * (d4 * d4) - h *
					    h * a0 - k * k *
					    b0 - h * k *
					    d0) / c0);
// sharp spots; do not use knowledge of where they are 
				    if (shrp) {
					x = aglq16(&h, &k, &l0, &l1, &ok);
				    } else {
// broad streaks 
					x = (*fn)(&h, &k, &l0, &l1, &ok);
				    }
				    if (! ok) {
					return ret_val;
				    }
//                goto 110 

// include weighting factors for radiation type 
				    if (rad_type ==
					    x_ray) {
					d3 = theta + tmp2 * .5;
					x = x * 2. * ((1. + Math.cos(2. * d3) *
						Math.cos(2. * d3)) / (Math.sin(d3) *
						 Math.sin(2. * d3)));
				    } else if (rad_type ==
					    neutrn) {
					d3 = theta + tmp2 * .5;
					x = x * 2. * (1. / (Math.sin(d3) * Math.sin(
						2. * d3)));
				    } else if (rad_type ==
					    electn) {
					d3 = theta + tmp2 * .5;
					x = x * 2. * (1. / (Math.sin(d3) * Math.sin(
						2. * d3)));
				    } else {
					ok = false;
					return ret_val;
//                  goto 130 
				    }

// see if not on l-axis 
				    if (! l_axis) {
// apply multiplicity factor 
					x *= mltplcty;
// if on boundary, apply appropriate weighting (mirror vs rotation only) 
					if (on_bndry) {
					    x *= bnds_wt;
					}
				    }
				    if (finite_width) {
					chwdth_(&h, &k, &l0, &l1, &x, &m, &
						max_indx);
				    } else {
					spec[m - 1] += x;
				    }
				    ++m;
// L40: 
				}
			    } else {
// line of sharp spots--detuned delta functions 
// use knowledge of where spots are 
// make sure we do all l values a multiple of d_l 
// starting with spot on hk-plane 
//              write(op,300) 'which is a line of sharp spots' 
				l00 = 0.;
				if (l_axis) {
				    l00 = d_l;
				    while(l00 >= th2_min) {
					l00 += d_l;
				    }
				}
// Computing 2nd power 
				d2 = Math.sin(max_th);
				l_max = Math.sqrt((fact * (d2 * d2) - h *
					h * a0 - k * k *
					b0 - h * k * d0) /
					 c0);
// avoid trouble by ignoring l = l_max 
				d2 = l_max;
				d1 = d_l;
				for (l = l00; d1 < 0 ? l >= d2 : l <=
					d2; l += d1) {
				    if (l != l_max) {
					theta = Math.asin(.5 * lambda *
						Math.sqrt(h * h * a0
						+ k * k * b0 + l * l
						* c0 + h * k *
						d0));
					d3 = h * h * a0 + k *
						k * b0 + l * l *
						c0 + h * k *
						d0;
					get_f(f, &d3, &l);
					tmp = intens_(f, &h, &k, &l, &ok) *
						1e-8;
// find width of peak 
					x = 1e-10;
					loopon = true;
					while(loopon) {
					    loopon = false;
// L80: 
					    if (! ok) {
			  return ret_val;
					    }
//               goto 120 
					    x *= 2.;
					    d3 = l + x;
					    d4 = h * h * a0 +
						    k * k * b0 +
						    d3 * d3 * c0
						    + h * k * d0;
					    d5 = l + x;
					    get_f(f, &d4, &d5);
					    d3 = l + x;
					    if (intens_(f, &h, &k, &d3, &
						    ok) > tmp && x <= d_l *
						    .01) {
			  loopon = true;
					    }
					}
					if (! ok) {
					    return ret_val;
					}
//                goto 120 
// Computing MAX 
					d3 = l - x;
					l0 = max(d3,0.);
// Computing MIN 
					d3 = l + x;
					l1 = min(d3,l_max);
					x = aglq16(&h, &k, &l0, &l1, &ok);
					if (! ok) {
					    return ret_val;
					}
//                goto 110 

// include weighting factors for radiation type 
					if (rad_type ==
						x_ray) {
					    x = x * 2. * ((1. + Math.cos(2. *
						    theta) * Math.cos(2. * theta))
						    / (Math.sin(theta) * Math.sin(2. *
						    theta)));
					} else if (rad_type ==
						neutrn) {
					    x = x * 2. * (1. / (Math.sin(theta) *
						    Math.sin(2. * theta)));
					} else if (rad_type ==
						electn) {
					    x = x * 2. * (1. / (Math.sin(theta) *
						    Math.sin(2. * theta)));
					} else {
					    ok = false;
					    return ret_val;
//                  goto 130 
					}

// see if not on l-axis 
					if (! l_axis) {
// apply multiplicity factor 
					    x *= mltplcty;
// if on boundary, apply appropriate weighting (mirror vs rotation only) 
					    if (on_bndry) {
			  x *= bnds_wt;
					    }
					}
					m = (int) (theta /
						d_theta) + 1;
					if (finite_width) {
					    chwdth_(&h, &k, &l0, &l1, &x, &
						    m, &max_indx);
					} else {
					    spec[m - 1] += x;
					}
				    }
// L70: 
				}
			    }
			}
		    }
		}
	    }
// L30: 
	}
// L20: 
    }
    ret_val = true;
    return ret_val;
//  110 write(op,300) 'GLQ16 returned error in GETSPC.' 
//      return 
//  120 write(op,300) 'INTENS returned error in GETSPC.' 
//      return 
//  130 write(op,300) 'ERROR: Radiation type is undefined in GETSPC' 
//  999 return 
//  990 write(op,300) 'Illegal cell parameters in GETSPC.' 
//      write(op,250) '4*a0*b0-d0*d0 = ', FOUR * a0 * b0 - d0 * d0 
//      return 
//  200 format(1x, a, 2i4, 6x, 3a) 
//  250 format(1x, a, g12.5) 
//  300 format(1x, a) 
//  400 format(1x, i3) 
} // getspc_ 



//  
// Title: GET_BDS 
// Author: MMJT 
// Date: 1 July 1989 
// Description:  This routine assigns reciprocal space vectors in the 
// h,k plane within which integration can be confined. Weightings are 
// assigned to off-axis spot intensities to allow for their 
// multiplicity relative to spots on the 00l axis. Spots that occur on 
// one of the boundaries are assigned special weighting depending on 
// whether or not the boundary is also a mirror plane. 

//      ARGUMENTS: 
//           No arguments are used. All data is in 'COMMON'. 

//      COMMON VARIABLES: 
//            uses:      SymGrpNo, h_mirror, k_mirror 

//        modifies:      h_start, k_start, h_end, k_end, mltplcty, 
//                       bnds_wt, rot_only, pnt_grp 
//  

int get_bds(void)
{
    // Local variables 

// 360 degrees, no symmetry (-1) 
// note, the scan vectors are not used in this instance since there is 
// an ambiguity between 0 and 360 degrees. We assign values anyway, so 
// that the variables are at least initialized in a controlled way. 

    if (symgrpno == 1) {
	h_start = 1.;
	k_start = 0.;
	h_end = 1.;
	k_end = 0.;
	mltplcty = 1.;
	bnds_wt = 1.;
	rot_only = true;
    }

// 180 degrees, rotation only (2/M, 1st setting) 
    if (symgrpno == 2) {
	h_start = 1.;
	k_start = 0.;
	h_end = -1.;
	k_end = 0.;
	mltplcty = 2.;
	bnds_wt = 1.;
	rot_only = true;
    }

// 180 degrees, vertical mirror (2/M, 2nd setting) 
// we need to know which mirror plane. 
    if (symgrpno == 3) {
	if (h_mirror && ! k_mirror) {
	    h_start = 1.;
	    k_start = 0.;
	    h_end = -1.;
	    k_end = 0.;
	    mltplcty = 2.;
	    bnds_wt = .5;
	    rot_only = false;
	} else if (k_mirror && ! h_mirror) {
	    h_start = 0.;
	    k_start = 1.;
	    h_end = 0.;
	    k_end = -1.;
	    mltplcty = 2.;
	    bnds_wt = .5;
	    rot_only = false;
	} else {
// In theory, the following should never be needed, but just in case, 
// let's bolster DIFFaX's immune system. 
//          write(op,400) 'DIFFaX is confused about vertical mirrors.' 
//          write(op,400) 'To be safe, symmetry is being set to -1' 
	    symgrpno = 1;
	    s_copy(pnt_grp, "-1", (ftnlen)12, (ftnlen)2);
	    h_start = 1.;
	    k_start = 0.;
	    h_end = 1.;
	    k_end = 0.;
	    mltplcty = 1.;
	    bnds_wt = 1.;
	    rot_only = true;
	}
    }

// 90 degrees, vertical mirrors (MMM) 
    if (symgrpno == 4) {
	h_start = 1.;
	k_start = 0.;
	h_end = 0.;
	k_end = 1.;
	mltplcty = 4.;
	bnds_wt = .5;
	rot_only = false;
    }

// 120 degrees, rotation only (-3) 
    if (symgrpno == 5) {
	h_start = 1.;
	k_start = 0.;
	h_end = -1.;
	k_end = 1.;
	mltplcty = 3.;
	bnds_wt = 1.;
	rot_only = true;
    }

// 60 degrees, vertical mirrors (-3M) 
    if (symgrpno == 6) {
	h_start = 1.;
	k_start = 0.;
	h_end = 0.;
	k_end = 1.;
	mltplcty = 6.;
	bnds_wt = .5;
	rot_only = false;
    }

// 90 degrees, rotation (4/M) 
    if (symgrpno == 7) {
	h_start = 1.;
	k_start = 0.;
	h_end = 0.;
	k_end = 1.;
	mltplcty = 4.;
	bnds_wt = 1.;
	rot_only = true;
    }

// 45 degrees, vertical mirrors (4/MMM) 
    if (symgrpno == 8) {
	h_start = 1.;
	k_start = 0.;
	h_end = 1.;
	k_end = 1.;
	mltplcty = 8.;
	bnds_wt = .5;
	rot_only = false;
    }

// 60 degrees, rotation (6/M) 
    if (symgrpno == 9) {
	h_start = 1.;
	k_start = 0.;
	h_end = 0.;
	k_end = 1.;
	mltplcty = 6.;
	bnds_wt = 1.;
	rot_only = true;
    }

// 30 degrees, vertical mirrors (6/MMM) 
    if (symgrpno == 10) {
	h_start = 1.;
	k_start = 0.;
	h_end = 1.;
	k_end = 1.;
	mltplcty = 12.;
	bnds_wt = .5;
	rot_only = false;
    }

// integrate along 0 0 l only (axial) 
// the following are somewhat arbitrary in this case. Assign values 
// anyway just to make sure they are initialized 
    if (symgrpno == 11) {
	h_start = 1.;
	k_start = 0.;
	h_end = 1.;
	k_end = 0.;
	mltplcty = 1.;
	bnds_wt = 1.;
	rot_only = true;
    }

    return 0;
//  400 format(1x, a) 
} // get_bds 



//  
// Title: GET_F 
// Author: MWD and MMJT 
// Date: 22 Mar 1989 
// Description: This routine calculates the form factors for each layer. 
// Since this routine is the main holdup for complex structures, it 
// attempts to make use of shortcuts detected in OPTIMZ. The Debye- 
// Waller diffuse background is assumed to be small and is not 
// calculated in this version. 

//      ARGUMENTS: 
//            f   -  Array of layer form factors. (output). 
//            Q2  -  Value of 1/d**2 at h,k,l. (input). 
//            l   -  reciprocal lattice vector l-component. (input). 

//      COMMON VARIABLES: 
//            uses:  x_sf, n_sf, e_sf, rad_type, n_atoms, l_symmetry, 
//                   one_B, l_n_atoms, a_type, hx_ky, a_pos, a_occup, 
//                   a_B, l_actual, CENTRO, ELECTN, NEUTRN, X_RAY 
//                   n_actual, n_layers 

//        modifies:  no COMMON variables are modified 
//  

int get_f(double[][] f, double s2, double l)
{
    // System generated locals 
    int i1, i2, i3, i4, i5;
    double d1, d2, d3;
    double[] z1, z2, z3;

    // Local variables 
    double[] ctmp_sum;
    int i, j, m, n;
    double q2, dot, tmp[20], fact[20];
    double[] ctmp[20];
    int type;
    double[] f_uniq[20];
    double tmp_sum;

    // Parameter adjustments 
    --f;

    // Function Body 
    q2 = *s2 * .25;
// Q2 = Math.sin(theta)**2 / lamba**2 
// determine scattering factors for each atom type 
    if (rad_type == x_ray || rad_type ==
	    electn) {
	i1 = n_atoms;
	for (i = 0; i < i1; i++) {
// This empirical formula comes from p. 71 of 
// "International Tables for X-ray Crystallography, Vol. IV" 
// (The Kynoch Press: Birmingham, England), 1974. 
	    fact[i - 1] = x_sf[i * 9 - 9] * Math.exp(
		    -x_sf[i * 9 - 8] * q2) + x_sf[i
		    * 9 - 7] * Math.exp(-x_sf[i * 9 - 6] * q2) +
		    x_sf[i * 9 - 5] * Math.exp(-x_sf[i *
		    9 - 4] * q2) + x_sf[i * 9 - 3] * Math.exp(
		    -x_sf[i * 9 - 2] * q2) + x_sf[i
		    * 9 - 1];
// L10: 
	}
    } else if (rad_type == neutrn) {
	i1 = n_atoms;
	for (i = 0; i < i1; i++) {
	    fact[i - 1] = n_sf[i - 1];
// L20: 
	}
    }

// get electron scattering factor from x-ray scattering factor 
// s = 4 pi Math.sin(theta) / lambda 
// f_electron(s) = (8 pi**2 m e**2 / h**2) {Z - fx(s)} / s**2 

//       = 0.023934 lambda**2 {Z - fx(s)} / Math.sin(theta)**2 
    if (rad_type == electn) {
	i1 = n_atoms;
	for (i = 0; i < i1; i++) {
	    fact[i - 1] = ((double) e_sf[i - 1] - fact[i
		    - 1]) * .023934 / q2;
// L30: 
	}
    }

    i1 = n_actual;
    for (m = 1; m <= i1; ++m) {
	tmp_sum = 0.;
	ctmp_sum[0] = 0., ctmp_sum[1] = 0.;
	i2 = n_atoms;
	for (n = 1; n <= i2; ++n) {
	    tmp[n - 1] = 0.;
	    i3 = n - 1;
	    ctmp[0][i3] = 0., ctmp[1][i3] = 0.;
// L35: 
	}

// First calculate the scattering factors of the unique layers. 
// Check to see if f_uniq(m) will all be real and if Debye-Waller is 
// invariant 
// Note: hx_ky(j,m) contains h*a_pos(1,j,m) + k*a_pos(2,j,m) 

	if (l_symmetry[m - 1] == centro &&
		one_b[m - 1]) {
	    i2 = l_n_atoms[m - 1];
	    for (j = 0; j < i2; j++) {
		type = a_type[j + m * 200 - 201];
		dot = hx_ky[j + m * 200 - 201] + *l *
			a_pos[(j + m * 200) * 3 - 601];
		tmp[type - 1] += a_occup[j + m * 200 - 201] *
			Math.cos(dot);
// L40: 
	    }
	    i2 = n_atoms;
	    for (j = 0; j < i2; j++) {
		tmp_sum += tmp[j - 1] * fact[j - 1];
// L45: 
	    }
// NOTE: twice since centrosymmetric 
	    i2 = m - 1;
	    d1 = Math.exp(-a_b[m * 200 - 200] * q2) * 2. * tmp_sum;
	    f_uniq[0][i2] = d1, f_uniq[1][i2] = 0.;

// Debye-Waller is not invariant 
	} else if (l_symmetry[m - 1] == centro) {
	    i2 = l_n_atoms[m - 1];
	    for (j = 0; j < i2; j++) {
		type = a_type[j + m * 200 - 201];
		dot = hx_ky[j + m * 200 - 201] + *l *
			a_pos[(j + m * 200) * 3 - 601];
		tmp[type - 1] += a_occup[j + m * 200 - 201] *
			Math.exp(-a_b[j + m * 200 - 201] * q2) * Math.cos(
			dot);
// L50: 
	    }
	    i2 = n_atoms;
	    for (j = 0; j < i2; j++) {
		tmp_sum += tmp[j - 1] * fact[j - 1];
// L55: 
	    }
// NOTE: twice since centrosymmetric 
	    i2 = m - 1;
	    d1 = tmp_sum * 2.;
	    f_uniq[0][i2] = d1, f_uniq[1][i2] = 0.;

// check if Debye-Waller is the only invariant 
// f(i) will be complex 
	} else if (one_b[m - 1]) {
	    i2 = l_n_atoms[m - 1];
	    for (j = 0; j < i2; j++) {
		type = a_type[j + m * 200 - 201];
		dot = hx_ky[j + m * 200 - 201] + *l *
			a_pos[(j + m * 200) * 3 - 601];
		i3 = type - 1;
		i4 = type - 1;
		i5 = j + m * 200 - 201;
		d1 = Math.cos(dot);
		d2 = Math.sin(dot);
		z3[0] = d1, z3[1] = d2;
		z2[0] = a_occup[i5] * z3[0], z2[1] =
			a_occup[i5] * z3[1];
		z1[0] = ctmp[0][i4] + z2[0], z1[1] = ctmp[1][i4] +
			z2[1];
		ctmp[0][i3] = z1[0], ctmp[1][i3] = z1[1];
// L60: 
	    }
	    i2 = n_atoms;
	    for (j = 0; j < i2; j++) {
		i3 = j - 1;
		i4 = j - 1;
		z2[0] = fact[i4] * ctmp[0][i3], z2[1] = fact[i4] *
			ctmp[1][i3];
		z1[0] = ctmp_sum[0] + z2[0], z1[1] = ctmp_sum[1] +
			z2[1];
		ctmp_sum[0] = z1[0], ctmp_sum[1] = z1[1];
// L65: 
	    }
	    i2 = m - 1;
	    d1 = Math.exp(-a_b[m * 200 - 200] * q2);
	    z1[0] = d1 * ctmp_sum[0], z1[1] = d1 * ctmp_sum[1];
	    f_uniq[0][i2] = z1[0], f_uniq[1][i2] = z1[1];

// Nothing is invariant 
	} else {
	    i2 = l_n_atoms[m - 1];
	    for (j = 0; j < i2; j++) {
		type = a_type[j + m * 200 - 201];
		dot = hx_ky[j + m * 200 - 201] + *l *
			a_pos[(j + m * 200) * 3 - 601];
		i3 = type - 1;
		i4 = type - 1;
		d1 = a_occup[j + m * 200 - 201] * Math.exp(
			-a_b[j + m * 200 - 201] * q2);
		d2 = Math.cos(dot);
		d3 = Math.sin(dot);
		z3[0] = d2, z3[1] = d3;
		z2[0] = d1 * z3[0], z2[1] = d1 * z3[1];
		z1[0] = ctmp[0][i4] + z2[0], z1[1] = ctmp[1][i4] +
			z2[1];
		ctmp[0][i3] = z1[0], ctmp[1][i3] = z1[1];
// L70: 
	    }
	    i2 = n_atoms;
	    for (j = 0; j < i2; j++) {
		i3 = j - 1;
		i4 = j - 1;
		z2[0] = fact[i4] * ctmp[0][i3], z2[1] = fact[i4] *
			ctmp[1][i3];
		z1[0] = ctmp_sum[0] + z2[0], z1[1] = ctmp_sum[1] +
			z2[1];
		ctmp_sum[0] = z1[0], ctmp_sum[1] = z1[1];
// L75: 
	    }
	    i2 = m - 1;
	    f_uniq[0][i2] = ctmp_sum[0], f_uniq[1][i2] = ctmp_sum[1];
	}
// L80: 
    }

// Now assign scattering factors to all the layers 
    i1 = n_layers;
    for (i = 0; i < i1; i++) {
	i2 = i;
	i3 = l_actual[i - 1] - 1;
	f[0][i2] = f_uniq[0][i3], f[1][i2] = f_uniq[1][i3];
// L90: 
    }

    return 0;
} // get_f 



//  
// Title: GET_G 
// Author: MWD and MMJT 
// Date: 18 Aug 1988; 15 Mar 1995 
// Description: This function determines g_i, the a-priori probability 
// that a layer of type i, will actually occur somewhere within the 
// crystal. 
// 'cnt' counts the l_alpha(i,i) = 1.0 terms. Then, l_g(i) = 1.0/cnt. 

//      ARGUMENTS: 
//            No input arguments. 

//      COMMON VARIABLES: 
//            uses:   n_layers, l_alpha 

//        modifies:   l_g 

//      GET_G returns boolean .true. if all went well. 
//  

boolean get_g(void)
{
    // System generated locals 
    int i1, i2;
    boolean ret_val;

    // Local variables 
    boolean singular;
    int i, j;
    double det;
    int cnt;
    double sum;
    double g_mat[20][20];
    int index[20];

    ret_val = false;
// set up matrix that represents the probabilities 
// only n-1 equations are independent 
    i1 = n_layers - 1;
    for (i = 0; i < i1; i++) {
	l_g[i - 1] = 0.;
	sum = 0.;
	i2 = n_layers;
	for (j = 0; j < i2; j++) {
	    sum += l_alpha[j + i];
// L20: 
	}
	sum = 1. / sum;
// sum should actually be ONE 
	i2 = n_layers;
	for (j = 0; j < i2; j++) {
	    g_mat[i + j] = sum * l_alpha[i + j *
		    20 - 21];
// L30: 
	}
	g_mat[i + i] += -1.;
// L10: 
    }
    l_g[n_layers - 1] = 1.;

// the sum of the g's must be 1 
    i1 = n_layers;
    for (i = 0; i < i1; i++) {
	g_mat[n_layers + i] = 1.;
// L40: 
    }

// before we invert the matrix, let's catch the pathoboolean values 
    cnt = 0;
    i1 = n_layers;
    for (i = 0; i < i1; i++) {
	if (l_alpha[i + i] == 1.) {
	    ++cnt;
	}
// L50: 
    }

    if (cnt != 0) {
	singular = true;
    } else {
	singular = false;
    }

    if (singular) {
	i1 = n_layers;
	for (i = 0; i < i1; i++) {
	    if (l_alpha[i + i] == 1.) {
// arbitrarily assume such layers occur with equal probability 
		l_g[i - 1] = 1. / (double) cnt;
	    } else {
		l_g[i - 1] = 0.;
	    }
// L60: 
	}
    } else {
// solve the matrix 
	if (! ludcmp_(g_mat, index, &n_layers, &c20, &det)) {
	    return ret_val;
	}
//        goto 100 
	lubksb_(g_mat, l_g, index, &n_layers, &c20);
    }

// If we are here then all went well 
    ret_val = true;

// Are some of the layers non-existent? 
    i1 = n_layers;
    for (i = 0; i < i1; i++) {
	if (l_g[i - 1] < 1e-6) {
//            write(op,410) 'WARNING: Layer ', i, 
//     |      ' does not occur in any significant quantity.' 
	}
// L70: 
    }

    return ret_val;
//  100 write(op,400) 
//     | 'ERROR: Stacking probabilities give a singular matrix in GET_G' 
//      return 
//  400 format(1x, a) 
//  410 format(1x, a, i2, a) 
} // get_g 



//  
// Title: GET_MAT 
// Author: MMJT 
// Date: 21 Mar 1990; 14th July 1995; 21st July 1997 
// Description:  This subroutine calculates the elements of 'mat', the 
// stacking transition matrix ie. {alpha * Math.exp(2*pi*u.R)}ij. The h and k 
// components were pre-calculated in PRE_MAT. GET_MAT calculates the 
// l-component to complete the matrix. However, the mat(i,i) terms 
// have yet to have 1 subtracted from them. This is done in GET_S. 

//      ARGUMENTS: 
//            h   -  reciprocal vector h-component. (input). 
//            k   -  reciprocal vector k-component. (input). 
//            l   -  reciprocal lattice vector l-component. (input). 

//      COMMON VARIABLES: 
//            uses:  same_rz, n_layers, same_Bs, Bs_zero, mat1, c0, bc0, 
//                   ca0, r_B33, r_B23, r_B31, l_r, PI2, there, 
//                   fatsWalla_hk 

//        modifies:  mat 
//  

int get_mat(int *h, int *k, double *l)
{
    // System generated locals 
    int i1, i2, i3, i4;
    double d1, d2, d3;
    double[] z1, z2, z3;

    // Local variables 
    int i, j;
    double fatswaller, dot;
    double twopi_l;

// set up matrix that represents the sequences 
// Note: mat is in 'i,j' order. 
    twopi_l = pi2 * *l;
    if (same_bs) {
	if (all_bs_zero) {
// all the fatsWalla_hk terms equal 1 
	    i1 = n_layers;
	    for (i = 0; i < i1; i++) {
		i2 = n_layers;
		for (j = 0; j < i2; j++) {
		    if (there[j + i]) {
			dot = twopi_l * l_r[(j + i * 20) * 3 -
				61];
			i3 = i + j;
			i4 = i + j;
			d1 = Math.cos(dot);
			d2 = Math.sin(dot);
			z2[0] = d1, z2[1] = d2;
			z1[0] = mat1[0][i4] * z2[0] -
				mat1[1][i4] * z2[1], z1[1] =
				mat1[0][i4] * z2[1] +
				mat1[1][i4] * z2[0];
			matv[0][i3] = z1v[0], mat[1][i3] =
				z1[1];
		    } else {
			i3 = i + j;
			mat[0][i3] = 0., mat[1][i3] = 0.;
		    }
// L20: 
		}
// L10: 
	    }
	} else {
// all the fatsWalla terms are identical, but are less than 1 
// fatsWalla_hk already contains the h-k component computed in PRE_MAT 
	    fatswaller = fatswalla_hk * Math.exp(-(*l * (
		    a_b33 * .25 * c0 * *l + (
		    a_b23 * bc0 * *k + a_b31 *
		    ca0 * *h) * .5)));
	    i1 = n_layers;
	    for (i = 0; i < i1; i++) {
		i2 = n_layers;
		for (j = 0; j < i2; j++) {
		    if (there[j + i]) {
			dot = twopi_l * l_r[(j + i * 20) * 3 -
				61];
			i3 = i + j;
			i4 = i + j;
			z2[0] = fatswaller * mat1[0][i4], z2[1] =
				fatswaller * mat1[1][i4];
			d1 = Math.cos(dot);
			d2 = Math.sin(dot);
			z3[0] = d1, z3[1] = d2;
			z1[0] = z2[0] * z3[0] - z2[1] * z3[1], z1[1] =
				z2[0] * z3[1] + z2[1] * z3[0];
			mat[0][i3] = z1[0], mat[1][i3] =
				z1[1];
		    } else {
			i3 = i + j;
			mat[0][i3] = 0., mat[1][i3] = 0.;
		    }
// L40: 
		}
// L30: 
	    }
	}
    } else {
// the fatsWalla terms differ. mat1 already contains the h-k component 
	i1 = n_layers;
	for (i = 0; i < i1; i++) {
	    i2 = n_layers;
	    for (j = 0; j < i2; j++) {
		if (there[j + i]) {
		    dot = twopi_l * l_r[(j + i * 20) * 3 - 61];
		    if (bs_zero[j + i]) {
			i3 = i + j;
			i4 = i + j;
			d1 = Math.cos(dot);
			d2 = Math.sin(dot);
			z2[0] = d1, z2[1] = d2;
			z1[0] = mat1[0][i4] * z2[0] -
				mat1[1][i4] * z2[1], z1[1] =
				mat1[0][i4] * z2[1] +
				mat1[1][i4] * z2[0];
			matv[i3] = z1[0], mat[1][i3] =
				z1[1];
		    } else {
			i3 = i + j;
			i4 = i + j;
			d1 = Math.cos(dot);
			d2 = Math.sin(dot);
			z3[0] = d1, z3[1] = d2;
			z2[0] = mat1[0][i4] * z3[0] -
				mat1[1][i4] * z3[1], z2[1] =
				mat1[0][i4] * z3[1] +
				mat1[1][i4] * z3[0];
			d3 = Math.exp(-(*l * (r_b33[j + i]
				 * .25 * c0 * *l + (r_b23[
				j + i] * bc0 * *k +
				r_b31[j + i] *
				ca0 * *h) * .5)));
			z1[0] = d3 * z2[0], z1[1] = d3 * z2[1];
			mat[0][i3] = z1[0], mat[1][i3] =
				z1[1];
		    }
		} else {
		    i3 = i + j;
		    mat[0][i3] = 0., mat[1][i3] = 0.;
		}
// L60: 
	    }
// L50: 
	}
    }

    return 0;
} // get_mat 



//  
// Title: GET_S 
// Author: MWD and MMJT 
// Date: 5th Aug 1991 
// Description:  This function determines the S's--the average scattered 
// wave functions of each layer at h, k, l for crystals with an 
// infinite number of layers. GET_MAT should be called prior to GET_S. 
// Note, since GET_S is called billions of times from deep within the 
// inner loops of DIFFaX's bowels, part of the matrix mat1 has been 
// precalculated in PRE_MAT in order to speed things up a bit. 

//      ARGUMENTS: 
//            f   -  Array of layer form factors. (input). 
//            s   -  Array of average layer wavefunctions. (output). 
//            h   -  reciprocal vector h-component. (input). 
//            k   -  reciprocal vector k-component. (input). 
//            l   -  reciprocal lattice vector l-component. (input). 

//      COMMON VARIABLES: 
//            uses:  n_layers 

//        modifies:  mat 

//      GET_S returns boolean .true. if all went well. 
//  

boolean get_s(double[] *f, double[] *s, int *h, int *k,
	double *l)
{
    // System generated locals 
    int i1, i2, i3;
    double[] z1, z2, z3, z4;
    boolean ret_val;

    // Local variables 
    int i;
    double[] det;
    int i_ok;
    int index[20];
    double[] s_tmp[2];

// i_ok is used by Linpack routines 

    // Parameter adjustments 
    --s;
    --f;

    // Function Body 
    ret_val = false;

// subtract identity matrix (need do this for diagonal terms only). 
    i1 = n_layers;
    for (i = 0; i < i1; i++) {
	i2 = i + i;
	i3 = i + i;
	z1[0] = mat[0][i3] - 1., z1[1] = mat[1][i3] -
		0.;
	mat[0][i2] = z1[0], mat[1][i2] = z1[1];
// L10: 
    }

// Now solve the system of equations. 
    if (n_layers > 2) {
// now call LINPACK routines 
	cgefa_(mat, &c20, &n_layers, index, &i_ok);
	if (i_ok != 0) {
	    return ret_val;
	}
//        goto 999 
	cgesl_(mat, &c20, &n_layers, index, &s[1], &
		c0);
    } else if (n_layers == 2) {
// its a simple 2 x 2, so solve it directly 
	z2[0] = mat[0][0] * mat[0][21] - mat[1][0] *
		mat[1][21], z2[1] = mat[0][0] * mat[1][
		21] + mat[1][0] * mat[0][21];
	z3[0] = mat[0][20] * mat[0][1] - mat[1][20] *
		 mat[1][1], z3[1] = mat[0][20] *
		mat[1][1] + mat[1][20] * mat[0][1];
	z1[0] = z2[0] - z3[0], z1[1] = z2[1] - z3[1];
	det[0] = z1[0], det[1] = z1[1];
	if (det[0] == 0. && det[1] == 0.) {
	    return ret_val;
	}
//        goto 999 
// copy s (remember, if infinitely thick, s = -f) 
	z1[0] = -s[0][1], z1[1] = -s[1][1];
	s_tmp[0][0] = z1[0], s_tmp[1][0] = z1[1];
	z1[0] = -s[0][2], z1[1] = -s[1][2];
	s_tmp[0][1] = z1[0], s_tmp[1][1] = z1[1];
	z3[0] = mat[0][20] * s_tmp[0][1] - mat[1][20] *
		s_tmp[1][1], z3[1] = mat[0][20] * s_tmp[1][1] +
		mat[1][20] * s_tmp[0][1];
	z4[0] = mat[0][21] * s_tmp[0][0] - mat[1][21] *
		s_tmp[1][0], z4[1] = mat[0][21] * s_tmp[1][0] +
		mat[1][21] * s_tmp[0][0];
	z2[0] = z3[0] - z4[0], z2[1] = z3[1] - z4[1];
	z_div(&z1, &z2, &det);
	s[0][1] = z1[0], s[1][1] = z1[1];
	z3[0] = mat[0][1] * s_tmp[0][0] - mat[1][1] *
		s_tmp[1][0], z3[1] = mat[0][1] * s_tmp[1][0] +
		mat[1][1] * s_tmp[0][0];
	z4[0] = mat[0][0] * s_tmp[0][1] - mat[1][0] *
		s_tmp[1][1], z4[1] = mat[0][0] * s_tmp[1][1] +
		mat[1][0] * s_tmp[0][1];
	z2[0] = z3[0] - z4[0], z2[1] = z3[1] - z4[1];
	z_div(&z1, &z2, &det);
	s[0][2] = z1[0], s[1][2] = z1[1];
    } else if (n_layers == 1) {
// only one layer, so solve it immediately 
	z2[0] = -f[0][1], z2[1] = -f[1][1];
	z_div(&z1, &z2, mat);
	s[0][1] = z1[0], s[1][1] = z1[1];
    }

    ret_val = true;
    return ret_val;
//  999 write(op,400) 'Solving for sequence produces a singular matrix.' 
//      write(op,401) h, k, l 
//      do 50 i = 1, n_layers 
//        write(op,402) i, f(i) 
//   50 continue 
//      return 
//  400 format(1x, 'GET_S:', a) 
//  401 format(1x, 'h = ', i3, ' k = ', i3, ' l = ', g12.5) 
//  402 format(5x, 'f(', i2, ') = (', g12.5, ',', g12.5, ')') 
} // get_s 



//  
// Title: GET_S2 
// Author: MMJT 
// Date: 5 Feb 1990 
// Description:  This function determines the S's--the average scattered 
// wave functions of each layer at h, k, l for crystals with only a 
// finite number of layers, l_cnt. The equation being solved is 

//   inv(Ident-T) * ((N+1)*Ident - inv(Ident-T)*(Ident-T**(N+1)) * F / N 

//  where N = l_cnt, and T is the stacking probability matrix 

//      ARGUMENTS: 
//            f   -  Array of layer form factors. (input). 
//            s   -  Array of average layer wavefunctions. (output). 
//            h   -  reciprocal vector h-component. (input). 
//            k   -  reciprocal vector k-component. (input). 
//            l   -  reciprocal lattice vector l-component. (input). 

//      COMMON VARIABLES: 
//            uses:  n_layers, l_cnt 

//        modifies:  mat 

//      GET_S2 returns boolean .true. if all went well. 
//  

boolean get_s2(double[] *f, double[] *s, int *h, int *k,
	 double *l)
{
    // System generated locals 
    int i1, i2, i3, i4;
    double d1, d2;
    double[] z1, z2, z3;
    boolean ret_val;

    // Local variables 
    int i, j;
    boolean ok;
    double[] ctmp;
    double[] mat_n = new double[2][20][20];
    double[] tmp_mat = new double[2][20][20];

    // Parameter adjustments 
    --s;
    --f;

    // Function Body 
    ret_val = false;

// get matrix mat multiplied by itself l_cnt+1 times 
    ok = mat2n_(mat_n);
    if (! ok) {
	return ret_val;
    }
//      goto 990 

// subtract identity matrix, and make a copy of mat. 
    i1 = n_layers;
    for (i = 0; i < i1; i++) {
	i2 = n_layers;
	for (j = 0; j < i2; j++) {
	    i3 = j + i;
	    i4 = j + i;
	    tmp_mat[0][i3] = mat[0][i4], tmp_mat[1][i3] =
		    mat[1][i4];
// L20: 
	}
	i2 = i + i;
	i3 = i + i;
	z1[0] = mat_n[0][i3] - 1., z1[1] = mat_n[i3] - 0.;
	mat_n[0][i2] = z1[1][0], mat_n[1][i2] = z1[1];
// L10: 
    }

// Multiply out -(Ident - T**(l_cnt+1))F. 
    i1 = n_layers;
    for (i = 0; i < i1; i++) {
	ctmp[0] = 0., ctmp[1] = 0.;
	i2 = n_layers;
	for (j = 0; j < i2; j++) {
	    i3 = i + j;
	    i4 = j;
	    z2[0] = mat_nv[i3] * f[0][i4] - mat_n[1][i3] * f[1][i4]
		    , z2[1] = mat_n[0][i3] * f[1][i4] + mat_n[1][i3]
		     * f[0][i4];
	    z1[0] = ctmp[0] + z2[0], z1[1] = ctmp[1] + z2[1];
	    ctmp[0] = z1[0], ctmp[1] = z1[1];
// L40: 
	}
	i2 = i;
	s[0][i2] = ctmp[0], s[1][i2] = ctmp[1];
// L30: 
    }
// Next, solve. ie. inv(Ident - T) * (Ident - T**(l_cnt+1))*F 
// where now s = (Ident - T**(l_cnt+1))*F 
    ok = get_s(&f[1], &s[1], h, k, l);
    if (! ok) {
	return ret_val;
    }
//      goto 999 

// Use result to build a new vector, and solve again. 
// First, reconstruct mat. 
    i1 = n_layers;
    for (i = 0; i < i1; i++) {
	i2 = i;
	i3 = i;
	i4 = i;
	d1 = (double) (l_cnt + 1);
	z3[0] = d1 * f[0][i4], z3[1] = d1 * f[1][i4];
	z2[0] = s[0][i3] - z3[0], z2[1] = s[1][i3] - z3[1];
	d2 = (double) l_cnt;
	z1[0] = z2[0] / d2, z1[1] = z2[1] / d2;
	s[0][i2] = z1[0], s[1][i2] = z1[1];
	i2 = n_layers;
	for (j = 0; j < i2; j++) {
	    i3 = j + i;
	    i4 = j + i;
	    mat[0][i3] = tmp_mat[0][i4], mat[1][i3] =
		    tmp_mat[1][i4];
// L60: 
	}
// L50: 
    }
// Solve with new RHS vector 
    ok = get_s(&f[1], &s[1], h, k, l);
    if (! ok) {
	return ret_val;
    }
//      goto 999 

    ret_val = true;
    return ret_val;
//  990 write(op,400) 'MAT2N returned an error in GET_S2.' 
//      write(op,401) h, k, l 
//      return 
//  999 write(op,400) 'Solving for sequence produces a singular matrix.' 
//      write(op,401) h, k, l 
//      do 70 i = 1, n_layers 
//        write(op,402) i, f(i) 
//   70 continue 
//     return 
//  400 format(1x, 'GET_S2:', a) 
//  401 format(1x, 'h = ', i3, ' k = ', i3, ' l = ', g12.5) 
//  402 format(5x, 'f(', i2, ') = (', g12.5, ',', g12.5, ')') 
} // get_s2 



//  
// Title: GET_SYM 
// Author: MMJT 
// Date: 19 June 1990 
// Determines the symmetry of the diffraction pattern, thereby 
// defining the smallest volume of reciprocal space which needs to 
// be integrated over. There are only 10 kinematical diffraction point 
// groups in the presence of streaking. Friedel's law ensures there 
// is always a center of symmetry, and the possibility of streaking 
// prohibits the cubic point groups. The point group symmetry number 
// is returned as GET_SYM. 
// The 10 groups are: 

//              GET_SYM          point group 
//           ------------------------------- 
//                 1:       |        -1 
//                 2:       |        2/M(1) (diad along streaks) 
//                 3:       |        2/M(2) (mirror contains streaks) 
//                 4:       |        MMM 
//                 5:       |        -3 
//                 6:       |        -3M 
//                 7:       |        4/M 
//                 8:       |        4/MMM 
//                 9:       |        6/M 
//                10:       |        6/MMM 

// The point group symbol is returned in the global character string 
// 'pnt_grp'. 

//      ARGUMENTS: 
//            ok  -  boolean flag indicating all went well. (output). 

//      COMMON VARIABLES: 
//            uses:  cell_gamma, DoSymDump, cell_a, cell_b, PI, PI2 
//                   RAD2DEG 

//        modifies:  max_var, pnt_grp, h_mirror, k_mirror 

//      GET_SYM returns one of the ten symmetry flags listed above. 
//  

int get_sym(boolean[] ok)
{
    // System generated locals 
    int ret_val;
    double d1;

    // Local variables 
    boolean eq_sides, diad;
    int idum;
    boolean cell90, hexad, triad, cell120, tetrad;
    double tmp_var;
    int rot_sym;

// initialize random numbers in RAN3 
    idum = -1;

// initialize function 
    ret_val = 0;

    max_var = 0.;
    diad = false;
    triad = false;
    tetrad = false;
    hexad = false;
    cell90 = (d1 = cell_gamma - pi * .5, Math.abs(d1)) <
	    pi * .5 * 1e-6;
    cell120 = (d1 = cell_gamma - pi2 / 3., Math.abs(d1)) <
	    pi2 * 1e-6 / 3.;

// sample reciprocal space to get an idea of the sort of intensities 
// that are out there. 
// test for vertical mirror (equivalent to a 2-fold, Friedel's law) 
    tmp_var = max_var;
    rot_sym = 2;
    diad = tst_rot(&rot_sym, &idum, ok);
    if (! (*ok)) {
	return ret_val;
    }
//      goto 997 
    if (! diad) {
	max_var = tmp_var;
    }

// if the cell angle is neither 90 nor 120 degrees, then the symmetry 
// has to be either -1 or 2/M. 
    if (! cell90 && ! cell120) {
	if (dosymdump) {
//          write(sy,'(a)') ' ' 
//          write(sy,220) cell_gamma * RAD2DEG 
//          write(sy,221) 
	}

	if (diad) {
	    ret_val = 2;
	    s_copy(pnt_grp, "2/M(1)", (ftnlen)12, (ftnlen)6);
	} else {
	    ret_val = 1;
	    s_copy(pnt_grp, "-1", (ftnlen)12, (ftnlen)2);
	}
	return ret_val;
//        goto 900 
    }
    eq_sides = (d1 = cell_a - cell_b, Math.abs(d1)) <=
	    (cell_a + cell_b) * 4.9999999999999998e-7;
    if (! eq_sides && dosymdump) {
//        write(sy,'(a)') ' ' 
//        write(sy,225) cell_a, cell_b 
//        write(sy,226) 
//        write(sy,221) 
    }

// cell_gamma is either 90 or 120 degrees. 
// if cell_a = cell_b, higher rotational symmetry is possible 
    if (eq_sides) {
// Note, it is quite possible for an oblique cell (whose cell_gamma is 
// not equal to 120 degrees) to have 3-fold symmetry. We do not test 
// for this. 
	tmp_var = max_var;
	if (cell120) {
	    rot_sym = 3;
	    triad = tst_rot(&rot_sym, &idum, ok);
	    if (! (*ok)) {
		return ret_val;
	    }
//          goto 997 
	} else {
	    triad = false;
	}
	if (! triad) {
	    max_var = tmp_var;
	}
	hexad = diad && triad;
//        if(hexad.and.DoSymDump) write(sy,200) 
	if (diad && cell90 && ! triad) {
	    tmp_var = max_var;
	    rot_sym = 4;
	    tetrad = tst_rot(&rot_sym, &idum, ok);
	    if (! (*ok)) {
		return ret_val;
	    }
//          goto 997 
	    if (! tetrad) {
		max_var = tmp_var;
	    }
	} else {
	    tetrad = false;
	}
    }

// Now test for mirrors. 
    tmp_var = max_var;
    h_mirror = tst_mir(&c1, &idum, ok);
    if (! (*ok)) {
	return ret_val;
    }
//      goto 998 
    if (! h_mirror) {
	max_var = tmp_var;
    }
    tmp_var = max_var;
    k_mirror = tst_mir(&c2, &idum, ok);
    if (! (*ok)) {
	return ret_val;
    }
//      goto 999 
    if (! k_mirror) {
	max_var = tmp_var;
    }
    tmp_var = max_var;
    hk_mirror = tst_mir(&c3, &idum, ok);
    if (! (*ok)) {
	return ret_val;
    }
//      goto 999 
    if (! hk_mirror) {
	max_var = tmp_var;
    }

// If h_mirror does not equal k_mirror, then there cannot be a higher 
// rotation symmetry than a diad. If, by some bizarre freak, this is 
// inconsistent with the result of TST_ROT, choose the lower symmetry. 
    if (h_mirror != k_mirror) {
	if (diad) {
	    ret_val = 2;
	    s_copy(pnt_grp, "2/M(1)", (ftnlen)12, (ftnlen)6);
	} else {
	    ret_val = 3;
	    s_copy(pnt_grp, "2/M(2)", (ftnlen)12, (ftnlen)6);
	}
	return ret_val;
//        goto 900 
    }
// Now check for combinations of mirrors and rotation axes. 

// 6-fold 
    if (hexad) {
	if (h_mirror || hk_mirror) {
	    ret_val = 10;
	    s_copy(pnt_grp, "6/MMM", (ftnlen)12, (ftnlen)5);
	} else {
	    ret_val = 9;
	    s_copy(pnt_grp, "6/M", (ftnlen)12, (ftnlen)3);
	}
	return ret_val;
//        goto 900 
    }
// 4-fold 
    if (tetrad) {
	if (h_mirror || hk_mirror) {
	    ret_val = 8;
	    s_copy(pnt_grp, "4/MMM", (ftnlen)12, (ftnlen)5);
	} else {
	    ret_val = 7;
	    s_copy(pnt_grp, "4/M", (ftnlen)12, (ftnlen)3);
	}
	return ret_val;
//        goto 900 
    }
// 3-fold 
    if (triad && ! diad) {
	if (h_mirror || hk_mirror) {
	    ret_val = 6;
	    s_copy(pnt_grp, "-3M", (ftnlen)12, (ftnlen)3);
	} else {
	    ret_val = 5;
	    s_copy(pnt_grp, "-3", (ftnlen)12, (ftnlen)2);
	}
	return ret_val;
//        goto 900 
    }
// 2-fold 
    if (diad) {
// handle special case of non-orthogonal mesh which has a diad, 
// no triad, and one mirror. Diad prevails, vertical mirrors ignored. 
	if ((h_mirror || hk_mirror) && cell90) {
	    ret_val = 4;
	    s_copy(pnt_grp, "MMM", (ftnlen)12, (ftnlen)3);
	} else {
	    ret_val = 2;
	    s_copy(pnt_grp, "2/M(1)", (ftnlen)12, (ftnlen)6);
	}
	return ret_val;
//        goto 900 
    }
// if no symmetry has been detected opt for lowest symmetry 
    ret_val = 1;
    s_copy(pnt_grp, "-1", (ftnlen)12, (ftnlen)2);

    return ret_val;
//  997 write(op,228) 'ERROR in GET_SYM: error returned by TST_ROT' 
//      write(op,229) '      while testing for ', rot_sym, '-fold axis.' 
//        return 
//  998 write(op,228) 'ERROR in GET_SYM: error returned by TST_MIR' 
//      write(op,228) '   while testing for mirror about the a - c plane' 
//        return 
//  999 write(op,228) 'ERROR in GET_SYM: error returned by TST_MIR' 
//      write(op,228) '   while testing for mirror about the b - c plane' 
//      return 
//  200 format(1x,'THE 2-FOLD AND 3-FOLD IMPLY 6-FOLD ROTATION SYMMETRY') 
//  220 format(1x, 'Cell_gamma = ', g12.5, ' degrees') 
//  221 format(1x, 'Rotational symmetry higher than 2-fold is unlikely.') 
//  225 format(1x, 'cell-a = ', g12.5, ' cell-b = ', g12.5) 
//  226 format(1x, 'Cell sides are not equal.') 
//  228 format(1x, a) 
//  229 format(1x, a, i2, a) 
} // get_sym 



//  
// Title: GAUSSN 
// Author: MMJT 
// Date: 17 Feb 1990; 7 Mar 1995 
// Description: This subroutine simulates Gaussian instrumental 
// broadening. std_dev is in degrees. The algorithm used does not 
// conserve intensity when std_dev is comparable to d_theta. Intensities 
// at the extreme ends of the spectrum are corrupted slightly. 

//      ARGUMENTS: 
//            th2_low  -  lowest 2theta angle to consider. (input). 

//      COMMON VARIABLES: 
//            uses:  NONE, PI2, RAD2DEG, blurring, d_theta, FWHM 
//                   th2_max 

//        modifies:  brd_spc, spec 

//  

int gaussn_(double *th2_low)
{
    // System generated locals 
    int i1, i2;
    double d1;

    // Local variables 
    int i, j, m;
    double k1, k2, k3, gss, tmp, tmp1, tmp2;
    int n_low;
    double const;
    int n_high;
    double std_dev;

    if (fwhm <= 0.) {
	blurring = none;
	return 0;
//      goto 999 
    }
    std_dev = fwhm / Math.sqrt(Math.log(2.) * 8.);

// check that cut-off is reasonable 
    if (*th2_low < 0. || *th2_low >= th2_max) {
//        write(op,101) 'GAUSSN: Cut-off angle ', th2_low, 
//     |        ' is out of bounds. Angle reset to zero.' 
	*th2_low = 0.;
    }

// th2_low is the angle relative to th2_min 
// 2*d_theta is the angular step size 
    n_low = (int) (*th2_low * .5 / d_theta) + 1;
    n_high = (int) ((th2_max - th2_min) * .5 /
	    d_theta) + 1;

    const = rad2deg * 2. * d_theta;
    k1 = const / (Math.sqrt(pi2) * std_dev);
// Computing 2nd power 
    d1 = const / std_dev;
    k2 = d1 * d1 * .5;

    i1 = n_high;
    for (i = 0; i < i1; i++) {
	brd_spc[i - 1] = 0.;
// L10: 
    }

// go out to 40 standard deviations, or to the end of the spectrum 
    d1 = std_dev * 40. / const;
    m = i_dnnt(&d1);
    if (m > n_high) {
	m = n_high;
    }
    i1 = m;
    for (i = 0; i <= i1; ++i) {
	k3 = k2 * (double) (i * i);
	gss = k1 * Math.exp(-k3);
	i2 = n_high;
	for (j = n_low + 1; j <= i2; ++j) {
	    tmp1 = 0.;
	    tmp2 = 0.;
	    if (j - i > n_low) {
		tmp1 = spec[j - i - 1];
	    }
	    if (j + i <= n_high) {
		tmp2 = spec[j + i - 1];
	    }
	    tmp = tmp1 + tmp2;
	    if (i == 0) {
		tmp *= .5;
	    }
	    brd_spc[j - 1] += gss * tmp;
// L30: 
	}
// L20: 
    }
    return 0;
//  999 write(op,101) 'Illegal FWHM ', FWHM, ' in GAUSSN.' 
//      write(op,100)'Gaussian instrumental broadening not added' 
// kill blurring option 
//      blurring = NONE 
//      return 
//  100 format(1x, a) 
//  101 format(1x, a, g12.5, a) 
} // gaussn_ 



//  
// Title: GLQ16 
// Authors: MWD and MMJT 
// Date: 6 April 1989; 13th July 95 
//  This routine performs 16-point Gauss-Legendre quadrature on an 
//  interval in reciprocal space. The interval is (h,k,a) to (h,k,b). 
//  The routine calls INTENS at each of the 16 points, and if all goes 
//  well, returns .true.. 
//  In the interests of speed, this routine has the option of calling 
//  APPR_F, which returns interpolated f values for each of the 16 
//  points. This modification slows down the procedure for structures 
//  with few atoms per layer, but speeds it up if there are many atoms 
//  per layer. 

//      ARGUMENTS: 
//            h   -  reciprocal lattice vector h-component. (input). 
//            k   -  reciprocal lattice vector k-component. (input). 
//            a   -  l-value of the lower bound of reciprocal 
//                   lattice integration region. (input). 
//            b   -  l-value of the upper bound of reciprocal 
//                   lattice integration region. (input). 
//            ok  -  boolean flag indicating all went well. (output). 

//      COMMON VARIABLES: 
//            uses:  a0, b0, c0, d0, recrsv 

//        modifies:  intp_F 

//      GLQ16 returns the integrated value. 
//  

double glq16(int h, int k, double a, double b, boolean[] ok)
{
    // System generated locals 
    int i1, i2, i3;
    double ret_val, d1;

    // Local variables 
    double[][][] f = new double[2][20][16];
    int i, j;
    boolean o;
    double c1, c2;
    boolean too_close;
    double[] ag_l = new double[16];
    int[] list = new int[3];
    double[] samp_l = new double[3];

// f is approximated by a polynomial of order (n-1) 

// statement function 
// Q2 is the value of 1/d**2 at hkl 

// initialize, to suppress compiler warnings 
    ret_val = 0.;

// check that the integration range is legitimate 
    if (b < a) {
	ok[0] = false;
	return ret_val;
//      goto 999 
    }
// catch pathoboolean values of a and b - i.e. zero integration range! 
    if (b == a) {
	ok[0] = true;
	return ret_val;
//        goto 900 
    }

// let's interpolate ('hard-wired' in this version) 
    intp_f = true;

    c1 = (b - a) * .5;
    c2 = c1 + a;

// set up the 16 l-values to sample 
    ag_l[0] = -c1 * .989400934991649932596 + c2;
    ag_l[1] = -c1 * .944575023073232576078 + c2;
    ag_l[2] = -c1 * .86563120238783174388 + c2;
    ag_l[3] = -c1 * .755404408355003033895 + c2;
    ag_l[4] = -c1 * .617876244402643748447 + c2;
    ag_l[5] = -c1 * .458016777657227386342 + c2;
    ag_l[6] = -c1 * .28160355077925891323 + c2;
    ag_l[7] = -c1 * .095012509837637440185 + c2;

    ag_l[8] = c1 * .095012509837637440185 + c2;
    ag_l[9] = c1 * .28160355077925891323 + c2;
    ag_l[10] = c1 * .458016777657227386342 + c2;
    ag_l[11] = c1 * .617876244402643748447 + c2;
    ag_l[12] = c1 * .755404408355003033895 + c2;
    ag_l[13] = c1 * .86563120238783174388 + c2;
    ag_l[14] = c1 * .944575023073232576078 + c2;
    ag_l[15] = c1 * .989400934991649932596 + c2;

    if (intp_f) {

// choose special values to sample (3 point interpolation) 
	list[0] = 1;
	list[1] = 8;
	list[2] = 16;
	samp_l[0] = ag_l[list[0] - 1];
	samp_l[1] = ag_l[list[1] - 1];
	samp_l[2] = ag_l[list[2] - 1];

// Deal with very rare cases when the spread in l values is too small 
	too_close = samp_l[0] == samp_l[2] || samp_l[0] == samp_l[1]
		 || samp_l[1] == samp_l[2];

	if (! too_close) {
	    appr_f(f, h, k, samp_l, ag_l, &c3, list, ok);
	    if (! (*ok)) {
		*ok = false;
		return ret_val;
//          goto 990 
	    }
	} else {
// sample f values once, and set all 16 values over l-range to be equal 
	    d1 = *h * *h * a0 + *k * *k * b0 + ag_l[
		    0] * ag_l[0] * c0 + *h * *k * d0;
	    get_f(f, &d1, ag_l);
	    i1 = n_layers;
	    for (j = 1; j <= i1; ++j) {
		for (i = 2; i <= 16; ++i) {
		    i2 = j + i;
		    i3 = j - 1;
		    f[0][i2] = f[0][i3], f[1][i2] = f[1][i3];
// L20: 
		}
// L10: 
	    }
	}

    } else {
// do not interpolate 
	for (i = 1; i <= 16; ++i) {
	    d1 = *h * *h * a0 + *k * *k * b0 + ag_l[
		    i - 1] * ag_l[i - 1] * c0 + *h * *k *
		    d0;
	    get_f(&f[i * 20 - 20], &d1, &ag_l[i - 1]);
// L30: 
	}

    }

// sum intensities 

    o = true;
    if (recrsv) {

	ret_val = c1 * ((intens_(f, h, k, ag_l, &o) + intens_(&f[300],
		h, k, &ag_l[15], &o)) * .027152459411754094852 + (intens_(
		&f[20], h, k, &ag_l[1], &o) + intens_(&f[280], h, k, &
		ag_l[14], &o)) * .062253523938647892863 + (intens_(&f[40],
		h, k, &ag_l[2], &o) + intens_(&f[260], h, k, &ag_l[13]
		, &o)) * .09515851168249278481 + (intens_(&f[60], h, k, &
		ag_l[3], &o) + intens_(&f[240], h, k, &ag_l[12], &o)) *
		.124628971255533872052 + (intens_(&f[80], h, k, &ag_l[4],
		&o) + intens_(&f[220], h, k, &ag_l[11], &o)) *
		.149595988816576732081 + (intens_(&f[100], h, k, &ag_l[5],
		 &o) + intens_(&f[200], h, k, &ag_l[10], &o)) *
		.169156519395002538189 + (intens_(&f[120], h, k, &ag_l[6],
		 &o) + intens_(&f[180], h, k, &ag_l[9], &o)) *
		.182603415044923588867 + (intens_(&f[140], h, k, &ag_l[7],
		 &o) + intens_(&f[160], h, k, &ag_l[8], &o)) *
		.189450610455068496285);

    } else {

	ret_val = c1 * ((inten2_(f, h, k, ag_l, &o) + inten2_(&f[300],
		h, k, &ag_l[15], &o)) * .027152459411754094852 + (inten2_(
		&f[20], h, k, &ag_l[1], &o) + inten2_(&f[280], h, k, &
		ag_l[14], &o)) * .062253523938647892863 + (inten2_(&f[40],
		h, k, &ag_l[2], &o) + inten2_(&f[260], h, k, &ag_l[13]
		, &o)) * .09515851168249278481 + (inten2_(&f[60], h, k, &
		ag_l[3], &o) + inten2_(&f[240], h, k, &ag_l[12], &o)) *
		.124628971255533872052 + (inten2_(&f[80], h, k, &ag_l[4],
		&o) + inten2_(&f[220], h, k, &ag_l[11], &o)) *
		.149595988816576732081 + (inten2_(&f[100], h, k, &ag_l[5],
		 &o) + inten2_(&f[200], h, k, &ag_l[10], &o)) *
		.169156519395002538189 + (inten2_(&f[120], h, k, &ag_l[6],
		 &o) + inten2_(&f[180], h, k, &ag_l[9], &o)) *
		.182603415044923588867 + (inten2_(&f[140], h, k, &ag_l[7],
		 &o) + inten2_(&f[160], h, k, &ag_l[8], &o)) *
		.189450610455068496285);

    }
    *ok = o;
    return ret_val;
//  999 ok = .false. 
//      write(op,100) 'GLQ16: Illegal integration interval!' 
//      write(op,101) h, k, a, b 
//      return 
//  990 ok = .false. 
//      write(op,100) 'GLQ16: ERROR returned from APPR_F' 
//      write(op,101) h, k, a, b 
//      return 
//  100 format(1x, a) 
//  101 format(1x, 'h = ',i4,', k = ',i4,', l0 = ',g12.5,', l1 = ',g12.5) 
} // glq16 



//  
// Title: GOINTR 
// Author: MMJT 
// Date: 23 Oct 1989 
// Description: This subroutine sets up the parameters necessary to 
// integrate over an interval of reciprocal space. 

//      ARGUMENTS: 
//            ok  -  boolean flag indicating all went well. (output). 

//      COMMON VARIABLES: 
//            uses:  cntrl, CFile 

//        modifies:  no COMMON variables are modified 
//  

int gointr(boolean[] ok)
{
    // Local variables 
    int i;


    i = 1;
    while(i == 1) {
//      write(op,100) ' ' 
//      write(op,100) 'INTEGRATING INTENSITY IN AN INTERVAL ALONG l. . .' 
//      write(op,100) 'Enter 1 for adaptive quadrature.' 
//      read(cntrl,*,err=1) i 
//      if(CFile) write(op,101) i 
	if (i == 1) {
	    integr_((D_fp)aglq16, ok);
	} else {
	    integr_((D_fp)glq16, ok);
	}
	if (! (*ok)) {
	    return 0;
	}
//      goto 999 
	i = 1;
//    2 write(op,100) 'Enter 1 to integrate another interval.' 
//      read(cntrl,*,err=2) i 
//      if(CFile) write(op,101) i 
    }

    return 0;
//  999 return 
//  100 format(1x, a) 
//  101 format(1x, i3) 
} // gointr_ 



//  
// Title: GOSADP 
// Author: MMJT 
// Date: 23 Oct 1989; 1st Mar 2000 
// Description: This subroutine sets up a file whose name is given 
// by 'outfile', which is derived from the input filename 'infile'. 
// The selected area diffraction pattern (SADP) is calculated and is 
// then written in binary format to 'outfile'. 

//      ARGUMENTS: 
//            infile   -  name of input file (input). 
//            outfile  -  name of output file (output). 
//            ok       -  boolean flag indicating all went well. 
//                                                      (output). 

//      COMMON VARIABLES: 
//            uses:  cntrl, CFile 

//        modifies:  loglin, brightness 
//  

int gosadp(boolean[] ok)
{
    // Local variables 
    int hk_lim;
    int i_adapt, i_plane;
    double l_upper;

// Get a suitable file name, and open file. 
    getfnm_(infile, outfile, "sadp", ok, infile_len, outfile_len, (ftnlen)4);
    if (! (*ok)) {
	return 0;
    }
//      goto 999 
// Open unformatted for binary write. 
//      if(sa.ne.op) open(unit = sa, file = outfile, status = 'new', 
//     |       form = 'unformatted', err = 990, iostat = io_err) 
// some compilers need the following added for true binary output 
//     |       ,recordtype = 'stream') 

//      write(op,100) ' ' 
//      write(op,100)'CALCULATING SELECTED AREA DIFFRACTION PATTERN. . .' 

// 1234 write(op,100) 'Enter 1 for adaptive quadrature.' 
//      read(cntrl,*,err=1234) i_adapt 
//      if(CFile) write(op,101) i_adapt 

//    1 write(op,100) 'Choose a plane in reciprocal space to view.' 
//      write(op,100) '       the l-axis is included by default.' 
//      write(op,100) '1: k = 0.   2: h = 0.   3: h = k.   4: h = -k.' 
//      read(cntrl,*,err=1) i_plane 
//      if(CFile) write(op,101) i_plane 
    if (i_plane < 1 || i_plane > 4) {
//        if(CFile) then 
//          write(op,100) 'Illegal choice. Default is k = 0.' 
	i_plane = 1;
//        else 
//          write(op,100) 'Illegal choice. Choose again.' 
//          goto 1 
//        endif 
    }

// Get upper bounds. The SADP will be square, so we only need one bound. 
// Choose l-axis, since this is common to all 4 views and makes scaling 
// simpler if more than one SAD pattern is requested. 
// 2345 write(op,100) 'Enter maximum value of l.' 
//      read(cntrl,*,err=2345) l_upper 
//      if(CFile) write(op,104) l_upper 

// 8-bit images or 16-bit images? 
// 3456 write(op,100) 'Choose the bit-depth of the image.' 
//      write(op,100) '   8: - 8-bits  16: - 16-bits' 
//      read(cntrl,*,err=3456) bitdepth 
//      if(CFile) write(op,101) bitdepth 
    if (bitdepth != 8 && bitdepth != 16) {
//        write(op,100) 'Illegal bit-depth.' 
//        if(CFile) then 
//          write(op,100) 'Using 8-bit as the default' 
//          bitdepth = 8 
//        else 
//          write(op,100) 'Re-enter. . .' 
//          goto 3456 
//        endif 
    }
    if (bitdepth == 16) {
// Bypass issue of signed or unsigned format. Use range 0 - 32767 only. 
//        write(op,100) 'File will be saved in unsigned 16-bit format.' 
	maxsad = 32767;
    } else {
//        write(op,100) 'File will be saved in unsigned 8-bit format.' 
	maxsad = 255;
    }

// Logarithmic or linear intensity scaling? 
//    2 write(op,100) 'Choose intensity scaling' 
//      write(op,100) '   0: - Logarithmic  1: - Linear' 
//      read(cntrl,*,err=2) loglin 
//      if(CFile) write(op,101) loglin 
    if (loglin < 0 || loglin > 1) {
//        write(op,100) 'Illegal intensity scaling type.' 
//        if(CFile) then 
//          write(op,100) 'Using linear as the default' 
	loglin = 1;
//        else 
//          write(op,100) 'Re-enter. . .' 
//          goto 2 
//        endif 
    }

// By how much do we wish to saturate? 
//    3 write(op,100) 'Enter a brightness (must be +ve)' 
    if (bitdepth == 16) {
//        write(op,100)'  1 - scales intensities to the range 0 - 32767' 
//        write(op,100)' 10 - scales intensities to the range 0 - 327670,' 
//        write(op,100)'      but values above 65535 will be saturated' 
    } else {
//        write(op,100)'  1 - scales intensities to the range 0 - 255' 
//        write(op,100)' 10 - scales intensities to the range 0 - 2550,' 
//        write(op,100)'      but values above 255 will be saturated' 
    }
//      read(cntrl,*,err=3) brightness 
//      if(CFile) write(op,104) brightness 
    if (brightness <= 0.) {
//        write(op,100) 'Illegal value for brightness. Must be positive' 
//        if(CFile) then 
	if (loglin == 0) {
//            write(op,100) 'Using default of 1 for logarithmic scaling' 
	    brightness = 1.;
	} else {
//            write(op,100) 'Using default of 10 for linear scaling' 
	    brightness = 10.;
	}
//        else 
	if (loglin == 0) {
//            write(op,100) '1 is a good default for logarithmic scaling' 
	} else {
//            write(op,100) '10 is a good default for linear scaling' 
	}
//          write(op,100) 'Re-enter. . .' 
//          goto 3 
//        endif 
    }

// Generate intensity data for the SAD pattern. 
    if (i_adapt == 1) {
	getsad_((D_fp)aglq16, &i_plane, &l_upper, &hk_lim, infile,
		ok, infile_len);
    } else {
	getsad_((D_fp)glq16, &i_plane, &l_upper, &hk_lim, infile, ok,
		 infile_len);
    }

    if (*ok) {
	wrtsad_(outfile, &i_plane, &l_upper, &hk_lim, ok, outfile_len);
    }

    return 0;
//  999 return 
//  990 write(op,102) 'Problems opening output file ', outfile 
//      write(op,103) 'IOSTAT = ', io_err 
//      ok = .false. 
//      return 
//  100 format(1x, a) 
//  101 format(1x, i3) 
//  102 format(1x, 2a) 
//  103 format(1x, a, i5) 
//  104 format(1x, g12.5) 
} // gosadp_ 



//  
// Title: GOSPEC 
// Author: MMJT 
// Date: 23 Oct 1989 
// Description: This subroutine sets up a file whose name is given 
// by 'outfile', which is derived from the input filename 'infile'. 
// The powder pattern data is then written to 'outfile', which is closed 
// by the subroutine 'WRTSPC'. 

//      ARGUMENTS: 
//            infile   -  name of input file (input) 
//            outfile  -  name of output file (output) 
//            ok       -  boolean flag indicating all went well. 
//                                                      (output). 

//      COMMON VARIABLES: 
//            uses:  blurring, CFile, GAUSS, LORENZ, NONE, PS_VGT 
//                   PV_GSS, PV_LRN, cntrl 

//        modifies:  full_brd 
//  

int gospec(boolean[] ok)
{
    // Local variables 
    double cut_off;

//      write(op,100) ' ' 
//      write(op,100) 'CALCULATING POWDER DIFFRACTION PATTERN. . .' 

// get angular range and step 
    *ok = rdrnge_();
    if (! (*ok)) {
	return 0;
    }
//      goto 999 
// create a new filename to save spectrum data to 
    getfnm_(infile, outfile, "spc", ok, infile_len, outfile_len, (ftnlen)3);
    if (! (*ok)) {
	return 0;
    }
//      goto 999 
//      if(sp.ne.op) open(unit = sp, file = outfile, status = 'new', 
//     |            err = 990, iostat = io_err) 
    full_brd = 1;
// 3456 write(op,100) 'Enter 1 for adaptive quadrature on broad peaks.' 
//      read(cntrl,*,err=3456) full_brd 
//      if(CFile) write(op,101) full_brd 
    if (full_brd == 1) {
	*ok = getspc_((D_fp)aglq16, infile, infile_len);
    } else {
	*ok = getspc_((D_fp)glq16, infile, infile_len);
    }
// suppress the huge peak near the origin if required 
    cut_off = 0.;
    if (*ok && th2_min == 0. && trim_origin) {
	*ok = trmspc_(&cut_off);
    }
    if (*ok) {
	if (blurring == gauss) {
//          write(op,104) 'Gaussian' 
	    gaussn_(&cut_off);
	} else if (blurring == lorenz) {
//          write(op,104) 'Lorentzian' 
	    lornzn_(&cut_off);
	} else if (blurring == ps_vgt) {
//          write(op,104) 'pseudo-Voigt' 
	    pv_(&cut_off);
	} else if (blurring == pv_gss) {
//          write(op,104) 'Gaussian' 
	    pv_(&cut_off);
	} else if (blurring == pv_lrn) {
//          write(op,104) 'Lorentzian' 
	    pv_(&cut_off);
	} else if (blurring != none) {
//          write(op,100) 
//     |         'Instrumental broadening type is undefined in GOSPEC.' 
	}
    }
    if (*ok) {
	wrtspc_(outfile, ok, outfile_len);
    }

    return 0;
//  999 return 
//  990 write(op,102) 'Problems opening output file ', outfile 
//      write(op,103) 'IOSTAT = ', io_err 
//      ok = .false. 
//      return 
//  100 format(1x, a) 
//  101 format(1x, i3) 
//  102 format(1x, 2a) 
//  103 format(1x, a, i5) 
//  104 format(1x, 'Adding ', a, ' instrumental broadening . . .') 
} // gospec_ 



//  
// Title: GOSTRK 
// Author: MMJT 
// Date: 23 Oct 1989 
// Description: This subroutine sets up a file whose name is given 
// by 'outfile', which is derived from the input filename 'infile'. 
// The streak intensity is then written to 'outfile', which is closed 
// by the subroutine 'STREAK'. 

//      ARGUMENTS: 
//            infile   -  name of input file. (input). 
//            outfile  -  name of output file. (output). 
//            ok       -  boolean flag indicating all went well. 
//                                                       (output). 

//      COMMON VARIABLES: 
//            uses:  cntrl, CFile 

//        modifies:  no COMMON variables are modified 
//  

int gostrk(boolean[] ok)
{
    // Local variables 
    int i;

    getfnm_(infile, outfile, "str", ok, infile_len, outfile_len, (ftnlen)3);
    if (! (*ok)) {
	return 0;
    }
//      goto 999 
//      if(sk.ne.op) open(unit = sk, file = outfile, status = 'new', 
//     |                              err = 990, iostat = io_err) 
// 4567 write(op,100) ' ' 
//      write(op,100) 'CALCULATING INTENSITY ALONG A STREAK. . .' 
//      write(op,100) 'Enter 1 for adaptive quadrature' 
//      read(cntrl,*,err=4567) i 
//      if(CFile) write(op,101) i 
// select integration function 
    if (i == 1) {
	streak_((D_fp)aglq16, outfile, ok, outfile_len);
    } else {
	streak_((D_fp)glq16, outfile, ok, outfile_len);
    }

    return 0;
//  999 return 
//  990 write(op,102) 'Problems opening output file ', outfile 
//      write(op,103) 'IOSTAT = ', io_err 
//      ok = .false. 
//      return 
//  100 format(1x, a) 
//  101 format(1x, i3) 
//  102 format(1x, 2a) 
//  103 format(1x, a, i5) 
} // gostrk_ 



//  
// Title: HKL_LIM 
// Author: MMJT 
// Date: 2 August 1989 
// Obtains upper limits of h, k, and l given the global variable 
// 'max_angle' (in radians). 
// The limits are returned in the global variables h_bnd, k_bnd and 
// l_bnd. HKL_LIM may need to decrease the value of lambda if h_bnd 
// and k_bnd are too small to allow adequate off-axis symmetry testing. 
// lambda is restored in OPTIMZ after symmetry testing. 

//      ARGUMENTS: 
//           No arguments are used. All data is in 'COMMON'. 

//      COMMON VARIABLES: 
//            uses:  a0, b0, c0, d0, lambda 

//        modifies:  h_bnd, k_bnd, l_bnd, lambda 
//  

int hkl_lim(void)
{
    // Local variables 

// HKLUP returns the maximum value of h, k or l given 'max_angle' 

// define upper h, k, l values consistent with max_angle 
    h_bnd = 0;
    while(h_bnd < 2 || k_bnd < 2) {
	h_bnd = (int) (2. * Math.sin(.5 * max_angle) / (
		lambda * Math.sqrt(a0)));
	k_bnd = (int) (2. * Math.sin(.5 * max_angle) / (
		lambda * Math.sqrt(b0)));
	l_bnd = (double) ((int) (2. * Math.sin(.5 *
		max_angle) / (lambda * Math.sqrt(c0)))
		);

// make sure bounds are not too small. This could occur for a small 
// unit cell, a small value of max_angle, or a long wavelength. 
	if (h_bnd < 2 || k_bnd < 2) {
	    lambda *= .5;
//        goto 1 
	}
    }

// Make sure bounds are not too large either 
    if (h_bnd > 10) {
	h_bnd = 10;
    }
    if (k_bnd > 10) {
	k_bnd = 10;
    }
    if (l_bnd > 10.) {
	l_bnd = 10.;
    }

    return 0;
} // hkl_lim 



//  
// Title: INTEGR 
// Author: MMJT and MWD 
// Date: 15 Feb 1990; 7 Mar 1995; 28th May 1996 
// Description:  This routine integrates intensity from 
//               h,k,l0 to h,k,l1. 

//      ARGUMENTS: 
//            FN   -  Function name passed by reference. The 
//                    choice is between GLQ16 (non-adaptive 
//                    Gauss-Legendre integration), and AGLQ16 
//                    (adaptive Gauss-Legendre integration). (input). 
//            ok   -  boolean flag indicating all went well. (output). 

//      COMMON VARIABLES: 
//            uses:   a0, b0, c0, d0, lambda, cntrl, CFile, xplcit, 
//                    X_RAY, rad_type, th2_max 

//        modifies:   no COMMON variables are modified 
//  

int integr_(D_fp fn, boolean *ok)
{
    // System generated locals 
    double d1, d2, d3, d4;

    // Local variables 
    int h, k;
    double x, l0, l1, q2, t1, tmp, sum;
    boolean loopdivided;
    double fact, d_th;
    double theta, l_tmp, max_th;
    boolean divided;
 
// statement functions 
// S is the value of 1/d**2 at hkl 
// ANGLE is the Bragg angle (in radians) of the h,k,l plane 
// LL is the maximum l value for a given h,k 
// W4 is the X-ray polarization factor 

    max_th = th2_max * .5;
    fact = 2. / lambda;
    q2 = fact * fact;
// set increment to a safe value, in case l1-l0 is too large 
    d_th = deg2rad * .001;

//   10 write(op,400) 'Enter h, k, l0, l1' 
//      read(cntrl,*,err=10)  h, k, l0, l1 
//      if(CFile) write(op,401) h, k, l0, l1 
// check values 
    if (l1 == l0) {
//        write(op,400) 'Illegal input: l1 equals l0' 
	if (cfile) {
	    l1 = l0 + d_th;
//          write(op,403) 'l1 is set to ', l1 
	} else {
// TODO          goto 10 
	}
    }
// make sure we are not going to blow up at the origin 
    if (h == 0 && k == 0 && rad_type == electn) {
	if (l0 * l1 <= 0.) {
//          write(op,400) 
//     |   'Cannot integrate across the origin for electron radiation' 
//          write(op,400) 'Re-enter. . .' 
// TODO          goto 10 
	}
    }
// Finally, check angles are meaningful 
    if (h * h * a0 + k * k * b0 + l0 * l0 * c0
	    + h * k * d0 > q2 || h * h * a0 + k * k *
	    b0 + l1 * l1 * c0 + h * k * d0 > q2)
	    {
//        if(S(h,k,l0).gt.Q2) write(op,402) h, k, l0, 
//     |            ' exceeds 180 degree scattering angle!' 
//        if(S(h,k,l1).gt.Q2) write(op,402) h, k, l1, 
//     |            ' exceeds 180 degree scattering angle!' 
// TODO        goto 10 
    }
// get angles corresponding to start and stop 
// check if we need to break the integration region into two parts 
// because h,k,l, and h,k,-l may subtend the same angle 
    divided = false;
    if (l0 <= 0. && l1 <= 0.) {
// use Friedel's law, and keep l +ve. Swap l0 and l1 
	h = -h;
	k = -k;
	tmp = -l0;
	l0 = -l1;
	l1 = tmp;
    } else if (l0 < 0. && l1 > 0.) {
	h = -h;
	k = -k;
	l_tmp = l1;
	l1 = -l0;
	l0 = 0.;
	divided = true;
    }
// swap if in reverse order 
    if (l0 > l1) {
	tmp = l0;
	l0 = l1;
	l1 = tmp;
    }

    sum = 0.;
    loopdivided = true;
    while(loopdivided) {
	loopdivided = false;
	max_th = Math.asin(.5 * lambda * Math.sqrt(h * h * a0 +
		k * k * b0 + l1 * l1 * c0 + h * k *
		d0));
	t1 = Math.asin(.5 * lambda * Math.sqrt(h * h * a0 + k * k
		* b0 + l0 * l0 * c0 + h * k * d0)
		);
	l1 = l0;
	xyphse_(&h, &k);
	pre_mat(&h, &k);
// integrate each d_th's range of reciprocal space 
	d1 = max_th - 1e-14;
	d2 = d_th;
	for (theta = t1; d2 < 0 ? theta >= d1 : theta <= d1; theta +=
		d2) {
	    l0 = l1;
// Computing MIN 
	    d3 = d_th, d4 = max_th - theta;
	    tmp = min(d3,d4);
	    d3 = theta + tmp;
	    l1 = Math.sqrt((q2 * Math.sin(d3) * Math.sin(d3) - h * h * a0 -
		    k * k * b0 - h * k * d0) /
		    c0);
	    x = (*fn)(&h, &k, &l0, &l1, ok);
	    if (! (*ok)) {
		return 0;
	    }
//        goto 999 
	    if (rad_type == x_ray) {
		d3 = theta + tmp * .5;
// Computing 2nd power 
		d4 = Math.cos(2. * d3);
		x *= .5 * (1. + d4 * d4);
	    }
	    sum += x;
// L40: 
	}
// do we need to integrate the other part? 
	if (divided) {
// goto -h,-k,-l and continue 
	    h = -h;
	    k = -k;
	    l0 = 0.;
	    l1 = l_tmp;
	    divided = false;
	    loopdivided = true;
//        goto 30 
	}
    }
//      write(op,403) 'Integrated intensity = ', sum 
    return 0;
//  999 return 
//  400 format(1x, a) 
//  401 format(1x, 2i3, 2g12.5) 
//  402 format(1x, 2i3, g12.5, a) 
//  403 format(1x, a, g15.8) 
} // integr_ 



//  
// Title: INTEN2 
// Author: MMJT 
// Date: 8 Apr 1992 
// Description: This routine determines the intensity at (h,k,l) 
// in reciprocal space, for an explicitly defined stacking sequence. 

//      ARGUMENTS: 
//            f   -  Layer form factors. (input). 
//            h   -  reciprocal lattice vector h-component. (input). 
//            k   -  reciprocal lattice vector k-component. (input). 
//            l   -  reciprocal lattice vector l-component. (input). 
//            ok  -  boolean flag indicating all went well. (output). 

//      COMMON VARIABLES: 
//            uses:  same_layer, l_seq, l_r, n_layers, l_phi, l_cnt 
//                   PI2 

//        modifies:  wavefn 

//      INTEN2 returns the intensity at h, k, l 
//  

double inten2_(double[] *f, int *h, int *k, double *l,
	boolean *ok)
{
    // System generated locals 
    int i1, i2, i3, i4;
    double ret_val, d1, d2;
    double[] z1, z2, z3, z4;

    // Local variables 
    int i, j, m;
    double[] z = new double[2], phi = new double[2][20][20];
    double dot, tmp;
    double[] z_to_n = new double[2];
    double twopi_l;

    // Parameter adjustments 
    --f;

    // Function Body 
    twopi_l = pi2 * *l;

// 'ok' is not used, but is included for compatibility with INTENS 
    *ok = true;

// Is there only one layer? If so, let's get it over with. 
    if (l_cnt == 1) {
	i1 = l_seq[0];
	wavefn[0] = f[0][i1], wavefn[1] = f[1][i1];
//        goto 900 
    } else {

// Check for an obvious optimization when all layers are identical. 
	if (same_layer) {
	    i = l_seq[0];
	    dot = pi2 * (*h * l_r[(i + i * 20) * 3
		    - 63] + *k * l_r[(i + i * 20) * 3 - 62] + *
		    l * l_r[(i + i * 20) * 3 - 61]);
	    d1 = Math.cos(dot);
	    d2 = Math.sin(dot);
	    z1[0] = d1, z1[1] = d2;
	    z[0] = z1[0], z[1] = z1[1];
	    tmp = dot / pi2;
// check we are not about to execute 0.0 / 0.0 
	    if ((d1 = tmp - i_dnnt(&tmp), Math.abs(d1)) <= 1e-5) {
		i1 = i;
		d1 = (double) l_cnt;
		z1[0] = d1 * f[0][i1], z1[1] = d1 * f[1][i1];
		wavefn[0] = z1[0], wavefn[1] = z1[1];
	    } else {
// sum the series 
		dot *= l_cnt;
		d1 = Math.cos(dot);
		d2 = Math.sin(dot);
		z1[0] = d1, z1[1] = d2;
		z_to_n[0] = z1[0], z_to_n[1] = z1[1];
		i1 = i;
		z3[0] = 1. - z_to_n[0], z3[1] = 0. - z_to_n[1];
		z2[0] = f[0][i1] * z3[0] - f[1][i1] * z3[1], z2[1] = f[0][
			i1] * z3[1] + f[1][i1] * z3[0];
		z4[0] = 1. - z[0], z4[1] = 0. - z[1];
		z_div(&z1, &z2, &z4);
		wavefn[0] = z1[0], wavefn[1] = z1[1];
	    }
	} else {

// Else do it the long way 
// Get phases 
	    i1 = n_layers;
	    for (i = 0; i < i1; i++) {
		i2 = n_layers;
		for (j = 0; j < i2; j++) {
		    dot = twopi_l * l_r[(j + i * 20) * 3 - 61];
		    i3 = j + i;
		    i4 = j + i;
		    d1 = Math.cos(dot);
		    d2 = Math.sin(dot);
		    z2[0] = d1, z2[1] = d2;
		    z1[0] = l_phi[0][i4] * z2[0] -
			    l_phi[1][i4] * z2[1], z1[1] =
			    l_phi[0][i4] * z2[1] +
			    l_phi[1][i4] * z2[0];
		    phi[0][i3] = z1[0], phi[1][i3] = z1[1];
// L20: 
		}
// L10: 
	    }
// Count down to the first layer (we know l_cnt is greater than 1) 
// Initialize wavefunction to the scattering factor of the last layer 
	    i1 = l_seq[l_cnt - 1];
	    wavefn[0] = f[0][i1], wavefn[1] = f[1][i1];
	    for (m = l_cnt - 1; m >= 1; --m) {
		i = l_seq[m - 1];
		j = l_seq[m];
		i1 = i;
		i2 = j + i;
		z2[0] = wavefn[0] * phi[0][i2] - wavefn[1] *
			 phi[1][i2], z2[1] = wavefn[0] * phi[1][i2]
			 + wavefn[1] * phi[0][i2];
		z1[0] = f[0][i1] + z2[0], z1[1] = f[1][i1] + z2[1];
		wavefn[0] = z1[0], wavefn[1] = z1[1];
// L30: 
	    }

// Normalize to the number of layers 
	}
    }
// L900: 
    d_cnjg(&z3, &wavefn);
    z2[0] = wavefn[0] * z3[0] - wavefn[1] * z3[1][0], z2[1] =
	     wavefn[0] * z3[1] + wavefn[1] * z3[0];
    d1 = (double) l_cnt;
    z1[0] = z2[0] / d1, z1[1] = z2[1] / d1;
    ret_val = z1[0];

    return ret_val;
} // inten2_ 



//  
// Title: INTENS 
// Author: MWD and MMJT 
// Date: 10 Feb 1989 
// Description: This routine determines the intensity at (h,k,l) 
// in reciprocal space, for recursive stacking. For this function 
// to be called, 'rcrsv' must be TRUE. 
// Note: The diffuse background is handled automatically by the 
// recursion algorithm. 

//      ARGUMENTS: 
//            f   -  Layer form factors. (input). 
//            h   -  reciprocal lattice vector h-component. (input). 
//            k   -  reciprocal lattice vector k-component. (input). 
//            l   -  reciprocal lattice vector l-component. (input). 
//            ok  -  boolean flag indicating all went well. (output). 

//      COMMON VARIABLES: 
//            uses:  only_real, l_g, n_layers, inf_thick 

//        modifies:  no COMMON variables are modified 

//      INTENS returns the intensity at h, k, l 
//  

double intens_(double[] *f, int *h, int *k, double *l,
	boolean *ok)
{
    // System generated locals 
    int i1, i2, i3;
    double ret_val;
    double[] z1 = new double[2], z2 = new double[2];

    // Local variables 
    int i;
    double[][] s = new double[2][20];
    double x, sum;

    // Parameter adjustments 
    --f;

    // Function Body 
    sum = 0.;
    get_mat(h, k, l);
    if (inf_thick) {
// initialize s to -f, since mat is -(1 - T) 
	i1 = n_layers;
	for (i = 0; i < i1; i++) {
	    i2 = i - 1;
	    i3 = i;
	    z1[0] = -f[0][i3], z1[1] = -f[1][i3];
	    s[0][i2] = z1[0], s[1][i2] = z1[1];
// L10: 
	}
	*ok = get_s(&f[1], s, h, k, l);
    } else {
// s is initialized inside GET_S2, from where GET_S is called 
	*ok = get_s2(&f[1], s, h, k, l);
    }
    if (*ok) {
// only use real part of f(i) if that's all that's there 
	if (only_real) {
	    i1 = n_layers;
	    for (i = 0; i < i1; i++) {
		i2 = i;
		i3 = i - 1;
		sum += l_g[i - 1] * f[0][i2] * s[0][i3];
// L20: 
	    }
	    sum *= 2.;
	    i1 = n_layers;
	    for (i = 0; i < i1; i++) {
		i2 = i;
		x = f[0][i2];
		sum -= l_g[i - 1] * x * x;
// L30: 
	    }
	} else {
// must use complex part of f(i) 
	    i1 = n_layers;
	    for (i = 0; i < i1; i++) {
		d_cnjg(&z2, &f[i]);
		i2 = i - 1;
		z1[0] = z2[0] * s[0][i2] - z2[1] * s[1][i2], z1[1] =
			z2[0] * s[1][i2] + z2[1] * s[0][i2];
		sum += l_g[i - 1] * z1[0];
// L40: 
	    }
	    sum *= 2.;
	    i1 = n_layers;
	    for (i = 0; i < i1; i++) {
		x = z_Math.abs(&f[i]);
		sum -= l_g[i - 1] * x * x;
// L50: 
	    }
	}
    }

    ret_val = sum;

    return ret_val;
} // intens_ 



//  
// Title: LAYCNT 
// Author: MMJT 
// Date: 3 Oct 1989 
// Description: Counts the number of int arguments in the character 
// string 'line', and returns the answer in LAYCNT. Legal separators 
// are, blanks, tabs, and commas. If LAYCNT encounters anything other 
// than a digit (0 - 9), or one of the three legal separators, LAYCNT 
// is set to -1. This routine is similar to CNTARG, accept it will only 
// accept ints as arguments. 

//      ARGUMENTS: 
//            line  -  Line of characters to be parsed. (input). 

//      LAYCNT returns the number of int arguments in the line. 
//      Returns -1 if an illegal character was detected. 
//  

int laycnt_(char *line, ftnlen line_len)
{
    // System generated locals 
    int ret_val, i1;

    // Local variables 
    int i, j;
    boolean legit, in_num;
    int lin_len, num_cnt;

    in_num = false;
    legit = true;
    num_cnt = 0;
    ret_val = -1;

    lin_len = i_len(line, line_len);
    i1 = lin_len;
    for (i = 0; i < i1; i++) {
	j = *(unsigned char *)&line[i - 1];
	if (j == 9 || j == 32 || j == 44) {
	    in_num = false;
	} else if (j >= 48 && j <= 57) {
	    if (! in_num) {
		in_num = true;
		++num_cnt;
	    }
	} else {
// illegal character 
	    legit = false;
	}
// L10: 
    }

    if (legit) {
	ret_val = num_cnt;
    }

    return ret_val;
} // laycnt_ 


//  
// Title: LENGTH 
// Author: MMJT 
// Date: 20 Nov 1989 
// Description:  This function returns the length of the first group 
// of contiguous, non-space characters in the passed string. If the 
// string has no blanks, the declared length of the string is returned. 

//      ARGUMENTS: 
//            string  -  Character string whose length is needed. 
//                                                            (input). 
//      LENGTH returns the string length. 
//  

int length_(char *string, ftnlen string_len)
{
    // System generated locals 
    int ret_val;

    // Local variables 
    int i;

    i = i_indx(string, " ", string_len, (ftnlen)1);
    if (i == 0) {
	ret_val = i_len(string, string_len);
    } else {
	ret_val = i - 1;
    }

    return ret_val;
} // length_ 


//  
// Title: LORNZN 
// Author: MMJT 
// Date: 17 Feb 1990; 7 Mar 1995 
// Description: This subroutine performs the Lorentzian 
// instrumental broadening. FWHM is in degrees. Does not conserve 
// intensity well when FWHM is comparable to d_theta. Data at the 
// extreme ends of the spectrum are corrupted slightly. 

//      ARGUMENTS: 
//            th2_low  -  lowest 2theta angle to consider. (input). 

//      COMMON VARIABLES: 
//            uses:  th2_max, d_theta, FWHM, spec, NONE, PI, RAD2DEG 
//                   blurring 

//        modifies:  brd_spc 
//  

int lornzn_(double *th2_low)
{
    // System generated locals 
    int i1, i2;
    double d1;

    // Local variables 
    int i, j;
    double k1, k2, k3, tmp, tmp1, tmp2;
    double lrnz;
    int n_low;
    double const;
    int n_high;

    if (fwhm <= 0.) {
	blurring = none;
	return 0;
//      goto 999 
    }
// check that cut-off is reasonable 
    if (*th2_low < 0. || *th2_low >= th2_max) {
//        write(op,101) 'LORNZN: Cut-off angle ', th2_low, 
//     |        ' is out of bounds. Angle reset to zero.' 
	*th2_low = 0.;
    }

// th2_low is the angle relative to th2_min 
// 2*d_theta is the angular step size 
    n_low = (int) (*th2_low * .5 / d_theta) + 1;
    n_high = (int) ((th2_max - th2_min) * .5 /
	    d_theta) + 1;

    const = rad2deg * 2. * d_theta;
    k1 = const * 2. / (pi * fwhm);
// Computing 2nd power 
    d1 = const * 2. / fwhm;
    k2 = d1 * d1;

    i1 = n_high;
    for (i = 0; i < i1; i++) {
	brd_spc[i - 1] = 0.;
// L10: 
    }

    i1 = n_high;
    for (i = 0; i <= i1; ++i) {
	k3 = k2 * i * i + 1.;
	lrnz = k1 / k3;
	i2 = n_high;
	for (j = n_low + 1; j <= i2; ++j) {
	    tmp1 = 0.;
	    tmp2 = 0.;
	    if (j - i > n_low) {
		tmp1 = spec[j - i - 1];
	    }
	    if (j + i <= n_high) {
		tmp2 = spec[j + i - 1];
	    }
	    tmp = tmp1 + tmp2;
	    if (i == 0) {
		tmp *= .5;
	    }
	    brd_spc[j - 1] += lrnz * tmp;
// L30: 
	}
// L20: 
    }
    return 0;
//  999 write(op,101) 'Illegal FWHM ', FWHM, ' in LORNZN()' 
//      write(op,100) 'Lorentzian instrumental broadening not added' 
// kill blurring option 
// TODO FOR ALL      blurring = NONE 
//      return 
//  100 format(1x, a) 
//  101 format(1x, a, g12.5, a) 
} // lornzn_ 



//  
// Title: LUBKSB 
// Author: MWD, adapted from 
// "Numerical Recipes: The Art of Scientific Computing." 
// Date: 18 Aug 1988 
//  Description: Solves the set of linear equations ax = b, 
//  where a, x and b contain real variables. 
//  Here, a is input as the LU-decomposition of a, determined by the 
//  routine LUDCMP. index is input as the permutation vector returned 
//  by LUDCMP. b is input as the right hand side vector, and returns 
//  with the solution x. a, n, MAX_N and index are not modified by this 
//  routine. In DIFFaX, LUDCMP and LUBKSB are used to solve for l_g(i), 
//  the a-priori probability that layer i exists within the crystal. 

//      ARGUMENTS: 
//            a      -  LU-decomposed square matrix of real numbers. 
//                                                           (input). 
//            b      -  vector of real numbers, the right hand side of 
//                      ax = b, is input. The solution x is output. 
//            index  -  vector containing the record of the row 
//                      permutation effected by the partial pivoting 
//                      carried out in CLUDCM (input). 
//            n      -  size of the square matrix. (input). 
//            MAX_N  -  physical dimension of a (MAX_N x MAX_N).(input). 
//  

int lubksb_(double *a, double *b, int *index,
	int *n, int *max_n)
{
    // System generated locals 
    int a_dim1, a_offset, i1, i2;

    // Local variables 
    int i, j, i2;
    double sum;
    int row;

    // Parameter adjustments 
    --index;
    --b;
    a_dim1 = *max_n;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    // Function Body 
    i2 = 0;
    i1 = *n;
    for (i = 0; i < i1; i++) {
	row = index[i];
	sum = b[row];
	b[row] = b[i];
	if (i2 != 0) {
	    i2 = i - 1;
	    for (j = i2; j <= i2; ++j) {
		sum -= a[i + j * a_dim1] * b[j];
// L10: 
	    }
	} else if (Math.abs(sum) != 0.) {
	    i2 = i;
	}
	b[i] = sum;
// L20: 
    }
    for (i = *n; i >= 1; --i) {
	sum = b[i];
	i1 = *n;
	for (j = i + 1; j <= i1; ++j) {
	    sum -= a[i + j * a_dim1] * b[j];
// L30: 
	}
	b[i] = sum / a[i + i * a_dim1];
// L40: 
    }
    return 0;
} // lubksb_ 


//  
// Title: LUDCMP 
// Author: MWD, adapted from 
// "Numerical Recipes: The Art of Scientific Computing." 
// Date: 18 Aug 1988 
//  Description: This is an LU decomposition routine, and accepts 
//  real*8 variables. 
//  Given an n x n matrix a, with physical dimension MAX_N, this 
//  routine replaces it by the LU decomposition of a rowwise permutation 
//  of itself. a and n are input. a is the LU decomposed output; index 
//  is an output vector which records the row permutation affected by 
//  the partial pivoting; Det is the determinant of a. This routine is 
//  used in combination with LUBKSB to solve linear equations. In DIFFaX, 
//  these routines are used to solve for l_g(i), the a-priori 
//  probability that layer i exists within the crystal. 
//  LUDCMP returns .false. if the matrix turns out to be singular. 

//      ARGUMENTS: 
//            a      -  Square matrix of real numbers to LU-decompose is 
//                      input. a is then replaced by the 
//                      LU-decomposed result. 
//            index  -  output vector which records the row permutation 
//                      affected by the partial pivoting. (output). 
//            n      -  size of the square matrix. (input). 
//            MAX_N  -  physical dimension of a (MAX_N x MAX_N).(input). 
//            Det    -  determinant of the matrix. (output). 

//      LUDCMP returns boolean .true. if all proceeded happily. 
//  

boolean ludcmp_(double *a, int *index, int *n, int *max_n,
	double *det)
{
    // System generated locals 
    int a_dim1, a_offset, i1, i2, i3;
    double d1, d2;
    boolean ret_val;

    // Local variables 
    int i, j, m;
    double max, tmp[100], sum;
    int row;
    double tmp2;

    // Parameter adjustments 
    --index;
    a_dim1 = *max_n;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    // Function Body 
    ret_val = false;
    *det = 1.;
    if (*n > 100) {
//        write(op,400) 'Matrix too large for LUDCMP' 
	return ret_val;
    }
    i1 = *n;
    for (i = 0; i < i1; i++) {
	max = 0.;
	i2 = *n;
	for (j = 0; j < i2; j++) {
	    if ((d1 = a[i + j * a_dim1], Math.abs(d1)) > max) {
		max = (d2 = a[i + j * a_dim1], Math.abs(d2));
	    }
// L10: 
	}
	if (max == 0.) {
	    return ret_val;
	}
//        goto 200 
	tmp[i - 1] = 1. / max;
// L20: 
    }
    i1 = *n;
    for (j = 1; j <= i1; ++j) {
	i2 = j - 1;
	for (i = 1; i <= i2; ++i) {
	    sum = a[i + j * a_dim1];
	    i3 = i - 1;
	    for (m = 1; m <= i3; ++m) {
		sum -= a[i + m * a_dim1] * a[m + j * a_dim1];
// L30: 
	    }
	    a[i + j * a_dim1] = sum;
// L40: 
	}
	max = 0.;
	i2 = *n;
	for (i = j; i <= i2; ++i) {
	    sum = a[i + j * a_dim1];
	    i3 = j - 1;
	    for (m = 1; m <= i3; ++m) {
		sum -= a[i + m * a_dim1] * a[m + j * a_dim1];
// L50: 
	    }
	    a[i + j * a_dim1] = sum;
	    tmp2 = tmp[i - 1] * Math.abs(sum);
	    if (Math.abs(tmp2) >= max) {
		row = i;
		max = tmp2;
	    }
// L60: 
	}
	if (j != row) {
	    i2 = *n;
	    for (m = 1; m <= i2; ++m) {
		tmp2 = a[row + m * a_dim1];
		a[row + m * a_dim1] = a[j + m * a_dim1];
		a[j + m * a_dim1] = tmp2;
// L70: 
	    }
	    *det = -(*det);
	    tmp[row - 1] = tmp[j - 1];
	}
	index[j] = row;
	if ((d1 = a[j + j * a_dim1], Math.abs(d1)) == 0.) {
	    a[j + j * a_dim1] = 1e-20;
	}
	tmp2 = 1. / a[j + j * a_dim1];
	i2 = *n;
	for (i = j + 1; i <= i2; ++i) {
	    a[i + j * a_dim1] *= tmp2;
// L80: 
	}
	*det *= a[j + j * a_dim1];
// L90: 
    }
    ret_val = true;
    return ret_val;
//  200 continue 
//      return 
//  400 format(1x, a) 
} // ludcmp_ 


//  
// Title: L_STEP 
// Author: MMJT 
// Date: 13 Aug 1989 
// Description:  L_STEP attempts to determine whether or not there 
// are any sharp peaks, and if so, their spacing. The algorithm inspects 
// the sum of certain permutations of the stacking vector z_components. 
// The components are chosen so that their values should be independent 
// of the layer origins chosen by the user. ie. Rz(i,i) and 
// Rz(i,j) + Rz(j,i) and Rz(i,j) + Rz(j,k) + Rz(k,i), etc... 
// In the interests of clarity, the permutations of the indices are 
// written out explicity, instead of being generated by a subroutine 
// (which would considerably shorten the code). We stop searching after 
// combinations of 4 stacking vectors, since then the structure is too 
// complicated to be attempting to trick DIFFaX into believing that we 
// have found sharp peaks. Under these circumstances, work out the 
// diffraction pattern the long, but reliable, way. 

//      ARGUMENTS: 
//            ok  -  boolean flag indicating all went well. (output). 

//      COMMON VARIABLES: 
//            uses:  n_layers, there, l_r 

//        modifies:  any_sharp 

//      L_STEP returns the l-increment that sharp peaks are likely to 
//      be found at. 
//  

double l_step(boolean *ok)
{
    // System generated locals 
    int i1, i2, i3, i4;
    double ret_val;

    // Local variables 
    boolean resonant;
    int i1, i2, i3, i4;
    double tmp;
    double z_step;
    boolean decided;

// initialize return value 
    ret_val = 0.;

    resonant = true;
    decided = false;
    z_step = 0.;

// Check z-components of Rii stacking vectors 
// if any of the transitions do not exist, set resonant to false. 
    i1 = n_layers;
    for (i1 = 1; i1 <= i1; ++i1) {
	if (resonant) {
	    if (there[i1 + i1]) {
		decided = true;
		tmp = l_r[(i1 + i1 * 20) * 3 - 61];
		resonant = resonant && yrdstk_(&z_step, &tmp, ok);
		if (! (*ok)) {
		    return ret_val;
		}
//            goto 990 
	    }
	}
// L10: 
    }

// Rii terms do not occur (ie. no layer i will stack to another layer i) 
// We must therefore check z-components of Rij + Rji sequences (i.ne.j). 
    if (n_layers > 1 && ! decided) {
	i1 = n_layers;
	for (i1 = 1; i1 <= i1; ++i1) {
	    i2 = n_layers;
	    for (i2 = i1 + 1; i2 <= i2; ++i2) {
		if (resonant) {
		    if (there[i2 + i1] && there[
			    i1 + i2]) {
			decided = true;
			tmp = l_r[(i2 + i1 * 20) * 3 - 61] +
				l_r[(i1 + i2 * 20) * 3 - 61];
			resonant = resonant && yrdstk_(&z_step, &tmp, ok);
			if (! (*ok)) {
			    return ret_val;
			}
//                goto 991 
		    }
		}
// L30: 
	    }
// L20: 
	}
    }

// No Rij + Rji sequences occur. 
// Check z-components of Rij + Rjk + Rki sequences (where i.ne.j.ne.k). 
    if (n_layers > 2 && ! decided) {
	i1 = n_layers;
	for (i1 = 1; i1 <= i1; ++i1) {
	    i2 = n_layers;
	    for (i2 = i1 + 1; i2 <= i2; ++i2) {
		i3 = n_layers;
		for (i3 = i2 + 1; i3 <= i3; ++i3) {
		    if (resonant) {
// There are 2 permutations 
			if (there[i2 + i1] &&
				there[i3 + i2] &&
				there[i1 + i3]) {
			    decided = true;
			    tmp = l_r[(i2 + i1 * 20) * 3 - 61] +
				    l_r[(i3 + i2 * 20) * 3 - 61] +
				    l_r[(i1 + i3 * 20) * 3 - 61];
			    resonant = resonant && yrdstk_(&z_step, &tmp,
				    ok);
			    if (! (*ok)) {
				return ret_val;
			    }
//                   goto 992 
			}
			if (there[i3 + i1] &&
				there[i2 + i3] &&
				there[i1 + i2] && resonant)
				 {
			    decided = true;
			    tmp = l_r[(i3 + i1 * 20) * 3 - 61] +
				    l_r[(i2 + i3 * 20) * 3 - 61] +
				    l_r[(i1 + i2 * 20) * 3 - 61];
			    resonant = resonant && yrdstk_(&z_step, &tmp,
				    ok);
			    if (! (*ok)) {
				return ret_val;
			    }
//                   goto 993 
			}
		    }
// L60: 
		}
// L50: 
	    }
// L40: 
	}
    }

// No Rij + Rjk + Rki sequences occur. 
// Check z-components of Rij + Rjk + Rkl + Rli sequences 
// (where i.ne.j.ne.k.ne.l). 
    if (n_layers > 3 && ! decided) {
	i1 = n_layers;
	for (i1 = 1; i1 <= i1; ++i1) {
	    i2 = n_layers;
	    for (i2 = i1 + 1; i2 <= i2; ++i2) {
		i3 = n_layers;
		for (i3 = i2 + 1; i3 <= i3; ++i3) {
		    i4 = n_layers;
		    for (i4 = i3 + 1; i4 <= i4; ++i4) {
			if (resonant) {
// There are 6 permutations 
			    if (there[i2 + i1] &&
				    there[i3 + i2] &&
				    there[i4 + i3] &&
				    there[i1 + i4]) {
				decided = true;
				tmp = l_r[(i2 + i1 * 20) * 3 - 61]
					+ l_r[(i3 + i2 * 20) * 3 -
					61] + l_r[(i4 + i3 * 20) *
					3 - 61] + l_r[(i1 + i4 *
					20) * 3 - 61];
				resonant = resonant && yrdstk_(&z_step, &
					tmp, ok);
				if (! (*ok)) {
				    return ret_val;
				}
//                       goto 994 
			    }
			    if (there[i2 + i1] &&
				    there[i4 + i2] &&
				    there[i3 + i4] &&
				    there[i1 + i3] &&
				    resonant) {
				decided = true;
				tmp = l_r[(i2 + i1 * 20) * 3 - 61]
					+ l_r[(i4 + i2 * 20) * 3 -
					61] + l_r[(i3 + i4 * 20) *
					3 - 61] + l_r[(i1 + i3 *
					20) * 3 - 61];
				resonant = resonant && yrdstk_(&z_step, &
					tmp, ok);
				if (! (*ok)) {
				    return ret_val;
				}
//                       goto 995 
			    }
			    if (there[i3 + i1] &&
				    there[i2 + i3] &&
				    there[i4 + i2] &&
				    there[i1 + i4] &&
				    resonant) {
				decided = true;
				tmp = l_r[(i3 + i1 * 20) * 3 - 61]
					+ l_r[(i2 + i3 * 20) * 3 -
					61] + l_r[(i4 + i2 * 20) *
					3 - 61] + l_r[(i1 + i4 *
					20) * 3 - 61];
				resonant = resonant && yrdstk_(&z_step, &
					tmp, ok);
				if (! (*ok)) {
				    return ret_val;
				}
//                       goto 996 
			    }
			    if (there[i3 + i1] &&
				    there[i4 + i3] &&
				    there[i2 + i4] &&
				    there[i1 + i2] &&
				    resonant) {
				decided = true;
				tmp = l_r[(i3 + i1 * 20) * 3 - 61]
					+ l_r[(i4 + i3 * 20) * 3 -
					61] + l_r[(i2 + i4 * 20) *
					3 - 61] + l_r[(i1 + i2 *
					20) * 3 - 61];
				resonant = resonant && yrdstk_(&z_step, &
					tmp, ok);
				if (! (*ok)) {
				    return ret_val;
				}
//                       goto 997 
			    }
			    if (there[i4 + i1] &&
				    there[i2 + i4] &&
				    there[i3 + i2] &&
				    there[i1 + i3] &&
				    resonant) {
				decided = true;
				tmp = l_r[(i4 + i1 * 20) * 3 - 61]
					+ l_r[(i2 + i4 * 20) * 3 -
					61] + l_r[(i3 + i2 * 20) *
					3 - 61] + l_r[(i1 + i3 *
					20) * 3 - 61];
				resonant = resonant && yrdstk_(&z_step, &
					tmp, ok);
				if (! (*ok)) {
				    return ret_val;
				}
//                       goto 998 
			    }
			    if (there[i4 + i1] &&
				    there[i3 + i4] &&
				    there[i2 + i3] &&
				    there[i1 + i2] &&
				    resonant) {
				decided = true;
				tmp = l_r[(i4 + i1 * 20) * 3 - 61]
					+ l_r[(i3 + i4 * 20) * 3 -
					61] + l_r[(i2 + i3 * 20) *
					3 - 61] + l_r[(i1 + i2 *
					20) * 3 - 61];
				resonant = resonant && yrdstk_(&z_step, &
					tmp, ok);
				if (! (*ok)) {
				    return ret_val;
				}
//                       goto 999 
			    }
			}
// L100: 
		    }
// L90: 
		}
// L80: 
	    }
// L70: 
	}
    }

// If there is no stacking sequence that can bring us back to a layer 
// similar to that at the origin after 4 layers, then this 
// structure is sufficiently complicated that we may be better 
// off doing adaptive integration anyway. (d_l still equals 0.0.) 

    if (decided && resonant && tmp != 0.) {
	ret_val = 1. / tmp;
	any_sharp = true;
    } else {
	ret_val = 0.;
	any_sharp = false;
    }

    return ret_val;
//  990 write(op,200) 
//      write(op,201) i1, i1 
//      return 
//  991 write(op,202) 
//      write(op,203) i1, i2, i2, i1 
//      return 
//  992 write(op,202) 
//      write(op,204) i1, i2, i2, i3, i3, i1 
//      return 
//  993 write(op,202) 
//      write(op,204) i1, i3, i3, i2, i2, i1 
//      return 
//  994 write(op,202) 
//      write(op,205) i1, i2, i2, i3, i3, i4, i4, i1 
//      return 
//  995 write(op,202) 
//      write(op,205) i1, i2, i2, i4, i4, i3, i3, i1 
//      return 
//  996 write(op,202) 
//      write(op,205) i1, i3, i3, i2, i2, i4, i4, i1 
//      return 
//  997 write(op,202) 
//      write(op,205) i1, i3, i3, i4, i4, i2, i2, i1 
//      return 
//  998 write(op,202) 
//      write(op,205) i1, i4, i4, i2, i2, i3, i3, i1 
//      return 
//  999 write(op,202) 
//      write(op,205) i1, i4, i4, i3, i3, i2, i2, i1 
//      return 
//  200 format(1x,'L_STEP: Non-physical z-component of stacking vector.') 
//  201 format(1x,'Rz(',i2,',',i2,') = 0.0') 
//  202 format(1x,'L_STEP:Non-physical z-components of stacking vectors') 
//  203 format(1x,'Rz(',i2,',',i2,') + Rz(',i2,',',i2,') = 0.0') 
//  204 format(1x,'Rz(',i2,',',i2,')',2(' + Rz(',i2,',',i2,')'),' = 0.0') 
//  205 format(1x,'Rz(',i2,',',i2,')',3(' + Rz(',i2,',',i2,')'),' = 0.0') 
} // l_step 



//  
// Title: MATMUL 
// Author: MMJT 
// Date: 5 Feb 1990 
// Description:  This subroutine multiplies the complex matrices 
// 'a' and 'b', of boolean size n x n. Result is returned in 'a'. 

//      ARGUMENTS: 
//            a   -  Complex*16 array to store result. (input and output). 
//            b   -  Complex*16 array. (input). 
//            n   -  Logical size of matrices. (input). 

//  

int matmul_(double[] *a, double[] *b, int *n)
{
    // System generated locals 
    int i1, i2, i3, i4, i5;
    double[] z1 = new double[2], z2 = new double[2];

    // Local variables 
    double[][][] c = new double[2][20][20];
    int i, j, m;
    double[] ctmp = new double[2];

// first copy a into c 
    // Parameter adjustments 
    b -= 21;
    a -= 21;

    // Function Body 
    i1 = *n;
    for (j = 1; j <= i1; ++j) {
	i2 = *n;
	for (i = 1; i <= i2; ++i) {
	    i3 = i + j;
	    i4 = i + j * 20;
	    c[0][i3] = a[0][i4], c[1][i3] = a[1][i4];
// L20: 
	}
// L10: 
    }

    i1 = *n;
    for (j = 1; j <= i1; ++j) {
	i2 = *n;
	for (i = 1; i <= i2; ++i) {
	    ctmp[0] = 0., ctmp[1] = 0.;
	    i3 = *n;
	    for (m = 1; m <= i3; ++m) {
		i4 = i + m;
		i5 = m + j * 20;
		z2[0] = c[0][i4] * b[0][i5] - c[1][i4] * b[1][i5],
			z2[1] = c[0][i4] * b[1][i5] + c[1][i4] * b[0][
			i5];
		z1[0] = ctmp[0] + z2[0], z1[1] = ctmp[1] + z2[1];
		ctmp[0] = z1[0], ctmp[1] = z1[1];
// L50: 
	    }
	    i3 = i + j * 20;
	    a[0][i3] = ctmp[0], a[1][i3] = ctmp[1];
// L40: 
	}
// L30: 
    }

    return 0;
} // matmul_ 


//  
// Title: MAT2N 
// Author: MMJT 
// Date: 5 Feb 1990 
// Description:  This function multiplies the matrix 'mat' by itself 
// n = l_cnt+1 times. In order to speed up the process, n has been broken 
// down into its binary representation (by the function BINPOW, which is 
// called once in OPTIMZ), and the result is given by 

//         mat**n = mat**n0 * mat**n1 * mat**n2 * mat**n3 * etc 

// where ni = 2**i 
// and n = n0 + n1 + n2 + n3 + . . . 

// mat**ni is given by (mat**n(i-1)) * (mat**n(i-1)) 
// ie. mat**8 = (mat**4) * (mat**4) 
// and similarly mat**4 = (mat**2) * (mat**2) 

// n must be such that n <= RCSV_MAX+1 <= 2**(MAX_BIN+1) - 1 

//      ARGUMENTS: 
//            a   -  Complex*16 array to store result. (output). 

//      COMMON VARIABLES: 
//            uses:  n_layers, pow, max_pow 

//        modifies:  No COMMON variable are modified. 

//      MAT2N returns TRUE if all went well. 
//  

boolean mat2n_(double[] *a)
{
    // System generated locals 
    int i1, i2, i3, i4;
    boolean ret_val;

    // Local variables 
    int i, j;
    double[][][][] tmp_mat = new double[2][20][20][10];

    // Parameter adjustments 
    a -= 21;

    // Function Body 
    ret_val = false;

// copy mat into the first 2-dimensional tmp_mat array. Initialize a 
// to be the identity matrix. 
    i1 = n_layers;
    for (j = 1; j <= i1; ++j) {
	i2 = n_layers;
	for (i = 1; i <= i2; ++i) {
	    i3 = i + j * 20;
	    a[0][i3] = 0., a[1][i3] = 0.;
	    i3 = i + (j + 20) * 20 - 421;
	    i4 = i + j;
	    tmp_mat[0][i3] = mat[0][i4], tmp_mat[1][i3] =
		    mat[1][i4];
// L30: 
	}
	i2 = j + j * 20;
	a[0][i2] = 1., a[1][i2] = 0.;
// L20: 
    }

    i1 = max_pow - 1;
    for (i = 0; i < i1; i++) {
	if (pow[i - 1] == 1) {
	    matmul_(&a[21], &tmp_mat[(i * 20 + 1) * 20 - 420], &
		    n_layers);
	}
	matsqr_(&tmp_mat[((i + 1) * 20 + 1) * 20 - 420], &tmp_mat[(i *
		 20 + 1) * 20 - 420], &n_layers);
// L40: 
    }
    if (pow[max_pow - 1] == 1) {
	matmul_(&a[21], &tmp_mat[(max_pow * 20 + 1) * 20 - 420],
		&n_layers);
    }

    ret_val = true;

    return ret_val;
} // mat2n_ 



//  
// Title: MATSQR 
// Author: MMJT 
// Date: 5 Feb 1990 
// Description:  This subroutine multiplies the complex matrix 
// 'b', of boolean size n x n, by itself. Result is returned in 'a'. 

//      ARGUMENTS: 
//            a   -  Complex*16 array to store result. (output). 
//            b   -  Complex*16 array to be 'squared'. (input). 
//            n   -  Logical size of matrices. (input). 

//  

int matsqr_(double[] *a, double[] *b, int *n)
{
    // System generated locals 
    int i1, i2, i3, i4, i5;
    double[] z1 = new double[2], z2 = new double[2];

    // Local variables 
    int i, j, m;
    double[] ctmp = new double[2];

    // Parameter adjustments 
    b -= 21;
    a -= 21;

    // Function Body 
    i1 = *n;
    for (j = 1; j <= i1; ++j) {
	i2 = *n;
	for (i = 1; i <= i2; ++i) {
	    ctmp[0] = 0., ctmp[1] = 0.;
	    i3 = *n;
	    for (m = 1; m <= i3; ++m) {
		i4 = i + m * 20;
		i5 = m + j * 20;
		z2[0] = b[0][i4] * b[0][i5] - b[1][i4] * b[1][i5],
			z2[1] = b[0][i4] * b[1][i5] + b[1][i4] * b[0][i5]
			;
		z1[0] = ctmp[0] + z2[0], z1[1] = ctmp[1] + z2[1];
		ctmp[0] = z1[0], ctmp[1] = z1[1];
// L30: 
	    }
	    i3 = i + j * 20;
	    a[0][i3] = ctmp[0], a[1][i3] = ctmp[1];
// L20: 
	}
// L10: 
    }

    return 0;
} // matsqr_ 


//  
// Title: NAMER 
// Author: MMJT 
// Date: 13 Oct 1988 
// Description: This subroutine creates the appropriate filenames for 
// the various files needed. 
// It generates the filename by reading the structure data file name. 
// It scans the name to see if a period ('.') is already within 
// the name (ie. if the data file is called 'Structure.dat'). It deletes 
// the characters after the period and appends whatever is in append. 
// 'name1' and 'name2' are assumed to be the same physical length. 
// This subroutine is called by GETFNM 

//      ARGUMENTS: 
//            name1   -  The name of the input data file. (input). 
//            name2   -  A derivative filename. (output). 
//            append  -  A token to be appended to name2. (input). 
//  

int namer_(char *name1, char *name2, char *append, ftnlen
	name1_len, ftnlen name2_len, ftnlen append_len)
{
    // System generated locals 
    int i1;

    // Local variables 
    int i, idot, namlen, applen;

// get length of the string holding the filename 
    namlen = i_len(name1, name1_len);
    s_copy(name2, " ", name2_len, (ftnlen)1);
    applen = length_(append, append_len);

    idot = i_indx(name1, ".", name1_len, (ftnlen)1);
    if (idot == 0) {
	idot = i_indx(name1, " ", name1_len, (ftnlen)1);
    }
    if (idot == 0) {
	if (namlen < 31) {
	    idot = namlen + 1;
	} else {
	    idot = 31;
	    namlen = 31;
	}
    }
// truncate root filename so that the appendage will appear correctly 
    if (idot + applen > namlen) {
	idot = namlen - applen;
    }

    i1 = idot - 1;
    for (i = 0; i < i1; i++) {
//        write(name2(i:i),'(a)') name1(i:i) 
// L10: 
    }

//      write(name2(idot:idot),'(a)') '.' 

    i1 = applen;
    for (i = 0; i < i1; i++) {
//        write(name2((idot+i):(idot+i)),'(a)') append(i:i) 
// L20: 
    }

    i1 = namlen;
    for (i = idot + applen + 1; i <= i1; ++i) {
//        write(name2(i:i),'(a)') ' ' 
// L30: 
    }

    return 0;
} // namer_ 


//  
// Title: NMCOOR 
// Author: MWD 
// Date: 18 Aug 1988 
// Description:  This subroutine multiplies the relative coordinates 
// of each atom by 2*pi, which is the useful form for calculating phases. 

//      ARGUMENTS: 
//            No input arguments. 

//      COMMON VARIABLES: 
//            uses:  n_actual, l_n_atoms, a_pos, PI2 

//        modifies:  a_pos 
//  

int nmcoor_(void)
{
    // System generated locals 
    int i1, i2;

    // Local variables 
    int i, j;

    i1 = n_actual;
    for (i = 0; i < i1; i++) {
	i2 = l_n_atoms[i - 1];
	for (j = 0; j < i2; j++) {
	    a_pos[(j + i * 200) * 3 - 603] *= pi2;
	    a_pos[(j + i * 200) * 3 - 602] *= pi2;
	    a_pos[(j + i * 200) * 3 - 601] *= pi2;
// L20: 
	}
// L10: 
    }

    return 0;
} // nmcoor_ 



//  
// Title: NXTARG 
// Author: MMJT 
// Date: 21 JUL 1997 
// Description: Removes the next argument from the character string 
// 'line', and returns that argument in the character string 'arg'. 

//      ARGUMENTS: 
//            line  -  Line of characters to be parsed. (input). 
//            arg   -  First argument found in 'line'.  (output). 

//      Returns  1 if an argument was found. 
//      Returns  0 if no argument was found. 
//      Returns <0 if an error occurred. 
//  

int nxtarg_(char *line, char *arg, ftnlen line_len, ftnlen arg_len)
{
    // System generated locals 
    int ret_val;

    // Local variables 
    int i, j, i1, i2, lin_len;

    ret_val = 0;

    lin_len = i_len(line, line_len);

    i = 0;
    j = 32;
// Strip leading blanks 
    while(j == 9 || j == 32 || j == 44) {
	++i;
	if (i > lin_len) {
	    ret_val = -3;
	    return ret_val;
	}
//        goto 997 
	j = *(unsigned char *)&line[i - 1];
    }
    i1 = i;

// Read off the non-blank characters 
    while(i < lin_len && j != 9 && j != 32 && j != 44) {
	++i;
	j = *(unsigned char *)&line[i - 1];
    }
    ret_val = 1;
//   30 continue 

    i2 = i;
    if (i2 < i1) {
	ret_val = -2;
	return ret_val;
    }
//        goto 998 
//      write(arg, '(a)', err=999) line(i1:i2) 
//      write(tmpline, '(a)', err=999) line(i2:lin_len) 
//      write(line, '(a)', err=999) tmpline(1:lin_len) 

    return ret_val;
//  997 NXTARG = -3 
//      return 
//  998 NXTARG = -2 
//      return 
//  999 NXTARG = -1 
//      return 
} // nxtarg_ 


//  
// Title: OPTIMZ 
// Author: MWD and MMJT 
// Date: 8 Apr 1992; 15 Mar 1995; 24 July 1997 
// Description:  This routine determines if any shortcuts can be taken 
// in the calculations. 

//      ARGUMENTS: 
//            rootnam  -  The name of the input data file. (input). 
//            ok       -  boolean flag indicating all went well. 
//                                                      (output). 

//      COMMON VARIABLES: 
//            uses:  a0, b0, d0, n_layers, l_alpha, l_actual, l_n_atoms, 
//                   a_B, r_B11, r_B22, r_B33, r_B12, r_B23, r_B31, 
//                   l_symmetry, l_seq, DoSymDump, SymGrpNo, lambda, 
//                   l_cnt, CENTRO, PI, l_r, n_actual, finite_width 

//        modifies:  there, one_B, Bs_zero, same_Bs, only_real, l_rz, 
//                   same_layer, max_var, no_trials, tolerance, SymGrpNo, 
//                   lambda, check_sym, has_l_mirror, theta1, theta2 
//                   h_end, h_start, k_end, k_start, pnt_grp, same_rz 
//                   xplcit, formfactor, all_Bs_zero, 
//                   a_B11,a_B22,a_B33,a_B12,a_B23,a_B31 
//  

void optimz(boolean[] ok)
{
    // Local variables 
    String sym_fnam;
    int i, j, m, n;
    double x, z;
    int j2;
    double old_lambda, tmp;
    double incr, error;
    boolean did_it[MAX_L][MAX_L];

// statement function 
// HKANGL is the angle between the vector (h_val,k_val,0) and (1,0,0) 

// set up logic table for stacking transitions 
    for (i = 0; i < n_layers; i++)
	  for (j = 0; j < n_layers; j++)
	    there[j][i] = l_alpha[j][i]  >= eps7;

// see if there are any overlapping atoms 
    overlp();

// multiply all atom coordinates by 2*PI 
    nmcoor();

// If calculation is to be recursive for a finite number of layers, 
// then store the binary form of l_cnt+1 in an array for efficient 
// matrix multiplication. 
    if (recrsv && !inf_thick) {
		ok[0] = binpow(l_cnt + 1);
	  if (!ok[0]) {
		System.out.println("ERROR returned by BINPOW to OPTIMZ");
		System.out.println("The argument passed was l_cnt+1 = " + l_cnt+1);
	    return;
	  }
    }

// see if Debye-Waller coefficients are same for all atoms in a layer 
    for (i = 0; i < n_layers; i++) {
	x = ZERO;
	j2 = l_actual[i];
	m = l_n_atoms[j2];
	i2 = m;
	for (j = 0; j < m; j++)
	    x += a_b[j][j2];
	x /= m;
	error = ZERO;
// find absolute deviation of coefficients 
	for (j = 0; j < m; j++)
	    error += Math.abs(a_b[j][j2] - x);
// get relative error 
	if (x != ZERO)
	    error /= x * m;
	one_b[j2] = Math.abs(error) <= eps3;
    }

// Check that the layer uncertainty factors are physically reasonable. 
    for (i = 0; i < n_layers; i++) {
	for (j = 0; j < n_layers; j++) {
	    if (there[j][i]) {
// check on r_B12 
		x = r_b11[j][i] * r_b22[j][i] * a0 * b0 - r_b12[j][i] * r_b12[j][i] * ab0 * ab0;
		if (x < ZERO) {
			System.out.println("ERROR: Non-physical interlayer uncertainty factor C12");
			System.out.println("       for stacking from layer " + i + " to " + j);
			System.out.println("       It is too large relative to C11 and C22");
		    ok[0] = false;
		    return;
		}
// check on r_B23 
		x = r_b22[j][i] * r_b33[j][i] * b0 * c0 - r_b23[j][i] * r_b23[j][i] * bc0 * bc0;
		if (x < ZERO) {
			System.out.println("ERROR: Non-physical interlayer uncertainty factor C23");
			System.out.println("       for stacking from layer " + i + " to " + j);
			System.out.println("       It is too large relative to C22 and C33");
		    ok[0] = false;
		    return;
		}
// check on r_B31 
		x = r_b11[j][i] * r_b33[j][i] * a0 * c0 - r_b31[j][i] * r_b31[j][i] * ca0 * ca0;
		if (x < ZERO) {
			System.out.println("ERROR: Non-physical interlayer uncertainty factor C13");
			System.out.println("       for stacking from layer " + i + " to " + j);
			System.out.println("       It is too large relative to C11 and C33");
		    ok[0] = false;
		    return;
		}
	    }
	}
    }

// see if stacking 'uncertainty' coefficients are same for all layers. 
// Special flag if they are all zero. 
    all_bs_zero = true;
    for (i = 0; i < n_layers; i++) {
	for (j = 0; j < n_layers; j++) {
	    bs_zero[j][i] = r_b11[j][i] == ZERO && r_b22[j][i] == ZERO && 
		                r_b33[j][i] == ZERO && r_b12[j][i] == ZERO &&
		                r_b23[j][i] == ZERO && r_b31[j][i] == ZERO;
	    all_bs_zero = all_bs_zero && bs_zero[j][i];
	}
    }

// run through all 6 coefficients 
// until it is clear that some are different 
    same_bs = equalb(r_b11, a_b11);
    if (same_bs) 
		same_bs = equalb(r_b22, a_b22);
    if (same_bs) 
		same_bs = equalb(r_b33, a_b33);
    if (same_bs) 
		same_bs = equalb(r_b12, a_b12);
    if (same_bs) 
		same_bs = equalb(r_b23, a_b23);
    if (same_bs) 
		same_bs = equalb(r_b31, a_b31);

// see if all layers are centrosymmetric 
    only_real = true;
    for (i = 0; i < n_actual; i++)
		only_real = only_real && l_symmetry[i] == centro;

// see if all z-components of the stacking vectors are the same 
    l_rz = ZERO;
    n = 0;
    for (i = 0; i < n_layers; i++)
	for (j = 0; j < n_layers; j++)
	    if (there[j][i]) {
		l_rz += l_r[2][j][i];
		++n;
	    }
    l_rz /= n;
    error = ZERO;
    for (i = 0; i < n_layers; i++)
		for (j = 0; j < n_layers; j++)
			if (there[j + i])
				error += Math.abs(l_r[2][j][i] - l_rz);
    same_rz = Math.abs(error) <= eps4;

// If the stacking is explicit, check to see if all the layers are 
// the same 
    same_layer = false;
    if (xplcit) {
	if (l_cnt != 1) {
	    same_layer = true;
	    j = l_seq[0];
	    i = 1;
	    while(same_layer && i < l_cnt)
			if (!l_seq[i++] == j)
				same_layer = false;

// Check if any of the layer transitions have non-zero probability 
// initialize flags so that we do not produce duplicate error messages 
	    for (i = 0; i < n_layers; i++)
		for (j = 0; j < n_layers; j++)
		    did_it[j][i] = false;

// now check for legal transitions 
		System.out.println("Checking for conflicts in layer stackings . . .");
	    for (n = 0; n < l_cnt - 1; n++) {
		i = l_seq[n];
		j = l_seq[n + 1];
		if (!there[j][i]) {
		    ok[0] = false;
		    if (!did_it[j][i]) {
			did_it[j][i] = true;
				System.out.println("ERROR: Layer " + j + " cannot stack after layer " + i);
		    }
		}
	    }
	}
    }
    if (!ok[0])
		return;

// Pre-compute a pseudo-Lorentzian form factor for lateral 
// planar widths. 

    if (finite_width) {
// ideally, FFACT_SIZE is odd 
	m = FFACT_SIZE/2;
// incr contains the correct increment size such that the first and 
// last array elements contain zero. 
	incr = (THREE*N_SIGMAS*N_SIGMAS + ONE) / (TWO*N_SIGMAS*m);
	ffact_scale = incr;
	z = ONE + N_SIGMAS*N_SIGMAS;
	tmp = ONE / (z * z);
// put the peak in the middle (if FFACT_SIZE is odd) 
	formfactor[m] = ONE;
	for (n = 0; n < m; n++) {
	    z = (1.0 + n) * incr;
	    if (z <= N_SIGMAS) {
// Lorentzian part 
		x = ONE / (z * z + ONE);
	    } else {
// Linear part 
		x = tmp*(THREE*N_SIGMAS*N_SIGMAS + ONE - TWO*N_SIGMAS*z);
	    }
	    if (m + n + 2 <= FFACT_SIZE) {
		formfactor[m + n] = x;
	    }
	    if (m - n + 2 > 0) {
		formfactor[m - n] = x;
	    }
	}
// compute half width in reciprocal Angstroms for use in computing the 
// subtle impact of a-b shape broadening on 00l reflections 
	tmp = wa * Math.sin(pi - cell_gamma);
	ffwdth = Math.sqrt(ONE / (tmp * tmp) + ONE / (wb * wb));
    }

// get diffraction symmetry 

// establish some bounds 
    no_trials = 25;
    max_var = ZERO;
// save lambda, HKL_LIM (called by THRESH), may change it. 
    old_lambda = lambda;

    thresh(ok);
    if (!ok[0])
		return;

	if (SymGrpNo == 11) 
		  System.out.println("Axial integration only selected.");
      else 
		  System.out.println("Evaluating point group symmetry of diffraction data. . .");

    if (dosymdump) {
//	getfnm_(rootnam, sym_fnam, "sym", ok, rootnam_len, (ftnlen)31, (
//		ftnlen)3);
//	if (! (*ok)) {
//          write(op,202) 'OPTIMZ: ERROR in creating symmetry dumpfile.' 
//	    return 0;
//          goto 999 
//	}
//        if(sy.ne.op) 
//     |      open(unit = sy, file = sym_fnam, status = 'new') 
//        write(op,204) 'Writing symmetry data to file ''', 
//     |                 sym_fnam(1:LENGTH(sym_fnam)),'''. . .' 
//        write(sy,303) '''', rootnam(1:LENGTH(rootnam)),'''' 
		System.out.println("SYMMETRY EVALUATIONS FOR DATA");
		System.out.println("Number of trials per symmetry element = " + no_trials);
		System.out.println("Threshold intensity = " + tiny_inty);
    }

    check_sym = false;
    if (symgrpno == UNKNOWN) {
	symgrpno = get_sym(ok);
	if (!ok[0])
	    return;
		System.out.println("Diffraction point symmetry is " + pnt_grp);
        if(symgrpno != 1) 
			System.out.println("  to within a tolerance of one part in " + (int)(ONE / tolerance));
    } else {
	check_sym = true;
	chk_sym(ok);
	if (!ok[0])
	    return;
    }

// restore lambda. 
    lambda = old_lambda;

    has_l_mirror = symgrpno != 1 && symgrpno != 3 && 
		           symgrpno != 5 && symgrpno != 6 &&
	               symgrpno != 11;

	if(dosymdump) { 
		System.out.println();
		System.out.println("The diffraction data fits the point group symmetry " + pnt_grp);
        if(symgrpno != 1 && symgrpno != 11) { 
          if(max_var > eps6 && max_var <= eps1) 
			  System.out.println("  with a tolerance of one part in " + (int)(ONE / max_var));
          else if(max_var > eps1) 
			  System.out.println("  with a tolerance of one part in " + ONE / max_var);
          else 
			  System.out.println("  with a tolerance better than one part in a million.");
 			  } else {
				  System.out.println("By definition, all diffraction data has a center of symmetry");
				  System.out.println("thus, there is no need to test for inversion.");
			  } 
// close file, unless the output was to the default device 
//        if(sy.ne.op) close (unit = sy) 
	} 
// establish integration limits and weighting factors 
    get_bds();
// compute angles of scanning vectors relative to 1 0 0 
    theta1 = Math.atan2(k_start * Math.sqrt(a0 * b0 - d0 * d0 * QUARTER),
						h_start * a0 + k_start * d0 * HALF);
    theta2 = Math.atan2(k_end * Math.sqrt(a0 * b0 - d0 * d0 * QUARTER), 
						h_end * a0 + k_end * d0 * HALF);
// resolve ambiguity in the case of -1 point symmetry 
    if (symgrpno == 1 || symgrpno == 11) {
	theta1 = -pi;
	theta2 = pi;
    }
    return;
} 



//  
// Title: OVERLP 
// Authors: MMJT 
// Date: 24 Feb 1990 
// Description: This function compares the coordinates of atoms in 
// adjacent layers, and searches for any overlap. If it finds any 
// overlap, it checks the summed occupancy to check that it does 
// not exceed 1. If OVERLP finds atoms too close it provides a warning 
// message only. 

//      ARGUMENTS: 
//            No arguments are passed. 

//      COMMON VARIABLES: 
//            uses:  n_layers, there, l_n_atoms, l_r, a_pos 

//        modifies:  No COMMON variables are modified 
//  

int overlp_(void)
{
    // System generated locals 
    int i1, i2, i3;
    double d1, d2, d3;

    // Local variables 
    int i, j, m, n, j2;
    double x1, y1, z1, x2, y2, z2;
    int nn;
    double lay[3][400], tmp;
    char txt[33];
    int fact;
    int err_no, at_num;
    boolean invert;
    double sum_occ;

//      write(op,400) 'Checking for conflicts in atom positions . . .' 

    err_no = 0;
    i1 = n_layers;
    for (i = 0; i < i1; i++) {
	fact = 1;
	invert = l_symmetry[i - 1] == centro;
	if (invert) {
	    fact = 2;
	}
	at_num = l_n_atoms[i - 1];
	i2 = at_num;
	for (j2 = 1; j2 <= i2; ++j2) {
	    lay[j2 * 3 - 3] = bounds_(&a_pos[(j2 + i * 200) * 3
		    - 603]);
	    lay[j2 * 3 - 2] = bounds_(&a_pos[(j2 + i * 200) * 3
		    - 602]);
// Remember, only the a and b directions are truly periodic. 
// The scaling along c is arbitrary. Furthermore, c is perpendicular 
// to a and b, and is not necessarily parallel to Rii, which (if it 
// exists) would define the third cell-repeat direction. In other words, 
// for the general case, we cannot use the BOUNDS function along c. 
	    lay[j2 * 3 - 1] = a_pos[(j2 + i * 200) * 3 - 601];
// L20: 
	}
	if (invert) {
	    i2 = at_num;
	    for (j2 = 1; j2 <= i2; ++j2) {
		d1 = -a_pos[(j2 + i * 200) * 3 - 603];
		lay[(at_num + j2) * 3 - 3] = bounds_(&d1);
		d1 = -a_pos[(j2 + i * 200) * 3 - 602];
		lay[(at_num + j2) * 3 - 2] = bounds_(&d1);
		lay[(at_num + j2) * 3 - 1] = -a_pos[(j2 + i *
			200) * 3 - 601];
// L30: 
	    }
	}
	i2 = at_num;
	for (m = 1; m <= i2; ++m) {
	    x1 = lay[m * 3 - 3];
	    y1 = lay[m * 3 - 2];
	    z1 = lay[m * 3 - 1];
	    i3 = fact * at_num;
	    for (n = m + 1; n <= i3; ++n) {
		if (n > at_num) {
		    nn = n - at_num;
		} else {
		    nn = n;
		}
		x2 = lay[n * 3 - 3];
		y2 = lay[n * 3 - 2];
		z2 = lay[n * 3 - 1];
		if ((d1 = x1 - x2, Math.abs(d1)) * cell_a <= .1 && (
			d2 = y1 - y2, Math.abs(d2)) * cell_b > .1 &&
			 (d3 = z1 - z2, Math.abs(d3)) * cell_c <= .1) {
		    sum_occ = a_occup[nn + i * 200 - 201] +
			    a_occup[m + i * 200 - 201];
		    if (sum_occ - 1. > 1e-4) {
			if (n <= at_num) {
			    s_copy(txt, "are too close in layer", (ftnlen)33,
				    (ftnlen)22);
			} else {
			    s_copy(txt, "(inverted) are too close in layer", (
				    ftnlen)33, (ftnlen)33);
			}
//                write(op,410) 'AtomSite ', a_name(nn,i), a_number(nn,i),
//     |                   ' and atom ', a_name(m,i), a_number(m,i) 
//                write(op,412) txt(1:PRUNE(txt)), i 
//                write(op,420) 'Their combined occupancy is ', sum_occ 
			++err_no;
			if (err_no > 100) {
			    return 0;
			}
//                goto 999 
		    }
		}
// L50: 
	    }
// L40: 
	}
// L10: 
    }

// now let's look at i-j layer transitions and generate a simple warning 
// message if it seems that the layers are intertwined. 
    i1 = n_layers;
    for (i = 0; i < i1; i++) {
	i2 = n_layers;
	for (j = 0; j < i2; j++) {
	    if (there[j + i] && i != j) {
		tmp = l_r[(j + i * 20) * 3 - 61] +
			low_atom[l_actual[j - 1] - 1] -
			high_atom[l_actual[i - 1] - 1];
		if (tmp * cell_c <= -.1) {
//              write(op,430) 'Atoms from layer', j, 
//     |                    ' extend into layer', i 
		}
	    }
// L70: 
	}
// L60: 
    }

    return 0;
//  999 write(op,401) 'WARNING: Number of errors exceeds ', max_err 
//      return 
//  400 format(1x, a) 
//  401 format(1x, a, i5) 
//  410 format(1x, 'WARNING: ', 2(2a, ' (number ', i3, ')' ) ) 
//  412 format(10x, a, 1x, i2) 
//  420 format(1x, a, g12.5) 
//  430 format(1x, 'WARNING: ', 2(a, i3)) 
} // overlp_ 




//  
// Title: PARENT 
// Author: MMJT 
// Date: 10 Aug 1989 
// Description: This routine detects whether or not a character 
// string contains any parentheses, '(' and ')'. If it does, they are 
// removed. PARENT is used by RDPRMS to detect whether or not the 
// anisotropic temperature factors are being used. 

//      ARGUMENTS: 
//            line  -  Line of characters to parse. (input). 
//                  -  Line stripped of any parentheses. (output). 

//      PARENT returns 0 if no parentheses were found, 1 if a balanced 
//      pair of parentheses were found, and -1 if there was an error. 
//  

int parent_(char *line, ftnlen line_len)
{
    // System generated locals 
    int ret_val;

    // Local variables 
    int i, j;

    ret_val = 0;
    i = i_indx(line, "(", line_len, (ftnlen)1);
    j = i_indx(line, ")", line_len, (ftnlen)1);
// no parentheses 
    if (i == 0 && j == 0) {
	return ret_val;
    }
//      goto 200 
// right hand parenthesis occurred before the left hand parenthesis! 
    if (j < i) {
	ret_val = -1;
	return ret_val;
//      goto 999 
    }
// only one parenthesis 
    if (i != j && (i == 0 || j == 0)) {
	ret_val = -1;
	return ret_val;
//      goto 999 
    }
// blot out the two parentheses found 
//      write(line(i:i),'(a)') ' ' 
//      write(line(j:j),'(a)') ' ' 
// check there are not any more 
    i = i_indx(line, "(", line_len, (ftnlen)1);
    j = i_indx(line, ")", line_len, (ftnlen)1);
    if (i == 0 && j == 0) {
// there were no more parentheses. Data is probably ok. 
	ret_val = 1;
    } else {
// there were more parentheses. Data is suspect. 
	ret_val = -1;
	return ret_val;
//      goto 999 
    }

    return ret_val;
//  999 PARENT = -1 
//      return 
} // parent_ 


//  
// Title: PNTINT 
// Author: MMJT 
// Date: 30 July 1989 
// Gets the intensity at the point h, k, l. This differs from the 
// subroutine POINT only in that PNTINT does not interact with the user. 

//      ARGUMENTS: 
//            h   -  reciprocal vector h-component. (input). 
//            k   -  reciprocal vector k-component. (input). 
//            l   -  reciprocal lattice vector l-component. (input). 
//            ok  -  boolean flag indicating all went well. (output). 

//      COMMON VARIABLES: 
//            uses:  a0, b0, c0, d0, lambda, recrsv, rad_type, X_RAY 

//        modifies:  no COMMON variables are modified 

//      PNTINT returns the intensity at h, k, l 
//  

double pntint_(int *h, int *k, double *l, boolean *ok)
{
    // System generated locals 
    double ret_val, d1, d2;

    // Local variables 
    double[][] f = new double[2][20];
    double x;

// statement functions 
// S is the value of 1/d**2 at hkl 
// ANGLE is the Bragg angle (in radians) of the h,k,l plane 
// W4 is the X-ray polarization factor 

    xyphse_(h, k);
    pre_mat(h, k);
    d1 = *h * *h * a0 + *k * *k * b0 + *l * *l *
	    c0 + *h * *k * d0;
    get_f(f, &d1, l);
    if (recrsv) {
	x = intens_(f, h, k, l, ok);
    } else {
	x = inten2_(f, h, k, l, ok);
    }
    if (! (*ok)) {
	if (recrsv) {
//          write(op,200) 'INTENS' 
	} else {
//          write(op,200) 'INTEN2' 
	}
//        write(op,201) 'h,k,l = ', h,',', k,',', l 
    }
    if (rad_type == x_ray) {
	d1 = Math.asin(.5 * lambda * Math.sqrt(*h * *h * a0 + *
		k * *k * b0 + *l * *l * c0 + *h * *k *
		d0));
// Computing 2nd power 
	d2 = Math.cos(2. * d1);
	x *= .5 * (1. + d2 * d2);
    }

    ret_val = x;

    return ret_val;
//  200 format(1x, 'ERROR returned from ', a, ' in subroutine PNTINT') 
//  201 format(1x, 2(a, i3), a, g12.5) 
} // pntint_ 



//  
// Title: POINT 
// Author: MWD and MMJT 
// Date: 18 Aug 1988 
// Description:  This subroutine prompts the user for h, k, l values 
// and displays the intensity at that point. 

//      ARGUMENTS: 
//            ok  -  boolean flag indicating all went well. (output). 

//      COMMON VARIABLES: 
//            uses:  a0, b0, c0, d0, lambda, cntrl, CFile, xplcit, 
//                   recrsv, rad_type, n_layers, inf_thick, wavefn 

//        modifies:  no COMMON variables are modified 
//  

int point(boolean[] ok)
{
    // System generated locals 
    int i1, i2, i3;
    double d1, d2;
    double[] z1 = new double[2];

    // Local variables 
    double[][] f = new double[2][20];
    int h, i, k;
    double l;
    double[][] s = new double[2][20];
    double x, q2;
    double shkl;

// statement functions 
// SS is the value of 1/d**2 at hkl 
// ANGLE is the Bragg angle (in radians) of the h,k,l plane 
// W4 is the X-ray polarization factor 

//    1 write(op,400) ' ' 
//      write(op,400) 'CALCULATING INTENSITY AT A POINT. . .' 
//   10 write(op,400) 'Enter h, k, l' 
//      read(cntrl,*,err=10)  h, k, l 
//      if(CFile) write(op,401) h, k, l 
// check angles are meaningful 
// Computing 2nd power 
    d1 = lambda;
    q2 = 4. / (d1 * d1);
    shkl = h * h * a0 + k * k * b0 + l * l *
	    c0 + h * k * d0;
// Check that SS(h,k,l) is legal 
    if (shkl < 0.) {
//        write(op,403) 'ERROR: In POINT(), 1/d**2 = ', Shkl 
	*ok = false;
	return 0;
//        goto 999 
    }
    if (shkl > q2) {
//        write(op,402) h, k, l,' exceeds 180 degree scattering angle!' 
//        write(op,400) 'Re-enter. . .' 
//        goto 10 
    }
// make sure we are not going to blow up at the origin 
    if (h == 0 && k == 0 && rad_type == electn) {
	if (shkl <= 1e-4) {
//          write(op,400) 
//     |   'Cannot integrate across the origin for electron radiation' 
//          write(op,400) 'Re-enter. . .' 
//          goto 10 
	}
    }
// get intensity 
    xyphse_(&h, &k);
    pre_mat(&h, &k);
    get_f(f, &shkl, &l);
    if (recrsv) {
	x = intens_(f, &h, &k, &l, ok);
//        if(.not.ok) goto 999 
// set up mat again to re-call GET_S (or GET_S2) 
	pre_mat(&h, &k);
	get_mat(&h, &k, &l);
	if (inf_thick) {
// initialize s to -f, since mat is -(ident - T) 
	    i1 = n_layers;
	    for (i = 0; i < i1; i++) {
		i2 = i - 1;
		i3 = i - 1;
		z1[0] = -f[0][i3], z1[1] = -f[1][i3];
		s[0][i2] = z1[0], s[1][i2] = z1[1];
// L20: 
	    }
	    *ok = get_s(f, s, &h, &k, &l);
	} else {
// s is initialized within GET_S2, which then calls GET_S 
	    *ok = get_s2(f, s, &h, &k, &l);
	}
    } else {
	x = inten2_(f, &h, &k, &l, ok);
    }
    if (! (*ok)) {
	return 0;
    }
//      goto 999 
// for diagnostics write out the s values as well 
    if (rad_type == x_ray) {
	d1 = Math.asin(.5 * lambda * Math.sqrt(h * h * a0 + k *
		k * b0 + l * l * c0 + h * k * d0)
		);
// Computing 2nd power 
	d2 = Math.cos(2. * d1);
	x *= .5 * (1. + d2 * d2);
    }
//      write(op,404) 
//     |     '2theta = ', RAD2DEG * TWO * ANGLE(h,k,l), ' degrees' 
//      if(Shkl .gt. ZERO) write(op,403) 'd = ', ONE / Math.sqrt(Shkl) 
//      if(Shkl .ge. ZERO) write(op,403) '1/d = ', Math.sqrt(Shkl) 
//      write(op,400) ' ' 
//      write(op,400) 'Layer scattering factors' 
    i1 = n_layers;
    for (i = 0; i < i1; i++) {
//        write(op,405) i, f(i) 
// L30: 
    }
//      write(op,400) ' ' 
    if (recrsv) {
//        write(op,400) 'Average scattered wavefunctions' 
	i1 = n_layers;
	for (i = 0; i < i1; i++) {
//          write(op,407) i, s(i) 
// L40: 
	}
    } else {
//        write(op,408) 'Crystal wavefunction', wavefn 
    }
//      write(op,400) ' ' 
//      write(op,406) 'Intensity at ', h, k, l, ' = ', x 
//      write(op,400) 'Another point? (y/n)' 
//      read(cntrl,'(a)') chr 
//      if(chr(1:1).eq.'y' .or. chr(1:1).eq.'Y') goto 1 

    return 0;
//  400 format(1x, a) 
//  401 format(1x, 2i3, g12.5) 
//  402 format(1x, 2i3, g12.5, a) 
//  403 format(1x, a, g12.5) 
//  404 format(1x, a, g12.5, a) 
//  405 format(1x, 'f(', i2, ') = (', g14.7, ',', g14.7, ')') 
//  406 format(1x, a, 2i3, g12.5, a, g14.7) 
//  407 format(1x, 'psi(', i2,') = (', g14.7, ',', g14.7, ')') 
//  408 format(1x, a, ' psi = (', g14.7, ',', g14.7, ')') 
} // point_ 



//  
// Title: POLINT 
// Author: MMJT, adapted from Numerical Recipes Software 
// Date: Copyright (C) 1985 
// Description: Given arrays xa and ya, each of length n, and given 
// a value x, this routine returns an interpolated estimate for y(x), 
// and an error estimate dy. If P(x) is the polynomial of degree n-1, 
// such that P(xa_i) = ya_i, i=1,....N, then the returned value 
// y = P(x). This routine is called by APPR_F. 

//      ARGUMENTS: 
//            xa  -  Array of length n containing the x values 
//                   corresponding to the n ya values. (input). 
//            ya  -  Array of length n containing input y values. (input). 
//            n   -  Length of the arrays entered. (input). 
//            x   -  x position at which an interpolated value is 
//                   required. (input). 
//            y   -  interpolated y value. (output). 
//            dy  -  estimate of the error in y. (output). 
//            ok  -  boolean flag indicating all went well. (output). 
//  

int polint(double[] xa, double[][] ya, int n,
	double x, double[] y, double[] dy, boolean[] ok)
{
    // System generated locals 
    int i1, i2, i3, i4;
    double d1;
    double[] z1 = new double[2];

    // Local variables 
    double[][] c = new double[2][10], d = new double[2][10];
    int i, m;
    double[] w = new double[2];
    double ho, hp;
    int ns;
    double dif;
    double[] den = new double[2];
    double dift;

    // Parameter adjustments 
    --ya;
    --xa;

    // Function Body 
    ns = 1;
    dif = (d1 = *x - xa[1], Math.abs(d1));
    i1 = *n;
    for (i = 0; i < i1; i++) {
	dift = (d1 = *x - xa[i], Math.abs(d1));
	if (dift < dif) {
	    ns = i;
	    dif = dift;
	}
	i2 = i - 1;
	i3 = i;
	c[0][i2] = ya[0][i3], c[1][i2] = ya[1][i3];
	i2 = i - 1;
	i3 = i;
	d[0][i2] = ya[0][i3], d[1][i2] = ya[1][i3];
// L11: 
    }
    i1 = ns;
    y[0] = ya[0][i1], y[1] = ya[1][i1];
    --ns;
    i1 = *n - 1;
    for (m = 1; m <= i1; ++m) {
	i2 = *n - m;
	for (i = 1; i <= i2; ++i) {
	    ho = xa[i] - *x;
	    hp = xa[i + m] - *x;
	    i3 = i;
	    i4 = i - 1;
	    z1[0] = c[i3] - d[0][i4], z1[1] = c[1][i3] - d[1][
		    i4];
	    w[0] = z1[0], w[1] = z1[1];
	    d1 = ho - hp;
	    z1[0] = d1, z1[1] = 0.;
	    den[0] = z1[0], den[1] = z1[1];
	    if (den[0] == 0. && den[1] == 0.) {
		*ok = false;
		return 0;
//          goto 999 
	    }
	    z_div(&z1, &w, &den);
	    den[0] = z1[0], den[1] = z1[1];
	    i3 = i - 1;
	    z1[0] = hp * den[0], z1[1] = hp * den[1];
	    d[0][i3] = z1[0], d[1][i3] = z1[1];
	    i3 = i - 1;
	    z1[0] = ho * den[0], z1[1] = ho * den[1];
	    c[0][i3] = z1[0], c[1][i3] = z1[1];
// L12: 
	}
	if (ns << 1 < *n - m) {
	    i2 = ns;
	    dy[0] = c[0][i2], dy[1] = c[1][i2];
	} else {
	    i2 = ns - 1;
	    dy[0] = d[0][i2], dy[1] = d[1][i2];
	    --ns;
	}
	z1[0] = y[0] + dy[0], z1[1] = y[1] + dy[1];
	y[0] = z1[0], y[1] = z1[1];
// L13: 
    }

    return 0;
//  999 ok = .false. 
//      write(op,100) 'ERROR: Zero denominator in POLINT.' 
//      return 
//  100 format(1x, a) 
} // polint_ 


//  
// Title: PRE_MAT 
// Author: MMJT 
// Date: 21 Mar 1990; 21 July 1997 
// Description:  This subroutine premultiplies some of the factors 
// needed to calculate the matrix mat ( = Identity - alphaij * Rij) 
// at (h,k,0) which remain constant when integrating along a streak. The 
// pre-multiplied factors are stored in mat1, and fatsWalla_hk. 

//      ARGUMENTS: 
//            h   -  reciprocal lattice vector h-component. (input). 
//            k   -  reciprocal lattice vector k-component. (input). 

//      COMMON VARIABLES: 
//            uses:   n_layers, l_r, there, detune, a0, b0, ab0, r_B11, 
//                    r_B22, r_B12, same_Bs, Bs_zero, PI2, l_alpha 

//        modifies:   l_phi, mat1, fatsWalla_hk 
//  

int pre_mat(int *h, int *k)
{
    // System generated locals 
    int i1, i2, i3, i4;
    double d1, d2;
    double[] z1 = new double[2], z2 = new double[2];

    // Local variables 
    int i, j;
    double dot;

// Set up matrix that represents the sequences 
// For the matrix inversion routines, 'mat' and 'mat1' have to be 
// in 'i,j' format rather than the quicker 'j,i' format 
    i1 = n_layers;
    for (i = 0; i < i1; i++) {
	i2 = n_layers;
	for (j = 0; j < i2; j++) {
	    if (there[j + i]) {
		dot = pi2 * (*h * l_r[(j + i * 20) *
			3 - 63] + *k * l_r[(j + i * 20) * 3 - 62]
			);
		i3 = j + i;
		d1 = Math.cos(dot);
		d2 = Math.sin(dot);
		z1[0] = d1, z1[1] = d2;
		l_phi[0][i3] = z1[0], l_phi[1][i3] =
			z1[1];
		if (same_bs || bs_zero[j + i * 20 -
			21]) {
		    i3 = i + j;
		    d1 = detune[j + i] *
			    l_alpha[j + i];
		    i4 = j + i;
		    z1[0] = d1 * l_phi[0][i4], z1[1] = d1 *
			    l_phi[1][i4];
		    mat1[0][i3] = z1[0], mat1[1][i3] =
			    z1[1];
		} else {
// h-k components only. l-components are handled later by GET_MAT. 
		    i3 = i + j;
		    d1 = detune[j + i] *
			    l_alpha[j + i];
		    i4 = j + i;
		    z2[0] = d1 * l_phi[0][i4], z2[1] = d1 *
			    l_phi[1][i4];
		    d2 = Math.exp((r_b11[j + i] *
			    a0 * *h * *h + r_b22[j +
			    i] * b0 * *k * *k) * -.25 +
			    r_b12[j + i] * .5 *
			    ab0 * *h * *k);
		    z1[0] = d2 * z2[0], z1[1] = d2 * z2[1];
		    mat1[0][i3] = z1[0], mat1[1][i3] =
			    z1[1];
		}
	    } else {
		i3 = i + j;
		mat1[0][i3] = 0., mat1[1][i3] = 0.;
	    }
// L20: 
	}
// L10: 
    }

// Are all the uncertainty factors identical? 
// Here, we compute only the h-k components. 
// The l-components are handled later by GET_MAT. 
    if (same_bs) {
	if (all_bs_zero) {
// This initialization is not actually necessary, since if we are here, 
// fatsWalla_hk will not be needed by GET_MAT. However, let's be safe. 
	    fatswalla_hk = 1.;
	} else {
	    fatswalla_hk = Math.exp(-((a_b11 * a0 *
		    *h * *h + a_b22 * b0 * *k * *k) *
		    .25 + a_b12 * .5 * ab0 * *h * *k));
	}
    }

    return 0;
} // pre_mat 



//  
// Title: PRUNE 
// Author: MMJT 
// Date: 4 Oct 1989 
// Description: This function determines the position of the last 
// non-blank character in a character string. 

//      ARGUMENTS: 
//            line     -  Line of characters to examine. (input). 

// PRUNE returns the position of the last non-blank character, or zero 
// if there was an error. 
//  

int prune_(char *line, ftnlen line_len)
{
    // System generated locals 
    int ret_val;

    // Local variables 
    int i;
    boolean loopon;
    int lin_len;

    ret_val = 0;

    lin_len = i_len(line, line_len);
    i = lin_len;
    loopon = true;
    while(loopon) {
	loopon = false;
	if (i > 0) {
	    if (*(unsigned char *)&line[i - 1] == ' ') {
		if (i > 1) {
		    --i;
		    loopon = true;
		}
	    }
	}
    }

    if (i > 0) {
	ret_val = i;
    }

    return ret_val;
} // prune_ 


//  
// Title: PV 
// Author: MMJT 
// Date: 21st Dec 1990; 7 Mar 1995; 9 June 1998 
// Description: This subroutine performs the pseudo-Voigt 
// instrumental broadening. Expects the standard u, v and w and gamma 
// parameters to be available in 'common'. PV does not conserve 
// intensity well when the broadening width is comparable to d_theta. 
// Data at the extreme ends of the spectrum are corrupted slightly. 

//      ARGUMENTS: 
//            th2_low  -  lowest 2theta angle to consider. (input). 

//      COMMON VARIABLES: 
//            uses:  pv_u, pv_v, pv_w, pv_gamma, d_theta, spec 
//                   NONE, PI, RAD2DEG, blurring, th2_max 

//        modifies:  brd_spc 
//  

int pv_(double *th2_low)
{
    // System generated locals 
    int i1, i2;
    double d1;

    // Local variables 
    int i, j;
    double k1, k2, k3, k4, k5, c00, th0, tmp;
    int indx;
    double speci, tn_th;
    int n_low;
    double const;
    int n_high;
    double hk_inv, th_rng, pvoigt;

// first check the numbers 
    if (pv_u == 0. && pv_v == 0. && pv_w ==
	    0.) {
	blurring = none;
	return 0;
//      goto 990 
    }
    if (pv_gamma < 0. || pv_gamma > 1.) {
	blurring = none;
	return 0;
//      goto 999 
    }

    if (*th2_low < 0. || *th2_low >= th2_max) {
//        write(op,103) 'PV: Cut-off angle ', th2_low, 
//     |        ' is out of bounds. Angle reset to zero.' 
	*th2_low = 0.;
    }

// th2_low is the angle relative to th2_min 
// 2*d_theta is the angular step size 
    n_low = (int) (*th2_low * .5 / d_theta) + 1;
    n_high = (int) ((th2_max - th2_min) * .5 /
	    d_theta) + 1;

    c00 = Math.log(2.) * 4.;
    const = rad2deg * 2. * d_theta;
    i1 = n_high;
    for (i = 0; i < i1; i++) {
	brd_spc[i - 1] = 0.;
// L10: 
    }
    k1 = pv_gamma * 2. / pi;
    k2 = (1. - pv_gamma) * Math.sqrt(c00 / pi);
    k3 = -c00;
    th0 = th2_min * .5;
    i1 = n_high;
    for (i = n_low; i <= i1; ++i) {
// get Math.tan((2theta)/2) 
	tn_th = Math.tan(i * d_theta + th0);
	tmp = (pv_u * tn_th + pv_v) * tn_th +
		pv_w;
	if (tmp <= 0.) {
	    blurring = none;
	    return 0;
//      goto 995 
	}
	hk_inv = 1. / Math.sqrt(tmp);
// Computing 2nd power 
	d1 = const * hk_inv;
	tmp = d1 * d1;
	k4 = k1 * hk_inv * const;
	k5 = k2 * hk_inv * const;
	speci = spec[i - 1];
	i2 = n_high - i;
	for (j = n_low - i; j <= i2; ++j) {
	    th_rng = tmp * j * j;
	    pvoigt = (k4 / (th_rng * 4. + 1.) + k5 * Math.exp(k3 * th_rng)) *
		    speci;
	    indx = i + j;
	    brd_spc[indx - 1] += pvoigt;
// L30: 
	}
// L20: 
    }
    return 0;
//  990 write(op,100) 'pseudo-Voigt parameters are zero in PV()' 
//      write(op,101) 
//      blurring = NONE 
//      return 
//  995 write(op,102) 
//     | 'ERROR: pseudo-Voigt spread function is complex at theta = ', 
//     |  i*d_theta 
//      write(op,100) '   u, v, w parameters are illegal.' 
//      write(op,101) 
//      blurring = NONE 
//      return 
//  999 write(op,100) 'Illegal pseudo-Voigt gamma value in PV()' 
//      write(op,101) 
//      blurring = NONE 
//      return 
//  100 format(1x, a) 
//  101 format(1x, '   pseudo-Voigt instrumental broadening not added') 
//  102 format(1x, a, g12.5) 
//  103 format(1x, a, g12.5, a) 
} // pv_ 



//  
// Title: RDBLUR 
// Authors: MMJT 
// Date: 4 Oct 1989 
// Description: This function reads the type of instrumental 
// broadening to be simulated, and the associated parameters. 
// Legal types are 'NONE ', 'PSEUDO-VOIGT ', 'GAUSSIAN ' 
// and 'LORENTZIAN '. PSEUDO-VOIGT requires 4 parameters, 
// GAUSSIAN requires a standard deviation in 2theta degrees, and 
// LORENTZIAN requires a halfwidth in 2theta degrees. 
// If the keyword 'TRIM' is at the end of the data line, intensity 
// near the origin will be ignored when the instrumental broadening 
// is added. 

//      ARGUMENTS: 
//            unit_no  -  Logical unit number that the data file is to 
//                        be read from. (input). 
//            messge   -  A short message indicating what went wrong 
//                        (if anything) during the datafile read. 
//                        messge is terminated by a token '$'. (output). 

//      COMMON VARIABLES: 
//        modifies:  blurring, pv_u, pv_v, pv_w, pv_gamma, 
//                   FWHM, GAUSS, LORENZ, NONE, PS_VGT, trim_origin 

//      RDBLUR returns boolean .true. if a legal function for 
//      simulating instrumental broadening was entered. 

//      Legal types are: 
//             'NONE' 
//             'PSEUDO-VOIGT' (with u, v, w and gamma parameters) 
//             'GAUSSIAN' (with a standard deviation in degrees) 
//             'LORENTZIAN' (with a half width in degrees) 
//  

boolean rdblur_(int *unit_no, char *messge, ftnlen messge_len)
{
    // System generated locals 
    int i1;
    double d1;
    boolean ret_val;

    // Local variables 
    int i;
    char line[200], list[80*4];
    int iflag;
    char dummy[200];
    int lin_len, arg_num;

    ret_val = false;


    s_copy(messge, "Instrumental broadening parameters were not specified.$",
	    messge_len, (ftnlen)55);
//      call GETLNE(unit_no, line, *999) 
    touppr_(line, (ftnlen)200);
    lin_len = i_len(line, (ftnlen)200);

// First, check whether or not user wants to trim the low angle 
// intensities for the broadened spectrum. If present, remove the 
// keyword 'TRIM', and set flag. 
    i = i_indx(line, " TRIM", (ftnlen)200, (ftnlen)5);
    if (i == 0) {
	trim_origin = false;
    } else {
	trim_origin = true;
//        write(line(i+1:i+4), '(a)') '    ' 
    }

    s_copy(list, "NONE ", (ftnlen)80, (ftnlen)5);
    s_copy(list + 80, "GAUSSIAN ", (ftnlen)80, (ftnlen)9);
    s_copy(list + 160, "LORENTZIAN ", (ftnlen)80, (ftnlen)11);
    s_copy(list + 240, "PSEUDO-VOIGT ", (ftnlen)80, (ftnlen)13);
    iflag = choice_(line, list, &c4, (ftnlen)200, (ftnlen)80);

    if (iflag < 1 || iflag > 4) {
//        write(op,100) 
//     |  'Illegal instrumental broadening type. Legal types are:' 
	for (i = 1; i <= 4; ++i) {
//          write(op,101) '               ', list(i) 
// L10: 
	}
	s_copy(messge, "Instrumental broadening incorrectly specified.$",
		messge_len, (ftnlen)47);
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    }

    if (iflag == 1) {
	blurring = none;
    }

    if (iflag == 2) {

// check the number of arguments 
	i1 = length_("GAUSSIAN", (ftnlen)8) + 1;
	s_copy(dummy, line + i1, (ftnlen)200, lin_len - i1);
	arg_num = cntarg_(dummy, (ftnlen)200);
	if (arg_num != 1 && arg_num != 3) {
	    s_copy(messge, "Illegal number of GAUSSIAN arguments. FWHM, or u"
		    ",v,w.$", messge_len, (ftnlen)54);
	    wrtlne_(line, (ftnlen)200);
//        goto 999 
	    return ret_val;
	}

	if (arg_num == 1) {
	    blurring = gauss;

	    s_copy(messge, "Problems reading Gaussian FWHM.$", messge_len, (
		    ftnlen)32);
// for strict FORTRAN77 compliance 
//          write(scrtch, *, err=990) dummy 
//          rewind(scrtch, err=990) 
//          read(scrtch, *, err=999) FWHM 
//          rewind(scrtch, err=990) 
// for more lenient, efficient, compilers 
//          read(dummy, *, err=999) FWHM 

	    if (fwhm < 0.) {
		s_copy(messge, "Gaussian FWHM is -ve.$", messge_len, (ftnlen)
			22);
		wrtlne_(line, (ftnlen)200);
//        goto 999 
		return ret_val;
	    }

	    if (fwhm < 1e-7) {
//            write(op,102) 'Gaussian FWHM is zero.' 
//            write(op,102) 
//     |        ' No instrumental broadening will be simulated.' 
		blurring = none;
	    }

	} else if (arg_num == 3) {
	    blurring = pv_gss;

	    s_copy(messge, "Problems reading Gaussian u, v, and w factors.$",
		    messge_len, (ftnlen)47);
// for strict FORTRAN77 compliance 
//          write(scrtch, *, err=990) dummy 
//          rewind(scrtch, err=990) 
//          read(scrtch, *, err=999) pv_u, pv_v, pv_w 
//          rewind(scrtch, err=990) 
// for more lenient, efficient, compilers 
//          read(dummy, *, err=999) pv_u, pv_v, pv_w 

	    pv_gamma = 0.;

	    if (pv_w < 0.) {
//            write(op,102) 
//     |        'WARNING: Gaussian w-factor is less than zero.' 
//            write(op,102) 
//     |        '  This may cause the broadening function to fail.' 
	    }

	    if (Math.abs(pv_u) < 1e-7 && Math.abs(pv_v) < 1e-7 &&
		    Math.abs(pv_w) < 1e-7) {
//            write(op,102) 'Gaussian u, v, w factors are zero.' 
//            write(op,102) 'No instrumental broadening will be applied.' 
		blurring = none;
	    }

// are the parameters equivalent to a Gaussian with 
// constant standard deviation? 
	    if (Math.abs(pv_u) <= 1e-7 && Math.abs(pv_v) <= 1e-7
		    && pv_w > 0.) {
		fwhm = Math.sqrt(pv_w);
//            write(op,102) 
//     |       'Gaussian factors imply a constant FWHM ', FWHM 
		blurring = gauss;
	    }

	}

    }

    if (iflag == 3) {

// check the number of arguments 
	i1 = length_("LORENTZIAN", (ftnlen)10) + 1;
	s_copy(dummy, line + i1, (ftnlen)200, lin_len - i1);
	arg_num = cntarg_(dummy, (ftnlen)200);
	if (arg_num != 1 && arg_num != 3) {
	    s_copy(messge, "Illegal number of LORENTZIAN arguments. FWHM, or"
		    " u,v,w.$", messge_len, (ftnlen)56);
	    wrtlne_(line, (ftnlen)200);
//        goto 999 
	    return ret_val;
	}

	if (arg_num == 1) {
	    blurring = lorenz;

	    s_copy(messge, "Problems reading Lorentzian width.$", messge_len,
		    (ftnlen)35);
// for strict FORTRAN77 compliance 
//          write(scrtch, *, err=990) dummy 
//          rewind(scrtch, err=990) 
//          read(scrtch, *, err=999) FWHM 
//          rewind(scrtch, err=990) 
// for more lenient, efficient, compilers 
//          read(dummy, *, err=999) FWHM 

	    if (fwhm < 0.) {
		s_copy(messge, "Negative value for Lorentzian FWHM. Must be "
			"positive.$", messge_len, (ftnlen)54);
		wrtlne_(line, (ftnlen)200);
//        goto 999 
		return ret_val;
	    }

	    if (fwhm < 1e-7) {
//            write(op,102) 'Lorentzian FWHM is zero.' 
//            write(op,102) 
//     |      ' No instrumental broadening will be simulated.' 
		blurring = none;
	    }

	} else if (arg_num == 3) {
	    blurring = pv_lrn;

	    s_copy(messge, "Problems reading Lorentzian u, v, and w factors.$"
		    , messge_len, (ftnlen)49);
// for strict FORTRAN77 compliance 
//          write(scrtch, *, err=990) dummy 
//          rewind(scrtch, err=990) 
//          read(scrtch, *, err=999) pv_u, pv_v, pv_w 
//          rewind(scrtch, err=990) 
// for more lenient, efficient, compilers 
//          read(dummy, *, err=999) pv_u, pv_v, pv_w 

	    pv_gamma = 1.;

	    if (pv_w < 0.) {
//            write(op,102) 
//     |        'WARNING: Lorentzian w-factor is less than zero.' 
//            write(op,102) 
//     |        '  This may cause the broadening function to fail.' 
	    }

	    if (pv_u < 1e-7 && pv_v < 1e-7 && Math.abs(
		    pv_w) < 1e-7) {
//            write(op,102) 'Lorentzian u, v, w factors are zero.' 
//            write(op,102) 'No instrumental broadening will be applied.' 
		blurring = none;
	    }

// are the parameters equivalent to a Gaussian with 
// constant standard deviation? 
	    if (pv_u <= 1e-7 && pv_v <= 1e-7 &&
		    pv_w > 0.) {
		fwhm = Math.sqrt(pv_w);
//            write(op,103) 
//     |       'Lorentzian factors imply a constant FWHM ', FWHM 
		blurring = lorenz;
	    }

	}

    }

//  500 continue 

    if (iflag == 4) {
	blurring = ps_vgt;

// check the number of arguments 
	i1 = length_("PSEUDO-VOIGT", (ftnlen)12) + 1;
	s_copy(dummy, line + i1, (ftnlen)200, i_len(line, (ftnlen)200) -
		i1);
	arg_num = cntarg_(dummy, (ftnlen)200);
	if (arg_num != 4) {
	    s_copy(messge, "Exactly 4 pseudo-Voigt arguments are required.$",
		    messge_len, (ftnlen)47);
	    wrtlne_(line, (ftnlen)200);
//        goto 999 
	    return ret_val;
	}

	s_copy(messge, "Problems reading pseudo-Voigt u, v, w, gamma factors"
		".$", messge_len, (ftnlen)54);

// for strict FORTRAN77 compliance 
//        write(scrtch, *, err=990) dummy 
//        rewind(scrtch, err=990) 
//        read(scrtch, *, err=999) pv_u, pv_v, pv_w, pv_gamma 
//        rewind(scrtch, err=990) 
// for more lenient, efficient, compilers 
//        read(dummy, *, err=999) pv_u, pv_v, pv_w, pv_gamma 

	if (pv_w < 0.) {
//          write(op,102) 
//     |        'WARNING: pseudo-Voigt w factor is less than zero.' 
//          write(op,102) 
//     |        '  This may cause the pseudo-Voigt function to fail.' 
	}

	if (Math.abs(pv_u) < 1e-7 && Math.abs(pv_v) < 1e-7 && Math.abs(
		pv_w) < 1e-7) {
//          write(op,102) 'Pseudo-Voigt u, v, w factors are zero.' 
//          write(op,102) 'No instrumental broadening will be simulated.' 
	    blurring = none;
	}

	if (pv_gamma < 0.) {
	    s_copy(messge, "Pseudo-Voigt gamma factor is -ve (must be betwee"
		    "n 0 and 1).$", messge_len, (ftnlen)60);
	    wrtlne_(line, (ftnlen)200);
//        goto 999 
	    return ret_val;
	}

	if (pv_gamma > 1.) {
	    s_copy(messge, "Pseudo-Voigt gamma factor is greater than 1.$",
		    messge_len, (ftnlen)45);
	    wrtlne_(line, (ftnlen)200);
//        goto 999 
	    return ret_val;
	}

// are the pseudo-Voigt parameters equivalent to a Gaussian with 
// constant standard deviation? 
	if (Math.abs(pv_u) <= 1e-7 && Math.abs(pv_v) <= 1e-7 &&
		pv_w > 0. && pv_gamma <= 1e-4) {
	    fwhm = Math.sqrt(pv_w);
//          write(op,102) 
//     |        'Pseudo-Voigt factors are equivalent to a Gaussian' 
//          write(op,103) ' with FWHM ', FWHM 
	    blurring = gauss;
	}

// are the pseudo-Voigt parameters equivalent to a Lorentzian with 
// constant half-width? 
	if (Math.abs(pv_u) <= 1e-7 && Math.abs(pv_v) <= 1e-7 &&
		pv_w > 0. && (d1 = pv_gamma - 1., Math.abs(
		d1)) <= 1e-4) {
	    fwhm = Math.sqrt(pv_w);
//          write(op,102) 
//     |        'Pseudo-Voigt factors are equivalent to a Lorentzian' 
//          write(op,103) ' with FWHM ', FWHM 
	    blurring = lorenz;
	}

    }

    ret_val = true;
    return ret_val;
//  990 messge = 'Problems using scratch file in RDBLUR.$' 
//  999 call WRTLNE(line) 
//      return 
//  100 format(1x, 'ERROR: ', a) 
//  101 format(1x, 2a) 
//  102 format(1x, a) 
//  103 format(1x, a, g12.5) 
} // rdblur_ 



//  
// Title: RDCELL 
// Authors: MMJT 
// Date: 4 Oct 1989 
// Description: This function reads the layer mesh dimensions from 
// the second data line of the data file. It checks that the header 
// 'STRUCTURAL' is present. 

//      ARGUMENTS: 
//            unit_no  -  Logical unit number that the data file is to 
//                        be read from. (input). 
//            messge   -  A short message indicating what went wrong 
//                        (if anything) during the datafile read. 
//                        messge is terminated by a token '$'. (output). 

//      COMMON VARIABLES: 
//        modifies:  cell_a, cell_b, cell_c, cell_gamma 

//      RDCELL returns boolean .true. if acceptable values 
//      for the layer mesh dimensions were read. 
//  

boolean rdcell_(int *unit_no, char *messge, ftnlen messge_len)
{
    // System generated locals 
    boolean ret_val;

    // Local variables 
    char line[200];

    ret_val = false;

// first check that 'STRUCTURAL' header is present 
    s_copy(messge, "'STRUCTURAL' header could not be read.$", messge_len, (
	    ftnlen)39);
//      call GETLNE(unit_no, line, *999) 
    touppr_(line, (ftnlen)200);
    s_copy(messge, "'STRUCTURAL' header not found.$", messge_len, (ftnlen)31);
    if (i_indx(line, "STRUCTURAL ", (ftnlen)200, (ftnlen)11) == 0) {
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    }

    s_copy(messge, "a, b, c, and gamma are improperly specified.$",
	    messge_len, (ftnlen)45);
//      call GETLNE(unit_no, line, *999) 

// for strict FORTRAN77 compliance 
//      write(scrtch, *, err=990) line 
//      rewind(scrtch, err=990) 
//      read(scrtch, *, err=999) cell_a, cell_b, cell_c, cell_gamma 
//      rewind(scrtch, err=990) 
// for more lenient, efficient, compilers 
//      read(line, *, err=999) cell_a, cell_b, cell_c, cell_gamma 

    if (cell_a <= 0.) {
	s_copy(messge, "Negative (or zero) value for cell_a.$", messge_len, (
		ftnlen)37);
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    }

    if (cell_b <= 0.) {
	s_copy(messge, "Negative (or zero) value for cell_b.$", messge_len, (
		ftnlen)37);
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    }

    if (cell_c <= 0.) {
	s_copy(messge, "Negative (or zero) value for cell_c.$", messge_len, (
		ftnlen)37);
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    }

    if (cell_gamma <= 0.) {
	s_copy(messge, "Negative (or zero) value for cell_gamma.$",
		messge_len, (ftnlen)41);
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    }

    if (cell_gamma >= 180.) {
	s_copy(messge, "Cell_gamma is greater than 180 degrees.$", messge_len,
		 (ftnlen)40);
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    }

    cell_gamma *= deg2rad;
    ret_val = true;
    return ret_val;
//  990 messge = 'Problems using scratch file in RDCELL.$' 
//  999 call WRTLNE(line) 
//      return 
} // rdcell_ 



//  
// Title: RDFILE 
// Author: MMJT 
// Date: 3 Oct 1989; 3 Mar 1995 
// Description: This function reads in the data from the user-defined 
// data file. The data file is in Naur-Backus form, and the user may 
// insert '{' and '}' delimiters to comment the data. The data file 
// is not read in its raw form. The subroutine GETLNE is used 
// to read each line, stripped of its comments, and left-justified. 

//      ARGUMENTS: 
//           fname  -  The name of the input data file to read. (input). 

//      RDFILE returns boolean .true. if the file read went well. 
//  

/* boolean rdfile_(char *fname, ftnlen fname_len)
{
    // System generated locals 
    boolean ret_val;

    // Local variables 
    boolean goodfile;
    char messge[200];
    int unit_no;

    ret_val = false;
    unit_no = 2;
//      write(op,100) 'Reading ''', fname(1:LENGTH(fname)),'''' 

// now read the file, one datum at a time. 
    goodfile = rdradn_(&unit_no, messge, (ftnlen)200);
    if (goodfile) {
	goodfile = rdwavl_(&unit_no, messge, (ftnlen)200);
    }
    if (goodfile) {
	goodfile = rdblur_(&unit_no, messge, (ftnlen)200);
    }
    if (goodfile) {
	goodfile = rdcell_(&unit_no, messge, (ftnlen)200);
    }
    if (goodfile) {
	goodfile = rdptgp_(&unit_no, messge, (ftnlen)200);
    }
    if (goodfile) {
	goodfile = rdnlay_(&unit_no, messge, (ftnlen)200);
    }
    if (goodfile) {
	goodfile = rdwdth_(&unit_no, messge, (ftnlen)200);
    }
    if (goodfile) {
	goodfile = rdlayr_(&unit_no, messge, (ftnlen)200);
    }
    if (goodfile) {
	goodfile = rdstak_(&unit_no, messge, (ftnlen)200);
    }
    if (goodfile) {
	goodfile = rdtrns_(&unit_no, messge, (ftnlen)200);
    }

    if (goodfile) {
//        write(op,100) '''', fname(1:LENGTH(fname)),''' read in.' 
    } else {
//        write(op,200) messge(1:index(messge,'$') - 1) 
    }

//      write(messge,101) 
//     |      'Problems closing ''', fname(1:LENGTH(fname)),'''$' 
//      close(df,err=900) 
// if all went well, set RDFILE to .true. 
    if (goodfile) {
	ret_val = true;
    }
    return ret_val;

//  900 write(op,200) messge(1:index(messge,'$') - 1) 
//      return 
//  100 format(1x, 3a) 
//  101 format(3a) 
//  200 format(1x, 'ERROR: ', a) 
} // rdfile_ */


//  
// Title: RDLAYR 
// Authors: MMJT 
// Date: 7 June 1990 
// Description: This function reads in the layer atomic coordinates. 
// Each set of data must be prefaced with a layer number, and a 
// statement of whether or not the layer is centrosymmetric. If the 
// layer is centrosymmetric, only the asymmetric coordinates need 
// be entered (with appropriate fractional occupancies for special 
// positions. 
// If a layer has the same structure as a previously defined layer 
// the layer can be defined as 'LAYER i = j', where j is a previously 
// defined layer. 
// The format assumed for atom coordinates is: 

//    'atom name', number, x, y, z, Debye-Waller factor, occupancy 

// 'atom name' must occupy 4 characters, eg. 'Si4+', 'O 1-' etc... 
// The allowed atom names can be found in the file 'data.sfc'. 

//      ARGUMENTS: 
//            unit_no  -  Logical unit number that the data file is to 
//                        be read from. (input). 
//            messge   -  A short message indicating what went wrong 
//                        (if anything) during the datafile read. 
//                        messge is terminated by a token '$'. (output). 

//      COMMON VARIABLES: 
//            uses:  n_layers, l_actual, CENTRO, NONE 

//        modifies:  l_symmetry, a_name, a_number, a_pos, a_B, a_occup, 
//                   l_n_atoms, n_actual 

//      RDLAYR returns boolean .true. if all the layer data 
//      was read successfully. 
//  

boolean rdlayr_(int *unit_no, char *messge, ftnlen messge_len)
{
    // System generated locals 
    int i1;
    boolean ret_val;

    // Local variables 
    int i, j, m, n, i2;
    boolean ok;
    char arg[200];
    double tmp;
    char line[200], list[80*2];
    int iflag;
    boolean loopon;
    char tmpline[200];

    ret_val = false;

    i1 = n_layers;
    for (i = 0; i < i1; i++) {
	high_atom[i - 1] = 0.;
	low_atom[i - 1] = 0.;
// L5: 
    }

    m = 0;
    s_copy(messge, "Unexpected EOF before LAYER 1.$", messge_len, (ftnlen)31);
//      call GETLNE(unit_no, line, *999) 
    touppr_(line, (ftnlen)200);

    i1 = n_layers;
    for (i = 0; i < i1; i++) {
//        write(messge,100) 'LAYER header not seen for LAYER ',i,'.$' 
	if (i_indx(line, "LAYER", (ftnlen)200, (ftnlen)5) != 1) {
	    wrtlne_(line, (ftnlen)200);
//        goto 999 
	    return ret_val;
	}
// read layer number 
//        write(messge,100) 'LAYER ',i,' number sequence invalid.$' 
//        write(tmpline, '(a)', err=999) 
//     |       line(LENGTH('LAYER')+1:len(line)) 
// for strict FORTRAN77 compliance 
//        write(scrtch, *, err=990) tmpline 
//        rewind(scrtch, err=990) 
//        read(scrtch, *, err=999) j 
//        rewind(scrtch, err=990) 
// for more lenient, efficient, compilers 
//        read(tmpline, *, err=999) j 

// layers must be entered in sequence 
	if (j != i) {
	    wrtlne_(line, (ftnlen)200);
//        goto 999 
	    return ret_val;
	}

	j = i_indx(line, "=", (ftnlen)200, (ftnlen)1);
// see if this layer is equivalenced to another 
	if (j != 0) {

//          write(messge,100) 
//     |           'Invalid equivalence number for LAYER ',i,'.$' 
//          write(tmpline, '(a)', err=999) line(j+1:len(line)) 
// for strict FORTRAN77 compliance 
//          write(scrtch, *, err=990) tmpline 
//          rewind(scrtch, err=990) 
//          read(scrtch, *, err=999) i2 
//          rewind(scrtch, err=990) 
// for more lenient, efficient, compilers 
//        read(tmpline, *, err=999) i2 

// layer i2 must be defined already 
	    if (i2 >= i || i2 < 1) {
		wrtlne_(line, (ftnlen)200);
//        goto 999 
		return ret_val;
	    }
	    l_actual[i - 1] = l_actual[i2 - 1];

//          write(messge,100) 'Unexpected EOF in LAYER ',i,'.$' 
//          call GETLNE(unit_no, line, *999) 
	    touppr_(line, (ftnlen)200);

	} else {
	    ++m;
	    l_actual[i - 1] = m;

//          write(messge,100) 
//     |           'No symmetry statement for LAYER ',i,'.$' 
//          call GETLNE(unit_no, line, *999) 
	    touppr_(line, (ftnlen)200);

	    s_copy(list, "NONE ", (ftnlen)80, (ftnlen)5);
	    s_copy(list + 80, "CENTROSYMMETRIC ", (ftnlen)80, (ftnlen)16);
	    iflag = choice_(line, list, &c2, (ftnlen)200, (ftnlen)80);

//          write(messge,100) 
//     |           'Invalid symmetry statement for LAYER ',i,'.$' 
	    if (iflag < 1 || iflag > 2) {
		wrtlne_(line, (ftnlen)200);
//        goto 999 
		return ret_val;
	    }

	    if (iflag == 1) {
		l_symmetry[m - 1] = none;
	    } else {
		l_symmetry[m - 1] = centro;
	    }

//          write(messge,100) 'No atoms specified for LAYER ',i,'.$' 
//          call GETLNE(unit_no, line, *999) 
	    if (i_indx(line, "LAYER", (ftnlen)200, (ftnlen)5) == 1) {
		wrtlne_(line, (ftnlen)200);
//        goto 999 
		return ret_val;
	    }

	    j = 0;
	    loopon = true;
	    while(loopon) {
		loopon = false;
		++j;
		if (j > MAX_A) {
//              write(op,101) 'ERROR: Too many atoms on LAYER ',i,'.' 
//              write(op,102) '       Maximum allowed = ', MAX_A 
		    s_copy(messge, " $", messge_len, (ftnlen)2);
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		}

		s_copy(chars1_1.a_name[j][m], line, (
			ftnlen)4, (ftnlen)4);

//            write(messge,103) 
//     |        'Problems reading data for atom ''', a_name(j,m), '''.$' 
//            write(tmpline, '(a)', err=999) line(5:len(line)) 
// read atom identifier 
		n = nxtarg_(tmpline, arg, (ftnlen)200, (ftnlen)200);
		if (n <= 0) {
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}
//            read(arg,'(i3)') a_number(j,m) 

// read x_relative position 
		n = nxtarg_(tmpline, arg, (ftnlen)200, (ftnlen)200);
		if (n <= 0) {
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}
		a_pos[(j + m * 200) * 3 - 603] = rdnmbr_(arg, &ok,
			(ftnlen)200);
		if (! ok) {
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}

// read y_relative position 
		n = nxtarg_(tmpline, arg, (ftnlen)200, (ftnlen)200);
		if (n <= 0) {
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}
		a_pos[(j + m * 200) * 3 - 602] = rdnmbr_(arg, &ok,
			(ftnlen)200);
		if (! ok) {
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}

// read z_relative position 
		n = nxtarg_(tmpline, arg, (ftnlen)200, (ftnlen)200);
		if (n <= 0) {
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}
		a_pos[(j + m * 200) * 3 - 601] = rdnmbr_(arg, &ok,
			(ftnlen)200);
		if (! ok) {
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}

// read Debye-Waller factor 
		n = nxtarg_(tmpline, arg, (ftnlen)200, (ftnlen)200);
		if (n <= 0) {
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}
		a_b[j + m * 200 - 201] = rdnmbr_(arg, &ok, (ftnlen)
			200);
		if (! ok) {
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}

// read occupancy 
		n = nxtarg_(tmpline, arg, (ftnlen)200, (ftnlen)200);
		if (n <= 0) {
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}
		a_occup[j + m * 200 - 201] = rdnmbr_(arg, &ok, (
			ftnlen)200);
		if (! ok) {
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}

// Check values 
		if (a_b[j + m * 200 - 201] < 0.) {
		    s_copy(messge, "Negative Debye-Waller factor.$",
			    messge_len, (ftnlen)30);
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}

		if (a_occup[j + m * 200 - 201] < 0.) {
		    s_copy(messge, "Negative atom occupancy.$", messge_len, (
			    ftnlen)25);
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}

		if (a_occup[j + m * 200 - 201] > 1.) {
		    s_copy(messge, "Occupancy greater than 1.$", messge_len, (
			    ftnlen)26);
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}

// get extreme upper and lower atom positions for error-checking later on 
		tmp = a_pos[(j + m * 200) * 3 - 601];
		if (tmp > high_atom[m - 1]) {
		    high_atom[m - 1] = tmp;
		}
		if (tmp < low_atom[m - 1]) {
		    low_atom[m - 1] = tmp;
		}

//            write(messge,103)'Unexpected EOF after atom ',line(1:4),'$' 
//            call GETLNE(unit_no, line, *999) 
// see if all atomic specifications for this layer have been read in 
// but first we must convert to uppercase for the test. If there are 
// more atoms, then we must convert 'line' to its original format. 
		s_copy(tmpline, line, (ftnlen)200, (ftnlen)200);
		touppr_(line, (ftnlen)200);
// check data is in correct sequence 
		s_copy(messge, "STACKING data is missing.$", messge_len, (
			ftnlen)26);
		if (i_indx(line, "PARAMETERS", (ftnlen)200, (ftnlen)10) == 1)
			{
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}
		if (i_indx(line, "LAYER", (ftnlen)200, (ftnlen)5) != 1 &&
			i_indx(line, "STACKING", (ftnlen)200, (ftnlen)8) != 1)
			 {
		    s_copy(line, tmpline, (ftnlen)200, (ftnlen)200);
		    loopon = true;
		}
	    }
	    l_n_atoms[m - 1] = j;
	}
// L30: 
    }
    n_actual = m;

// make sure we have genuine lowest and highest atoms in each layer. 
    i1 = n_actual;
    for (i = 0; i < i1; i++) {
	if (l_symmetry[i - 1] == centro) {
	    if (-low_atom[i - 1] > high_atom[i - 1])
		     {
		high_atom[i - 1] = -low_atom[i - 1];
	    } else if (-low_atom[i - 1] < high_atom[
		    i - 1]) {
		low_atom[i - 1] = -high_atom[i - 1];
	    }
	}
// L40: 
    }

// If we have reached here, then we have read one line too many. 
//      write(messge,100) 'Problems ''backspacing'' unit ',unit_no, '.$' 
//      backspace(unit = unit_no, err = 980) 

    ret_val = true;
//  980 return 
//  990 messge = 'Problems using scratch file in RDLAYR.$' 
//  999 call WRTLNE(line) 
    return ret_val;
//  100 format(a, i2, a) 
//  101 format(1x, a, i2, a) 
//  102 format(1x, a, i4) 
//  103 format(3a) 
} // rdlayr_ 



//  
// Title: RDNLAY 
// Authors: MMJT 
// Date: 4 Oct 1989 
// Description: This function reads the first line of the data file 
// to extract the number of layers in the structure. 

//      ARGUMENTS: 
//            unit_no  -  Logical unit number that the data file is to 
//                        be read from. (input). 
//            messge   -  A short message indicating what went wrong 
//                        (if anything) during the datafile read. 
//                        messge is terminated by a token '$'. (output). 

//      COMMON VARIABLES: 
//        modifies:  n_layers 

//      RDNLAY returns boolean .true. if an acceptable value 
//      for the number of layers was read. 
//  

boolean rdnlay_(int *unit_no, char *messge, ftnlen messge_len)
{
    // System generated locals 
    boolean ret_val;

    // Local variables 
    char line[200];

    ret_val = false;

    s_copy(messge, "No lines in file.$", messge_len, (ftnlen)18);
//      call GETLNE(unit_no, line, *999) 

    s_copy(messge, "Problems reading number of layers.$", messge_len, (ftnlen)
	    35);
// for strict FORTRAN77 compliance 
//      write(scrtch, *, err=990) line 
//      rewind(scrtch, err=990) 
//      read(scrtch, *, err=999) n_layers 
//      rewind(scrtch, err=990) 
// for more lenient, efficient, compilers 
//      read(line, *, err=999) n_layers 

    if (n_layers == 0) {
	s_copy(messge, "Zero number of layers entered.$", messge_len, (ftnlen)
		31);
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    } else if (n_layers < 0) {
	s_copy(messge, "Negative number of layers entered.$", messge_len, (
		ftnlen)35);
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    } else if (n_layers > MAX_L) {
//        write(messge,100) 
//     |  'Too many layers: number should not exceed ',MAX_L,'.$' 
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    }

    ret_val = true;
    return ret_val;
//  990 messge = 'Problems using scratch file in RDNLAY.$' 
//  999 call WRTLNE(line) 
//      return 
//  100 format(a, i2, a) 
} // rdnlay_ 



//  
// Title: RDNMBR 
// Author: MMJT 
// Date: 21 January 2005 
// Description: Reads the character string 'numberstring' to extract a 
// floating point number. The character string can be either a true 
// number, such as '0.3333', or can be expressed as a ratio, '1/3'. 

//      ARGUMENTS: 
//            arg   -  Character string containing numeric data.(input). 
//            ok    -  True of all went well.(output). 

//      Returns the floating point number 
//  

double rdnmbr_(char *numberstring, boolean *ok, ftnlen numberstring_len)
{
    // System generated locals 
    double ret_val;

    // Local variables 
    int i, j, numerator;
    char arg[200];
    int denominator;
    int lin_len;

    *ok = true;
    ret_val = 0.;

    lin_len = i_len(numberstring, numberstring_len);
    j = i_indx(numberstring, "/", numberstring_len, (ftnlen)1);
    if (j == 0) {
// It was already a number 
//        read(numberstring, *) RDNMBR 
    } else if (j < 0 || j >= lin_len) {
	*ok = false;
//        goto 999 
	return ret_val;
    } else {
// It was expressed as a ratio. Blank out the slash, '/'. 
	*(unsigned char *)&numberstring[j - 1] = ' ';
// 'numberstring' now contains two arguments, numerator and denominator. 
	i = nxtarg_(numberstring, arg, numberstring_len, (ftnlen)200);
	if (i <= 0) {
	    *ok = false;
//        goto 999 
	    return ret_val;
	}
//        read(arg, *) numerator 
	i = nxtarg_(numberstring, arg, numberstring_len, (ftnlen)200);
	if (i <= 0) {
	    *ok = false;
//        goto 999 
	    return ret_val;
	}
//        read(arg, *) denominator 
	if ((double) denominator <= 0.) {
	    *ok = false;
//        goto 999 
	    return ret_val;
	}
	ret_val = (double) numerator / (double) denominator;
    }

    return ret_val;
//  999 ok = .false. 
//      return 
} // rdnmbr_ 


//  
// Title: RDPTGP 
// Author: MMJT 
// Date: 4 Oct 1989 
// Description: This function reads the user's estimate of the 
// point group symmetry of the diffraction data. 

//      ARGUMENTS: 
//            unit_no  -  Logical unit number that the data file is to 
//                        be read from. (input). 
//            messge   -  A short message indicating what went wrong 
//                        (if anything) during the datafile read. 
//                        messge is terminated by a token '$'. (output). 

//      COMMON VARIABLES: 
//        modifies:  pnt_grp, SymGrpNo, tolerance 

//      RDPTGP returns boolean .true. if a legal point group 
//      was entered, or if 'unknown' (with a search tolerance) 
//      was specified. 

//      Legal entries are: 
//                     '-1' 
//                     '2/M(1)'    (diad along streaks) 
//                     '2/M(2)'    (mirror contains streaks) 
//                     'MMM' 
//                     '-3' 
//                     '-3M' 
//                     '4/M' 
//                     '4/MMM' 
//                     '6/M' 
//                     '6/MMM' 
//                     'AXIAL'     (for integration along 00l only) 
//                     'UNKNOWN'   (followed by an optional tolerance) 
//  

boolean rdptgp_(int *unit_no, char *messge, ftnlen messge_len)
{
    // System generated locals 
    boolean ret_val;

    // Local variables 
    int i;
    char line[200], list[80*12];
    int iflag;
    char oldline[200];

    ret_val = false;

    s_copy(messge, "Illegal diffraction point group symmetry.$", messge_len, (
	    ftnlen)42);
//      call GETLNE(unit_no, line, *999) 
    touppr_(line, (ftnlen)200);

    s_copy(list, "-1 ", (ftnlen)80, (ftnlen)3);
    s_copy(list + 80, "2/M(1) ", (ftnlen)80, (ftnlen)7);
    s_copy(list + 160, "2/M(2) ", (ftnlen)80, (ftnlen)7);
    s_copy(list + 240, "MMM ", (ftnlen)80, (ftnlen)4);
    s_copy(list + 320, "-3 ", (ftnlen)80, (ftnlen)3);
    s_copy(list + 400, "-3M ", (ftnlen)80, (ftnlen)4);
    s_copy(list + 480, "4/M ", (ftnlen)80, (ftnlen)4);
    s_copy(list + 560, "4/MMM ", (ftnlen)80, (ftnlen)6);
    s_copy(list + 640, "6/M ", (ftnlen)80, (ftnlen)4);
    s_copy(list + 720, "6/MMM ", (ftnlen)80, (ftnlen)6);
    s_copy(list + 800, "AXIAL ", (ftnlen)80, (ftnlen)6);
    s_copy(list + 880, "UNKNOWN ", (ftnlen)80, (ftnlen)8);
    iflag = choice_(line, list, &c12, (ftnlen)200, (ftnlen)80);
    if (iflag < 1 || iflag > 12) {
//        write(op,100) 'Unrecognized symmetry type. Legal types are:' 
	for (i = 1; i <= 12; ++i) {
//          write(op,101) '               ',list(i) 
// L10: 
	}
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    }

    s_copy(pnt_grp, list + (iflag - 1) * 80, (ftnlen)12, (ftnlen)
	    80);

// set default tolerance on intensity variability 
    tolerance = 1.;

    if (iflag >= 1 && iflag <= 11) {
	symgrpno = iflag;
    } else if (iflag == 12) {
// a value of UNKNOWN = -1 alerts OPTIMZ that user entered 'UNKNOWN' 
	symgrpno = -1;
// Strip off the symmetry point group text string. 
	s_copy(oldline, line, (ftnlen)200, (ftnlen)200);
//        write(line,'(a)') oldline(LENGTH(list(iflag))+1:len(oldline)) 
// If the remainder of the line is blank, no tolerance was specified. 
	i = prune_(line, (ftnlen)200);
	if (i <= 0) {
	    s_copy(messge, "Problems reading symmetry tolerance parameter.$",
		    messge_len, (ftnlen)47);
	    wrtlne_(line, (ftnlen)200);
//        goto 999 
	    return ret_val;
	} else if (i > 1) {
// There was something on the line, try to read tolerance 
	    s_copy(messge, "Problems reading symmetry tolerance parameter.$",
		    messge_len, (ftnlen)47);
// for strict FORTRAN77 compliance 
//          write(scrtch, *, err=990) line 
//          rewind(scrtch, err=990) 
//          read(scrtch, *, err=999) tolerance 
//          rewind(scrtch, err=990) 
// for more lenient, efficient, compilers 
//          read(line, *, err=999) tolerance 

	    if (tolerance <= 0.) {
		s_copy(messge, "Negative (or zero) value for tolerance.$",
			messge_len, (ftnlen)40);
		wrtlne_(line, (ftnlen)200);
//        goto 999 
		return ret_val;
	    }

	    if (tolerance < .01) {
		tolerance = .01;
//            write(op,102) 
//            write(op,103) tolerance 
	    }

	}
    }

// convert from a percentage 
    tolerance *= .01;

    ret_val = true;
    return ret_val;
//  990 messge = 'Problems using scratch file in RDPTGP.$' 
//  999 call WRTLNE(line) 
//      return 
//  100 format(1x, 'ERROR: ', a) 
//  101 format(1x, 2a) 
//  102 format(1x, 'WARNING: Tolerance for symmetry tests is too small.') 
//  103 format(1x, '         Tolerance is reset to ', g12.5, ' percent') 
} // rdptgp_ 



//  
// Title: RDRADN 
// Authors: MMJT 
// Date: 15 Feb 1990 
// Description: This function reads the radiation type. The choices are 
// X-RAY, NEUTRON and ELECTRON. Checks that the keword 'INSTRUMENTAL' 
// is present. 

//      ARGUMENTS: 
//            unit_no  -  Logical unit number that the data file is to 
//                        be read from. (input). 
//            messge   -  A short message indicating what went wrong 
//                        (if anything) during the datafile read. 
//                        messge is terminated by a token '$'. (output). 

//      COMMON VARIABLES: 
//            uses:  ELECTN, NEUTRN, X_RAY 

//        modifies:  rad_type 

//      RDRADN returns boolean .true. if an acceptable 
//      radiation type was read. 
//  

boolean rdradn_(int *unit_no, char *messge, ftnlen messge_len)
{
    // System generated locals 
    boolean ret_val;

    // Local variables 
    char line[200], list[80*3];
    int iflag;

    ret_val = false;

// first check that 'INSTRUMENTAL' header is present 
    s_copy(messge, "'INSTRUMENTAL' header could not be read.$", messge_len, (
	    ftnlen)41);
//      call GETLNE(unit_no, line, *999) 
    touppr_(line, (ftnlen)200);
    s_copy(messge, "'INSTRUMENTAL' header not found.$", messge_len, (ftnlen)
	    33);
    iflag = i_indx(line, "INSTRUMENTAL ", (ftnlen)200, (ftnlen)13);
    if (iflag == 0) {
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    }

    s_copy(messge, "radiation type not specified.$", messge_len, (ftnlen)30);
//      call GETLNE(unit_no, line, *999) 
    touppr_(line, (ftnlen)200);
    s_copy(list, "X-RAY ", (ftnlen)80, (ftnlen)6);
    s_copy(list + 80, "NEUTRON ", (ftnlen)80, (ftnlen)8);
    s_copy(list + 160, "ELECTRON ", (ftnlen)80, (ftnlen)9);
    iflag = choice_(line, list, &c3, (ftnlen)200, (ftnlen)80);

    if (iflag == 1) {
	rad_type = x_ray;
    } else if (iflag == 2) {
	rad_type = neutrn;
    } else if (iflag == 3) {
	rad_type = electn;
    } else {
	s_copy(messge, "Invalid radiation type (X-RAY, NEUTRON or ELECTRON).$"
		, messge_len, (ftnlen)53);
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    }

    ret_val = true;
    return ret_val;
//  999 call WRTLNE(line) 
//      return 
} // rdradn_ 



//  
// Title: RDRNGE 
// Authors: MMJT 
// Date: 15 Feb 1990 
// Description: This function reads the angular range for the spectrum. 

//      ARGUMENTS: 
//            unit_no  -  Logical unit number that the data file is to 
//                        be read from. (input). 

//      COMMON VARIABLES: 
//            uses:  DEG2RAD 

//        modifies:  th2_min, th2_max, d_theta 

//      RDRNGE returns boolean .true. if an acceptable angular 
//      range was read. 
//  

boolean rdrnge_(void)
{
    // System generated locals 
    boolean ret_val;

    // Local variables 

// define some useful numerical constants 

    ret_val = false;

//  123 write(op,100) 'Enter angular range:' 
//      write(op,100) '  2theta min, 2theta max, 2theta increment.' 
//      read(cntrl,*,err=123) th2_min, th2_max, d_theta 
//      if(CFile) write(op,105) th2_min, th2_max, d_theta 

    if (th2_min < 0.) {
//        write(op,110) '2theta min is negative.' 
	return ret_val;
    }

    if (th2_min >= 180.) {
//        write(op,110) '2theta min exceeds 180 degrees.' 
	return ret_val;
    }

    if (th2_max <= 0.) {
//        write(op,110) 'Negative (or zero) value for 2theta min.' 
	return ret_val;
    }

    if (th2_max > 180.) {
//        write(op,110) '2theta max exceeds 180 degrees.' 
	return ret_val;
    }

// if th2_max = 180, reduce it slightly to keep us out of trouble 
    if (th2_max == 180.) {
	th2_max += -1e-4;
    }

    if (d_theta <= 0.) {
//        write(op,110) 'Negative (or zero) value for 2theta increment.' 
	return ret_val;
    }

    if (th2_min >= th2_max) {
//        write(op,110) '2theta min is larger than 2theta max.' 
	return ret_val;
    }

    if (th2_max - th2_min < d_theta) {
//        write(op,110) 
//     |  '2theta increment is larger than 2theta max - 2theta min.' 
	return ret_val;
    }

    th2_min *= deg2rad;
    th2_max *= deg2rad;
    d_theta = deg2rad * .5 * d_theta;

    ret_val = true;
    return ret_val;

//  999 return 
//  100 format(1x, a) 
//  105 format(1x, g11.4, 2(3x, g11.4)) 
//  110 format(1x, 'ERROR: ', a) 
} // rdrnge_ 



//  
// Title: RDSTAK 
// Author: MMJT 
// Date: 4 Oct 1989 
// Description: This function reads the STACKING input of the datafile. 
// The stacking is either 'EXPLICIT', whereupon DIFFaX expects the 
// user to enter an explicit sequence of layers, or 'RECURSIVE'. If 
// 'EXPLICIT' was specified, the user may state that the sequence is 
// 'RANDOM', whereby DIFFaX will generate an explicit sequence of 
// layers weighted by the stacking probabilities specified later on 
// under 'PARAMETERS'. The user can specify a crystal thickness (in 
// terms of the number of layers) under the 'RECURSIVE' option. 

//      ARGUMENTS: 
//            unit_no  -  Logical unit number that the data file is to 
//                        be read from. (input). 
//            messge   -  A short message indicating what went wrong 
//                        (if anything) during the datafile read. 
//                        messge is terminated by a token '$'. (output). 

//      COMMON VARIABLES: 
//            uses:  n_layers 

//        modifies:  recrsv, xplcit, rndm, l_cnt, l_seq, inf_thick 

//      RDSTAK returns boolean .true. if the stacking 
//      method was properly specified. 
//  

boolean rdstak_(int *unit_no, char *messge, ftnlen messge_len)
{
    // System generated locals 
    int i1;
    boolean ret_val;

    // Local variables 
    int i;
    char line[200], list[80*2];
    int iflag;

    ret_val = false;

//      call GETLNE(unit_no, line, *999) 
    touppr_(line, (ftnlen)200);
    if (i_indx(line, "STACKING ", (ftnlen)200, (ftnlen)9) != 1) {
//        write(messge,103) 'The ', n_layers, 
//     |   ' layers are read in. ''STACKING'' section not found.$' 
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    }

    s_copy(messge, "Illegal STACKING description.$", messge_len, (ftnlen)30);
//      call GETLNE(unit_no, line, *999) 
    touppr_(line, (ftnlen)200);

    s_copy(list, "EXPLICIT ", (ftnlen)80, (ftnlen)9);
    s_copy(list + 80, "RECURSIVE ", (ftnlen)80, (ftnlen)10);
    iflag = choice_(line, list, &c2, (ftnlen)200, (ftnlen)80);

    if (iflag < 1 || iflag > 2) {
//        write(op,100) 'Illegal STACKING keyword. Legal types are:' 
	for (i = 1; i <= 2; ++i) {
//          write(op,101) '               ',list(i) 
// L10: 
	}
	s_copy(messge, " $", messge_len, (ftnlen)2);
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    }

// set flags to indicate stacking type 
    recrsv = false;
    xplcit = false;
    rndm = false;
    inf_thick = false;
// initialize l_cnt 
    l_cnt = 0;
    if (iflag == 1) {
	xplcit = true;
    } else if (iflag == 2) {
	recrsv = true;
    }

    if (recrsv) {
	s_copy(messge, "Problem reading crystal thickness for recursion.$",
		messge_len, (ftnlen)49);
//        call GETLNE(unit_no, line, *999) 
	touppr_(line, (ftnlen)200);

	if (i_indx(line, "INFINITE", (ftnlen)200, (ftnlen)8) == 0) {
	    inf_thick = false;
	    s_copy(messge, "Problems reading the number of RECURSIVE layers.$"
		    , messge_len, (ftnlen)49);
// for strict FORTRAN77 compliance 
//          write(scrtch, *, err=990) line 
//          rewind(scrtch, err=990) 
//          read(scrtch, *, err=999) l_cnt 
//          rewind(scrtch, err=990) 
// for more lenient, efficient, compilers 
//          read(line, *, err=999) l_cnt 
// check l_cnt 
	    if (l_cnt == 0) {
		s_copy(messge, "The number of RECURSIVE layers cannot be zer"
			"o.$", messge_len, (ftnlen)47);
	    } else if (l_cnt < 0) {
		s_copy(messge, "The number of RECURSIVE layers cannot be neg"
			"ative.$", messge_len, (ftnlen)51);
	    } else if (l_cnt > 1022) {
//            write(op,104) 
//     |       'WARNING: Number of RECURSIVE layers ', l_cnt, 
//     |       ' exceeds the maximum ', RCSV_MAX 
//            write(op,105) 'An INFINITE thickness is assumed.' 
// re-set l_cnt and proceed assuming infinite thickness 
		l_cnt = 0;
		inf_thick = true;
	    }
	} else {
	    inf_thick = true;
	}

    } else if (xplcit) {
// read in layer sequence 
	s_copy(messge, "Problem reading EXPLICIT layer sequencing.$",
		messge_len, (ftnlen)43);
//        call GETLNE(unit_no, line, *999) 
	touppr_(line, (ftnlen)200);
	rndm = i_indx(line, "RANDOM ", (ftnlen)200, (ftnlen)7) != 0;
// catch possible error - user requested INFINITE 
	if (! rndm) {
	    if (i_indx(line, "INFINITE", (ftnlen)200, (ftnlen)8) != 0) {
//            write(op,107) 'Maximum number of layers allowed is ',XP_MAX 
		s_copy(messge, "Too many layers for an EXPLICIT sequence.$",
			messge_len, (ftnlen)42);
		wrtlne_(line, (ftnlen)200);
//        goto 999 
		return ret_val;
	    }
	}

	if (rndm) {
// the user asked for a random layer sequencing. 

	    s_copy(messge, "Problems reading the number of random EXPLICIT l"
		    "ayers.$", messge_len, (ftnlen)55);
//          write(tmpline, '(a)', err=999) 
//     |        line(LENGTH('RANDOM')+2:len(line)) 
// for strict FORTRAN77 compliance 
//          write(scrtch, *, err=990) tmpline 
//          rewind(scrtch, err=990) 
//          read(scrtch, *, err=999) l_cnt 
//          rewind(scrtch, err=990) 
// for more lenient, efficient, compilers 
//          read(tmpline, *, err=999) l_cnt 

	} else {
	    l_cnt = 1;
	    s_copy(messge, "Problems reading EXPLICIT layer sequence in RDST"
		    "AK.$", messge_len, (ftnlen)52);
L20:
	    i = laycnt_(line, (ftnlen)200);

	    if (i >= 0 && i <= 40) {

// for strict FORTRAN77 compliance 
//            write(scrtch, *, err=990) line 
//            rewind(scrtch, err=990) 
//            read(scrtch, *, err=999) (l_seq(j), j=l_cnt,l_cnt+i-1) 
//            rewind(scrtch, err=990) 
// for more lenient, efficient, compilers 
//            read(line, *, err=999) (l_seq(j), j=l_cnt,l_cnt+i-1) 

		l_cnt += i;
	    } else {
		wrtlne_(line, (ftnlen)200);
//        goto 999 
		return ret_val;
	    }

//          call GETLNE(unit_no, line, *999) 
	    touppr_(line, (ftnlen)200);
// check we have not run into the 'TRANSITIONS' section yet. 
	    if (i_indx(line, "TRANSITIONS", (ftnlen)200, (ftnlen)11) != 1) {
		goto L20;
	    }
	    --l_cnt;

// If we are here, we have gone one line too far. Back up. 
//          write(messge,103) 
//     |       'Problems ''backspacing'' unit ', unit_no, '.$' 
//          backspace(unit = unit_no, err = 980) 

	}

// check we didn't enter too many layers 
	if (l_cnt > 5000) {
//          write(op,104) 'WARNING: No. of EXPLICIT layers, ',l_cnt, 
//     |      ' exceeds maximum of ',XP_MAX 
//          write(op,106) XP_MAX, ' EXPLICIT layers assumed' 
	    l_cnt = 5000;
	}

// check that all the layer numbers are within bounds 
	if (! rndm) {
	    i1 = l_cnt;
	    for (i = 0; i < i1; i++) {
		if (l_seq[i - 1] > n_layers) {
//              write(messge,102) 'Illegal layer number,', l_seq(i), 
//     |         '. Number must not exceed', n_layers, '.$' 
		    return ret_val;
		} else if (l_seq[i - 1] < 1) {
//              write(messge,103) 'Illegal layer number,', l_seq(i), 
//     |         '. Number must be greater than 0.$' 
		    return ret_val;
		}
// L30: 
	    }
	}
    }

    ret_val = true;
    return ret_val;
//  990 messge = 'Problems using scratch file in RDSTAK.$' 
//  999 call WRTLNE(line) 
//      return 
//  100 format(1x, 'ERROR: ', a) 
//  101 format(1x, 2a) 
//  102 format(2(a, i4), a) 
//  103 format(a, i2, a) 
//  104 format(1x, 2(a, i4)) 
//  105 format(1x, a) 
//  106 format(1x, i4, a) 
//  107 format(1x, a, i5) 
} // rdstak_ 



//  
// Title: RDTRNS 
// Authors: MMJT 
// Date: 4 Oct 1989; 15 Mar 1995; 29th Aug 2000 
// Description: This function reads the layer stacking probabilities 
// and stacking vectors. 

//      ARGUMENTS: 
//            unit_no  -  Logical unit number that the data file is to 
//                        be read from. (input). 
//            messge   -  A short message indicating what went wrong 
//                        (if anything) during the datafile read. 
//                        messge is terminated by a token '$'. (output). 

//      COMMON VARIABLES: 
//            uses:  n_layers, recrsv, rndm, xplcit 

//        modifies:  l_alpha, l_r, r_B11, r_B22, r_B33, r_B12, r_B23, 
//                   r_B31 

//      RDTRNS returns boolean .true. if all the stacking 
//      parameters were properly specified. 
//  

boolean rdtrns_(int *unit_no, char *messge, ftnlen messge_len)
{
    // System generated locals 
    int i1, i2;
    double d1;
    boolean ret_val;

    // Local variables 
    int tempfact, i, j, n;
    boolean ok;
    char arg[200];
    double sum;
    char line[200];
    char tmpline[200];

    ret_val = false;

//      call GETLNE(unit_no, line, *999) 
    touppr_(line, (ftnlen)200);
    if (i_indx(line, "TRANSITIONS ", (ftnlen)200, (ftnlen)12) != 1) {
	s_copy(messge, "'TRANSITIONS' section not found.$", messge_len, (
		ftnlen)33);
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    }

    i1 = n_layers;
    for (i = 0; i < i1; i++) {
	i2 = n_layers;
	for (j = 0; j < i2; j++) {
//          write(messge,100) 
//     |     'Unexpected EOF at vector ',i,j,' in ''TRANSITIONS''.$' 
//          call GETLNE(unit_no, line, *999) 
// Track down if there are parentheses in this line. If so then there 
// are FatsWaller (Debye-Waller for layers!) factors 
//          write(messge,100) 
//     |      'Confusing use of parentheses in data for vector ',i,j,'.$' 
	    tempfact = parent_(line, (ftnlen)200);
	    if (tempfact < 0) {
		wrtlne_(line, (ftnlen)200);
//        goto 999 
		return ret_val;
	    }
// initialize 
	    l_r[(j + i * 20) * 3 - 63] = 0.;
	    l_r[(j + i * 20) * 3 - 62] = 0.;
	    l_r[(j + i * 20) * 3 - 61] = 0.;
	    r_b11[j + i] = 0.;
	    r_b22[j + i] = 0.;
	    r_b33[j + i] = 0.;
	    r_b12[j + i] = 0.;
	    r_b31[j + i] = 0.;
	    r_b23[j + i] = 0.;

	    s_copy(tmpline, line, (ftnlen)200, (ftnlen)200);

	    if (tempfact == 1) {
//            write(messge,100) 
//     |     'Expected alpha_ij, R_ij, (dR_ij) for vector ',i,j,'.$' 
	    } else {
//            write(messge,100) 
//     |     'Expected alpha_ij, R_ij data for vector ',i,j,'.$' 
	    }

// read stacking probability 
	    n = nxtarg_(tmpline, arg, (ftnlen)200, (ftnlen)200);
	    if (n <= 0) {
		wrtlne_(line, (ftnlen)200);
//        goto 999 
		return ret_val;
	    }
	    l_alpha[j + i] = rdnmbr_(arg, &ok, (ftnlen)
		    200);
	    if (! ok) {
		wrtlne_(line, (ftnlen)200);
//        goto 999 
		return ret_val;
	    }
// If zero, there is no need to read further 
	    if ((d1 = l_alpha[j + i], Math.abs(d1)) >=
		    1e-6) {

// read stacking x-vector 
		n = nxtarg_(tmpline, arg, (ftnlen)200, (ftnlen)200);
		if (n <= 0) {
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}
		l_r[(j + i * 20) * 3 - 63] = rdnmbr_(arg, &ok, (
			ftnlen)200);
		if (! ok) {
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}

// read stacking y-vector 
		n = nxtarg_(tmpline, arg, (ftnlen)200, (ftnlen)200);
		if (n <= 0) {
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}
		l_r[(j + i * 20) * 3 - 62] = rdnmbr_(arg, &ok, (
			ftnlen)200);
		if (! ok) {
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}

// read stacking z-vector 
		n = nxtarg_(tmpline, arg, (ftnlen)200, (ftnlen)200);
		if (n <= 0) {
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}
		l_r[(j + i * 20) * 3 - 61] = rdnmbr_(arg, &ok, (
			ftnlen)200);
		if (! ok) {
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}

// Read temperature factors, if present 
		if (tempfact == 1) {

// read B11 "Fats-Waller" 
		    n = nxtarg_(tmpline, arg, (ftnlen)200, (ftnlen)200);
		    if (n <= 0) {
			wrtlne_(line, (ftnlen)200);
//        goto 999 
			return ret_val;
		    }
		    r_b11[j + i] = rdnmbr_(arg, &ok, (
			    ftnlen)200);
		    if (! ok) {
			wrtlne_(line, (ftnlen)200);
//        goto 999 
			return ret_val;
		    }

// read B22 "Fats-Waller" 
		    n = nxtarg_(tmpline, arg, (ftnlen)200, (ftnlen)200);
		    if (n <= 0) {
			wrtlne_(line, (ftnlen)200);
//        goto 999 
			return ret_val;
		    }
		    r_b22[j + i] = rdnmbr_(arg, &ok, (
			    ftnlen)200);
		    if (! ok) {
			wrtlne_(line, (ftnlen)200);
//        goto 999 
			return ret_val;
		    }

// read B33 "Fats-Waller" 
		    n = nxtarg_(tmpline, arg, (ftnlen)200, (ftnlen)200);
		    if (n <= 0) {
			wrtlne_(line, (ftnlen)200);
//        goto 999 
			return ret_val;
		    }
		    r_b33[j + i] = rdnmbr_(arg, &ok, (
			    ftnlen)200);
		    if (! ok) {
			wrtlne_(line, (ftnlen)200);
//        goto 999 
			return ret_val;
		    }

// read B12 "Fats-Waller" 
		    n = nxtarg_(tmpline, arg, (ftnlen)200, (ftnlen)200);
		    if (n <= 0) {
			wrtlne_(line, (ftnlen)200);
//        goto 999 
			return ret_val;
		    }
		    r_b12[j + i] = rdnmbr_(arg, &ok, (
			    ftnlen)200);
		    if (! ok) {
			wrtlne_(line, (ftnlen)200);
//        goto 999 
			return ret_val;
		    }

// read B31 "Fats-Waller" 
		    n = nxtarg_(tmpline, arg, (ftnlen)200, (ftnlen)200);
		    if (n <= 0) {
			wrtlne_(line, (ftnlen)200);
//        goto 999 
			return ret_val;
		    }
		    r_b31[j + i] = rdnmbr_(arg, &ok, (
			    ftnlen)200);
		    if (! ok) {
			wrtlne_(line, (ftnlen)200);
//        goto 999 
			return ret_val;
		    }

// read B23 "Fats-Waller" 
		    n = nxtarg_(tmpline, arg, (ftnlen)200, (ftnlen)200);
		    if (n <= 0) {
			wrtlne_(line, (ftnlen)200);
//        goto 999 
			return ret_val;
		    }
		    r_b23[j + i] = rdnmbr_(arg, &ok, (
			    ftnlen)200);
		    if (! ok) {
			wrtlne_(line, (ftnlen)200);
//        goto 999 
			return ret_val;
		    }

		}

// stacking probabilities should not be negative. 
		if (l_alpha[j + i] < 0.) {
//            write(messge,100) 'alpha',i,j,' is negative.$' 
		    wrtlne_(line, (ftnlen)200);
//        goto 999 
		    return ret_val;
		}
// the Rz(i,i) vectors should not be zero or negative. 
// if an i-i transition exists 
		if (j == i) {
		    if (l_alpha[i + i] != 0.) {
			if (l_r[(i + i * 20) * 3 - 61] <= 0.) {
//                write(messge,100) 
//     |'Invalid negative (or zero) z-component for Rz',i,i,'.$' 
			    wrtlne_(line, (ftnlen)200);
//        goto 999 
			    return ret_val;
			}
		    }
		}
	    }
// L20: 
	}
// L10: 
    }

// Check stacking probabilities. 
// Generate error message if they do not sum to 1, and we are using 
// a RECURSIVE stacking description. If EXPLICIT stacking, then we 
// do not use the probabilities (but we still assume they are there 
// to be read) but generate a warning. 

    i1 = n_layers;
    for (i = 0; i < i1; i++) {
	sum = 0.;
	i2 = n_layers;
	for (j = 0; j < i2; j++) {
	    sum += l_alpha[j + i];
// L40: 
	}
	if ((d1 = sum - 1., Math.abs(d1)) > 1e-6) {
//          write(messge,101) 
//     |     'Stacking probabilities from LAYER ',i,' do not sum to 1.$' 
	    if (recrsv || rndm) {
		return ret_val;
	    }
//          if(xplcit .and. .not.rndm) then 
//            write(op,102) messge(1:index(messge, '$') - 1) 
//          endif 
	}
// L30: 
    }

    ret_val = true;
    return ret_val;
//  999 call WRTLNE(line) 
//      return 
//  100 format(a, '(', i2, ',', i2, ')', a) 
//  101 format(a, i2, a) 
//  102 format(1x, 'WARNING: ', a) 
} // rdtrns_ 



//  
// Title: RDWAVL 
// Authors: MMJT 
// Date: 4 Oct 1989 
// Description: This function reads the radiation wavelength. 

//      ARGUMENTS: 
//            unit_no  -  Logical unit number that the data file is to 
//                        be read from. (input). 
//            messge   -  A short message indicating what went wrong 
//                        (if anything) during the datafile read. 
//                        messge is terminated by a token '$'. (output). 

//      COMMON VARIABLES: 
//        modifies:  lambda 

//      RDWAVL returns boolean .true. if an acceptable value 
//      for the radiation wavelength was read. 
//  

boolean rdwavl_(int *unit_no, char *messge, ftnlen messge_len)
{
    // System generated locals 
    boolean ret_val;

    // Local variables 
    char line[200];

    ret_val = false;

    s_copy(messge, "lambda is specified improperly.$", messge_len, (ftnlen)32)
	    ;
//      call GETLNE(unit_no, line, *999) 

// for strict FORTRAN77 compliance 
//      write(scrtch, *, err=990) line 
//      rewind(scrtch, err=990) 
//      read(scrtch, *, err=999) lambda 
//      rewind(scrtch, err=990) 

// for more lenient, efficient compilers 
//      read(line, *, err=999) lambda 

    if (lambda <= 0.) {
	s_copy(messge, "lambda is negative (or zero).$", messge_len, (ftnlen)
		30);
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    }

    ret_val = true;
    return ret_val;
//  990 messge = 'Problems using scratch file in RDWAVL.$' 
//  999 call WRTLNE(line) 
//      return 
} // rdwavl_ 



//  
// Title: RDWDTH 
// Authors: MMJT 
// Date: 3 Mar 1995 
// Description: This function reads the lateral layer dimensions 
// from the data file. First we check that the data on the line does 
// not refer to the layer atomic positions. If it does, assume infinite 
// widths, backspace the file, and return to let RDLAYR do its job. 
// Unlike the other read functions, RDWDTH does not return an error in 
// this instance, so as to maintain compatibility with data files 
// written for v 1.7xx which did not treat lateral layer width 
// broadening. 

//      ARGUMENTS: 
//            unit_no  -  Logical unit number that the data file is to 
//                        be read from. (input). 
//            messge   -  A short message indicating what went wrong 
//                        (if anything) during the datafile read. 
//                        messge is terminated by a token '$'. (output). 

//      COMMON VARIABLES: 
//        modifies:  finite_width, Wa, Wb 

//      RDWAVL returns boolean .true. if no errors were encountered. 
//  

boolean rdwdth_(int *unit_no, char *messge, ftnlen messge_len)
{
    // System generated locals 
    boolean ret_val;

    // Local variables 
    int n;
    char line[200];

    ret_val = false;

// Does the next line belong to the layer structural data? If so, 
// assume infinite layer widths. No error message is generated so 
// that there is compatibility with data files for versions 1.7xx. 
    s_copy(messge, "layer width, or layer data, are specified improperly.$",
	    messge_len, (ftnlen)54);
//      call GETLNE(unit_no, line, *999) 
    touppr_(line, (ftnlen)200);
    if (i_indx(line, "LAYER", (ftnlen)200, (ftnlen)5) == 1) {
	finite_width = false;
//        write(messge,100) 
//     |   'Problems ''backspacing'' unit ',unit_no, '.$' 
// backspace file so that RDLAYR will work properly 
//        backspace(unit = unit_no, err = 990) 
	ret_val = true;
//        goto 900 
	return ret_val;
    }

// assume that layer width data is specified 
    if (i_indx(line, "INFINITE", (ftnlen)200, (ftnlen)8) == 1) {
	finite_width = false;
	ret_val = true;
//        goto 900 
	return ret_val;
    } else {
	finite_width = true;
    }

// are both Wa and Wb specified, or just a generic width? 
    s_copy(messge, "layer characteristic widths are specified improperly.$",
	    messge_len, (ftnlen)54);
    n = cntarg_(line, (ftnlen)200);

// for strict FORTRAN77 compliance 
//      write(scrtch, *, err=990) line 
//      rewind(scrtch, err=990) 

    if (n == 1) {
// for strict FORTRAN77 compliance 
//        read(scrtch, *, err=999) Wa 
//        rewind(scrtch, err=990) 
// for more lenient, efficient compilers 
//        read(line, *, err=999) Wa 
	wb = wa;
    } else if (n == 2) {
// for strict FORTRAN77 compliance 
//        read(scrtch, *, err=999) Wa, Wb 
//        rewind(scrtch, err=990) 
// for more lenient, efficient compilers 
//        read(line, *, err=999) Wa, Wb 
    } else {
	wrtlne_(line, (ftnlen)200);
//        goto 999 
	return ret_val;
    }

    if (finite_width) {
	if (wa > 1e4 && wb > 1e4) {
	    finite_width = false;
//          write(op,101) 'Layers will be treated as if infinitely wide.' 
	}
	if (wa <= 0. || wb <= 0.) {
//          write(op,101)'Illegal values for layer characteristic widths' 
	    wrtlne_(line, (ftnlen)200);
//        goto 999 
	    return ret_val;
	}
    }

    ret_val = true;
    return ret_val;
//  990 messge = 'Problems using scratch file in RDWDTH.$' 
//  999 call WRTLNE(line) 
//      return 
//  100 format(a, i2, a) 
//  101 format(1x, a) 
} // rdwdth_ 



//  
// Title: RAN3 
// Authors: Press, Flannery, Teukolsky and Vetterling 
// Date: Copyright (C) 1985 
// Returns a uniform random deviate between 0.0 and 1.0. Set 'idum' 
// to any negative value to initialize or reinitialize the sequence. 
// This version is modified to return real*8 values, and enforces 
// storage of all local variables by use of the 'save' statement 
// (In fact 'seed' is the important variable to save, but we save all 
// anyway). 

//      ARGUMENTS: 
//            idum       -  Set -ve to initialize. (input). 

//      RAN3 returns a real random number between 0 and 1 
//  

double ran3_(int *idum)
{
    // Initialized data 

    int iff = 0;

    // System generated locals 
    double ret_val;

    // Local variables 
    int i, j;
    double ma[55];
    int ii;
    double mj, mk;
    int inext, inextp;

    if (*idum < 0 || iff == 0) {
	iff = 1;
	mj = 1618033. - Math.abs(*idum);
	mj = d_mod(&mj, &c_b592);
	ma[54] = mj;
	mk = 1.;
	for (i = 1; i <= 54; ++i) {
	    ii = i * 21 % 55;
	    ma[ii - 1] = mk;
	    mk = mj - mk;
	    if (mk < 0.) {
		mk += 4e6;
	    }
	    mj = ma[ii - 1];
// L11: 
	}
	for (j = 1; j <= 4; ++j) {
	    for (i = 1; i <= 55; ++i) {
		ma[i - 1] -= ma[(i + 30) % 55];
		if (ma[i - 1] < 0.) {
		    ma[i - 1] += 4e6;
		}
// L12: 
	    }
// L13: 
	}
	inext = 0;
	inextp = 31;
	*idum = 1;
    }

    ++inext;
    if (inext == 56) {
	inext = 1;
    }
    ++inextp;
    if (inextp == 56) {
	inextp = 1;
    }
    mj = ma[inext - 1] - ma[inextp - 1];
    if (mj < 0.) {
	mj += 4e6;
    }
    ma[inext - 1] = mj;
    ret_val = mj * 2.5e-7;

    return ret_val;
} // ran3_ 


//  
// Title: SALUTE 
// Author: MMJT 
// Date: 3rd July, 2005 
// Draws the DIFFaX flag. Saluting is optional. 

//      ARGUMENTS: 
//           No arguments are needed. 
//  

void salute()
{
  System.out.println("***************************************************");
  System.out.println("***************************************************");
  System.out.println("*                                                 *");
  System.out.println("*  DDDD     II   FFFFFF   FFFFFF           X   X  *");
  System.out.println("*  D   D    II   F        F        aaaa     X X   *");
  System.out.println("*  D    D   II   FFFF     FFFF    a    a     X    *");
  System.out.println("*  D   D    II   F        F       a   aa    X X   *");
  System.out.println("*  DDDD     II   F        F        aaa a   X   X  *");
  System.out.println("*                                                 *");
  System.out.println("***************************************************");
  System.out.println("****************** DIFFaX v1.812 ******************");
  System.out.println("***************************************************");
  System.out.println("***************** 3rd July,  2005 *****************");
  System.out.println("***************************************************");
  System.out.println("*                                                 *");
  System.out.println("*   A computer program for calculating            *");
  System.out.println("*   Diffraction Intensity From Faulted Crystals   *");
  System.out.println("*                                                 *");
  System.out.println("*   Authors: Michael M. J. Treacy                 *");
  System.out.println("*            Michael W. Deem                      *");
  System.out.println("*                                                 *");
  System.out.println("***************************************************");
  System.out.println();

    return;
} // salute 


//  
// Title: SFC 
// Authors: MWD and MMJT 
// Date: 11 Feb 90; 7 Mar 1995 
// Description: This routine reads the atomic scattering factor constants 
// from the file 'sfname' (usually given as "data.sfc"). The scattering 
// factors are used to generate the layer form factors. SFC returns 
// .false. if there are too many distinct atom types (ie. more than 
// MAX_TA). The value of MAX_TA can be reset in 'DIFFaXj.par'. 

//      ARGUMENTS: 
//           No arguments are used. All data is in 'COMMON'. 

//      COMMON VARIABLES: 
//            uses:  n_actual, l_n_atoms, a_name, n_atoms, rad_type 
//                   NEUTRN, X_RAY, rad_type, sfname 

//        modifies:  n_atoms, x_sf, n_sf, e_sf, a_type 

//      SFC returns boolean .true. if it read the scattering factor data 
//      safely. 
//  

boolean sfc(void)
{
    // System generated locals 
    int i1, i2;
    boolean ret_val;

    // Local variables 
    boolean new_atom, our_atom;
    int i, j, m, n;
    boolean ok;
    char name[4];
    boolean done;
    char line[120];
    boolean list[21];

//      write(op,301) 'Reading scattering factor datafile ''', 
//     |                   sfname(1:LENGTH(sfname)),'''. . .' 

    ret_val = false;
    for (i = 1; i <= 20; ++i) {
	list[i - 1] = false;
	s_copy(chars1_1.atom_l + (i - 1 << 2), " ", (ftnlen)4, (ftnlen)1);
// L10: 
    }
    m = 0;
// determine all distinct atom types 
    i1 = n_actual;
    for (i = 0; i < i1; i++) {
	i2 = l_n_atoms[i - 1];
	for (j = 0; j < i2; j++) {
	    s_copy(name, chars1_1.a_name + (j + i * 200 - 201 << 2), (
		    ftnlen)4, (ftnlen)4);
	    touppr_(name, (ftnlen)4);
// see if this name has been seen already 
	    n = 0;
	    new_atom = true;
	    while(new_atom && n < m) {
		++n;
		new_atom = s_cmp(name, chars1_1.atom_l + (n - 1 << 2), (
			ftnlen)4, (ftnlen)4) != 0;
	    }
	    if (new_atom) {
		++m;
		if (m > 20) {
		    return ret_val;
		}
//            goto 220 
		s_copy(chars1_1.atom_l + (m - 1 << 2), name, (ftnlen)4, (
			ftnlen)4);
		a_type[j + i * 200 - 201] = m;
	    } else {
		a_type[j + i * 200 - 201] = n;
	    }
// L40: 
	}
// L30: 
    }
    n_atoms = m;
// now find data for each atom type in file 
// pass through file only once 
    done = false;
    while(! done) {
//   60 read(sf, 300, end=90, err=200) line 
	s_copy(name, line, (ftnlen)4, (ftnlen)4);
	touppr_(name, (ftnlen)4);
// see if this is one of the distinct atoms 
	i = 0;
	our_atom = false;
	while(! our_atom && i < n_atoms) {
	    ++i;
	    our_atom = s_cmp(name, chars1_1.atom_l + (i - 1 << 2), (
		    ftnlen)4, (ftnlen)4) == 0;
	}
// have we read all that we need to know? 
	done = true;
	i1 = n_atoms;
	for (n = 1; n <= i1; ++n) {
	    done = done && list[n - 1];
// L80: 
	}
// If we're done, close file. 
	if (! done) {
	    if (our_atom && ! list[i - 1]) {
// mark this atom's data as read in 
		list[i - 1] = true;
// and read it in 
		if (rad_type == x_ray) {
//            read(line,310,err=210) (x_sf(j,i), j=1, 9) 
		} else if (rad_type == neutrn) {
//            read(line,320,err=210) n_sf(i) 
		} else {
//            read(line,310,err=210) (x_sf(j,i), j=1, 9), tmp, e_sf(i) 
		}
	    }
	}
    }
//   90 close(sf,err=240) 
// see if all the data for each atom has been read in 
    ok = true;
    i1 = n_atoms;
    for (i = 0; i < i1; i++) {
	if (! list[i - 1]) {
	    ok = false;
//          write(op,330) 'ERROR: Data for atom ''', atom_l(i), 
//     |     ''' not found in file ''', sfname(1:LENGTH(sfname)), '''' 
	    atoms_();
	}
// L100: 
    }
    if (ok) {
	ret_val = true;
//        write(op,400) 'Scattering factor data read in.' 
    }
    return ret_val;
//  200 write(op,301) 'Scattering factor file ''', 
//     |    sfname(1:LENGTH(sfname)), ''' defective.' 
//      close(sf,err=240) 
//      return 
//  210 write(op,400) 'ERROR reading scattering factor data.' 
//      close(sf,err=240) 
//      return 
//  220 write(op,402) 'There are too many types of atoms in layer ', i 
//      write(op,400) 'Atoms recorded so far are:' 
//      do 110 j = 1, MAX_TA 
//        write(op,403) '       AtomSite type ', j, '   ', atom_l(j)
//  110 continue 
//      write(op,402) '  Maximum number of types allowed is ', MAX_TA 
//      return 
//  240 write(op,301) 'Unable to close scattering factor file ''', 
//     |    sfname(1:LENGTH(sfname)), '''.' 
//      return 
//  300 format(a) 
//  301 format(1x, 3a) 
//  310 format(t5, 10f11.6, i3) 
//  320 format(t104, f11.6) 
//  330 format(1x, 5a) 
//  400 format(1x, a) 
//  401 format(1x, 2a) 
//  402 format(1x, a, i2) 
//  403 format(1x, a, i2, 2a) 
} // sfc_ 



//  
// Title: SHARP 
// Author: MMJT 
// Date: 29 Aug 1991 
// Description: This subroutine determines whether or not 
// spots on a given h, k row are sharp. It does this by examining 
// the intensity at l = 0 (or, if absent, at l = d_l) and comparing 
// with the intensity at a nearby l-value. If the peak is sharp, there 
// will be a large change in intensity. 

//      ARGUMENTS: 
//            h    -  reciprocal vector h-component. (input). 
//            k    -  reciprocal vector k-component. (input). 
//            d_l  -  steps in l-component of the reciprocal lattice 
//                    vector that sharp spots are likely to be found at. 
//                                                            (input). 

//      COMMON VARIABLES: 
//            uses:  a0, b0, c0, d0, lambda, PI 

//        modifies:  no COMMON variables are modified 

//      SHARP returns boolean .true. if it thinks h, k contains sharp 
//      spots. 
//  

boolean sharp_(int *h, int *k, double *d_l)
{
    // System generated locals 
    double d1, d2;
    boolean ret_val;

    // Local variables 
    double l, x, i1, i2;
    boolean ok;
    double theta, l_next;
    boolean loopon;

// statement functions 
// S is the value of 1/d**2 at hkl 
// ANGLE is the Bragg angle (in radians) of the h,k,l plane 
// LL is the maximum allowable l value for a given h,k and theta 

    ret_val = false;

// get the intensity at hkl, with l = 0 initially 
    l = 0.;
    loopon = true;
    while(loopon) {
	loopon = false;
	i1 = pntint_(h, k, &l, &ok);
	if (! ok) {
	    return ret_val;
	}
// If there is an extinction at hkl, try again at l = l + d_l 
	if (i1 < 1e-4) {
	    l += *d_l;
	    if (Math.asin(.5 * lambda * Math.sqrt(*h * *h * a0 + *
		    k * *k * b0 + l * l * c0 + *h * *k *
		    d0)) < pi * .5) {
		loopon = true;
	    } else {
		return ret_val;
	    }
	}
    }

// Define a spot to be sharp if intensity is halved at l = l + d_l/100 
    theta = Math.asin(.5 * lambda * Math.sqrt(*h * *h * a0 + *k *
	    *k * b0 + l * l * c0 + *h * *k * d0))
	    ;
// Computing MIN 
    d1 = d_theta, d2 = th2_max * .5 - theta;
    x = min(d1,d2);
    d1 = theta + x;
// Computing 2nd power 
    d2 = 2. * Math.sin(d1) / lambda;
    l_next = Math.sqrt((d2 * d2 - *h * *h * a0 - *k * *k *
	    b0 - *h * *k * d0) / c0);
    d1 = l + l_next * .01;
    i2 = pntint_(h, k, &d1, &ok);
    if (! ok) {
	return ret_val;
    }

    ret_val = i1 > i2 * 2.;

    return ret_val;
} // sharp_ 



//  
// Title: SMUDGE 
// Author: MMJT 
// Date: 30 Oct 1989; 16 April 1999 
// Description: This subroutine convolutes 'array', of length 
// 'arrsize' with a Gaussian of standard deviation 'sigma'. 
// NOTE: This routine does not conserve area under the convoluted curve. 
// It is designed so that the peak heights are unchanged. 

//      ARGUMENTS: 
//            array    -  The name of the input array. (input). 
//            arrsize  -  Size of the input array. (input). 
//            sigma    -  Standard deviation of the Gaussian as a 
//                        multiple of the array sampling step. (input). 
//            ok       -  boolean flag indicating all went well. 
//                                                      (output). 
//  

int smudge_(double *array, int *arrsize, double *
	sigma, boolean *ok)
{
    // System generated locals 
    int i1, i2;
    double d1;

    // Local variables 
    int i, j, m;
    double k1, k2, normalize, gss, tmp, tmp1, tmp2;
    double tmparr[256];

    // Parameter adjustments 
    --array;

    // Function Body 
    if (*sigma == 0.) {
	return 0;
    }

    i1 = *arrsize;
    for (i = 0; i < i1; i++) {
	if (array[i] > (double) maxsad) {
	    array[i] = (double) maxsad;
	}
	tmparr[i - 1] = array[i];
	array[i] = 0.;
// L10: 
    }

// go out to 5 standard deviations, or to the end of the spectrum 
    d1 = *sigma * 5.;
    m = i_dnnt(&d1);
    k1 = .5 / (*sigma * *sigma);
    if (m > *arrsize) {
	m = *arrsize;
    }
// get normalization constant. We wish peak heights to be unchanged. 
    normalize = 1.;
    i1 = m;
    for (i = 0; i < i1; i++) {
	normalize += Math.exp(-k1 * (double) (i * i)) * 2.;
// L20: 
    }

    if (normalize == 0.) {
//        write(op,100) 'ERROR in SMUDGE: Zero normalization constant.' 
	*ok = false;
	return 0;
    }
    normalize = 1. / normalize;

    i1 = m;
    for (i = 0; i <= i1; ++i) {
	k2 = k1 * (double) (i * i);
	gss = Math.exp(-k2);
	i2 = *arrsize;
	for (j = 0; j < i2; j++) {
	    tmp1 = 0.;
	    tmp2 = 0.;
	    if (j - i > 0) {
		tmp1 = tmparr[j - i - 1];
	    }
	    if (j + i <= *arrsize) {
		tmp2 = tmparr[j + i - 1];
	    }
	    tmp = tmp1 + tmp2;
	    if (i == 0) {
		tmp *= .5;
	    }
	    array[j] += gss * tmp * normalize;
// L40: 
	}
// L30: 
    }

    return 0;
//  100 format(1x, a) 
} // smudge_ 



//  
// Title: SPHCST 
// Author: MWD 
// Date: 18 Aug 1988 
// Description: This subroutine determines the constants used in 
// determining the magnitude of reciprocal lattice vectors. 

//      ARGUMENTS: 
//            No input arguments. 

//      COMMON VARIABLES: 
//            uses:  cell_a, cell_b, cell_c, cell_gamma 

//        modifies:  a0, b0, c0, d0, ab0, bc0, ca0 
//  

int sphcst()
{
    // System generated locals 
    double d1;

    // Local variables 

// Computing 2nd power 
    d1 = cell_a * Math.sin(cell_gamma);
    a0 = 1. / (d1 * d1);
// Computing 2nd power 
    d1 = cell_b * Math.sin(cell_gamma);
    b0 = 1. / (d1 * d1);
// Computing 2nd power 
    d1 = cell_c;
    c0 = 1. / (d1 * d1);
// Computing 2nd power 
    d1 = Math.sin(cell_gamma);
    d0 = Math.cos(cell_gamma) * -2. / (cell_a *
	    cell_b * (d1 * d1));

    ab0 = Math.sqrt(a0 * b0);
    bc0 = Math.sqrt(b0 * c0);
    ca0 = Math.sqrt(c0 * a0);

    return 0;
} // sphcst_ 



//  
// Title: STREAK 
// Author: MWD & MMJT 
// Date: 18 Aug 1988, revised 22 June 1989. 
// Description:  This routine outputs the integrated (ie. averaged) 
// intensitites from h,k,l0 to h,k,l1, in steps of dl, to the file 
// strkfile. 

//      ARGUMENTS: 
//            FN        -  Function name passed by reference. The 
//                         choice is between GLQ16 (non-adaptive 
//                         Gauss-Legendre integration), and AGLQ16 
//                         (adaptive Gauss-Legendre integration). 
//                                                            (input). 
//            strkfile  -  The name of the file that streak data is 
//                         to be output to. (input). 
//            ok        -  boolean flag indicating all went well. 
//                                                      (output). 

//      COMMON VARIABLES: 
//            uses:  a0, b0, c0, d0, lambda, cntrl, CFile, xplcit, 
//                   rad_type, X_RAY 

//        modifies:  no COMMON variables are modified 
//  

int streak_(D_fp fn, char *strkfile, boolean *ok, ftnlen
	strkfile_len)
{
    // System generated locals 
    double d1, d2, d3, d4, d5;

    // Local variables 
    int h, i, k;
    double l, x, l0, l1, q2, dl;
    int i_step;
    boolean loopon;
    boolean its_hot;

// statement functions 
// S is the value of 1/d**2 at hkl 
// ANGLE is the Bragg angle (in radians) of the h,k,l plane 
// W4 is the X-ray polarization factor 

// Computing 2nd power 
    d1 = lambda;
    q2 = 4. / (d1 * d1);
    loopon = true;
    while(loopon) {
	loopon = false;
//   10 write(op,400) 'Enter h, k, l0, l1, delta l' 
//      read(cntrl,*,err=10) h, k, l0, l1, dl 
//      if(CFile) write(op,401) h, k, l0, l1, dl 
// check input 
	if (l1 == l0) {
//        write(op,400) 'Illegal input: l0 equals l1' 
	    return 0;
//        goto 999 
	} else if (dl == 0.) {
//        write(op,400) 'Illegal zero value of dl entered' 
//        write(op,402)'A value of ',(l1-l0)/(FIVE*HUNDRED),' is assumed' 
	} else if (l1 > l0 && dl < 0.) {
//        write(op,400) 'l1 is greater than l0. +ve dl assumed' 
	    dl = -dl;
	} else if (l1 < l0 && dl > 0.) {
//        write(op,400) 'l0 is greater than l1. -ve dl assumed' 
	    dl = -dl;
	}
// The origin may be hotter than hell! Let's check first. 
	its_hot = h == 0 && k == 0 && l0 * l1 <= 0. &&
		rad_type == electn;
//      if(its_hot) then 
//        write(op,400) 'Cannot scan the origin with electron radiation' 
//        write(op,400) 'Origin will be skipped.' 
//      endif 
// check angles are meaningful 
	if (h * h * a0 + k * k * b0 + l0 * l0 *
		c0 + h * k * d0 > q2 || h * h *
		a0 + k * k * b0 + l1 * l1 * c0 +
		h * k * d0 > q2) {
	    loopon = true;
	}
    }
//       then 
//        if(S(h,k,l0).gt.Q2) write(op,403) h, k, l0, 
//    |            ' exceeds 180 degree scattering angle!' 
//        if(S(h,k,l1).gt.Q2) write(op,403) h, k, l1, 
//     |            ' exceeds 180 degree scattering angle!' 
//        goto 10 
//      endif 

//      write(op,404) 'Writing streak data to file ''', 
//     |      strkfile(1:LENGTH(strkfile)),'''. . .' 
    xyphse_(&h, &k);
    pre_mat(&h, &k);

    d1 = (l1 - l0) / (dl * 20.);
    i_step = i_dnnt(&d1);
// Immune system to the rescue! 
    if (i_step <= 0) {
	i_step = 10;
    }
    i = 0;
    d1 = l1;
    d2 = dl;
    for (l = l0; d2 < 0 ? l >= d1 : l <= d1; l += d2) {
// If we are dealing with electrons, make sure we avoid the origin 
	if (its_hot && l * (l + dl) <= 0.) {
	    x = 0.;
	} else {
	    ++i;
//        if(mod(i,i_step).eq.0) write(op,405) 'Reached l = ',l 
	    d3 = l + dl;
	    x = (*fn)(&h, &k, &l, &d3, ok);
	    if (! (*ok)) {
		return 0;
	    }
//        goto 999 
// note: since this is streak data, only the X_RAY input needs 
// correcting for polarization. 
	    if (rad_type == x_ray) {
		d3 = l + dl * .5;
		d4 = Math.asin(.5 * lambda * Math.sqrt(h * h *
			a0 + k * k * b0 + d3 * d3 *
			c0 + h * k * d0));
// Computing 2nd power 
		d5 = Math.cos(2. * d4);
		x *= .5 * (1. + d5 * d5);
	    }
	}
//   30   write(sk,406,err=100) l, char(9), x 
// L20: 
    }
//      if(sk.ne.op) close(sk,err=110) 
//      write(op,404) 'Streak data file, ''', 
//     |      strkfile(1:LENGTH(strkfile)),''' written to disk.' 
    return 0;
//  100 write(op,404) 'ERROR writing to streak data file ''', 
//     |      strkfile(1:LENGTH(strkfile)),'''' 
//      if(sk.ne.op) close(sk,err=110) 
//      return 
//  110 write(op,404) 'Unable to close streak data file ''', 
//     |      strkfile(1:LENGTH(strkfile)),'''' 
//      return 
//  999 write(op,405) 'ERROR encountered in streak integration at l = ',l 
//      return 
//  400 format(1x, a) 
//  401 format(1x, 2i3, 3f10.5) 
//  402 format(1x, a, f10.5, a) 
//  403 format(1x, 2i3, f10.5, a) 
//  404 format(1x, 3a) 
//  405 format(1x, a, f10.5) 
//  406 format(1x, e12.5, a, e14.6) 
} // streak_ 



//  
// Title: THRESH 
// Author: MMJT 
// Date: 21 Jan 1995 
// Samples intensities in reciprocal space to get a "feeling" for what 
// kind of values are out there prior to testing diffraction symmetry. 
// CHK_SYM and GET_SYM measure the fractional deviations of intensity 
// from (potentially) symmetry-related points in reciprocal space. 
// This method runs into problems when the intensity being measured 
// is close to zero. Miniscule intensity variations can appear to be 
// huge relative to zero! 
// This function is needed in order to obtain a (crude) estimate of 
// which intensity values are too small to worry about, even if the 
// relative intensity variations seem to be large. 
// This function will be of no use if there are no streaks, that is, 
// if the crystal is perfect. 

//      ARGUMENTS: 
//            ok       -  boolean flag indicating all went well. 
//                                                      (output). 

//      COMMON VARIABLES: 
//            uses:  a0, b0, c0, d0, lambda, no_trials, 
//                   max_angle 

//        modifies:  ok, max_angle, h_bnd, k_bnd, l_bnd, tiny_inty 

//  

int thresh_(boolean *ok)
{
    // System generated locals 
    int i1;

    // Local variables 
    int h, i, k;
    double l;
    int idum;
    boolean loopon;
    double tot_int;

// statement functions 
// S is the value of 1/d**2 at hkl 

// initialize random numbers in RAN3 
    idum = -1;

// First define angular range to sample. h_bnd, k_bnd, l_bnd 
// (defined in HKL_LIM) and max_angle, are used later on 
// in GET_SYM and CHK_SYM. 
    max_angle = pi * .25;
    hkl_lim();

// Sample the typical intensities that are out there 
    tot_int = 0.;
    i1 = no_trials;
    for (i = 0; i < i1; i++) {
	loopon = true;
	while(loopon) {
	    loopon = false;
	    h = (int) ((double) (h_bnd + 1) * ran3_(&
		    idum));
	    k = (int) ((double) (k_bnd + 1) * ran3_(&idum))
		    ;
	    l = l_bnd * ran3_(&idum);
// make sure we are not sampling at too high an angle 
	    if (Math.asin(.5 * lambda * Math.sqrt(h * h * a0 + k *
		     k * b0 + l * l * c0 + h * k *
		    d0)) * 2. > max_angle) {
		loopon = true;
	    }
	}
// get I(h,k,l) 
	tot_int += pntint_(&h, &k, &l, ok);
	if (! (*ok)) {
	    return 0;
	}
//        goto 999 
// L10: 
    }

// Estimate some suitable fraction of the average intensity. This 
// fraction defines a baseline intensity equivalent to zero for 
// the purposes of the symmetry testing later on. 

    tiny_inty = tot_int * 1e-5 / no_trials;

    return 0;
//  999 write(op,100) 'ERROR in intensity calculation in THRESH' 
//      write(op,200) '   at h,k,l = ', h,',', k,',', l 
//      return 
//  100 format(1x, a) 
//  200 format(1x, a, i3, a, i3, a, f7.2) 
} // thresh_ 



//  
// Title: TOUPPR 
// Author: MMJT 
// Date: 3 August 1989 
// Description: Converts alphabetic characters in 'line' to uppercase. 
// Works for ascii characters, since it assumes alphabetic characters 
// are contiguous. 
// The neat trick of using 
//                 lower_case = ichar(chr).and.95 
// will not always work in ansi77 fortran, as some compilers evaluate 
// the right hand side as a boolean .true. or .false. rather than a 
// bitwise 'anded' int. 
// The alternative 
//                 lower_case = ichar(chr).iand.95 
// is equally undesirable since the operator .iand. is a military 
// extension, and is not ansi77. 

//      ARGUMENTS: 
//            line  -  Input line of characters to be converted to 
//                     upper case. The converted string is returned 
//                     in 'line'. 
//  

int touppr_(char *line, ftnlen line_len)
{
    // Local variables 
    int a, i, z, itmp, offset, lin_len;

    a = 'a';
    z = 'z';
    offset = a - 'A';
    lin_len = i_len(line, line_len);

    i = 1;
    while(i <= lin_len) {
	itmp = *(unsigned char *)&line[i - 1];
	if (itmp >= a && itmp <= z) {
	    *(unsigned char *)&line[i - 1] = (char) (itmp - offset);
	}
	++i;
    }

    return 0;
} // touppr_ 


//  
// Title: TRMSPC 
// Author: MMJT 
// Date: 20 April 1989; 7 Mar 1995 
// Description:  This function locates a suitable cut-off angle, 
// below which we can ignore the huge intensities which may occur 
// close to the origin when the full adaptive integration option is used. 
// TRMSPC is called only when theta = 0 is in the integrated range. 

//      ARGUMENTS: 
//            th2_low  -  a 2theta cut-off angle to be found (output) 

//      COMMON VARIABLES: 
//            uses:  th2_min, th2_max, d_theta, spec, RAD2DEG 

//        modifies:  spec 

//            TRMSPC returns boolean .true. if all went well. 
//  

boolean trmspc(double[] th2_low)
{
    // Local variables
    int i;
    int i_min, i_max;
    boolean loopon;

    i_max = (int) ((th2_max - th2_min) * HALF /
	    d_theta) + 1;
// spec(1) corresponds to the intensity at the origin and is always zero. 
    i = 2;
    loopon = true;
    while(loopon) {
	loopon = false;
	++i;
	if (i >= i_max + 1) {
//          write(op,100) 'No peaks were found in spectrum.' 
	    return true;
	}
// locate the first minimum after the huge peak at the origin 
	if (spec[i - 1] <= spec[i - 2]) {
	    loopon = true;
	}
    }
    i_min = i - 1;

// NOTE: The absolute angle is th2_low + th2_min 
    th2_low[0] = i_min * d_theta;

    return true;
//  100 format(1x, a) 
}



//  
// Title: TST_MIR 
// Author: MMJT 
// Date: 30 July 1989; 22 Feb 1995 
// Identifies the presence of mirror planes which contain the streaky 
// axis. The int argument 'mir_sym' can have one of two values, 
// 1 or 2. 
// mir_sym = 1 is the plane containing the cell sides h and l. 
// mir_sym = 2 is the plane containing the cell sides k and l. 
// For rotational symmetry greater than 2, one mirror implies the 
// presence of the other. For diffraction symmetry group No 3 (2/M), 
// only one or the other can occur, but we must test for both. 
// TST_MIR returns '.true.' if a mirror was found, '.false.' otherwise. 

//      ARGUMENTS: 
//            mir_sym  -  Plane across which we wish to test for mirror 
//                        symmetry. Takes the values 1 or 2. (input). 
//            idum     -  parameter used by RAN3. Is -ve if RAN3 
//                        is to be reset. (input). 
//            ok       -  boolean flag indicating all went well. 
//                                                      (output). 

//      COMMON VARIABLES: 
//            uses:  a0, b0, c0, d0, lambda, DoSymDump, no_trials, 
//                   max_angle, PI, PI2, RAD2DEG, cell_gamma, check_sym 
//                   h_bnd, k_bnd, l_bnd, tolerance, tiny_inty 

//        modifies:  max_var 

//      TST_MIR returns boolean .true. if the diffraction intensities 
//      have the mirror symmetry about the plane requested. 
//  

boolean tst_mir(int mir_sym, int idum, boolean[] ok)
{
    // System generated locals 
    int i1;
    double d1, d2;
    boolean ret_val;

    // Local variables 
    double variance;
    boolean eq_sides;
    int h, i, k;
    double l, i1, i2, tol;
    double i_avg;
    boolean cell90, match;
    int h_tmp, k_tmp;
    boolean cell120, loopon;
    boolean is_good;
    double rel_var;

// statement functions 
// S is the value of 1/d**2 at hkl 
// ANGLE is the Bragg angle (in radians) of the h,k,l plane 

    cell90 = (d1 = cell_gamma - pi * .5, Math.abs(d1)) <
	    pi * .5 * 5.0000000000000001e-4;
    cell120 = (d1 = cell_gamma - pi2 / 3., Math.abs(d1)) <
	    pi2 * 5.0000000000000001e-4 / 3.;
    eq_sides = (d1 = cell_a - cell_b, Math.abs(d1)) <=
	    (cell_a + cell_b) * 4.9999999999999998e-7;
    ret_val = false;
    is_good = false;
    if (*mir_sym < 1 || *mir_sym > 3) {
	return ret_val;
    }
    if (dosymdump) {
//        write(sy,200) 
	if (! cell90 && ! cell120) {
//          write(sy,230) 'cell angle = ', cell_gamma * RAD2DEG, 
//     |      ' degrees. NO HORIZONTAL MIRRORS ARE LIKELY.' 
	    return ret_val;
	}
    }

    if (dosymdump) {
	if (*mir_sym == 1) {
//          write(sy,199) 'Testing for mirror about the h-l plane' 
	} else if (*mir_sym == 2) {
//          write(sy,199) 'Testing for mirror about the k-l plane' 
	} else if (*mir_sym == 3) {
//          write(sy,199) 'Testing for mirror about the h=k,l plane' 
	}
//        write(sy,200) 
//        write(sy,210) 
    }
    i1 = no_trials;
    for (i = 0; i < i1; i++) {
// get usable h,k >= 0 
	loopon = true;
	while(loopon) {
	    loopon = false;
	    if (*mir_sym == 1) {
		h_tmp = (int) ((double) (h_bnd + 1) *
			ran3_(idum));
		k_tmp = 0;
		while(k_tmp == 0) {
		    k_tmp = (int) ((double) (k_bnd + 1) *
			     ran3_(idum));
		}
	    } else if (*mir_sym == 2) {
		k_tmp = (int) ((double) (k_bnd + 1) *
			ran3_(idum));
		h_tmp = 0;
		while(h_tmp == 0) {
		    h_tmp = (int) ((double) (h_bnd + 1) *
			     ran3_(idum));
		}
	    } else if (*mir_sym == 3) {
		k_tmp = (int) ((double) (k_bnd + 1) *
			ran3_(idum));
		h_tmp = k_tmp;
		while(h_tmp == k_tmp) {
		    h_tmp = (int) ((double) (h_bnd + 1) *
			     ran3_(idum));
		}
	    }
// get usable l > 0 
	    l = .01;
	    while(Math.abs(l) <= .01) {
		l = l_bnd * ran3_(idum);
	    }
// make sure we are not sampling at too high an angle 
	    if (Math.asin(.5 * lambda * Math.sqrt(h_tmp * h_tmp *
		    a0 + k_tmp * k_tmp * b0 + l * l *
		    c0 + h_tmp * k_tmp * d0)) * 2. >
		    max_angle) {
		loopon = true;
	    }
	}
// I(h,k,l) 
	h = h_tmp;
	k = k_tmp;
	i1 = pntint_(&h, &k, &l, ok);
	if (! (*ok)) {
	    return ret_val;
	}
//        goto 999 
//        if(DoSymDump) write(sy,220) h, k, l, i1 
// mirror on h-l plane 
	if (*mir_sym == 1) {
// is the cell angle equal to 90 degrees 
	    if (cell90) {
// I(h,-k,l), rectangular cell 
		h = h_tmp;
		k = -k_tmp;
		i2 = pntint_(&h, &k, &l, ok);
		if (! (*ok)) {
		    return ret_val;
		}
//        goto 999 
//            if(DoSymDump) write(sy,220) h, k, l, i2 
	    } else if (cell120) {
// I(h+k,-k,l), hexagonal cell 
		h = h_tmp + k_tmp;
		k = -k_tmp;
		i2 = pntint_(&h, &k, &l, ok);
		if (! (*ok)) {
		    return ret_val;
		}
//        goto 999 
//            if(DoSymDump) write(sy,220) h, k, l, i2 
	    }
// else mirror on k-l plane, mir = 2 
	} else if (*mir_sym == 2) {
// is the cell angle equal to 90 degrees 
	    if (cell90) {
// I(-h,k,l), rectangular cell 
		h = -h_tmp;
		k = k_tmp;
		i2 = pntint_(&h, &k, &l, ok);
		if (! (*ok)) {
		    return ret_val;
		}
//        goto 999 
//            if(DoSymDump) write(sy,220) h, k, l, i2 
	    } else if (cell120) {
// I(-h,h+k,l), hexagonal cell 
		h = -h_tmp;
		k = h_tmp + k_tmp;
		i2 = pntint_(&h, &k, &l, ok);
		if (! (*ok)) {
		    return ret_val;
		}
//        goto 999 
//            if(DoSymDump) write(sy,220) h, k, l, i2 
	    }
// else mirror on hk-l plane, mir = 3 
	} else if (*mir_sym == 3) {
// The following if block is redundant, and in special 
// cases fails to print to the .sym file. mmjt 3/18/04 
// is the cell square 
//          if(cell90 .and. eq_sides) then 
// I(-h,k,l), square cell 
//            h = k_tmp 
//            k = h_tmp 
//            i2 = PNTINT(h, k, l, ok) 
//            if(.not.ok) goto 999 
//            if(DoSymDump) write(sy,220) h, k, l, i2 
//          else if(cell120) then 
// I(-h,h+k,l), hexagonal cell 
//            h = k_tmp 
//            k = h_tmp 
//            i2 = PNTINT(h, k, l, ok) 
//            if(.not.ok) goto 999 
//            if(DoSymDump) write(sy,220) h, k, l, i2 
//          endif 
	    h = k_tmp;
	    k = h_tmp;
	    i2 = pntint_(&h, &k, &l, ok);
	    if (! (*ok)) {
		return ret_val;
	    }
//        goto 999 
//          if(DoSymDump) write(sy,220) h, k, l, i2 
	}
// compare mirrored intensities 
	i_avg = (i1 + i2) * .5;
	variance = ((d1 = i_avg - i1, Math.abs(d1)) + (d2 = i_avg - i2,
		Math.abs(d2))) * .5;
// Be careful intensities are not actually zero 
	if (i_avg < tiny_inty) {
	    tol = tiny_inty;
	} else {
	    tol = i_avg * tolerance;
	    rel_var = variance / i_avg;
	    if (rel_var > max_var) {
		max_var = rel_var;
	    }
	}
	match = (d1 = i_avg - i1, Math.abs(d1)) < tol && (d2 = i_avg -
		i2, Math.abs(d2)) < tol;
	is_good = (i == 1 || is_good) && match;
	if (dosymdump) {
//          write(sy,270) 
//          write(sy,240) i_avg 
//          write(sy,360) variance, HUNDRED * variance / i_avg 
//          if(.not.check_sym) then 
//            write(sy,260) tol 
	    if (match) {
		if (*mir_sym == 1) {
//                write(sy,250) 
		} else if (*mir_sym == 2) {
//                write(sy,280) 
		} else if (*mir_sym == 3) {
//                write(sy,285) 
		}
	    } else {
		if (*mir_sym == 1) {
//                write(sy,290) 
		} else if (*mir_sym == 2) {
//                write(sy,300) 
		} else if (*mir_sym == 3) {
//                write(sy,305) 
		}
	    }
//          endif 
//          write(sy,200) 
	}
// L10: 
    }
    ret_val = is_good;

    if (dosymdump) {
//        if(.not.check_sym) then 
	if (*mir_sym == 1) {
	    if (is_good) {
//              write(sy,310) 
	    } else {
//              write(sy,320) 
	    }
	} else if (*mir_sym == 2) {
	    if (is_good) {
//              write(sy,330) 
	    } else {
//              write(sy,340) 
	    }
	} else if (*mir_sym == 3) {
	    if (is_good) {
//              write(sy,345) 
	    } else {
//              write(sy,346) 
	    }
	}
//        endif 
//        write(sy,200) 
    }

    return ret_val;
//  999 write(op,199) 'ERROR in intensity calculation in TST_MIR' 
//      write(op,350) '   at h,k,l = ', h,',', k,',', l 
//      return 
//  199 format(1x, a) 
//  200 format(' ') 
//  210 format(1x, '  h', 5x, 'k', 7x, 'l', 20x, 'Intensity') 
//  220 format(1x, i3, 3x, i3, 2x, f9.4, 5x, f22.6) 
//  230 format(1x, a, f6.2, a) 
//  240 format(6x, 'Average Intensity = ', f22.6) 
//  250 format(1x, 'Intensities are consistent with an h-l mirror plane') 
//  260 format(1x, 'Intensity tolerance = +/-', f22.6) 
//  270 format(26x, '----------------------') 
//  280 format(1x, 'Intensities are consistent with a k-l mirror plane') 
//  285 format(1x, 'Intensities are consistent with a h=k,l mirror plane') 
//  290 format(1x, 'Intensities not consistent with an h-l mirror plane') 
//  300 format(1x, 'Intensities not consistent with a k-l mirror plane') 
//  305 format(1x, 'Intensities not consistent with a h=k,l mirror plane') 
//  310 format(1x, 'THERE IS A MIRROR ABOUT THE H-L PLANE') 
//  320 format(1x, 'THERE IS NO MIRROR ABOUT THE H-L PLANE') 
//  330 format(1x, 'THERE IS A MIRROR ABOUT THE K-L PLANE') 
//  340 format(1x, 'THERE IS NO MIRROR ABOUT THE K-L PLANE') 
//  345 format(1x, 'THERE IS A MIRROR ABOUT THE H=K,L PLANE') 
//  346 format(1x, 'THERE IS NO MIRROR ABOUT THE H=K,L PLANE') 
//  350 format(1x, a, i3, a, i3, a, f7.2) 
//  360 format(1x,'  Average variation = +/-', f22.6,'  (+/-',g9.2,'%)') 
} // tst_mir 



//  
// Title: TST_ROT 
// Author: MMJT 
// Date: 30 July 1989; 22 Feb 1995 
// Identifies the rotational symmetry of the diffraction pattern. 
// The rotational symmetry to test for is passed as 'rot_sym'. 
// The values accepted for 'rot_sym' are 2, 3 and 4. If the diffraction 
// intensity has the symmetry requested, TST_ROT returns '.true.'. If 
// the pattern does not have the requested symmetry, or if an illegal 
// value was passed (i.e. rot_sym = 5), TST_ROT returns '.false.'. 
// NOTE. A 6-fold axis is found by calling TST_ROT twice: once for 
// rot_sym = 2 and again for rot_sym = 3. 

//      ARGUMENTS: 
//            rot_sym  -  Rotational symmetry to test for. 
//                        Accepts the values 2, 3 or 4. (input). 
//            idum     -  parameter used by RAN3. Is -ve if RAN3 
//                        is to be reset. (input). 
//            ok       -  boolean flag indicating all went well. 
//                                                      (output). 

//      COMMON VARIABLES: 
//            uses:  a0, b0, c0, d0, lambda, DoSymDump, no_trials, 
//                   max_angle, check_sym, h_bnd, k_bnd, l_bnd 
//                   tolerance 

//        modifies:  max_var 

//      TST_ROT returns boolean .true. if the diffraction intensities 
//      have the requested rotational symmetry. 
//  

boolean tst_rot(int rot_sym, int idum, boolean[] ok)
{
    // System generated locals 
    int i1;
    double d1, d2, d3, d4;
    boolean ret_val;

    // Local variables 
    double variance;
    int h, i, k;
    double l, i1, i2, i3, i4, tol;
    double i_avg;
    boolean match;
    int h_tmp, k_tmp;
    boolean loopon;
    boolean loopon1, is_good;
    double rel_var;

// statement functions 
// S is the value of 1/d**2 at hkl 

    ret_val = false;
    is_good = false;
// Is rot valid? 
    if (*rot_sym < 2 || *rot_sym > 4) {
//        goto 900 
//      if(DoSymDump) then 
//        if(.not.check_sym) then 
//          if(is_good) then 
//            write(sy,300) rot_sym 
//          else 
//            write(sy,310) rot_sym 
//          endif 
//        endif 
//        write(sy,210) 
//      endif 
	return ret_val;
    }
// Now test for rotational symmetry. 
// 2-fold and 4-fold 
// Avoid both h, k = 0. Also, avoid l = 0, since Friedel's law will 
// create a pseudo 2-fold. 
    if (*rot_sym == 2 || *rot_sym == 4) {
//        if(DoSymDump) then 
//          write(sy,210) 
//          write(sy,330) 'Testing for ', rot_sym, '-fold axis' 
//          write(sy,210) 
//        endif 
	i1 = no_trials;
	for (i = 0; i < i1; i++) {
//          if(DoSymDump) write(sy,220) 
// get usable h,k >= 0 
	    loopon = true;
	    while(loopon) {
		loopon = false;
		h_tmp = (int) ((double) (h_bnd + 1) *
			ran3_(idum));
		k_tmp = (int) ((double) (k_bnd + 1) *
			ran3_(idum));
		if (h_tmp != 0 || k_tmp != 0) {
// get usable l > 0 
		    loopon1 = true;
		    while(loopon1) {
			loopon1 = false;
			l = l_bnd * ran3_(idum);
// keep l off the l = 0 plane, else we might confuse the inversion 
// with a 2-fold 
			if (Math.abs(l) <= .01) {
			    loopon1 = true;
			}
		    }
// make sure we are not sampling at too high an angle 
		}
		if (Math.asin(.5 * lambda * Math.sqrt(h_tmp * h_tmp *
			a0 + k_tmp * k_tmp * b0 + l * l
			* c0 + h_tmp * k_tmp * d0)) *
			2. > max_angle) {
		    loopon = true;
		}
	    }
// I(h,k,l) 
	    h = h_tmp;
	    k = k_tmp;
	    i1 = pntint_(&h, &k, &l, ok);
	    if (! (*ok)) {
		return ret_val;
	    }
//            goto 999 
//          if(DoSymDump) write(sy,230) h, k, l, i1 
// I(-h,-k,l) 
	    h = -h_tmp;
	    k = -k_tmp;
	    i2 = pntint_(&h, &k, &l, ok);
	    if (! (*ok)) {
		return ret_val;
	    }
//            goto 999 
//          if(DoSymDump) write(sy,230) h, k, l, i2 
// compare 2-fold intensities 
	    if (*rot_sym == 2) {
		i_avg = (i1 + i2) * .5;
		variance = ((d1 = i_avg - i1, Math.abs(d1)) + (d2 =
			i_avg - i2, Math.abs(d2))) * .5;
// Be careful intensities are not actually zero 
		if (i_avg < tiny_inty) {
		    tol = tiny_inty;
		} else {
		    tol = i_avg * tolerance;
		    rel_var = variance / i_avg;
		    if (rel_var > max_var) {
			max_var = rel_var;
		    }
		}
		match = (d1 = i_avg - i1, Math.abs(d1)) < tol && (d2 =
			i_avg - i2, Math.abs(d2)) < tol;
		is_good = (i == 1 || is_good) && match;
	    } else {
// I(-k,h,l) 
		h = -k_tmp;
		k = h_tmp;
		i3 = pntint_(&h, &k, &l, ok);
		if (! (*ok)) {
		    return ret_val;
		}
//            goto 999 
//            if(DoSymDump) write(sy,230) h, k, l, i3 
// I(k,-h,l) 
		h = k_tmp;
		k = -h_tmp;
		i4 = pntint_(&h, &k, &l, ok);
		if (! (*ok)) {
		    return ret_val;
		}
//            goto 999 
//            if(DoSymDump) write(sy,230) h, k, l, i4 
// compare 4-fold intensities 
		i_avg = (i1 + i2 + i3 + i4) * .25;
		variance = ((d1 = i_avg - i1, Math.abs(d1)) + (d2 =
			i_avg - i2, Math.abs(d2)) + (d3 = i_avg - i3, Math.abs(
			d3)) + (d4 = i_avg - i4, Math.abs(d4))) * .25;
// Be careful intensities are not actually zero 
		if (i_avg < tiny_inty) {
		    tol = tiny_inty;
		} else {
		    tol = i_avg * tolerance;
		    rel_var = variance / i_avg;
		    if (rel_var > max_var) {
			max_var = rel_var;
		    }
		}
		match = (d1 = i_avg - i1, Math.abs(d1)) < tol && (d2 =
			i_avg - i2, Math.abs(d2)) < tol && (d3 = i_avg -
			i3, Math.abs(d3)) < tol && (d4 = i_avg - i4, Math.abs(
			d4)) < tol;
		is_good = (i == 1 || is_good) && match;
	    }

//          if(DoSymDump) then 
//            write(sy,240) 
//            write(sy,250) i_avg 
//            write(sy,260) variance, HUNDRED * variance / i_avg 
//            if(.not.check_sym) then 
//              write(sy,270) tol 
//              if(match) then 
//                write(sy,280) rot_sym 
//              else 
//                write(sy,290) rot_sym 
//              endif 
//            endif 
//            write(sy,210) 
//          endif 
// L10: 
	}
	ret_val = is_good;
//        goto 900 
//      if(DoSymDump) then 
//        if(.not.check_sym) then 
//          if(is_good) then 
//            write(sy,300) rot_sym 
//          else 
//            write(sy,310) rot_sym 
//          endif 
//        endif 
//        write(sy,210) 
//      endif 
	return ret_val;
    }
// 3-fold 
// Avoid both h, k = 0. 
    if (*rot_sym == 3) {
//        if(DoSymDump) then 
//          write(sy,200) rot_sym 
//          write(sy,210) 
//          write(sy,220) 
//        endif 
	i1 = no_trials;
	for (i = 0; i < i1; i++) {
// get usable h,k >= 0 
	    loopon = true;
	    while(loopon) {
		loopon = false;
		h_tmp = (int) ((double) (h_bnd + 1) *
			ran3_(idum));
		k_tmp = (int) ((double) (k_bnd + 1) *
			ran3_(idum));
		if (h_tmp != 0 || k_tmp != 0) {
// get l (l=0 is allowed) 
		    l = l_bnd * ran3_(idum);
// make sure we are not sampling at too high an angle 
		}
		if (Math.asin(.5 * lambda * Math.sqrt(h_tmp * h_tmp *
			a0 + k_tmp * k_tmp * b0 + l * l
			* c0 + h_tmp * k_tmp * d0)) *
			2. > max_angle) {
		    loopon = true;
		}
	    }
// I(h,k,l) 
	    h = h_tmp;
	    k = k_tmp;
	    i1 = pntint_(&h, &k, &l, ok);
	    if (! (*ok)) {
		return ret_val;
	    }
//          goto 999 
//          if(DoSymDump) write(sy,230) h, k, l, i1 
// I(-h-k,h,l) 
	    h = -(h_tmp + k_tmp);
	    k = h_tmp;
	    i2 = pntint_(&h, &k, &l, ok);
	    if (! (*ok)) {
		return ret_val;
	    }
//          goto 999 
//          if(DoSymDump) write(sy,230) h, k, l, i2 
// I(k,-h-k,l) 
	    h = k_tmp;
	    k = -(h_tmp + k_tmp);
	    i3 = pntint_(&h, &k, &l, ok);
//          if(DoSymDump) write(sy,230) h, k, l, i3 
// compare intensities 
	    i_avg = (i1 + i2 + i3) / 3.;
	    variance = ((d1 = i_avg - i1, Math.abs(d1)) + (d2 = i_avg -
		    i2, Math.abs(d2)) + (d3 = i_avg - i3, Math.abs(d3))) / 3.;
// Be careful intensities are not actually zero 
	    if (i_avg < tiny_inty) {
		tol = tiny_inty;
	    } else {
		tol = i_avg * tolerance;
		rel_var = variance / i_avg;
		if (rel_var > max_var) {
		    max_var = rel_var;
		}
	    }
	    match = (d1 = i_avg - i1, Math.abs(d1)) < tol && (d2 = i_avg
		    - i2, Math.abs(d2)) < tol && (d3 = i_avg - i3, Math.abs(d3))
		     < tol;
	    is_good = (i == 1 || is_good) && match;
//          if(DoSymDump) then 
//            write(sy,240) 
//            write(sy,250) i_avg 
//            write(sy,260) variance, HUNDRED * variance / i_avg 
//            if(.not.check_sym) then 
//              write(sy,270) tol 
//              if(match) then 
//                write(sy,280) rot_sym 
//              else 
//                write(sy,290) rot_sym 
//              endif 
//            endif 
//            write(sy,210) 
//          endif 
// L40: 
	}
	ret_val = is_good;
    }

//      if(DoSymDump) then 
//        if(.not.check_sym) then 
//          if(is_good) then 
//            write(sy,300) rot_sym 
//          else 
//            write(sy,310) rot_sym 
//          endif 
//        endif 
//        write(sy,210) 
//      endif 
    return ret_val;

//  999 write(op,400) 'ERROR in intensity calculation in TST_ROT' 
//      write(op,320) '   at h,k,l = ', h,',', k,',', l 
//      return 
//  200 format(1x, 'Testing for a ', i1, '-fold axis') 
//  210 format(' ') 
//  220 format(1x, '  h', 5x, 'k', 7x, 'l', 20x, 'Intensity') 
//  230 format(1x, i3, 3x, i3, 2x, f9.4, 5x, f22.6) 
//  240 format(26x, '----------------------') 
//  250 format(6x, 'Average Intensity = ', f22.6) 
//  260 format(1x, '  Average variation = +/-',f22.6,'  (+/-',g9.2,'%)') 
//  270 format(1x, 'Intensity tolerance = +/-', f22.6) 
//  280 format(1x, 'Intensities are consistent with a ', i1, '-fold') 
//  290 format(1x, 'Intensities are not consistent with a ', i1, '-fold') 
//  300 format(1x, 'INTENSITY DISTRIBUTION HAS A ', i1, '-FOLD AXIS') 
//  310 format(1x, 'INTENSITY DISTRIBUTION HAS NO ', i1, '-FOLD AXIS') 
//  320 format(1x, a, i3, a, i3, a, f7.2) 
//  330 format(1x, a, i1, a) 
//  400 format(1x, a) 
} // tst_rot 



//  
// Title: WRTLNE 
// Author: MMJT 
// Date: 4 Oct 1989 
// Description: This subroutine writes out the contents of 
// the character string 'line' with an appropriate preamble. 
// This routine strips off trailing blanks. 

//      ARGUMENTS: 
//            line     -  Line of characters to write out. (input). 
//  

int wrtlne_(char *line, ftnlen line_len)
{
    // Local variables 
    int i;
    boolean loopon;
    int lin_len;

    lin_len = i_len(line, line_len);
    i = lin_len;
    loopon = true;
    while(loopon) {
	loopon = false;
	if (i > 0) {
	    if (*(unsigned char *)&line[i - 1] == ' ') {
		if (i > 1) {
		    --i;
		    loopon = true;
		}
	    }
	}
    }

//      write(op,100) 'The data on the current line was read as:' 
//      write(op,101) '  ''', line(1:i), '''' 

    return 0;
//  100 format(1x, a) 
//  101 format(1x, 3a) 
} // wrtlne_ 


//  
// Title: WRTSAD 
// Author: MMJT 
// Date: 29 Oct 1989; 1st Mar 2000 
// Description: This subroutine writes the selected area diffraction 
// pattern (SADP) data to a binary file. 

//      ARGUMENTS: 
//            outfile  -  The name of the file that the binary 
//                        selected area diffraction data is to be 
//                        written to. (input). 
//            view     -  Choice of beam direction. (input). 
//                              1  =  normal to the plane k = 0 
//                              2  =  normal to the plane h = 0 
//                              3  =  normal to the plane h = k 
//                              4  =  normal to the plane h = -k 
//            l_upper  -  Upper limit of l. (input). 
//            hk_lim   -  Upper limit of h (or k). (input). 
//            ok       -  boolean flag indicating all went well. 
//                                                      (output). 

//      COMMON VARIABLES: 
//            uses:  a0, b0, c0, d0, sadblock, scaleint, has_l_mirror 

//        modifies:  spec 
//  

int wrtsad_(char *outfile, int *view, double *
	l_upper, int *hk_lim, boolean *ok, ftnlen outfile_len)
{
    // System generated locals 
    int i1, i2;
    double d1;

    // Local variables 
    int i, j;
    double x;
    int p1, p2;
    double incr, sigma;
    double rowint[256];

//      write(op,102) 'Writing SADP data to file ''', 
//     |  outfile(1:LENGTH(outfile)), '''. . .' 

// set standard deviation 
    sigma = 0.;

// Establish scaling in reciprocal space, the number of pixels per unit 
// first get number of pixels per l 
    incr = 128. / *l_upper;
    if (*view == 1) {
// number of pixels per unit h 
	incr *= Math.sqrt(a0 / c0);
    } else if (*view == 2) {
// number of pixels per unit k 
	incr *= Math.sqrt(b0 / c0);
    } else if (*view == 3) {
// number of pixels per unit along (h = k) 
	incr *= Math.sqrt((a0 + b0 + d0) / c0);
    } else if (*view == 4) {
// number of pixels per unit along (h = -k) 
	incr *= Math.sqrt((a0 + b0 - d0) / c0);
    }

    for (j = sadblock - 1; j >= 0; --j) {
	for (i = 1; i <= 256; ++i) {
	    rowint[i - 1] = 0.;
// L20: 
	}
	i1 = *hk_lim;
	for (i = 0; i <= i1; ++i) {
	    d1 = i * incr;
	    p1 = i_dnnt(&d1) + 129;
	    d1 = i * incr;
	    p2 = 128 - i_dnnt(&d1) + 1;

// cycle if we have gone out of bounds 
	    if (p1 <= 256 && p1 >= 0 && p2 <= 256 && p2 >= 0) {

		x = spec[i * sadblock + j] *
			scaleint;
// handle saturated pixels 
		if (x > (double) maxsad) {
		    x = (double) maxsad;
		}
		rowint[p1 - 1] = x;
		if (has_l_mirror) {
		    rowint[p2 - 1] = x;
		} else {
		    x = spec[i * sadblock +
			    sadblock - j - 1] * scaleint;
// handle saturated pixels 
		    if (x > (double) maxsad) {
			x = (double) maxsad;
		    }
		    rowint[p2 - 1] = x;
		}
	    }
// L30: 
	}
	smudge_(rowint, &c256, &sigma, ok);
	if (! (*ok)) {
	    return 0;
	}
//        goto 999 
	if (bitdepth == 8) {
//          write(sa,err=900) (char(nint(rowint(i))), i = 1, SADSIZE) 
	} else {
//          write(sa,err=900) 
//     | (char(int(rowint(i)/256)), 
//     |  char(int(rowint(i)-dreal(int(rowint(i)/256))*256)),i=1,SADSIZE) 
	}
// L10: 
    }

// Repeat, in reverse for the bottom half if data had a mirror. 
    if (has_l_mirror) {
	i1 = sadblock - 1;
	for (j = 1; j <= i1; ++j) {
	    for (i = 1; i <= 256; ++i) {
		rowint[i - 1] = 0.;
// L50: 
	    }
	    i2 = *hk_lim;
	    for (i = 0; i <= i2; ++i) {
		d1 = i * incr;
		p1 = i_dnnt(&d1) + 129;
		d1 = i * incr;
		p2 = 128 - i_dnnt(&d1) + 1;

// cycle if we have gone out of bounds 
		if (p1 <= 256 && p1 >= 0 && p2 <= 256 && p2 >= 0) {

		    x = spec[i * sadblock + j] *
			    scaleint;
// handle saturated pixels 
		    if (x > (double) maxsad) {
			x = (double) maxsad;
		    }
		    rowint[p1 - 1] = x;
		    rowint[p2 - 1] = x;
		}
// L60: 
	    }
	    smudge_(rowint, &c256, &sigma, ok);
	    if (! (*ok)) {
		return 0;
	    }
//          goto 999 
	    if (bitdepth == 8) {
//            write(sa,err=900) (char(nint(rowint(i))), i = 1, SADSIZE) 
	    } else {
//            write(sa,err=900) 
//     | (char(int(rowint(i)/256)), 
//     |  char(int(rowint(i)-dreal(int(rowint(i)/256))*256)),i=1,SADSIZE) 
	    }
// L40: 
	}
// write a blank last line to make the array SADSIZE x SADSIZE square 
	if (bitdepth == 8) {
//          write(sa,err=900) (char(0), i = 1, SADSIZE) 
	} else {
//          write(sa,err=900) (char(0), i = 1, 2*SADSIZE) 
	}
    }

//      if(sa.ne.op) close(unit = sa, err = 990) 

//      write(op,103) SADSIZE, ' x ', SADSIZE, ' pixels: ', 
//     |              bitdepth, ' bits deep.' 

    return 0;
//  900 write(op,100) 'ERROR: problems writing to binary SADP file.' 
//      ok = .false. 
//      return 
//  990 write(op,100) 'ERROR: problems closing binary SADP file.' 
//      ok = .false. 
//      return 
//  999 write(op,100) 'ERROR: SMUDGE returned error to WRTSAD.' 
//      write(op,101) i, j 
//      return 
//  100 format(1x, a) 
//  101 format(5x, 'with local variables i = ', i5, ' j = ', i5) 
//  102 format(1x, 3a) 
//  103 format(1x, i4, a, i4, a, i2, a) 
} // wrtsad_ 


//  
// Title: WRTSPC 
// Author: MWD and MMJT 
// Date: 18 Aug 1988; 7 Mar 1995 
// Description:  This routine writes the spectrum arrays to file. 

//      ARGUMENTS: 
//            spcfile  -  The name of the output data file. (input). 
//            ok       -  boolean flag indicating all went well. 
//                                                      (output). 

//      COMMON VARIABLES: 
//            uses:  th2_min, th2_max, d_theta, spec, brd_spc, blurring 
//                   NONE, RAD2DEG 

//        modifies:  no COMMON variables are modified 
//  

int wrtspc_(char *spcfile, boolean *ok, ftnlen spcfile_len)
{
    // System generated locals 
    int i1;

    // Local variables 
    int i;
    char tab[1];
    double theta;
    int n_low, n_high;

    n_low = 1;
    n_high = (int) ((th2_max - th2_min) * .5 /
	    d_theta) + 1;

    *(unsigned char *)tab = '\t';
    theta = th2_min * rad2deg;
//      write(op,200) 'Writing spectrum data to file ''', 
//     |      spcfile(1:LENGTH(spcfile)), '''. . .' 
//      do 10 i = int(HALF*th2_min / d_theta) + 1, 
//     |                  int(HALF*th2_max / d_theta) + 1 
    if (blurring == none) {
	i1 = n_high;
	for (i = n_low; i <= i1; ++i) {
//          write(sp,101,err=50) theta, tab, spec(i) 
	    theta += rad2deg * 2. * d_theta;
// L10: 
	}
    } else {
	i1 = n_high;
	for (i = n_low; i <= i1; ++i) {
//          write(sp,100,err=50) theta, tab, spec(i), tab, brd_spc(i) 
	    theta += rad2deg * 2. * d_theta;
// L20: 
	}
    }

//      if(sp.ne.op) close(sp,err=60) 
//      write(op,202) 'Spectrum written.' 
//      write(op,202) 
    return 0;
//   50 write(op,202) 'Unable to write to spectrum file.' 
//      if(sp.ne.op) close(sp,err=60) 
//      ok = .false. 
//      return 
//   60 write(op,202) 'Unable to close spectrum file.' 
//      ok = .false. 
//      return 
//  100 format(1x, e12.5, 2(a, g13.6)) 
//  101 format(1x, e12.5, a, g13.6) 
//  200 format(1x, 3a) 
//  201 format(1x, a, i2, 2a) 
//  202 format(1x, a) 
} // wrtspc_ 



//  
// Title: XYPHSE 
// Author: MMJT 
// Date: 8 June 1990 
// Description:  This routine pre-calculates the h and k components of 
// phases for each atom. This is called when l = 0, since h and k 
// are held constant while l is varied along each streak. 

//      ARGUMENTS: 
//            h  -  reciprocal lattice vector h-component. (input). 
//            k  -  reciprocal lattice vector k-component. (input). 

//      COMMON VARIABLES: 
//            uses:  n_actual, l_n_atoms, a_pos, l_actual, n_layers 

//        modifies:  hx_ky 
//  

int xyphse_(int *h, int *k)
{
    // System generated locals 
    int i1, i2;

    // Local variables 
    int i, m;

    i1 = n_actual;
    for (m = 1; m <= i1; ++m) {
	i2 = l_n_atoms[m - 1];
	for (i = 1; i <= i2; ++i) {
	    hx_ky[i + m * 200 - 201] = *h * a_pos[(
		    i + m * 200) * 3 - 603] + *k * a_pos[(i +
		    m * 200) * 3 - 602];
// L20: 
	}
// L10: 
    }

    return 0;
} // xyphse_ 



//  
// Title: YRDSTK 
// Author: MMJT 
// Date: 12 Aug 1989 
// Description: YRDSTK checks that y is a multiple of x. 

//      ARGUMENTS: 
//            x   -  reference value. (input). 
//            y   -  value to be tested. (input) 
//            ok  -  boolean flag set to .false. if y is zero. (output). 

//      YRDSTK returns boolean .true. if y is a multiple of x. 
//  

boolean yrdstk_(double *x, double *y, boolean *ok)
{
    // System generated locals 
    double d1;
    boolean ret_val;

    // Local variables 
    double tmp;

    ret_val = false;
    if (*y == 0.) {
	*ok = false;
	return ret_val;
//      goto 999 
    }
    if (*x == 0.) {
// This is the first visit to YRDSTK. Set things up. 
	*x = *y;
	ret_val = true;
    } else {
	tmp = *y / *x;
	if ((d1 = i_dnnt(&tmp) - tmp, Math.abs(d1)) <= tmp * .001) {
	    ret_val = true;
	}
    }

    return ret_val;
//  999 ok = .false. 
//      return 
} // yrdstk_ 

}
