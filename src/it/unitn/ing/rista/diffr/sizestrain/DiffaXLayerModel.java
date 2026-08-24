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

import it.unitn.ing.rista.diffr.*;

/**
 * The DiffaXLayerModel is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jun 30, 2009 11:14:24 AM $
 * @since JDK1.1
 */


public class DiffaXLayerModel extends PlanarDefects {

  public static String[] diclistc = {
/*      "_riet_deformation_fault_intrinsic",
      "_riet_deformation_fault_extrinsic",
      "_riet_twin_fault_probability"*/
  };
  public static String[] diclistcrm = {
/*      "deformation fault probability (intrinsic)",
      "deformation fault probability (extrinsic)",
      "twin fault probability"*/
  };

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  public DiffaXLayerModel(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Disabled DiffaX";
    IDlabel = "Disabled DiffaX";
    description = "select this to use the DiffaX model for planar defects";
  }

  public DiffaXLayerModel(XRDcat aobj) {
    this(aobj, "DiffaX planar defect");
  }

  public DiffaXLayerModel() {
    identifier = "Disabled DiffaX";
    IDlabel = "Disabled DiffaX";
    description = "select this to use the DiffaX model for planar defects";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
    for (int i = 0; i < totsubordinate - totparameterloop; i++)
      classlists[i] = classlistcs[i];
  }

  public void initParameters() {
    super.initParameters();
/*    parameterField[0] = new Parameter(this, getParameterString(0), 0.0,
        ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(0) + ".max", 0.01));
    parameterField[0].setPositiveOnly();
    parameterField[0].setMinimumSignificantValue(0.0001);
    parameterField[1] = new Parameter(this, getParameterString(1), 0.0,
        ParameterPreferences.getDouble(getParameterString(1) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(1) + ".max", 0.01));
    parameterField[1].setPositiveOnly();
    parameterField[1].setMinimumSignificantValue(0.0001);
    parameterField[2] = new Parameter(this, getParameterString(2), 0.0,
        ParameterPreferences.getDouble(getParameterString(2) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(2) + ".max", 0.01));
    parameterField[2].setPositiveOnly();
    parameterField[2].setMinimumSignificantValue(0.0001);*/
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);
/*    parameterField[0].setPositiveOnly();
    parameterField[1].setPositiveOnly();
    parameterField[2].setPositiveOnly();
    parameterField[0].setMinimumSignificantValue(0.0001);
    parameterField[1].setMinimumSignificantValue(0.0001);
    parameterField[2].setMinimumSignificantValue(0.0001);*/
  }


}



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
