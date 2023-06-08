/*
 * @(#)StructureSolutionReverseMontecarlo.java created 11/08/2001 Casalino
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

package it.unitn.ing.rista.diffr.sdpd;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;

import java.awt.*;

/**
 *  The StructureSolutionReverseMontecarlo is a method to solve the crystal
 *  structure derived from the idea of Le Bail in Espoir
 *
 * @version $Revision: 1.7 $, $Date: 2006/07/20 13:39:06 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class StructureSolutionReverseMontecarlo extends StructureSolutionMethod {

  boolean startRandomConfiguration = true;
  boolean looping = false;
  int maxTrialForSolving = 40000; // in secs, limiting time for solution
  int saveTrialInterval = 20000;
  int iprint = 5000;  //
  boolean shortDistanceConstraints = false;
  boolean moleculeReplacement = false;
  int iseed = 0;
  double Rmax = 0.25;           // 0.25-0.30 for small structures, 035-0.40 for large
  int timeToCheckRmax = 40000;  // 40000 for small structures, 120000 for large
  boolean useRforChiTest = false;
  int intervalForPermutations = 10; // try a permutation after n moves
  double annealingExponent = 2.;
  int atomNumber = 0;
  double reject = 0.005;
  ec.util.MersenneTwisterFast randomizer = null;


  StructureFactorList[] structureFactorList = null;
  double[] defParams = null;
  double[] bestParams = null;
  double defWSS = 0.0;
  double bestWSS = 0.0;


  public StructureSolutionReverseMontecarlo(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Disabled Reverse Montecarlo SDPD";
    IDlabel = "Reverse Montecarlo SDPD";
    description = "select this to use a Reverse Montecarlo";
  }

  public StructureSolutionReverseMontecarlo(XRDcat aobj) {
    this(aobj, "Reverse Montecarlo SDPD");
  }

  public StructureSolutionReverseMontecarlo() {
    identifier = "Disabled Reverse Montecarlo SDPD";
    IDlabel = "Reverse Montecarlo SDPD";
    description = "select this to use a Reverse Montecarlo";
  }

  public boolean canSolveStructure() {
    return true;
  }

  public boolean solveStructure(StructureFactorList[] structureFactorList) {

    Phase aphase = (Phase) getParent();

    this.structureFactorList = structureFactorList;

    initAll();

//  Generate the starting structure configuration
    generateStartingPositions();

    startSolutionLoop();
    return true;
  }

  void initAll() {
    for (int i = 0; i < structureFactorList.length; i++) {
      int reflectionNumber = structureFactorList[i].structureFactor.length;
      for (int j = 0; j < reflectionNumber; j++) {
        StructureFactor sf = structureFactorList[i].structureFactor[j];
        sf.Fhkl_exp = sf.Fhkl_calc; // the calc is the mean value for all exp
      }
    }
    Phase aphase = (Phase) getParent();
    defParams = aphase.getStructureParams();
    bestParams = new double[defParams.length];
    defWSS = getFitness(defParams);
    bestWSS = 1.0E50;
    for (int i = 0; i < bestParams.length; i++)
      bestParams[i] = defParams[i];

//  Init the randomizer for random number generation
    initRandomizer();
  }

  void generateStartingPositions() {
    if (startRandomConfiguration)
      generateRandomConfiguration();
    else
      pickLastStructure();
  }

  void generateRandomConfiguration() {
  }

  void pickLastStructure() {
  }

  void startSolutionLoop() {

    atomNumber = bestParams.length / 3;
    double[] actualParams = new double[bestParams.length];
    for (int i = 0; i < bestParams.length; i++)
      actualParams[i] = bestParams[i];
    double[] tmpParams = new double[bestParams.length];
    for (int i = 0; i < bestParams.length; i++)
      tmpParams[i] = bestParams[i];

    boolean acceptable = true;

//  ngen   = number of generated moves
//  ntried = Number of tried moves
//  nacc   = Number of accepted moves
    int ngen = 0;
    int ntried = 0;
    int nacc = 0;
    looping = true;
    int save_time = Math.min(maxTrialForSolving, saveTrialInterval);
    int ngen0 = ngen;
    int ngenp = 0;
    int ngent = 0;
    int naccp = 0;
    int ntriedp = 0;
    double dump = 1.;
    double dump0 = 1.;
    double diff0 = 1.;
    int lastprint = ngen - ngen % iprint;
    boolean first = true;
    int nper = -1;
    int ngrav = 0;
    double chisq0 = 0.0, chisq1 = 0.0, diff1 = 0.0, prat = 0.0, diff = 0.0;
    double bestChi = 1.0E50;
    double rej = 0.0, pdiff = 0.0;
    int time_used = 0;

    while (looping) {
      acceptable = true;
      ++nper;
      if (!first) {
        if (nper != intervalForPermutations) {

//           Generate a random move or a model rotation
          ngen = (ngen + 1) % 10000000;

//   Here is practiced some annealing
//   dump = dumping factor for reducing the move size with
//          the number of already generated moves (move+perm in fact)
          dump = Math.pow(1. - (ngen + ngenp) / maxTrialForSolving, annealingExponent);

          if (moleculeReplacement) {
/*	    			if (diff0 < Rmax)
							dump0 = dump;
	    			rotate(xl, xln, na, tm1, tm2, s, dump0, ngrav, xg);
	    			ngrav = 1;*/
          } else {
            genmove(actualParams, tmpParams, dump);
//	    			newpos(xl, move, imove);
          }

//          Check distances
          if (shortDistanceConstraints) {
            if (moleculeReplacement) {
//	    				acceptable = checkmr(xln, ntyp, metric, ncum1, rcut, nas, natt);
            } else
              acceptable = check(tmpParams);
          }
        } else {

/*
 *           Generate a random permutation or a random translation
 *           Care if an atom permute with itself
 *           Care if an atom permute with another same atom-type
 */
          if (moleculeReplacement) {
/*	    			for (i = 1; i <= na; ++i) {
							xlnn[1][i] = xln[1][i];
							xlnn[2][i] = xln[2][i];
							xlnn[3][i] = xln[3][i];
	    			}
	    			transla(xlnn, xln, na, delta, a, bb, c, dump);
	    			++ngenp;

//          Check distances
	    			if (shortDistanceConstraints)
	    				acceptable = checkmr(xln, ntyp, metric, ncum1, rcut, nas, natt);*/
          } else {
            genmovep(actualParams, tmpParams, dump);
//	    			newposp(xl, movep, imove);
            ++ngenp;
          }

        }
        ngent = ngen + ngenp;
        if (acceptable && nper != intervalForPermutations) {
          ntried = (ntried + 1) % 10000000;
        } else if (nper == intervalForPermutations)
          ++ntriedp;
/*	    	if (acceptable) {
//           Calculate sums for chi-squared
					if (nper != intervalForPermutations && !moleculeReplacement) {
						sumstot(nhkl, fobs, kxr, xl, imove, move, diff,
												al, na, sum_fs, sum_f, sum_y);
					} else {
						if (moleculeReplacement)
							summr(nhkl, fobs, kxr, xln, diff, al, na, sum_fs, sum_f,
												sum_y);
						else
							sumstotp(	nhkl, fobs, kxr, xl, imove, movep,
			 											diff, al, na, sum_fs, sum_f, sum_y);
					}
				}*/
      }

//           Calculate chi-squared

      if (acceptable) {
        chisq1 = getFitness(tmpParams);
        diff = chisq1;
      }

//        Decide whether to accept move

      if (first) {
        acceptable = false;
        chisq0 = chisq1;
        bestChi = chisq1;
        diff0 = diff1;
      } else {
        prat = chisq0 - chisq1;
        if (prat < 0.) {

/*  Accept times to times an event that does not improve the fit
 *  on a criteria linked to dump
 */

          rej = reject * dump;
          pdiff = diff0 - diff1;
          if (pdiff < 0.) {
            if (pdiff > -rej)
              prat = Math.exp(pdiff / rej);
            if (prat < randomGenerator())
              acceptable = false;
            if (pdiff <= -rej)
              acceptable = false;
//	    			if (acceptable)
//							++nreject;
          }
        }
      }
      if (acceptable) {
        nacc = (nacc + 1) % 10000000;
        if (nper == intervalForPermutations)
          ++naccp;
      }

//        Write summary
      if (first || (Math.abs(ngent - lastprint) >= iprint && acceptable)) {
/*	    	espo_summary(	nacc, ngen, ntried, naccp, ngenp, ntriedp,
 *	    								diff, chisq, chisq1, nreject, dump);
*/
        System.out.println("Last configuration accepted: " + chisq0);
        for (int i = 0; i < bestParams.length; i++)
          System.out.println(actualParams[i]);

        lastprint = ngent - ngent % iprint;
      }

      if (acceptable) {
        makemove(actualParams, tmpParams);

        if (nper != intervalForPermutations && !moleculeReplacement) {
//           Make move
//	    		makemove();
          chisq0 = chisq1;
          diff0 = diff1;
        } else {
//           Make move in case of accepted permutation
          if (moleculeReplacement) {
/*	    			makemr(xl, xln);
	    			ngrav = 0;*/
          } else {
//	    			makemovep();
          }
          chisq0 = chisq1;
          diff0 = diff1;
        }
        if (chisq1 < bestChi) {
          makemove(bestParams, actualParams);
          bestChi = chisq1;
        }
      }
//   counter for permutation reset to zero
      if (intervalForPermutations == nper)
        nper = 0;

//        Check time

      time_used += 1.;
      if (time_used < 0. || time_used >= save_time) {
/*
 *           Write out the configuration,
 *      call write_config(TEXT,ngent,ntried+ntriedp,nacc,nsaved,
 *     										NA,NTYP,NAT,XL,3)
 */
        save_time += saveTrialInterval;
      }

//        Decide whether to continue program

      if (time_used < 0. || time_used >= maxTrialForSolving)
        looping = false;
      first = false;
      if (ngent >= timeToCheckRmax) {
        if (diff >= Rmax && acceptable) {
          System.out.println("Solution not found, try again!");
        }
      }
    } //        End of the solving loop

    System.out.println("Final chi :" + getFitness(bestParams));
  }

/*     initRandomizer must set the seed for the random number
       generator and obtain the current cpu clock reading in minutes
*/

  void initRandomizer() {

//    iseed = 2 * ((int) secnds_(0.0)) + 1;

/*  run the random number generator 100 times
    for avoiding affects of starting value */

    int time = (int) System.currentTimeMillis();  // safe because we're getting low-order bits
    randomizer = new ec.util.MersenneTwisterFast(time);

    return;
  }


  public double getFitness(double[] params) {

    Phase aphase = (Phase) getParent();
    aphase.setStructureParams(params);

    double ws1 = 0.0;
    double ws2 = 0.0;
    for (int i = 0; i < structureFactorList.length; i++) {
      int reflectionNumber = structureFactorList[i].structureFactor.length;
      for (int j = 0; j < reflectionNumber; j++) {
        StructureFactor sf = structureFactorList[i].structureFactor[j];
        Radiation rad = structureFactorList[i].radiation.getRadiation(0);
// todo fix        double Fhkl2 = aphase.Fhklcomp(sf.h, sf.k, sf.l, sf.d_spacing,
//                rad.getRadiationIDNumber(), rad.tubeNumber);
//        sf.Fhkl_calc = Math.sqrt(Fhkl2);
        if (sf.weight > 0) {
          ws1 += sf.Fhkl_calc;
          ws2 += sf.Fhkl_exp;
        }
      }
    }

//    System.out.println("Structure Factor normalization: " + Fmt.format(ws1/ws2));

    double wss = 0.0;
    for (int i = 0; i < structureFactorList.length; i++) {
      int reflectionNumber = structureFactorList[i].structureFactor.length;
      for (int j = 0; j < reflectionNumber; j++) {
        StructureFactor sf = structureFactorList[i].structureFactor[j];
        sf.Fhkl_calc *= ws2 / ws1;
        if (sf.weight > 0)
          wss += Math.abs(sf.Fhkl_calc - sf.Fhkl_exp);// * sf.Fhkl_esd;
      }
    }

    if (wss < bestWSS) {
      bestWSS = wss;
      for (int i = 0; i < bestParams.length; i++)
        bestParams[i] = params[i];
    }
    return wss;
  }

/*     randomGenerator generate a random integer number and return it
*/

  double randomGenerator() {
    // return value between 0 and 1 escluded
//		iseed = randomize(iseed);

    double random = randomizer.nextDouble();
    while (random == 1.0)  // escluding 1.0
      random = randomizer.nextDouble();
    return random;
  }

/*     genmove to generate a new move
*/

  void genmove(double[] oldParams, double[] newParams, double dump) {
    double delta = 1.0; // max move
    int imove = (int) (randomGenerator() * atomNumber);
    if (imove >= atomNumber)
      imove = atomNumber - 1;
    imove *= 3;
    for (int i = 0; i < oldParams.length; i++)
      newParams[i] = oldParams[i];
    for (int i = imove; i < imove + 3; i++) {
      newParams[i] += (randomGenerator() - .5) * 2. * delta * dump;
      while (newParams[i] >= 1.0)
        newParams[i] -= 1.0;
      while (newParams[i] < 0.0)
        newParams[i] += 1.0;
    }

/*    imove[2] = (int) (randomGenerator() * na) + 1;
    int mtype = 1;
    while(imove[2] > ncum[mtype])
			++mtype;
    if (delta[mtype] > 0.) {
    	switch (nsp[imove[2]]) {
				case 1:  //  General position
    			move[1][4] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
    			move[1][5] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
    			move[1][6] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
          break;
				case 2:  //  x,x,x : supposes a cubic symmetry
    			move[1][4] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
    			move[1][5] = move[1][4];
    			move[1][6] = move[1][4];
    			break;
				case 3:  //  0,0,0 : no move
					break;
				case 4:  //  x,1/4,z
    			move[1][4] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
    			move[1][5] = 0.;
    			move[1][6] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
    			break;
				case 5:  //  0,1/2,z
    			move[1][4] = 0.;
   			  move[1][5] = 0.;
    			move[1][6] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
    			break;
				case 6:  //  x,0,z
    			move[1][4] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
    			move[1][5] = 0.;
    			move[1][6] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
    			break;
				case 7:  //  x,0,1/4
    			move[1][4] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
    			move[1][5] = 0.;
    			move[1][6] = 0.;
    			break;
				case 8:  //  0,0,z
    			move[1][4] = 0.;
    			move[1][5] = 0.;
    			move[1][6] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
    			break;
				case 9:  //  x,y,0
    			move[1][4] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
    			move[1][5] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
    			move[1][6] = 0.;
    			break;
				case 10:  //  0,y,z
    			move[1][4] = 0.;
    			move[1][5] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
    			move[1][6] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
    			break;
				case 11:  //  1/2,y,0
    			move[1][4] = 0.;
    			move[1][5] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
    			move[1][6] = 0.;
    			break;
				case 12:  //  0,y,0
    			move[1][4] = 0.;
    			move[1][5] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
    			move[1][6] = 0.;
    			break;
				case 13:  //  x,0,0
    			move[1][4] = (randomGenerator() - .5) * 2. * delta[mtype] * dump;
    			move[1][5] = 0.;
			    move[1][6] = 0.;
    	}
    }*/
    return;
  }


/*     genmovep to generate a new permutation
*/

  void genmovep(double[] oldParams, double[] newParams, double dump) {

//  Find two atoms to be permuted

    if (atomNumber == 1) {
      genmove(oldParams, newParams, dump);
      return;
    }
    int imove1 = (int) (randomGenerator() * atomNumber);
    if (imove1 >= atomNumber)
      imove1 = atomNumber - 1;
    int imove2 = imove1;
    if (atomNumber == 2)
      imove2 = atomNumber - 1 - imove1;
    while (imove2 == imove1) {
      imove2 = (int) (randomGenerator() * atomNumber);
      if (imove2 >= atomNumber)
        imove2 = atomNumber - 1;
    }
    imove1 *= 3;
    imove2 *= 3;
    for (int i = 0; i < oldParams.length; i++)
      newParams[i] = oldParams[i];
    for (int i = 0; i < 3; i++) {
      newParams[imove1 + i] = oldParams[imove2 + i];
      newParams[imove2 + i] = oldParams[imove1 + i];
    }

/*    int jn = 0;
		while (++jn < 1000) {
    	imove[0] = (int) (randomGenerator() * na) + 1;
    	++jn;
    	int mtype = 1;
    	while(imove[0] > ncum[mtype])
				++mtype;
    	if (delta[mtype] > 0.) {
    		do {
    			imove[1] = (int) (randomGenerator() * na) + 1;
    			if (++jn < 1000)
    				break;
    			mtype = 1;
    			while(imove[1] > ncum[mtype])
						++mtype;
				} while(delta[mtype] <= 0.);
				if (jn < 1000) {
    			if (mtyp[imove[0]] != mtyp[imove[1]] &&
    					nsp[imove[0]] == nsp[imove[1]]) {
    				movep[1][4] = xl[1][imove[1]];
    				movep[1][5] = xl[2][imove[1]];
    				movep[1][6] = xl[3][imove[1]];
    				movep[2][4] = xl[1][imove[0]];
    				movep[2][5] = xl[2][imove[0]];
    				movep[2][6] = xl[3][imove[0]];
    				return;
    			}
				}
			}
		}*/
    return;
  }


/*     check whether the move
       is acceptable according to the cut-off restrictions
*/
  boolean check(double[] params) {

/*    int im = imove[2] * nas - nas + 1;
    int itype = 1;
    int jtype = 1;
    while(im > ncum[jtype])
			++jtype;
// Sum on independent atoms
    for (int i = 1; nas < 0 ? i >= natt : i <= natt; i += nas) {
// Sum on moving atoms
			for (l = 1; l <= nas; ++l) {
	    	if (i > ncum[itype])
					++itype;
	    	if (i != l + im - 1) {
					int i1 = Math.min(itype,jtype);
					int j1 = Math.max(itype,jtype);
					int ic = (i1 - 1) * (2 * ntyp - i1) / 2 + j1;

					double x = xll[1][i] - move[l][1] + 3.;
					double y = xll[2][i] - move[l][2] + 3.;
					double z = xll[3][i] - move[l][3] + 3.;
					x = (x / 2. - (int) (x / 2.)) * 2. - 1.;
					y = (y / 2. - (int) (y / 2.)) * 2. - 1.;
					z = (z / 2. - (int) (z / 2.)) * 2. - 1.;
					double dold = metric[1][1] * x * x + metric[2][2] * y * y +
												metric[3][3] * z * z + (metric[1][2] * x * y +
												metric[1][3] * x * z + metric[2][3] * y * z) * 2.;
					dold = Math.sqrt(dold);

// Avoid problems with special positions

					if (dold >= 1.e-4) {
						x = xll[1][i] - move[l][4] + 3.;
						y = xll[2][i] - move[l][5] + 3.;
						z = xll[3][i] - move[l][6] + 3.;
						x = (x / 2. - (int) (x / 2.)) * 2. - 1.;
						y = (y / 2. - (int) (y / 2.)) * 2. - 1.;
						z = (z / 2. - (int) (z / 2.)) * 2. - 1.;
						double dnew = metric[1][1] * x * x + metric[2][2] * y * y +
													metric[3][3] * z * z + (metric[1][2] * x * y +
													metric[1][3] * x * z + metric[2][3] * y * z) * 2.;
						dnew = Math.sqrt(dnew);
						if (dnew < rcut[ic] && dnew < dold)
		    			return false;
	    		}
				}
    	}
    }*/
    return true;
  }


/*     checkmr check whether the move for molecula
       is acceptable according to the cut-off restrictions
*/
/*	boolean checkmr(double[][] xl, int ntyp, double[][] metric,
									int[] ncum, double[] rcut, int nas, int natt) {

    int i = 0;
    double x, y, z, d;

    for (int l = 1; l <= na; ++l) {
			x = xl[1][l];
			y = xl[2][l];
			z = xl[3][l];
			for (int k = 1; k <= nas; ++k) {
	    	++i;
	    	xll[1][i] = x * smt[k][1][1] + y * smt[k][1][2] +
		    						z * smt[k][1][3] + tt[k][1];
	    	xll[2][i] = x * smt[k, 2][1] + y * smt[k][2][2] +
		    						z * smt[k][2][3] + tt[k][2];
	    	xll[3][i] = x * smt[k, 3][1] + y * smt[k][3][2] +
		    						z * smt[k][3][3] + tt[k][3];
			}
    }
    int itype = 1;
// Sum on independent atoms
    for (i = 1; nas < 0 ? i >= natt : i <= natt; i += nas) {
// Sum on moving atoms
			for (int l = 1; l <= natt; ++l) {
	    	if (i > ncum[itype])
					++itype;
	    	if (i != l) {
					jtype = 1;
					while(l > ncum[jtype])
		    		++jtype;
					int i1 = Math.min(itype,jtype);
					int j1 = Math.max(itype,jtype);
					int ic = (i1 - 1) * (2 * ntyp - i1) / 2 + j1;

					x = xll[1][i] - xll[1][l] + 3.;
					y = xll[2][i] - xll[2][l] + 3.;
					z = xll[3][i] - xll[3][l] + 3.;
					x = (x / 2. - (int) (x / 2.)) * 2. - 1.;
					y = (y / 2. - (int) (y / 2.)) * 2. - 1.;
					z = (z / 2. - (int) (z / 2.)) * 2. - 1.;
					d = metric[1][1] * x * x + metric[2][2] * y * y +
							metric[3][3] * z * z + (metric[1][2] * x * y +
							metric[1][3] * x * z + metric[2][3] * y * z) * 2.;
					d = Math.sqrt(d);

// Avoid problems with special positions

					if (d < 1.e-4)
						break;
					if (d < rcut[ic])
						return false;
	    	}
	    }
    }
    return true;
	}*/


/*     makemove make the move
*/
  void makemove(double[] actualParams, double[] params) {
    for (int i = 0; i < params.length && i < bestParams.length; i++)
      actualParams[i] = params[i];
    return;
  }


/*     makemr make permutation
*/
/*	void makemr(double[][] xl, double[][] xln, int nas, int na) {

    int i, j, k;
    double x, y, z;
    int im;

    for (im = 1; im <= na; ++im) {
			xl[1][im] = xln[1][im];
			xl[2][im] = xln[2][im];
			xl[3][im] = xln[3][im];
			if (shortDistanceConstraints) {
				i = (im - 1) * nas;
				x = xl[1][im];
				y = xl[2][im];
				z = xl[3][im];
				for (k = 1; k <= nas; ++k) {
	    		++i;
	    		xll[1][i] = x * smt[k][1][1] + y * smt[k][1][2] +
		    							z * smt[k][1][3] + tt[k][1];
	    		xll[2][i] = x * smt[k][2][1] + y * smt[k][2][2] +
		    							z * smt[k][2][3] + tt[k][2];
	    		xll[3][i] = x * smt[k][3][1] + y * smt[k][3][2] +
		    							z * smt[k][3][3] + tt[k][3];
	    		for (j = 1; j <= 3; ++j) {
						xll[j][i] = xll[j][i] - (int) xll[j][i];
						if (xll[j][i] > 1.)
		    			xll[j][i] = xll[j][i] - 1.;
						if (xll[j][i] < 0.)
		    			xll[j][i] = xll[j][i] + 1.;
	    		}
				}
			}
    }
    return;
	}*/

/*
//     dcell to compute reciprocal cell

	double[] dcell(double[] celln, double[] al, double v) {

    int i, j, k, l;
    double[] cell = new double[6];
    int icell = 0;
    double degrad = .017453292519944444;

    for (i = 1; i <= 6; ++i) {
      cell[i - 1] = celln[i];
    }
    for (i = 1; i <= 3; ++i) {
      l = i + 3;
      if (cell[l - 1] - 90. != 0.)
        cell[l - 1] = cos(degrad * cell[l - 1]);
      else
        cell[l - 1] = 0.;
    }
    rcelln = trcl(cell);
//     RCELL IS THE RECIPROCAL CELL CONSTANTS
    for (i = 1; i <= 3; ++i) {
      al[i][i] = rcelln[i] * rcelln[i];
      int[] jk = perm(i);
      j = jk[0];
      k = jk[1];
      if (j - k >= 0) {
        al[k][j] = rcelln[j] * 2. * rcelln[k] * rcelln[i + 3];
      } else {
        al[j][k] = rcelln[j] * 2. * rcelln[k] * rcelln[i + 3];
      }
    }
    for (i = 4; i <= 6; ++i)
      rcelln[i] = Math.acos(rcelln[i]) / degrad;
    return rcelln;
  }

//     trcl to compute reciprocal cell

	double[] trcl(double[] celln) {

    double r__1;
    int i, j, k, l;
    double abc;
    double[] sina = new double[3];
    double prod;

//     TRANSFORMS REAL CELL TO RECIPROCAL OR VICE VERSA
//     INPUT CELL IS IN ARRAY CELL AS LENGTHS AND COSINES

    abc = 1.;
    prod = 2.;
    double v = -2.;
    for (i = 1; i <= 3; ++i) {
      l = i + 3;
// Computing 2nd power
      sina[i - 1] = 1. - celln[l] * celln[l];
      v += sina[i - 1];
      sina[i - 1] = Math.sqrt(sina[i - 1]);
      prod *= celln[l];
      abc *= celln[i];
    }
    v = abc * Math.sqrt(v + prod);
//      V IS CELL VOLUME
//     PUT INVERTED CELL INTO RCELL

    double[] rcelln = new double[7];
    for (i = 1; i <= 3; ++i) {
      int[] jk = perm(i);
      j = jk[0];
      k = jk[1];
      rcelln[i] = celln[j] * celln[k] * sina[i - 1] / v;
      l = i + 3;
      rcelln[l] = (celln[j + 3] * celln[k + 3] - celln[l]) /
      						(sina[j - 1] * sina[k - 1]);
    }
    return rcelln;
	}


//     perm to compute permutation

  int[] perm(int i) {

    int[] jk = new int[2];

//     PERMS USEFUL COMBINATIONS OF INTEGERS IN THE RANGE 1 TO 3
    if (i - 2 < 0) {
      jk[0] = 2;
      jk[1] = 3;
    } else if (i - 2 == 0) {
      jk[0] = 3;
      jk[1] = 1;
    } else {
      jk[0] = 1;
      jk[1] = 2;
    }
    return jk;
  }

//     matrix to compute cell matrices

	void matrix(double[] celln, double[] rcelln, double[] tm2, double[] tm1) {

    double rad, cosg, sing;

    rad = 57.2957795;

// *** CALCULATE ORTHOGONAL MATRIX ***

// 180./Pi
    sing = Math.sin(celln[6] / rad);
    cosg = Math.cos(celln[6] / rad);
    tm1[1] = 1. / celln[1];
    tm1[2] = -cosg / (celln[1] * sing);
    tm1[3] = rcelln[1] * Math.cos(rcelln[5] / rad);
    tm1[4] = 1. / (celln[2] * sing);
    tm1[5] = rcelln[2] * Math.cos(rcelln[4] / rad);
    tm1[6] = rcelln[3];

// *** CALCULATE INVERSE MATRIX ***

    tm2[1] = celln[1];
    tm2[2] = celln[2] * cosg;
    tm2[3] = celln[3] * Math.cos(celln[5] / rad);
    tm2[4] = celln[2] * sing;
    tm2[5] = -celln[3] * Math.sin(celln[5] / rad) * Math.cos(rcelln[4] / rad);
    tm2[6] = 1 / tm1[6];
    return;
} */


/*     transla compute molecule translation
*/
  void transla(double[][] xl, double[][] xln, int na, double[] delta,
               double a, double b, double c, double dump) {

/*    int i;
    double amove1, amove2, amove3;

    amove1 = (randomGenerator() - .5) * 2. * delta[1] / a * dump;
    amove2 = (randomGenerator() - .5) * 2. * delta[1] / b * dump;
    amove3 = (randomGenerator() - .5) * 2. * delta[1] / c * dump;
    for (i = 1; i <= na; ++i) {
      xln[1][i] = xl[1][i] + amove1;
      xln[2][i] = xl[2][i] + amove2;
      xln[3][i] = xl[3][i] + amove3;
    }
*/
    return;
  }

/*     rotate compute molecule rotation
*/
  void rotate(double[][] xl, double[][] xln, int na, int iseed,
              double[] tm1, double[] tm2, double s, double dump,
              int ngrav, double[] xg) {

/*    int i;
    double 	x, y, z, r1, r2, t1, t2, t3, r3, r4, r5, r6, r7, r8, r9, pi,
	     			xa, ya, za, xx, yy, zz, ct1, ct2, ct3, pi2, st1, st2, st3;

//  Find current gravity center

    if (ngrav != 1) {
      x = 0.;
      y = 0.;
      z = 0.;
      for (i = 1; i <= na; ++i) {
        x += xl[1][i];
        y += xl[2][i];
        z += xl[3][i];
      }
      xg[1] = x / s;
      xg[2] = y / s;
      xg[3] = z / s;
    }

//  Change coordinates of the rigid model in Cartesian coordinates
    for (i = 1; i <= na; ++i) {
      xa = xl[1][i] - xg[1];
      ya = xl[2][i] - xg[2];
      za = xl[3][i] - xg[3];
      xln[1][i] = tm1[1] * xa + tm1[2] * ya + tm1[3] * za;
      xln[2][i] = tm1[4] * ya + tm1[5] * za;
      xln[3][i] = tm1[6] * za;
    }

//  Find 3 Euler angles at random

//      t1=ran(iseed)*PI2*dump
    t1 = randomGenerator() * Math.PI * dump;
    t2 = randomGenerator() * Constants.PI2 * dump;
    t3 = randomGenerator() * Constants.PI2 * dump;
    ct1 = Math.cos(t1);
    st1 = Math.sin(t1);
    ct2 = Math.cos(t2);
    st2 = Math.sin(t2);
    ct3 = Math.cos(t3);
    st3 = Math.sin(t3);
    r1 = ct1 * ct3 - st1 * ct2 * st3;
    r2 = st1 * ct3 + ct1 * ct2 * st3;
    r3 = st2 * st3;
    r4 = -st1 * ct2 * ct3 - ct1 * st3;
    r5 = -st1 * st3 + ct1 * ct2 * ct3;
    r6 = st2 * ct3;
    r7 = st1 * st2;
    r8 = -ct1 * st2;
    r9 = ct2;
    for (i = 1; i <= na; ++i) {

//  Apply the rotation matrix

	    xx = r1 * xln[1][i] + r2 * xln[2][i] + r3 * xln[3][i];
      yy = r4 * xln[1][i] + r5 * xln[2][i] + r6 * xln[3][i];
	    zz = r7 * xln[1][i] + r8 * xln[2][i] + r9 * xln[3][i];
      xln[1][i] = xx;
      xln[2][i] = yy;
      xln[3][i] = zz;

//  Refurbish the coordinates in the unknown structure cell

      xln[1][i] = tm2[1] * xln[1][i] + tm2[2] * xln[2][i]
        				+ tm2[3] * xln[3][i] + xg[1];
      xln[2][i] = tm2[4] * xln[2][i] + tm2[5] * xln[3][i]
        				+ xg[2];
      xln[3][i] = tm2[6] * xln[3][i] + xg[3];
    }

//  We have now the new rotated rigid model inside the unknown
*/
    return;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JRMSDPDOptionsD(parent, this);
    return adialog;
  }

  public class JRMSDPDOptionsD extends JOptionsDialog {

//    JTextField popTF = null;
//    JTextField genTF = null;

    public JRMSDPDOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

/*			JPanel tfPanel = new JPanel();
      tfPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 6, 6));
      principalPanel.add(BorderLayout.NORTH, tfPanel);

			tfPanel.add(new JLabel("Number of generations: "));
			genTF = new JTextField(Constants.FLOAT_FIELD);
			genTF.setToolTipText("Set the number of generations for the Genetic Algorithm");
			tfPanel.add(genTF);

			tfPanel = new JPanel();
      tfPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 6, 6));
      principalPanel.add(BorderLayout.CENTER, tfPanel);

			tfPanel.add(new JLabel("Population size: "));
			popTF = new JTextField(Constants.FLOAT_FIELD);
			popTF.setToolTipText("Set the population size for each generation for the Genetic Algorithm");
			tfPanel.add(popTF);*/

/*			tfPanel = new JPanel();
      tfPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      principalPanel.add(BorderLayout.SOUTH, tfPanel);

      JButton jb = new JButton("Solve structure");
			tfPanel.add(jb);
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					solveCrystalStructure();
          Component parent = getParent();
          while (parent != null && !(parent instanceof myJFrame)) {
            parent = parent.getParent();
          }
          if (parent != null)
            ((myJFrame) parent).updateFieldsChanged();
				}
			});
			jb.setToolTipText("Press this to solve the crystal structure from structure factors");
			*/
      setTitle("GA Structure Solution options panel");
      initParameters();
      pack();
    }

    public void initParameters() {
//      popTF.setText(getPopulationSize());
//      genTF.setText(getGenerationsNumber());
    }

    public void retrieveParameters() {
//      setPopulationSize(popTF.getText());
//      setGenerationsNumber(genTF.getText());
    }

  }
}

