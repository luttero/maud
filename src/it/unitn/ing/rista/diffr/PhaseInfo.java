/*
 * @(#)PhaseInfo.java created 01/05/16 Casalino
 *
 * Copyright (c) 1996-2016 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of the author and it is 
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

package it.unitn.ing.rista.diffr;

import it.unitn.ing.jsginfo.T_RTMx;
import it.unitn.ing.jsginfo.T_SgInfo;
import it.unitn.ing.rista.util.*;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Vector;

/**
 * The PhaseInfo is a class ....
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.0 $, $Date: 01/05/16 18:13 $
 * @since JDK1.1
 */

public class PhaseInfo {

	public T_SgInfo SgInfo = null;

	public int LGgroup = -1;
	public int PGgroup = -1;

	public Vector<SitePosition> sitePositionv = new Vector<SitePosition>(0, 1);

	public PhaseInfo() {

	}

	public int InversionOffOrigin() {
		if (SpaceGroups.useCCTBX())
			return SgInfo.InversionOffOrigin;
		else
			return SgInfo.InversionOffOrigin;
	}

	public void refreshSpaceGroupComputation(String SgName, int sgconv) {

		sitePositionv.removeAllElements();

		if (SpaceGroups.useCCTBX()) {

		} else {
			int i;
			char F_Convention;

			int iList, fi;
			int nTrV, iTrV, nLoopInv, iLoopInv;
			int[] TrV;
			T_RTMx SMx = new T_RTMx();
			T_RTMx[] lsmx;
			double[] signx = new double[3], signy = new double[3], signz = new double[3],
					constvalue = new double[3];


			F_Convention = 'A';
			if (sgconv == 3)
				F_Convention = 'H';

			SgInfo = new T_SgInfo(SgName, F_Convention);

			LGgroup = T_SgInfo.LG_Number(SgInfo.PointGroup);
			PGgroup = T_SgInfo.PG_Index(SgInfo.PointGroup);

			nLoopInv = SgInfo.Sg_nLoopInv();

			nTrV = SgInfo.LatticeInfo.nTrVector;
			TrV = SgInfo.LatticeInfo.TrVector;
			int kTrV = 0;
			lsmx = SgInfo.ListSeitzMx;


			for (iTrV = 0; iTrV < nTrV; iTrV++, kTrV += 3) {
				for (iLoopInv = 0; iLoopInv < nLoopInv; iLoopInv++) {
					if (iLoopInv == 0)
						fi = 1;
					else
						fi = -1;

					int ilsmx = 0;

					for (iList = 0; iList < SgInfo.nList; iList++, ilsmx++) {
						for (i = 0; i < 9; i++)
							SMx.s.R[i] = fi * lsmx[ilsmx].s.R[i];

						for (i = 0; i < 3; i++)
							SMx.s.T[i] = T_SgInfo.iModPositive(fi * lsmx[ilsmx].s.T[i] + TrV[i + kTrV], T_SgInfo.STBF);

						signx[0] = SMx.s.R[0];
						signy[0] = SMx.s.R[1];
						signz[0] = SMx.s.R[2];
						constvalue[0] = ((double) SMx.s.T[0]) / T_SgInfo.STBF;

						signx[1] = SMx.s.R[3];
						signy[1] = SMx.s.R[4];
						signz[1] = SMx.s.R[5];
						constvalue[1] = ((double) SMx.s.T[1]) / T_SgInfo.STBF;

						signx[2] = SMx.s.R[6];
						signy[2] = SMx.s.R[7];
						signz[2] = SMx.s.R[8];
						constvalue[2] = ((double) SMx.s.T[2]) / T_SgInfo.STBF;
						addPosition(signx[0], signy[0], signz[0], constvalue[0],
								signx[1], signy[1], signz[1], constvalue[1],
								signx[2], signy[2], signz[2], constvalue[2]);
					}
				}
			}

		}

//    System.out.println("- Refreshing Space Group -");
//    System.out.println("Laue group: " + SpaceGroups.laueGroup[SpaceGroups.getLGNumber(PGgroup)]);
	}

	public int getSitePositionNumber() {
		return sitePositionv.size();
	}

	public SitePosition getSitePosition(int i) {
		return sitePositionv.elementAt(i);
	}

	public void addPosition(double signx1, double signy1, double signz1, double constant1,
	                        double signx2, double signy2, double signz2, double constant2,
	                        double signx3, double signy3, double signz3, double constant3) {
		sitePositionv.addElement(new SitePosition(signx1, signy1, signz1, constant1,
				signx2, signy2, signz2, constant2,
				signx3, signy3, signz3, constant3));
	}

	public void printCustomInformations(OutputStream out) throws IOException {
		// to be implemented by subclasses
		printLine(out, "       General position");
		for (int i = 0; i < sitePositionv.size(); i++) {
			printString(out, (i + 1) + ")  " + (sitePositionv.elementAt(i)).getx());
			printString(out, "  |  " + (sitePositionv.elementAt(i)).gety());
			printLine(out, "  |  " + (sitePositionv.elementAt(i)).getz());
		}
		newLine(out);
	}

	public final void printLine(OutputStream out, String string) throws IOException {
		printString(out, string);
		newLine(out);
	}

	public final void newLine(OutputStream out) throws IOException {
		synchronized(this) {
			if (out != null)
				printString(out, Constants.lineSeparator);
		}
	}

	public final void printString(OutputStream out, String string) throws IOException {
		synchronized(this) {
			if (out != null)
				out.write(string.getBytes());
		}
	}

}
