/*
 * @(#)ConvertImageToSpectra.java created Oct 26, 2007 Caen
 *
 * Copyright (c) 2007 Luca Lutterotti All Rights Reserved.
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
 * The ConvertImageToSpectra is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Oct 26, 2007 12:44:31 AM $
 * @since JDK1.1
 */
public class ConvertImageToSpectra {
  double[] deltaE = new double[4], deltaB = new double[4];
  double PHIDISC = 0.0;
  double sigma = 16.5;
  double phis0 = 0.0;
  double DD = 13.86;
  double rdet = 7.2;
  double sigmaDA = 28.1;
  double phiDA = 152.0;
  double roDAS = 0;
  double phiDAS = 0;
  double OmegaDN = 0;
  int IRESOLUTION = 2048;
  double[] intensity;
  double[] xcoord;
  double[] ycoord;
  int v1301 = 1301;
  int v19 = 19;
  int shape = CIRCULAR;

  public static boolean needArea = false;

  public static int DONOTCHECK = 0;
  public static int CIRCULAR = 1;
  public static int RECTANGULAR = 2;
  public static int SQUARE = 3;

  static double PIH = Math.PI / 2.0;
//  static double PIF = Math.PI / 180.0;

  public ConvertImageToSpectra(double[] intensity, double[] xcoord, double[] ycoord) {
    this(intensity, xcoord, ycoord, 0.0, 16.5, 0.0, 13.86, 7.2, 28.1, 152.0, 0.0, 0.0, 0.0, 2048, CIRCULAR);
  }

  public ConvertImageToSpectra(double[] intensity, double[] xcoord, double[] ycoord,
                     double PHIDISC, double sigma, double phis0, double DD, double rdet, double sigmaDA,
                     double phiDA, double roDAS, double phiDAS, double OmegaDN,
                     int IRESOLUTION) {
    this(intensity, xcoord, ycoord, PHIDISC, sigma, phis0, DD, rdet, sigmaDA,
        phiDA, roDAS, phiDAS, OmegaDN,
        IRESOLUTION, CIRCULAR);
  }

  public ConvertImageToSpectra(double[] intensity, double[] xcoord, double[] ycoord,
                     double PHIDISC, double sigma, double phis0, double DD, double rdet, double sigmaDA,
                     double phiDA, double roDAS, double phiDAS, double OmegaDN,
                     int IRESOLUTION, int shape) {
    this.intensity = intensity;
    this.xcoord = xcoord;
    this.ycoord = ycoord;
    this.PHIDISC = PHIDISC;
    this.sigma = sigma;
    this.phis0 = phis0;
    this.DD = DD;
    this.rdet = rdet;
    this.sigmaDA = sigmaDA;
    this.phiDA = phiDA;
    this.roDAS = roDAS;
    this.phiDAS = phiDAS;
    this.OmegaDN = OmegaDN;
    this.IRESOLUTION = IRESOLUTION;
    this.shape = shape;
  }

  public double[][][] convertToSpectra(double delta2Eta,
                                       double delta2Bragg, double theta2Start, double theta2End) {

    double deltaEta = delta2Eta / 2.0;
    double deltaBragg = delta2Bragg / 2.0;
    v19 = (int) (360.0 / delta2Eta) + 1;
    v1301 = (int) ((theta2End - theta2Start) / delta2Bragg) + 1;
    deltaE = new double[]{-deltaEta, deltaEta, deltaEta, -deltaEta};
    deltaB = new double[]{-deltaBragg, -deltaBragg, deltaBragg, deltaBragg};
//    int ITEST = 1912962;

    double[] sETAcorner = new double[4], cETAcorner = new double[4];
    int[] ICHANmin = new int[v19], ICHANmax = new int[v19];
    double[] ThetacoR = new double[4], PHIcoR = new double[4];
    double[] ThetaYcoR = new double[4], PHIycoR = new double[4];
    double[][][] SPECTR = new double[2][v1301][v19];
    double[][] gwork = new double[3][3], gdiscrot = new double[3][3];

    double RDETF2 = rdet * rdet;

//    int InsidePIX = 0;
    double IFmax = 0.0, IFmin = 1.0E40;
    int totalPixels = intensity.length;
    for (int i = 0; i < totalPixels; i++) {
      double R2 = xcoord[i] * xcoord[i] + ycoord[i] * ycoord[i];
//      if (i == ITEST) System.out.println(" Intensity(" + ITEST + ") = " + intensity[i]
//          + ", " + R2 + " > " + RDETF2);
      if (shape == CIRCULAR && R2 > RDETF2)
        intensity[i] = -100;
      else {
        double IDETH = intensity[i];
        if (IDETH < IFmin) IFmin = IDETH;
        if (IDETH > IFmax) IFmax = IDETH;
//        InsidePIX++;
      }
    }

    double sigmar = sigma * Constants.DEGTOPI;
    double sSIG = Math.sin(sigmar);
    double cSIG = Math.cos(sigmar);

    double sigmaDAr = sigmaDA * Constants.DEGTOPI;
    double phiDAr = phiDA * Constants.DEGTOPI;

    double alphawr = PIH - OmegaDN * Constants.DEGTOPI;
    double betawr = PIH + sigmaDAr;
    double gammawr = -phiDAr;

    double caw = Math.cos(alphawr);
    double saw = Math.sin(alphawr);
    double cbw = Math.cos(betawr);
    double sbw = Math.sin(betawr);
    double cgw = Math.cos(gammawr);
    double sgw = Math.sin(gammawr);

    gwork[0][0] = caw * cbw * cgw - saw * sgw;
    gwork[0][1] = saw * cbw * cgw + caw * sgw;
    gwork[0][2] = -sbw * cgw;
    gwork[1][0] = -caw * cbw * sgw - saw * cgw;
    gwork[1][1] = -saw * cbw * sgw + caw * cgw;
    gwork[1][2] = sbw * sgw;
    gwork[2][0] = caw * sbw;
    gwork[2][1] = saw * sbw;
    gwork[2][2] = cbw;

//     vector of the detector center seen from KBS

    double detcentX = DD * Math.sin(PIH - sigmaDAr) * Math.cos(phiDAr);
    double detcentY = DD * Math.sin(PIH - sigmaDAr) * Math.sin(phiDAr);
    double detcentZ = DD * Math.cos(PIH - sigmaDAr);

//     vector describing DAS from KBI (basic coordinate system)

    double DASX = roDAS * Math.cos(phiDAS * Constants.DEGTOPI);
    double DASY = roDAS * Math.sin(phiDAS * Constants.DEGTOPI);

//                                numerator for d-determination at line

    double hnumerator = DASX * detcentX + DASY * detcentY + DD * DD;

//                  direct beam below IP (unit vector in KBI)

    double dbX = -Math.cos(sigmar);
    double dbZ = -Math.sin(sigmar);

//     rotation matrix of KBI -> KAI (including sample rotation by PHIDISC)

    double BAPHIr = (-phis0 + PHIDISC) * Constants.DEGTOPI;
    double sBA = Math.sin(BAPHIr);
    double cBA = Math.cos(BAPHIr);

    gdiscrot[0][0] = cBA;
    gdiscrot[0][1] = sBA;
    gdiscrot[0][2] = 0.0;
    gdiscrot[1][0] = -sBA;
    gdiscrot[1][1] = cBA;
    gdiscrot[1][2] = 0.0;
    gdiscrot[2][0] = 0.0;
    gdiscrot[2][1] = 0.0;
    gdiscrot[2][2] = 1.0;

//     source in KBI (unit vector)

    double Xs = -dbX;
//      Ys=0.D0
    double Zs = -dbZ;

//     preparation to summarize the pixel intensities into channels

    double[][] SUMEBCELL = new double[v1301][v19];
    int[][] ITREFF = new int[v1301][v19];
    double[] sT2BRAGGCM = new double[v1301], cT2BRAGGCM = new double[v1301],
        Theta2BRAGGCM = new double[v1301];

    for (int ICHAN = 0; ICHAN < v1301; ICHAN++) {
      double Theta2BRAGGC = theta2Start + delta2Bragg * ICHAN; // todo the spectra coordinate to define
      Theta2BRAGGCM[ICHAN] = Theta2BRAGGC;
      double Theta2BRAGGCR = Theta2BRAGGC * Constants.DEGTOPI;
      sT2BRAGGCM[ICHAN] = Math.sin(Theta2BRAGGCR);
      cT2BRAGGCM[ICHAN] = Math.cos(Theta2BRAGGCR);
    }

    int IETAmin = v19 - 1;
    int IETAmax = 0;
    int ICHANmina = v1301 - 1;
    int ICHANmaxa = 0;

//     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! START of the pixel loop !!!!!!!!!!!!

    for (int i = 0; i < totalPixels; i++) { // do 5

      double IDETINTH = intensity[i];

//      if (i == ITEST) System.out.println(" Intensity(" + ITEST + ") = " + IDETINTH);
      if (IDETINTH >= 0) {

//l    NY=I/2048
//l    NX=I-2048*NY

        double XR = xcoord[i]; //l (NX-1023.5D0)*pixelsize
        double YR = ycoord[i]; //l (1023.5D0-NY)*pixelsize
        double ZR = 0.0;

//  Pixelposition in the detector
//  vector R from the detector center to the pixel centre
//  transferred by gwork into KB sitting in the detector center ('KBdet')

        double[] hKBdetV = gmalvector(gwork, XR, YR, ZR);

//    Transfer of KBdetv into KBS and then into KBI (Basic coordinate system)

        double wX = detcentX + hKBdetV[0] + DASX;
        double wY = detcentY + hKBdetV[1] + DASY;
        double wZ = detcentZ + hKBdetV[2];

//    unit vector of the reflected beam in KBI

        double rw = Math.sqrt(wX * wX + wY * wY + wZ * wZ);
        double wUX = wX / rw;
        double wUY = wY / rw;
        double wUZ = wZ / rw;

        double cosBRAGG2Theta = wUX * dbX + wUZ * dbZ;
        double sinBRAGG2Theta = Math.sqrt(1.0 - cosBRAGG2Theta * cosBRAGG2Theta);
        double BRAGG2Theta = Math.acos(cosBRAGG2Theta) * Constants.PITODEG;

//        if (i == ITEST) System.out.println(" 2ThetaBRAGG(" + ITEST + ") = " + BRAGG2Theta);

        int ICHAN = (int) ((BRAGG2Theta - (theta2Start - deltaBragg)) / delta2Bragg);

        if (ICHAN < ICHANmina) {
          ICHANmina = ICHAN;
        }
        if (ICHAN > ICHANmaxa) {
          ICHANmaxa = ICHAN;
        }

        double cosETA = (-wUX * sSIG + wUZ * cSIG) / sinBRAGG2Theta;
        double ETA = Math.acos(cosETA) * Constants.PITODEG;
        if (wUY > 0.0) ETA = -ETA;
        while (ETA > 180.0)
          ETA -= 360.0;
//        if (i == ITEST) System.out.println(" ETA(" + ITEST + ") = " + ETA);

        int IETA = (int) ((ETA + 180 - deltaEta) / delta2Eta);
/*        if (ETA >= -(deltaEta / 2.0)) { //l GOTO 88
          if (ETA >= (deltaEta * 3.0) && ETA < (deltaEta * 5.0)) IETA = 1;
          else if (ETA >= deltaEta && ETA < (deltaEta * 3.0)) IETA = 2;
          else if (ETA >= -deltaEta && ETA < deltaEta) IETA = 3;
        } else //l 88
          IETA = (int) ((-ETA + (deltaEta * 7.0)) / (deltaEta * 2.0));*/
//                                                                      Cell intensities
        if (IETA > 0 && ICHAN >= 0 && IETA <= v19 && ICHAN < v1301) {
          SUMEBCELL[ICHAN][IETA - 1] += IDETINTH;

          ITREFF[ICHAN][IETA - 1]++;

          if (IETA < IETAmin) IETAmin = IETA - 1;
          if (IETA > IETAmax) IETAmax = IETA - 1;
        }
      } // end of if intensity[i] >= 0
//              !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! END of the pixel loop !!!!!!!!!!!!
    } // 5   CONTINUE

//    System.out.println("  IETAmin = " + IETAmin + "  ETACmax = " + (15.0-5.0*IETAmin));
//    System.out.println("  IETAmax = " + IETAmax + "  ETACmin = " + (15.0-5.0*IETAmax));

//    double Theta2BRAGGCmin=20.0+0.05*ICHANmina;
//    double Theta2BRAGGCmax=20.0+0.05*ICHANmaxa;

/*    System.out.println(" absolute ICHANmin = "+ICHANmina+
                  " absolute Theta2BRAGGCmin = "+Theta2BRAGGCmin);
    System.out.println(" absolute ICHANmax = "+ICHANmaxa+
                  " absolute Theta2BRAGGCmax = "+Theta2BRAGGCmax);*/

    for (int IETA = 0; IETA < v19; IETA++) {   // do 33
      ICHANmin[IETA] = 88888;
      ICHANmax[IETA] = -88888;
      int ICHANminh = 88888;
      int ICHANmaxh = -88888;

      for (int ICHAN = 0; ICHAN < v1301; ICHAN++) {    // do 34
        if (ITREFF[ICHAN][IETA] <= 0)
//                                                    no cell-hits -> dooped
          SUMEBCELL[ICHAN][IETA] = -100.0;
        else {

          if (ICHAN < ICHANminh) ICHANminh = ICHAN;
          if (ICHAN > ICHANmaxh) ICHANmaxh = ICHAN;
          ICHANmin[IETA] = ICHANminh;
          ICHANmax[IETA] = ICHANmaxh;
        }
      }
    }

    for (int ICHAN = 0; ICHAN < v1301; ICHAN++) {
      for (int LAUF = 0; LAUF < v19; LAUF++)
        SPECTR[1][ICHAN][LAUF] = -888.0;
    }
    double sEC = 0, cEC = 0;
    for (int IETA = IETAmin; IETA <= IETAmax; IETA++) {   //l do 30

      if (needArea) {
        double ETAC = delta2Eta * IETA - 180.0;
        double ETACR = ETAC * Constants.DEGTOPI;
        sEC = Math.sin(ETACR);
        cEC = Math.cos(ETACR);
//                                                  4 EtaBRAGG-cell corners
        for (int ICORN = 0; ICORN < 4; ICORN++) {
          double ETAcorner = ETAC + deltaE[ICORN];
          double ETAcornerR = ETAcorner * Constants.DEGTOPI;
          sETAcorner[ICORN] = Math.sin(ETAcornerR);
          cETAcorner[ICORN] = Math.cos(ETAcornerR);
        }
      }

//      System.out.println(IETA + ", " + ICHANmin[IETA] + ", " + ICHANmax[IETA]);
      for (int ICHAN = ICHANmin[IETA]; ICHAN < ICHANmax[IETA]; ICHAN++) { //l do 31

        double Theta2BRAGGC = Theta2BRAGGCM[ICHAN];
        if (needArea) {
          double sT2BRAGGC = sT2BRAGGCM[ICHAN];
          double cT2BRAGGC = cT2BRAGGCM[ICHAN];
//                                            Theta,Phi cell centres in KBI
//                                            ThetaCC PhiCC
//                                     really unit vector 'Rscatter' in KBI

          double wUXC = -sSIG * cEC * sT2BRAGGC - cSIG * cT2BRAGGC;
          double wUYC = -sEC * sT2BRAGGC;
          double wUZC = cSIG * cEC * sT2BRAGGC - sSIG * cT2BRAGGC;

          double ABSH = Math.abs(wUZC);
          double ThetaCC, PhiCC;
          if (ABSH < 1.0) {
            ThetaCC = Math.acos(wUZC) * Constants.PITODEG;
            double SINTCC = Math.sqrt(1.0 - wUZC * wUZC);
            double SINX = wUYC / SINTCC;
            double COSX = wUXC / SINTCC;

            PhiCC = WINKELDEG(COSX, SINX);
          } else {
            ThetaCC = 0.0;
            if (wUZC < 0.0) ThetaCC = 180.0;
            PhiCC = 0.0;
          }
          double dsmall = hnumerator / (wUXC * detcentX + wUYC * detcentY + wUZC * detcentZ);

          for (int IVAR = 0; IVAR < 4; IVAR++) {       // do 50
//                                                  Theta,Phi corners in KBI
            double sEco = sETAcorner[IVAR];
            double cEco = cETAcorner[IVAR];

            double BRAGG2corner = Theta2BRAGGC + deltaB[IVAR];
            double BRAGG2cornerR = BRAGG2corner * Constants.DEGTOPI;
            double sT2BRAGGco = Math.sin(BRAGG2cornerR);
            double cT2BRAGGco = Math.cos(BRAGG2cornerR);
//                                                     really unit vector
            double wUXco = -sSIG * cEco * sT2BRAGGco - cSIG * cT2BRAGGco;
            double wUYco = -sEco * sT2BRAGGco;
            double wUZco = cSIG * cEco * sT2BRAGGco - sSIG * cT2BRAGGco;

            ABSH = Math.abs(wUZco);
            if (ABSH >= 1.0) {
//        Thetaco=Math.acos(wUZco)*Constants.PITODEG
              ThetacoR[IVAR] = Math.acos(wUZco);
              double SINTco = Math.sqrt(1.0 - wUZco * wUZco);
              double SINX = wUYco / SINTco;
              double COSX = wUXco / SINTco;

              double PHIco = WINKELDEG(COSX, SINX);

              PHIcoR[IVAR] = PHIco * Constants.PITODEG;
            } else {
              ThetacoR[IVAR] = 0.0;
//        Thetaco=0.0
              if (wUZco < 0.0) ThetacoR[IVAR] = Math.PI;
//        IF(wUZco.LT.0.0)Thetaco=180.0
//        PHIco=0.0
              PHIcoR[IVAR] = 0.0;
//                                                        ThetaYcorner/PhiYcorner-part
            }
//  scattering vector N in KBI (N = source(unit vector) + reflected beam(unit vector))

            double XN = wUXco + Xs;
            double YN = wUYco;
            double ZN = wUZco + Zs;
//                                   unit vector of N in KBI

            double rw = Math.sqrt(XN * XN + YN * YN + ZN * ZN);
            XN = XN / rw;
            YN = YN / rw;
            ZN = ZN / rw;

//   Description of N in the (may be rotated) KAI -> 'Y' (N-direction on pole sphere)

            double[] YXX = gmalvector(gdiscrot, XN, YN, ZN);
            double YX = YXX[0];
            double YY = YXX[1];
            double YZ = YXX[2];

            ABSH = Math.abs(YZ);
            if (ABSH < 1.0) {
              ThetaYcoR[IVAR] = Math.acos(YZ);
              double SINT = Math.sqrt(1.0 - YZ * YZ);
              double SINX = YY / SINT;
              double COSX = YX / SINT;

              double PHIyco = WINKELDEG(COSX, SINX);
              PHIycoR[IVAR] = PHIyco * Constants.DEGTOPI;
            } else {
              ThetaYcoR[IVAR] = 0.0;
              if (YZ < 0.0) ThetaYcoR[IVAR] = Math.PI;
              PHIycoR[IVAR] = 0.0;
            }//   ===============================================  end IVAR-corner-loop
          }  //l 50     CONTINUE
//                                                        SC-determination
          double SC = SSTEREO(ThetacoR, PHIcoR);
//                                                        SY-determination
          double SY = SSTEREO(ThetaYcoR, PHIycoR);
//                                                             ThetaYCC,PhiYCC
//  scattering vector N in KBI (N = source(unit vector) + reflected beam(unit vector))

          double XN = wUXC + Xs;
          double YN = wUYC;
          double ZN = wUZC + Zs;
//                                                 unit vector of N in KBI
          double rw = Math.sqrt(XN * XN + YN * YN + ZN * ZN);
          XN = XN / rw;
          YN = YN / rw;
          ZN = ZN / rw;

//    Description of N in the (may be rotated) KAI -> 'Y' (N-direction on pole sphere)

          double[] YXX = gmalvector(gdiscrot, XN, YN, ZN);
          double YX = YXX[0];
          double YY = YXX[1];
          double YZ = YXX[2];

          ABSH = Math.abs(YZ);
          double PHIy, ThetaY;
          if (ABSH < 1.0) {
            ThetaY = Math.acos(YZ) * Constants.PITODEG;
            double SINT = Math.sqrt(1.0 - YZ * YZ);
            double SINX = YY / SINT;
            double COSX = YX / SINT;

            PHIy = WINKELDEG(COSX, SINX);
          } else {
            ThetaY = 0.0;
            if (YZ < 0.0) ThetaY = 180.0;
            PHIy = 0.0;
          }
        }
// ................................................................................
// DIMENSION SPECTR(0:1300,11)
//                  ICHAN  LAUF
//
// ICHAN - 2ThetaBRAGG (0.05degree-size) channel number
//
//               LAUF=1 -> ICHAN
//               LAUF=2 -> 2ThetaBRAGG (channel center 'CC')
//               LAUF=3 -> Intensity SUMEBCELL(ICHAN,IETA)
//               LAUF=4 -> ThetaCC in KBI
//               LAUF=5 -> PhiCC   in KBI
//               LAUF=6 -> SC stereo angle of the cell from KBI
//               LAUF=7 -> ThetaYCC of the scattering vector N for the cell centre
//                         on the pole sphere described in KA
//               LAUF=8 -> PhiYCC   of the scattering vector N for the cell centre
//                         on the pole sphere described in KA
//               LAUF=9 -> SY stereo angle of the EB-cell-Ns on the pole sphere
//               LAUF=10-> d distance of the centre of the EB-cell on the photoplate
//                         from KBI
//               LAUF=11-> HITnumber (ITREFF) number of pixels that hitted the cell
//
// Sitting in the IETA-loop (only IETA from IETAmin up to IETAmax are considered)
// IETA=1-19 number of the (5degree-size) ETA channel
// ETAC = 10 - 5*(IETA - 1)   ETAC - channel centre
//
// ICHAN - 2ThetaBRAGG (0.05degree-size) channel number
// only the values given in SPECTR for ICHANmin up to ICHANmax are sensefull !!!!
//
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        SPECTR[0][ICHAN][IETA] = Theta2BRAGGC;
//        System.out.println("2Theta " + ICHAN + ", " + IETA + ", " + Theta2BRAGGC);
        SPECTR[1][ICHAN][IETA] = SUMEBCELL[ICHAN][IETA];
//        System.out.println("Intensity " + SUMEBCELL[ICHAN][IETA]);
//        SPECTR[ICHAN][0] = ICHAN;
//        SPECTR[ICHAN][1] = Theta2BRAGGC;
//        SPECTR[ICHAN][2] = SUMEBCELL[ICHAN][IETA];
//        SPECTR[ICHAN][3] = ThetaCC;
//        SPECTR[ICHAN][4] = PhiCC;
//        SPECTR[ICHAN][5] = SC;
//        SPECTR[ICHAN][6] = ThetaY;
//        SPECTR[ICHAN][7] = PHIy;
//        SPECTR[ICHAN][8] = SY;
//        SPECTR[ICHAN][9] = dsmall;
//        SPECTR[ICHAN][10] = ITREFF[ICHAN][IETA];
//                                                      end of the ICHAN-loop
      } //l 31     CONTINUE

    }        //l30     CONTINUE
    return SPECTR;
  }

  protected double WINKELDEG(double COSX, double SINX) {

    double WINK = 0.0;

    if (Math.abs(COSX) < 1) {
      WINK = Math.acos(COSX) * Constants.PITODEG;
      if (SINX <= 0.0) WINK = 360.0 - WINK;
    } else if (COSX < 0.0) WINK = 180.0;

    return WINK;
  }

  protected double[] gmalvector(double[][] g, double x, double y, double z) {

    double[] xyzres = new double[3];

    xyzres[0] = g[0][0] * x + g[0][1] * y + g[0][2] * z;
    xyzres[1] = g[1][0] * x + g[1][1] * y + g[1][2] * z;
    xyzres[2] = g[2][0] * x + g[2][1] * y + g[2][2] * z;

    return xyzres;
  }

  protected double SSTEREO(double[] ThetaycornerR, double[] PhiycornerR) {
    double SY;

    double casmall = Math.sin(ThetaycornerR[0]) * Math.sin(ThetaycornerR[1]) *
        Math.cos(PhiycornerR[0] - PhiycornerR[1]) +
        Math.cos(ThetaycornerR[0]) * Math.cos(ThetaycornerR[1]);

    double cbsmall = Math.sin(ThetaycornerR[1]) * Math.sin(ThetaycornerR[2]) *
        Math.cos(PhiycornerR[1] - PhiycornerR[2]) +
        Math.cos(ThetaycornerR[1]) * Math.cos(ThetaycornerR[2]);

    double ccsmall = Math.sin(ThetaycornerR[2]) * Math.sin(ThetaycornerR[0]) *
        Math.cos(PhiycornerR[2] - PhiycornerR[0]) +
        Math.cos(ThetaycornerR[2]) * Math.cos(ThetaycornerR[0]);

    double sa = Math.sqrt(1.0 - casmall * casmall);
    double sb = Math.sqrt(1.0 - cbsmall * cbsmall);
    double sc = Math.sqrt(1.0 - ccsmall * ccsmall);

    double argA = (casmall - cbsmall * ccsmall) / (sb * sc);
    double argB = (cbsmall - ccsmall * casmall) / (sc * sa);
    double argC = (ccsmall - casmall * cbsmall) / (sa * sb);

    double SY1 = Math.acos(argA) + Math.acos(argB) + Math.acos(argC) - Math.PI;

//                                                    second spherical triangle

    casmall = Math.sin(ThetaycornerR[2]) * Math.sin(ThetaycornerR[3]) *
        Math.cos(PhiycornerR[2] - PhiycornerR[3]) +
        Math.cos(ThetaycornerR[2]) * Math.cos(ThetaycornerR[3]);

    cbsmall = Math.sin(ThetaycornerR[3]) * Math.sin(ThetaycornerR[0]) *
        Math.cos(PhiycornerR[3] - PhiycornerR[0]) +
        Math.cos(ThetaycornerR[3]) * Math.cos(ThetaycornerR[0]);

    sa = Math.sqrt(1.0 - casmall * casmall);
    sb = Math.sqrt(1.0 - cbsmall * cbsmall);

    argA = (casmall - cbsmall * ccsmall) / (sb * sc);
    argB = (cbsmall - ccsmall * casmall) / (sc * sa);
    argC = (ccsmall - casmall * cbsmall) / (sa * sb);

    double SY2 = Math.acos(argA) + Math.acos(argB) + Math.acos(argC) - Math.PI;
    SY = SY1 + SY2;

    return SY;
  }

  public static double[][] getTransformationMatrix(double omegaDN, double phiDA, double theta2DET, double omega) {
    omegaDN *= Constants.DEGTOPI;
    phiDA *= Constants.DEGTOPI;
    theta2DET *= Constants.DEGTOPI;
    omega *= Constants.DEGTOPI;

    double cosOmegaDN = Math.cos(omegaDN);
    double sinOmegaDN = Math.sin(omegaDN);
    double cosPhiDA = Math.cos(phiDA);
    double sinPhiDA = Math.sin(phiDA);
    double cosTheta2DET = Math.cos(theta2DET - omega);
    double sinTheta2DET = Math.sin(theta2DET - omega);
    double cosOmega = Math.cos(omega);
    double sinOmega = Math.sin(omega);

    double[][] a = new double[3][3];
    a[0][0] = cosOmegaDN;
    a[0][1] = -sinOmegaDN;
    a[0][2] = 0;
    a[1][0] = sinOmegaDN;
    a[1][1] = cosOmegaDN;
    a[1][2] = 0;
    a[2][0] = 0;
    a[2][1] = 0;
    a[2][2] = 1;

    double[][] b = new double[3][3];
    b[0][0] = 1;
    b[0][1] = 0;
    b[0][2] = 0;
    b[1][0] = 0;
    b[1][1] = cosTheta2DET;
    b[1][2] = -sinTheta2DET;
    b[2][0] = 0;
    b[2][1] = sinTheta2DET;
    b[2][2] = cosTheta2DET;

    double[][] tmat = MoreMath.MatProduct(b, a);

    a[0][0] = cosPhiDA;
    a[0][1] = 0;
    a[0][2] = -sinPhiDA;
    a[1][0] = 0;
    a[1][1] = 1;
    a[1][2] = 0;
    a[2][0] = sinPhiDA;
    a[2][1] = 0;
    a[2][2] = cosPhiDA;

    b = MoreMath.MatProduct(a, tmat);


    a[0][0] = 1;
    a[0][1] = 0;
    a[0][2] = 0;
    a[1][0] = 0;
    a[1][1] = cosOmega;
    a[1][2] = -sinOmega;
    a[2][0] = 0;
    a[2][1] = sinOmega;
    a[2][2] = cosOmega;

    return MoreMath.MatProduct(a, b);
  }

  public static double[][] getTransformationMatrixExplicit(double omegaDN, double phiDA, double theta2DET) {
    double[][] tmat = new double[3][3];

    omegaDN *= Constants.DEGTOPI;
    phiDA *= Constants.DEGTOPI;
    theta2DET *= Constants.DEGTOPI;

    double cosOmegaDN = Math.cos(omegaDN);
    double sinOmegaDN = Math.sin(omegaDN);
    double cosPhiDA = Math.cos(phiDA);
    double sinPhiDA = Math.sin(phiDA);
    double cosTheta2DET = Math.cos(theta2DET);
    double sinTheta2DET = Math.sin(theta2DET);

    tmat[0][0] = cosOmegaDN * cosPhiDA;
    tmat[0][1] = -sinOmegaDN * cosPhiDA;
    tmat[0][2] = -sinPhiDA;
    tmat[1][0] = sinOmegaDN * cosTheta2DET + cosOmegaDN * sinPhiDA * sinTheta2DET;
    tmat[1][1] = cosOmegaDN * cosTheta2DET - sinOmegaDN * sinPhiDA * sinTheta2DET;
    tmat[1][2] = cosPhiDA * sinTheta2DET;
    tmat[2][0] = -sinOmegaDN * sinTheta2DET + cosOmegaDN * sinPhiDA * cosTheta2DET;
    tmat[2][1] = -cosOmegaDN * sinTheta2DET - sinOmegaDN * sinPhiDA * cosTheta2DET;
    tmat[2][2] = cosPhiDA * cosTheta2DET;

    return tmat;
  }

  public static double[] getTransformedVector(double[][] tmat, double x, double y, double xCenter, double yCenter,
                                              double detectorDistance, double zs) {
    double[] xf = new double[3];
    double x0 = x - xCenter;
    double y0 = y - yCenter;

    xf[0] = tmat[0][0] * x0 + tmat[0][1] * y0 + tmat[0][2] * detectorDistance;
    xf[1] = tmat[1][0] * x0 + tmat[1][1] * y0 + tmat[1][2] * detectorDistance;
    xf[2] = tmat[2][0] * x0 + tmat[2][1] * y0 + tmat[2][2] * detectorDistance - zs;

    return xf;
  }

  public static double[][] getTransformationMatrixNew(double omegaDN, double phiDA, double etaDA, double theta2DET, double omega) {
    omegaDN *= Constants.DEGTOPI;
    phiDA *= Constants.DEGTOPI;
    etaDA *= Constants.DEGTOPI;
    theta2DET *= Constants.DEGTOPI;
    omega *= Constants.DEGTOPI;

    double cosOmegaDN = Math.cos(omegaDN);
    double sinOmegaDN = Math.sin(omegaDN);
    double cosPhiDA = Math.cos(phiDA);
    double sinPhiDA = Math.sin(phiDA);
    double cosTheta2DET = Math.cos(theta2DET);
    double sinTheta2DET = Math.sin(theta2DET);
    double cosEtaDA = Math.cos(etaDA);
    double sinEtaDA = Math.sin(etaDA);

    double[][] a = new double[3][3];
    a[0][0] = 0;
    a[0][1] = 0;
    a[0][2] = -1;
    a[1][0] = sinOmegaDN;
    a[1][1] = cosOmegaDN;
    a[1][2] = 0;
    a[2][0] = cosOmegaDN;
    a[2][1] = -sinOmegaDN;
    a[2][2] = 0;

    double[][] b = new double[3][3];
    b[0][0] = cosPhiDA;
    b[0][1] = sinPhiDA;
    b[0][2] = 0;
    b[1][0] = -sinPhiDA;
    b[1][1] = cosPhiDA;
    b[1][2] = 0;
    b[2][0] = 0;
    b[2][1] = 0;
    b[2][2] = 1;

    double[][] tmat = MoreMath.MatProduct(b, a);

    a[0][0] = cosTheta2DET;
    a[0][1] = 0;
    a[0][2] = sinTheta2DET;
    a[1][0] = 0;
    a[1][1] = 1;
    a[1][2] = 0;
    a[2][0] = -sinTheta2DET;
    a[2][1] = 0;
    a[2][2] = cosTheta2DET;

    b = MoreMath.MatProduct(a, tmat);

    a[0][0] = 1;
    a[0][1] = 0;
    a[0][2] = 0;
    a[1][0] = 0;
    a[1][1] = cosEtaDA;
    a[1][2] = sinEtaDA;
    a[2][0] = 0;
    a[2][1] = -sinEtaDA;
    a[2][2] = cosEtaDA;

    return MoreMath.MatProduct(a, b);
  }

  public static double[] getTransformedVectorNew(double[][] tmat, double x, double y, double xCenter, double yCenter,
                                              double detectorDistance) {
    double[] xf = new double[3];
    double x0 = x - xCenter;
    double y0 = y - yCenter;
    double z0 = detectorDistance;

    xf[0] = tmat[0][0] * x0 + tmat[0][1] * y0 + tmat[0][2] * z0;
    xf[1] = tmat[1][0] * x0 + tmat[1][1] * y0 + tmat[1][2] * z0;
    xf[2] = tmat[2][0] * x0 + tmat[2][1] * y0 + tmat[2][2] * z0;

    return xf;
  }

  public static double[] get2ThetaEtaNew(double[] xf) {
    double[] thetaEta = new double[2];
    double z2 = xf[2] * xf[2];
    double y2 = xf[1] * xf[1];
    double x2 = xf[0] * xf[0];

    // compute 2theta
    double a = Math.sqrt(z2 + y2);
    if (Math.abs(a) < 1.0E-9)
      return thetaEta;
    double b = Math.sqrt(z2 + y2 + x2);
    thetaEta[0] = Math.asin(a / b);
    if (xf[0] > 0.0)
      thetaEta[0] = Constants.PI - thetaEta[0];

    // compute eta
    if (Math.abs(xf[2]) < 1.0E-9) {
      if (xf[1] >= 0.0)
        thetaEta[1] = Constants.PI2;
      else
        thetaEta[1] = Constants.PI + Constants.PI2;
    } else {
      thetaEta[1] = Math.atan(-xf[1] / xf[2]);
      if (xf[2] < 0.0)
        thetaEta[1] += Constants.PI;
    }
    return thetaEta;
  }

  public static double get2ThetaNew(double[] xf) {
    double theta = 0.0;
    double z2 = xf[2] * xf[2];
    double y2 = xf[1] * xf[1];
    double x2 = xf[0] * xf[0];
// System.out.println(x2 + " " + y2 + " " + z2);
    // compute 2theta
    double a = Math.sqrt(z2 + y2);
    if (Math.abs(a) < 1.0E-9)
      return theta;
    double b = Math.sqrt(z2 + y2 + x2);
    theta = Math.asin(a / b);
    if (xf[0] > 0.0)
      theta = Constants.PI - theta;
    return theta;
  }

  public static double[] getABCForYNew(double omegaDN, double phiDA, double etaDA, double theta2DET, double omega,
                                       double xCenter, double y, double yCenter, double detectorDistance,
                                       double theta2Circle) {

    double[] ABC = new double[3];
    omegaDN *= Constants.DEGTOPI;
    phiDA *= Constants.DEGTOPI;
    etaDA *= Constants.DEGTOPI;
    theta2DET *= Constants.DEGTOPI;
    theta2Circle *= Constants.DEGTOPI;

    double cosOmegaDN = Math.cos(omegaDN);
    double sinOmegaDN = Math.sin(omegaDN);
    double cosPhiDA = Math.cos(phiDA);
    double sinPhiDA = Math.sin(phiDA);
    double cosEtaDA = Math.cos(etaDA);
    double sinEtaDA = Math.sin(etaDA);
    double cosTheta2DET = Math.cos(theta2DET);
    double sinTheta2DET = Math.sin(theta2DET);
    double sin2Theta2Circle = Math.sin(theta2Circle);
    sin2Theta2Circle *= sin2Theta2Circle;
    double oneMinusSin2Theta2Circle = 1.0 - sin2Theta2Circle;
    double dy = y - yCenter;

    double c1 = sinOmegaDN * sinPhiDA;
    double c2 = dy * cosOmegaDN - xCenter * sinOmegaDN;
    double c4 = -dy * sinOmegaDN - xCenter * cosOmegaDN;
    double c3 = c2 * sinPhiDA - detectorDistance * cosPhiDA;

    double A1 = cosTheta2DET * c1 + sinTheta2DET * cosOmegaDN;
    double B1 = cosTheta2DET * c3 + sinTheta2DET * c4;
    double A2c = cosPhiDA * sinOmegaDN;
    double B2c = detectorDistance *sinPhiDA + cosPhiDA * c2;
    double A3c = -sinTheta2DET * c1 + cosTheta2DET * cosOmegaDN;
    double B3c = -sinTheta2DET * c3 + cosTheta2DET * c4;
    double A2 = A2c * cosEtaDA + A3c * sinEtaDA;
    double B2 = B2c * cosEtaDA + B3c * sinEtaDA;
    double A3 = A3c * cosEtaDA - A2c * sinEtaDA;
    double B3 = B3c * cosEtaDA - B2c * sinEtaDA;

    ABC[0] = (A3 * A3 + A2 * A2) * oneMinusSin2Theta2Circle - A1 * A1 * sin2Theta2Circle;
    ABC[1] = 2.0 * ((A3 * B3 + A2 * B2) * oneMinusSin2Theta2Circle - A1 * B1 * sin2Theta2Circle);
    ABC[2] = (B3 * B3 + B2 * B2) * oneMinusSin2Theta2Circle - B1 * B1 * sin2Theta2Circle;

    return ABC;
  }

  public static double get2Theta(double[] x) {
    if (Math.abs(x[2]) < 1.0E-9) {
      return 90.0;
    }
    double tt = Math.sqrt(x[0] * x[0] + x[1] * x[1]) / x[2];
    tt = Math.atan(tt) * Constants.PITODEG;
    if (x[2] < 0)
      tt += 180.0;
    return tt;
  }

  public static double getEta(double[] x) {
    if (Math.abs(x[1]) < 1.0E-9) {
      if (x[0] > 0.0)
        return 90.0;
      else
        return -90.0;
    }
    double eta = x[0] / x[1];
    eta = Math.atan(eta) * Constants.PITODEG;
    if (x[1] < 0)
      eta += 180.0;
    return eta;
  }

  public static double get2ThetaRad(double[] x) {
    if (Math.abs(x[2]) < 1.0E-9) {
      return Math.PI / 2.0;
    }
    double tt = Math.sqrt(x[0] * x[0] + x[1] * x[1]) / x[2];
    tt = Math.atan(tt);
    if (x[2] < 0)
      tt += Math.PI;
    return tt;
  }

  public static double getEtaRad(double[] x) {
    if (Math.abs(x[1]) < 1.0E-9) {
      if (x[0] > 0.0)
        return Math.PI / 2.0;
      else
        return -Math.PI / 2.0;
    }
    double eta = -x[0] / x[1];
    eta = Math.atan(eta);
//    if (x[1] < 0)
//      eta += Math.PI;
    return eta;
  }
}
