/*
 * @(#)ArbitraryTexture.java created 09/02/2000 Casalino
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.rta;

import it.unitn.ing.rista.diffr.*;

/**
 *  The ArbitraryTexture is a class that implements an arbitrary correction of the
 *  intensity based only on the extracted values that are used without any
 *  manipolation.
 *
 *
 * @version $Revision: 1.5 $, $Date: 2004/08/12 09:36:07 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class ArbitraryTexture extends Texture {
  //insert class definition here

  public boolean notLoaded = true;
  boolean needRestore = false;

  public ArbitraryTexture(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "arbitrary tex";
    IDlabel = "arbitrary tex";
    description = "select this for arbitrary correction of intensities";
  }

  public ArbitraryTexture(XRDcat aobj) {
    this(aobj, "arbitrary tex");
  }

  public ArbitraryTexture() {
    identifier = "arbitrary tex";
    IDlabel = "arbitrary tex";
    description = "select this for arbitrary correction of intensities";
  }

  public void computeTextureFactor(Phase aphase, Sample asample) {
//    if (notLoaded)
//      loadTextureFactors(aphase, asample);
    FilePar aparFile = getFilePar();
    if (!aparFile.isTextureComputationPermitted() || !refreshComputation)
      return;

	  for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
		  for (int k = 0; k < asample.getActiveDataSet(i).activedatafilesnumber(); k++)
			  asample.getActiveDataSet(i).getActiveDataFile(k).storeExperimentalOverComputedTextureFactors(aphase);
	  }
	  refreshComputation = false;
  }

/*  public void loadTextureFactors(Phase aphase, Sample asample) {
    notLoaded = false;
    String filename = new String(getFilePar().getDirectory() +
      aphase.toXRDcatString() + ".apf");
    BufferedReader PFreader = Misc.getReader(filename);
    double[] sampleAngles = asample.getSampleAngles();
//    double[] expInt = null;
    int[][] hkli = null;
    double[] weight = null;
    Vector expPF = null;
    String token = null;
    int maxizoveri_local = 1;

    if (PFreader != null) {
      try {

        String line;
        PFreader.readLine();
        StringTokenizer st = null;
        if (filename.toLowerCase().endsWith(".apf")) {
          // texture weights Maud export format

          line = PFreader.readLine();
          st = new StringTokenizer(line, "' ,\t\r\n");
          int numberPoleFiguresPF = Integer.valueOf(st.nextToken());
          boolean mistake = false;
          for (int i = 0; i < numberPoleFiguresPF; i++) {
            Reflection refl = aphase.reflectionv.elementAt(i);
            if (refl.isGoodforTexture()) {
              if (!mistake)
                line = PFreader.readLine();
              mistake = false;
//              System.out.println("Reading line: " + line);
              st = new StringTokenizer(line, "' ,\t\r\n");
              int h = Integer.valueOf(st.nextToken());
              int k = Integer.valueOf(st.nextToken());
              int l = Integer.valueOf(st.nextToken());
              line = PFreader.readLine();
//              System.out.println("Reading line 2: " + line);
              st = new StringTokenizer(line, "' ,\t\r\n");
              int numberOfPFPoint = Integer.valueOf(st.nextToken());
              for (int j = 0; j < numberOfPFPoint; j++) {
                line = PFreader.readLine();
                if (line == null || line.contains("H K L")) {
                  mistake = true;
                  break;
                }
//                System.out.println("Reading line data: " + line);
                st = new StringTokenizer(line, "' ,\t\r\n");
                if (st.hasMoreTokens()) st.nextToken();
                if (st.hasMoreTokens()) st.nextToken();
                double pf = Double.valueOf(st.nextToken());
                refl.setExpTextureFactor(j, pf);
                if (st.hasMoreTokens()) {
                  double pfc = Double.valueOf(st.nextToken());
                  refl.setTextureFactor(j, pfc);
                }
              }
            }
          }
        }
      }catch(Exception ex) {}
    }

  }*/

  public void refreshForNotificationDown(XRDcat source, int reason) {
    if (!getFilePar().isComputingDerivate())
      refreshComputation = true;
  }

  public boolean needIntensityExtractor() {
    return true;
  }

}
