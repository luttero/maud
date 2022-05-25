/*
 * @(#)lFilePar.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr;

import it.unitn.ing.rista.interfaces.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.comp.*;

import java.io.*;

/**
 * The lFilePar is a class
 *
 * @version $Revision: 1.8 $, $Date: 2004/11/18 09:30:49 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public interface lFilePar extends basicObj {

  public void mainfunction(boolean hasoutput, boolean refreshAll);

//  public void update(Maincat aobject);

  public void refreshparametersV();

  public String getOperatorField();

  public String getTitleField();

  public void setOperatorField(String value);

  public void setTitleField(String value);

  public int getNumberofIterations();

  public String getDirectory();

  public void setDirectory(String folder);

  public String getFileName();

  public void setFileName(String thename, boolean reading);

  public void readall(Reader in, ProgressPanel pcontrol);

  public void writeall(BufferedWriter out);

  public void startingRefine();

  public void launchrefine(OutputPanel aframe);

  public void refineWizard(OutputPanel aframe, int wizardindex);

  public void prepareWizard(OutputPanel aframe, int wizardindex);

  public void resetWizard();

  public void stoprefinement();

  public void endOfComputation();

  public void stopcomputation();

  public void fittingFileOutput();

  public void compute(OutputPanel aframe);

  public Sample getSelectedSample();

  public boolean isSaved();

  public int getNumberOfFreeParameters();

  public ListVector getList(int index);

  public void newObject(int index);

  public void removeObject(int index);

  public void loadObject(int index, String filename);

  public void prepareComputation();

  public OptimizationAlgorithm getOptimizationAlgorithm();

}
