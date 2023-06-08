/*
 * @(#)StructureSolutionGeneticAlgorithmClassic.java created 19/08/2001 Riva Del Garda
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
import it.unitn.ing.rista.util.*;
import java.awt.*;
import java.util.Vector;
import javax.swing.*;
import ec.*;
import it.unitn.ing.rista.interfaces.*;

/**
 *  The StructureSolutionGeneticAlgorithm is a method to solve the Crystal 
 *  Structure using a Genetic Algorithm
 *  
 * @version $Revision: 1.10 $, $Date: 2006/07/20 13:39:06 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class StructureSolutionGeneticAlgorithmClassic extends StructureSolutionMethod
              implements GAProblem {
	
	public static String[] diclistc = {	"_riet_ga_population_size", "_riet_ga_generations_number",
                                      "_riet_ga_mutation_prob", "_riet_ga_permutation_prob"
                                    };
  public static String[] diclistcrm = {	"_riet_ga_population_size", "_riet_ga_generations_number",
                                      "_riet_ga_mutation_prob", "_riet_ga_permutation_prob"
                                    };
	public static String[] classlistc = {};
	public static String[] classlistcs = {};
	
  EvolutionState state;
  boolean startRandomConfiguration = true;
  
  String[] argsToDefine = {
// number of threads
    "evalthreads=", "1",
    "breedthreads=", "1",
// a good random seed for thread 0
    "seed.0=", "4357",
// ec.EvolutionState
    "generations=", "20",
    "pop.subpop.0.species.ind.min-gene=", "0.0",
    "pop.subpop.0.species.ind.max-gene=", "1.0",
    "pop.subpop.0.species.ind.genome-size=", "100",
    "select.tournament.size=", "2",
    "pop.subpop.0.species.ind.mutation-prob=", "0.01",
    "pop.subpop.0.species.ind.crossover-type=", "one",
    "pop.subpop.0.size=", "500",
    "pop.subpop.0.species.ind.permutation-prob=", "0.01"
  };
  
  static int EVALTHREADS = 0;
  static int BREEDTHREADS = 2;
  static int GENERATIONS_NUMBER = 6;
  static int GENOME_SIZE = 12;
  static int POPULATION_SIZE = 20;
  static int MUTATION_PROB = 16;
  static int PERMUTATION_PROB = 22;
  
  StructureFactorList[] structureFactorList = null;
  double[] defParams = null;
  double[] bestParams = null;
  double defWSS = 0.0;
  double bestWSS = 0.0;
  
	public StructureSolutionGeneticAlgorithmClassic(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initBaseObject();
		identifier = "Disabled Genetic Algorithm SDPD Classic";
  	IDlabel = "Genetic Algorithm SDPD Classic";
  	description = "select this to use a Genetic Algorithm";
	}
	
	public StructureSolutionGeneticAlgorithmClassic(XRDcat aobj) {
		this(aobj, "Genetic Algorithm SDPD Classic");
	}
	
	public StructureSolutionGeneticAlgorithmClassic() {
		identifier = "Disabled Genetic Algorithm SDPD Classic";
  	IDlabel = "Genetic Algorithm SDPD Classic";
  	description = "select this to use a Genetic Algorithm";
	}

	public void initConstant() {
		Nstring = 4;
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
		setPopulationSize("500");
		setGenerationsNumber("20");
    setMutationProbability("0.01");
    setPerMutationProbability("0.01");
	}
	
	public void setPopulationSize(String value) {
		stringField[0] = new String(value);
	}
	
	public String getPopulationSize() {
		return stringField[0];
	}
	
	public void setGenerationsNumber(String value) {
		stringField[1] = new String(value);
	}
	
	public String getGenerationsNumber() {
		return stringField[1];
	}
	
	public void setMutationProbability(String value) {
		stringField[2] = new String(value);
	}
	
	public String getMutationProbability() {
		return stringField[2];
	}
	
	public void setPerMutationProbability(String value) {
		stringField[3] = new String(value);
	}
	
	public String getPerMutationProbability() {
		return stringField[3];
	}
	
	public boolean canSolveStructure() {
		return true;
	}
	
	public boolean solveStructure(StructureFactorList[] structureFactorList) {

    this.structureFactorList = structureFactorList;
    
//  Init the randomizer for random number generation
    initAll();
    
//  Generate the starting structure configuration
    generateStartingPositions();
    
    startSolutionLoop();
    return true;
	}
  
  double[][][][] scatf = null;
  int nAtom = 0;
  int sitenumber = 0;
  SitePosition[] sitepos = null;
  double x[][][] = null;
  
  void initAll() {
    int maxReflectionNumber = 0;
    for (int i = 0; i < structureFactorList.length; i++) {
      int reflectionNumber = structureFactorList[i].structureFactor.length;
      if (maxReflectionNumber < reflectionNumber)
        maxReflectionNumber = reflectionNumber;
      for (int j = 0; j < reflectionNumber; j++) {
        StructureFactor sf = structureFactorList[i].structureFactor[j];
        sf.Fhkl_exp = sf.Fhkl_calc; // the calc is the mean value for all exp
      }
    }
    Phase aphase = (Phase) getParent();
    aphase.cellVolumeComp();
    defParams = aphase.getStructureParams();
//    aphase.setStructureParams(bestParams);
    bestParams = new double[defParams.length];
//    double defWSS1 = getOldFitness(defParams);
    sitenumber = aphase.getPhaseInfo().getSitePositionNumber();
    sitepos = new SitePosition[sitenumber];
    for (int i = 0; i < sitenumber; i++)
    	sitepos[i] = aphase.getPhaseInfo().sitePositionv.elementAt(i);

    nAtom = 0;
    Vector atomVector = aphase.getFullAtomList();
    for (int i = 0; i < atomVector.size(); i++) {
      if (((AtomSite) atomVector.elementAt(i)).useThisAtom) {
        nAtom++;
      }
    }
    x = new double[nAtom][sitenumber][3];
    scatf = new double[2][nAtom][structureFactorList.length][maxReflectionNumber];
//    int np = 0;
//    System.out.println(nAtom + " " + sitenumber);
  	for (int n = 0; n < nAtom; n++) {
  		AtomSite ato = aphase.getFullAtomList().get(n);
      if (ato.useThisAtom) {
      for (int i = 0; i < structureFactorList.length; i++) {
        int reflectionNumber = structureFactorList[i].structureFactor.length;
        for (int j = 0; j < reflectionNumber; j++) {
          StructureFactor sf = structureFactorList[i].structureFactor[j];
          Radiation rad = structureFactorList[i].radiation.getRadiation(0);
          double[] scatft = ato.scatfactor(sf.d_spacing, rad);
          double i_dspace = 0.25 / (sf.d_spacing * sf.d_spacing);
          double DWfactor = ato.DebyeWaller(sf.h, sf.k, sf.l, i_dspace) * ato.getQuantityD() / sitenumber;
            //ato.DebyeWaller(sf.h, sf.k, sf.l, sf.d_spacing) * ato.getQuantityD() / sitenumber;
          scatf[0][n][i][j] = scatft[0] * DWfactor;
          scatf[1][n][i][j] = scatft[1] * DWfactor;
          
//          System.out.println(n + " " + ato.DebyeWaller(sf.h, sf.k, sf.l, sf.d_spacing,
//                                        structureFactorList[i].radiation));

        }
      }
      }
    }
    defWSS = getFitness(defParams);
//    System.out.println(defWSS1 + " " + defWSS);
    bestWSS = 1.0E50;
    for (int i = 0; i < bestParams.length; i++)
      bestParams[i] = defParams[i];
    
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
    String genomeSize = Integer.toString(defParams.length);
    String[] args = new String[argsToDefine.length];
    for (int i = 0; i < argsToDefine.length; i+=2) {
      args[i] = "-p";
      if (i == GENOME_SIZE)
        args[i+1] = argsToDefine[i] + genomeSize;
      else if (i == POPULATION_SIZE)
        args[i+1] = argsToDefine[i] + getPopulationSize();
      else if (i == GENERATIONS_NUMBER)
        args[i+1] = argsToDefine[i] + getGenerationsNumber();
      else if (i == MUTATION_PROB)
        args[i+1] = argsToDefine[i] + getMutationProbability();
      else if (i == PERMUTATION_PROB)
        args[i+1] = argsToDefine[i] + getPerMutationProbability();
      else
        args[i+1] = argsToDefine[i] + argsToDefine[i+1];
    }
    GeneticAlgorithm.evolve(this, args);
    System.out.println("End of solution loop, GA finished");
    Phase aphase = (Phase) getParent();
    if (bestWSS < defWSS) {
      aphase.setStructureParams(bestParams);
      System.out.println("Final chi :" + getFitness(bestParams));
    } else {
      aphase.setStructureParams(defParams);
      System.out.println("Final chi :" + getFitness(defParams));
    }
    structureFactorList = null;
    GeneticAlgorithm.cleanUp();
    defParams = null;
    bestParams = null;
  }

  public double getOldFitness(double[] params) {

    Phase aphase = (Phase) getParent();
    aphase.setStructureParams(params);
    
    double ws1 = 0.0;
    double ws2 = 0.0;
    for (int i = 0; i < structureFactorList.length; i++) {
      int reflectionNumber = structureFactorList[i].structureFactor.length;
      for (int j = 0; j < reflectionNumber; j++) {
        StructureFactor sf = structureFactorList[i].structureFactor[j];
        Radiation rad = structureFactorList[i].radiation.getRadiation(0);
        double Fhkl2 = 1.0; //aphase.Fhklcomp(	sf.h, sf.k, sf.l, sf.d_spacing,   todo, fix this
                                      //  rad.getRadiationIDNumber(), rad.tubeNumber);
        sf.Fhkl_calc = Math.sqrt(Fhkl2);
        if (sf.weight > 0) {
          ws1 += sf.Fhkl_calc;
          ws2 += sf.Fhkl_exp;
        }
      }
    }
    
//    System.out.println("Structure Factor normalization: " + Fmt.format(ws1/ws2));
    
    double wsratio = ws2 / ws1;
    double wss = 0.0;
    for (int i = 0; i < structureFactorList.length; i++) {
      int reflectionNumber = structureFactorList[i].structureFactor.length;
      for (int j = 0; j < reflectionNumber; j++) {
        StructureFactor sf = structureFactorList[i].structureFactor[j];
        sf.Fhkl_calc *= wsratio;
        if (sf.weight > 0)
          wss += Math.abs(sf.Fhkl_calc - sf.Fhkl_exp);// * sf.Fhkl_esd;
      }
    }
    
    return wss;
  }

  public double getFitness(double[] params) {

    setStructureParams(params);
    
    double ws1 = 0.0;
    double ws2 = 0.0;
    for (int i = 0; i < structureFactorList.length; i++) {
      int reflectionNumber = structureFactorList[i].structureFactor.length;
      for (int j = 0; j < reflectionNumber; j++) {
        StructureFactor sf = structureFactorList[i].structureFactor[j];
        double Fhkl2 = Fhklcomp(sf.h, sf.k, sf.l, i, j);
//        System.out.println("f " + Fhkl2);
        sf.Fhkl_calc = Math.sqrt(Fhkl2);
        if (sf.weight > 0) {
          ws1 += sf.Fhkl_calc;
          ws2 += sf.Fhkl_exp;
        }
      }
    }
    
//    System.out.println("Structure Factor normalization: " + Fmt.format(ws1/ws2));
    
    double wsratio = ws2 / ws1;
    double wss = 0.0;
    for (int i = 0; i < structureFactorList.length; i++) {
      int reflectionNumber = structureFactorList[i].structureFactor.length;
      for (int j = 0; j < reflectionNumber; j++) {
        StructureFactor sf = structureFactorList[i].structureFactor[j];
        sf.Fhkl_calc *= wsratio;
        if (sf.weight > 0)
          wss += Math.abs(sf.Fhkl_calc - sf.Fhkl_exp);// * sf.Fhkl_esd;
      }
    }
    
    if (wss < bestWSS) {
      bestWSS = wss;
      for (int i = 0; i < bestParams.length; i++)
        bestParams[i] = params[i];
      Phase aphase = (Phase) getParent();
      aphase.setStructureParams(bestParams);
    }
    return wss;
  }

  double[] xf = new double[3];
  
  public void setStructureParams(double[] params) {
    int np = 0;
  	for (int n = 0; n < nAtom; n++) {
      
      xf[0] = params[np++];
      xf[1] = params[np++];
      xf[2] = params[np++];
		
      for (int i = 0; i < sitenumber; i++) {
        for (int j = 0; j < 3; j++)
          x[n][i][j] = sitepos[i].getcoordNoCheck(j, xf);
      }
    }
  }

  public static final double pi2 = 2.0 * Constants.PI;
  double scat1, scat2;
	
	public double Fhklcomp(int h, int k, int l, int index1, int index2) {
    double a1 = 0.0;
  	double a2 = 0.0;
  	for (int j = 0; j < nAtom; j++) {
      scat1 = scatf[0][j][index1][index2];
      scat2 = scatf[1][j][index1][index2];
//      System.out.println(scat1 + " " + scat2);
   	 	for (int ix = 0; ix < sitenumber; ix++) {
      	double arg = pi2 * (h * x[j][ix][0] + k * x[j][ix][1] + l * x[j][ix][2]);
//      System.out.println(arg);
      	double w1 = Math.cos(arg);
      	double w2 = Math.sin(arg);
      	a1 += scat1 * w1 - scat2 * w2;
      	a2 += scat1 * w2 + scat2 * w1;
    	}
  	}
		double structurefactor = (a1 * a1 + a2 * a2);
		return structurefactor;
 	}
  
	public JOptionsDialog getOptionsDialog(Frame parent) {
		JOptionsDialog adialog = new JGASDPDOptionsD(parent, this);
		return adialog;
	}
	
	public class JGASDPDOptionsD extends JOptionsDialog
	{

    JTextField[] parsTF = null;
    
		public JGASDPDOptionsD(Frame parent, XRDcat obj) {

	  	super(parent, obj);
	  
			principalPanel.setLayout(new BorderLayout(6, 6));

			JPanel tfPanel = new JPanel();
      tfPanel.setLayout(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.CENTER, tfPanel);
      
      String[] labels = {	
                          "Population size:         ",
                          "Number of generations:   ", 
                          "Mutation probability:    ",
                          "Permutation probability: "};
      
      int numberFields = labels.length;
      parsTF = new JTextField[numberFields];
      
      for (int i = 0; i < numberFields; i++) {
        tfPanel.add(new JLabel(labels[i]));
        parsTF[i] = new JTextField(Constants.FLOAT_FIELD);
//        genTF.setToolTipText("Set the number of generations for the Genetic Algorithm");
        tfPanel.add(parsTF[i]);
      }

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
      for (int i = 0; i < parsTF.length; i++)
      	parsTF[i].setText(stringField[i]);
		}

		public void retrieveParameters() {
      for (int i = 0; i < parsTF.length; i++)
      	stringField[i] = parsTF[i].getText();
		}

	}
}

