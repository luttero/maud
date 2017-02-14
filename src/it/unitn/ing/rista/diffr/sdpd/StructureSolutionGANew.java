/*
 * @(#)StructureSolutionGANew.java created Feb 25, 2003 Mesiano
 *
 * Copyright (c) 2003 Luca Lutterotti All Rights Reserved.
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

import ec.EvolutionState;
import ec.util.Output;
import ec.util.Parameter;
import it.unitn.ing.rista.awt.AttentionD;
import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.awt.StructureParamEditD;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.structure.StructureAtomic;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.Vector;

/**
 *  The StructureSolutionGANew is a
 *
 * @version $Revision: 1.22 $, $Date: 2006/12/04 14:30:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class StructureSolutionGANew extends StructureSolutionMethod {


	protected StructureAtomic gaStructure = null;
	protected Phase gaPhase = null;
	protected Vector freeStructureParams;
	protected StructureFactorList[] structureFactorList = null;

	double startFitness = 0.0;
	double bestFitness = 0.0;
	double worstFitness = 0.0;

	//double minWss = 1.0E-10;
	//double maxWss = 1.0E10;
	//double minEnergy = 1.0E-10;
	//double maxEnergy = 1.0E10;

	public static String[] diclistc = {
		"_riet_ga_population_size",
		"_riet_ga_generations_number",
		"_riet_ga_mutation_prob",
		"_riet_ga_permutation_prob",
		"_riet_ga_chunk_size",
		"_riet_ga_energy_ratio",
		"_riet_ga_trialpop_size",
		"_riet_ga_energy_norm",
		"_riet_ga_wss_norm"
	};

  public static String[] diclistcrm = {
    "_riet_ga_population_size",
    "_riet_ga_generations_number",
    "_riet_ga_mutation_prob",
    "_riet_ga_permutation_prob",
    "_riet_ga_chunk_size",
    "_riet_ga_energy_ratio",
    "_riet_ga_trialpop_size",
    "_riet_ga_energy_norm",
    "_riet_ga_wss_norm"
  };

	public static String[] classlistc = {};
	public static String[] classlistcs = {};


	public StructureSolutionGANew(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initXRD();
		identifier = "Disabled Genetic Algorithm NEW";
		IDlabel = "Genetic Algorithm NEW";
		description = "select this to use a Genetic Algorithm";
	}

	public StructureSolutionGANew(XRDcat aobj) {
		this(aobj, "Genetic Algorithm NEW");
	}

	public StructureSolutionGANew() {
		identifier = "Disabled Genetic Algorithm NEW";
		IDlabel = "Genetic Algorithm NEW";
		description = "select this to use a Genetic Algorithm";
	}

	public void initConstant() {
		Nstring = 9;
		Nstringloop = 0;
		Nparameter = 0;
		Nparameterloop = 0;
		Nsubordinate = classlistcs.length;
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
		setChunkSize("1");
		setTrialPopulationSize("500");
		setEnergyRatio(0.5);
		setMaxWss(1.0E10);
		setMaxEnergy(1.0E10);
	}



	//private StructureProblem m_GAProblem;
	//public static final String P_EVALTHREADS = "evalthreads";

	public StructureAtomic getParentStructure() {
		//temporary hack
		Phase t_Phase = (Phase) getParent();
		return (StructureAtomic) t_Phase.getActiveStructureModel();
	}

	public Phase getParentPhase() {
		//temporary hack
		return (Phase) getParent();
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
		stringField[3] = value;
	}

	public String getPerMutationProbability() {
		return stringField[3];
	}

	private void setChunkSize(String size) {
		stringField[4] = size;
	}

	private String getChunkSize() {
		return stringField[4];
	}

	public void setEnergyRatio(double eRatio) {
		stringField[5] = Double.toString(eRatio);
	}

	public double getEnergyRatio() {
		return Double.valueOf(stringField[5]).doubleValue();
	}

	public void setTrialPopulationSize(String tSize) {
		stringField[6] = tSize;
	}

	public String getTrialPopulationSize() {
		return stringField[6];
	}

	public void setMaxWss(double wMax) {
		stringField[7] = Double.toString(wMax);
	}

	public double getMaxWss() {
		return Double.valueOf(stringField[7]).doubleValue();
	}

	public void setMaxEnergy(double eMax) {
		stringField[8] = Double.toString(eMax);
	}

	public double getMaxEnergy() {
		return Double.valueOf(stringField[8]).doubleValue();
	}

///////////////////////////////////////////////////////////

	public void runTrialPopulation() {

		initMethod();
		if (freeStructureParams.size() < 1) {
			(new AttentionD("No structure parameters selected!")).setVisible(true);
			return;
		}

		setMaxWss(0.001);
		setMaxEnergy(0.001);

		final EvolutionState state = gaEvolve.make();
		setECJParams(state, defaultGAParams);

		gaStructureProblem the_problem = (gaStructureProblem) state.parameters.getInstanceForParameter(
			new Parameter("eval.problem"), null, ec.Problem.class);

		the_problem.setGaMethod(this, the_problem.RUNTRIAL);

		state.parameters.set(new Parameter("pop.subpop.0.species.genome-size"), Integer.toString(freeStructureParams.size()));
		state.parameters.set(new Parameter("pop.subpop.0.species.chunk-size"), "1");
		state.parameters.set(new Parameter("generations"), "1");
		state.parameters.set(new Parameter("pop.subpop.0.size"), getTrialPopulationSize());
		state.parameters.set(new Parameter("pop.subpop.0.species.mutation-prob"), getMutationProbability());
		state.parameters.set(new Parameter("pop.subpop.0.species.permutation-prob"), getMutationProbability());
		state.parameters.set(new Parameter("pop.subpop.0.species.chunk-size"), getChunkSize());

		//state.parameters.set(new Parameter("stat"), "it.unitn.ing.rista.util.gaTrialStatistics");

		for (int np = 0; np < freeStructureParams.size(); np++) {
			it.unitn.ing.rista.diffr.Parameter p_tmp = (it.unitn.ing.rista.diffr.Parameter) freeStructureParams.get(np);
			state.parameters.set(new Parameter("pop.subpop.0.species.min-gene." + Integer.toString(np)), p_tmp.getValueMin());
			state.parameters.set(new Parameter("pop.subpop.0.species.max-gene." + Integer.toString(np)), p_tmp.getValueMax());
		}

		if (state != null) {
			try {
				state.run(EvolutionState.C_STARTED_FRESH);
			} catch (IOException e) {
				Output.initialError(
					"An IO Exception was generated upon" +
					"starting up, probably in setting up a log" +
					"\nHere it is:\n" + e);
			}
		}
	}

	public void startSolutionLoop() {

		initMethod();
		if (freeStructureParams.size() < 1) {
			(new AttentionD("No structure parameters selected!")).setVisible(true);
			return;
		}

		final EvolutionState state = gaEvolve.make();
		setECJParams(state, defaultGAParams);

		gaStructureProblem the_problem = (gaStructureProblem) state.parameters.getInstanceForParameter(
			new Parameter("eval.problem"), null, ec.Problem.class);

		the_problem.setGaMethod(this, the_problem.RUNFULL);

		state.parameters.set(new Parameter("pop.subpop.0.species.genome-size"), Integer.toString(freeStructureParams.size()));
		state.parameters.set(new Parameter("pop.subpop.0.species.chunk-size"), "1");
		state.parameters.set(new Parameter("generations"), getGenerationsNumber());
		state.parameters.set(new Parameter("pop.subpop.0.size"), getPopulationSize());
		state.parameters.set(new Parameter("pop.subpop.0.species.mutation-prob"), getMutationProbability());
		state.parameters.set(new Parameter("pop.subpop.0.species.permutation-prob"), getMutationProbability());
		state.parameters.set(new Parameter("pop.subpop.0.species.chunk-size"), getChunkSize());
		state.parameters.set(new Parameter("stat"), "it.unitn.ing.rista.util.GAStatistics");

		for (int np = 0; np < freeStructureParams.size(); np++) {
			it.unitn.ing.rista.diffr.Parameter p_tmp = (it.unitn.ing.rista.diffr.Parameter) freeStructureParams.get(np);
			state.parameters.set(new Parameter("pop.subpop.0.species.min-gene." + Integer.toString(np)), p_tmp.getValueMin());
			state.parameters.set(new Parameter("pop.subpop.0.species.max-gene." + Integer.toString(np)), p_tmp.getValueMax());
		}

		if (state != null) {
			try {
				state.run(EvolutionState.C_STARTED_FRESH);
			} catch (IOException e) {
				Output.initialError(
					"An IO Exception was generated upon" +
					"starting up, probably in setting up a log" +
					"\nHere it is:\n" + e);
			}
		}
	}

	public double getFitnessE() {
		double E = ((ForceField) gaStructure.getActiveSubordinateModel(gaStructure.forceFieldID)).computeEnergy();
		if (E > getMaxEnergy()) setMaxEnergy(E);
		return E;
	}

	public double getFitnessW_() {
		return 0.5;
	}

	public double getFitnessW() {

		double ws1 = 0.0;
		double ws2 = 0.0;
		for (int i = 0; i < structureFactorList.length; i++) {
			int reflectionNumber = structureFactorList[i].structureFactor.length;
			for (int j = 0; j < reflectionNumber; j++) {
				StructureFactor sf = structureFactorList[i].structureFactor[j];
        Radiation rad = structureFactorList[i].radiation.getRadiation(0);
// todo fix        double Fhkl2 = gaPhase.Fhklcomp(sf.h, sf.k, sf.l, sf.d_spacing,
//					rad.getRadiationIDNumber(), rad.tubeNumber);
//				sf.Fhkl_calc = Math.sqrt(Fhkl2);
        if (sf.weight > 0) {
          ws1 += sf.Fhkl_calc;
				  ws2 += sf.Fhkl_exp;
        }
      }
		}

//Structure Factor normalization

		double wsratio = ws2 / ws1;
		double WSS = 0.0;
		for (int i = 0; i < structureFactorList.length; i++) {
			int reflectionNumber = structureFactorList[i].structureFactor.length;
			for (int j = 0; j < reflectionNumber; j++) {
				StructureFactor sf = structureFactorList[i].structureFactor[j];
				sf.Fhkl_calc *= wsratio;
        if (sf.weight > 0)
				  WSS += Math.abs(sf.Fhkl_calc - sf.Fhkl_exp);// * sf.Fhkl_esd;
			}
		}
		if (WSS > getMaxWss()) setMaxWss(WSS);
		return WSS;
	}

	public void updateStructure(double[] genome) {
		for (int np = 0; np < freeStructureParams.size(); np++) {
			((it.unitn.ing.rista.diffr.Parameter) freeStructureParams.get(np)).setValue(genome[np]);
		}
		gaPhase.refreshOccupancyAndQuantity();
	}

	public void initMethod() {

		// refresh free parameters

		gaStructure = getParentStructure();
		gaPhase = getParentPhase();

		Vector paramList = gaStructure.getParameterVector(true, true);
		freeStructureParams = new Vector();
		for (int np = 0; np < paramList.size(); np++) {
			it.unitn.ing.rista.diffr.Parameter p_tmp = (it.unitn.ing.rista.diffr.Parameter) paramList.get(np);
//System.out.println("p_tmp " + p_tmp + " p_tmp.getFree() " + p_tmp.getFree());
			if (p_tmp.getFree())
				freeStructureParams.addElement(p_tmp);
		}
/*for (int np = 0; np < freeStructureParams.size(); np++) {
			System.out.println("freeStructureParams.get(np)" + freeStructureParams.get(np));
		} */

//


		gaPhase.refreshReflectionv = true;
		gaPhase.sghklcompute(false);
		gaPhase.cellVolumeComp();

		StructureFactorModel sfModel = (StructureFactorModel) gaPhase.getActiveSubordinateModel(gaPhase.structureFactorModelID);
		structureFactorList = sfModel.getStructureFactorList();

	}


	public JOptionsDialog getOptionsDialog(Frame parent) {
		JOptionsDialog adialog = new JGASDPDOptionsD(parent, this);
		return adialog;
	}


///////////////////////////////////////////////////////////


	public class JGASDPDOptionsD extends JOptionsDialog {

		JTextField[] parsTF = null;
		JSlider sliderPERatio;
		JTextField textPopSize = null;
		JTextField textEMax = null;
		JTextField textWMax = null;
		JComboBox ForceFieldCB = null;

		public JGASDPDOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			setResizable(false);
			principalPanel.setLayout(new BorderLayout(10, 10));

//Genetic Algorithm Options panel

			JPanel tfPanel = new JPanel();

			GridBagLayout gridbag = new GridBagLayout();
			GridBagConstraints c = new GridBagConstraints();
			c.fill = GridBagConstraints.HORIZONTAL;
			c.insets = new Insets(5, 5, 5, 5);
			c.weightx = 1.0;
			c.weighty = 1.0;

			tfPanel.setLayout(gridbag);

			String[] labels = {
				"Population size:         ",
				"Number of generations:   ",
				"Mutation probability:    ",
				"Permutation probability: ",
				"Chunk size: "
			};

			int numberFields = labels.length;
			parsTF = new JTextField[numberFields];

			for (int i = 0; i < numberFields; i++) {
				JLabel tfLabel = new JLabel(labels[i]);
				c.gridwidth = 1;
				c.gridx = GridBagConstraints.RELATIVE;
				gridbag.setConstraints(tfLabel, c);
				tfPanel.add(tfLabel);
				parsTF[i] = new JTextField(Constants.FLOAT_FIELD);
				c.gridwidth = GridBagConstraints.REMAINDER;
				gridbag.setConstraints(parsTF[i], c);
				tfPanel.add(parsTF[i]);
			}

			JButton editParamButton = new JButton("Select parameters");
			editParamButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					StructureParamsDialog();
				}
			});
			editParamButton.setToolTipText("Select parameters");
			gridbag.setConstraints(editParamButton, c);
			tfPanel.add(editParamButton);

			JButton runButton = new JButton("Run GA Structure Solution");
			runButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					runFull();
				}
			});
			runButton.setToolTipText("Start structure solution using genetic algorithm");
			gridbag.setConstraints(runButton, c);
			tfPanel.add(runButton);

			principalPanel.add(BorderLayout.WEST, tfPanel);

//Fitness Function Options panel

			JPanel fitOptPanel = new JPanel();

			gridbag = new GridBagLayout();
			c = new GridBagConstraints();
			c.fill = GridBagConstraints.HORIZONTAL;
			c.insets = new Insets(5, 5, 5, 5);
			c.weightx = 1.0;
			c.weighty = 1.0;

			fitOptPanel.setLayout(gridbag);


//Potential Energy Ratio

			JLabel pELabel = new JLabel("Potential Energy Ratio:  ");
			gridbag.setConstraints(pELabel, c);
			fitOptPanel.add(pELabel);

			sliderPERatio = new JSlider(JSlider.HORIZONTAL, 0, 100, 0);

			Hashtable labeltable = new Hashtable();
			labeltable.put(new Integer(0), new JLabel("0.0"));
			labeltable.put(new Integer(20), new JLabel("0.2"));
			labeltable.put(new Integer(40), new JLabel("0.4"));
			labeltable.put(new Integer(60), new JLabel("0.6"));
			labeltable.put(new Integer(80), new JLabel("0.8"));
			labeltable.put(new Integer(100), new JLabel("1.0"));
			sliderPERatio.setLabelTable(labeltable);
			sliderPERatio.setMajorTickSpacing(10);
			sliderPERatio.setPaintTicks(true);
			sliderPERatio.setPaintLabels(true);
			sliderPERatio.setToolTipText("Set the potential energy ratio");
			c.gridwidth = GridBagConstraints.REMAINDER;
			gridbag.setConstraints(sliderPERatio, c);
			fitOptPanel.add(sliderPERatio);

//Force Field Selection

			c.gridwidth = 1;
			c.gridx = GridBagConstraints.RELATIVE;
			ForceFieldCB = new JComboBox();
			ForceFieldCB.setToolTipText("Select the force field model to be used in the fitnes function");

			gridbag.setConstraints(ForceFieldCB, c);
			fitOptPanel.add(ForceFieldCB);
			JButton fFieldButton = new JButton("force field options");
			fFieldButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					retrieveParameters();
					ForceFieldDialog();
				}
			});
			c.gridwidth = GridBagConstraints.REMAINDER;
			gridbag.setConstraints(fFieldButton, c);
			fitOptPanel.add(fFieldButton);

//Fitness normalization

			c.gridwidth = 1;
			c.gridx = GridBagConstraints.RELATIVE;
			JLabel mWLabel = new JLabel("MAX Wss:");
			gridbag.setConstraints(mWLabel, c);
			fitOptPanel.add(mWLabel);

			textWMax = new JTextField(stringField[7]);
			textWMax.setToolTipText("Set the Wss normalization factor");
			c.gridwidth = GridBagConstraints.REMAINDER;
			gridbag.setConstraints(textWMax, c);
			fitOptPanel.add(textWMax);

			c.gridwidth = 1;
			c.gridx = GridBagConstraints.RELATIVE;
			JLabel mELabel = new JLabel("MAX Energy:");
			gridbag.setConstraints(mELabel, c);
			fitOptPanel.add(mELabel);

			textEMax = new JTextField(stringField[8]);
			textEMax.setToolTipText("Set the energy normalization factor");
			c.gridwidth = GridBagConstraints.REMAINDER;
			gridbag.setConstraints(textEMax, c);
			fitOptPanel.add(textEMax);

//Trial Population

			c.gridwidth = 1;
			c.gridx = GridBagConstraints.RELATIVE;
			JButton tPopButton = new JButton("Run Trial Population");
			tPopButton.setToolTipText("Run trial population to set the fitness scale");
			tPopButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					runTrial();
				}
			});

			gridbag.setConstraints(tPopButton, c);
			fitOptPanel.add(tPopButton);

			JLabel tPopLabel = new JLabel("Size:  ");
			gridbag.setConstraints(tPopLabel, c);
			fitOptPanel.add(tPopLabel);

			textPopSize = new JTextField(stringField[6]);
			textPopSize.setToolTipText("Set the trial population size");
			c.gridwidth = GridBagConstraints.REMAINDER;
			gridbag.setConstraints(textPopSize, c);
			fitOptPanel.add(textPopSize);


			principalPanel.add(BorderLayout.EAST, fitOptPanel);


			setTitle("GA Structure Solution options panel");
			initParameters();

			pack();
		}

		public void runTrial() {
			retrieveParameters();
			(new PersistentThread() {
        public void executeJob() {
					runTrialPopulation();
					textWMax.setText(stringField[7]);
					textEMax.setText(stringField[8]);
				}
			}).start();
		}

		public void runFull() {
			retrieveParameters();
			(new PersistentThread() {
        public void executeJob() {
					startSolutionLoop();
				}
			}).start();
		}

		public void initParameters() {
			for (int i = 0; i < parsTF.length; i++)
				parsTF[i].setText(stringField[i]);
			sliderPERatio.setValue((int) (sliderPERatio.getMaximum() * getEnergyRatio()));
			System.out.println("sliderPERatio.getMaximum()*getEnergyRatio() " + (sliderPERatio.getMaximum() * getEnergyRatio()));
			textPopSize.setText(stringField[6]);
			textWMax.setText(stringField[7]);
			textEMax.setText(stringField[8]);

			StructureAtomic m_Struct = getParentStructure();
			for (int i = 0; i < m_Struct.getsubordClassNumber(m_Struct.forceFieldID); i++)
				ForceFieldCB.addItem(m_Struct.getsubordIdentifier(m_Struct.forceFieldID, i));
			ForceFieldCB.setSelectedItem(m_Struct.getActiveSubordinateModel(m_Struct.forceFieldID).identifier);
		}

		public void retrieveParameters
			() {
			for (int i = 0; i < parsTF.length; i++)
				stringField[i] = parsTF[i].getText();
			setEnergyRatio(sliderPERatio.getValue() / sliderPERatio.getMaximum());
			stringField[6] = textPopSize.getText();
			stringField[7] = textWMax.getText();
			stringField[8] = textEMax.getText();

			StructureAtomic m_Struct = getParentStructure();
			m_Struct.setSubordinateModel(m_Struct.forceFieldID, ForceFieldCB.getSelectedItem().toString());
		}

		public void ForceFieldDialog
			() {
			StructureAtomic m_Struct = getParentStructure();
			m_Struct.getActiveSubordinateModel(m_Struct.forceFieldID).getOptionsDialog(this).setVisible(true);
		}

		public void StructureParamsDialog
			() {
			new StructureParamEditD(this, getParentStructure()).show();
		}
	}

	public void setECJParams(EvolutionState state, String[] stringParams) {
		for (int i = 0; i < stringParams.length; i++) {
			StringTokenizer strtok = new StringTokenizer(stringParams[i]);
			String str_param = strtok.nextToken("=");
			String str_value = strtok.nextToken();
			//System.out.println(str_param + "=" + str_value);
			state.parameters.set(new Parameter(str_param), str_value);
		}
	}

	private static String[] defaultGAParams = {

// ec.EvolutionState
//==============================

// We're not garbage collecting.  If we were, we'd do it every generation,
// and we'd be aggressive, that is, we'd garbage collect until we knew
// there's nothing left to GC
		"gc=false",
		"gc-modulo=1",
		"aggressive=true",

// We're not writing checkpoint files.  If we were, we'd do it every
// generation, and the prefix to all the files would be "ec.*"
		"checkpoint=false",
		"checkpoint-modulo=1",
		"prefix=ec",

		"eval.problem=it.unitn.ing.rista.util.gaStructureProblem",
		//"eval.problem.type=GAproblem",


		"init=ec.simple.SimpleInitializer",
		"finish=ec.simple.SimpleFinisher",
		"exch=ec.simple.SimpleExchanger",
		"breed=ec.simple.SimpleBreeder",
		"eval=ec.simple.SimpleEvaluator",
		"stat=ec.simple.SimpleStatistics",
//		"stat=it.unitn.ing.rista.util.GAStatistics",
		"stat.file= $out.stat",
		"generations=100",
		"quit-on-run-complete=true",

		"max-fitness=2.0",
		"pop=ec.Population",

		"pop.subpops=1",
		"pop.subpop.0=ec.Subpopulation",
		"pop.subpop.0.size=100",
		"pop.subpop.0.duplicate-retries=2", //0
		"pop.subpop.0.fitness=ec.simple.SimpleFitness",
		"pop.subpop.0.species=ec.vector.FloatVectorSpecies",
		"pop.subpop.0.species.ind=ec.vector.DoubleVectorIndividual",

		"pop.subpop.0.species.genome-size=0",
		"pop.subpop.0.species.chunk-size=0",
		"pop.subpop.0.species.crossover-type=one",
		"pop.subpop.0.species.crossover-prob=1.0",
		"pop.subpop.0.species.mutation-prob=0.05",
		"pop.subpop.0.species.min-gene=0.0",
		"pop.subpop.0.species.max-gene=1.0",

		"pop.subpop.0.species.pipe=ec.vector.breed.VectorMutationPipeline",
		"pop.subpop.0.species.pipe.source.0=ec.vector.breed.VectorCrossoverPipeline",
		"pop.subpop.0.species.pipe.source.0.source.0=ec.select.TournamentSelection",
		"pop.subpop.0.species.pipe.source.0.source.1=ec.select.TournamentSelection",
		"select.tournament.size=2",

	};


}
