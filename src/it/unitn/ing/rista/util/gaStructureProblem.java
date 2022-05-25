/*
 * @(#)gaStructureProblem.java created Mar 26, 2003 Mesiano
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

package it.unitn.ing.rista.util;

import ec.EvolutionState;
import ec.Individual;
import ec.Problem;
import ec.util.Parameter;
import ec.util.Output;
import ec.simple.SimpleFitness;
import ec.simple.SimpleProblemForm;
import ec.vector.DoubleVectorIndividual;
import it.unitn.ing.rista.diffr.sdpd.StructureSolutionGANew;
import it.unitn.ing.rista.awt.MaudProgressBar;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;

/**
 *  The gaStructureProblem is a
 *
 *
 * @version $Revision: 1.9 $, $Date: 2006/01/19 14:45:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class gaStructureProblem extends Problem implements SimpleProblemForm {


	private static StructureSolutionGANew gaMethod;
	private static gaTrialStatistics trialDialog = null;
	public static final int RUNTRIAL = 0;
	public static final int RUNFULL = 1;
	private static int runType = RUNFULL;
	private static int nIndividualCurrent = 0;
	private static int nIndividualTotal = 0;


        public static StructureSolutionGANew getGaMethod() {
		return gaMethod;
	}

	public void setGaMethod(StructureSolutionGANew gaMethod, int type) {
		gaStructureProblem.gaMethod = gaMethod;
		runType = type;
		if (runType == RUNTRIAL)
			(trialDialog = new gaTrialStatistics()).show();
	}

	public void setup(final EvolutionState state, final Parameter base)
	{
		super.setup(state,base);
		nIndividualTotal = state.parameters.getIntWithDefault(
			new Parameter("pop.subpop.0.size"), null, 200);
		nIndividualCurrent = 0;

	}


	public double getFitness(double[] genome) {

		gaMethod.updateStructure(genome);
		double Emax = gaMethod.getMaxEnergy();
		double Wmax = gaMethod.getMaxWss();
		//System.out.println("Emax " + Emax);
		//System.out.println("Wmax " + Wmax);
		double E = gaMethod.getFitnessE() / Emax;
		double W = gaMethod.getFitnessW() / Wmax;

		//System.out.println("gaMethod.getFitnessE() " + E);
		//System.out.println("gaMethod.getFitnessW() " + W);

		if (E > 1) E = 1;
		if (W > 1) W = 1;
		double A = gaMethod.getEnergyRatio();
		double F = (A * E + (1 - A) * W);
		//System.out.println("Fitness: " + "Wss: " + W + "E: " + E + "Fitness: " + F);
		return F;
	}

	public void evaluate(final EvolutionState state,
			final Individual ind,
			final int threadnum) {
		if (ind.evaluated) return;

		if (!(ind instanceof DoubleVectorIndividual))
			state.output.fatal("The individuals for this problem should be DoubleVectorIndividuals.");

		double[] genome = ((DoubleVectorIndividual) ind).genome;
		int len = genome.length;
		double value = 0;

		value = getFitness(genome);

		value = 1.0 / (1.0 + value);
		((SimpleFitness) (ind.fitness)).setFitness(state, (float) value, value == 1.0);

		ind.evaluated = true;
		nIndividualCurrent++;

		if (runType == RUNTRIAL)
			updateTrialStatistics(state);

	}

	private void updateTrialStatistics(EvolutionState state) {
		if (trialDialog == null)
			return;
		trialDialog.textMAXEnergy.setText(Fmt.format(gaMethod.getMaxEnergy()));
		trialDialog.textMAXWSS.setText(Fmt.format(gaMethod.getMaxWss()));
		trialDialog.progressRun.setValue(nIndividualCurrent);

		long timeElapsed = System.currentTimeMillis() - trialDialog.timeStart;
		long timeRemaining = (timeElapsed/nIndividualCurrent)*(nIndividualTotal-nIndividualCurrent);

		trialDialog.textTElapsed.setText(Float.toString(timeElapsed/1000F));
		trialDialog.textTRemaining.setText(Float.toString(timeRemaining/1000F));

	}

	public void describe(final Individual ind,
			final EvolutionState state,
			final int threadnum,
			final int log,
			final int verbosity) {

	}

	class gaTrialStatistics extends JDialog {

		protected long timeStart = 0;
		//protected long timeElapsed = 0;
		protected JTextField textMAXEnergy;
		protected JTextField textMAXWSS;
		protected JTextField textTElapsed;
		protected JTextField textTRemaining;
		protected MaudProgressBar progressRun;
		private JPanel panelStatistics;
		private JPanel panelProgress;
		private JPanel panelBottom;
		private JButton btnCancel;

		public gaTrialStatistics() {
			super();
			timeStart = System.currentTimeMillis();
			textMAXWSS = new JTextField("0");
			textMAXEnergy = new JTextField("0");
			textTElapsed = new JTextField("0");
			textTRemaining = new JTextField("0");
			progressRun = new MaudProgressBar();
			progressRun.setMinimum(0);
			progressRun.setMaximum(Integer.parseInt(gaMethod.getTrialPopulationSize()));
			initComponents();
		}

		private void initComponents() {
			GridBagConstraints gridBagConstraints;

			panelStatistics = new JPanel();
			panelProgress = new JPanel();
			panelBottom = new JPanel();
			btnCancel = new JButton();

			getContentPane().setLayout(new GridBagLayout());

			setTitle("Fitness Normalization");
			addWindowListener(new WindowAdapter() {
				public void windowClosing(WindowEvent evt) {
					closeDialog();
				}
			});

			panelStatistics.setLayout(new GridLayout(2, 4, 5, 5));

			panelStatistics.add(new JLabel("Max WSS"));
			textMAXWSS.setEditable(false);
			panelStatistics.add(textMAXWSS);
			panelStatistics.add(new JLabel("Time Elapsed"));
			textTElapsed.setEditable(false);
			panelStatistics.add(textTElapsed);
			panelStatistics.add(new JLabel("Max Energy"));
			textMAXEnergy.setEditable(false);
			panelStatistics.add(textMAXEnergy);
			panelStatistics.add(new JLabel("Time remaining"));
			textTRemaining.setEditable(false);
			panelStatistics.add(textTRemaining);

			gridBagConstraints = new GridBagConstraints();
			gridBagConstraints.gridwidth = GridBagConstraints.REMAINDER;
			gridBagConstraints.insets = new Insets(15, 5, 5, 5);
			getContentPane().add(panelStatistics, gridBagConstraints);

			panelProgress.setLayout(new BoxLayout(panelProgress, BoxLayout.X_AXIS));

			panelProgress.add(progressRun);

			gridBagConstraints = new GridBagConstraints();
			gridBagConstraints.gridwidth = GridBagConstraints.REMAINDER;
			gridBagConstraints.fill = GridBagConstraints.BOTH;
			gridBagConstraints.insets = new Insets(10, 10, 5, 10);
			getContentPane().add(panelProgress, gridBagConstraints);

			btnCancel.setText("Cancel");
			btnCancel.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent evt) {
					btnCancelActionPerformed();
				}
			});

			panelBottom.add(btnCancel);

			gridBagConstraints = new GridBagConstraints();
			gridBagConstraints.insets = new Insets(5, 5, 5, 5);
			getContentPane().add(panelBottom, gridBagConstraints);

			pack();
		}

		private void btnCancelActionPerformed() {
			// Add your handling code here:
		}

		private void closeDialog() {
			setVisible(false);
			dispose();
		}
	}
}


