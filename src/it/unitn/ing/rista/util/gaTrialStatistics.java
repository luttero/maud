/*
 * @(#)gaTrialStatistics.java created Mar 28, 2003 Mesiano
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

import ec.Statistics;
import ec.EvolutionState;
import ec.util.Parameter;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

//import it.unitn.ing.rista.diffr.sdpd.StructureSolutionGANew;
import it.unitn.ing.rista.awt.MaudProgressBar;

/**
 *  The gaTrialStatistics is a
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:10 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class gaTrialStatistics { /*luca} extends Statistics {

	gaTrialStatisticsD statDialog;
	static StructureSolutionGANew gaMethod;

	public void postInitializationStatistics(final EvolutionState state) {

		super.postInitializationStatistics(state);

		gaStructureProblem the_problem = (gaStructureProblem) state.parameters.getInstanceForParameter(
			new Parameter("eval.problem"), null, ec.Problem.class);
		gaMethod = the_problem.getGaMethod();
		statDialog = new gaTrialStatisticsD();
		statDialog.show();
	}

	public void postEvaluationStatistics(final EvolutionState state) {
		super.postEvaluationStatistics(state);
		statDialog.updateStatistics(state);
		statDialog.repaint();
	}

	class gaTrialStatisticsD extends JDialog {

		protected double timeStart = 0;
		protected double timeCurrent = 0;
		protected JTextField textMAXEnergy;
		protected JTextField textMAXWSS;
		protected JTextField textTElapsed;
		protected JTextField textTRemaining;
		protected JTextField textGenerationNumber;
		protected JTextField textGenerationCurrent;
		protected JTextField textIndividualNumber;
		protected JTextField textIndividualCurrent;
		protected MaudProgressBar progressRun;

		private JPanel panelStatistics;
		private JPanel panelProgress;
		private JPanel panelBottom;
		private JButton btnCancel;

		public gaTrialStatisticsD() {
			timeStart = System.currentTimeMillis();
			initComponents();
		}

		private void initComponents() {
			GridBagConstraints gridBagConstraints;

			panelStatistics = new JPanel();
			panelProgress = new JPanel();
			panelBottom = new JPanel();
			btnCancel = new JButton();

			textMAXWSS = new JTextField("0");
			textMAXEnergy = new JTextField("0");
			textTElapsed = new JTextField("0");
			textTRemaining = new JTextField("0");
			textGenerationNumber = new JTextField("0");
			textGenerationCurrent = new JTextField("0");
			textIndividualNumber = new JTextField("0");
			textIndividualCurrent = new JTextField("0");
			progressRun = new MaudProgressBar();

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

		public void updateStatistics(final EvolutionState state) {
			textMAXWSS.setText(Double.toString(gaMethod.getMaxWss()));
			textMAXEnergy.setText(Double.toString(gaMethod.getMaxEnergy()));
		}
	}*/
}
