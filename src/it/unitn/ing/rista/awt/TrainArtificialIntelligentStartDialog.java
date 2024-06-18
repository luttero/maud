/*
 * @(#)TrainArtificialIntelligentStartDialog.java created 21/5/2024 Casalino
 *
 * Copyright (c) 2024- Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.awt;

import it.unitn.ing.rista.util.Constants;
import org.jdesktop.swingx.JXTitledPanel;
import org.jdesktop.swingx.border.DropShadowBorder;

import javax.swing.*;
import javax.swing.border.BevelBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * The TrainArtificialIntelligentStartDialog is a dialog that permits to set up
 * the training of AI generating some simulated data.
 *
 * @version $Revision: 1.0 $, $Date: 2024/05/21 18:31:49 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class TrainArtificialIntelligentStartDialog extends myJFrame {

  JTextField numberTF;

  JTextField prefixTF;
	public TrainArtificialIntelligentStartDialog(DiffractionMainFrame parentFrame) {
		super(parentFrame, "Generating data for AI training");

//		createDefaultMenuBar();

//		setTitle("Refinement wizard");

		setHelpFilename("generateSimulatedDataForAI.txt");

//    FilePar parameterfile = (FilePar) getFileParent();

		String[] labelRB = {""};

		Container c1 = getContentPane();
		c1.setLayout(new BorderLayout());

		JPanel principalPanel = new JPanel();
		principalPanel.setLayout(new GridLayout(0, 2));
		principalPanel.setBorder(new BevelBorder(BevelBorder.LOWERED));
		c1.add(BorderLayout.NORTH, principalPanel);

    principalPanel.add(new JLabel("Number of simulations: "));
    numberTF = new JTextField(Constants.FLOAT_FIELD);
    numberTF.setText("1000");
    principalPanel.add(numberTF);

    principalPanel.add(new JLabel("File output prefix: "));
    prefixTF = new JTextField(Constants.FLOAT_FIELD);
    prefixTF.setText("sim_");
    principalPanel.add(prefixTF);



    JPanel p1 = new JPanel();
		p1.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
		c1.add(BorderLayout.CENTER, p1);
		JButton closeButton = new JIconButton("TrafficGreen.gif", "Go!");
		p1.add(closeButton);
		closeButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent event) {
        int numberOfSim = Integer.parseInt(numberTF.getText());
        String prefix = prefixTF.getText();
				parentFrame.generateDataForAI(numberOfSim, prefix);
				setVisible(false);
				dispose();
			}
		});
		getRootPane().setDefaultButton(closeButton);
		JButton cancelButton = new JButton("Cancel");
		p1.add(cancelButton);
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent event) {
				setVisible(false);
				dispose();
			}
		});
		setHelpButton(p1);

		initParameters();

		pack();

//    centerOnScreen();
	}



}
