/*
 * @(#)StructureParamEditD.java created Mar 10, 2003 Mesiano
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

package it.unitn.ing.rista.awt;

import it.unitn.ing.rista.diffr.structure.StructureAtomic;
import it.unitn.ing.rista.diffr.Parameter;
import it.unitn.ing.rista.diffr.AtomSite;
import it.unitn.ing.rista.diffr.Fragment;

import javax.swing.*;
import javax.swing.event.TreeSelectionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeSelectionModel;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.Vector;

/**
 *  The StructureParamEditD is a
 *
 *
 * @version $Revision: 1.8 $, $Date: 2004/08/12 09:36:03 $
 * @author Mauro Bortolotti
 * @since JDK1.1
 */

public class StructureParamEditD extends JOptionsDialog {

	private StructureAtomic m_Struct;

	private JPanel panelMain;

	private JPanel panelParamTree;
	private JScrollPane scrollParams;
	private JTree treeParams;
	private DefaultMutableTreeNode structNode;

	private JPanel panelParamEdit;
	private JPanel panelFree;
	private ButtonGroup groupFree;
	private JRadioButton radioFixed;
	private JRadioButton radioFree;

	private JTextField textValue;
	private JCheckBox checkboxRange;

	private ButtonGroup groupMode;
	private JRadioButton radioMinMax;
	private JRadioButton radioDelta;
	private JPanel panelMinMax;
	private JTextField textMax;
	private JTextField textMin;
	private JPanel panelDelta;
	private JTextField textDelta;

	private JButton btnApply;


	public StructureParamEditD(Frame parentFrame, StructureAtomic struct) {
		super(parentFrame, struct, "Edit structure parameters");
		m_Struct = struct;
		createNodes();
		initComponents();
	}

	private void initComponents() {

		java.awt.GridBagConstraints gridBagConstraints;

		panelMain = new JPanel();
		panelParamTree = new JPanel();
		scrollParams = new JScrollPane();
		treeParams = new JTree(structNode);
		panelParamEdit = new JPanel();
		panelFree = new JPanel();
		groupFree = new ButtonGroup();
		radioFixed = new JRadioButton();
		radioFree = new JRadioButton();
		groupMode = new ButtonGroup();
		radioMinMax = new JRadioButton();
		radioDelta = new JRadioButton();
		textValue = new JTextField();
		checkboxRange = new JCheckBox();
		panelMinMax = new JPanel();
		textMin = new JTextField();
		textMax = new JTextField();
		panelDelta = new JPanel();
		textDelta = new JTextField();
		btnApply = new JButton();

		panelMain.setLayout(new java.awt.GridBagLayout());

		panelParamTree.setBorder(new javax.swing.border.EtchedBorder());
		scrollParams.setPreferredSize(new java.awt.Dimension(250, 300));
		scrollParams.setViewportView(treeParams);
		treeParams.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
		treeParams.addTreeSelectionListener(new TreeSelectionListener() {
			public void valueChanged(TreeSelectionEvent e) {
				treeSelectedParameter();
			}
		});

		panelParamTree.add(scrollParams);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.insets = new java.awt.Insets(5, 5, 5, 2);
		panelMain.add(panelParamTree, gridBagConstraints);

		panelParamEdit.setLayout(new java.awt.GridBagLayout());

		panelParamEdit.setBorder(new javax.swing.border.EtchedBorder());
		panelParamEdit.setPreferredSize(new java.awt.Dimension(200, 250));
		panelParamEdit.setMinimumSize(null);
		panelParamEdit.setMaximumSize(null);
		panelFree.setLayout(new java.awt.GridBagLayout());

		panelFree.setBorder(new javax.swing.border.EtchedBorder());
		groupFree.add(radioFixed);
		groupFree.add(radioFree);
		radioFixed.setSelected(true);
		radioFixed.setText("Fixed");
		radioFixed.setPreferredSize(new java.awt.Dimension(64, 25));
		radioFixed.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				radioFixedActionPerformed();
			}
		});

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
		panelFree.add(radioFixed, gridBagConstraints);

		radioFree.setText("Free");
		radioFree.setPreferredSize(new java.awt.Dimension(64, 25));
		radioFree.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				radioFreeActionPerformed();
			}
		});

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
		panelFree.add(radioFree, gridBagConstraints);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.insets = new java.awt.Insets(10, 10, 15, 10);
		panelParamEdit.add(panelFree, gridBagConstraints);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.insets = new java.awt.Insets(2, 10, 2, 4);
		panelParamEdit.add(new JLabel("Value"), gridBagConstraints);

		textValue.setPreferredSize(new java.awt.Dimension(64, 20));
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
		gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 10);
		panelParamEdit.add(textValue, gridBagConstraints);

		///////////////////////////////////////

		checkboxRange.setText("Set Parameter Range");
		checkboxRange.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				checkboxRangeActionPerformed();
			}
		});

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.insets = new java.awt.Insets(5, 8, 5, 8);
		panelParamEdit.add(checkboxRange, gridBagConstraints);

		panelMinMax.setLayout(new java.awt.GridBagLayout());
		panelMinMax.setBorder(new javax.swing.border.EtchedBorder());

		groupMode.add(radioMinMax);
		groupMode.add(radioDelta);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridheight = 2;
		gridBagConstraints.insets = new java.awt.Insets(5, 5, 5, 5);
		panelMinMax.add(radioMinMax, gridBagConstraints);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.insets = new java.awt.Insets(5, 10, 2, 4);
		panelMinMax.add(new JLabel("Min      "), gridBagConstraints);

		textMin.setPreferredSize(new java.awt.Dimension(64, 20));
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
		gridBagConstraints.insets = new java.awt.Insets(5, 2, 2, 10);
		panelMinMax.add(textMin, gridBagConstraints);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.insets = new java.awt.Insets(2, 10, 5, 4);
		panelMinMax.add(new JLabel("Max      "), gridBagConstraints);

		textMax.setPreferredSize(new java.awt.Dimension(64, 20));
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
		gridBagConstraints.insets = new java.awt.Insets(2, 2, 5, 10);
		panelMinMax.add(textMax, gridBagConstraints);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.insets = new java.awt.Insets(5, 5, 5, 5);
		panelParamEdit.add(panelMinMax, gridBagConstraints);

		panelDelta.setLayout(new java.awt.GridBagLayout());
		panelDelta.setBorder(new javax.swing.border.EtchedBorder());

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.insets = new java.awt.Insets(5, 5, 5, 5);
		panelDelta.add(radioDelta, gridBagConstraints);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.insets = new java.awt.Insets(2, 10, 2, 4);
		panelDelta.add(new JLabel("Delta +/-"), gridBagConstraints);

		textDelta.setPreferredSize(new java.awt.Dimension(64, 20));
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
		gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 10);
		panelDelta.add(textDelta, gridBagConstraints);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.insets = new java.awt.Insets(5, 5, 5, 5);
		panelParamEdit.add(panelDelta, gridBagConstraints);

		////////////////////////////////////////////////////////////

		btnApply.setText("Apply");
		btnApply.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				btnApplyActionPerformed();
			}
		});

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
		gridBagConstraints.insets = new java.awt.Insets(15, 10, 10, 10);
		panelParamEdit.add(btnApply, gridBagConstraints);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
		gridBagConstraints.insets = new java.awt.Insets(5, 2, 5, 5);
		panelMain.add(panelParamEdit, gridBagConstraints);

		getContentPane().add(panelMain);
		setResizable(false);
		pack();

	}

	private void radioFixedActionPerformed() {
		enableEditFull(!radioFixed.isSelected());
	}

	private void radioFreeActionPerformed() {
		enableEditFull(radioFree.isSelected());
		//enableEditRange(checkboxRange.isSelected());
	}

	private void checkboxRangeActionPerformed() {
		enableEditRange(checkboxRange.isSelected());
	}

	private void btnApplyActionPerformed() {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) treeParams.getLastSelectedPathComponent();
		if (!(node.isLeaf()) || (node == null)) return;
		if (!(node.getUserObject() instanceof Parameter))
			return;
		Parameter m_Param = (Parameter) node.getUserObject();
		m_Param.setFree(radioFree.isSelected());
		m_Param.setValue(textValue.getText());
		m_Param.setRangeActive(checkboxRange.isSelected());
		if (checkboxRange.isSelected()) {
			m_Param.setValueMin(textMin.getText());
			m_Param.setValueMax(textMax.getText());
		}
	}

	private void treeSelectedParameter() {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) treeParams.getLastSelectedPathComponent();
		if (node == null)
			return;
		if (!(node.getUserObject() instanceof Parameter))
			return;
		Parameter param = (Parameter) node.getUserObject();
		radioFixed.setSelected(!param.getFree());
		radioFree.setSelected(param.getFree());
		checkboxRange.setSelected(param.isRangeActive());
		textValue.setText(param.getValue());
		textMin.setText(param.getValueMin());
		textMax.setText(param.getValueMax());
		enableEditFull(radioFree.isSelected());
		enableEditRange(radioFree.isSelected() && checkboxRange.isSelected());
	}

	private void createNodes() {

		DefaultMutableTreeNode atomTree;
		DefaultMutableTreeNode fragmentTree;
		structNode = new DefaultMutableTreeNode("Structure");
		atomTree = new DefaultMutableTreeNode("Atoms");
		fragmentTree = new DefaultMutableTreeNode("Fragments");
		int na = m_Struct.getAtomNumber();
		int nf = m_Struct.getFragmentNumber();

		if (na > 0) {
			structNode.add(atomTree);
			for (int i = 0; i < na; i++) {
				AtomSite a_tmp = m_Struct.getAtom(i);
				DefaultMutableTreeNode atomNode = new DefaultMutableTreeNode(a_tmp.getLabel());
				atomTree.add(atomNode);
				Vector atomParams = a_tmp.getParameterVector(true, false);
				for (int j = 0; j < atomParams.size(); j++)
					atomNode.add(new DefaultMutableTreeNode(atomParams.elementAt(j)));
			}
		}

		if (nf > 0) {
			structNode.add(fragmentTree);
			for (int i = 0; i < nf; i++) {
				Fragment f_tmp = m_Struct.getFragment(i);
				DefaultMutableTreeNode fragmentNode = new DefaultMutableTreeNode(f_tmp.getLabel());
				fragmentTree.add(fragmentNode);
				Vector fragmentParams = f_tmp.getParameterVector(true, false);
				for (int j = 0; j < fragmentParams.size(); j++)
					fragmentNode.add(new DefaultMutableTreeNode(fragmentParams.elementAt(j)));
			}
		}
	}

	private void enableEditFull(boolean mode) {
		checkboxRange.setEnabled(mode);
		enableEditRange(checkboxRange.isSelected());
	}

	private void enableEditRange(boolean mode) {
		radioMinMax.setEnabled(mode);
		radioDelta.setEnabled(mode);
		textMin.setEnabled(mode);
		textMax.setEnabled(mode);
		textDelta.setEnabled(mode);
	}


}
