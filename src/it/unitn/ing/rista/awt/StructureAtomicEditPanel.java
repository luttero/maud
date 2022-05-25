/*
 * @(#)StructureAtomicEditPanel.java created Feb 26, 2003 Mesiano
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

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.structure.StructureAtomic;
import it.unitn.ing.rista.interfaces.i3DCanvas;
import it.unitn.ing.rista.interfaces.AtomsStructureI;
import it.unitn.ing.rista.render3d.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.io.MolecularImporter;

import javax.swing.*;
import javax.swing.tree.*;
import java.awt.*;
import java.awt.event.*;
import java.util.Vector;

/**
 *  The StructureAtomicEditPanel is a
 *
 *
 * @version $Revision: 1.35 $, $Date: 2006/01/19 14:45:53 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class StructureAtomicEditPanel extends iJPanel {

  StructureAtomic m_Struct;
  myJFrame parentD;

  /** The view panel - contains a GLCanvas and the view/edit buttons */

  /** The tabbed structure panel contains the atoms, bonds and fragment editing panels */

  AtomPanel m_atomPanel;
  BondPanel m_bondPanel;
  FragPanel m_fragPanel;
  StructureViewPanel m_viewPanel;
  JToggleButton toggleviewB;
  JComboBox forceFieldCB;
  JTextField energyWeightTF;

  public StructureAtomicEditPanel(myJFrame parent, StructureAtomic tStruct) {
    m_Struct = tStruct;
    parentD = parent;
    initComponents();
  }

  public void initComponents() {

    //JButton btnEdit = null, btnZoomIn = null, btnZoomOut = null;

    //setLayout(new BorderLayout());

    setLayout(new BorderLayout(6, 6));

    m_viewPanel = new StructureViewPanel();
    m_viewPanel.setVisible(false);
//    add(m_viewPanel, BorderLayout.CENTER);
    JPanel bpanel = new JPanel(new BorderLayout());
    JPanel smallP = new JPanel(new FlowLayout());
    toggleviewB = new JToggleButton(new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + "FingerRight.gif")));
    bpanel.add(BorderLayout.CENTER, smallP);
    smallP.add(toggleviewB);
    toggleviewB.setToolTipText("View/Hide the atomic structure panel");
    toggleviewB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        toggleStructureView();
      }
    });
    add(bpanel, BorderLayout.EAST);

    JPanel westPanel = new JPanel(new BorderLayout(6, 6));
    JTabbedPane tabStructure = new JTabbedPane();
    add(westPanel, BorderLayout.WEST);
    westPanel.add(BorderLayout.CENTER, tabStructure);
    tabStructure.addTab("Atoms", m_atomPanel = new AtomPanel(parentD, m_Struct));
/*    m_atomPanel.addFocusListener(new FocusAdapter() {
      public void focusGained(FocusEvent evt) {
        m_atomPanel.update();
        System.out.println("m_atomPanel.update()");
      }
      public void focusLost(FocusEvent evt) {
        m_atomPanel.update();
        System.out.println("m_atomPanel.update()");
      }
    });  */

    if (Constants.testing) {
      tabStructure.addTab("Bonds", m_bondPanel = new BondPanel());
/*    m_bondPanel.addFocusListener(new FocusAdapter() {
      public void focusGained(FocusEvent evt) {
        m_bondPanel.update();
        System.out.println("m_bondPanel.update()");
      }
    }); */

    }
    tabStructure.addTab("Fragments", m_fragPanel = new FragPanel());
/*    m_fragPanel.addFocusListener(new FocusAdapter() {
      public void focusGained(FocusEvent evt) {
        m_fragPanel.update();
        System.out.println("m_fragPanel.update()");
      }
    });   */

    JPanel energyP = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
    westPanel.add(BorderLayout.SOUTH, energyP);
    energyP.add(new JLabel("Energy computation:"));
    forceFieldCB = new JComboBox();
    forceFieldCB.setToolTipText("Select the energy computation model");
    energyP.add(forceFieldCB);
    JButton jb = new JIconButton("Eyeball.gif", "Options");
    jb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        forceFieldOptions();
      }
    });
    energyP.add(jb);

    for (int i = 0; i < m_Struct.getsubordClassNumber(m_Struct.forceFieldID); i++) {
      forceFieldCB.addItem(m_Struct.getsubordIdentifier(m_Struct.forceFieldID, i));
    }
    forceFieldCB.setSelectedItem(m_Struct.getSubordinateModel(m_Struct.forceFieldID));

    energyP.add(new JLabel("Energy weight: "));
    energyWeightTF = new JTextField(m_Struct.getEnergyWeightS());
    energyP.add(energyWeightTF);
  }

  public void retrieveParameters() {
    String selectedForceField = forceFieldCB.getSelectedItem().toString();
    if (!m_Struct.getSubordinateModel(m_Struct.forceFieldID).equals(selectedForceField))
      m_Struct.setSubordinateModel(m_Struct.forceFieldID, forceFieldCB.getSelectedItem().toString());
    m_atomPanel.retrieveParameters();
    if (Constants.testing)
      m_bondPanel.retrieveParameters();
    m_Struct.setEnergyWeight(energyWeightTF.getText());
  }

  public void dispose() {
    m_atomPanel.dispose();
    if (Constants.testing)
      m_bondPanel.dispose();
    if (frameView != null) {
      frameView.setVisible(false);
      frameView.dispose();
      frameView = null;
    }
  }

  public void forceFieldOptions() {
    String selectedForceField = forceFieldCB.getSelectedItem().toString();
    if (!m_Struct.getSubordinateModel(m_Struct.forceFieldID).equals(selectedForceField))
      m_Struct.setSubordinateModel(m_Struct.forceFieldID, forceFieldCB.getSelectedItem().toString());
    (m_Struct.getForceFieldModel().getOptionsDialog(getFrameParent())).setVisible(true);
  }

  public Frame getFrameParent() {
    Container cont = getParent();
    while (cont != null && !(cont instanceof Frame))
      cont = cont.getParent();
    return (Frame) cont;
  }

  public void setatomsite() {
    m_atomPanel.setatomsite();
  }

  public void setatomsite(int number) {
    m_atomPanel.setatomsite(number);
  }

  JFrame frameView = null;
  boolean separatedView = MaudPreferences.getBoolean("structureView.separatedWindow", false);
  boolean alreadyAdded = false;

  public void toggleStructureView() {
    if (m_viewPanel.isVisible()) {
      toggleviewB.setIcon(new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + "FingerRight.gif")));
      if (separatedView) {
        if (frameView != null) {
//          m_viewPanel.setVisible(false);
          frameView.setVisible(false);
//          frameView.dispose();
//          frameView = null;
        }
      } else {
        m_viewPanel.setVisible(false);
        //      remove(m_viewPanel);
        parentD.pack();
      }
    } else {
      toggleviewB.setIcon(new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + "FingerLeft.gif")));
      if (separatedView) {
        if (frameView == null) {
          frameView = new JFrame();
          frameView.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
          frameView.setVisible(false);
          m_viewPanel.setVisible(true);
          frameView.getContentPane().add(m_viewPanel);
          frameView.pack(); //setSize(400, 400);
          frameView.setVisible(true);
          m_viewPanel.canvasStruct3d.start();
        }
        frameView.setVisible(true);
      } else {
        //      parentD.setVisible(false);
        m_viewPanel.setVisible(true);
        if (!alreadyAdded) {
          add(m_viewPanel, BorderLayout.CENTER);
          alreadyAdded = true;
        }
        parentD.pack();
        //      parentD.setVisible(true);
        m_viewPanel.canvasStruct3d.start();
      }
//      m_viewPanel.updateStructure();

    }

  }

  public void importStructure(int formatIndex) {
    switch (formatIndex) {
      case MolecularImporter.XYZformat:
        String XYZ_filename = Utility.browseFilename(getFrameParent(), "Import XYZ molecule");
        if (XYZ_filename != null)
          MolecularImporter.importXYZmolecule(m_fragPanel.getSelectedFragment(), XYZ_filename);
        break;
        default: {}
    }
  }

  class StructureViewPanel extends JPanel {

    i3DCanvas canvasStruct3d;
    JButton btnDisplayCell;
    JButton btnDisplayStruct;
    JToggleButton btnEdit;
    JToggleButton btnRotate;
    JButton btnZoomIn;
    JButton btnZoomOut;
    JButton btnAtomColors;
    boolean notInitialized = true;
//    Animator animator = null;
    boolean animated = false;


    public StructureViewPanel() {
    }

    public void initComponents() {
      notInitialized = false;
      JPanel panel3DButtons = null;

      setLayout(new BorderLayout());

/*      if (Constants.OpenGL) {
        try {
          canvasStruct3d = new Structure3Dgl(m_Struct, 400, 400, true);
          GLProfile glp = GLProfile.getDefault();
          GLCanvas canvas = new GLCanvas(new GLCapabilities(glp));
          canvas.addGLEventListener((Structure3Dgl) canvasStruct3d);
          canvas.setSize(400, 400);
          add(canvas, BorderLayout.CENTER);
          ((AnimatedRendering3Dgl)canvasStruct3d).setCanvas(canvas);
        } catch (Throwable e) {
          e.printStackTrace();
          Constants.OpenGL = false;
          canvasStruct3d = new Structure3Djgl(m_Struct, 400, 400, true);
          add((Component) canvasStruct3d, BorderLayout.CENTER);
        }
      } else {*/
        canvasStruct3d = new Structure3Djgl(m_Struct, 400, 400, true);
        add((Component) canvasStruct3d, BorderLayout.CENTER);
//      }

//      canvasStruct3d.setSize(new Dimension(300, 300));

      add(panel3DButtons = new JPanel(), BorderLayout.SOUTH);

      panel3DButtons.add(btnDisplayCell = new JIconButton("CellCubic.gif"));
      btnDisplayCell.setToolTipText("Display Cell");
      btnDisplayCell.setSelected(true);
      btnDisplayCell.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          btnDisplayCell.setSelected(!btnDisplayCell.isSelected());
          if (canvasStruct3d instanceof Structure3Dgl)
            ((Structure3Dgl) canvasStruct3d).b_drawCell = btnDisplayCell.isSelected();
          else
            ((Structure3Djgl) canvasStruct3d).b_drawCell = btnDisplayCell.isSelected();
          canvasStruct3d.update();
        }
      });

      panel3DButtons.add(btnDisplayStruct = new JIconButton("AcquaMol.gif"));
      btnDisplayStruct.setToolTipText("Display structure");
      btnDisplayStruct.setSelected(true);
      btnDisplayStruct.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          btnDisplayStruct.setSelected(!btnDisplayStruct.isSelected());
          if (canvasStruct3d instanceof Structure3Dgl)
            ((Structure3Dgl) canvasStruct3d).b_drawAtoms = btnDisplayStruct.isSelected();
          else
            ((Structure3Djgl) canvasStruct3d).b_drawAtoms = btnDisplayStruct.isSelected();
          canvasStruct3d.update();
        }
      });

      panel3DButtons.add(btnEdit = new JToggleButton(
              new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + "TrafficGreen.gif"))));
      btnEdit.setToolTipText("Start/Stop rotation");
      btnEdit.setSelected(true);
      btnEdit.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          if (btnEdit.isSelected()) {
            startRotation();
            btnEdit.setIcon(new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + "TrafficGreen.gif")));
          } else {
            stopRotation();
            btnEdit.setIcon(new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + "TrafficRed.gif")));
          }
        }
      });

      panel3DButtons.add(btnZoomIn = new JButton(
              new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + "MagnifyPlus.gif"))));
      btnZoomIn.setToolTipText("Zoom in");
      btnZoomIn.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          btnZoomInActionPerformed(evt);
        }
      });

      panel3DButtons.add(btnZoomOut = new JButton(
              new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + "MagnifyMinus.gif"))));
      btnZoomOut.setToolTipText("Zoom out");
      btnZoomOut.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          btnZoomOutActionPerformed(evt);
        }
      });

      panel3DButtons.add(btnAtomColors = new JButton(
              new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + "Palette.gif"))));
      btnAtomColors.setToolTipText("Edit AtomSite Colors");
      btnAtomColors.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          btnAtomColorsActionPerformed(evt);
        }
      });

//      JMenuBar mainMenu = new JMenuBar();
//      add(mainMenu, BorderLayout.NORTH);

//		JMenu structMenu = new JMenu("Structure");       // In Maud c'e' gia' la possibilita' di importare/esportare
      // una struttura intera

//      JMenuItem aMenuItem;
/*		structMenu.add(aMenuItem = new JMenuItem("Import from file"));
		aMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				StructImportActionPerformed(evt);
			}
		});
		structMenu.add(aMenuItem = new JMenuItem("Save and exit"));
		aMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				StructSaveActionPerformed(evt);
			}
		});   */

/*      JMenu viewMenu = new JMenu("View");
      mainMenu.add(viewMenu);

      viewMenu.add(aMenuItem = new JMenuItem("Start rotation"));
      aMenuItem.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          menuViewStartActionPerformed(evt);
        }
      });
      viewMenu.add(aMenuItem = new JMenuItem("Stop rotation"));
      aMenuItem.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          menuViewStopActionPerformed(evt);
        }
      });
      viewMenu.add(aMenuItem = new JMenuItem("Rotate X axis"));
      aMenuItem.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          checkXRotateActionPerformed(evt);
        }
      });
      viewMenu.add(aMenuItem = new JMenuItem("Rotate Y axis"));
      aMenuItem.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          checkYRotateActionPerformed(evt);
        }
      });
      viewMenu.add(aMenuItem = new JMenuItem("Rotate Z axis"));
      aMenuItem.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          checkZRotateActionPerformed(evt);
        }
      });        */

    }

    public void setVisible(boolean visible) {
      if (visible && notInitialized) {
        initComponents();
        startRotation();
      } else {
        if (visible)
          startRotation();
        else
          stopRotation();
      }
      super.setVisible(visible);
    }

    public void updateStructure() {
      if (canvasStruct3d == null || !this.isVisible())
        return;
      canvasStruct3d.update();
    }

    void btnEditActionPerformed(ActionEvent evt) {
      // Add your handling code here:
    }

    void checkXRotateActionPerformed(ActionEvent evt) {
      // Add your handling code here:
    }

    void checkYRotateActionPerformed(ActionEvent evt) {
      // Add your handling code here:
    }

    void checkZRotateActionPerformed(ActionEvent evt) {
      // Add your handling code here:
    }

    void StructSaveActionPerformed(ActionEvent evt) {
      // Add your handling code here:
    }

    public void startRotation() {
//      if (canvasStruct3d instanceof i3DCanvas)
      if (animated)
        return;
      animated = true;
      canvasStruct3d.start();
//      if (animator != null)
//        animator.start();
      // Add your handling code here:
    }

    public void stopRotation() {
//      if (canvasStruct3d instanceof i3DCanvas)
      if (!animated)
        return;
      animated = false;
      canvasStruct3d.stop();
//      if (animator != null)
//        animator.stop();
      // Add your handling code here:
    }

    void StructImportActionPerformed(ActionEvent evt) {
      // Add your handling code here:
    }

    void btnZoomInActionPerformed(ActionEvent evt) {
      canvasStruct3d.zoomIn();
    }

    void btnZoomOutActionPerformed(ActionEvent evt) {
      canvasStruct3d.zoomOut();
    }

    void btnAtomColorsActionPerformed(ActionEvent evt) {
      AtomColorPreferences.showPrefs(parentD);
    }

  }


   /**
   * FragmentPanel - the Fragment List editing panel
   */

   class FragPanel extends JPanel {

    protected JScrollPane scrollFragments;
    protected DefaultMutableTreeNode structNode;
    protected DefaultTreeModel treeModel;
    protected JTree treeFragments;

    protected Fragment currentFragment = null;

    //JTextField textFragment;

    JButton btnNew;
    JButton btnRemove;
    JButton btnEdit;
    JButton btnDuplicate;

    JPanel fragmentEditPanel;


    public FragPanel() {
      initComponents();
      initListeners();
    }

    void initListeners() {

      /*treeFragments.addTreeSelectionListener(new TreeSelectionListener() {
        public void valueChanged(TreeSelectionEvent e) {
          DefaultMutableTreeNode node = (DefaultMutableTreeNode)
            treeFragments.getLastSelectedPathComponent();

          if (node == null) return;

          Object nodeInfo = node.getUserObject();
          if (node.isLeaf()) {
            Fragment f_tmp = (Fragment) nodeInfo;
            System.out.print(f_tmp.getLabel() + ":  \n    ");
          } else {

          }
        }
      });*/

      MouseListener ml = new MouseAdapter() {
        public void mousePressed(MouseEvent e) {
          if (e.getClickCount() == 2) {
            TreePath selPath = treeFragments.getPathForLocation(e.getX(), e.getY());
            if ((selPath == null) || !(((DefaultMutableTreeNode) selPath.getLastPathComponent()).getUserObject() instanceof Fragment))
              return;
            final Fragment f_tmp = (Fragment) ((DefaultMutableTreeNode) selPath.getLastPathComponent()).getUserObject();
            if (f_tmp == null)
              return;
            final LabelD labD = new LabelD(null, "Change fragment label", true);
            labD.setTextField(f_tmp.getLabel());
            labD.jbok.addActionListener(new ActionListener() {
              public void actionPerformed(ActionEvent e) {
                f_tmp.setLabel(labD.getTextField());
                labD.dispose();
              }
            });
          }
        }
      };
      treeFragments.addMouseListener(ml);


/*      treeModel.addTreeModelListener(new TreeModelListener() {
        public void treeNodesChanged(TreeModelEvent e) {
          DefaultMutableTreeNode node;
          node = (DefaultMutableTreeNode) (e.getTreePath().getLastPathComponent());
          int index = e.getChildIndices()[0];
          node = (DefaultMutableTreeNode) (node.getChildAt(index));
//          System.out.println("Object " + node);
        }

        public void treeNodesInserted(TreeModelEvent e) {
        }

        public void treeNodesRemoved(TreeModelEvent e) {
        }

        public void treeStructureChanged(TreeModelEvent e) {
        }
      });*/
    }

    void initComponents() {

      structNode = new DefaultMutableTreeNode(m_Struct);
      treeModel = new DefaultTreeModel(structNode);
      treeFragments = new JTree(treeModel);
      treeFragments.setEditable(true);
      treeFragments.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
      treeFragments.setShowsRootHandles(true);
      createNodes(structNode);
      scrollFragments = new JScrollPane(treeFragments);

      fragmentEditPanel = new JPanel();
      btnNew = new JButton();
      btnRemove = new JButton();
      //textFragment = new javax.swing.JTextField();
      btnEdit = new JButton();

      //setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
      setLayout(new FlowLayout(FlowLayout.CENTER, Constants.borderInside, Constants.borderInside));

      setPreferredSize(new java.awt.Dimension(280, 300));
      scrollFragments.setPreferredSize(new java.awt.Dimension(250, 200));

      add(scrollFragments);

      fragmentEditPanel.setLayout(new java.awt.GridLayout(0, 1, Constants.borderInside, Constants.borderInside));

      btnNew.setToolTipText("Add new fragment");
      btnNew.setText("Add new");
      btnNew.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
          btnNewActionPerformed();
        }
      });

      fragmentEditPanel.add(btnNew);

      btnRemove.setToolTipText("Remove selected fragment");
      btnRemove.setText("Remove");
      btnRemove.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
          btnRemoveActionPerformed();
        }
      });

      fragmentEditPanel.add(btnRemove);

      //fragmentEditPanel.add(new JLabel("label"));

      //fragmentEditPanel.add(textFragment);

      btnEdit.setToolTipText("Edit the selected fragment");
      btnEdit.setText("Edit fragment");
      btnEdit.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
          btnEditActionPerformed();
        }
      });

      fragmentEditPanel.add(btnEdit);

      btnDuplicate = new JButton("Duplicate");
      btnDuplicate.setToolTipText("Duplicate the selected fragment");
      btnDuplicate.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
          btnDuplicateActionPerformed();
        }
      });
      fragmentEditPanel.add(btnDuplicate);

      btnDuplicate = new JButton("Get selected atoms");
      btnDuplicate.setToolTipText("Move the selected atom from phase to the selected fragment");
      btnDuplicate.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
          btnMoveFromStructureActionPerformed();
        }
      });
      fragmentEditPanel.add(btnDuplicate);

      btnDuplicate = new JButton("Move atoms to parent");
      btnDuplicate.setToolTipText("Move the atoms of the selected fragment to its parent");
      btnDuplicate.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
          btnMoveToParentActionPerformed();
        }
      });
      fragmentEditPanel.add(btnDuplicate);

      add(fragmentEditPanel);

    }

    void createNodes(DefaultMutableTreeNode root) {

/*      if (Constants.testing) {
        System.out.println("m_Struct.getFragmentNumber() " + m_Struct.getFragmentNumber());
        System.out.println("m_Struct.getBondNumber() " + m_Struct.getBondNumber());
      }*/

      for (int nf = 0; nf < m_Struct.getFragmentNumber(); nf++) {
/*        if (Constants.testing) {
          System.out.println("m_Struct.getFragment(nf) " + m_Struct.getFragment(nf));
          System.out.println("m_Struct.getFragment(nf) " + m_Struct.getFragment(nf).getFragmentNumber());
        }*/
        Fragment f_tmp = m_Struct.getFragment(nf);
        DefaultMutableTreeNode n_tmp = new DefaultMutableTreeNode(f_tmp);
        treeModel.insertNodeInto(n_tmp, structNode, nf);
        populateNode(f_tmp, n_tmp);
      }
    }

    void populateNode(Fragment parent_frag, DefaultMutableTreeNode parent_node) {
      for (int nf = 0; nf < parent_frag.getFragmentNumber(); nf++) {
        Fragment child_frag = parent_frag.getFragment(nf);
        DefaultMutableTreeNode child_node = new DefaultMutableTreeNode(child_frag);
        treeModel.insertNodeInto(child_node, parent_node, nf);
        populateNode(child_frag, child_node);
      }
    }

    void updateStructure() {
      /*treeFragments.removeAll();
      structNode = new DefaultMutableTreeNode("Structure");
      treeFragments.
      System.out.println("m_Struct.getFragmentNumber() " + m_Struct.getFragmentNumber());
      System.out.println("m_Struct.getBondNumber() " + m_Struct.getBondNumber());

      for (int nf = 0; nf < m_Struct.getFragmentNumber(); nf++) {
        System.out.println("m_Struct.getFragment(nf) " + m_Struct.getFragment(nf));
        System.out.println("m_Struct.getFragment(nf) " + m_Struct.getFragment(nf).getFragmentNumber());
        populateNode(m_Struct.getFragment(nf), structNode);
      }*/

    }

    void retrieveFragment() {

    }

    void setFragment() {

    }

    void btnEditActionPerformed() {
      DefaultMutableTreeNode n_frag = (DefaultMutableTreeNode) treeFragments.getLastSelectedPathComponent();
      if ((n_frag == null) || !(n_frag.getUserObject() instanceof Fragment))
        return;
      final myJFrame atomPanelF = new myJFrame(null);
      atomPanelF.setTitle("Edit atom list for " + ((Fragment) n_frag.getUserObject()).getLabel());
      atomPanelF.getContentPane().setLayout(new BorderLayout(3, 3));
      final AtomPanel atomPanel = new AtomPanel(atomPanelF, (Fragment) n_frag.getUserObject());
      atomPanelF.getContentPane().add(atomPanel, BorderLayout.NORTH);

      JPanel centerPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 3, 3));
      final FragmentEditPanel fragmentCoordP = new FragmentEditPanel((Fragment) n_frag.getUserObject(), atomPanelF,
          atomPanel);
      atomPanelF.getContentPane().add(centerPanel, BorderLayout.CENTER);
      centerPanel.add(fragmentCoordP);

      final JSubordListPane bondsPane = new JSubordListPane(parentD, false);
      centerPanel.add(bondsPane);
      String labels[] = {"Bond length"};

      bondsPane.setList((Fragment) n_frag.getUserObject(), 2, labels.length, labels);

      JPanel buttonP = new JPanel(new FlowLayout(FlowLayout.RIGHT, 3, 3));
      atomPanelF.getContentPane().add(buttonP, BorderLayout.SOUTH);
      JButton closeButton = new JCloseButton();
      closeButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          atomPanel.retrieveParameters();
          fragmentCoordP.retrieveParameters();
          bondsPane.retrieveparlist();
          atomPanel.dispose();
          atomPanelF.setVisible(false);
          atomPanelF.dispose();
        }
      });
      buttonP.add(closeButton);
      atomPanelF.getRootPane().setDefaultButton(closeButton);

      atomPanelF.pack();
      atomPanelF.setVisible(true);
    }

    private void btnDuplicateActionPerformed() {
      DefaultMutableTreeNode n_frag = (DefaultMutableTreeNode) treeFragments.getLastSelectedPathComponent();
      Fragment fragment = (Fragment) n_frag.getUserObject();
      if ((n_frag == null) || !(fragment instanceof Fragment))
        return;

      XRDcat parent = fragment.getParent();
      Fragment newfrag = (Fragment) fragment.getCopy(parent);
      newfrag.setLabel(fragment.getLabel() + " copy");
      if (parent instanceof StructureAtomic)
        ((StructureAtomic) parent).addFragment(newfrag);  // fragment.getCopy(n_parent)
      if (parent instanceof Fragment)
        ((Fragment) parent).addFragment(newfrag);  // fragment.getCopy(n_parent)
      parent.notifyParentChanged();
      DefaultMutableTreeNode n_tmp = new DefaultMutableTreeNode(newfrag);
      DefaultMutableTreeNode n_parent = (DefaultMutableTreeNode) (n_frag.getParent());
      treeModel.insertNodeInto(n_tmp, n_parent, n_parent.getChildCount());
    }

    void btnRemoveActionPerformed() {
      TreePath cur_sel = treeFragments.getSelectionPath();
      if (cur_sel != null) {
        DefaultMutableTreeNode n_child = (DefaultMutableTreeNode) (cur_sel.getLastPathComponent());
        DefaultMutableTreeNode n_parent = (DefaultMutableTreeNode) (n_child.getParent());
        if (n_parent != null) {
          Vector fullAtomList = ((Fragment) n_child.getUserObject()).getFullAtomList();
          AtomsStructureI atomstructure = (AtomsStructureI) n_parent.getUserObject();
          for (int i = 0; i < fullAtomList.size(); i++) {
            AtomSite ato = (AtomSite) ((XRDcat) fullAtomList.elementAt(i)).getCopy((XRDcat) atomstructure);
            atomstructure.addAtom(ato);
          }
          atomstructure.removeFragmentAt(n_parent.getIndex(n_child));
        }
        treeModel.removeNodeFromParent(n_child);
      }
    }

     void btnMoveFromStructureActionPerformed() {
       TreePath cur_sel = treeFragments.getSelectionPath();
       if (cur_sel != null) {
         DefaultMutableTreeNode n_child = (DefaultMutableTreeNode) (cur_sel.getLastPathComponent());
         AtomSite[] atomlist = m_atomPanel.getSelectedAtoms();
         if (atomlist != null) {
           AtomsStructureI atomstructure = (AtomsStructureI) n_child.getUserObject();
           for (int i = 0; i < atomlist.length; i++) {
             AtomSite ato = (AtomSite) atomlist[i].getCopy((XRDcat) atomstructure);
             double[] lcoord = {ato.getLocalCoordX().getValueD(), ato.getLocalCoordY().getValueD(),
                 ato.getLocalCoordZ().getValueD()};
             double[] acoord = ato.transformAbsoluteCoordToLocal(lcoord);
             ato.setLocalCoordinates(new Coordinates(acoord[0], acoord[1], acoord[2]));
             atomstructure.addAtom(ato);
           }
         }
         m_atomPanel.removeSiteAction();
       }
     }

     void btnMoveToParentActionPerformed() {
       TreePath cur_sel = treeFragments.getSelectionPath();
       if (cur_sel != null) {
         DefaultMutableTreeNode n_child = (DefaultMutableTreeNode) (cur_sel.getLastPathComponent());
         DefaultMutableTreeNode n_parent = (DefaultMutableTreeNode) (n_child.getParent());
         if (n_parent != null) {
           while (!n_parent.isRoot())
            n_parent = (DefaultMutableTreeNode) (n_parent.getParent());
           Vector atomList = ((Fragment) n_child.getUserObject()).getAtomList();
           AtomsStructureI atomstructure = (AtomsStructureI) n_parent.getUserObject();
           for (int i = 0; i < atomList.size(); i++) {
             AtomSite originalAtom = (AtomSite) atomList.elementAt(i);
             AtomSite ato = (AtomSite) ((XRDcat) atomList.elementAt(i)).getCopy((XRDcat) atomstructure);
             double[] lcoord = {ato.getLocalCoordX().getValueD(), ato.getLocalCoordY().getValueD(),
                 ato.getLocalCoordZ().getValueD()};
             double[] acoord = originalAtom.transformLocalCoordToAbsolute(lcoord);
             ato.setAbsoluteCoordinates(acoord);
             atomstructure.addAtom(ato);
           }
           for (int i = atomList.size() - 1; i >= 0; i--)
             ((Fragment) n_child.getUserObject()).removeAtomAt(i);
         }
       }
     }

    void btnNewActionPerformed() {
      DefaultMutableTreeNode n_parent = null;
      TreePath parentPath = treeFragments.getSelectionPath();

      if (parentPath == null) {
        n_parent = structNode;
      } else {
        n_parent = (DefaultMutableTreeNode) (parentPath.getLastPathComponent());
      }

//      System.out.println("n_parent.getUserObject() " + n_parent.getUserObject().getClass());

      Fragment f_tmp = new Fragment((XRDcat) n_parent.getUserObject());
      DefaultMutableTreeNode n_child = new DefaultMutableTreeNode(f_tmp);
      treeModel.insertNodeInto(n_child, n_parent, n_parent.getChildCount());
      treeFragments.scrollPathToVisible(new TreePath(n_child.getPath()));

      if (n_parent.getUserObject() instanceof StructureAtomic)
        ((StructureAtomic) n_parent.getUserObject()).addFragment(f_tmp);
      if (n_parent.getUserObject() instanceof Fragment)
        ((Fragment) n_parent.getUserObject()).addFragment(f_tmp);
    }

    public AtomsStructureI getSelectedFragment() {
      AtomsStructureI atomstructure = null;
      TreePath cur_sel = treeFragments.getSelectionPath();
      if (cur_sel != null) {
        DefaultMutableTreeNode n_child = (DefaultMutableTreeNode) (cur_sel.getLastPathComponent());
        if (n_child != null) {
          atomstructure = (AtomsStructureI) n_child.getUserObject();
        } else {
          return m_Struct;
        }
      } else {
        return m_Struct;
      }
      return atomstructure;
    }
  }

  /**
   * BondPanel - the Bond List editing Panel
   */


  class BondPanel extends JPanel {

/*		int bondselected = -1;

		JScrollPane listScrollPane;
		javax.swing.JList listBonds;
		//DefaultListModel listModel;
		javax.swing.JPanel bondEditP;
		javax.swing.JLabel labelBond;
		javax.swing.JTextField textBondLabel;
		javax.swing.JButton btnNew;
		javax.swing.JButton btnRemove;
		javax.swing.JComboBox comboBondType;
		javax.swing.JComboBox comboAtom1;
		javax.swing.JComboBox comboAtom2;    */
    //javax.swing.JButton btnApply;
    JSubordSListPane bondPanel = null;

    public BondPanel() {
      initComponents();
      initListeners();
    }

    void initListeners() {
      String exlabels[] = {"First atom:", "Second atom:"};
      bondPanel.setList(m_Struct, StructureAtomic.BondListID, 2, exlabels);
/*			listBonds.addListSelectionListener(new ListSelectionListener() {
				public void valueChanged(ListSelectionEvent event) {
					bondList_ListSelect();
				}
			});
			if (m_Struct.getAtomList().size() > 0)
				bondselected = 0;       */
    }

    public void retrieveParameters() {
      bondPanel.retrieveparlist();
    }

    public void dispose() {
      bondPanel.dispose();
    }

    void initComponents() {

      bondPanel = new JSubordSListPane(parentD, false);
      setLayout(new FlowLayout(FlowLayout.CENTER, 3, 3));
      add(bondPanel);


/*
			java.awt.GridBagConstraints gridBagConstraints;

			//listModel = new DefaultListModel();
			listBonds = new javax.swing.JList();
			listScrollPane = new JScrollPane(listBonds, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                          JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

			bondEditP = new javax.swing.JPanel();
			btnNew = new javax.swing.JButton();
			btnRemove = new javax.swing.JButton();
			labelBond = new javax.swing.JLabel();
			textBondLabel = new javax.swing.JTextField();
			comboBondType = new javax.swing.JComboBox();
			comboAtom1 = new javax.swing.JComboBox();
			comboAtom2 = new javax.swing.JComboBox();

			setLayout(new java.awt.GridBagLayout());

<<<<<<< StructureAtomicEditPanel.java
			//setPreferredSize(new java.awt.Dimension(290, 290));
			bondEditP.setLayout(new BorderLayout(6, 6));
=======
			setPreferredSize(new java.awt.Dimension(280, 300));
			bondEditP.setLayout(new FlowLayout());
>>>>>>> 1.21
			bondEditP.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "Bonds"));

			listBonds.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
			add(listScrollPane);

			//bondEditP.setLayout(new java.awt.GridBagLayout());

			btnNew.setToolTipText("Add new bond");
			btnNew.setText("Add new");
			btnNew.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent evt) {
					m_Struct.addBond();
				}
			});

			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
			gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
			gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
			//bondEditP.add(btnNew, gridBagConstraints);
			bondEditP.add(btnNew);

			btnRemove.setToolTipText("Remove bond");
			btnRemove.setText("Remove");
			btnRemove.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent evt) {
					if (listBonds.isSelectionEmpty())
						return;
					int index = listBonds.getSelectedIndex();
					m_Struct.removeBondAt(index);
				}
			});

			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
			gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
			//bondEditP.add(btnRemove, gridBagConstraints);
			bondEditP.add(btnRemove);

			labelBond.setLabelFor(textBondLabel);
			labelBond.setText("label");
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.insets = new java.awt.Insets(15, 0, 0, 0);
			//bondEditP.add(labelBond, gridBagConstraints);
			bondEditP.add(labelBond);

			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
			gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
			gridBagConstraints.insets = new java.awt.Insets(15, 0, 0, 0);
			//bondEditP.add(textBondLabel, gridBagConstraints);
			bondEditP.add(textBondLabel);

			comboBondType.setToolTipText("Select bond type");
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
			gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
			gridBagConstraints.insets = new java.awt.Insets(5, 0, 5, 0);
			//bondEditP.add(comboBondType, gridBagConstraints);
			bondEditP.add(comboBondType);

			comboAtom1.setToolTipText("Select first atom");
			comboAtom1.setPreferredSize(new java.awt.Dimension(60, 24));
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			//bondEditP.add(comboAtom1, gridBagConstraints);
			bondEditP.add(comboAtom1);

			comboAtom2.setToolTipText("Select second atom");
			comboAtom2.setPreferredSize(new java.awt.Dimension(60, 24));
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
			//bondEditP.add(comboAtom2, gridBagConstraints);
			bondEditP.add(comboAtom2);

			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
			gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
			//gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
			gridBagConstraints.insets = new java.awt.Insets(5, 5, 5, 5);
			//add(bondEditP, gridBagConstraints);
			add(bondEditP);


			initBondList(); */

    }

/*		public void initBondList() {
			int bondnumb = m_Struct.getBondList().setList(listBonds);
		}

		void bondList_ListSelect() {
      comboAtom1.removeAllItems();
      comboAtom2.removeAllItems();
      for (int na = 0; na < m_Struct.getAtomNumber(); na++) {
        comboAtom1.addItem(((AtomSite) m_Struct.getAtomList().get(na)).getLabel());
        comboAtom2.addItem(((AtomSite) m_Struct.getAtomList().get(na)).getLabel());
      }
			retrieveBond();
			if (listBonds != null) {
				bondselected = listBonds.getSelectedIndex();
				setBond(bondselected);
			}
		}

		void retrieveBond() {
			if (bondselected >= 0) {
				Bond b_tmp = getSelectedBond();
				if (b_tmp != null) {
					b_tmp.setAtom1((String)comboAtom1.getSelectedItem());
					b_tmp.setAtom2((String)comboAtom2.getSelectedItem());
					b_tmp.setLabel(textBondLabel.getText());
				}
			}
		}

		public void setBond() {
			Bond b_tmp = getSelectedBond();
			if (b_tmp != null) {
				comboAtom1.setSelectedItem(b_tmp.getAtom1());
				comboAtom2.setSelectedItem(b_tmp.getAtom1());
				textBondLabel.setText(b_tmp.getLabel());
			}
			setBondComponent(b_tmp);
		}

		public void setBond(int numb) {
			listBonds.setSelectedIndex(numb);
			setBond();
		}

		private void setBondComponent(Bond b_tmp) {
			if (b_tmp != null) {
			}
		}

		private Bond getSelectedBond() {
			return (Bond) m_Struct.getBondList().selectedElement();
		}  */

  }


}


