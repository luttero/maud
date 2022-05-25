/*
 * @(#)Show3DShape.java created Jul 16, 2003 Berkeley
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.render3d;

import it.unitn.ing.rista.interfaces.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import javax.media.opengl.*;
import javax.media.opengl.awt.*;
import java.awt.*;
import java.awt.event.*;

/**
 * The Show3DShape is a class
 *
 * @version $Revision: 1.6 $, $Date: 2006/11/10 09:33:01 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Show3DShape extends JPanel {

  i3DCanvas canvasStruct3d;
//  JButton btnDisplayShape;
//  JButton btnDisplayStruct;
  JToggleButton btnEdit;
  JToggleButton btnRotate;
  JButton btnZoomIn;
  JButton btnZoomOut;
  JButton btnColors;
  boolean notInitialized = true;
  Shape3D crystalliteShape = null;
  int drawmode = 0;
  int resolution = 400;
  double scaleplot = 0;
//  Animator animator = null;
  boolean animated = false;

  public Show3DShape(Shape3D crystalliteShape, int drawmode,
                     int resolution, double scaleplot) {
    this.crystalliteShape = crystalliteShape;
    this.drawmode = drawmode;
    this.resolution = resolution;
    this.scaleplot = scaleplot;
  }

  public void initComponents() {
    notInitialized = false;
    JPanel panel3DButtons = null;

    setLayout(new BorderLayout());

/*    if (Constants.OpenGL) {
      try {
        canvasStruct3d = new Crystallite3Dgl(crystalliteShape, drawmode, resolution, scaleplot);
        GLProfile glp = GLProfile.getDefault();
        GLCanvas canvas = new GLCanvas(new GLCapabilities(glp));
        canvas.addGLEventListener((Crystallite3Dgl) canvasStruct3d);
        add(canvas, BorderLayout.CENTER);
        ((AnimatedRendering3Dgl)canvasStruct3d).setCanvas(canvas);
      } catch (Throwable e) {
        Constants.OpenGL = false;
        canvasStruct3d = new Crystallite3Djgl(crystalliteShape, drawmode, resolution, scaleplot);
        add((Component) canvasStruct3d, BorderLayout.CENTER);
      }
    } else {*/
      canvasStruct3d = new Crystallite3Djgl(crystalliteShape, drawmode, resolution, scaleplot);
      add((Component) canvasStruct3d, BorderLayout.CENTER);
 //   }

    // canvas.setSize(new Dimension(300, 300));
    add(panel3DButtons = new JPanel(), BorderLayout.SOUTH);

/*    panel3DButtons.add(btnDisplayShape = new JIconButton("CellCubic.gif"));
    btnDisplayShape.setToolTipText("Display Cell");
    btnDisplayShape.setSelected(true);
    btnDisplayShape.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        btnDisplayShape.setSelected(!btnDisplayShape.isSelected());
        if (canvasStruct3d instanceof Structure3Dgl)
          ((Structure3Dgl) canvasStruct3d).b_drawCell = btnDisplayShape.isSelected();
        else
          ((Structure3Djgl) canvasStruct3d).b_drawCell = btnDisplayShape.isSelected();
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
    });*/

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

    panel3DButtons.add(btnColors = new JButton(
            new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + "Palette.gif"))));
    btnColors.setToolTipText("Reload 3D pref");
    btnColors.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        btnEditActionPerformed(evt);
      }
    });

//    startRotation();
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
    Constants.read3DPreferences();
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
//    if (animator != null)
//      animator.start();
    // Add your handling code here:
  }

  public void stopRotation() {
//      if (canvasStruct3d instanceof i3DCanvas)
    if (!animated)
      return;
    animated = false;
    canvasStruct3d.stop();
//    if (animator != null)
//      animator.stop();
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

  void btnColorsActionPerformed(ActionEvent evt) {
//    AtomColorPreferences.showPrefs(parentD);
  }


}
