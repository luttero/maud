/*
 * @(#)PeriodicTable.java created 16/09/2000 Casalino
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

package it.unitn.ing.rista.chemistry;

import java.awt.*;
import java.util.StringTokenizer;

import it.unitn.ing.rista.awt.*;

import javax.swing.*;
import java.awt.event.*;

/**
 * The PeriodicTable is canvas to display the periodic table of elements.
 *
 * @version $Revision: 1.4 $, $Date: 2004/12/27 16:05:17 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class PeriodicTable extends JComponent {
  Graphics gCanvas;
  Image mendeleevImage = null;
  int maxColorsNumber = 100;
  public static int maxAtomNumber = 103;
  Color cellColor[];
  Color characterColor[];
  String atomLabel[];
  int atomCoordinate[][];
  int colorCellNumber[];
  int characterNumber[];
  int atomNumber;
  ChooseAtomD atomFrame = null;
  int cellDimX = 32;
  int cellDimY = 32;
  int leftBorder = 14;
  int upperBorder = 18;

  public PeriodicTable(ChooseAtomD atomFrame) {
    this.atomFrame = atomFrame;
    cellColor = new Color[maxColorsNumber];
    characterColor = new Color[7];
    atomLabel = new String[maxAtomNumber];
    atomCoordinate = new int[maxAtomNumber][2];
    colorCellNumber = new int[maxAtomNumber];
    characterNumber = new int[maxAtomNumber];
    atomNumber = 0;
    String atomLabelsString = "H He Li Be B C N O F Ne Na Mg Al Si P S Cl Ar K Ca Sc Ti V Cr Mn Fe Co Ni Cu Zn Ga Ge As Se Br Kr Rb Sr Y Zr Nb Mo Tc Ru Rh Pd Ag Cd In Sn Sb Te I Xe Cs Ba La Ce Pr Nd Pm Sm Eu Gd Tb Dy Ho Er Tm Yb Lu Hf Ta W Re Os Ir Pt Au Hg Tl Pb Bi Po At Rn Fr Ra Ac Th Pa U Np Pu Am Cm Bk Cf Es Fm Md No Lw Uq Up Uh";
    String string1 = "9 1 1 3 1 18 9 2 1 9 2 2 12 2 13 12 2 14 12 2 15 12 2 16 12 2 17 3 2 18 9 3 1 9 3 2 12 3 13 12 3 14 12 3 15 12 3 16 12 3 17 3 3 18 9 4 1 9 4 2 14 4 3 14 4 4 14 4 5 14 4 6 14 4 7 14 4 8 14 4 9 14 4 10 14 4 11 14 4 12 12 4 13 12 4 14 12 4 15 12 4 16 12 4 17 3 4 18 9 5 1 9 5 2 14 5 3 14 5 4 14 5 5 14 5 6 14 5 7 14 5 8 14 5 9 14 5 10 14 5 11 14 5 12 12 5 13 12 5 14 12 5 15 12 5 16 12 5 17 3 5 18 9 6 1 9 6 2 14 6 3 19 8 4 19 8 5 19 8 6 19 8 7 19 8 8 19 8 9 19 8 10 19 8 11 19 8 12 19 8 13 19 8 14 19 8 15 19 8 16 19 8 17 14 6 4 14 6 5 14 6 6 14 6 7 14 6 8 14 6 9 14 6 10 14 6 11 14 6 12 12 6 13 12 6 14 12 6 15 12 6 16 12 6 17 3 6 18 9 7 1 9 7 2 14 7 3 19 9 4 19 9 5 19 9 6 19 9 7 19 9 8 19 9 9 19 9 10 19 9 11 19 9 12 19 9 13 19 9 14 19 9 15 19 9 16 19 9 17 14 7 4 14 7 5 14 7 6";
    String string2 = "0 6 1 4 5 5 5 5 5 6 1 1 4 4 5 5 5 6 1 1 1 4 4 5 5 4 4 1 1 4 4 4 5 5 5 6 1 1 1 4 5 5 5 5 4 1 3 1 3 4 5 5 5 6 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 4 5 5 5 5 1 1 4 1 1 4 5 4 5 6 1 1 2 2 2 4 4 4 2 2 2 2 2 2 2 2 2 2 2 2";
    StringTokenizer stringTokenizer = new StringTokenizer(string2);
    for (int i = 0; i < maxAtomNumber; i++)
      characterNumber[i] = Integer.valueOf(stringTokenizer.nextToken()).intValue();
    stringTokenizer = new StringTokenizer(atomLabelsString);
    for (int j = 0; j < maxAtomNumber; j++)
      atomLabel[j] = stringTokenizer.nextToken();
    stringTokenizer = new StringTokenizer(string1);
    for (int k = 0; k < maxAtomNumber; k++) {
      colorCellNumber[k] = Integer.valueOf(stringTokenizer.nextToken()).intValue();
      atomCoordinate[k][0] = Integer.valueOf(stringTokenizer.nextToken()).intValue();
      atomCoordinate[k][1] = Integer.valueOf(stringTokenizer.nextToken()).intValue();
    }
    cellColor[12] = new Color(100, 100, 255);
    cellColor[9] = new Color(255, 255, 100);
    cellColor[14] = new Color(255, 100, 100);
    cellColor[19] = new Color(0, 150, 150);
    cellColor[3] = new Color(255, 100, 255);
    characterColor[0] = Color.white;
    characterColor[1] = new Color(97, 144, 237);
    characterColor[2] = new Color(157, 186, 242);
    characterColor[3] = new Color(237, 195, 47);
    characterColor[4] = new Color(247, 220, 150);
    characterColor[5] = new Color(237, 85, 112);
    characterColor[6] = Color.lightGray;

    addMouseListener(new PTmouse());
  }

  public PeriodicTable(ChooseAtomD atomFrame, String atomLabel) {
    this(atomFrame);
    for (int j = 0; j < maxAtomNumber; j++)
      if (this.atomLabel[j].equalsIgnoreCase(atomLabel))
        atomNumber = j;
  }

  public Dimension getPreferredSize() {
    return new Dimension(600, 314);
  }

  void drawCell(Graphics g, int i, int j, Color color, int k) {
    if (k == atomNumber)
      g.setColor(Color.white);
    else
      g.setColor(color);
    g.fillRect((i - 1) * cellDimX + leftBorder, (j - 1) * cellDimY + upperBorder, cellDimX - 2, cellDimY - 2);
    g.setColor(characterColor[characterNumber[k]]);
//        g.fillRect(i * cellDimX + leftBorder - 9, (j - 1) * cellDimY + upperBorder + 3, 5, 5);
    if (k == atomNumber) {
      g.setColor(Color.blue);
      g.drawRect((i - 1) * cellDimX + leftBorder + 1, (j - 1) * cellDimY + upperBorder + 1, cellDimX - 4, cellDimY - 4);
    } else {
      g.setColor(Color.black);
    }
    g.drawRect((i - 1) * cellDimX + leftBorder, (j - 1) * cellDimY + upperBorder, cellDimX - 2, cellDimY - 2);
  }

  void drawLabels(Graphics g) {
    String astring[] = {"basic", "amphoteric", "acidic", "noble gas"};
    g.setColor(Color.black);
    g.setFont(new Font("TimesRoman", 1, 24));
    g.drawString("PERIODIC TABLE", 213, 37);
    g.drawString(" of elements  ", 213, 57);
    g.setColor(new Color(220, 220, 220));
    g.drawString("PERIODIC TABLE", 210, 34);
    g.drawString(" of elements  ", 210, 54);
/*        if (atomNumber < 103)
        {
            String string1 = " " + jpt.resultPanel.a60[jpt.resultPanel.a59];
            if (jpt.resultPanel.a54[atomNumber] == 9999.0)
                string1 = " (?)  ";
            String string2 = " : " + string1;
            g.setColor(Color.black);
            g.drawString(string2, 240, 260);
            g.setColor(Color.magenta);
            g.drawString(string2, 238, 258);
        }*/
    for (int i = 1; i < 7; i++) {
      g.setColor(characterColor[i]);
      g.fillRect(cellDimY, 270 + i * 10, 10, 5);
    }
    gCanvas.setFont(new Font("TimesRoman", 0, 10));
    gCanvas.setColor(Color.lightGray);
    g.drawString(astring[0], 55, 290);
    g.drawString(astring[1], 55, 310);
    g.drawString(astring[2], 55, 325);
    g.drawString(astring[3], 55, 335);
  }

  public void paint(Graphics g) {
    super.paint(g);
    update(g);
  }

  public void update(Graphics g) {
    byte b = 40;
    Dimension canvasSize = getSize();
    Image aimage = createImage(canvasSize.width, canvasSize.height);
    gCanvas = aimage.getGraphics();
    gCanvas.setColor(Color.lightGray);
    gCanvas.fillRect(0, 0, canvasSize.width, canvasSize.height);
    for (int i1 = 0; i1 < maxAtomNumber; i1++)
      drawCell(gCanvas, atomCoordinate[i1][1], atomCoordinate[i1][0], cellColor[colorCellNumber[i1]], i1);
    gCanvas.setFont(new Font("TimesRoman", 0, 10));
//        gCanvas.setColor(Color.lightGray);
//        gCanvas.drawString("D.I.Mendeleev (1834-1907)", 215, 115);
//        gCanvas.drawString("(C) L. Lutterotti, 2000", 230, 70);
    gCanvas.setColor(Color.black);
    for (int j = 0; j < maxAtomNumber; j++) {
      int atomicNumber = j;
//      if (atomicNumber == 0)
        atomicNumber++;
      gCanvas.drawString(Integer.toString(atomicNumber), (atomCoordinate[j][1] - 1) * cellDimX + leftBorder + 2, (atomCoordinate[j][0] - 1) * cellDimY + upperBorder + 10);
    }
    gCanvas.setFont(new Font("TimesRoman", 1, 12));
    for (int k = 0; k < maxAtomNumber; k++)
      gCanvas.drawString(atomLabel[k], (atomCoordinate[k][1] - 1) * cellDimX + leftBorder + cellDimX / 2 - 8, (atomCoordinate[k][0] - 1) * cellDimY + upperBorder + cellDimY / 2 + 8);
    gCanvas.setColor(Color.white);
    gCanvas.drawString("I", leftBorder + 8, upperBorder - 4);
    gCanvas.drawString("II", cellDimX + leftBorder + 8, upperBorder - 4);
    gCanvas.drawString("III", 12 * cellDimX + leftBorder + 8, upperBorder + cellDimY - 4);
    gCanvas.drawString("IV", 13 * cellDimX + leftBorder + 8, upperBorder + cellDimY - 4);
    gCanvas.drawString("V", 14 * cellDimX + leftBorder + 8, upperBorder + cellDimY - 4);
    gCanvas.drawString("VI", 15 * cellDimX + leftBorder + 8, upperBorder + cellDimY - 4);
    gCanvas.drawString("VII", 16 * cellDimX + leftBorder + 8, upperBorder + cellDimY - 4);
    gCanvas.drawString("VIII", 17 * cellDimX + leftBorder + 8, upperBorder - 4);
    for (int i2 = 1; i2 <= 7; i2++)
      gCanvas.drawString(Integer.toString(i2), leftBorder - 10, (i2 - 1) * cellDimY + upperBorder + cellDimY / 2 + 2);
//        drawLabels(gCanvas);
//        if (mendeleevImage != null)
//            gCanvas.drawImage(mendeleevImage, 105, 10, this);
    g.drawImage(aimage, 0, 0, this);
  }

  class PTmouse extends MouseAdapter {

    /**
     * Handle the Mouse Up events
     */

    public void mouseClicked(MouseEvent evt) {
    }

    public void mouseEntered(MouseEvent evt) {
    }

    public void mouseExited(MouseEvent evt) {
    }

    public void mousePressed(MouseEvent evt) {
    }

    public void mouseReleased(MouseEvent evt) {
      int b = -1;
      int i = (int) Math.floor((double) ((evt.getX() - leftBorder + cellDimX) / cellDimX));
      int j = (int) Math.floor((double) ((evt.getY() - upperBorder + cellDimY) / cellDimY));
      for (int k = 0; k < maxAtomNumber; k++)
        if (i == atomCoordinate[k][1] && j == atomCoordinate[k][0])
          b = k;
      if (b >= 0 && b < maxAtomNumber && b != atomNumber) {
        atomNumber = b;
        atomFrame.setAtomLabel(atomLabel[b]);
        repaint();
      }
    }
  }

}
