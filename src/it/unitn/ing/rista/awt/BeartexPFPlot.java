/*
 * @(#)BeartexPFPlot.java created 2/25/2003 Berkeley
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
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

import it.unitn.ing.jgraph.*;
import it.unitn.ing.rista.util.*;

import java.awt.*;
import java.awt.print.Printable;
import java.awt.print.PrinterJob;
import java.awt.print.PageFormat;
import java.awt.print.PrinterException;
import java.awt.event.*;
import java.awt.datatransfer.*;
import java.io.BufferedReader;
import java.util.*;

/**
 *  The BeartexPFPlot is a class to plot pole figures from Beartex.
 * <p>
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:52 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class BeartexPFPlot extends Frame implements ClipboardOwner, Printable {

  CopyPrintPanel pfPanel = null;
  protected Component componentToPrint = null;
  static boolean newBehavior = true;
  Menu editMenu = null;
  PoleFigureMap[] ccolorMap = null;

  public BeartexPFPlot(double[][][] pole, int hkl[][], int numberPoleFigures,
                       int numberofPoints, int mode, int pixelsNumber, double zoom,
                       boolean logScale, double maxAngle, double min, double max, double smooth,
                       boolean grayScale, int colrsNumber, String titles, boolean exitAtEnd) {

    super();

    int numberPoles = numberPoleFigures;

    createDefaultMenuBar();

    Panel c1 = new Panel(new BorderLayout(0, 0));
    c1.setBackground(Color.white);

    add(c1);

    String meanLabel = "1 mrd";
    double meanValue = 1.0;

    String first = new String("Reconstructed pole figures");
    String log = "";
    if (logScale)
      log = " (Log scale, contours in log units)";
    Label title = new Label(titles, Label.CENTER);
    title.setFont(new Font("Arial", Font.PLAIN, 16));
    Panel p1 = new Panel(new FlowLayout(FlowLayout.CENTER, 6, 6));
    p1.setBackground(Color.white);
    p1.add(title);
    c1.add(BorderLayout.NORTH, p1);
    p1 = new Panel(new FlowLayout(FlowLayout.CENTER, 3, 3));
    p1.setBackground(Color.white);
    p1.add(new Label(" "));
    c1.add(BorderLayout.WEST, p1);
    p1 = new Panel(new FlowLayout(FlowLayout.CENTER, 3, 3));
    p1.setBackground(Color.white);
    p1.add(new Label(" "));
    c1.add(BorderLayout.SOUTH, p1);

    pfPanel = new CopyPrintPanelNoBkg();
    //  PoleFigureFocusListener theFocusListener = new PoleFigureFocusListener();

    Object[] listGrid = new Object[numberPoles];
    String[] label = new String[numberPoles];
    boolean computeMinMax = false;
    if (min == max) {
      min = 1.0E30;
      max = -1.0E30;
      computeMinMax = true;
    }

    int izoom = 1;
    int tempGrid = numberofPoints;
    while (tempGrid < pixelsNumber) {
      izoom *= 2;
      tempGrid *= 2;
    }
    int gridNumber = PlotPoleFigure.getNewGridNumber(numberofPoints, izoom);
    int size = pixelsNumber + PoleFigureMap.inset * 2;
    if (!newBehavior)
      size = gridNumber + PoleFigureMap.inset * 2;
    int col = 0;
    Dimension screenSize = getToolkit().getScreenSize();
    while (col * size + 50 < screenSize.width)
      col++;
    col--;
    int row = (int) (0.99 + (1.0 + numberPoles) / col);
//    System.out.println(row + " " + col);
    col = (int) (0.99 + (1.0 + numberPoles) / row);
//System.out.println(row + " " + col);

    pfPanel.setLayout(new GridLayout(0, col, 0, 0));
    pfPanel.setBackground(Color.white);

    for (int i = 0; i < numberPoles; i++) {
      double[][] grid = PlotPoleFigure.enlargeGrid(pole[i], numberofPoints, izoom, 0.7);

      label[i] = new String(Integer.toString(hkl[i][0]) + " " +
              Integer.toString(hkl[i][1]) + " " +
              Integer.toString(hkl[i][2]));

      System.out.println("Plotting pole " + label[i] + ", number of points: " + numberofPoints +
              ", pixels:" + gridNumber);

      listGrid[i] = grid;
      if (computeMinMax)
      for (int j = 0; j < gridNumber; j++)
        for (int k = 0; k < gridNumber; k++) {
          if (grid[j][k] != ColorMap.DUMMY_VALUE && !Double.isNaN(grid[j][k])) {
            min = Math.min(grid[j][k], min);
            max = Math.max(grid[j][k], max);
          }
        }
    }
    double[] limits = confirmLimits(min, max);
	  if (limits[0] <= 0)
		  limits[0] = 0.01;
	  if (limits[1] <= limits[0])
		  limits[1] = limits[0] + 1;
	  if (logScale)
      for (int j = 0; j < 2; j++)
        limits[j] = MoreMath.log10(limits[j]);
    if (newBehavior)
      ccolorMap = new PoleFigureMap[numberPoles];
    for (int i = 0; i < numberPoles; i++) {
      double[][] grid = (double[][]) listGrid[i];
      if (logScale) {
        for (int j = 0; j < gridNumber; j++)
          for (int k = 0; k < gridNumber; k++) {
            if (grid[j][k] != ColorMap.DUMMY_VALUE && !Double.isNaN(grid[j][k]) && grid[j][k] > 0.0)
              grid[j][k] = MoreMath.log10(grid[j][k]);
          }
      }
      if (newBehavior) {
        ccolorMap[i] = new PoleFigureMap(grid, gridNumber, limits[0], limits[1],
              grayScale, label[i], colrsNumber, editMenu, zoom, pixelsNumber, true);
      //   colorMap.addFocusListener(theFocusListener);
        pfPanel.add(ccolorMap[i]);
      } else {
        ColorMap colorMap =  new ColorMap(grid, gridNumber, limits[0], limits[1],
                grayScale, label[i], colrsNumber);
        pfPanel.add(colorMap);
      }
    }

    if (limits[0] != limits[1]) {
	    int legendHeight = gridNumber;
	    int pwidth = legendHeight / 5;
	    int pheight = legendHeight;
	    if (!newBehavior) {
		    double[] legendGrid = new double[pheight];
		    double step = (limits[1] - limits[0]) / pheight;
		    for (int j = 0; j < pheight; j++) {
			    legendGrid[j] = step * j + limits[0];
		    }
		    if (logScale)
			    meanValue = Math.log(1.0) / Math.log(10.0);

		    int decimals = 100;
		    MapLegend mapLegend = new MapLegend(legendGrid, pwidth, pheight, limits[0], limits[1], grayScale,
				    logScale, meanValue, meanLabel, colrsNumber, decimals);
		    pfPanel.add(mapLegend);
	    } else {
		    double[][] legendGrid = new double[pwidth][pheight];
		    double step = (limits[1] - limits[0]) / pheight;
//			  System.out.println(step + " " + limits[1] + " " + limits[0] + " " + pheight);
		    for (int j = 0; j < pheight; j++) {
			    legendGrid[0][j] = step * j + limits[0];
			    for (int i = 1; i < pwidth; i++)
				    legendGrid[i][j] = legendGrid[0][j];
		    }
		    String unit = "mrd";
		    if (logScale) {
			    meanValue = 0;
			    unit = "Log(mrd)";
		    }
		    LegendPoleFigureMap mapLegend =  new LegendPoleFigureMap(legendGrid, pwidth, pheight, limits[0], limits[1], grayScale, unit,
				    colrsNumber, editMenu, zoom, pixelsNumber, meanValue);
		    pfPanel.add(mapLegend);
	    }
    }
    int dummyAdded = 1;
    while (row * col != numberPoles + dummyAdded) {
      pfPanel.add(new PoleFigureMap.WhiteMap(pixelsNumber, pixelsNumber));
      dummyAdded++;
    }

    c1.add(BorderLayout.CENTER, pfPanel); //scrollPane);

    setComponentToPrint(pfPanel);
    listGrid = null;

    if (exitAtEnd) {
    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        System.exit(0);
      }
    });
    addKeyListener(new KeyListener() {
      public void keyPressed(KeyEvent e) {
        if (e.getKeyCode() == KeyEvent.VK_ENTER || e.getKeyCode() == KeyEvent.VK_SPACE)
          System.exit(0);
      }
      public void keyReleased(KeyEvent e) {
      }
      public void keyTyped(KeyEvent e) {
      }
    });
    } else {
      addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent e) {
          setVisible(false);
          dispose();
        }
      });
    }

//    setSize(numberPoles * pixelsNumber +80 , pixelsNumber + 80);
    setVisible(true);
    pack();
    setBatch(false);
  }

  public void setBatch(boolean value) {
    if (ccolorMap != null) {
      for (int i = 0; i < ccolorMap.length; i++)
        ccolorMap[i].setBatch(value);
    }
  }

  /**
   * Set the component that will be printed by calling the default print method
   * (letsTryToPrint).
   * @param    component the component to print
   */

  public void setComponentToPrint(Component component) {
    componentToPrint = component;
  }

  public void createDefaultMenuBar() {
    MenuBar menuBar = new MenuBar();
    Menu menu = new Menu("File");
    menuBar.add(menu);
    MenuItem menuItem = new MenuItem("Print");
    menuItem.setShortcut(new MenuShortcut((int) 'p'));
    menuItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        if (command.equals("Print"))
          letsTryToPrint();
      }
    });
    menu.add(menuItem);
    menuItem = new MenuItem("Quit");
    menuItem.setShortcut(new MenuShortcut((int) 'q'));
    menuItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        if (command.equals("Quit"))
          System.exit(0);
      }
    });
    menu.add(menuItem);
    menuBar.add(createEditMenu());
    setMenuBar(menuBar);
  }

  public Menu createEditMenu() {
    MenuItem menuitem = null;

    editMenu = new Menu("Edit");

    editMenu.add(menuitem = new MenuItem("Copy"));
    menuitem.setShortcut(new MenuShortcut((int) 'c'));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        Clipboard clipboard = getToolkit().getSystemClipboard();
        Component comp = componentToPrint;
        if (comp != null) {
//          setBatch(true);
          Rectangle rect = comp.getBounds();
          Image fileImage =
                  createImage(rect.width, rect.height);
          Graphics g = fileImage.getGraphics();

          //write to the image
//          ((CopyPrintPanel) comp).clearComponent(g);
//          ((CopyPrintPanel) comp).paintComponent(g, comp);
          comp.print(g);
          clipboard.setContents(new ClipImage(fileImage), BeartexPFPlot.this);
          // write it out in the format you want

          //dispose of the graphics content
          g.dispose();
  //        setBatch(false);
        }
      }
    });

    return editMenu;
  }

  public void lostOwnership(Clipboard parClipboard, Transferable parTransferable) {
// 	  System.out.println ("Lost ownership");
  }

  /**
   * Send the content of the frame to the printer.
   */

  public void letsTryToPrint() {
//    setBatch(true);
    PrinterJob pjob = PrinterJob.getPrinterJob();
    if (pjob != null) {
      PageFormat graphicsPageFormat = new PageFormat();
      graphicsPageFormat = pjob.pageDialog(graphicsPageFormat);
      if (pjob.printDialog()) {
//        graphicsPageFormat = pjob.validatePage(graphicsPageFormat);
        pjob.setPrintable(this, graphicsPageFormat);
        try {
          pjob.print();
        } catch (Exception PrintException) {
          PrintException.printStackTrace();
        }
      }
    }
//    setBatch(false);
  }

  public int print(Graphics graphics, PageFormat pageFormat, int pageIndex) throws PrinterException {
//		Graphics2D  g2 = (Graphics2D) graphics;
//    if (componentToPrint == null)
//      componentToPrint = getContentPane();
//		System.out.println("Printing: " + componentToPrint);
    if (pageIndex > 0) return NO_SUCH_PAGE;
    graphics.translate((int) pageFormat.getImageableX(), (int) pageFormat.getImageableY());
    componentToPrint.print(graphics);
    return PAGE_EXISTS;
  }

  double intensityMin = 0.0;
  double intensityMax = 1.0;

  public double[] confirmLimits(double min, double max) {
    double[] limits = new double[2];
    intensityMin = min;
    intensityMax = max;
       LimitsDialog rangeDialog = new LimitsDialog(this);
       rangeDialog.setVisible(true);
       while (rangeDialog.isVisible()) {
         try {
           Thread.currentThread().sleep(100);
         } catch (InterruptedException ie) {
         }
       }
    limits[0] = intensityMin;
    limits[1] = intensityMax;
    return limits;
  }

  public double[][] createGrid(double[][] pole, int mode, int numberofPoints,
                               double maxAngle, int zoom, double smooth) {
    double PF[][] = pole;

    return PlotPoleFigure.enlargeGrid(PF, numberofPoints, zoom, smooth);
  }

  class LimitsDialog extends Dialog {

    private TextField yminText = null;
    private TextField ymaxText = null;

    public LimitsDialog(Frame aframe) {
      super(aframe, true);
      Panel pane = new Panel();
      LimitsDialog.this.add(pane);
      pane.setLayout(new BorderLayout());

      Panel rangepane = new Panel();
      rangepane.setLayout(new BorderLayout(6, 6));
      pane.add(rangepane, BorderLayout.CENTER);

      Panel jp1 = new Panel();
      jp1.setLayout(new GridLayout(0, 1, 6, 6));
      yminText = addRow(jp1, new Label("Min:"), intensityMin);
      ymaxText = addRow(jp1, new Label("Max:"), intensityMax);
      rangepane.add("Center", jp1);

      jp1 = new Panel();
      jp1.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      Button cancel = new Button("No common range");
      Button reset = new Button("Reset");
      Button done = new Button("Accept");
      jp1.add(cancel);
      jp1.add(reset);
      jp1.add(done);
      rangepane.add("South", jp1);

      cancel.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          LimitsDialog.this.setVisible(false);
        }
      });

      reset.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          intensityMin = 0.0;
          intensityMax = 0.0;
          LimitsDialog.this.setVisible(false);
        }
      });

      done.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          Double d;
          double tymin = intensityMin;
          double tymax = intensityMax;

          d = Double.valueOf(yminText.getText());
          if (d != null) tymin = d.doubleValue();
          d = Double.valueOf(ymaxText.getText());
          if (d != null) tymax = d.doubleValue();

          if (tymax > tymin) {
            intensityMin = tymin;
            intensityMax = tymax;
          }

          LimitsDialog.this.setVisible(false);
        }
      });

      LimitsDialog.this.setTitle("Pole figure(s) intensity range");
      LimitsDialog.this.pack();

    }

    public TextField addRow(Panel panel, Label l1, double value) {

      Panel jp = new Panel();
      jp.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      jp.add(l1);
      TextField textfield = new TextField(20);
      textfield.setText(String.valueOf(value));
      jp.add(textfield);
      panel.add(jp);
      return textfield;
    }

  }

/*  Component focusedComponent = null;

  public class PoleFigureFocusListener implements FocusListener {
     public void focusGained(FocusEvent fe) {
       focusedComponent = fe.getComponent();

     }

     public void focusLost(FocusEvent fe) {

     }
  } */

  public static void main(String args[]) {
    String filename = Misc.filterFileName(args[0]);
    BufferedReader PFreader = Misc.getReader(filename);
    String title = null;
    String token = null;
    if (args.length > 1 && args[1].equalsIgnoreCase("classic"))
      newBehavior = false;

    if (PFreader != null) {
      try {

        int n1 = 0;
        int n2 = 0;
        String line = null;
        StringTokenizer st = null;
        line = PFreader.readLine();
        st = new StringTokenizer(line, "' ,\t\r\n");
        double max = Double.parseDouble(st.nextToken());
        double min = Double.parseDouble(st.nextToken());

        line = PFreader.readLine();
        st = new StringTokenizer(line, "' ,\t\r\n");
        int logs = Integer.parseInt(st.nextToken());
        boolean logScale = false;
        if (logs == 1)
          logScale = true;

        line = PFreader.readLine();
        st = new StringTokenizer(line, "' ,\t\r\n");
        int negative = Integer.parseInt(st.nextToken());

        line = PFreader.readLine();
        st = new StringTokenizer(line, "' ,\t\r\n");
        int colorScheme = Integer.parseInt(st.nextToken());
        boolean grayshade = false;
        if (colorScheme == 1)
          grayshade = true;

        line = PFreader.readLine();
        st = new StringTokenizer(line, "' ,\t\r\n");
        int colorNumbers = Integer.parseInt(st.nextToken());

        line = PFreader.readLine();
        st = new StringTokenizer(line, "' ,\t\r\n");
        double smooth = Double.parseDouble(st.nextToken());

        line = PFreader.readLine();
        st = new StringTokenizer(line, "' ,\t\r\n");
        int pixels = Integer.parseInt(st.nextToken());

        line = PFreader.readLine();
        st = new StringTokenizer(line, "' ,\t\r\n");
        st.nextToken();
        int numberPoleFiguresPF = Integer.valueOf(st.nextToken()).intValue();
        double[][][] trialPole = null;
        int[][] hkl = new int[numberPoleFiguresPF][3];
        for (int i = 0; i < numberPoleFiguresPF; i++) {
          if (i != 0)
            line = PFreader.readLine();
          title = PFreader.readLine();
          line = PFreader.readLine();
          st = new StringTokenizer(line, "' ,\t\r\n");
          for (int j = 0; j < 3; j++)
            hkl[i][j] = Integer.valueOf(st.nextToken()).intValue();
          line = PFreader.readLine();
          if (i == 0) {
            st = new StringTokenizer(line, "' ,\t\r\n");
            n1 = Integer.valueOf(st.nextToken()).intValue();
            n2 = Integer.valueOf(st.nextToken()).intValue();
            trialPole = new double[numberPoleFiguresPF][n1][n1];
          }
          for (int j = 0; j < n2; j++) {
            line = PFreader.readLine();
            st = new StringTokenizer(line, "' ,\t\r\n");
            for (int k = 0; k < n1 && st.hasMoreTokens(); k++) {
              String value = st.nextToken();
              if (value.equals("-.99"))
                trialPole[i][k][n1 - j - 1] = Double.NaN;
              else
                trialPole[i][k][n1 - j - 1] = Double.parseDouble(value);
            }
          }
          for (int j = n2; j < n1; j++) {
            for (int k = 0; k < n1; k++)
              trialPole[i][k][n1 - j - 1] = Double.NaN;
          }
        }

        PFreader.close();
        new BeartexPFPlot(trialPole, hkl, numberPoleFiguresPF, n1, 0, pixels, 1.0,
                logScale, 90.0, min, max, smooth,
                grayshade, colorNumbers, title, true);
      } catch (Throwable io) {
        io.printStackTrace();
        try {
          PFreader.close();
        } catch (Throwable to) {
        }
        System.out.println("Error catched, check it out or send it to maud@ing.unitn.it");
        System.out.println("Your command was not completed successfully!");
      }
    }

  }

}




