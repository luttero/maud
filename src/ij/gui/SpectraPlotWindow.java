package ij.gui;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.awt.datatransfer.*;
import java.util.*;

import ij.*;
import ij.process.*;
import ij.util.*;
import ij.text.TextWindow;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.Utility;


/** This class is an extended ImageWindow that displays Spectra. */
public class SpectraPlotWindow extends ImageWindow implements ActionListener, ClipboardOwner {

  /** Display points using a circle 5 pixels in diameter. */
  public static final int CIRCLE = 0;

  /** Display points using an X-shaped mark. */
  public static final int X = 1;

  /** Connect points with solid lines. */
  public static final int LINE = 2;

  static final int LEFT_MARGIN = 50;
  static final int RIGHT_MARGIN = 20;
  static final int TOP_MARGIN = 20;
  static final int BOTTOM_MARGIN = 30;
  static final int WIDTH = 600;
  static final int HEIGHT = 400;

  static final String MIN = "pp.min";
  static final String MAX = "pp.max";
  static final String PLOT_WIDTH = "pp.width";
  static final String PLOT_HEIGHT = "pp.height";
  static final String OPTIONS = "pp.options";
  static final int SAVE_X_VALUES = 1;
  static final int AUTO_CLOSE = 2;

  private int frameWidth;
  private int frameHeight;
  private int xloc;
  private int yloc;

  private Rectangle frame = null;
  private float[] xValues, yValues;
  private float[] errorBars;
  private int nPoints;
  private double xScale, yScale;
  private double xMin, xMax, yMin, yMax;
  private Button list, save, copy, add;
  private Label coordinates;
  private static String defaultDirectory = null;
  private String xLabel;
  private String yLabel;
  Checkbox calAnglesB = null;

  private Font font = new Font("Helvetica", Font.PLAIN, 12);
  private boolean fixedYScale;
  private ImageProcessor ip;
  private ImagePlus impOriginal;
  private static int options;

  /** Save x-values only. To set, use Edit/Options/
   Profile Plot Options. */
  public static boolean saveXValues;

  /** Automatically close window after saving values. To
   set, use Edit/Options/Profile Plot Options. */
  public static boolean autoClose;

  /** The width of the plot in pixels. */
  public static int plotWidth = WIDTH;

  /** The height of the plot in pixels. */
  public static int plotHeight = HEIGHT;

  TextField minT = null, maxT = null, rT = null;

  // static initializer
  static {
    IJ.register(SpectraPlotWindow.class); //keeps options from being reset on some JVMs
    options = Prefs.getInt(OPTIONS, SAVE_X_VALUES);
    saveXValues = (options & SAVE_X_VALUES) != 0;
    autoClose = (options & AUTO_CLOSE) != 0;
    plotWidth = Prefs.getInt(PLOT_WIDTH, WIDTH);
    plotHeight = Prefs.getInt(PLOT_HEIGHT, HEIGHT);
  }

  public SpectraPlotWindow(ImagePlus imp, String title, String xLabel, String yLabel, float[] xValues, float[] yValues) {
    super(NewImage.createByteImage(title,
            WIDTH + LEFT_MARGIN + RIGHT_MARGIN, HEIGHT + TOP_MARGIN + BOTTOM_MARGIN,
            1, NewImage.FILL_WHITE));
    this.xLabel = xLabel;
    this.yLabel = yLabel;
    this.xValues = xValues;
    this.yValues = yValues;
    this.impOriginal = imp;
    double[] a = Tools.getMinMax(xValues);
    xMin = a[0];
    xMax = a[1];
    a = Tools.getMinMax(yValues);
    yMin = a[0];
    yMax = a[1];
    fixedYScale = false;
    nPoints = xValues.length;
  }

  /** This version of the constructor excepts double arrays. */
  public SpectraPlotWindow(ImagePlus imp, String title, String xLabel, String yLabel, double[] xValues, double[] yValues) {
    this(imp, title, xLabel, yLabel, Tools.toFloat(xValues), Tools.toFloat(yValues));
  }

  public void setLimits(double xMin, double xMax, double yMin, double yMax) {
    this.xMin = xMin;
    this.xMax = xMax;
    this.yMin = yMin;
    this.yMax = yMax;
    fixedYScale = true;
  }

  /** Adds a set of points to the plot.
   * @param x			the x-coodinates
   * @param y			the y-coodinates
   * @param shape		CIRCLE, X or LINE
   */
  public void addPoints(float[] x, float[] y, int shape) {
    setup();
    switch (shape) {
      case CIRCLE:
      case X:
        for (int i = 0; i < x.length; i++) {
          int xt = LEFT_MARGIN + (int) ((x[i] - xMin) * xScale);
          int yt = TOP_MARGIN + frameHeight - (int) ((y[i] - yMin) * yScale);
          drawShape(shape, xt, yt, 5);
        }
        break;
      case LINE:
        int xts[] = new int[x.length];
        int yts[] = new int[y.length];
        for (int i = 0; i < x.length; i++) {
          xts[i] = LEFT_MARGIN + (int) ((x[i] - xMin) * xScale);
          yts[i] = TOP_MARGIN + frameHeight - (int) ((y[i] - yMin) * yScale);
        }
        drawPolyline(ip, xts, yts, x.length);
        break;
    }
  }

  /** Adds a set of points to the plot using double arrays.
   Must be called before the plot is displayed. */
  public void addPoints(double[] x, double[] y, int shape) {
    addPoints(Tools.toFloat(x), Tools.toFloat(y), shape);
  }

  void drawShape(int shape, int x, int y, int size) {
    int xbase = x - size / 2;
    int ybase = y - size / 2;
    if (shape == X) {
      ip.drawLine(xbase, ybase, xbase + size, ybase + size);
      ip.drawLine(xbase + size, ybase, xbase, ybase + size);
    } else { // 5x5 oval
      ip.drawLine(x - 1, y - 2, x + 1, y - 2);
      ip.drawLine(x - 1, y + 2, x + 1, y + 2);
      ip.drawLine(x + 2, y + 1, x + 2, y - 1);
      ip.drawLine(x - 2, y + 1, x - 2, y - 1);
    }
  }

  /** Adds error bars to the plot. */
  public void addErrorBars(float[] errorBars) {
    if (errorBars.length != nPoints)
      throw new IllegalArgumentException("errorBars.length != npoints");
    this.errorBars = errorBars;
  }

  /** Draws a label. */
  public void addLabel(double x, double y, String label) {
    setup();
    int xt = LEFT_MARGIN + (int) (x * frameWidth);
    int yt = TOP_MARGIN + (int) (y * frameHeight);
    ip.drawString(label, xt, yt);
  }

  /** Changes the drawing color. The frame and labels are
   always drawn in black. */
  public void setColor(Color c) {
    setup();
    ip = ip.convertToRGB();
    ip.setColor(c);
  }

  /** Displays the plot. */
  public void draw() {
    Panel lower = new Panel();
    lower.setLayout(new BorderLayout(3, 3));
    Panel buttons = new Panel();
    buttons.setLayout(new FlowLayout(FlowLayout.RIGHT));
/*		list = new Button(" List ");
		list.addActionListener(this);
		buttons.add(list);*/
    calAnglesB = new Checkbox("Angles calibrated", MaudPreferences.getBoolean("anglesCalibration.imageToSpectra", false));
    buttons.add(calAnglesB);
    save = new Button("Save...");
    save.addActionListener(this);
    buttons.add(save);
/*		copy = new Button("Copy...");
		copy.addActionListener(this);
		buttons.add(copy);*/
    coordinates = new Label("                   ");
//		coordinates.setFont(new Font("Monospaced", Font.PLAIN, 12));
    buttons.add(coordinates);
    lower.add(buttons, BorderLayout.CENTER);
    add = new Button("Add to dataset and save...");
    add.addActionListener(this);
    buttons.add(add);

    buttons = new Panel();
    buttons.setLayout(new FlowLayout(FlowLayout.RIGHT));
    Label coord = new Label(" Starting: ");
//		coord.setFont(new Font("Monospaced", Font.PLAIN, 12));
    buttons.add(coord);
    minT = new TextField((new Double(xValues[0] * MoreMath.InchesTomm -
            MaudPreferences.getDouble("camera.startingPointX", -70.0))).toString());
    buttons.add(minT);
    coord = new Label(" Delta: ");
//		coord.setFont(new Font("Monospaced", Font.PLAIN, 12));
    buttons.add(coord);
    maxT = new TextField((new Double((xValues[nPoints - 1] - xValues[0]) * MoreMath.InchesTomm)).toString());
    buttons.add(maxT);
    coord = new Label(" Radius (mm, set 0 for deg coords): ");
//		coord.setFont(new Font("Monospaced", Font.PLAIN, 12));
    buttons.add(coord);
    rT = new TextField(Double.toString(AreaImage.getData().getInstrument().getAngularCalibration().getRadius()));
    buttons.add(rT);
    lower.add(buttons, BorderLayout.SOUTH);
    add(lower);

    drawPlot();
    pack();
    if (ip instanceof ColorProcessor)
      imp.setProcessor(null, ip);
    else
      imp.updateAndDraw();
  }

  void setup() {
    if (ip != null)
      return;
    ip = imp.getProcessor();
    ip.setColor(Color.black);
    if (Line.getWidth() > 3)
      ip.setLineWidth(1);
    ip.setFont(font);
    if (frameWidth == 0) {
      frameWidth = WIDTH;
      frameHeight = HEIGHT;
    }
    frame = new Rectangle(LEFT_MARGIN, TOP_MARGIN, frameWidth, frameHeight);
    if ((xMax - xMin) == 0.0)
      xScale = 1.0;
    else
      xScale = frame.width / (xMax - xMin);
    if ((yMax - yMin) == 0.0)
      yScale = 1.0;
    else
      yScale = frame.height / (yMax - yMin);
  }

  String d2s(double n) {
    if (Math.round(n) == n)
      return (IJ.d2s(n, 0));
    else
      return (IJ.d2s(n));
  }

  public void mouseMoved(int x, int y) {
    if (frame == null || coordinates == null)
      return;
    if (frame.contains(x, y)) {
      x -= frame.x;
      y -= frame.y;
      coordinates.setText("X=" + d2s(x / xScale + xMin) + ", Y=" + d2s((frameHeight - y) / yScale + yMin));
    } else
      coordinates.setText("");
  }

  void drawPlot() {
    int x, y;
    double v;

    setup();

    int xpoints[] = new int[nPoints];
    int ypoints[] = new int[nPoints];
    double value;
    for (int i = 0; i < nPoints; i++) {
      value = yValues[i];
      if (value < yMin) value = yMin;
      if (value > yMax) value = yMax;
      xpoints[i] = LEFT_MARGIN + (int) ((xValues[i] - xMin) * xScale);
      ypoints[i] = TOP_MARGIN + frame.height - (int) ((value - yMin) * yScale);
    }
    drawPolyline(ip, xpoints, ypoints, nPoints);

    if (this.errorBars != null) {
      xpoints = new int[2];
      ypoints = new int[2];
      for (int i = 0; i < nPoints; i++) {
        xpoints[0] = xpoints[1] = LEFT_MARGIN + (int) ((xValues[i] - xMin) * xScale);
        ypoints[0] = TOP_MARGIN + frame.height - (int) ((yValues[i] - yMin - errorBars[i]) * yScale);
        ypoints[1] = TOP_MARGIN + frame.height - (int) ((yValues[i] - yMin + errorBars[i]) * yScale);
        drawPolyline(ip, xpoints, ypoints, 2);
      }
    }

    if (ip instanceof ColorProcessor)
      ip.setColor(Color.black);
    ip.drawRect(frame.x, frame.y, frame.width + 1, frame.height + 1);
    String s = d2s(yMax);
    int sw = ip.getStringWidth(s);
    if ((sw + 4) > LEFT_MARGIN)
      ip.drawString(s, 4, TOP_MARGIN - 4);
    else
      ip.drawString(s, LEFT_MARGIN - ip.getStringWidth(s) - 4, TOP_MARGIN + 10);
    s = d2s(yMin);
    sw = ip.getStringWidth(s);
    if ((sw + 4) > LEFT_MARGIN)
      ip.drawString(s, 4, TOP_MARGIN + frame.height);
    else
      ip.drawString(s, LEFT_MARGIN - ip.getStringWidth(s) - 4, TOP_MARGIN + frame.height);
    x = LEFT_MARGIN;
    y = TOP_MARGIN + frame.height + 15;
    ip.drawString(d2s(xMin), x, y);
    s = d2s(xMax);
    ip.drawString(s, x + frame.width - ip.getStringWidth(s) + 6, y);
    ip.drawString(xLabel, LEFT_MARGIN + (frame.width - ip.getStringWidth(xLabel)) / 2, y + 3);
    drawYLabel(yLabel, LEFT_MARGIN, TOP_MARGIN, frame.height);
  }

  void drawPolyline(ImageProcessor ip, int[] x, int[] y, int n) {
    ip.moveTo(x[0], y[0]);
    for (int i = 0; i < n; i++)
      ip.lineTo(x[i], y[i]);
  }

  void drawYLabel(String yLabel, int x, int y, int height) {
    if (yLabel.equals(""))
      return;
    FontMetrics fm = ip.getFontMetrics();
    int w = fm.stringWidth(yLabel) + 5;
    int h = fm.getHeight() + 5;
    ImageProcessor label = new ByteProcessor(w, h);
    label.setColor(Color.white);
    label.fill();
    label.setColor(Color.black);
    label.setFont(font);
    label.setAntialiasedText(true);
    int descent = fm.getDescent();
    label.drawString(yLabel, 0, h - descent);
    label = label.rotateLeft();
    int y2 = y + (height - ip.getStringWidth(yLabel)) / 2;
    if (y2 < y) y2 = y;
    int x2 = x - h - 2;
    ip.insert(label, x2, y2);
  }

  void showList() {
    StringBuffer sb = new StringBuffer();
    String headings;
    if (errorBars != null) {
      if (saveXValues)
        headings = "X\tY\tErrorBar";
      else
        headings = "Y\tErrorBar";
      for (int i = 0; i < nPoints; i++) {
        if (saveXValues)
          sb.append(d2s(xValues[i]) + "\t" + d2s(yValues[i]) + "\t" + d2s(errorBars[i]) + "\n");
        else
          sb.append(d2s(yValues[i]) + "\t" + d2s(errorBars[i]) + "\n");
      }
    } else {
      if (saveXValues)
        headings = "X\tY";
      else
        headings = "Y";
      for (int i = 0; i < nPoints; i++) {
        if (saveXValues)
          sb.append(d2s(xValues[i]) + "\t" + d2s(yValues[i]) + "\n");
        else
          sb.append(d2s(yValues[i]) + "\n");
      }
    }
    TextWindow tw = new TextWindow("Plot Values", headings, sb.toString(), 200, 400);
    if (autoClose) {
      imp.changes = false;
      close();
    }
  }

  String saveAsText() {

    DataFileSet data = AreaImage.getData();
    boolean calibrated = calAnglesB.getState();
    MaudPreferences.setPref("anglesCalibration.imageToSpectra", calibrated);

    if (data == null)
      return null;

    String filename = Utility.openFileDialog(this, "Save as CIF...",
            FileDialog.SAVE, data.getFilePar().getDirectory(), null, "put a name.cif");
    if (filename == null)
      return null;

    String[] folderAndName = Misc.getFolderandName(filename);

    String folder = folderAndName[0];
    filename = folderAndName[1];

    if (!filename.endsWith(".cif"))
      filename = filename + ".cif";

    double xmin = 0.0;
    double xmax = Double.valueOf(maxT.getText()).doubleValue();
    double radius = Double.valueOf(rT.getText()).doubleValue();
    AreaImage.getData().getInstrument().getAngularCalibration().setRadius(rT.getText());
    if (calibrated && radius > 0) {
      xmin = Double.valueOf(minT.getText()).doubleValue();
      xmax = (xmax + xmin) / radius * 180.0 / Math.PI;
      xmin = xmin / radius * 180.0 / Math.PI;
    }
    double stepx = (xmax - xmin) / (nPoints - 1);

    if (filename != null) {

      IJ.wait(250);  // give system time to redraw ImageJ window
      IJ.showStatus("Saving plot values...");

      for (int i = 0; i < nPoints; i++)
        if (Double.isNaN(yValues[i]))
          yValues[i] = 0.0f;
      BufferedWriter output = Misc.getWriter(folder, filename);
      try {
        if (impOriginal != null) {
          if (impOriginal.getOriginalFileInfo() != null) {
            if (impOriginal.getOriginalFileInfo().description != null) {
              output.write("_pd_meas_special_details '" + impOriginal.getOriginalFileInfo().description + "'");
            } else {
              System.out.println("description");
            }
          } else {
            System.out.println("originalFileInfo");
            System.out.println("imp " + impOriginal);
          }
        } else {
          System.out.println("imp");
        }
        output.newLine();
        output.newLine();
        output.write("_pd_meas_number_of_points " + Integer.toString(nPoints));
        output.newLine();
        if (calibrated && (radius > 0 || xmax < 180.0))
          output.write("_riet_meas_datafile_calibrated true");
        else
          output.write("_riet_meas_datafile_calibrated false");
        output.newLine();
        output.newLine();
        output.write("loop_");
        output.newLine();
        output.write(DiffrDataFile.CIFXcoord2T);
        output.newLine();
        output.write(DiffrDataFile.intensityExpCIFstring);
        output.newLine();
        for (int i = 0; i < nPoints; i++) {
          output.write(" " + Fmt.format(xmin + stepx * i) + " " + Fmt.format(yValues[i]));
          output.newLine();
        }
      } catch (IOException io) {
      }
      try {
        output.close();
      } catch (IOException io) {
      }
    }
    if (autoClose) {
      imp.changes = false;
      close();
    }

    return folder + filename;

  }

  void copyToClipboard() {
    Clipboard systemClipboard = null;
    try {
      systemClipboard = getToolkit().getSystemClipboard();
    } catch (Exception e) {
      systemClipboard = null;
    }
    if (systemClipboard == null) {
      IJ.error("Unable to copy to Clipboard.");
      return;
    }
    IJ.showStatus("Copying plot values...");
    CharArrayWriter aw = new CharArrayWriter(nPoints * 4);
    PrintWriter pw = new PrintWriter(aw);
    for (int i = 0; i < nPoints; i++) {
      if (saveXValues)
        pw.print(d2s(xValues[i]) + "\t" + d2s(yValues[i]) + "\n");
      else
        pw.print(d2s(yValues[i]) + "\n");
    }
    String text = aw.toString();
    pw.close();
    StringSelection contents = new StringSelection(text);
    systemClipboard.setContents(contents, this);
    IJ.showStatus(text.length() + " characters copied to Clipboard");
    if (autoClose) {
      imp.changes = false;
      close();
    }
  }

  public void lostOwnership(Clipboard clipboard, Transferable contents) {
  }

  /** Called once when ImageJ quits. */
  public static void savePreferences(Properties prefs) {
    double min = ProfilePlot.getFixedMin();
    double max = ProfilePlot.getFixedMax();
    if (!(min == 0.0 && max == 0.0) && min < max) {
      prefs.put(MIN, Double.toString(min));
      prefs.put(MAX, Double.toString(max));
    }
    int options = 0;
    if (saveXValues)
      options |= SAVE_X_VALUES;
    if (autoClose)
      options |= AUTO_CLOSE;
    prefs.put(OPTIONS, Integer.toString(options));
  }

  public void actionPerformed(ActionEvent e) {
    Object b = e.getSource();
    if (b == list)
      showList();
    else if (b == save)
      saveAsText();
    else if (b == add) {
      String filename = saveAsText();
      IJ.wait(250);  // give system time to save the file
      DataFileSet data = AreaImage.getData();
      if (filename != null && data != null)
        data.addDataFileforName(filename, false);
    } else
      copyToClipboard();
  }

}
