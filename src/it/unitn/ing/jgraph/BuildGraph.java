package it.unitn.ing.jgraph;

import java.applet.Applet;
import java.awt.*;
import java.io.InputStream;
import java.net.URL;
import java.util.Stack;
import java.util.Vector;

/*
**************************************************************************
**
**    Class  BuildGraph
**
**************************************************************************
**    Copyright (C) 1996 Leigh Brookshaw
**
**    This program is free software; you can redistribute it and/or modify
**    it under the terms of the GNU General Public License as published by
**    the Free Software Foundation; either version 2 of the License, or
**    (at your option) any later version.
**
**    This program is distributed in the hope that it will be useful,
**    but WITHOUT ANY WARRANTY; without even the implied warranty of
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**    GNU General Public License for more details.
**
**    You should have received a copy of the GNU General Public License
**    along with this program; if not, write to the Free Software
**    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
**************************************************************************
**
**    This class will parse a description file and build the
**    plot based on the instructions in the file
**
*************************************************************************/

/**
 * This class will parse a description file and build a
 * plot based on the instructions in the file.
 * The build Graph class, with attached Axes and DataSets can be accessed
 * though methods in the class
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:51 $
 * @author Leigh Brookshaw
 */

public class BuildGraph extends ScanWord {

/*
**********************
**  Constants
*********************/

/*
**   Specifiy the integer values associated with keywords. Every integer
**   in this list is associated with keyword used in the input file
*/

  static final int BEGIN = 256;
  static final int GRAPH2D = 257;
  static final int MARKER = 258;
  static final int AXIS = 259;
  static final int URL = 260;
  static final int DATA = 261;
  static final int SIZE = 262;
  static final int COLOR = 263;
  static final int NAME = 264;
  static final int TITLE = 265;
  static final int LABEL = 266;
  static final int FONT = 267;
  static final int STYLE = 268;
  static final int BOTTOM = 269;
  static final int TOP = 270;
  static final int LEFT = 271;
  static final int RIGHT = 272;
  static final int END = 273;
  static final int G2DINT = 274;
  static final int OFF = 275;
  static final int ON = 276;
  static final int ATTACH = 277;
  static final int PLAIN = 278;
  static final int BOLD = 279;
  static final int ITALIC = 280;
  static final int DEBUG = 281;
  static final int GRID = 282;
  static final int ZERO = 283;

  static final int CONTOUR = 284;
  static final int LLEVELS = 285;
  static final int NUMBER = 286;
  static final int CDATA = 287;
  static final int FUNCTION = 288;
  static final int XRANGE = 289;
  static final int YRANGE = 290;
  static final int BACKGROUND = 291;
  static final int LOGCONTOURS = 292;
  static final int CLABELS = 293;
  static final int SQUARE = 294;


/*
**    Specify Internal interger values. These constants are associated
**    with objects nolonger in the context of the input file. But have
**    been constructed by the program from a number of keyword commands.
*/
  static final int MARKER_STYLE = 512;
  static final int MARKER_COLOR = 513;
  static final int MARKER_SIZE = 514;
  static final int LABEL_COLOR = 515;
  static final int LABEL_FONT = 517;
  static final int GRID_COLOR = 520;
  static final int GRID_OFF = 521;
  static final int ZERO_COLOR = 522;
  static final int ZERO_OFF = 523;
  static final int MARKER_URL = 524;
  static final int GRID_ON = 525;
  static final int ZERO_ON = 526;
  static final int XMIN = 527;
  static final int XMAX = 528;
  static final int YMIN = 529;
  static final int YMAX = 530;
  static final int XNUMBER = 531;
  static final int YNUMBER = 532;
  static final int LABEL_OFF = 533;

  /**
   * The BUILD stack. Everything pushed on this stack has to be
   * built from other objects. An example would be a Font, which
   * is constructed from Logical name, style and size.
   */
  protected Stack build = new Stack();
  /**
   *   The Object stack. All input tokens get pushed onto and
   *    popped off this stack.
   */
  protected Stack object = new Stack();
  /**
   * The class vector. This contains final class objects that cannot be
   * deleted. That is
   * we don't want these classes unnattached and collected into the garbage.
   * The obvious classes here are the main Graph2D class, the Axes of the plot
   * DataSets etc.
   */
  protected Vector built = new Vector();
  /**
   * The constructed Graph Object
   */
  protected Object graph = null;

  /**
   *   The TextLine containing the title
   */
  protected TextLine graphtitle = null;

  /**
   *   The Calling Applet
   */
  protected Applet applet;

  /**
   * Debug. If true output copious debug messages. Though unless you know
   * what is going on with the code most messages will probably be
   * meaningless
   */
  protected boolean debug = false;

  /*
  ** This vector contains all the datasets as they are built.
  ** the data sets in this lists are then searched so that they can
  ** be attached to axis.
  */
  private Vector datasets = new Vector();

/*
*********************
** Public Variables
********************/
  /**
   * Current line being parsed
   */
  public int lineno = 1;
  /**
   * The current brace level. At the end of the input if all
   * braces are matched the level should be back to zero.
   */
  public int level = 0;


/*
*********************
** Constructors
*******************/


  /**
   * Instantiate the class
   * @param in The inputstream to be read
   * @param ap the driving applet.
   */

  public BuildGraph(InputStream in, Applet ap) {
    super(in);
    applet = ap;
/*
**   Specifiy The Key Words and associate them with the integer Values
*/

    addKeyWord("{", BEGIN);
    addKeyWord("}", END);
    addKeyWord("graph2d", GRAPH2D);
    addKeyWord("g2dint", G2DINT);
    addKeyWord("marker", MARKER);
    addKeyWord("axis", AXIS);
    addKeyWord("url", URL);
    addKeyWord("data", DATA);
    addKeyWord("size", SIZE);
    addKeyWord("color", COLOR);
    addKeyWord("name", NAME);
    addKeyWord("title", TITLE);
    addKeyWord("label", LABEL);
    addKeyWord("font", FONT);
    addKeyWord("style", STYLE);
    addKeyWord("bottom", BOTTOM);
    addKeyWord("top", TOP);
    addKeyWord("left", LEFT);
    addKeyWord("right", RIGHT);
    addKeyWord("on", ON);
    addKeyWord("off", OFF);
    addKeyWord("attach", ATTACH);
    addKeyWord("plain", PLAIN);
    addKeyWord("bold", BOLD);
    addKeyWord("italic", ITALIC);
    addKeyWord("debug", DEBUG);
    addKeyWord("grid", GRID);
    addKeyWord("zero", ZERO);
    addKeyWord("contour", CONTOUR);
    addKeyWord("number", NUMBER);
    addKeyWord("cdata", CDATA);
    addKeyWord("function", FUNCTION);
    addKeyWord("xrange", XRANGE);
    addKeyWord("yrange", YRANGE);
    addKeyWord("background", BACKGROUND);
    addKeyWord("llevels", LLEVELS);
    addKeyWord("square", SQUARE);
    addKeyWord("log", LOGCONTOURS);


  }

  /********************
   **
   **  Public Methods
   **
   ********************/

  /**
   * Get the Graph object that has been built!
   */
  public Object getGraph() {
    return graph;
  }

  /**
   * Get the title of the Graph
   */
  public TextLine getGraphTitle() {
    return graphtitle;
  }

  /**
   *  Get a vector of all the built Classes.
   *  Ie the DataSets, Axis, Graph etc.
   *  This can be used by an applet to override anything specified in the
   *       input file
   */
  public Vector getBuilt() {
    return built;
  }


  /**
   * This is THE method that parses the input file and constructs the plot.
   */
  public void parse() {
    int token;

    NamedObject nobj;
    boolean cont = true;


    while (cont) {
/*
**           Grab a word from the stream and Do something with it!
*/
      token = nextWord();

      debugMessage("Main", token);

      switch (token) {
/*
**                         Turn on the debug messages.
*/
        case DEBUG:
          debug = !debug;
          break;
/*
**                         Instantiate the Graph class and push it onto
**                         the object stack!
*/
        case CONTOUR:
          if (graph != null) {
            errorAtLine("Graph already defined.");
            return;
          }
          graph = new Contour();
          nobj = new NamedObject(graph, CONTOUR);
          object.push(nobj);
          break;
        case G2DINT:
          if (graph != null) {
            errorAtLine("Graph already defined.");
            return;
          }
          graph = new G2Dint();
          nobj = new NamedObject(graph, G2DINT);
          object.push(nobj);
          break;
        case GRAPH2D:
          if (graph != null) {
            errorAtLine("Graph already defined.");
            return;
          }
          graph = new Graph2D();
          nobj = new NamedObject(graph, GRAPH2D);
          object.push(nobj);
          break;

/*
**                         End of the file. Exit the loop
*/
        case TT_EOF:
          cont = false;
          break;
/*
**                         End of Line - Increment the line counter
*/
        case TT_EOL:
          lineno++;
          if (lineno == (lineno / 10) * 10) {
            applet.showStatus("Reading input: Line " + lineno);
          }
          break;
/*
**  LEFT, RIGHT, BOTTOM and TOP perform different tasks based on Context.
**  Inside an AXIS group they position the axis and are standalone keywords
**  Inside a GRAPH context they are followed by an integer and are the
**  border widths in pixels.
*/
        case LEFT:
        case RIGHT:
        case TOP:
        case BOTTOM:
          if (isContext(AXIS)) {
            nobj = new NamedObject(token);
          } else {
            int toke = nextWord();
            if (toke == TT_NUMBER) {
              nobj = new NamedObject(
                      new Integer((int) (nval + 0.01)), token);
            } else {
              errorAtLine(
                      "In this context LEFT, RIGHT, TOP or BOTTOM should be followed an integer");
              return;
            }
          }

          object.push(nobj);
          break;
/*
**                         Build a URL object and push it onto the object stack
**                         The URL keyword MUST be followed by a string
*/
        case URL:
          token = nextWord();
          if (token == STRING) {
            try {
              URL url = new URL(applet.getDocumentBase(), sval);
              nobj = new NamedObject(url, URL);
            } catch (Exception e) {
              errorAtLine("Failed to build URL!");
              return;
            }
          } else {
            errorAtLine("URL should be followed by a string");
            return;
          }
          object.push(nobj);
          break;
/*
**                         The ATTACH keyword is followed by the name of the
**                         DataSet to be attached. Attach only appears
**                         inside an AXIS group.
*/
        case ATTACH:
          if (!isContext(AXIS)) {
            errorAtLine("ATTACH should only appear in AXIS context");
            return;
          }

          token = nextWord();
          if (token == STRING) {
            nobj = new NamedObject(sval, ATTACH);
          } else {
            errorAtLine("ATTACH should be followed by a string");
            return;
          }
          object.push(nobj);
          break;
/*
**                         Marker style, or font style depending on context
*/
        case STYLE:
          if (!isContext(MARKER) && !isContext(FONT)) {
            errorAtLine(
                    "STYLE should only appear in MARKER or FONT context");
            return;
          }
          token = nextWord();
          if (token == PLAIN) {
            nobj = new NamedObject(PLAIN);
          } else if (token == BOLD) {
            nobj = new NamedObject(BOLD);
          } else if (token == ITALIC) {
            nobj = new NamedObject(PLAIN);
          } else if (token == TT_NUMBER) {
            nobj = new NamedObject(new Integer((int) (nval + 0.01)), STYLE);
          } else {
            errorAtLine(
                    "STYLE should be followed an integer or Keyword");
            return;
          }
          object.push(nobj);
          break;
/*
**                         Size of font or marker. Size should be followed by an integer
*/
        case SIZE:
          if (!isContext(MARKER) && !isContext(FONT)) {
            errorAtLine(
                    "SIZE should only appear in MARKER or FONT context");
            return;
          }
          token = nextWord();
          if (token == TT_NUMBER) {
            nobj = new NamedObject(new Integer((int) (nval + 0.01)), SIZE);
          } else {
            errorAtLine("SIZE should be followed by an integer");
            return;
          }
          object.push(nobj);
          break;
          /*
          ** Which contour levels are to get labels.
          */
        case LLEVELS:
          if (!isContext(CONTOUR)) {
            errorAtLine(
                    "LLEVELS should only appear in CONTOUR context");
            return;
          }
          token = nextWord();
          if (token == TT_NUMBER) {
            nobj = new NamedObject(new Integer((int) (nval + 0.01)), LLEVELS);
          } else {
            errorAtLine("LLEVELS should be followed by an integer");
            return;
          }
          object.push(nobj);
          break;
/*
**                         Parse a color. Color should be followed by 3 integers in
**                         the range 0-255
*/
        case COLOR:
        case BACKGROUND:
          int i;
          int rgb[] = {-1, -1, -1};
          for (i = 0; i < 3; i++) {
            int t = nextWord();
            if (t != TT_NUMBER || nval < 0 || nval > 255) {
              errorAtLine("Incorrect Color definition");
              return;
            }
            rgb[i] = (int) (nval + 0.001);
          }
          if (token == COLOR) {
            nobj = new NamedObject(new Color(rgb[0], rgb[1], rgb[2]), COLOR);
            object.push(nobj);
          } else {
            if (isContext(DATA) || isContext(CDATA)) {
              ((Graph2D) graph).setDataBackground(
                      new Color(rgb[0], rgb[1], rgb[2]));
            } else {
              ((Graph2D) graph).setGraphBackground(
                      new Color(rgb[0], rgb[1], rgb[2]));
            }
          }
          break;
/*
**                         Name the current context or the string to be used as a label
**                         or title. Name should be followed by a string
*/
        case NAME:
          token = nextWord();
          if (token == STRING) {
            nobj = new NamedObject(sval, NAME);
          } else {
            errorAtLine("NAME should be followed by a string");
            return;
          }
          object.push(nobj);
          break;
        case FUNCTION:
          token = nextWord();
          if (token == STRING) {
            nobj = new NamedObject(sval, FUNCTION);
          } else {
            errorAtLine("FUNCTION should be followed by a string");
            return;
          }
          object.push(nobj);
          break;
        case NUMBER:
          token = nextWord();
          if (token == TT_NUMBER) {
            nobj = new NamedObject(
                    new Integer((int) (nval + 0.01)), NUMBER);
          } else {
            errorAtLine("NUMBER should be followed by an integer");
            return;
          }
          object.push(nobj);
          break;
        case XRANGE:
        case YRANGE:
          boolean error = false;
          double min = 0.0;
          double max = 0.0;
          int num = 0;
          int t = nextWord();
          if (t == TT_NUMBER) {
            num = (int) (nval + 0.001);
            t = nextWord();
            if (t == TT_NUMBER) {
              min = nval;
              t = nextWord();
              if (t == TT_NUMBER) {
                max = nval;
              } else {
                error = true;
              }
            } else {
              error = true;
            }
          } else {
            error = true;
          }

          if (error) {
            errorAtLine(
                    "Range limits must be followed by 3 numbers");
            return;
          }
          if (min == max) {
            errorAtLine(
                    "Range limits must not be the same");
            return;
          }
          if (num <= 1) {
            errorAtLine(
                    "Number of points must be greater than 1");
            return;
          }


          if (min > max) {
            double tmp = max;
            max = min;
            min = tmp;
          }

          if (token == XRANGE) {
            nobj = new NamedObject(new Integer(num), XNUMBER);
            object.push(nobj);
            nobj = new NamedObject(new Double(min), XMIN);
            object.push(nobj);
            nobj = new NamedObject(new Double(max), XMAX);
            object.push(nobj);
          } else {
            nobj = new NamedObject(new Integer(num), YNUMBER);
            object.push(nobj);
            nobj = new NamedObject(new Double(min), YMIN);
            object.push(nobj);
            nobj = new NamedObject(new Double(max), YMAX);
            object.push(nobj);
          }
          break;

/*
**                         unknown keyword!
*/
        case UNKNOWN:
          errorAtLine("Unknown keyword!!");
          break;
/*
**                         begin a new group or context. The object to be built
**                         should be the last thing pushed onto the object stack.
**                         Move it to the build stack and push BEGIN onto the object
**                         stack to act as a delimiter on the stack
*/
        case BEGIN:
          level++;
          build.push(object.pop());
          object.push(new NamedObject(BEGIN));
          break;
/*
**                         End of a group or context. Everything on the object stack
**                         down to the BEGIN object relate to the current build object
**                         Build the object and push this built object onto the
**                         object stack.
*/
        case END:
          level--;
          build((NamedObject) (build.pop()));
          break;
/*
**                         Whatever it is push it onto the object stack.
*/
        default:
          object.push(new NamedObject(token));
          break;
      }

    }
/*
**        Finished - lets see if anything major was not defined
*/
    closeStream();

    if (graph == null) {
      Graph2D.out.println("Graph never defined! Cannot build graph!");
      return;
    }

    if (level != 0) {
      Graph2D.out.println("Mismatched Braces!! Will try and build graph.");
    }
/*
**        Errors could have occured - but lets try and repaint and keep going
*/

    if (graph instanceof Graph2D) {
      ((Graph2D) graph).repaint();
    } else if (graph instanceof G2Dint) {
      ((G2Dint) graph).repaint();
    } else if (graph instanceof Contour) {
      ((Contour) graph).repaint();
    } else {
      Graph2D.out.println("Unknown graph type - totally confused!");
    }

  }


/*
*********************
**
** Protected Methods
**
*********************/

  /**
   * Check if the object is on the "to be built" stack. This way context can be
   *   checked so potential errors flagged.
   * @param token is this the object we are currently building?
   * @return <i>true</i> if the token matches current object being built
   */
  protected boolean isContext(int token) {
    int i;

    for (i = 0; i < build.size(); i++) {
      if (((NamedObject) (build.elementAt(i))).id == token)
        return true;
    }

    return false;
  }

  /**
   *  Based on the parsed object build something.
   * @param nobj The object to be built
   * @return <i>true</i> if the build was successful.
   */
  protected boolean build(NamedObject nobj) {

    switch (nobj.id) {

      case FONT:
        return buildFont();
      case MARKER:
        return buildMarker();
      case TITLE:
        return buildTitle();
      case LABEL:
        return buildLabel();
      case DATA:
      case CDATA:
        return buildData(nobj.id);
      case AXIS:
        return buildAxis();
      case GRID:
        return buildGrid();
      case ZERO:
        return buildZero();
      case G2DINT:
      case GRAPH2D:
      case CONTOUR:
        return buildGraph(nobj.id);
      default:
        errorAtLine("Incorrect keyword followed by braces");
        return false;
    }
  }

  /**
   * Pop things off the object stack and build the graph
   * @param type Type of graph to build, Graph2D, G2Dint, Contour.
   * @return <i>true</i> if the build was successful
   */
  protected boolean buildGraph(int type) {
    NamedObject nobj;
    Axis axis;
    String dname;

    if (type == CONTOUR)
      dname = "Contour";
    else if (type == G2DINT)
      dname = "G2Dint";
    else
      dname = "Graph";


    while (true) {
      nobj = (NamedObject) (object.pop());

      debugMessage(dname, nobj.id);


      switch (nobj.id) {

        case AXIS:
          axis = (Axis) (nobj.getObject());
          if (type == GRAPH2D) {
            ((Graph2D) graph).attachAxis(axis);
          } else {
            ((G2Dint) graph).attachAxis(axis);
          }
          break;
        case DATA:
          ((Graph2D) graph).attachDataSet(
                  (DataSet) (nobj.getObject()));
          break;
        case ATTACH:
          axis = (Axis) (nobj.getObject());
          String name = nobj.getName();
          boolean attach = false;
          for (int i = 0; i < datasets.size(); i++) {
            nobj = (NamedObject) (datasets.elementAt(i));
            if (name.equals(nobj.getName())) {
              axis.attachDataSet((DataSet) (nobj.getObject()));
              attach = true;
              break;
            }
          }

          if (!attach) {
            errorAtLine("Data name not found for Attach keyword");
          }
          break;
        case GRID_COLOR:
          ((Graph2D) graph).gridcolor = (Color) (nobj.getObject());
          break;
        case GRID_OFF:
          ((Graph2D) graph).drawgrid = false;
          break;
        case GRID_ON:
          ((Graph2D) graph).drawgrid = true;
          break;
        case ZERO_COLOR:
          ((Graph2D) graph).zerocolor = (Color) (nobj.getObject());
          break;
        case ZERO_OFF:
          ((Graph2D) graph).drawzero = false;
          break;
        case ZERO_ON:
          ((Graph2D) graph).drawzero = true;
          break;
        case MARKER:
          ((Graph2D) graph).setMarkers(
                  (Markers) (nobj.getObject()));
          break;
        case TITLE:
          graphtitle = (TextLine) (nobj.getObject());
          built.addElement(graphtitle);
          break;
        case LEFT:
          ((Graph2D) graph).borderLeft =
                  ((Integer) (nobj.getObject())).intValue();
          break;
        case RIGHT:
          ((Graph2D) graph).borderRight =
                  ((Integer) (nobj.getObject())).intValue();
          break;
        case TOP:
          ((Graph2D) graph).borderTop =
                  ((Integer) (nobj.getObject())).intValue();
          break;
        case BOTTOM:
          ((Graph2D) graph).borderBottom =
                  ((Integer) (nobj.getObject())).intValue();
          break;
        case SQUARE:
          ((Graph2D) graph).square = true;
          break;
        case BEGIN:
          built.addElement(graph);
          applet.showStatus("BuildGraph: Built Graph!!!");

          return true;
        default:
          if (type == CONTOUR) {
            if (!buildContour(nobj)) {
              errorAtLine(
                      "Incorrect keyword in Contour specification");
              return false;
            } else {
              return true;
            }
          } else {
            errorAtLine("Incorrect keyword in Graph specification");
            return false;
          }
      }
    }
  }

  /**
   * @return <i>true</i> if the build was successful
   */

  protected boolean buildContour(NamedObject nobj) {

    switch (nobj.id) {

      case NUMBER:
        ((Contour) graph).setNLevels(
                ((Integer) (nobj.getObject())).intValue());
        break;

      case LLEVELS:
        ((Contour) graph).setLabelLevels(
                ((Integer) (nobj.getObject())).intValue());
        break;
      case COLOR:
        ((Contour) graph).setContourColor(
                (Color) (nobj.getObject()));
        break;
      case LOGCONTOURS:
        ((Contour) graph).logLevels = true;
        break;
      default:
        return false;
    }

    return true;
  }

  /**
   * @return <i>true</i> if the build was successful
   */

  protected boolean buildFont() {
    String name = "TimesRoman";
    int style = Font.PLAIN;
    int size = 20;
    NamedObject nobj;
    Font f;

    while (true) {
      nobj = (NamedObject) (object.pop());

      debugMessage("Font", nobj.id);

      switch (nobj.id) {

        case NAME:
          name = (String) (nobj.getObject());
          break;
        case SIZE:
          size = ((Integer) (nobj.getObject())).intValue();
          break;
        case PLAIN:
          style += Font.PLAIN;
          break;
        case BOLD:
          style += Font.BOLD;
          break;
        case ITALIC:
          style += Font.ITALIC;
          break;
        case BEGIN:
          try {
            f = new Font(name, style, size);
            nobj = new NamedObject(f, FONT);
            object.push(nobj);
            applet.showStatus("BuildGraph: Built Font!");
            return true;
          } catch (Exception e) {
            errorAtLine("Ill formed font specification");
            return false;
          }
        default:
          errorAtLine("Incorrect keyword in Font specification");
          return false;
      }
    }
  }

  /**
   * @return <i>true</i> if the build was successful
   */
  protected boolean buildTitle() {
    NamedObject nobj;

    TextLine title = new RTextLine();

    while (true) {
      nobj = (NamedObject) (object.pop());

      debugMessage("Title", nobj.id);

      switch (nobj.id) {

        case NAME:
          title.setText((String) (nobj.getObject()));
          break;
        case FONT:
          title.setFont((Font) (nobj.getObject()));
          break;
        case COLOR:
          title.setColor((Color) (nobj.getObject()));
          break;
        case BEGIN:
          nobj = new NamedObject(title, TITLE);
          object.push(nobj);
          applet.showStatus("BuildGraph: Built Title!");
          return true;
        default:
          errorAtLine("Incorrect keyword in Title specification");
          return false;
      }
    }
  }

  /**
   * @return <i>true</i> if the build was successful
   */
  protected boolean buildLabel() {
    NamedObject nobj;
    NamedObject color = null;
    NamedObject font = null;

    while (true) {
      nobj = (NamedObject) (object.pop());

      debugMessage("Label", nobj.id);

      switch (nobj.id) {

        case FONT:
          font = nobj;
          font.setId(LABEL_FONT);
          break;
        case COLOR:
          color = nobj;
          color.setId(LABEL_COLOR);
          break;
        case BEGIN:
          if (color != null) object.push(color);
          if (font != null) object.push(font);
          applet.showStatus("BuildGraph: Built Axis Label!");
          return true;
        default:
          errorAtLine("Incorrect keyword in Label specification");
          return false;
      }
    }
  }

  /*
   * @return <i>true</i> if the build was successful
   */
  protected boolean buildMarker() {
    NamedObject nobj;
    Markers m = null;
    NamedObject size = null;
    NamedObject color = null;
    NamedObject style = null;


    while (true) {
      nobj = (NamedObject) (object.pop());

      debugMessage("Marker", nobj.id);

      switch (nobj.id) {

        case SIZE:
          size = nobj;
          size.setId(MARKER_SIZE);
          break;
        case COLOR:
          color = nobj;
          color.setId(MARKER_COLOR);
          break;
        case STYLE:
          style = nobj;
          if (style.getObject() instanceof Integer) {
            style.setId(MARKER_STYLE);
          } else {
            errorAtLine("Style should be an Integer in MARKER");
            return false;
          }
          break;
/*        case URL:
          try {
            m = new Markers((URL) nobj.getObject());
          } catch (Exception e) {
            errorAtLine("Failed to load markers: " +
                    e.getMessage());
          }
          break;*/
        case BEGIN:
          if (m != null) {
            nobj = new NamedObject(m, MARKER);
            object.push(nobj);
            built.addElement(m);
            applet.showStatus(
                    "BuildGraph: Loaded Marker file!");
            return true;
          } else {
            if (color != null) object.push(color);
            if (size != null) object.push(size);
            if (style != null) object.push(style);
            applet.showStatus("BuildGraph: Built Marker!");
            return true;
          }
        default:
          errorAtLine("Incorrect keyword in MARKER specification");
          return false;
      }
    }
  }

  /**
   * @return <i>true</i> if the build was successful
   */
  protected boolean buildData(int type) {
    NamedObject nobj;
    DataSet data = new DataSet();
    NamedObject ndata = new NamedObject(data, DATA);
    ParseFunction parsef = null;
    LoadData load = null;
    double xmin = 0.0;
    double xmax = 0.0;
    double ymin = 0.0;
    double ymax = 0.0;

    int nx = 0;
    int ny = 0;

    if (type == CDATA && !(graph instanceof Contour)) {
      errorAtLine("CDATA can only be used in CONTOUR!");
    }

    while (true) {
      nobj = (NamedObject) (object.pop());

      debugMessage("Data", nobj.id);

      switch (nobj.id) {

        case NAME:
          ndata.setName((String) (nobj.getObject()));
          break;
        case MARKER_STYLE:
          data.marker = ((Integer) (nobj.getObject())).intValue();
          break;
        case MARKER_SIZE:
          data.markerscale = (double) (((Integer) (nobj.getObject())).intValue());
          break;
        case MARKER_COLOR:
          data.markercolor = (Color) (nobj.getObject());
          break;
        case OFF:
          data.linestyle = 0;
          break;
        case ON:
          data.linestyle = 1;
          break;
        case COLOR:
          if (type == CDATA) {
            ((Contour) graph).setContourColor(
                    (Color) (nobj.getObject()));
          } else {
            data.linecolor = (Color) (nobj.getObject());
          }
          break;
        case URL:
          load = new LoadData(data);
          load.loadDataSet((URL) (nobj.getObject()), graph);
          break;
        case FUNCTION:
          parsef = new ParseFunction((String) (nobj.getObject()));
          if (!parsef.parse()) {
            errorAtLine("Error in function definition!");
            return false;
          }
          break;
        case XNUMBER:
          nx = ((Integer) (nobj.getObject())).intValue();
          break;
        case XMIN:
          xmin = ((Double) (nobj.getObject())).doubleValue();
          break;
        case XMAX:
          xmax = ((Double) (nobj.getObject())).doubleValue();
          break;
        case YNUMBER:
          ny = ((Integer) (nobj.getObject())).intValue();
          break;
        case YMIN:
          ymin = ((Double) (nobj.getObject())).doubleValue();
          break;
        case YMAX:
          ymax = ((Double) (nobj.getObject())).doubleValue();
          break;
        case BEGIN:
          if (parsef != null && type == CDATA) {
            if (!(graph instanceof Contour)) {
              errorAtLine("CDATA should only be used for Contours!");
              return false;
            }
            double array[] = arrayFromFunction(parsef, nx, xmin, xmax,
                    ny, ymin, ymax);
            if (array == null) {
              errorAtLine("Unable to build Data from Function");
              return false;
            }
            ((Contour) graph).setRange(xmin, xmax, ymin, ymax);
            ((Contour) graph).setGrid(array, nx, ny);

            return true;

          } else if (parsef != null) {
            double array[] = arrayFromFunction(parsef, nx, xmin, xmax);
            if (array == null) {
              errorAtLine("Unable to build Data from Function");
              return false;
            }
            try {
              data.append(array, nx);
              object.push(ndata);
              datasets.addElement(ndata);
              built.addElement(data);
              return true;
            } catch (Exception e) {
              return false;
            }
          } else if (load == null) {
            errorAtLine("No URL has been defined for Data!");
            return false;
          }

          object.push(ndata);
          built.addElement(data);
          built.addElement(load);
          datasets.addElement(ndata);
          applet.showStatus("BuildGraph: Loading Data!");
          return true;

        default:
          errorAtLine("Incorrect keyword in Data specification");
          return false;
      }
    }
  }

  protected double[] arrayFromFunction(ParseFunction f, int nx,
                                       double xmin, double xmax) {

    double x;
    double y;
    double array[] = new double[2 * nx];
    double xinc = (xmax - xmin) / (nx - 1);
    int count = 0;


    for (int i = 0; i < nx; i++) {
      x = xmin + i * xinc;
      try {
        y = f.getResult(x);
        array[count++] = x;
        array[count++] = y;
      } catch (Exception e) {
      }
    }

    if (count == 0) return null;

    return array;
  }

  protected double[] arrayFromFunction(ParseFunction f,
                                       int nx, double xmin, double xmax,
                                       int ny, double ymin, double ymax) {

    double x;
    double y;
    double array[] = new double[ny * nx];
    double xinc = (xmax - xmin) / (nx - 1);
    double yinc = (ymax - ymin) / (ny - 1);

    int count = 0;

    for (int j = 0; j < ny; j++) {
      y = ymin + j * yinc;
      for (int i = 0; i < nx; i++) {
        x = xmin + i * xinc;
        try {
          array[count++] = f.getResult(x, y);
        } catch (Exception e) {
          array[count++] = 0.0;
        }
      }

    }

    if (count == 0) return null;

    return array;


  }

  /**
   * @return <i>true</i> if the build was successful
   */
  protected boolean buildAxis() {
    Stack attach = new Stack();
    NamedObject nobj;
    Axis axis = new Axis();
    NamedObject naxis = new NamedObject(axis, AXIS);
    int angle = 0;

    while (true) {
      nobj = (NamedObject) (object.pop());

      debugMessage("Axis", nobj.id);

      switch (nobj.id) {

        case NAME:
          naxis.setName((String) (nobj.getObject()));
          break;
        case TITLE:
          axis.title = (RTextLine) (nobj.getObject());
          break;
        case LABEL_FONT:
          axis.label.setFont((Font) (nobj.getObject()));
          break;
        case LABEL_COLOR:
          axis.label.setColor((Color) (nobj.getObject()));
          break;
        case BOTTOM:
          axis.setPosition(Axis.BOTTOM);
          break;
        case TOP:
          axis.setPosition(Axis.TOP);
          break;
        case LEFT:
          axis.setPosition(Axis.LEFT);
          angle = 90;
          break;
        case RIGHT:
          axis.setPosition(Axis.RIGHT);
          angle = -90;
          break;
        case ATTACH:
          attach.push(new NamedObject(axis,
                  (String) (nobj.getObject()), ATTACH));
          break;
        case BEGIN:
          axis.g2d = (Graph2D) graph;
          axis.setTitleRotation(angle);
          object.push(naxis);
          while (attach.size() > 0) {
            object.push(attach.pop());
          }
          built.addElement(axis);
          applet.showStatus("BuildGraph: Built Axis!");
          return true;

        default:
          errorAtLine("Incorrect keyword in Data specification");
          return false;
      }
    }
  }

  /**
   * @return <i>true</i> if the build was successful
   */
  protected boolean buildGrid() {
    NamedObject nobj;
    NamedObject color = null;
    NamedObject grid = null;


    while (true) {
      nobj = (NamedObject) (object.pop());

      debugMessage("Grid", nobj.id);

      switch (nobj.id) {

        case OFF:
          grid = nobj;
          grid.setId(GRID_OFF);
          break;
        case ON:
          grid = nobj;
          grid.setId(GRID_ON);
          break;
        case COLOR:
          color = nobj;
          color.setId(GRID_COLOR);
          break;
        case BEGIN:
          if (grid != null) object.push(grid);
          if (color != null) object.push(color);
          return true;

        default:
          errorAtLine("Incorrect keyword in Grid specification");
          return false;
      }
    }
  }

  /**
   * @return <i>true</i> if the build was successful
   */
  protected boolean buildZero() {
    NamedObject nobj;
    NamedObject color = null;
    NamedObject zero = null;


    while (true) {
      nobj = (NamedObject) (object.pop());

      debugMessage("Zero", nobj.id);

      switch (nobj.id) {

        case OFF:
          zero = nobj;
          zero.setId(ZERO_OFF);
          break;
        case ON:
          zero = nobj;
          zero.setId(ZERO_ON);
          break;
        case COLOR:
          color = nobj;
          color.setId(ZERO_COLOR);
          break;
        case BEGIN:
          if (zero != null) object.push(zero);
          if (color != null) object.push(color);
          return true;

        default:
          errorAtLine("Incorrect keyword in Zero specification");
          return false;
      }
    }
  }

  /**
   * output the parsed error message with the line number of the line
   * that triggered the error.
   * @param s Message to ouput
   */

  private void errorAtLine(String s) {
    Graph2D.out.println("Error at line " + lineno + ": " + s);
    applet.showStatus("Error at line " + lineno + ": " + s);
  }

  /**
   * output the parsed debug message, appending line number, and token string
   * @param s Message to ouput
   * @param token token to be replaced with a string
   */

  private void debugMessage(String s, int token) {
    String st = null;

    if (!debug) return;

    switch (token) {
      case DEBUG:
        st = null;
        break;
      case G2DINT:
        st = "G2DINT";
        break;
      case GRAPH2D:
        st = "GRAPH2D";
        break;
      case CONTOUR:
        st = "CONTOUR";
        break;
      case TT_EOF:
        st = null;
        break;
      case TT_EOL:
        st = null;
        break;
      case URL:
        st = "URL";
        break;
      case ATTACH:
        st = "ATTACH";
        break;
      case STYLE:
        st = "STYLE";
        break;
      case SIZE:
        st = "SIZE";
        break;
      case COLOR:
        st = "COLOR";
        break;
      case NAME:
        st = "NAME";
        break;
      case BEGIN:
        st = "BEGIN";
        break;
      case END:
        st = "END";
        break;
      case MARKER:
        st = "MARKER";
        break;
      case AXIS:
        st = "AXIS";
        break;
      case TITLE:
        st = "TITLE";
        break;
      case DATA:
        st = "DATA";
        break;
      case LABEL:
        st = "LABEL";
        break;
      case FONT:
        st = "FONT";
        break;
      case BOTTOM:
        st = "BOTTOM";
        break;
      case TOP:
        st = "TOP";
        break;
      case LEFT:
        st = "LEFT";
        break;
      case RIGHT:
        st = "RIGHT";
        break;
      case ON:
        st = "ON";
        break;
      case OFF:
        st = "OFF";
        break;
      case PLAIN:
        st = "PLAIN";
        break;
      case BOLD:
        st = "BOLD";
        break;
      case ITALIC:
        st = "ITALIC";
        break;
      case MARKER_STYLE:
        st = "MARKER_STYLE";
        break;
      case MARKER_COLOR:
        st = "MARKER_COLOR";
        break;
      case MARKER_SIZE:
        st = "MARKER_SIZE";
        break;
      case LABEL_COLOR:
        st = "LABEL_COLOR";
        break;
      case LABEL_FONT:
        st = "LABEL_FONT";
        break;
      case GRID_COLOR:
        st = "GRID_COLOR";
        break;
      case GRID:
        st = "GRID";
        break;
      case GRID_OFF:
        st = "GRID_OFF";
        break;
      case GRID_ON:
        st = "GRID_ON";
        break;
      case ZERO_COLOR:
        st = "ZERO_COLOR";
        break;
      case ZERO:
        st = "ZERO";
        break;
      case ZERO_OFF:
        st = "ZERO_OFF";
        break;
      case ZERO_ON:
        st = "ZERO_ON";
        break;
      case MARKER_URL:
        st = "MARKER_URL";
        break;
      case FUNCTION:
        st = "FUNCTION";
        break;
      case XRANGE:
        st = "XRANGE";
        break;
      case YRANGE:
        st = "YRANGE";
        break;
      case NUMBER:
        st = "NUMBER";
        break;
      case SQUARE:
        st = "SQUARE";
        break;
      case BACKGROUND:
        st = "BACKGROUND";
        break;

      default:
        st = "UNKNOWN";
        break;
    }


    if (st != null) Graph2D.out.println("(" + lineno + ") " + s + ": " + st);
  }


}


/**
 * this is a structure class used exclusively BuildGraph.
 * the class contains an Object variable, a String name identifying the object,
 * and/or  an integer token identifying the object.
 *
 */


class NamedObject extends Object {
  protected Object o = null;
  protected String s = null;
  protected int id = ScanWord.UNKNOWN;

  public NamedObject() {
  }

  public NamedObject(Object o) {
    this.o = o;
  }

  public NamedObject(Object o, String s) {
    this.o = o;
    this.s = s;
  }

  public NamedObject(Object o, String s, int id) {
    this.o = o;
    this.s = s;
    this.id = id;
  }

  public NamedObject(Object o, int id) {
    this.o = o;
    this.id = id;
  }

  public NamedObject(int id) {
    this.id = id;
  }

  public NamedObject(String s) {
    this.s = s;
  }

  public Object getObject() {
    return o;
  }

  public String getName() {
    return s;
  }

  public int getId() {
    return id;
  }

  public void setObject(Object o) {
    this.o = o;
  }

  public void setId(int id) {
    this.id = id;
  }

  public void setName(String s) {
    this.s = s;
  }

  public boolean equals(Object obj) {

    if (obj instanceof NamedObject) {
      NamedObject nobj = (NamedObject) obj;
      if (nobj.s == null && s != null ||
              nobj.s != null && s == null ||
              nobj.id != this.id ||
              !nobj.s.equals(s))
        return false;
      if (nobj.o.equals(o)) return true;
    }

    return false;
  }
}

