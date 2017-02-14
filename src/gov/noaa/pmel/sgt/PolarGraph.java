/*
 * $Id: PolarGraph.java,v 1.1 2004/12/27 16:15:20 luca Exp $
 *
 * This software is provided by NOAA for full, free and open release.  It is
 * understood by the recipient/user that NOAA assumes no liability for any
 * errors contained in the code.  Although this software is released without
 * conditions or restrictions in its use, it is expected that appropriate
 * credit be given to its author and to the National Oceanic and Atmospheric
 * Administration should the software be included by the recipient as an
 * element in other product development.
 */

package gov.noaa.pmel.sgt;

import gov.noaa.pmel.sgt.dm.SGTData;

import java.awt.*;
import java.beans.PropertyChangeEvent;

/**
 * Description of Class PolarGraph
 *
 * @author Donald Denbo
 * @version $Revision: 1.1 $, $Date: 2004/12/27 16:15:20 $
 * @since 2.x
 */
public class PolarGraph extends Graph implements Cloneable {
  public PolarGraph() {
  }
  public Graph copy() {
    PolarGraph newGraph;
    try {
      newGraph = (PolarGraph)clone();
    } catch (CloneNotSupportedException e) {
      newGraph = new PolarGraph();
    }
    return (Graph)newGraph;
  }
  void draw(Graphics g) {
	  g.setColor(Color.black);
	  Rectangle rect = g.getClipBounds();
	  System.out.println(rect.x + " " + rect.y + " " + rect.width + " " + rect.height);
	  g.drawOval(rect.x, rect.y, rect.width, rect.height);
//    throw new MethodNotImplementedError();
  }
  public Object getObjectAt(Point pt) {
    throw new MethodNotImplementedError();
  }
  public SGTData getDataAt(Point pt) {
    throw new MethodNotImplementedError();
  }

  public void propertyChange(PropertyChangeEvent evt) {
  }
}

