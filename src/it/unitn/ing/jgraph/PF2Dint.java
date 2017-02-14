package it.unitn.ing.jgraph;

import java.awt.*;


/*
**************************************************************************
**
**    Class  PF2Dint
**
**************************************************************************
**    Copyright (C) 1998 Luca Lutterotti
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
**    This class is an extension of G2Dint class.
**    It is for Pole Figure plotting.
**
*************************************************************************/

/**
 *    This class is an extension of G2Dint class.
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:51 $.
 * @author Luca Lutterotti
 */

public class PF2Dint extends G2Dint {

  public PF2Dint() {

    super();

  }

  public void paintBeforeData(Graphics g, Rectangle r) {
    super.paintBeforeData(g, r);
    g.setColor(Color.black);
    g.drawOval(r.x, r.y, r.width, r.height);
    g.drawLine(r.x, r.y + r.height / 2, r.x + r.width, r.y + r.height / 2);
    g.drawLine(r.x + r.width / 2, r.y, r.x + r.width / 2, r.y + r.height);
  }
}
