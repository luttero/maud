package it.unitn.ing.esqui.client;

import java.awt.*;

/** WaintingWindow.java
 * <br>
 * Title:			<b>ESQUI Wainting Window</b>
 * </br>
 * Description:	Class for creating a waiting window
 * @author:			Leonardo Cont, December 2000
 * @revision:		February 2001
 * @comment:		none
 */

class WaitingWindow extends InfoWindow {

  public WaitingWindow(Window owner, String title) {
    super(owner, title);

    showWindow();
  }
}
