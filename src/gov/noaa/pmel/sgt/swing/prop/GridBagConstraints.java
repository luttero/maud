/*
 * $Id: GridBagConstraints.java,v 1.1 2004/12/27 16:15:22 luca Exp $
 *
 * This software is provided by NOAA for full, free and open release.  It is
 * understood by the recipient/user that NOAA assumes no liability for any
 * errors contained in the code.  Although this software is released without
 * conditions or restrictions in its use, it is expected that appropriate
 * credit be given to its author and to the National Oceanic and Atmospheric
 * Administration should the software be included by the recipient as an
 * element in other product development.
 */
package gov.noaa.pmel.sgt.swing.prop;
/**
 * Creates a <code>GridBagConstraints</code> object from the
 * constructor.  This class duplicates functionality available in jdk1.2
 *
 * @author Donald Denbo
 * @version $Revision: 1.1 $, $Date: 2004/12/27 16:15:22 $
 * @since 2.0
 */
public class GridBagConstraints extends java.awt.GridBagConstraints {

/**
  * Creates a <code>GridBagConstraint</code> object.
  */

    public GridBagConstraints(int gridX,
        int gridY,
        int gridWidth,
        int gridHeight,
        double weightX,
        double weightY,
        int anchor,
        int fills,
        java.awt.Insets inset,
        int ipadX,
        int ipadY) {

        gridx = gridX;
        gridy = gridY;
        gridwidth = gridWidth;
        gridheight = gridHeight;
        weightx = weightX;
        weighty = weightY;
        this.anchor = anchor;
        fill = fills;
        insets = inset;
        ipadx = ipadX;
        ipady = ipadY;

    }
}





