/*
 * $Id: Cartesian.java,v 1.1 2004/12/27 16:15:21 luca Exp $
 *
 * This software is provided by NOAA for full, free and open release.  It is
 * understood by the recipient/user that NOAA assumes no liability for any
 * errors contained in the code.  Although this software is released without
 * conditions or restrictions in its use, it is expected that appropriate
 * credit be given to its author and to the National Oceanic and Atmospheric
 * Administration should the software be included by the recipient as an
 * element in other product development.
 */
 
package gov.noaa.pmel.sgt.dm;
 
/**
 * The <code>Cartesian</code> interface indicates to the
 * <code>sgt</code> classes that
 * X and Y coordinates are to be interpreted as Cartesian
 * coordinates.  X or Y can optionally, when independent, a time
 * axis.
 *
 * @author Donald Denbo
 * @version $Revision: 1.1 $, $Date: 2004/12/27 16:15:21 $
 * @since 1.0
 */
public interface Cartesian extends CoordinateSystem {
}
