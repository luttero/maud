/*
 * $Id: Range.java,v 1.1 2004/12/27 16:15:23 luca Exp $
 *
 * This software is provided by NOAA for full, free and open release.  It is
 * understood by the recipient/user that NOAA assumes no liability for any
 * errors contained in the code.  Although this software is released without
 * conditions or restrictions in its use, it is expected that appropriate
 * credit be given to its author and to the National Oceanic and Atmospheric
 * Administration should the software be included by the recipient as an
 * element in other product development.
 */
 
package  gov.noaa.pmel.util;
 
/**
 * Contains minimum and maximum integer values.
 *
 * @author Donald Denbo
 * @version $Revision: 1.1 $, $Date: 2004/12/27 16:15:23 $
 * @since sgt 1.0
 */
public class Range implements java.io.Serializable {
  /** The range's first value */
  public int start;
  /** The range's last value */
  public int end;
  /**
   * Default constructor
   */
  public Range() {
    this(0, 0);
  }
  /**
   * Initializes Range with start and end integral values.
   *
   * @param start first value
   * @param end last value
   */
  public Range(int start,int end) {
    this.start = start;
    this.end = end;
  }
  /**
   * Test <code>Range</code> for equality.  Both start and end must be
   * equal for equality.
   */
  public boolean equals(Range r) {
    return (start == r.start && end == r.end);
  }
}
