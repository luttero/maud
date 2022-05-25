/*
 * $Id: YearDecadeAxis.java,v 1.1 2004/12/27 16:15:20 luca Exp $
 *
 * This software is provided by NOAA for full, free and open release.  It is
 * understood by the recipient/user that NOAA assumes no liability for any
 * errors contained in the code.  Although this software is released without
 * conditions or restrictions in its use, it is expected that appropriate
 * credit be given to its author and to the National Oceanic and Atmospheric
 * Administration should the software be included by the recipient as an
 * element in other product development.
 */

package  gov.noaa.pmel.sgt;

import gov.noaa.pmel.util.GeoDate;
import gov.noaa.pmel.util.TimeRange;
import gov.noaa.pmel.util.IllegalTimeValue;

/**
 * Draws time axes using the year/decade style.
 *
 * <pre>
 *            |..........|..........|..........|..........|
 *                 84         85         86         87
 *                               1980
 * </pre>
 *
 * @author Donald Denbo
 * @version $Revision: 1.1 $, $Date: 2004/12/27 16:15:20 $
 * @see Axis
 * @see TimeAxis
 */
class YearDecadeAxis implements TimeAxisStyle {
  static final int DECADE_TEST__ = 1000;
  static final String defaultMinorLabelFormat__ = "yy";
  //  static final String defaultMajorLabelFormat__ = "yyyy";
  static final String defaultMajorLabelFormat__ = "decade";
  int defaultMinorLabelInterval_ = 2;
  int defaultMajorLabelInterval_ = 1;
  static final int defaultNumSmallTics__ = 0;
  static final double incrementValue__ = 1.0;
  static final int incrementUnits__ = GeoDate.YEARS;
  /**
   * YearDecadeAxis constructor.
   *
   * @param id axis identifier
   **/
  public YearDecadeAxis() {
  }
  public void computeDefaults(GeoDate delta) {
    if(delta.getTime()/GeoDate.MSECS_IN_DAY > 2500) {
      defaultMinorLabelInterval_ = 2;
    } else {
      defaultMinorLabelInterval_ = 1;
    }
  }
  public double computeLocation(double prev,double now) {
    return (prev + now)*0.5;
  }
  public int getMinorValue(GeoDate time) {
    return time.getGMTYear() - (time.getGMTYear()/10)*10 + 1;
  }
  public int getMajorValue(GeoDate time) {
    return (time.getGMTYear()/10)*10;
  }
  public boolean isRoomForMajorLabel(GeoDate delta) {
    return delta.getTime()/GeoDate.MSECS_IN_DAY > DECADE_TEST__;
  }
  public boolean isStartOfMinor(GeoDate time) {
    return (time.getGMTYear() % 10) == 0;
  }
  public String getDefaultMinorLabelFormat() {
    return defaultMinorLabelFormat__;
  }
  public String getDefaultMajorLabelFormat() {
    return defaultMajorLabelFormat__;
  }
  public int getDefaultNumSmallTics() {
    return defaultNumSmallTics__;
  }
  public int getDefaultMajorLabelInterval() {
    return defaultMajorLabelInterval_;
  }
  public int getDefaultMinorLabelInterval() {
    return defaultMinorLabelInterval_;
  }
  public GeoDate getStartTime(TimeRange tRange) {
    boolean time_increasing;
    GeoDate time = null;
    time_increasing = tRange.end.after(tRange.start);
    try {
      if(time_increasing) {
        time = new GeoDate(1, 1, tRange.start.getGMTYear(), 0, 0, 0, 0);
        if(!time.equals(tRange.start)) time.increment(1.0, GeoDate.YEARS);
      } else {
        time = new GeoDate(1, 1, tRange.end.getGMTYear(), 0, 0, 0, 0);
        if(!time.equals(tRange.end)) time.increment(1.0, GeoDate.YEARS);
      }
    } catch (IllegalTimeValue e) {}
    return time;
  }
  public double getIncrementValue() {
    return incrementValue__;
  }
  public int getIncrementUnits() {
    return incrementUnits__;
  }
  public String toString() {
    return "YearDecadeAxis";
  }
}
