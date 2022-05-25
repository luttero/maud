package it.unitn.ing.rista.mdyn;

import java.lang.*;


/**
 * This class is implementing a data structure to store
 * the information about the bonding between atoms.
 *
 * @version 1.0 (11-Jul-96)
 * @author  Thilo Stoeferle
 * @author  Luca Luterotti
 */

public class Bond
        extends Object {


  public int MAXBONDS = 4;
  public int baseatom;
  private int[] bonds;
  private int bondcnt;


  /** Creates an object to store bonding information
   */

  public Bond() {

    this.bonds = new int[4];
    this.bondcnt = 0;

  } // end Bond


  /** Creates an object to store bonding information
   * @param base serial number of the base atom
   */

  public Bond(int base) {

    this();
    this.baseatom = base;

  } // end Bond


  /** Adds a bond to a base atom
   * @param num serial number of the atom to add
   */

  public void addBond(int num) {

    if (bondcnt < MAXBONDS) {
      bonds[bondcnt] = num;
      bondcnt++;
    }

  } // end addBond


  /** Counts the number of bonds of this atom
   * @return number of bonds
   */

  public int countBonds() {

    return (bondcnt);

  } // end countBonds


  /** Get serial number of the selected bonded atom
   * @param index index of bonded atom
   * @return serial number of bonded atom
   */

  public int getBond(int index) {

    if (index < bondcnt) {
      return (bonds[index]);
    } else {
      return (0);
    }

  } // end getBond


  /** Creates a string representation of this bond
   * @return a string representation of this bond
   */

  public String toString() {

    int i;
    String str;

    str = "* " + baseatom + " * ";
    for (i = 0; i < bondcnt; i++) {
      str = str + " - " + bonds[i];
    }

    return (str);

  } // end toXRDcatString

} // end Bond
