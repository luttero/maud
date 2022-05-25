package it.unitn.ing.rista.mdyn;

import java.lang.*;
import java.util.*;


/**
 * This class implements a data structure to describe a molecule
 *
 * @version 1.0 (11-Jul-96)
 * @author  Thilo Stoeferle
 * @author  Luca Luterotti
 */

public class Molecule
        extends Object {


  public Vector atoms;
  public Vector body;
  public Vector3 minBound,maxBound;
  private double totalMass = 0;


  /** Creates a molecule
   * The public field atoms holds the AtomSite objects,
   * public field body contains the Bond objects.
   */

  public Molecule() {

    atoms = new Vector(50);
    body = new Vector(50);

  } // end Molecule


  /** Calculates the total mass of the molecule
   * @return total mass of molecule
   */

  public double getTotalMass() {

    if (totalMass == 0) {
      for (Enumeration en = atoms.elements(); en.hasMoreElements();) {
        totalMass += ((Atom) en.nextElement()).getMass();
      } // end for
    } // end if
    return (totalMass);

  } // end getTotalMass


  /** Calculates the bounding box to hold the molecule
   * This uses model coordinates, not (probably scaled)
   * world coordinates.
   * The result is stored in the public fields
   * minBound, maxBound
   */

  public void getBounds() {

    Atom at;

    // create enumeration of Atoms
    Enumeration en = atoms.elements();

    // get first one
    if (!(en.hasMoreElements())) {
      minBound = maxBound = new Vector3(0, 0, 0);
      return;
    }
    at = (Atom) en.nextElement();
    minBound = new Vector3(at.getLoc().x, at.getLoc().y, at.getLoc().z);
    maxBound = new Vector3(at.getLoc().x, at.getLoc().y, at.getLoc().z);

    // walk through all atoms
    while (en.hasMoreElements()) {
      at = (Atom) en.nextElement();

      // x value
      if (at.getLoc().x < minBound.x) {
        minBound.x = at.getLoc().x;
      } else if (at.getLoc().x > maxBound.x) {
        maxBound.x = at.getLoc().x;
      }

      // y value
      if (at.getLoc().y < minBound.y) {
        minBound.y = at.getLoc().y;
      } else if (at.getLoc().y > maxBound.y) {
        maxBound.y = at.getLoc().y;
      }

      // z value
      if (at.getLoc().z < minBound.z) {
        minBound.z = at.getLoc().z;
      } else if (at.getLoc().z > maxBound.z) {
        maxBound.z = at.getLoc().z;
      }

    } // end while

  } // end getBounds


  /** Creates a string representation of this molecule
   * @return a string representation of this molecule
   */

  public String toString() {

    int i;
    String str;

    str = "Molecule\n";
    str = str + "Atoms:\n";
    for (Enumeration e = atoms.elements(); e.hasMoreElements();) {
      str = str + (Atom) e.nextElement() + "\n";
    }
    str = str + "Bonds:\n";
    for (Enumeration e = body.elements(); e.hasMoreElements();) {
      str = str + (Bond) e.nextElement() + "\n";
    }

    return (str);

  } // end toXRDcatString

} // end Molecule
