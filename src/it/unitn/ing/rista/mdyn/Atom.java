package it.unitn.ing.rista.mdyn;

import java.lang.*;


/**
 * A class to represent an atom modeled as a simple particle
 * in a 3D space.
 *
 * @author Thilo Stoeferle
 * @author Luca Luterotti
 * @version 1.0 (10-Jul-96)
 */

public class Atom {


  // element numbers
  public static final int NUMH = 1;
  public static final int NUMC = 6;
  public static final int NUMN = 7;
  public static final int NUMO = 8;
  public static final int NUMMONOMER = -1;
  public static final int NUMENDMONOMER = -2;

  /**
   * 3D coordinates in the system space
   * Unit: Nanometers (= 1e-9 m)
   */
  private Vector3 loc = new Vector3();

  /**
   * Velocity vector of this particle
   * Unit: Nanometers / Picosecond (= 1e-3 m/s)
   */
  private Vector3 vel = new Vector3();

  /**
   * Accumlated force on this particle
   * Unit: Gramms Nanometers / mol Picosecond^2
   */
  private Vector3 force = new Vector3();

  /**
   * 3D coordinates in the world space
   * This is actually a hack to have a place to store these
   * transformed coordinates somewhere.
   * An atom object really should not have to know about its
   * world coordinates.
   */
  private Vector3 world = new Vector3();

  /**
   * AtomSite name (in most cases just e.g. "H" or "C", but also
   * more descriptive names like "CG2" are possible.
   */
  private String name;

  /**
   * Element number of this atom.
   * Use NUM... constants
   */
  private int number;

  /**
   * AtomSite mass of one mol of atoms.
   * Unit: Gramms
   */
  private double mass;

  /**
   * Reverse mass of one mol of atoms.
   * This is actually more useful for faster calculations (no divisions).
   * Unit: 1 / Gramms
   */
  private double rmass;


  /**
   * Creates an atom at the x,y,z coordinates.
   *
   * @param x coordinate x of the atom to be created
   * @param y coordinate y of the atom to be created
   * @param z coordinate z of the atom to be created
   */

  public Atom(double x, double y, double z) {
    super();

    getLoc().x = x;
    getLoc().y = y;
    getLoc().z = z;

  } // end AtomSite


  /**
   * Creates a specific atom at the x,y,z coordinates.
   *
   * @param tname element name (e.g. "H" or "CG2")
   * @param x     coordinate x of the atom to be created
   * @param y     coordinate y of the atom to be created
   * @param z     coordinate z of the atom to be created
   */

  public Atom(String tname, double x, double y, double z) {

    this(x, y, z);
    setName(tname);

  } // end AtomSite


  /**
   * Sets the element name of an atom.
   *
   * @param elementName element name (e.g. "H" or "CG2")
   */

  public void setName(String elementName) {

    name = elementName;
    number = getElementNum(name);
    mass = elementMass();
    rmass = 1.0 / mass;

  } // end setName


  /**
   * Get element mass
   *
   * @return element mass in g/mol
   */

  private double elementMass() {

    switch (number) {
      case Atom.NUMH:
        return 1.0079;
      case Atom.NUMC:
        return 12.011;
      case Atom.NUMN:
        return 14.007;
      case Atom.NUMO:
        return 15.9994;
      case Atom.NUMMONOMER:
        return 14.0; // CH2
      case Atom.NUMENDMONOMER:
        return 15.0; // CH3
      default:
        return 12.011; // if unknown, use C as resonable default

    } // end switch


  } // end elementMass


  /**
   * Gets the name of the atom
   *
   * @return atom name
   */

  public String getName() {

    return name;

  } // end getName


  /**
   * Gets the mass of the atom
   *
   * @return atom mass
   */

  public double getMass() {

    return mass;

  } // end getMass


  /**
   * Gets the reciprocal mass of the atom
   *
   * @return reciprocal atom mass
   */

  public double getRMass() {

    return rmass;

  } // end getRMass


  /**
   * Figures out the element number of the atom.
   * Any descripitve strings following the first character
   * are ignored.
   * This method currently knows only about the elements:
   * H, C, N, O, M(onomer), (E)ndmonomer
   *
   * @param tname atom name
   * @return element number
   */

  static private int getElementNum(String tname) {

    try {

      switch (tname.charAt(0)) {
        case 'H':
          return Atom.NUMH;
        case 'C':
          return Atom.NUMC;
        case 'N':
          return Atom.NUMN;
        case 'O':
          return Atom.NUMO;
        case 'M':
          return Atom.NUMMONOMER;
        case 'E':
          return Atom.NUMENDMONOMER;
        default :
      } // end switch

    } catch (StringIndexOutOfBoundsException e) {
    }

    return Atom.NUMC; // return C12 as resonable default

  } // end getElementNum


  /**
   * Gets the element number of the atom
   *
   * @return element number
   */

  public int elementNumber() {

    return number;

  } // end elementNumber


  /**
   * Gets the Lennard Jones epsilon for this atom
   *
   * @return epsilon for this atom
   */

  public double getLJEpsilon() {

    switch (number) {
      case Atom.NUMMONOMER:
        return 0.391; // CH2
      case Atom.NUMENDMONOMER:
        return 0.948; // CH3
      default:
        return 0.500; // if unknown, use 0.500 kJ/mol as resonable default

    } // end switch

  } // end getLJEpsilon


  /**
   * Creates a string representation of an AtomSite
   *
   * @return a string representation of this AtomSite
   */

  public String toString() {

    return (name + " " + getLoc().toString());

  } // end toXRDcatString


  public Vector3 getForce() {
    return force;
  }

  public void setForce(Vector3 force) {
    this.force = force;
  }

  public Vector3 getLoc() {
    return loc;
  }

  public void setLoc(Vector3 loc) {
    this.loc = loc;
  }

  public Vector3 getVel() {
    return vel;
  }

  public void setVel(Vector3 vel) {
    this.vel = vel;
  }

  public Vector3 getWorld() {
    return world;
  }

  public void setWorld(Vector3 world) {
    this.world = world;
  }


} // end AtomSite
