package it.unitn.ing.rista.mdyn;

import java.lang.*;
import java.util.*;


/**
 * This class implements dihedral ("torsion") forces between atoms
 *
 * @version 1.0 (25-Sep-96)
 * @author  Thilo Stoeferle
 * @author  Luca Luterotti
 */

public class ForceTorsion
        extends Forces {


  // all collected connections
  private Vector conn;

  // dihedral force constants
  private double[] k = new double[4];

  // dihedral phase
  private int[] phase = new int[4];


  /** Create a torsion force object
   */

  public ForceTorsion() {

    k[0] = -6.70;
    k[1] = 3.63;
    k[2] = -13.98;
    k[3] = 0;
    phase[0] = 1;
    phase[1] = 1;
    phase[2] = 1;
    phase[3] = 1;

  } // end ForceTorsion


  /** Make necessary initializations
   * @param mol Molecule on which the force is going to act on
   */

  public void initialize(Molecule mol) {

    // get all 4-atom connections
    conn = Forces.collectAtoms(mol, 4);

  } // end initialize


  /** Apply force on molecule
   */

  public void apply() {

    // private potential counter
    double potential = 0;

    // iterate through all connections
    for (Enumeration en = conn.elements(); en.hasMoreElements();) {

      // get array of connected atoms
      Atom[] atom = (Atom[]) en.nextElement();

      Vector3 dist1 = Vector3.sub(atom[1].getLoc(), atom[0].getLoc());
      Vector3 dist2 = Vector3.sub(atom[2].getLoc(), atom[1].getLoc());
      Vector3 dist3 = Vector3.sub(atom[3].getLoc(), atom[2].getLoc());

      Vector3 s = Vector3.crossProduct(dist1, dist2);
      Vector3 t = Vector3.crossProduct(dist3, dist2);

      double rlens2 = 1.0 / Vector3.scalarProduct(s, s);
      double rlent2 = 1.0 / Vector3.scalarProduct(t, t);
      double rlenst = Math.sqrt(rlens2 * rlent2);

      // cos(phi) of dihedral
      double cosphi = Vector3.scalarProduct(s, t) * rlenst;
      double cosphi2 = cosphi * cosphi;

      // - dU / dcos(phi)
      double dUdcos = -(k[0] * phase[0] +
              k[1] * phase[1] * (4.0 * cosphi2) +
              k[2] * phase[2] * (-3.0 + 12.0 * cosphi2) +
              k[3] * phase[3] * ((-16.0 + 32.0 * cosphi2) * cosphi));

      // dcos(phi) / ds
      Vector3 dUds = new Vector3(dUdcos * (t.x * rlenst - cosphi * rlens2 * s.x),
              dUdcos * (t.y * rlenst - cosphi * rlens2 * s.y),
              dUdcos * (t.z * rlenst - cosphi * rlens2 * s.z));
      // dcos(phi) / dt
      Vector3 dUdt = new Vector3(dUdcos * (s.x * rlenst - cosphi * rlens2 * t.x),
              dUdcos * (s.y * rlenst - cosphi * rlens2 * t.y),
              dUdcos * (s.z * rlenst - cosphi * rlens2 * t.z));

      // d(s,t) / d(1,2,3) * dcos d(s,t)
      Vector3 dsd1 = Vector3.crossProduct(dist2, dUds);
      Vector3 dsd2 = Vector3.crossProduct(dUds, dist1);
      Vector3 dtd2 = Vector3.crossProduct(dUdt, dist2);
      Vector3 dtd3 = Vector3.crossProduct(dist3, dUdt);

      // force
      atom[0].getForce().sub(dsd1);
      atom[1].getForce().add(dsd1);
      atom[1].getForce().sub(dsd2);
      atom[2].getForce().add(dsd2);
      atom[1].getForce().sub(dtd2);
      atom[2].getForce().add(dtd2);
      atom[2].getForce().sub(dtd3);
      atom[3].getForce().add(dtd3);

      // potential
      potential += k[0] * (1.0 + phase[0] * cosphi) +
              k[1] * (1.0 + phase[1] * (-1.0 + 2.0 * cosphi2)) +
              k[2] * (1.0 + phase[2] * ((-3.0 + 4.0 * cosphi2) * cosphi)) +
              k[3] * (1.0 + phase[3] * (1.0 - 8.0 * cosphi2 + 8.0 * cosphi2 * cosphi2));

    } // end for

    // set energy
    energy = potential;

    // interaction counter
    interactions = conn.size();

  } // end apply


  /** Set dihedral force constants
   * @param kdihedral dihedral force constants
   */

  public void setDihedral(double[] kdihedral) {

    System.arraycopy(kdihedral, 0, k, 0, k.length);

  } // end setDihedral


  /** Get dihedral force constants
   * @return dihedral force constants
   */

  public double[] getDihedral() {

    double[] copy = new double[k.length];
    System.arraycopy(k, 0, copy, 0, k.length);
    return (copy);

  } // end getDihedral


  /** Set phase values.
   * phase=0 : +1 , phase=pi : -1
   * @param phase phase values
   */

  public void setPhase(int[] phase) {

    System.arraycopy(phase, 0, this.phase, 0, this.phase.length);

  } // end setPhase


  /** Get phase values
   * @return phase values
   */

  public int[] getPhase() {

    int[] copy = new int[phase.length];
    System.arraycopy(phase, 0, copy, 0, phase.length);
    return (copy);

  } // end getPhase


} // end ForceBend

