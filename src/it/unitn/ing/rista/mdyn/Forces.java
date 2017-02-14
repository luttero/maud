package it.unitn.ing.rista.mdyn;

import java.lang.*;
import java.util.*;


/**
 * Basic class for intra-molecule forces. The specific
 * implementation of the forces must be done in the subclasses.
 *
 * @version 1.0 (20-Sep-96)
 * @author  Thilo Stoeferle
 * @author  Luca Luterotti
 */

abstract public class Forces
        extends Object {


  /** Total potential in this kind of force.
   * Unit: kJ / mol
   */
  public double energy;


  /** Number of interactions by this kind of force.
   */
  public int interactions;


  /** Convert an array of AtomSite indices to an array of AtomSite object references.
   * @param mol Molecule object
   * @param indices Vector of arrays of AtomSite indices
   * @return Vector of arrays[num] of AtomSite references
   */

  static protected Vector indices2Atoms(Molecule mol, Vector indices) {

    // allocate AtomSite vector
    Vector at = new Vector(20);

    // iterate through all connections
    for (Enumeration en = indices.elements(); en.hasMoreElements();) {

      // get connection indices
      int[] connInd = (int[]) en.nextElement();

      // allocate atom array
      Atom[] connAt = new Atom[connInd.length];

      // convert each atom index
      for (int i = 0; i < connInd.length; i++) {
        connAt[i] = (Atom) mol.atoms.elementAt(connInd[i]);
      } // end for

      // add atom array
      at.addElement(connAt);

    } // end for

    // return array of Atoms
    return (at);

  } // end indices2Atoms



  /* Recursively walk through all bonds
   * vec = Vector where all complete connections are added
   * mol = Molecule structure
   * at  = Array of indices of Atoms, which is the "history" of the walk
   * cur = Number of AtomSite currently examined
   * num = Number of Atoms still to go
   */

  static private void recurCollect(Vector vec, Molecule mol, int[] at, int cur, int num) {

    // add current base atom to connection history
    at[at.length - num] = cur;

    // still some bonds to go?
    if (num > 1) {

      // get Bond structure of the current atom
      for (Enumeration en = mol.body.elements(); en.hasMoreElements();) {

        Bond thisbond = (Bond) en.nextElement();
        if (thisbond.baseatom == cur) {

          // walk through all adjected bonds
          for (int i = 0; i < thisbond.countBonds(); i++) {
            recurCollect(vec, mol, at,
                    thisbond.getBond(i), num - 1);
          } // end for

        } // end if

      } // end for

    } else {

      // copy array
      int[] conn = new int[at.length];
      System.arraycopy(at, 0, conn, 0, at.length);

      // add connection
      vec.addElement(conn);

    } // end else

  } // recurCollect


  /** Collect all structures with a certain number of bonds
   * This only steps SEQUENTIALLY through the bonds
   * @param mol Molecule object
   * @param num number of atoms in a row
   * @return Vector of arrays[num] of AtomSite indices
   */

  static protected Vector collectIndices(Molecule mol, int num) {

    // initialize Vector where the arrays are stored in
    Vector vec = new Vector(20);

    if (mol != null) {

      // iterate through all bonds sequentially
      for (Enumeration en = mol.body.elements(); en.hasMoreElements();) {

        // collect all connections with num atoms
        recurCollect(vec, mol, new int[num],
                ((Bond) en.nextElement()).baseatom, num);

      } // end for

    } // end if

    // return all found connections
    return (vec);

  } // end collectIndices


  /** Collect all structures with a certain number of bonds
   * This only steps SEQUENTIALLY through the bonds
   * @param mol Molecule object
   * @param num number of atoms in a row
   * @return Vector of arrays[num] of AtomSite references
   */

  static protected Vector collectAtoms(Molecule mol, int num) {

    return (indices2Atoms(mol, collectIndices(mol, num)));

  } // end collectAtoms


  /** Make necessary initializations
   * @param mol Molecule on which the force is going to act on
   */

  abstract public void initialize(Molecule mol);


  /** Apply force on molecule
   */

  abstract public void apply();


} // end Forces
