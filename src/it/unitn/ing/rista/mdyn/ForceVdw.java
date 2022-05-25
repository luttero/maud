package it.unitn.ing.rista.mdyn;

import java.lang.*;
import java.util.*;


/**
 * This class implements van-der-Waals forces (using "Lennard Jones")
 * between atoms
 *
 * @version 1.0 (20-Sep-96)
 * @author  Thilo Stoeferle
 * @author  Luca Luterotti
 */

public class ForceVdw
        extends Forces {


  // all atoms
  private Vector atoms;

  // exclusion table
  private Hashtable[] excl;

  // epsilon value
  private double epsilon = 0.500; // default: 0.500 kJ/mol

  // sigma value
  private double sigma = 0.38; // default: 0.38 nm

  // cutoff value
  private double cutoff = 2.5; // default: 2.5 * sigma

  // automatic sigma/epsilon
  private boolean automatic = false; // default: epsilon automatic off
  final static private int IDXSIGMA6 = 0;
  final static private int IDXEPS4 = 1;
  private Hashtable autotab = new Hashtable();


  /** Creates a table of atoms NOT to interact
   * @param mol Molecule object
   */

  private void createExclusionTable(Molecule mol) {

    // allocate table for each atom
    excl = new Hashtable[mol.atoms.size()];
    for (int k = 0; k < excl.length; k++) {
      excl[k] = new Hashtable();
    }

    // get all 4-atom connections
    Vector conn = Forces.collectIndices(mol, 4);

    // iterate through all connections
    for (Enumeration en = conn.elements(); en.hasMoreElements();) {

      // these are the atoms we do not want to interact
      int[] connInd = (int[]) en.nextElement();

      // iterate through it for the first atom
      for (int i = 0; i < connInd.length; i++) {

        // iterate through it for the second atom
        for (int j = 0; j < connInd.length; j++) {

          // don't add same atom as base atom
          if (i != j) {
            // is it already there?
            if (!(excl[connInd[i]].containsKey(new Integer(connInd[j])))) {
              // add index
              excl[connInd[i]].put(new Integer(connInd[j]), new Integer(0));
            } // end if
          } // end if

        } // end for

      } // end for

    } // end for

  } // end createExclusionTable


  /** Make necessary initializations
   * @param mol Molecule on which the force is going to act on
   */

  public void initialize(Molecule mol) {

    // save atoms vector
    atoms = mol.atoms;
    // what atoms shall not interact?
    createExclusionTable(mol);

  } // end initialize


  /** Get sigma/epsilon in automatic mode
   * @param at1,at2 Atoms to interact
   * @return value array (with IDX... as indices for the values)
   */

  private double[] getAutoValues(Atom at1, Atom at2) {

    // check if interaction type is in cache
    Integer hashKey = new Integer(at1.elementNumber() + (at2.elementNumber() << 8));
    double[] vals = (double[]) autotab.get(hashKey);

    // calculate values if not already in cache
    if (vals == null) {

      vals = new double[2];

      // calculate sigma (ATTENTION: HARDCODED VALUE!)
      double sig = 0.45; // 0.45 nm
      double sigma2 = sig * sig;
      vals[IDXSIGMA6] = sigma2 * sigma2 * sigma2;

      // calculate epsilon
      vals[IDXEPS4] = 4 * Math.sqrt(at1.getLJEpsilon() * at2.getLJEpsilon());

      // add to cache
      autotab.put(hashKey, vals);

    } // end if

    // return values
    return (vals);

  } // end getAutoValues


  /** Apply force on molecule
   */

  public void apply() {

    // private potential counter
    double potential = 0;

    // private interaction counter
    int count = 0;

    double sigma2 = sigma * sigma;
    double sigma6 = sigma2 * sigma2 * sigma2;
    double sigma12 = sigma6 * sigma6;
    double eps4 = 4.0 * epsilon;

    // calculate cutoff
    double cutoffdist2 = cutoff * cutoff * sigma2;
    double rcutoffdist6 = 1.0 / (cutoffdist2 * cutoffdist2 * cutoffdist2);
    double ushift = -eps4 * (sigma12 * rcutoffdist6 * rcutoffdist6 - sigma6 * rcutoffdist6);

    // iterate through all atoms
    for (int i = 0; i < atoms.size() - 1; i++) {

      // first atom
      Atom at1 = (Atom) atoms.elementAt(i);

      // iterate through all remaining atoms
      for (int j = i + 1; j < atoms.size(); j++) {

        // test if atom is excluded from interaction
        if (excl[i].containsKey(new Integer(j))) {
          continue;
        }

        // second atom
        Atom at2 = (Atom) atoms.elementAt(j);

        Vector3 dist = Vector3.sub(at1.getLoc(), at2.getLoc());
        double len2 = Vector3.scalarProduct(dist, dist);

        // skip calculation if more far than cutoff distance
        if (len2 >= cutoffdist2) {
          continue;
        }

        // increase interaction counter
        count++;

        if (automatic) {
          double[] autovals = getAutoValues(at1, at2);
          sigma6 = autovals[IDXSIGMA6];
          sigma12 = sigma6 * sigma6;
          eps4 = autovals[IDXEPS4];
          ushift = -eps4 * (sigma12 * rcutoffdist6 * rcutoffdist6 - sigma6 * rcutoffdist6);
        }

        double rlen2 = 1.0 / len2;
        double rlen6 = rlen2 * rlen2 * rlen2;
        double rlen12 = rlen6 * rlen6;

        double sr6 = sigma6 * rlen6;
        double sr12 = sigma12 * rlen12;

        // force F = 48 * epsilon * ( (sigma^12 / r^13) - 0.5 * (sigma^6 / r^7) ) * 1/r
        dist.scale(12.0 * eps4 * (sr12 - 0.5 * sr6) * rlen2);
        at1.getForce().add(dist);
        at2.getForce().sub(dist);

        // potential U = 4 * epsilon * ( (sigma/r)^12 - (sigma/r)^6 ) + ushift
        potential += eps4 * (sr12 - sr6) + ushift;

      } // end for

    } // end for

    // interaction counter
    interactions = count;

    // set energy
    energy = potential;

  } // end apply


  /** Set epsilon
   * @param epsilon epsilon value
   */

  public void setEpsilon(double epsilon) {

    this.epsilon = epsilon;

  } // end setEpsilon


  /** Get epsilon
   * @return epsilon value
   */

  public double getEpsilon() {

    return (epsilon);

  } // end getEpsilon


  /** Set sigma
   * @param sigma sigma value
   */

  public void setSigma(double sigma) {

    this.sigma = sigma;

  } // end setSigma


  /** Get sigma
   * @return sigma value
   */

  public double getSigma() {

    return (sigma);

  } // end getSigma


  /** Set cutoff
   * @param cutoff cutoff value (in multiples of sigma)
   */

  public void setCutoff(double cutoff) {

    this.cutoff = cutoff;

  } // end setCutoff


  /** Get cutoff
   * @return cutoff value (in multiples of sigma)
   */

  public double getCutoff() {

    return (cutoff);

  } // end getCutoff


  /** Set automatic for sigma/epsilon
   * @param auto automatic on/off
   */

  public void setAutomatic(boolean auto) {

    automatic = auto;

  } // end setAutomatic


  /** Get automatic for sigma/epsilon
   * @return status of automatic switch
   */

  public boolean getAutomatic() {

    return (automatic);

  } // end getAutomatic


} // end ForceVdw

