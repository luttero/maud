package it.unitn.ing.rista.mdyn;

import java.lang.*;
import java.util.*;


/**
 * This class controls the molecular dynamics.
 *
 * @version 1.0 (21-Sep-96)
 * @author  Thilo Stoeferle
 * @author  Luca Luterotti
 */

public class MDSimulation
        extends Object
        implements Runnable {


  // constants for get/setForceSwitch()
  final static public int FORCEHOOKE = 1;
  final static public int FORCEBEND = 2;
  final static public int FORCETORSION = 3;
  final static public int FORCEVDW = 4;

  // force switches
  private boolean fhookeSwitch = true;
  private boolean fbendSwitch = true;
  private boolean ftorsionSwitch = false;
  private boolean fvdwSwitch = true;

  // our own thread
  private Thread thread;
  // tells us when the thread has to die
  private boolean dontquit;
  // is calculation done?
  private boolean calcdone;

  // simulated molecule
  private Molecule mol = null;
  private Molecule newMol;

  // time between integrations
  private double dt = 0.002; // default: 0.002 ps

  // initial temperature
  private double initTemp = 360; // default: 360 K

  // total kinetic energy
  double kinetic; // unit: kJ/mol

  // forces
  public ForceHooke fhooke = new ForceHooke();
  public ForceBend fbend = new ForceBend();
  public ForceTorsion ftorsion = new ForceTorsion();
  public ForceVdw fvdw = new ForceVdw();

  // performance (frames per second)
  public double fps = 0;
  private long lastTime = 0;
  private int perfUpdate = 0;
  private int PERFUPDATERATE = 5; // update every 5 steps


  /** Get random velocity from Boltzmann distribution
   * @param rand random generator
   * @param temp temperature (in Kelvin)
   * @param rmass reverse mass per mol
   * @return velocity (in nm/ps)
   */

  protected double getRandVelocity(Random rand, double temp, double rmass) {

    /* v = x * sqrt( k * temp / mass )
     * adjustment of units:
     * with k = boltzmann[J/K] * avogadro[1/mol] * 1e3 [g/kg] = 1e3 * R
     * result is in 1e3 nm/ps (our velocity units)
     */
    return (rand.nextGaussian() * Math.sqrt(8.314510e3 * temp * rmass) * 1e-3);

  } // end getRandVelocity


  /** Reset atom forces
   */

  protected void initAtomForces() {

    for (Enumeration en = mol.atoms.elements(); en.hasMoreElements();) {
      ((Atom) en.nextElement()).setForce(new Vector3(0, 0, 0));
    } // end for

  } // end initAtomForces


  /** Initialize atom velocities
   * Current implementation simply sets all velocities to zero
   */

  protected void initAtomVelocities() {

    // initialize random generator
    Random rand = new Random();

    // all velocity vectors summed up
    Vector3 totalVel = new Vector3();

    for (Enumeration en = mol.atoms.elements(); en.hasMoreElements();) {

      Atom at = (Atom) en.nextElement();
      double rmass = at.getRMass();
      at.setVel(new Vector3(getRandVelocity(rand, initTemp, rmass),
              getRandVelocity(rand, initTemp, rmass),
              getRandVelocity(rand, initTemp, rmass)));
      totalVel.add(at.getVel());

    } // end for

    // substract velocity "displacement" of the system
    totalVel.scale(1.0 / mol.atoms.size());
    for (Enumeration en = mol.atoms.elements(); en.hasMoreElements();) {
      ((Atom) en.nextElement()).getVel().sub(totalVel);
    } // end for

  } // end initAtomVelocities


  /** Integrates atom locations
   * r(t+dt) = r(t) + v(t)*dt + 1/2*f(t)*dt^2/m
   * @param deltat time step
   */

  protected void intLocations(double deltat) {

    for (Enumeration en = mol.atoms.elements(); en.hasMoreElements();) {
      Atom at = (Atom) en.nextElement();
      at.getLoc().add(Vector3.scale(at.getVel(), deltat));
      at.getLoc().add(Vector3.scale(at.getForce(), 0.5 * deltat * deltat * at.getRMass()));
    } // end for

  } // end intLocations


  /** Integrates atom velocities
   * v(t+dt) = v(t) + f(t)*dt/m
   * @param deltat time step
   */

  protected void intVelocities(double deltat) {

    for (Enumeration en = mol.atoms.elements(); en.hasMoreElements();) {
      Atom at = (Atom) en.nextElement();
      at.getVel().add(Vector3.scale(at.getForce(), deltat * at.getRMass()));
    } // end for

  } // end intVelocities


  /** Use the Force, Luke
   * @param on force switch
   */

  private void useForce(Forces f, boolean on) {

    if (on) {
      f.apply();
    } else {
      f.energy = 0;
      f.interactions = 0;
      if (f instanceof ForceHooke) {
        ((ForceHooke) f).avgLength = 0;
      } else if (f instanceof ForceBend) {
        ((ForceBend) f).avgAngle = 0;
      }
    }

  } // end useForce


  /** Recalculates the intra molecular forces
   */

  protected void recalcForces() {

    // reset atom forces
    initAtomForces();

    // apply forces
    useForce(fhooke, fhookeSwitch);
    useForce(fbend, fbendSwitch);
    useForce(ftorsion, ftorsionSwitch);
    useForce(fvdw, fvdwSwitch);

  } // end recalcForces


  /** Calculates the total kinetic energy
   * Ekin = SUMn 1/2 * m * v^2
   */

  protected void calcKinetic() {

    // private kinetic energy
    double ekin = 0;

    for (Enumeration en = mol.atoms.elements(); en.hasMoreElements();) {
      Atom at = (Atom) en.nextElement();
      ekin += 0.5 * at.getMass() * Vector3.scalarProduct(at.getVel(), at.getVel());
    } // end for

    // set kinetic
    kinetic = ekin;

  } // end calcKinetic


  /** Gets the molecule which is currently simulated.
   * @return simulated molecule
   */

  public Molecule getMolecule() {

    // return most current molecule
    if (newMol == null) {
      return (mol);
    } else {
      return (newMol);
    }

  } // end getMolecule


  /** Sets the molecule which is simulated.
   * This has to be called before launching the simulation thread.
   * @param mol molecule to simulate
   */

  public void setMolecule(Molecule mol) {

    newMol = mol;

  } // end setMolecule


  /** Initializes the molecule which is simulated.
   */

  private void initMolecule() {

    mol = newMol;
    newMol = null;

    // set atoms initial velocity atoms
    initAtomForces();
    initAtomVelocities();

    // initialize forces
    fhooke.initialize(mol);
    fbend.initialize(mol);
    ftorsion.initialize(mol);
    fvdw.initialize(mol);

  } // end initMolecule


  /** Enables/disables a single force
   * @param fnum which force (one of the FORCE... constants)
   * @param on true if enable
   */

  public void setForceSwitch(int fnum, boolean on) {

    switch (fnum) {
      case FORCEHOOKE:
        fhookeSwitch = on;
        break;
      case FORCEBEND:
        fbendSwitch = on;
        break;
      case FORCETORSION:
        ftorsionSwitch = on;
        break;
      case FORCEVDW:
        fvdwSwitch = on;
        break;
    } // end switch

  } // end setForceSwitch


  /** Returns state of a single force
   * @param fnum which force (one of the FORCE... constants)
   */

  public boolean getForceSwitch(int fnum) {

    switch (fnum) {
      case FORCEHOOKE:
        return (fhookeSwitch);
      case FORCEBEND:
        return (fbendSwitch);
      case FORCETORSION:
        return (ftorsionSwitch);
      case FORCEVDW:
        return (fvdwSwitch);
    } // end switch

    return (false);

  } // end getForceSwitch


  /** Sets the integration time step.
   * @param step time between two calculations
   */

  public void setIntStep(double step) {

    dt = step;

  } // end setIntStep


  /** Gets the integration time step.
   * @return time between two calculations
   */

  public double getIntStep() {

    return (dt);

  } // end getIntStep


  /** Sets the initial temperature.
   * @param temp initial temperature
   */

  public void setInitTemp(double temp) {

    initTemp = temp;

  } // end setInitTemp


  /** Gets the initial temperature.
   * @return initial temperature
   */

  public double getInitTemp() {

    return (initTemp);

  } // end getInitTemp


  /** Method to start the calculation as a separate thread.
   * Calculation is initiated with the startCalc() method.
   */

  public void run() {

    while (dontquit) {

      // initialize molecule if necessary
      if (newMol != null) {
        initMolecule();
      } // end if

      // calculate performance
      if (perfUpdate == 0) {
        long newTime = System.currentTimeMillis();
        if (lastTime > 0) {
          fps = PERFUPDATERATE * 1000.0 / ((double) (newTime - lastTime));
        }
        lastTime = newTime;
        perfUpdate = PERFUPDATERATE;
      } else {
        perfUpdate--;
      }

      // wait for next calculation job
      waitStart();

      // integration
      if (mol != null) {
        intLocations(dt);
        intVelocities(0.5 * dt);
        recalcForces();
        intVelocities(0.5 * dt);
        calcKinetic();
      } // end if

    } // end while

    // clear fps counter
    fps = 0;
    lastTime = 0;

  } // end run


  /** Wait for initiation of calculation
   */

  synchronized private void waitStart() {

    calcdone = true;
    // wait for start notification
    try {
      wait();
    } catch (InterruptedException e) {
    }

  } // end waitStart


  /** Initiate fresh calculation of the molecule
   */

  synchronized public void startCalc() {

    if (calcdone) {
      calcdone = false;
      // initiate calculation
      notifyAll();
    } // end if

  } // end startCalc


  /** Kick off our own thread
   */

  public void startSimulation() {

    if (thread != null) {
      thread.stop();
      thread = null;
    }
    dontquit = true;
    thread = new Thread(this, "Simulation");
    thread.start();

  } // end startSimulation


  /** Gracefully let the thread die
   */

  public void stopSimulation() {

    // let's say it's over
    dontquit = false;

  } // end stopSimulation


} // end MDSimulation
