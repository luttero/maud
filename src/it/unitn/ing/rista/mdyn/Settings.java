package it.unitn.ing.rista.mdyn;

import java.io.*;
import java.net.*;
import java.awt.*;
import java.applet.*;


/** Settings
 * This class implements a configuration mechanism
 *
 * @version 1.0 (23-Sep-96)
 * @author  Thilo Stoeferle
 * @author  Luca Luterotti
 */

public class Settings
        extends Object
        implements Cloneable, Runnable {


  /** Molecule
   */
  public Molecule mol;


  /** some general settings
   */
  public boolean generalDoSimulation;
  public boolean generalCenterMolecule;
  public boolean generalAutoRotation;
  public String generalMolName;
  public String generalMolChain;
  public String generalParamName;


  /** renderer switches
   */
  public int rendStyle;
  public boolean rendDoubleBuffer;
  public boolean rendDepthCueing;


  // simulation settings

  /** forces' switches
   */
  public boolean forceHookeEnabled;
  public boolean forceBendEnabled;
  public boolean forceTorsionEnabled;
  public boolean forceVdwEnabled;

  /** integration
   */
  public double simuIntStep;

  /** initializations
   */
  public double simuInitTemp;


  // force parameters

  /** Hooke force parameters
   */
  public double fhookeEqual;
  public double fhookeHooke;

  /** Bend force parameters
   */
  public double fbendEqual;
  public double fbendBend;

  /** Torsion force parameters
   */
  public double[] ftorsionDihedral;
  public int[] ftorsionPhase;

  /** VDW force parameters
   */
  public double fvdwSigma;
  public double fvdwEpsilon;
  public double fvdwCutoff;
  public boolean fvdwAutomatic;


  // global vars for getParams()
  private Applet app;
  private String[] args;

  // loader thread
  private Thread loadThread;
  private URL loadURL;
  private int loadType = LOADNOTHING;
  private Renderer loadRend;
  private MDSimulation loadSimu;
  static final private int LOADNOTHING = 0;
  static final private int LOADMOLECULE = 1;
  static final private int LOADPARAMETER = 2;


  /** Gets current renderer settings
   */

  public void getRendererSettings(Renderer rend) {

    rendDoubleBuffer = rend.getDoubleBuffer();
    if (rend instanceof LineRenderer) {
      rendDepthCueing = ((LineRenderer) rend).getDepthCueing();
    } // end if
    if (rend instanceof BallRenderer) {
      rendStyle = ((BallRenderer) rend).getRenderStyle();
    } // end if

  } // end getRendererSettings


  /** Sets the renderer settings
   */

  public void setRendererSettings(Renderer rend) {

    rend.setDoubleBuffer(rendDoubleBuffer);
    if (rend instanceof LineRenderer) {
      ((LineRenderer) rend).setDepthCueing(rendDepthCueing);
    } // end if
    if (rend instanceof BallRenderer) {
      ((BallRenderer) rend).setRenderStyle(rendStyle);
    } // end if

  } // end setRendererSettings


  /** Gets current simulation settings
   */

  public void getSimulationSettings(MDSimulation simu) {

    forceHookeEnabled = simu.getForceSwitch(MDSimulation.FORCEHOOKE);
    forceBendEnabled = simu.getForceSwitch(MDSimulation.FORCEBEND);
    forceTorsionEnabled = simu.getForceSwitch(MDSimulation.FORCETORSION);
    forceVdwEnabled = simu.getForceSwitch(MDSimulation.FORCEVDW);

    simuIntStep = simu.getIntStep();
    simuInitTemp = simu.getInitTemp();

    fhookeEqual = simu.fhooke.getEqual();
    fhookeHooke = simu.fhooke.getHooke();

    fbendEqual = simu.fbend.getEqualAngle();
    fbendBend = simu.fbend.getBend();

    ftorsionDihedral = simu.ftorsion.getDihedral();
    ftorsionPhase = simu.ftorsion.getPhase();

    fvdwSigma = simu.fvdw.getSigma();
    fvdwEpsilon = simu.fvdw.getEpsilon();
    fvdwCutoff = simu.fvdw.getCutoff();
    fvdwAutomatic = simu.fvdw.getAutomatic();

  } // end getSimulationSettings


  /** Sets the simulation settings
   */

  public void setSimulationSettings(MDSimulation simu) {

    simu.setForceSwitch(MDSimulation.FORCEHOOKE, forceHookeEnabled);
    simu.setForceSwitch(MDSimulation.FORCEBEND, forceBendEnabled);
    simu.setForceSwitch(MDSimulation.FORCETORSION, forceTorsionEnabled);
    simu.setForceSwitch(MDSimulation.FORCEVDW, forceVdwEnabled);

    simu.setIntStep(simuIntStep);
    simu.setInitTemp(simuInitTemp);

    simu.fhooke.setEqual(fhookeEqual);
    simu.fhooke.setHooke(fhookeHooke);

    simu.fbend.setEqualAngle(fbendEqual);
    simu.fbend.setBend(fbendBend);

    simu.ftorsion.setDihedral(ftorsionDihedral);
    simu.ftorsion.setPhase(ftorsionPhase);

    simu.fvdw.setSigma(fvdwSigma);
    simu.fvdw.setEpsilon(fvdwEpsilon);
    simu.fvdw.setCutoff(fvdwCutoff);
    simu.fvdw.setAutomatic(fvdwAutomatic);

  } // end setSimulationSettings


  /** Creates a URL using the appropriate base URL
   * @param file file name
   * @return URL of the file
   */

  private URL makeURL(String file) {

    try {
      URL myURL;
      if (app != null) {
        return (new URL(app.getCodeBase(), file));
      } else {
        return (new URL(new URL("file:"), file));
      }
    } catch (MalformedURLException e) {
      return (null);
    }

  } // end makeURL


  /** Reads a molecule
   * @param molURL URL of the molecule
   * @return molecule
   */

  public Molecule readMolecule(URL molURL, String chain) {

    Molecule mol = null;

    try {
      InputStream is = molURL.openStream();
      mol = new Molecule();
      Importable inf;
      String name = molURL.getFile();
      if (name.regionMatches(true, name.length() - 4, ".pdb", 0, 4)) {
        inf = new ImportPDB();
      } else {
        inf = new ImportCfg();
      }
      inf.importModel(is, mol, chain);
      is.close();
    } catch (Exception e) {
    }

    return (mol);

  } // end readMolecule


  /** Reads parameters
   * @param paramURL URL of the parameters
   */

  public void readParameter(URL paramURL) {

    try {
      InputStream is = paramURL.openStream();
      ParameterImporter inf;
      inf = new ParamCfg();
      inf.importParameter(is, this);
      is.close();
    } catch (Exception e) {
    }

  } // end readParameter


  /** Loader thread run() method
   */

  public void run() {

    // load differently for each type
    switch (loadType) {

      case LOADMOLECULE:
        mol = readMolecule(loadURL, generalMolChain);
        loadRend.setMolecule(mol);
        loadSimu.setMolecule(mol);
        generalCenterMolecule = true;
        break;

      case LOADPARAMETER:
        readParameter(loadURL);
        setSimulationSettings(loadSimu);
        break;

    } // end switch

    // destroy thread object
    loadThread = null;

  } // end run


  /** Starts a loader thread
   * @param sync load synchronously or asynchonously
   * @param type type of the object to load (LOAD... constants)
   * @param url URL of the object to load
   * @param rend Renderer object
   * @param simu MDSimulation object
   */

  private boolean startLoad(boolean sync, int type, URL url, Renderer rend, MDSimulation simu) {

    // is url not present or loader busy?
    if ((url == null) || (loadThread != null)) {
      return (false);
    } // end

    // set loader information
    loadType = type;
    loadURL = url;
    loadRend = rend;
    loadSimu = simu;

    if (sync) {
      // run loader ourselves
      run();
    } else {
      // start loader thread
      loadThread = new Thread(this, "Loader");
      loadThread.start();
    } // end else

    // return success
    return (true);

  } // end startLoad


  /** Aborts a loading operation
   * @param type type of the loading operation
   */

  private void abortLoad(int type) {

    if ((loadThread != null) && (type == loadType)) {
      loadThread.stop();
      loadThread = null;
    } // end if

  } // end abortLoad


  /** Loads a molecule
   * @param rend Renderer object
   * @param simu MDSimulation object
   * @param sync load synchronously or asynchonously
   * @return success (true for okay, false for loader busy or error)
   */

  public boolean loadMolecule(Renderer rend, MDSimulation simu, boolean sync) {

    return (startLoad(sync, LOADMOLECULE, makeURL(generalMolName),
            rend, simu));

  } // end loadMolecule


  /** Aborts loading of a molecule
   */

  public void abortMolecule() {

    abortLoad(LOADMOLECULE);

  } // end abortMolecule


  /** Loads parameters
   * @param rend Renderer object
   * @param simu MDSimulation object
   * @param sync load synchronously or asynchonously
   * @return success (true for okay, false for loader busy or error)
   */

  public boolean loadParameter(Renderer rend, MDSimulation simu, boolean sync) {

    return (startLoad(sync, LOADPARAMETER, makeURL(generalParamName),
            rend, simu));

  } // end loadParameter


  /** Aborts loading of a parameter file
   */

  public void abortParameter() {

    abortLoad(LOADPARAMETER);

  } // end abortParameter


  /** Gets a string form the command line arguments
   * @param name name of the parameter
   * @return string of the parameter (null if not found)
   */

  private String getArgString(String name) {

    // step through arguments (every 2nd is a keyword)
    for (int i = 0; i < args.length; i = i + 2) {
      if (args[i].equalsIgnoreCase(name)) {
        if (i < args.length - 1) {
          return (args[i + 1]);
        } // end if
      } // end if
    } // end for

    // none found
    return (null);

  } // end getArgString


  /** Gets the value of a text parameter
   * @param name name of the parameter
   * @return string parameter
   */

  protected String getText(String name) {

    if (app != null) {
      return (app.getParameter(name));
    } else {
      return (getArgString(name));
    }

  } // end getText


  /** Gets the value of a "switch" parameter
   * @param name name of the switch parameter
   * @param def default switch status
   * @return boolean switch value
   */

  protected boolean getSwitch(String name, boolean def) {

    // get parameter
    String str = getText(name);

    // switch available?
    if (str == null) {
      return (def);
    }

    // is it switched on?
    if (str.equalsIgnoreCase("1") ||
            str.equalsIgnoreCase("TRUE") ||
            str.equalsIgnoreCase("Y") ||
            str.equalsIgnoreCase("YES")) {
      return (true);
      // is it switched off?
    } else if (str.equalsIgnoreCase("0") ||
            str.equalsIgnoreCase("FALSE") ||
            str.equalsIgnoreCase("N") ||
            str.equalsIgnoreCase("NO")) {
      return (false);
      // use default status
    } else {
      return (def);
    }

  } // end getSwitch


  /** Getss the value of a "color" parameter
   * @param name name of the parameter
   * @param def default color value
   * @return Color value
   */

  protected Color getColor(String name, Color def) {

    // get parameter
    String str = getText(name);

    // color available?
    if (str == null) {
      return (def);
    }

    // color usable?
    try {
      return (new Color(Integer.valueOf(str, 16).intValue()));
      // use default color
    } catch (NumberFormatException e) {
      return (def);
    }

  } // end getColor


  /** All argument parsing is done here
   * @param rend Renderer object
   * @param simu MDSimulation object
   */

  protected void parseParams(Renderer rend, MDSimulation simu) {

    // automatic rotation
    generalAutoRotation = getSwitch("autorotation", true);

    // double buffering
    rendDoubleBuffer = getSwitch("doublebuffer", true);

    // depth cueing
    rendDepthCueing = getSwitch("depthcue", true);

    // depth cueing
    String str = getText("renderstyle");
    if (str != null) {
      if (str.equalsIgnoreCase("LINE")) {
        rendStyle = BallRenderer.LINE;
      } else if (str.equalsIgnoreCase("BALL")) {
        rendStyle = BallRenderer.BALL;
      } else if (str.equalsIgnoreCase("LINEBALL")) {
        rendStyle = BallRenderer.LINEBALL;
      }
    } // end if

    // molecule model data
    generalMolName = getText("molecule");
    generalMolChain = getText("chain");
    loadMolecule(rend, simu, true);

    // parameter data
    generalParamName = getText("parameter");
    loadParameter(rend, simu, true);

    // background color
    rend.setBackgroundColor(getColor("backcolor", Color.black));

    // background image
    String imgName = getText("backimage");
    try {
      if (imgName != null) {
        Image backImg;
        if (app != null) {
          backImg = app.getImage(app.getCodeBase(), imgName);
        } else {
          backImg = Toolkit.getDefaultToolkit().getImage(new URL(imgName));
        }
        rend.setBackgroundImage(backImg);
      } // end if
    } catch (MalformedURLException e) {
    }

    // set all renderer settings
    setRendererSettings(rend);

  } // end parseParams


  /** Gets all arguments from an applet
   * @param app Applet where to get the parameters from
   * @param rend Renderer object
   * @param simu MDSimulation object
   */

  public void getParams(Applet app, Renderer rend, MDSimulation simu) {

    this.app = app;
    this.args = null;
    parseParams(rend, simu);

  } // getParams


  /** Gets all arguments from an application's command line
   * @param args command line arguments where to get the parameters from
   * @param rend Renderer object
   * @param simu MDSimulation object
   */

  public void getParams(String[] args, Renderer rend, MDSimulation simu) {

    this.app = null;
    this.args = args;
    parseParams(rend, simu);

  } // getParams


} // end Settings
