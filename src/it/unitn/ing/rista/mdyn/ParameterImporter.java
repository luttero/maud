package it.unitn.ing.rista.mdyn;

import java.lang.*;
import java.io.*;
//import Settings;


/** ParameterImporter
 * This interface defines the methods every import filter for
 * simulation parameters has to implement.
 *
 * @version 1.0 (08-Oct-96)
 * @author  Thilo Stoeferle
 * @author  Luca Luterotti
 */

public interface ParameterImporter {

  /** Import from the given InputStream using the Settings object
   * @param is InputStream containing the parameter settings
   * @param cfg Settings object where the parameters will be stored
   * @exception java.io.IOException when an I/O error has occured or data format does not match
   */

  void importParameter(InputStream is, Settings cfg)
          throws IOException;

} // end ParameterImporter
