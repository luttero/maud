package it.unitn.ing.rista.mdyn;

import java.lang.*;
import java.io.*;


/** Importable
 * This interface defines the methods every import filter for
 * chemical model descriptions has to implement.
 *
 * @version 1.0 (10-Jul-96)
 * @author  Thilo Stoeferle
 * @author  Luca Luterotti
 */

public interface Importable {

  /** Import from the given InputStream using the Vectors
   * for atom and body structure data.
   * @param is InputStream containing the molecule description
   * @param mol Molecule object where the data will be stored
   * @param chain Name of chain to read
   * @exception java.io.IOException when an I/O error has occured or data format does not match
   */

  void importModel(InputStream is, Molecule mol, String chain)
          throws IOException;

} // end Importable
