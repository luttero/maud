package it.unitn.ing.rista.mdyn;

import java.lang.*;
import java.util.*;
import java.io.*;


/** ImportPDB
 * This class implements an import filter for PDB format files.
 * It does not respect the Fortran-style data columns but instead
 * does parsing on a per-data-item basis in a way that all data columns
 * have to be present (which is not the correct behavior!).
 * The Protein Data Bank format for storing molecule data has been
 * defined by the Brookhaven National Laboratory, New York USA.
 *
 * @version 1.0 (10-Jul-96)
 * @author  Thilo Stoeferle
 * @author  Luca Luterotti
 */

public class ImportPDB
        extends Object
        implements Importable {


  /** Import from the given InputStream using the Vectors
   * for atom and body structure data.
   * @param is InputStream containing the molecule description
   * @param mol Molecule object where the data will be stored
   * @param chain Name of chain to read
   * @exception java.io.IOException when an I/O error has occured or data format does not match
   */

  public void importModel(InputStream is, Molecule mol, String chain)
          throws IOException {

    // create a StreamTokenizer for the InputStream
    StreamTokenizer st = new StreamTokenizer(new BufferedInputStream(is, 4000));
    st.eolIsSignificant(true);
    st.commentChar('#');

    boolean notdone = true;
    boolean islinebegin = true;

    // parse stream
    while (notdone) {

      switch (st.nextToken()) {

        case StreamTokenizer.TT_EOF:
          notdone = false;
          break;
        case StreamTokenizer.TT_WORD:
          if (islinebegin) {
            if (st.sval.equals("ATOM")) {
              readAtom(st, mol.atoms, chain);
              islinebegin = false;
            } else if (st.sval.equals("CONECT")) {
              readBonds(st, mol.body);
              islinebegin = true;
            }
          } // end if
          break;
        case StreamTokenizer.TT_EOL:
          islinebegin = true;
          break;

      }  // end switch

    } // end while

  } // end importModel


  /** Reads a single atom definition and adds it to the Vector
   * @param st the input StreamTokenizer
   * @param atoms the Vector of Atoms
   * @param chain selected chain
   * @exception java.io.IOException
   */

  private void readAtom(StreamTokenizer st, Vector atoms, String chain)
          throws IOException {

    String name;
    int num;
    double x,y,z;

    // read serial number
    st.nextToken();
    num = new Double(st.nval).intValue();

    // read name
    st.nextToken();
    name = new String(st.sval);

    // skip residue name
    st.nextToken();

    // read chain
    st.nextToken();
    if (chain != null) {
      if (chain.length() > 0) {
        if (!(chain.equals(st.sval))) {
          return;
        } // end if
      } // end if
    } // end if

    // skip residue sequence number
    st.nextToken();

    // read coordinates
    st.nextToken();
    x = st.nval * 0.1; // convert Angstroems to Nanometers
    st.nextToken();
    y = st.nval * 0.1; // convert Angstroems to Nanometers
    st.nextToken();
    z = st.nval * 0.1; // convert Angstroems to Nanometers

    // add atom
    atoms.addElement(new Atom(name, x, y, z));

  } // end readAtom


  /** Reads the bonds of a single atom and adds them to the Vector
   * @param st the input StreamTokenizer
   * @param body the Vector of body structure
   * @exception java.io.IOException
   */

  private void readBonds(StreamTokenizer st, Vector body)
          throws IOException {

    // read serial number of base atom
    st.nextToken();
    Bond bond = new Bond(new Double(st.nval).intValue() - 1);
    body.addElement(bond);

    // read bonds
    while (true) {
      switch (st.nextToken()) {
        case StreamTokenizer.TT_EOL:
        case StreamTokenizer.TT_EOF:
          return;
        case StreamTokenizer.TT_NUMBER:
          bond.addBond(new Double(st.nval).intValue() - 1);
          break;
      } // end switch
    } // end while

  } // end readBonds

} // end ImportPDB
