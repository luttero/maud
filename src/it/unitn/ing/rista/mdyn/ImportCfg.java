package it.unitn.ing.rista.mdyn;

import java.lang.*;
import java.util.*;
import java.io.*;


/** ImportCfg
 * This class implements an import filter for ".cfg" files.
 * (GEOHMC format)
 *
 * @version 1.0 (23-Sep-96)
 * @author  Thilo Stoeferle
 * @author  Luca Luterotti
 */

public class ImportCfg
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
    boolean inatommode = false;

    // only numeric chain names
    int chainnum = 0;
    if (chain != null) {
      if (chain.length() > 0) {
        chainnum = Integer.valueOf(chain).intValue();
      } // end if
    } // end if

    // parse stream
    while (notdone) {

      switch (st.nextToken()) {

        case StreamTokenizer.TT_EOF:
          notdone = false;
          break;
        case StreamTokenizer.TT_WORD:
          if (islinebegin) {
            if (st.sval.equals("ENDPARAM")) {
              inatommode = true;
            } else {
              islinebegin = false;
            }
          } // end if
          break;
        case StreamTokenizer.TT_NUMBER:
          if (islinebegin) {
            if (st.nval < chainnum) {
              islinebegin = false;
            } else if (st.nval == chainnum) {
              readAtom(st, mol.atoms);
              islinebegin = false;
            } else if (st.nval > chainnum) {
              notdone = false;
            }
          } // end if
          break;
        case StreamTokenizer.TT_EOL:
          islinebegin = true;
          break;

      }  // end switch

    } // end while

    markEnds(mol.atoms);
    createBonds(mol.atoms.size(), mol.body);

  } // end importModel


  /** Reads a single atom definition and adds it to the Vector
   * @param st the input StreamTokenizer
   * @param atoms the Vector of Atoms
   * @exception java.io.IOException
   */

  private void readAtom(StreamTokenizer st, Vector atoms)
          throws IOException {

    int num;
    double x,y,z;

    // read serial number
    st.nextToken();
    num = new Double(st.nval).intValue();

    // read coordinates
    st.nextToken();
    x = st.nval * 0.38; // convert reduced to Nanometers
    st.nextToken();
    y = st.nval * 0.38; // convert reduced to Nanometers
    st.nextToken();
    z = st.nval * 0.38; // convert reduced to Nanometers

    // add atom
    atoms.addElement(new Atom("M", x, y, z));

  } // end readAtom


  /** Mark ending monomers
   * @param atoms the Vector of atoms
   */

  private void markEnds(Vector atoms) {

    if (atoms.size() > 0) {
      ((Atom) atoms.firstElement()).setName("E");
      ((Atom) atoms.lastElement()).setName("E");
    } // end if

  } // end markEnds


  /** Creates linear bonds and adds them to the Vector
   * @param numatoms number of atoms
   * @param body the Vector of body structure
   */

  private void createBonds(int numatoms, Vector body) {

    for (int i = 0; i < numatoms - 1; i++) {
      Bond bond = new Bond(i);
      body.addElement(bond);
      bond.addBond(i + 1);
    } // end for

  } // end createBonds

} // end ImportPDB
