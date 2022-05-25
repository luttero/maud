package it.unitn.ing.rista.mdyn;

import java.lang.*;
import java.io.*;


/** ParamCfg
 * This interface implements an import filter for ".cfg" files.
 *
 * @version 1.0 (08-Oct-96)
 * @author  Thilo Stoeferle
 * @author  Luca Luterotti
 */

public class ParamCfg
        extends Object
        implements ParameterImporter {


  /** Import from the given InputStream using the Settings object
   * @param is InputStream containing the parameter settings
   * @param cfg Settings object where the parameters will be stored
   * @exception java.io.IOException when an I/O error has occured or data format does not match
   */

  public void importParameter(InputStream is, Settings cfg)
          throws IOException {

    // create a StreamTokenizer for the InputStream
    StreamTokenizer st = new StreamTokenizer(new BufferedInputStream(is, 4000));
    st.eolIsSignificant(true);
    st.wordChars('A', 'z');
    st.commentChar(';');

    boolean notdone = true;
    boolean islinebegin = true;
    String tag = "";

    // parse stream
    while (notdone) {

      switch (st.nextToken()) {

        case StreamTokenizer.TT_EOF:
          notdone = false;
          break;
        case StreamTokenizer.TT_WORD:
          if (islinebegin) {
            if (st.sval.equals("ENDPARAM")) {
              notdone = false;
            } else {
              tag = new String(st.sval);
              islinebegin = false;
            }
          } else {
            parseParam(cfg, tag, st.sval);
          }
          break;
        case StreamTokenizer.TT_NUMBER:
          if (!(islinebegin)) {
            parseParam(cfg, tag, new Double(st.nval).toString());
          }
          break;
        case StreamTokenizer.TT_EOL:
          islinebegin = true;
          break;

      }  // end switch

    } // end while


  } // end importParameter


  /** Parse a single parameter / value pair
   * @param cfg Settings object
   * @param name name tag of the parameter
   * @param value value tag of the parameter
   */

  private void parseParam(Settings cfg, String name, String value) {

    double dblval;
    try {
      dblval = new Double(value).doubleValue();
    } catch (NumberFormatException e) {
      dblval = 0;
    }

    if (name.equals("Int_stepsize")) {
      cfg.simuIntStep = dblval * 2.01;   // reduced to ps
    } else if (name.equals("BS_bondlength")) {
      cfg.fhookeEqual = dblval * 0.38;   // reduced (sigma) to nm
    } else if (name.equals("BS_hookeforce")) {
      cfg.fhookeHooke = dblval * 3.4626; // reduced to kJ/mol nm^2
    } else if (name.equals("BS_bendangle")) {
      cfg.fbendEqual = Math.acos(-dblval) * 180 / Math.PI;
    } else if (name.equals("BS_bendforce")) {
      cfg.fbendBend = dblval * 1.3158;   // reduced to kJ/mol nm
    } else if (name.equals("Cutoff")) {
      cfg.fvdwCutoff = dblval;
    } else if (name.equals("Temperature")) {
      cfg.simuInitTemp = dblval * 60.1;  // reduced to K
    }

  } // end parseParam


} // end ParamCfg
