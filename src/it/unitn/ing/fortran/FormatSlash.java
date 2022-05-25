package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This class represents an / item.
*/

class FormatSlash extends FormatElement {
  public void write(FormatOutputList vp, PrintStream out) {
    out.println();
  }


  public void read(FormatInputList vp,
                   InputStreamAndBuffer in,
                   FormatMap format_map
                   )
          throws InputFormatException {
    in.readLine(vp.getPtr(), this);
  }


  public String toString() {
    return "/";
  }
}


