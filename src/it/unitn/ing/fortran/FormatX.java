package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This class represents an X format element.
*/

class FormatX extends FormatElement {
  public void write(FormatOutputList vp, PrintStream out) {
    out.print(" ");
  }


  public void read(FormatInputList vp,
                   InputStreamAndBuffer in,
                   FormatMap format_map
                   ) {
    in.advance(1);
  }


  public String toString() {
    return "X";
  }
}


