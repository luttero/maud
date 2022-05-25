package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This class represents a repeated item, e.g. 3F12.5 or 3X.
   The integer r gives the repetition factor.
   The item may be either a format element, or an entire format.
   To cater for either, we hold it in a FormatUniv object (this is
   why we introduced the class FormatUniv).
*/

public class FormatRepeatedItem extends FormatUniv {
  private int r = 1;
  private FormatUniv format_univ = null;


  public FormatRepeatedItem(FormatUniv format_univ) {
    this(1, format_univ);
  }

  public FormatRepeatedItem(int r, FormatUniv format_univ) {
    this.r = r;
    this.format_univ = format_univ;
  }


  public void write(FormatOutputList vp, PrintStream out)
          throws OutputFormatException {
    for (int i = 1; i <= this.r; i++)
      this.format_univ.write(vp, out);
  }


  public void read(FormatInputList vp,
                   InputStreamAndBuffer in,
                   FormatMap format_map
                   )
          throws InputFormatException {
    for (int i = 1; i <= this.r; i++)
      this.format_univ.read(vp, in, format_map);
  }


  public String toString() {
    if (r == 1)
      return this.format_univ.toString();
    else
      return this.r + "(" + this.format_univ.toString() + ")";
  }
}


