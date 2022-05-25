package it.unitn.ing.fortran;

import java.io.PrintStream;

/* This class represents an embedded literal, e.g. 'Title'.
   toXRDcatString() does not yet handle embedded quotes.
*/

class FormatString extends FormatElement {
  private String s;


  public FormatString(String s) {
    this.s = s;
  }


  public void write(FormatOutputList vp, PrintStream out) {
    out.print(this.s);
  }


  public void read(FormatInputList vp,
                   InputStreamAndBuffer in,
                   FormatMap format_map
                   )
          throws InputFormatException {
    String s = in.getSlice(this.s.length(), vp.getPtr(), this);
    if (!(this.s.equals(s)))
      throw new UnmatchedStringOnReadException(s,
              vp.getPtr(),
              this.toString(),
              in.getLineErrorReport()
      );
    in.advance(this.s.length());
  }


  public String toString() {
    return "'" + this.s + "'";
  }
}


