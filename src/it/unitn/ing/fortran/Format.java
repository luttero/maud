package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This class represents a complete format, i.e. a sequence of
   elements such as F12.5 and so on. Some of the elements may
   themselves be formats.
   We implement it as a vector of elements.
*/

public class Format extends FormatUniv {
  private Vector elements = new Vector();


  public Format(String s) throws InvalidFormatException {
    FormatParser fp =
            Parsers.theParsers().format_parser;
    fp.ReInit(new StringBufferInputStream(s));
    try {
      Format f = fp.Format();
      this.elements = f.elements;
    } catch (ParseException e) {
      throw new InvalidFormatException(e.getMessage());
    } catch (TokenMgrError e) {
      throw new InvalidFormatException(e.getMessage());
    }
  }

  // We call this one from inside the parser, which needs a Format
  // with its vector initialised.
  Format() {
  }


  public void addElement(FormatUniv fu) {
    this.elements.addElement(fu);
  }


  public void write(FormatOutputList vp, PrintStream out)
          throws OutputFormatException {
    for (int i = 0; i < this.elements.size(); i++) {
      FormatUniv fu = (FormatUniv) this.elements.elementAt(i);
      fu.write(vp, out);
    }
  }


  public void read(FormatInputList vp,
                   InputStreamAndBuffer in,
                   FormatMap format_map
                   )
          throws InputFormatException {
    for (int i = 0; i < this.elements.size(); i++) {
      FormatUniv fu = (FormatUniv) this.elements.elementAt(i);
      fu.read(vp, in, format_map);
    }
  }


  public String toString() {
    String s = "";
    for (int i = 0; i < this.elements.size(); i++) {
      if (i != 0)
        s = s + ", ";
      s = s + this.elements.elementAt(i).toString();
    }
    return s;
  }
}


