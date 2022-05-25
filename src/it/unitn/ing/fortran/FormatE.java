package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This class represents an Ew.d format element.
   Numbers should be output as
     s0.dd...ddEsdd
   where s is a sign.
*/

class FormatE extends FormatIOElement {
  int d;


  public FormatE(int w, int d) {
    setWidth(w);
    this.d = d;
  }


  String convertToString(Object o, int vecptr)
          throws IllegalObjectOnWriteException,
          NumberTooWideOnWriteException {
    String s;

    /* Convert the number to a string. */
    if (o instanceof Integer || o instanceof Long ||
            o instanceof Float || o instanceof Double) {
      CJFormat cjf = new CJFormat();
      cjf.setWidth(getWidth());
      cjf.setPrecision(this.d);
      cjf.setPre("");
      cjf.setPost("");
      cjf.setLeadingZeroes(false);
      cjf.setShowPlus(false);
      cjf.setAlternate(false);
      cjf.setShowSpace(false);
      cjf.setLeftAlign(false);
      cjf.setFmt('E');
      s = cjf.form(((Number) o).doubleValue());

      /* Throw an exception if the string won't fit. */
      if (s.length() > getWidth())
        throw new NumberTooWideOnWriteException((Number) o,
                vecptr,
                this.toString()
        );
      else
        return s;
    } else
      throw new IllegalObjectOnWriteException(o,
              vecptr,
              this.toString()
      );
  }


  /* vp and in are used only in generating error messages.
  */
  Object convertFromString(String s,
                           FormatInputList vp,
                           InputStreamAndBuffer in
                           )
          throws InvalidNumberOnReadException {
    /* Parse the string to check it's a valid number,
       and convert if so.
    */
    NumberParser np =
            Parsers.theParsers().number_parser;
    np.ReInit(new StringBufferInputStream(s));
    try {
      int start = np.Float();
      Double d = new Double(s.substring(start));
      return d;
    } catch (ParseException e) {
      throw new InvalidNumberOnReadException(s,
              vp.getPtr(),
              this.toString(),
              in.getLineErrorReport(),
              e.getMessage()
      );
    } catch (TokenMgrError e) {
      throw new InvalidNumberOnReadException(s,
              vp.getPtr(),
              this.toString(),
              in.getLineErrorReport(),
              e.getMessage()
      );
    }
  }


  public String toString() {
    return "E" + getWidth() + "." + this.d;
  }
}


