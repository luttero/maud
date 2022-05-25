package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This exception is thrown if formatted output detects a number
   that won't fit in its format, e.g. trying to output 1234
   against an I3 element.
*/

public class NumberTooWideOnWriteException extends OutputFormatException {
  public NumberTooWideOnWriteException(Number n,
                                       int vecptr,
                                       String format
                                       ) {
    this("Number too wide while writing formatted data:\n" +
            "  Number = \"" + n + "\"\n" +
            "  Index  = " + vecptr + "\n" +
            "  Format = " + format + " ."
    );
  }

  public NumberTooWideOnWriteException(String s) {
    super(s);
  }

  public NumberTooWideOnWriteException() {
    super();
  }
}


