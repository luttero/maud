package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This exception is thrown if formatted output detects a string
   that won't fit in its format, e.g. trying to output abcde
   against an A4 element.
*/

public class StringTooWideOnWriteException extends OutputFormatException {
  public StringTooWideOnWriteException(String s,
                                       int vecptr,
                                       String format
                                       ) {
    this("String too wide while writing formatted data:\n" +
            "  String = \"" + s + "\"\n" +
            "  Index  = " + vecptr + "\n" +
            "  Format = " + format + " ."
    );
  }

  public StringTooWideOnWriteException(String s) {
    super(s);
  }

  public StringTooWideOnWriteException() {
    super();
  }
}


