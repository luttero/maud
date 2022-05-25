package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

public class KeyNotStringOnReadException extends InputFormatException {
  public KeyNotStringOnReadException(Object key,
                                     int vecptr,
                                     String format,
                                     String line_error_report
                                     ) {
    this("Key not string while reading formatted data:\n" +
            "  Key    = \"" + vecptr + "\"\n" +
            "  Index  = " + vecptr + "\n" +
            "  Format = " + format + "\n" +
            line_error_report + "\n"
    );
  }

  public KeyNotStringOnReadException(String s) {
    super(s);
  }

  public KeyNotStringOnReadException() {
    super();
  }
}


