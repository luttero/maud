package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

public class UnmatchedStringOnReadException extends InputFormatException {
  public UnmatchedStringOnReadException(String string,
                                        int vecptr,
                                        String format,
                                        String line_error_report
                                        ) {
    this("Unmatched string while reading formatted data:\n" +
            "  String = \"" + string + "\"\n" +
            "  Index  = " + vecptr + "\n" +
            "  Format = " + format + "\n" +
            line_error_report + "\n"
    );
  }

  public UnmatchedStringOnReadException(String s) {
    super(s);
  }

  public UnmatchedStringOnReadException() {
    super();
  }
}


