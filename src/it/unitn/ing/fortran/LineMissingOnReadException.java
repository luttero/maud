package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

public class LineMissingOnReadException extends InputFormatException {
  public LineMissingOnReadException(int vecptr,
                                    String format,
                                    String line,
                                    int line_number
                                    ) {
    this("End of file while reading formatted data:\n" +
            "  Index  = " + vecptr + "\n" +
            "  Format = " + format + "\n" +
            "Last line was number " + line_number + ":\n" +
            line
    );
  }

  public LineMissingOnReadException(String s) {
    super(s);
  }

  public LineMissingOnReadException() {
    super();
  }
}


