package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

public class DataMissingOnReadException extends InputFormatException {
  public DataMissingOnReadException(int vecptr,
                                    String format,
                                    String line_error_report
                                    ) {
    this("End of line while reading formatted data:\n" +
            "  Index  = " + vecptr + "\n" +
            "  Format = " + format + "\n" +
            line_error_report
    );
  }

  public DataMissingOnReadException(String s) {
    super(s);
  }

  public DataMissingOnReadException() {
    super();
  }
}


