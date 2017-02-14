package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

public class InvalidNumberOnReadException extends InputFormatException {
  public InvalidNumberOnReadException(String number,
                                      int vecptr,
                                      String format,
                                      String line_error_report,
                                      String parser_message
                                      ) {
    this("Invalid number while reading formatted data:\n" +
            "  Number = \"" + number + "\"\n" +
            "  Index  = " + vecptr + "\n" +
            "  Format = " + format + "\n" +
            line_error_report + "\n" +
            parser_message
    );
  }

  public InvalidNumberOnReadException(String s) {
    super(s);
  }

  public InvalidNumberOnReadException() {
    super();
  }
}


