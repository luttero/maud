package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

public class IOExceptionOnReadException extends InputFormatException {
  public IOExceptionOnReadException(String line,
                                    int line_number,
                                    String IOMessage
                                    ) {
    this("IOException while reading formatted data:\n" +
            "Last line was number " + line_number + ":\n" +
            line + "\n" +
            IOMessage
    );
  }

  public IOExceptionOnReadException(String s) {
    super(s);
  }

  public IOExceptionOnReadException() {
    super();
  }
}


