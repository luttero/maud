package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This exception is thrown if formatted output runs off the
   end of the vector being output before it has completed the
   format.
*/

public class EndOfVectorOnWriteException extends OutputFormatException {
  public EndOfVectorOnWriteException(int vecptr,
                                     String format
                                     ) {
    this("End of vector while writing formatted data:\n" +
            "  Index  = " + vecptr + "\n" +
            "  Format = " + format + " ."
    );
  }

  public EndOfVectorOnWriteException(String s) {
    super(s);
  }

  public EndOfVectorOnWriteException() {
    super();
  }
}


