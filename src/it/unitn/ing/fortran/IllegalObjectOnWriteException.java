package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This exception is thrown if formatted output detects an object
   that's the wrong type for a format element, e.g. a real
   when outputting against an Iw element.
*/

public class IllegalObjectOnWriteException extends OutputFormatException {
  public IllegalObjectOnWriteException(Object o,
                                       int vecptr,
                                       String format
                                       ) {
    this("Illegal object while writing formatted data:\n" +
            "  Object = \"" + o + "\"\n" +
            "  Index  = " + vecptr + "\n" +
            "  Format = " + format + " ."
    );
  }

  public IllegalObjectOnWriteException(String s) {
    super(s);
  }

  public IllegalObjectOnWriteException() {
    super();
  }
}


