package it.unitn.ing.fortran;

import java.io.IOException;

/* This exception is a generic one, a superclass of all those
   thrown to report an error while doing formatted output.
*/

public abstract class OutputFormatException extends Exception {
  public OutputFormatException(String s) {
    super(s);
  }

  public OutputFormatException() {
    super();
  }
}


