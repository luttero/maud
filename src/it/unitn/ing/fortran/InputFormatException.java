package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This exception is a generic one, a superclass of all those
   thrown to report an error while doing formatted input.
*/

public abstract class InputFormatException extends Exception {
  public InputFormatException(String s) {
    super(s);
  }

  public InputFormatException() {
    super();
  }


}


