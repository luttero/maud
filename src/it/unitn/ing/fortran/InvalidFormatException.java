package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This exception is thrown when a syntax error is detected while
   parsing a format string.
*/

public class InvalidFormatException extends Exception {
  public InvalidFormatException(String parser_message) {
    super(parser_message);
  }

  public InvalidFormatException() {
    super();
  }
}


