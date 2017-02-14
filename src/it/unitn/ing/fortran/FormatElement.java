package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This class represents a single format element such as
   F12.5, I2, or X.
*/

public abstract class FormatElement extends FormatUniv {
  /* This method will be defined differently by each subclass.
  */
  public abstract void write(FormatOutputList vp, PrintStream out)
          throws OutputFormatException;
}


