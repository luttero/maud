package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* Below, we define various classes for holding complete formats,
   format elements, and so on. The class FormatUniv is a superclass
   of them all. This makes it a convenient "universal type" to
   use to hold any piece of, or a complete, format.
*/

public abstract class FormatUniv {
  abstract void write(FormatOutputList vp, PrintStream out)
          throws OutputFormatException;

  abstract void read(FormatInputList vp,
                     InputStreamAndBuffer in,
                     FormatMap format_map
                     )
          throws InputFormatException;
}


