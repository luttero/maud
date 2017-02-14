package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This class represents a mapping from input data. We use it to specify,
   for example, that on input, an "X" should be replaced by a "0" before
   being interpreted by the formatted input routines.
   The user must provide an instance of this class, with getMapping
   defined. getMapping should return either null, if the input string
   is to be left as it is, or a replacement string.
*/

abstract class FormatMap {
  public abstract String getMapping(String in);
}


