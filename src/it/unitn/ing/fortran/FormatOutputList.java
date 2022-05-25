package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

interface FormatOutputList {
  boolean hasCurrentElement();

  void checkCurrentElementForWrite(FormatElement format_element)
          throws EndOfVectorOnWriteException;

  Object getCurrentElement();

  Object getCurrentElementAndAdvance();

  /* Returns the current pointer.
     Used only in generating error messages.
  */
  int getPtr();
}


