package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This class represents a Vector and a current-element pointer.
   We use it when outputting or inputting a Vector against a format:
   the pointer keeps track of the current element being output, and
   can be incremented by the format write and read methods.
*/

class VectorAndPointer implements FormatInputList, FormatOutputList {
  private Vector v = null;
  private int vecptr = 0;
  // On output, vecptr points at the next element to be used.
  // On input, it points at the next free slot to be filled.


  public VectorAndPointer(Vector v) {
    this.v = v;
  }


  public VectorAndPointer() {
    this.v = new Vector();
  }


  public boolean hasCurrentElement() {
    return (this.vecptr < this.v.size());
  }


  public void checkCurrentElementForWrite(FormatElement format_element)
          throws EndOfVectorOnWriteException {
    if (!hasCurrentElement())
      throw new EndOfVectorOnWriteException(this.vecptr,
              format_element.toString()
      );
  }


  /* Checks that the current element in the input list is OK and
     throws an exception if not. For this implementation of
     FormatInputList, there are no error conditions - we
     introduced the method for the StringHashtableAndPointer class,
     and need it here for compatibility.
     format_element and in are only for generating error messages.
  */
  public void checkCurrentElementForRead(FormatElement format_element,
                                         InputStreamAndBuffer in
                                         ) {
  }


  public Object getCurrentElement() {
    return this.v.elementAt(this.vecptr);
  }

  public Object getCurrentElementAndAdvance() {
    this.vecptr = this.vecptr + 1;
    return this.v.elementAt(this.vecptr - 1);
  }


  /* Puts o into the input list and advances its pointer.
     format_element and in are only for generating error messages,
     and not used in this implementation, since no error conditions
     can arise.
  */
  public void putElementAndAdvance(Object o,
                                   FormatElement format_element,
                                   InputStreamAndBuffer in
                                   ) {
    this.v.addElement(o);
    this.vecptr = this.vecptr + 1;
  }


  public void advance() {
    this.vecptr = this.vecptr + 1;
  }


  /* Returns the current pointer.
     Used only in generating error messages.
  */
  public int getPtr() {
    return this.vecptr;
  }
}


