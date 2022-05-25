package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This class represents a Vector of Strings and a current-element pointer.
   We use it when inputting data against a format.
*/

class StringsHashtableAndPointer implements FormatInputList {
  private VectorAndPointer vp;
  private Hashtable ht;


  public StringsHashtableAndPointer(Vector strings, Hashtable ht) {
    this.vp = new VectorAndPointer(strings);
    this.ht = ht;
  }


  /* Checks that there is a current element in the key vector, and
     throws an exception if not.
     format_element and in are only for generating error messages.
  */
  public void checkCurrentElementForRead(FormatElement format_element,
                                         InputStreamAndBuffer in
                                         )
          throws EndOfKeyVectorOnReadException {
    if (!(this.vp.hasCurrentElement()))
      throw new EndOfKeyVectorOnReadException(this.vp.getPtr(),
              format_element.toString(),
              in.getLineErrorReport()
      );
  }


  /* Puts o into the input list and advances its pointer.
     In this implementation, that means getting the current key,
     putting o into an appropriate hashtable slot, and advancing
     the pointer in the vector of keys.
     format_element and in are only for generating error messages.
  */
  public void putElementAndAdvance(Object o,
                                   FormatElement format_element,
                                   InputStreamAndBuffer in
                                   )
          throws KeyNotStringOnReadException {
    Object current_key = this.vp.getCurrentElement();
    if (current_key instanceof String) {
      this.ht.put((String) current_key, o);
      this.vp.advance();
    } else
      throw new KeyNotStringOnReadException(current_key,
              this.vp.getPtr(),
              format_element.toString(),
              in.getLineErrorReport()
      );
  }


  /* Returns the current pointer.
     Used only in generating error messages.
  */
  public int getPtr() {
    return this.vp.getPtr();
  }
}


