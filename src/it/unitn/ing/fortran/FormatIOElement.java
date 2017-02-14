package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This class represents a format element that reads or writes
   data. So F12.5 or I3, but not X.
   We assume that all format elements are fixed width.
*/

public abstract class FormatIOElement extends FormatElement {
  private int width;

  void setWidth(int width) {
    this.width = width;
  }

  int getWidth() {
    return this.width;
  }


  public void write(FormatOutputList vp, PrintStream out)
          throws OutputFormatException {
    vp.checkCurrentElementForWrite(this);
    Object o = vp.getCurrentElementAndAdvance();
    out.print(convertToString(o, vp.getPtr() - 1));
  }


  /* This method is called by write, above. It will be
     defined differently for each subclass of FormatIOElement.
     The idea is that getting the next element to write from
     the output list, and printing its string representation,
     are the same for all FormatIOElements. However, the
     conversion to string is different for each one.
  */
  abstract String convertToString(Object o, int vecptr)
          throws OutputFormatException;


  public void read(FormatInputList vp,
                   InputStreamAndBuffer in,
                   FormatMap format_map
                   )
          throws InputFormatException {
    /* Get next width characters. */
    String s = in.getSlice(this.width, vp.getPtr(), this);

    /* Try translating if there's a format map. */
    if (format_map != null) {
      String repl = format_map.getMapping(s);
      if (repl != null)
        s = repl;
    }

    /* Parse the string to check it's a valid number, and put into
       the vector if so.
       Also, advance the stream input pointer.
    */
    Object o = convertFromString(s, vp, in);
    vp.checkCurrentElementForRead(this, in);
    vp.putElementAndAdvance(o, this, in);
    in.advance(this.width);
  }


  /* This method is called by read, above. It will be
     defined differently for each subclass of FormatIOElement.
     The idea is that getting the next element to read from
     the input stream, and putting it into the input list,
     are the same for all FormatIOElements. However, the
     conversion from string is different for each one.
     vp and in are used only in generating error messages.
  */
  abstract Object convertFromString(String s,
                                    FormatInputList vp,
                                    InputStreamAndBuffer in
                                    )
          throws InputFormatException;
}


