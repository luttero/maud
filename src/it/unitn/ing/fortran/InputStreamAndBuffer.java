package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This class holds an input stream and a line buffer.
*/

class InputStreamAndBuffer {
  private DataInputStream in;
  // The stream we read from.

  private String line;
  // The line just read.

  private int ptr;
  // Initialised to 0 after reading a line. Index of the next
  // character to use in line.

  private int line_number;
  // Initially 0. Is incremented each time a line is read, so
  // the first line read is number 1.

  private boolean nothing_read;
  // Initially true. Is set false after reading a line. We
  // use this so that the first call of getSlice
  // knows to read a line.


  public InputStreamAndBuffer(DataInputStream in) {
    this.in = in;
    this.ptr = 0;
    this.line = "";
    this.line_number = 0;
    this.nothing_read = true;
  }


  /* Reads the next line into the line buffer.
     vecptr and format are used only in generating error messages.
  */
  public void readLine(int vecptr, FormatElement format)
          throws EndOfFileWhenStartingReadException,
          LineMissingOnReadException,
          IOExceptionOnReadException {
    try {
      String line = this.in.readLine();

      if (line == null) {
        if (this.nothing_read)
          throw new EndOfFileWhenStartingReadException(vecptr,
                  format.toString(),
                  this.line,
                  this.line_number
          );
        else
          throw new LineMissingOnReadException(vecptr,
                  format.toString(),
                  this.line,
                  this.line_number
          );
      } else {
        this.ptr = 0;
        this.nothing_read = false;
        this.line_number = this.line_number + 1;
        this.line = line;
        // Don't do the assignment until we've checked for a null
        // line, because then we can then use this.line as the
        // previous value for error messages.
      }
    } catch (IOException e) {
      throw new IOExceptionOnReadException(this.line, this.line_number,
              e.getMessage()
      );
    }
  }


  /* Returns a string consisting of the next width characters,
     and throws an exception if the line is not long enough.
     The 'vecptr' and 'format' parameters are used only in generating error
     messages.
  */
  public String getSlice(int width, int vecptr, FormatElement format)
          throws DataMissingOnReadException,
          LineMissingOnReadException,
          EndOfFileWhenStartingReadException,
          IOExceptionOnReadException {
    if (this.nothing_read)
      readLine(vecptr, format);
    if (this.ptr + width > this.line.length())
      throw new DataMissingOnReadException(vecptr,
              format.toString(),
              getLineErrorReport()
      );
    else {
      return this.line.substring(this.ptr, this.ptr + width);
    }
  }


  /* Advances the pointer by width.
  */
  public void advance(int width) {
    this.ptr = this.ptr + width;
  }


  /* Generates an error report showing the line, character pointer
     ptr and line number.
  */
  public String getLineErrorReport() {
    StringBuffer s = new StringBuffer();

    /* Report the line number. */
    s.append("  Line number = " + this.line_number + ":\n");

    /* Show the line. */
    s.append(this.line + "\n");

    /* Show an arrow under ptr. */
    for (int i = 0; i < this.ptr; i++)
      s.append(" ");
    s.append("^");

    return s.toString();
  }
}


