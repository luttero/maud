/* Formatter.java */

package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This class holds a Format, and has methods for reading and
   writing data against it.
*/

public class Formatter {
  private Format format = null;
  private FormatMap format_map = null;


  public Formatter(String format) throws InvalidFormatException {
    this(new Format(format));
  }

  public Formatter(Format format) {
    this.format = format;
  }


  public void setFormatMap(FormatMap format_map) {
    this.format_map = format_map;
  }


  public void write(Vector v, PrintStream out)
          throws OutputFormatException {
    FormatOutputList vp = new VectorAndPointer(v);
    this.format.write(vp, out);
  }

  public void write(int i, PrintStream out)
          throws OutputFormatException {
    write(new Integer(i), out);
  }

  public void write(long l, PrintStream out)
          throws OutputFormatException {
    write(new Long(l), out);
  }

  public void write(float f, PrintStream out)
          throws OutputFormatException {
    write(new Float(f), out);
  }

  public void write(double d, PrintStream out)
          throws OutputFormatException {
    write(new Double(d), out);
  }

  public void write(Object o, PrintStream out)
          throws OutputFormatException {
    Vector v = new Vector();
    v.addElement(o);
    write(v, out);
  }


  public void read(Vector v, DataInputStream in)
          throws InputFormatException {
    FormatInputList vp = new VectorAndPointer(v);
    InputStreamAndBuffer inb = new InputStreamAndBuffer(in);
    this.format.read(vp, inb, this.format_map);
  }

  public void read(Vector v, Hashtable ht, DataInputStream in)
          throws InputFormatException {
    FormatInputList vp = new StringsHashtableAndPointer(v, ht);
    InputStreamAndBuffer inb = new InputStreamAndBuffer(in);
    this.format.read(vp, inb, this.format_map);
  }

  public void read(String[] s, Hashtable ht, DataInputStream in)
          throws InputFormatException {
    Vector v = new Vector();
    for (int i = 0; i < s.length; i++)
      v.addElement(s[i]);
    read(v, ht, in);
  }

  public Object read(DataInputStream in)
          throws InputFormatException {
    Vector v = new Vector();
    read(v, in);
    return v.elementAt(0);
  }


  public boolean eof(DataInputStream in)
          throws IOException {
    return (in.available() <= 0);
  }


  public String toString() {
    return "[Formatter " + this.format.toString() + "]";
  }
}


