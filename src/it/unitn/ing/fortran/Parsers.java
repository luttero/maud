package it.unitn.ing.fortran;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.util.Hashtable;
import java.util.Vector;

/* This class is used to hold the parsers for formats and numbers.
   We generate them static (see JavaCC documentation) because it
   makes them more efficient. However, that then means that we need
   somewhere to put an instance of each. That's what we use the result
   of Parsers.theParsers() for.
*/

class Parsers {
  static boolean already_created = false;
  static Parsers parsers = null;

  FormatParser format_parser = null;
  NumberParser number_parser = null;


  static Parsers theParsers() {
    if (!(already_created)) {
      parsers = new Parsers();
      already_created = true;
    }
    return parsers;
  }


  private Parsers() {
    this.format_parser = new FormatParser(new StringBufferInputStream(""));
    this.number_parser = new NumberParser(new StringBufferInputStream(""));
  }
}
