package it.unitn.ing.jgraph;

import java.util.*;
import java.lang.*;
import java.io.StreamTokenizer;
import java.io.InputStream;

/*
************************************************************************
**
**    Class  ScanWord
**
**************************************************************************
**    Copyright (C) 1996 Leigh Brookshaw
**
**    This program is free software; you can redistribute it and/or modify
**    it under the terms of the GNU General Public License as published by
**    the Free Software Foundation; either version 2 of the License, or
**    (at your option) any later version.
**
**    This program is distributed in the hope that it will be useful,
**    but WITHOUT ANY WARRANTY; without even the implied warranty of
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**    GNU General Public License for more details.
**
**    You should have received a copy of the GNU General Public License
**    along with this program; if not, write to the Free Software
*    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
**************************************************************************/


/**
 * This class extends the StreamTokenizer class. It allows words
 * to be defined and recognised and returned as a token. The TT_NUMBER
 * token of the StreamTokenizer is also modified so that scientific notation
 * is recognised.
 *
 * @version $Revision: 1.3 $, $Date: 2006/01/19 14:45:51 $
 * @author Leigh Brookshaw
 */

public class ScanWord extends StreamTokenizer {

/*
*********************
** Public Constants
********************/

  /**
   * Token returned when an unknown characters match nothing
   */
  public final static int UNKNOWN = -256;

  /**
   * Token returned when an error is encountered
   */
  public final static int ERROR = -257;
  /**
   * Token returned when a string has been found. A string is defined to be
   * text between matching quotes " ".
   */
  public final static int STRING = -258;


  /********************
   ** Private Variables
   ********************/

  private InputStream is = null;
/*
**   The hash table containing the keyword/value pairs.
*/
  private Hashtable kwords = new Hashtable();
/*
**   The character that delimeters a string
*/
  private char StringChar = '"';

/*
******************
**
** Constructors
**
*****************/

  /**
   * Instantiate the class with the parsed input stream.
   * @param in Input stream to read from
   */
  public ScanWord(InputStream in) {
    super(in);
    this.is = in;
    resetWordSyntax();
  }

/*
*******************
**
**  Public Methods
**
*******************/

  /**
   * Add a keyword/token pair.
   * @param s String contining the keyword
   * @param i Token to return when the keyword is encountered.
   */
  public void addKeyWord(String s, int i) {

    if (s == null) return;

    kwords.put(s.toLowerCase(), new Integer(i));

  }

  /**
   *   return the matching token given the keyword
   * @param s Keyword.
   */
  public int getKeyValue(String s) {

    if (s == null) return UNKNOWN;

    Integer i = (Integer) kwords.get(s.toLowerCase());

    if (i == null)
      return UNKNOWN;
    else
      return i.intValue();
  }

  /**
   *   Clear the internal table containing the keyword/token pairs
   */
  public void resetKeyWords() {
    kwords.clear();
  }

  /**
   *  Read the input stream and return the next token found.
   */
  public int nextWord() {
    int word;

    try {
      ttype = nextToken();
    } catch (Exception e) {
      return ERROR;
    }

    if (ttype == StringChar) {
      return STRING;
    } else if (ttype == TT_WORD) {
      word = getKeyValue(super.sval);
      if (word == UNKNOWN) word = getNumber(super.sval);
      if (word == UNKNOWN) return ttype;
      ttype = word;
      return word;
    } else {
      return ttype;
    }
  }

  /**
   * Set the character that delimeters a string.
   * @param c character to delimeter strings. Default is ".
   */
  public void setStringChar(char c) {
    StringChar = c;
    quoteChar(StringChar);
  }



/*
*********************
**
** Protected Methods
**
********************/

  /**
   *   Check to see if the returned word is a number
   *   The native StreamTokenizer number method does not work for
   *   scientific notation so we us the Double class doubleValue() method
   * @param s String to find number.
   *
   */
  protected int getNumber(String s) {

    try {
      super.nval = Double.valueOf(s).doubleValue();
      return TT_NUMBER;
    } catch (Exception e) {
      return UNKNOWN;
    }

  }

  /**
   *   Reset the syntax of the parent class
   */
  protected void resetWordSyntax() {
    resetSyntax();
    wordChars('a', 'z');
    wordChars('A', 'Z');
    wordChars('0', '9');
    wordChars('.', '.');
    wordChars('_', '_');
    wordChars('{', '}');
    wordChars('+', '+');
    wordChars('-', '-');
    wordChars(128 + 32, 255);

    quoteChar(StringChar);

    whitespaceChars(0, ' ');

    commentChar('#');

    lowerCaseMode(false);

    eolIsSignificant(true);
  }

  /**
   *  Close the input stream
   */
  protected void closeStream() {
    try {
      is.close();
    } catch (Exception e) {
    }

    is = null;
  }

}
