/*
 * @(#)BasicInput.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with the author.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.rista.io;

import java.io.*;

import it.unitn.ing.rista.io.StringNumber;
import it.unitn.ing.rista.util.Misc;

/**
 * The BasicInput is a class
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:58 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class BasicInput extends StreamTokenizer {
  public int status = 0;
  protected InputStream infile;

  public BasicInput(InputStream in) {
    super(in);
    infile = in;

    setDefaultSintax();
  }

  public void setDefaultSintax() {
    resetSyntax();
    whitespaceChars(0, 32);
    whitespaceChars(44, 44);
    wordChars(33, 43);
    wordChars(45, 125);
    quoteChar('\'');
    quoteChar('"');
    slashStarComments(false);
    slashSlashComments(false);
    eolIsSignificant(false);
  }

  public void setAlternateSintax() {
    resetSyntax();
    whitespaceChars(0, 31);
    wordChars(32, 125);
    quoteChar('\'');
    quoteChar('"');
    slashStarComments(false);
    slashSlashComments(false);
    eolIsSignificant(true);
  }

  public double inputnumber() {
    int tokentype;

    try {
      tokentype = super.nextToken();
      if (tokentype != TT_EOF)
        if (sval != null) {
          StringNumber sn = new StringNumber(sval);
          if (sn.isanumber())
            return sn.value;
          else
            return 0;
        } else
          return 0;
      else {
        status = TT_EOF;
        return 0;
      }
    } catch (IOException ioe) {
      System.out.println("IO error");
      return 0;
    }
  }

  public String inputstring() {
    int tokentype;

    try {
      tokentype = super.nextToken();
      if (tokentype != TT_EOF)
        if (sval != null) {
          return sval;
        } else
          return new String("");
      else {
        status = TT_EOF;
        return new String("");
      }
    } catch (IOException ioe) {
      System.out.println("IO error");
      return new String("");
    }
  }

  public String inputline() {
    int newtoken;
    String thestring;

    try {
      thestring = new String("");
      setAlternateSintax();
      do {
        newtoken = super.nextToken();
        if (sval != null)
          thestring = thestring.concat(sval);
      } while (ttype != TT_EOL && ttype != TT_EOF);
      if (ttype == TT_EOF)
        pushBack();
      setDefaultSintax();
      return thestring;
    } catch (IOException ioe) {
      setDefaultSintax();
      System.out.println("IO error");
      return new String("");
    }
  }

  public void close() {
    try {
      infile.close();
    } catch (IOException e) {
    }
  }
}
