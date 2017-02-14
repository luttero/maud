/*
 * @(#)CIFtoken.java created 1997 Mesiano
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.io.cif;

import java.io.*;
import java.lang.*;

import it.unitn.ing.rista.io.*;
import it.unitn.ing.rista.util.*;

/**
 *  The CIFtoken is a class
 *
 *
 * @version $Revision: 1.9 $, $Date: 2006/01/19 14:45:58 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class CIFtoken extends StreamTokenizer {
  public static final int TT_DATA = -4;
  public static final int TT_LOOP = -5;
  public static final int TT_PHASE = -6;
  public static final int TT_INST = -7;
  public static final int TT_CIFE = -8;
  public static final int TT_QUEST = -9;
  public static final int TT_STRING = -10;
  public static final int TT_GLOB = -11;
  public static final int TT_DATASET = -12;
  public static final int TT_SAMPLE = -13;
  public static final int TT_BOUND = -14;
  public static final int TT_SUBO = -15;
  public static final int TT_SUBE = -16;
  public static final int TT_COMMENT = -17;
  public static final int TT_CUSTOM = -18;
  public static final int TT_CUSTOM_END = -19;

  public String thestring,thestringerror, minValue, maxValue, refName, refBound, constant, ratio, expression;
  public double thevalue, theerror;
  public boolean free = false, autoTrace = false, positive = false;

  public CIFtoken(InputStream in) {
    super(in);

    resetSyntax();
    setSyntax();
  }

  public CIFtoken(Reader in) {
    super(in);

    resetSyntax();
    setSyntax();
  }

  public void setSyntax() {
    whitespaceChars(0, 32);
    wordChars(33, 125);
    wordChars('~', '~');
    wordChars('?', '?');
//		commentChar('#');
//    quoteChar(';');
//    quoteChar('\'');
//    quoteChar('"');
    eolIsSignificant(false);
    setEssentialSyntax();
  }

	public void setSyntaxTakeSpace() {
		whitespaceChars(0, 32);
		wordChars(33, 125);
		wordChars('~', '~');
		wordChars('?', '?');
//		commentChar('#');
//    quoteChar(';');
//    quoteChar('\'');
//    quoteChar('"');
		eolIsSignificant(false);
		setEssentialSyntax();
	}

	public void setEssentialSyntax() {
    slashStarComments(false);
    slashSlashComments(false);
  }

  public int nextToken() throws IOException {
    int newtoken;
    int tokentype = super.nextToken();

//		System.out.println("Sval " + sval);
    if (sval != null && sval.equals("?")) {
      thestring = sval;
      return TT_QUEST;
    }
    switch (ttype) {
      case TT_WORD:
        if (sval.toLowerCase().equals("loop_"))
          return TT_LOOP;
        if (sval.toLowerCase().startsWith("data_global")) {
          thestring = getStringStartingFrom(sval, 12);
          return TT_GLOB;
        }
        if (sval.toLowerCase().startsWith("data_instrument")) {
          thestring = Misc.toStringRecoverBlank(getStringStartingFrom(sval, 16));
          return TT_INST;
        }
        if (sval.toLowerCase().startsWith("data_phase")) {
          thestring = Misc.toStringRecoverBlank(getStringStartingFrom(sval, 11));
          return TT_PHASE;
        }
        if (sval.toLowerCase().startsWith("data_dataset")) {
          thestring = Misc.toStringRecoverBlank(getStringStartingFrom(sval, 13));
          return TT_DATASET;
        }
        if (sval.toLowerCase().startsWith("data_sample")) {
          thestring = Misc.toStringRecoverBlank(getStringStartingFrom(sval, 12));
          return TT_SAMPLE;
        }
        if (sval.toLowerCase().startsWith("data_bound")) {
          thestring = "none";
          return TT_BOUND;
        }
        if (sval.startsWith("_")) {
          thestring = sval;
          return TT_CIFE;
        }
        if (sval.toLowerCase().startsWith("data_")) {
          thestring = getStringStartingFrom(sval, 5);
          return TT_DATA;
        }
        if (sval.startsWith("#")) {
          thestring = sval.substring(1);
          if (thestring.toLowerCase().startsWith("subordinateobject"))
            return TT_SUBO;
          if (thestring.toLowerCase().startsWith("end_subordinateobject"))
            return TT_SUBE;
          if (thestring.toLowerCase().startsWith("custom_object"))
            return TT_CUSTOM;
          if (thestring.toLowerCase().startsWith("end_custom_object"))
            return TT_CUSTOM_END;
          return TT_COMMENT;
        }
        if (sval.startsWith(";")) {
          thestring = sval.substring(1);
          resetSyntax();
          setEssentialSyntax();
          eolIsSignificant(true);
          wordChars(32, 32);
          if (!thestring.equals(""))
            thestring = thestring.concat("\n");
          do {
            newtoken = super.nextToken();
            if (sval != null) {
              thestring = thestring.concat(sval);
            } else if (ttype == TT_EOL) {
              thestring = thestring.concat("\n");
            }
          } while (sval != null && !sval.equals(";") && ttype != TT_EOF);
          if (ttype == TT_EOF)
            pushBack();
          setSyntax();
          return TT_STRING;
        }
	      if (sval.startsWith("\'")) {

		      thestring = sval.substring(1);
		      if (thestring.length() > 0 && thestring.endsWith("\'")) {
			      thestring = thestring.substring(0, thestring.length() - 1);
			      return TT_STRING;
		      }
		      resetSyntax();
		      setSyntaxTakeSpace();
		      do {
			      newtoken = super.nextToken();
//			      System.out.println(newtoken + " - Adding " + sval);
			      if (sval != null) {
				      thestring = thestring.concat(" " + sval);
				      if (thestring.endsWith("\'"))
					      thestring = thestring.substring(0, thestring.length() - 1);
			      }
		      } while (sval != null && !sval.endsWith("\'") && ttype != TT_EOF);
		      if (ttype == TT_EOF)
			      pushBack();
		      setSyntax();
//		      System.out.println("Final " + thestring);

		      return TT_STRING;
	      }
	      if (sval.startsWith("\"")) {
		      thestring = sval.substring(1);
		      if (sval.endsWith("\"")) {
			      thestring = thestring.substring(0, thestring.length() - 1);
			      return TT_STRING;
		      }
		      resetSyntax();
		      setSyntaxTakeSpace();
		      do {
			      newtoken = super.nextToken();
			      if (sval != null) {
				      thestring = thestring.concat(" " + sval);
				      if (thestring.endsWith("\""))
					      thestring = thestring.substring(0, thestring.length() - 1);
			      }
		      } while (sval != null && !sval.endsWith("\"") && ttype != TT_EOF);
		      if (ttype == TT_EOF)
			      pushBack();
		      setSyntax();
//		      System.out.println("Final " + thestring);
		      return TT_STRING;
	      }
        if (sval != null) {
          StringNumber sn = new StringNumber(sval);
          if (sn.isanumber()) {
            thevalue = sn.value;
            theerror = sn.error;
            thestring = sn.thestring;
            thestringerror = new String((new Double(sn.error)).toString());
            if (sn.hasError)
              free = true;
            else
              free = false;

            resetOtherValues();
            // we check for the min max
            newtoken = super.nextToken();
//        		System.out.println("Sval1 " + newtoken);
            if (newtoken == TT_EOF)
              return TT_NUMBER;
            if (sval.startsWith("#autotrace")) {
              autoTrace = true;
              newtoken = super.nextToken();
            }
//            newtoken = super.nextToken();
//            System.out.println("Sval2 " + newtoken);
            if (sval.startsWith("#positive")) {
              positive = true;
              newtoken = super.nextToken();
            }
//            else
//              pushBack();
//            newtoken = super.nextToken();
//            System.out.println("Sval3 " + newtoken);
            if (sval.startsWith("#min")) {
              newtoken = super.nextToken();
              minValue = sval;
              newtoken = super.nextToken();
              if (sval.startsWith("#max")) {
                newtoken = super.nextToken();
//                System.out.println("Sval3 " + newtoken);
                maxValue = sval;
              } else  // should not happen
                pushBack();
            } else
              pushBack();

            // we check for the bound reference
            newtoken = super.nextToken();
//            System.out.println("Sval4 " + newtoken);
            if (newtoken == TT_EOF)
              return TT_NUMBER;
            if (sval.startsWith("#ref")) {
//              newtoken = super.nextToken();
              refName = sval;
//              System.out.println("Reading ref: " + refName);
            } else
              pushBack();

            // we check for the bound
            newtoken = super.nextToken();
            if (sval.startsWith("#equal") || sval.startsWith("#boundWith")) {
	            if (sval.startsWith("#boundWith")) {
		            newtoken = super.nextToken();
		            expression = sval;
		            while (sval.endsWith("}"))
			            expression += " " + sval;
	            } else {
		            newtoken = super.nextToken();
		            constant = sval;
		            newtoken = super.nextToken();
		            newtoken = super.nextToken();
		            ratio = sval;
		            newtoken = super.nextToken();
		            newtoken = super.nextToken();
		            refBound = sval;
//              System.out.println("Reading bound: " + refBound + " " + constant + " " + ratio);
	            }
            } else
              pushBack();

            return TT_NUMBER;
          }
        }
        thestring = sval;
        return TT_WORD;
      case '\'':
      case '"':
        thestring = sval;
        return TT_STRING;
      default:
        {
          return tokentype;
        }
    }
  }

  public void resetOtherValues() {
    minValue = "0";
    maxValue = "0";
    refName = null;
    refBound = null;
    constant = null;
    ratio = null;
	  expression = null;
    autoTrace = false;
    positive = false;
  }

  public String getStringStartingFrom(String sval, int index) {
    if (sval.length() >= index)
      return sval.substring(index);
    else
      return "";
  }
}
