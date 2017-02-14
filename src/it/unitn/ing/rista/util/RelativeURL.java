/*
 * @(#)RelativeURL.java created 01/01/1997 Mesiano
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

package it.unitn.ing.rista.util;


import java.net.URL;
import java.net.MalformedURLException;

import it.unitn.ing.rista.util.Context;

import java.io.*;


/**
 * This class is used in conjunction with class symantec.itools.OS.Context to
 * provide URLs that are relative to an applet\ufffds or application\ufffds
 * document base. For applets the document base is the URL of the document
 * that the applet is embedded in. For applications the document base is the
 * same as the \ufffduser.dir\ufffd system property.
 * @version $Revision: 1.2 $, $Date: 1970/01/04 19:14:51 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class RelativeURL {
  /**
   * Don\ufffdt use, this is an all-static class.
   */
  public RelativeURL() {
  }

  /**
   * Determines the absolute URL given a relative URL.
   * If the spec parameter is relative, it is considered to be relative
   * to the current document base as determined by getDocumentBase() in
   * class symantec.itools.lang.Context.
   *
   * @param spec a possibly relative URL
   * @return the absolute URL equivalent to the given relativeURL
   * @exception MalformedURLException
   * if cannot generate the resultant URL due to a bad spec parameter
   * or a bad document base
   */
  public static URL getURL(String spec)
          throws MalformedURLException {

    // InternetExplorer for some reason strips out a single anchor symbol (#)
    // when the URL is passed to showDocument() so we double up the symbol (##)
    if (System.getProperty("java.vendor").startsWith("Microsoft") &&
            spec.indexOf('#') > -1) {
      spec = ieAnchorHack(spec);
    }

    URL documentBase = Context.getDocumentBase();

    if (documentBase != null && spec.indexOf("//") == -1) {
      return new URL(documentBase, spec);
    }

    return new URL(spec);
  }

  private static String ieAnchorHack(String spec) {
    String str = spec.substring(0, spec.lastIndexOf('#'));
    str += "#";
    str += spec.substring(spec.lastIndexOf('#'));
    return str;
  }


}
