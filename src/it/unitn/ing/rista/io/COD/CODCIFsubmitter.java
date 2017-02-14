/*
 * @(#)CODCIFsubmitter.java created Feb 27, 2003 Berkeley
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.io.COD;

import HTTPClient.*;

import java.io.IOException;

import it.unitn.ing.rista.util.Misc;
import it.unitn.ing.rista.util.MaudPreferences;
//import it.unitn.ing.rista.io.XMLReader;
import org.w3c.dom.*;

/**
 * The CODCIFsubmitter is a class
 *
 * @version $Revision: 1.3 $, $Date: 2006/01/19 14:45:58 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class CODCIFsubmitter {
  public static void submit(String filename) {
    String CODurl = MaudPreferences.getPref("codAddress.forSubmission", "www.crystallography.net");
    NVPair[] form_data = new NVPair[1];
    NVPair[] file_data = new NVPair[1];
    form_data[0] = new NVPair("MAX_FILE_SIZE", "1000000");
    file_data[0] = new NVPair("crystal", filename);
    NVPair[] new_pair = new NVPair[1];
    boolean submission = true;
    try {
      byte[] raw_data = Codecs.mpFormDataEncode(form_data, file_data, new_pair);
      while (submission) {
//      System.out.println("Size: " + raw_data.length);
//      URI form_uri = new URI("http://sdpd.univ-lemans.fr/cod", "/TransfertFichier.php");
//      HTTPConnection con = new HTTPConnection(form_uri);
      HTTPConnection con = new HTTPConnection(CODurl);
      HTTPResponse rsp = con.Post(/*form_uri.getPathAndQuery()*/"TransfertFichier.php", raw_data, new_pair);
      String response = rsp.getText();
        System.out.println(response);
      if (rsp.getStatusCode() >= 300) {
        System.out.println("Received Error: " + rsp.getReasonLine());
        submission = false;
        break;
      } else {
/*        XMLReader xreader = new XMLReader();
        if (xreader != null) {
          String offendingPart = "<!DOCTYPE";
          if (response.startsWith(offendingPart)) {
            response = response.substring(response.indexOf(">") + 1, response.length());
          }
          System.out.println("Parsing " + response);
          xreader.readString(response);
          Element element = xreader.getElementById("redir");
          String redirectUrl = null;
          if (element != null)
            redirectUrl = getStringForKey(element, "src");
          if (redirectUrl != null) {
            CODurl = redirectUrl.substring(0, redirectUrl.indexOf("/TransfertFichier.php"));
            System.out.println("New COD url: " + CODurl);
          } else {
            System.out.println("Done successfully!");
            submission = false;
          }
        }*/
        if (response != null && response.contains("File received")) {
          System.out.println("Submission to COD done!");
          submission = false;
        } else {
        String redirectUrl = null;
        if (response != null)
          redirectUrl = getStringForRedirection(response);
        if (redirectUrl != null) {
          CODurl = redirectUrl.substring(0, redirectUrl.indexOf("/TransfertFichier.php") -1);
          System.out.println("New COD url: " + CODurl);
        } else {
          System.out.println("Unknown error!");
          submission = false;
        }
        }
      }
      }
    } catch (IOException ioe) {
      ioe.printStackTrace();
      System.out.println(ioe.toString());
    } catch (ModuleException me) {
      me.printStackTrace();
      System.out.println("Error handling request: " + me.getMessage());
    } catch (ParseException e) {
      e.printStackTrace();
    }
  }

  public static String getStringForKey(final Element element, final String searchKey) {
//    System.out.println("Scanning element: " + element.toString());
    String url = element.getAttribute(searchKey);
    return url;
  }

  public static String getStringForRedirection(String string) {
    String result = null;
    if (string.contains("name=\'redir\'")) {
      result = string.substring(string.indexOf("name=\'redir\'" ) + 25, string.length());
      result = result.substring(0, result.indexOf('\"'));
//      System.out.println("Result: " + result);
    }
    return result;
  }

}
