/*
 * @(#)CODhttpConnector.java created Jul 15, 2005 Riva Del Garda (Italstructures)
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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


/**
 * The CODhttpConnector is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/01/19 14:45:58 $
 * @since JDK1.1
 */

public class CODhttpConnector {

  public static void submit(String filename) {
    try {
      NVPair[] form_data = new NVPair[1];
      NVPair[] file_data = new NVPair[1];
      form_data[0] = new NVPair("MAX_FILE_SIZE", "1000000");
      file_data[0] = new NVPair("crystal", filename);
      NVPair[] new_pair = new NVPair[1];
      byte[] raw_data = Codecs.mpFormDataEncode(form_data, file_data, new_pair);
//      System.out.println("Size: " + raw_data.length);
//      URI form_uri = new URI("http://www.crystallography.net/cod", "/TransfertFichier.php");
//      HTTPConnection con = new HTTPConnection(form_uri);
      HTTPConnection con = new HTTPConnection("localhost");
      HTTPResponse rsp = con.Post("/cod/result.php?text1=enstatite&text2=&el1=&el2=&el3=&el4=&el5=&el6=&el7=&el8=&nel1=&nel2=&nel3=&nel4=&vmin=&vmax=&strict=&submit=Send", raw_data, new_pair);
      if (rsp.getStatusCode() >= 300) {
        System.out.println("Received Error: " + rsp.getReasonLine());
        System.out.println(rsp.getText());
      } else {
        System.out.println(rsp.getText());
        System.out.println("Done successfully!");
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
}
