/*
 * @(#)XGridClientProfile.java created Feb 18, 2006 Atlantic ocean, somewhere
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

package it.unitn.ing.xgridcontroller;

import org.beepcore.beep.profile.Profile;
import org.beepcore.beep.profile.ProfileConfiguration;
import org.beepcore.beep.core.*;
import org.beepcore.beep.util.BufferSegment;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import it.unitn.ing.xgridclient.XGridClient;


/**
 * The XGridClientProfile is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:05 $
 * @since JDK1.1
 */

public class XGridClientProfile implements Profile, StartChannelListener, RequestHandler
{

  public static final String URI = XGridClient.PRO_XGRIDCLIENT_URI;
//      "http://www.apple.com/beep/xgrid/controller/client";

  private Log log = LogFactory.getLog(this.getClass());

  public StartChannelListener init(String uri, ProfileConfiguration config)
      throws BEEPException
  {
      return this;
  }

  public void startChannel(Channel channel, String encoding, String data)
          throws StartChannelException
  {
      log.debug("XGridController StartChannel Callback");
      channel.setRequestHandler(this);
  }

  public void closeChannel(Channel channel) throws CloseChannelException
  {
      log.debug("XGridController CloseChannel Callback");
      channel.setRequestHandler(null);
  }

  public boolean advertiseProfile(Session session)
  {
      return true;
  }

  public void receiveMSG(MessageMSG message)
  {
      OutputDataStream data = new OutputDataStream();
      InputDataStream ds = message.getDataStream();

      while (true) {
          try {
              BufferSegment b = ds.waitForNextSegment();
              if (b == null) {
                  break;
              }
              data.add(b);
          } catch (InterruptedException e) {
              message.getChannel().getSession().terminate(e.getMessage());
              return;
          }
      }

      data.setComplete();

      try {
          message.sendRPY(data);
      } catch (BEEPException e) {
          try {
              message.sendERR(BEEPError.CODE_REQUESTED_ACTION_ABORTED,
                              "Error sending RPY");
          } catch (BEEPException x) {
              message.getChannel().getSession().terminate(x.getMessage());
          }
          return;
      }
  }


}
