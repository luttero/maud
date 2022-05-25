/*
 * XGridPasswordProfile.java  $Revision: 1.1 $ $Date: 2006/07/20 14:06:05 $
 *
 * Copyright (c) 2006 Luca Lutterotti.  All rights reserved.
 *
 * The contents of this file are subject to the Blocks Public License (the
 * "License"); You may not use this file except in compliance with the License.
 *
 * You may obtain a copy of the License at http://www.beepcore.org/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied.  See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 */
package it.unitn.ing.xgridclient;


import java.util.Hashtable;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import org.beepcore.beep.core.*;
import org.beepcore.beep.profile.*;
import org.beepcore.beep.profile.sasl.*;


/**
 * This class implements the XGrid Password mechanism
 * as an extension of the base SASL profile.
 *
 * It uses the TuningProfile methods begin, abort, complete
 * and doesn't really have to do any SASL message exchange really
 * But you could write one that did, and see, the nice thing
 * is that with the CCLs, you can register whatever you want
 * as the profile handler. It just expects (sort of) to find some
 * user info in the blob...as who you're authenticating as.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:05 $
 *
 */
public class XGridPasswordProfile
    extends SASLProfile implements Profile, StartChannelListener
{

    // Constants
    public static final String uri = "http://www.apple.com/beep/xgrid/authentication/two-way-random";
    public static final String ANONYMOUS = "XGridPassword";
    public static final String MECHANISM = "TWO-WAY-RANDOM";
    private static XGridPasswordProfile instance = null;

    private Log log = LogFactory.getLog(this.getClass());

    public XGridPasswordProfile() 
    {        
        // @todo Probably do this slightly differently, although
        // the static routines make it tougher.
        instance = this;
    }

    public StartChannelListener init(String uri, ProfileConfiguration config)
        throws BEEPException
    {
        return this;
    }

    /**
     * Method getInstance
     *
     */
    static synchronized XGridPasswordProfile getInstance()
    {
        if(instance == null)  
        {
            instance = new XGridPasswordProfile();
        }
        return instance;
    }

    /**
     * Our extension of Start Channel (see ChannelControlListener)
     * does a lot of things.  It begins the authentication, and in
     * some cases (if the user has packed data in <blob> form on
     * the startChannel request) can actually finish the anonymous
     * authentication.
     */
    public void startChannel(Channel channel, String encoding, String data)
            throws StartChannelException
    {
        log.debug("XGridPasswordProfile.startChannel");
        clearCredential(channel.getSession(), this);
        Session t = channel.getSession();

        try {
            XGridPasswordAuthenticator auth = new XGridPasswordAuthenticator(this);
            auth.started(channel);
        } catch (Exception x) {
            channel.getSession().terminate(x.getMessage());
            return;
        }
    }

    public void closeChannel(Channel channel) 
        throws CloseChannelException 
    {}

    public boolean advertiseProfile(Session session)
    {
        return true;
    }

    /**
     * Method authencitateXGridPassword is an Initiator routine designed
     * to allow a peer to authenticate to another one. 
     *
     * @param session Session the current session
     * @param id The identity of the peer withing to authenticate
     *
     * @throws SASLException if any failure occurs.
     */
    public static Session AuthenticateXGridPassword(Session session, String id)
            throws BEEPException, AuthenticationFailureException
    {
        if (id == null) {
            id = ANONYMOUS;
        }

        clearCredential(session, null);

        XGridPasswordAuthenticator auth = new XGridPasswordAuthenticator(getInstance());
        Channel ch = session.startChannel(XGridPasswordProfile.uri, auth);
        auth.started(ch);
        auth.sendIdentity(id);
        synchronized(auth)
        {
            try
            {
                auth.wait();

                //FIX for bug 469725, if authentication fails no local Cred is
                //set and a AuthenticationFailureException is thrown
                if (ch.getSession().getLocalCredential() == null)
                {
                    throw new AuthenticationFailureException( 
                        "Could not authenticate with SASL/ANON");
                }

                return ch.getSession();
            }
            catch(InterruptedException x)
            {}
        }
        return null;
    }

    /**
     * Method authencitateXGridPasswordPiggyback is an Initiator
     * routine designed to allow a peer to authenticate to another
     * one.  It is distinct in that it piggybacks the data for the
     * authentication request on the startChannel request.
     *
     * @param session Session the current session
     * @param id The identity of the peer withing to authenticate
     *
     * @throws SASLException if any failure occurs.
     */
    public static Session AuthenticateXGridPasswordPiggyback(Session session, 
                                                             String id)
            throws BEEPException
    {
        if (id == null) {
            id = ANONYMOUS;
        }

        clearCredential(session, null);

        Channel ch = session.startChannel(XGridPasswordProfile.uri,
                                          false,
                                          new Blob(Blob.STATUS_NONE, id).toString());

        if ((ch.getStartData() != null)
                && (ch.getStartData().indexOf("<error ") != -1)) {
            throw new BEEPException(ch.getStartData());
        }

        String goo = ch.getStartData();

        if(goo != null)
        {
            Blob blob = new Blob(goo);
            if(blob.getStatus().equals(Blob.COMPLETE)) 
            {
                getInstance().finishInitiatorAuthentication(generateCredential(id),
                                                            ch.getSession());
                return session;
            }
        } 
        // @todo allow non-piggybacked
        throw new BEEPException("Auth failed.");
    }

    /**
     * Method generateCredential simply generates a XGridPassword
     * style credential.
     */
    static SessionCredential generateCredential()
    {
        return generateCredential(ANONYMOUS);
    }

    /**
     * Method generateCredential simply generates a XGridPassword
     * style credential.
     */
    static SessionCredential generateCredential(String id)
    {
        if (id == null) {
            id = ANONYMOUS;
        }

        Hashtable ht = new Hashtable(4);

        ht.put(SessionCredential.AUTHENTICATOR, id);
        ht.put(SessionCredential.AUTHENTICATOR_TYPE, MECHANISM);

        return new SessionCredential(ht);
    }

    /**
     * Method finishInitiatorAuthentication
     * 
     * see SASLProfile's version of this.
     */
    protected void finishInitiatorAuthentication(SessionCredential c,
                                                 Session s)
    {
        super.finishInitiatorAuthentication(c,s);
    }

    /**
     * Method finishListenerAuthentication
     * 
     * see SASLProfile's version of this.
     */
    protected void finishListenerAuthentication(SessionCredential c,
                                                Session s)
        throws SASLException
    {
        super.finishListenerAuthentication(c,s);
    }    
}
