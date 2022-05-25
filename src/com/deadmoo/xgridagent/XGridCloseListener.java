/*
 * Created on Nov 14, 2005
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.deadmoo.xgridagent;

import org.beepcore.beep.core.event.ChannelEvent;
import org.beepcore.beep.core.event.ChannelListener;

/**
 * @author cjcamp4
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class XGridCloseListener implements ChannelListener {

    private Agent agent;

    /**
     * 
     */
    public XGridCloseListener(Agent agent) {
        super();
        this.agent = agent;
    }

    /* (non-Javadoc)
     * @see org.beepcore.beep.core.event.ChannelListener#channelStarted(org.beepcore.beep.core.event.ChannelEvent)
     */
    public void channelStarted(ChannelEvent e) {
    }

    /* (non-Javadoc)
     * @see org.beepcore.beep.core.event.ChannelListener#channelClosed(org.beepcore.beep.core.event.ChannelEvent)
     */
    public void channelClosed(ChannelEvent e) {
        synchronized(agent) {
            agent.closeSession(e.toString());
            agent.restart();
        }
    }

    // don't need session listener
    /* (non-Javadoc)
     * @see org.beepcore.beep.core.event.SessionListener#greetingReceived(org.beepcore.beep.core.event.SessionEvent)
     */
    /*public void greetingReceived(SessionEvent e) {
    }*/

    /* (non-Javadoc)
     * @see org.beepcore.beep.core.event.SessionListener#sessionClosed(org.beepcore.beep.core.event.SessionEvent)
     */
    /*public void sessionClosed(SessionEvent e) {
        synchronized(agent) {
            agent.restart();
        }
    }*/

    /* (non-Javadoc)
     * @see org.beepcore.beep.core.event.SessionListener#sessionReset(org.beepcore.beep.core.event.SessionResetEvent)
     */
    /*public void sessionReset(SessionResetEvent e) {
        synchronized(agent) {
            agent.restart();
        }
    }*/

}
