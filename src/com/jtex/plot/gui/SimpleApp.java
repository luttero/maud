/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.plot.gui;

import com.jtex.qta.ODF;
import com.jtex.qta.PoleFigure;
import javax.swing.event.EventListenerList;

/**
 *
 * @author hios
 */
public class SimpleApp {

    EventListenerList listeners = new EventListenerList();

    PoleFigure pf;
    ODF odf;

    public void addAppListener(SimpleAppListener listener) {
        listeners.add(SimpleAppListener.class, listener);
    }

    public void removeAppListener(SimpleAppListener listener) {
        listeners.remove(SimpleAppListener.class, listener);
    }

    protected void fireAppChanged() {
        for (SimpleAppListener listener : listeners.getListeners(SimpleAppListener.class)) {
            listener.appChanged(this);
        }
    }

    public void setPf(PoleFigure pf) {
        System.gc();
        this.pf = pf;
        this.odf = null;
        fireAppChanged();
    }

    public void setOdf(ODF odf) {
        this.odf = odf;
        fireAppChanged();
    }

    public ODF getOdf() {
        return odf;
    }

    public PoleFigure getPf() {
        return pf;
    }

}
