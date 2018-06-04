/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.plot;

import java.awt.Graphics;
import java.awt.font.TextAttribute;
import java.awt.geom.Rectangle2D;
import java.text.AttributedCharacterIterator;
import java.text.AttributedString;
import java.text.CharacterIterator;
import java.util.ArrayList;
import java.util.Formatter;
import java.util.List;
import java.util.Locale;

/**
 *
 * @author flb
 */
public class PlotAnnotation {

    String tl, tr, bl, br;

    public static String getFormatted(String format, Object... obj) {
        return new Formatter(Locale.US).format(format, obj).toString();
    }

    public PlotAnnotation(double max, double min) {
        this(max, min, null);
    }

    public PlotAnnotation(double max, double min, String tr) {
        this(max, min, tr, null);
    }

    public PlotAnnotation(double max, double min, String tr, String br) {
        this(getFormatted("Max:\n%4.2f", max), tr, getFormatted("Min:\n%4.2f", min), br);
    }

    public PlotAnnotation(String tl, String tr, String bl, String br) {
        this.tl = tl;
        this.tr = tr;
        this.bl = bl;
        this.br = br;
    }

    private static AttributedString make(String t) {

        StringBuilder b = new StringBuilder();

        List<Integer> l = new ArrayList<Integer>();
        for (int i = 0; i < t.length(); i++) {
            char c = t.charAt(i);
            if (c != '_') {
                b.append(c);
            } else {
                l.add(b.length());
            }
        }
        AttributedString as = new AttributedString(b.toString());

        for (int i : l) {
            as.addAttribute(TextAttribute.SUPERSCRIPT, TextAttribute.SUPERSCRIPT_SUB, i, i + 1);
        }
        return as;
    }

    private static Rectangle2D getAB(AttributedString as, Graphics g) {
        int ni = 1;
        AttributedCharacterIterator iterator = as.getIterator();
        char c;
        while ((c = iterator.next()) != CharacterIterator.DONE) {
            ni++;
        };
        //stupid to count chars.?! 
        return g.getFont().getStringBounds(as.getIterator(), 0, ni, g.getFontMetrics().getFontRenderContext());

    }

    private static String[] split(String s) {
        if (s != null && !s.isEmpty()) {
            return s.split("\n");
        }
        return new String[0];
    }

    private static String[] splitReverse(String s) {
        String[] sp = split(s);
        String[] ref = new String[sp.length];

        for (int i = 0; i < sp.length; i++) {
            ref[i] = sp[sp.length - i - 1];
        }
        return ref;

    }

    public void paint(Graphics g, int w, int h) {

        double yoff = 0;
        for (String split1 : split(tl)) {
            AttributedString st = make(split1);
            g.drawString(st.getIterator(), 1, (int) (yoff += getAB(st, g).getHeight()));
        }

        yoff = h - 2;
        for (String split1 : splitReverse(bl)) {
            AttributedString st = make(split1);
            g.drawString(st.getIterator(), 1, (int) (yoff));
            yoff -= getAB(st, g).getHeight();
        }

        yoff = 0;
        for (String split1 : split(tr)) {
            AttributedString st = make(split1);
            Rectangle2D ab = getAB(st, g);
            g.drawString(st.getIterator(), (int) (w - ab.getWidth() - 3), (int) (yoff += ab.getHeight()));
        }

        yoff = h - 2;
        for (String split1 : splitReverse(br)) {
            AttributedString st = make(split1);
            Rectangle2D ab = getAB(st, g);
            g.drawString(st.getIterator(), (int) (w - ab.getWidth() - 3), (int) (yoff));
            yoff -= ab.getHeight();
        }

    }

}
