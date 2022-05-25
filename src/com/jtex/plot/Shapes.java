/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.plot;

import java.awt.Shape;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;

/**
 *
 * @author hios
 */
public class Shapes {

    public static enum Named {

        CIRCLE,
        SQUARE,
        UPTRIANGLE,
        DOWNTRIANGLE,
        DIAMOND,
        CROSS,
        DCROSS;

        public Shape create(final float s) {

            switch (this) {
                case CIRCLE:
                    return createCirc(s);
                case SQUARE:
                    return createSquare(s);
                case UPTRIANGLE:
                    return createUpTriangle(s);
                case DOWNTRIANGLE:
                    return createDownTriangle(s);
                case DIAMOND:
                    return createDiamond(s);
                case CROSS:
                    return createCross(s);
                case DCROSS:
                    return createDCross(s);
            }
            return null;
        }

    }

    public static Shape createUpTriangle(final float s) {
        final GeneralPath p0 = new GeneralPath();
        p0.moveTo(0.0f, -s);
        p0.lineTo(s, s);
        p0.lineTo(-s, s);
        p0.closePath();
        return p0;
    }

    public static Shape createDownTriangle(final float s) {
        final GeneralPath p0 = new GeneralPath();
        p0.moveTo(0.0f, s);
        p0.lineTo(s, -s);
        p0.lineTo(-s, -s);
        p0.closePath();

        return p0;
    }

    public static Shape createDiamond(final float s) {
        final GeneralPath p0 = new GeneralPath();
        p0.moveTo(0f, s);
        p0.lineTo(s / 2, 0f);
        p0.lineTo(0f, -s);
        p0.lineTo(-s / 2, 0f);
        p0.closePath();

        return p0;
    }

    public static Shape createSquare(final float s) {
        final GeneralPath p0 = new GeneralPath();
        p0.moveTo(-s, -s);
        p0.lineTo(-s, s);
        p0.lineTo(s, s);
        p0.lineTo(s, -s);
        p0.closePath();
        return p0;
    }

    public static Shape createDCross(final float s) {
        final GeneralPath p0 = new GeneralPath();
        p0.moveTo(-s, -s);
        p0.lineTo(s, s);
        p0.moveTo(-s, s);
        p0.lineTo(s, -s);
        p0.closePath();
        return p0;
    }

    public static Shape createCross(final float s) {
        final GeneralPath p0 = new GeneralPath();
        p0.moveTo(0f, s);
        p0.lineTo(0f, -s);
        p0.moveTo(s, 0f);
        p0.lineTo(-s, 0f);
        p0.closePath();

        return p0;
    }

    public static Shape createCirc(final float s) {
        final GeneralPath p0 = new GeneralPath();
        p0.append(new Ellipse2D.Double(-s, -s, 2 * s, 2 * s), false);
        return p0;
    }
}
