/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.plot;

import com.jtex.arrays.Array1D;
import java.awt.Color;
import java.awt.image.IndexColorModel;

/**
 *
 * @author hios
 */
public class ColorMap {

    public static enum Name {

        WHITEJET("white jet"),
        WHITE2BLACK("white to black"),
        BLACK2WHITE("black to white"),
        LABOTEX("labotex"),
        BLUE2RED("blue to red"),
        RED2BLUE("red to blue"),
        JET("jet");

        String name;

        private Name(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

        @Override
        public String toString() {
            return name;
        }

    }

    private Name name;
    private Color colors[]; // makeColorMapJet();

    public static ColorMap getColormap(Name rainbow, int i) {
        return new ColorMap(rainbow, i);
    }

    private ColorMap(Name m, int i) {
        this.name = m;
        switch (m) {
            case JET:
                this.colors = makeColorMapJet(i);
                break;
            case WHITEJET:
                this.colors = makeColorMapWhiteJet(i);
                break;
            case LABOTEX:
                this.colors = makeColorLabotex(i);
                break;
            case WHITE2BLACK:
                this.colors = makeColorWhite2Black(i);
                break;
            case BLACK2WHITE:
                this.colors = makeColorBlack2White(i);
                break;
            case BLUE2RED:
                this.colors = makeColorBlue2Red(i);
                break;
            case RED2BLUE:
                this.colors = makeColorRed2Blue(i);
                break;
        }
    }

    public Color getColor(int i) {
        return colors[i];
    }

    public Name getName() {
        return name;
    }

    public int size() {
        return this.colors.length;
    }

    public IndexColorModel getColorModel() {
        int colorCnt = colors.length;

        byte r[] = new byte[colorCnt],
                g[] = new byte[colorCnt],
                b[] = new byte[colorCnt];

        for (int i = 0; i < colorCnt; i++) {
            r[i] = (byte) colors[i].getRed();
            g[i] = (byte) colors[i].getGreen();
            b[i] = (byte) colors[i].getBlue();
        }
        return new IndexColorModel(8, colorCnt, r, g, b);
    }

    public static Color[] makeColorMapJet(int n) {
        Array1D stepping = new Array1D(25, 10, 10, 15, 8);
        Color[] c = new Color[5];
        c[0] = new Color(0f, 0f, 1f);
        c[1] = new Color(0f, 1f, 1f);
        c[2] = new Color(1f, 1f, 0f);
        c[3] = new Color(1f, 0f, 0f);
        c[4] = new Color(.5f, 0f, 0f);
        return makeColorMap(n, stepping.toIntArray(), c);
    }

    public static Color[] makeColorMapWhiteJet(int n) {
        Array1D stepping = new Array1D(15, 40, 25, 13, 7);
        Color[] c = new Color[6];
        c[0] = new Color(1f, 1f, 1f);
        c[1] = new Color(0f, 0f, .7f);
        c[2] = new Color(0f, .85f, .65f);
        c[3] = new Color(1f, 1f, 0f);
        c[4] = new Color(1f, 0f, 0f);
        c[5] = new Color(.5f, 0f, 0f);
        return makeColorMap(n, stepping.toIntArray(), c);
    }

    public static Color[] makeColorBlack2White(int n) {
        Array1D stepping = new Array1D(10, 10, 10);
        Color[] c = new Color[3];
        c[0] = new Color(0f, 0f, 0f);
        c[1] = new Color(.7f, .7f, .7f);
        c[2] = new Color(1f, 1f, 1f);
        return makeColorMap(n, stepping.toIntArray(), c);
    }

    public static Color[] makeColorWhite2Black(int n) {
        Array1D stepping = new Array1D(10, 10, 10);
        Color[] c = new Color[3];
        c[0] = new Color(1f, 1f, 1f);
        c[1] = new Color(.7f, .7f, .7f);
        c[2] = new Color(0f, 0f, 0f);
        return makeColorMap(n, stepping.toIntArray(), c);
    }

    public static Color[] makeColorLabotex(int n) {
        Array1D stepping = new Array1D(10, 10, 10);
        Color[] c = new Color[3];
        c[0] = new Color(1f, 1f, 1f);
        c[1] = new Color(1f, .4f, .4f);
        c[2] = new Color(.5f, 0f, 0f);
        return makeColorMap(n, stepping.toIntArray(), c);
    }

    private static Color[] makeColorBlue2Red(int n) {
        Array1D stepping = new Array1D(10, 10, 10);
        Color[] c = new Color[3];
        c[0] = new Color(0f, 0f, 1f);
        c[1] = new Color(1f, 1f, 1f);
        c[2] = new Color(1f, 0f, 0f);
        return makeColorMap(n, stepping.toIntArray(), c);
    }

    private static Color[] makeColorRed2Blue(int n) {
        Array1D stepping = new Array1D(10, 10, 10);
        Color[] c = new Color[3];
        c[0] = new Color(1f, 0f, 0f);
        c[1] = new Color(1f, 1f, 1f);
        c[2] = new Color(0f, 0f, 1f);
        return makeColorMap(n, stepping.toIntArray(), c);
    }

    private static Color[] makeColorMap(int n, int[] steps, Color[] colors) {

        Array1D step = new Array1D(steps);
        int stepping[] = step.multiply(n / step.sum()).round().toIntArray();

        Array1D r[] = new Array1D[stepping.length - 1];
        Array1D g[] = new Array1D[stepping.length - 1];
        Array1D b[] = new Array1D[stepping.length - 1];

        for (int i = 0; i < stepping.length - 1; i++) {
            float start[] = colors[i].getRGBColorComponents(null);
            float stop[] = colors[i + 1].getRGBColorComponents(null);
            r[i] = Array1D.linspace(start[0], stop[0], stepping[i] + 1);
            g[i] = Array1D.linspace(start[1], stop[1], stepping[i] + 1);
            b[i] = Array1D.linspace(start[2], stop[2], stepping[i] + 1);
        }

        float[] rr = Array1D.concat(r).toFloatArray();
        float[] gg = Array1D.concat(g).toFloatArray();
        float[] bb = Array1D.concat(b).toFloatArray();
        Color[] cmap = new Color[rr.length];
        for (int i = 0; i < cmap.length; i++) {
            cmap[i] = new Color(rr[i], gg[i], bb[i]);
        }

        return cmap;
    }

    Color[] colors() {
        return colors;
    }

    public int[] getRed() {
        int[] r = new int[colors.length];
        for (int i = 0; i < colors.length; i++) {
            r[i] = colors[i].getRed();
        }
        return r;
    }

    public int[] getGreen() {
        int[] r = new int[colors.length];
        for (int i = 0; i < colors.length; i++) {
            r[i] = colors[i].getGreen();
        }
        return r;
    }

    public int[] getBlue() {
        int[] r = new int[colors.length];
        for (int i = 0; i < colors.length; i++) {
            r[i] = colors[i].getBlue();
        }
        return r;
    }

    public int[] getAlpha() {
        int[] r = new int[colors.length];
        for (int i = 0; i < colors.length; i++) {
            r[i] = colors[i].getAlpha();
        }
        return r;
    }

}
