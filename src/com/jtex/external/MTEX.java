/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.external;

/**
 *
 * @author hios
 */
public class MTEX {

    static {
        Native.loadLibraries();
    }

    public static double[] pf2odf(int[] lP, int[] lh, double[] refl, int iter_max, int iter_min, int flags,
            double[] P, double[] r, double[] gh, double[] A, double[] c0, double[] w, double[] RM, double evaldata, double evalmatrix) {

        Native.ParamSet set = new Native.ParamSet(Native.PF2ODF, 2);

        set.addinline("lP", lP);
        set.addinline("lh", lh);
        set.addinline("refl", refl);
        set.addinline("iter_max", iter_max);
        set.addinline("iter_min", iter_min);
        set.addinline("flags", 1);

        set.add("P", P);
        set.add("r", r);
        set.add("gh", gh);
        set.add("A", A);
        set.add("c0", c0);
        set.add("w", w);

        return Native.execute(set);

    }

    public static double[] odf2pf(double[] gh, double[] r, double[] c, double[] A) {

        Native.ParamSet set = new Native.ParamSet(Native.ODF2PF, 1);

        set.add("gh", gh);
        set.add("r", r);
        set.add("c", c);
        set.add("Al", A);
//        set.add("refl", refl);

        return Native.execute(set);
    }

    public static double[] odf2fc(double[] g, double[] c, double[] A) {

        Native.ParamSet set = new Native.ParamSet(Native.ODF2FC, 1);

        set.add("g", g);
        set.add("c", c);
        set.add("A", A);

        return Native.execute(set);
    }

    public static double[] fc2odf(int L, double[] f_hat, double[] g) {

        Native.ParamSet set = new Native.ParamSet(Native.FC2ODF, 1);

        set.addinline("L", L);
        set.add("g", g);
        set.add("f_hat", f_hat);

        return Native.execute(set);
    }

    public static double[] pdf2pf(double[] r, double[] P_hat) {

        Native.ParamSet set = new Native.ParamSet(Native.PDF2PF, 1);

        set.add("r", r);
        set.add("P_hat", P_hat);

        return Native.execute(set);
    }

//    public  sphericalY(int l, double[] jr);
//
//    public native static double[] SO3GridDistRegion(double[] jyalpha, double[] jybeta, double[] jygamma,
//            double[] jsgamma, int[] jigamma, int[] jialphabeta, double jpalpha,
//            double jpgamma, double[] jxalpha, double[] jxbeta,
//            double[] jxgamma, double jepsilon);
}
