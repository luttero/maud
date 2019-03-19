/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.qta.odf;

import com.jtex.arrays.Array1C;
import com.jtex.qta.kernel.Kernel;
import com.jtex.arrays.Array1D;
import com.jtex.external.MTEX;
import com.jtex.geom.Miller;
import com.jtex.geom.Symmetry;
import com.jtex.geom.Quaternion;
import com.jtex.geom.Vec3;
import com.jtex.qta.ODF;
import com.jtex.qta.PoleFigure;
import com.jtex.qta.kernel.DeLaValleePoussin;
import java.util.ArrayList;
import java.util.Formatter;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author hios
 */
public class UnimodalComponent extends ODFComponent {

    Symmetry cs, ss;
    double p;
    Kernel psi;
    Array1D coef;
    Quaternion g;
    double cosMax;

    public UnimodalComponent(Quaternion g, Array1D c, Kernel psi, Symmetry cs, Symmetry ss) {

        this.g = g;
        this.coef = c;
        this.psi = psi;
        this.cs = cs;
        this.ss = ss;
        this.p = 1D;

        this.coef.divided(this.coef.sum());
        cosMax = Math.cos(cs.getMaxAngel(new Symmetry()) / 2);
    }

    @Override
    public Array1D pdf(Miller h, Vec3 r) {

        Miller hs = h.normalize();
        Vec3 rr = r.normalize();

        Vec3 in;
        Vec3 out;
        if (hs.size() > 1 /* grid*/) {
            Quaternion sq = this.ss.multiply(this.g);

            Vec3 inn[] = new Vec3[cs.size()];
            for (int i = 0; i < cs.size(); i++) {
                inn[i] = (sq.multiply(this.cs.get(i))).inverse().rotate(rr);
            }
            in = Vec3.concat(inn);

            out = hs;
        } else {
            hs = hs.symmetrise();
            int nss = this.ss.size();
            Vec3 inn[] = new Vec3[hs.size() * nss];
            for (int i = 0; i < hs.size(); i++) {
                Vec3 gh = this.g.rotate(hs.get(i));
                for (int j = 0; j < nss; j++) {
                    inn[i * nss + j] = this.ss.get(j).rotate(gh);
                }
            }
            in = Vec3.concat(inn);
            out = rr;
        }

        Array1D inRhoTheta = in.getRhoTheta();
        Array1D outRhoTheta = out.getRhoTheta();

        Array1D A = psi.A();

//        // antipodal !!
        double[] f = MTEX.odf2pf(inRhoTheta.toDoubleArray(), outRhoTheta.toDoubleArray(), this.coef.toDoubleArray(), A.toDoubleArray());
        return new Array1D(f).divide(ss.size() * cs.size());

    }

    @Override
    public Array1D eval(Quaternion qr) {

//        double cosMax = Math.cos(cs.getMaxAngel(new Symmetry()) / 2);
        Quaternion qinv = g.inverse();
        double[] out = new double[qr.size()];

        double[] csa = cs.a();
        double[] csb = cs.b();
        double[] csc = cs.c();
        double[] csd = cs.d();

        double aa, bb, cc, dd, di;

        for (int l = 0; l < ss.size(); l++) {

            for (int i = 0; i < qr.size(); i++) {
                double dist[] = new double[qinv.size()];

                Quaternion mm = qinv.multiply(ss.get(l).multiply(qr.get(i)));
                double[] a = mm.a();
                double[] b = mm.b();
                double[] c = mm.c();
                double[] d = mm.d();

                for (int j = 0; j < qinv.size(); j++) {
                    aa = a[j];
                    bb = b[j];
                    cc = c[j];
                    dd = d[j];

                    for (int k = 0; k < csa.length; k++) {

                        di = Math.abs(aa * csa[k] + bb * csb[k] + cc * csc[k] + dd * csd[k]);

                        if (di > cosMax) {
                            dist[j] = di;
                            break;
                        } else if (di > dist[j]) {
                            dist[j] = di;
                        }
                    }

                }
                out[i] += psi.K(new Array1D(dist)).dot(coef) / (ss.size() * cs.size());
            }
        }

// make this fast
//        double epsilon = Math.PI / 3;//        quat gin = ;
//        quat mis = this.g.inverse();
////        quat mis = this.g;
//
////        double f[] = new double[q.size()];
////        for (int i = 0; i < q.size(); i++) {
////            
////            quat qcs = q.get(i).multiply(cs);
////
////            Array1D dist = Array1D.zeros(g.size());
////            for (int j = 0; j < cs.size(); j++) {
////                dist = dist.max(g.dot(qcs.get(j)).abs());
////            }
////
////            f[i] = psi.K(dist).dot(this.coef);
////
////        }
//        double f[] = new double[q.size()];
//        for (int i = 0; i < q.size(); i++) {
//            quat d = mis.multiply(q.get(i));
//            Array1D dist = Array1D.zeros(this.g.size());
//            for (int j = 0; j < this.cs.size(); j++) {
//
//                dist = dist.max(d.dot(this.cs.get(j)).abs());
//            }
//
//            f[i] = psi.K(dist).dot(this.coef);
//        }
//        return new Array1D(f).divided((double) this.cs.size());
        return new Array1D(out);

    }

    private double[] dot_special(Quaternion q2) {
        double cosMax = Math.cos(cs.getMaxAngel(new Symmetry()) / 2);

        Quaternion q = g.inverse();
        double[] out = new double[q2.size()];

        List<Thread> threads = new ArrayList<Thread>();
        for (int i = 0; i < q2.size(); i++) {
            EvalThread runnable = new EvalThread(q, q2.get(i), i, out);
            Thread t = new Thread(runnable);
            threads.add(t);
            t.start();
        }

        for (Thread t : threads) {
            try {
                t.join();
            } catch (InterruptedException ex) {
            }
        }

        return out;

    }

/* Luca    public static void main(String[] args) {
        Quaternion qr = Quaternion.rand(100);

        Symmetry cs = new Symmetry("m-3m");

        UnimodalComponent odf = new UnimodalComponent(qr,
                Array1D.fill(qr.size(), 1D / qr.size()),
                new DeLaValleePoussin(Math.toRadians(10)), cs, new Symmetry());
        System.out.println(odf.eval(qr));

        FourierComponent fodf = new FourierComponent(odf.calcFourier(32), odf.cs, odf.ss);

        System.out.println(fodf.C.multiply(fodf.C.conjugate()).real().sum());
        System.out.println(fodf.eval(qr));

        System.out.println(odf.pdf(new Miller(0, 0, 1, cs), new Vec3(0, 0, 1)));
        System.out.println(fodf.pdf(new Miller(0, 0, 1, cs), new Vec3(0, 0, 1)));

    } */

    @Override
    public double volume(Quaternion q, double radius) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public int bandwidth() {
        return psi.bandwidth();
    }

    @Override
    public double fibreVolume(Miller h, Vec3 r, double radius) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public ODFComponent rotate(Quaternion q) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public double getPortion() {
        return p;
    }

    @Override
    public void setPortion(double p) {
        this.p = p;
    }

    private class EvalThread implements Runnable {

        Quaternion qinv;
        Quaternion qr;
        double[] out;
        final int i;

        public EvalThread(Quaternion qinv, Quaternion qr, int i, double[] out) {
            this.qinv = qinv;
            this.qr = qr;
            this.out = out;
            this.i = i;

        }

        @Override
        public void run() {
            Quaternion mm = qinv.multiply(qr);

            double[] a = mm.a();
            double[] b = mm.b();
            double[] c = mm.c();
            double[] d = mm.d();
            double[] csa = cs.a();
            double[] csb = cs.b();
            double[] csc = cs.c();
            double[] csd = cs.d();

            double aa, bb, cc, dd, di;
            double dist[] = new double[qinv.size()];

            for (int j = 0; j < qinv.size(); j++) {
                aa = a[j];
                bb = b[j];
                cc = c[j];
                dd = d[j];

                for (int k = 0; k < csa.length; k++) {

                    di = Math.abs(aa * csa[k] + bb * csb[k] + cc * csc[k] + dd * csd[k]);

                    if (di > cosMax) {
                        dist[j] = di;
                        break;
                    } else if (di > dist[j]) {
                        dist[j] = di;
                    }
                }
            }

            out[i] = psi.K(new Array1D(dist)).dot(coef) / cs.size();
        }
    }

    protected String paramString() {
        Formatter formatter = new Formatter(Locale.US);

        String str = psi.toString();
        str += ",size=" + this.g.size();

//        String str = "hw=" + formatter.format("%.1f", Math.toDegrees(p2hw())).toString();
//        str += ",data=[" + formatter.toString() + "]";
        return str;
    }

    @Override
    public Array1C calcFourier(int L) {

        // nono do it right...
        Array1D cc = coef.divide(this.cs.size() * this.ss.size());

        Vec3 abg = g.euler("nfft");

        Array1D A = psi.A();
        L = Math.max(10, Math.min(A.size(), L + 1));
        A = A.get(Array1D.index(L).toIntArray());
        Array1C c_hat = new Array1C(
                MTEX.odf2fc(abg.getXYZ().toDoubleArray(), cc.toDoubleArray(), A.toDoubleArray()));

        Vec3 csabg = cs.euler("nfft");
        Array1C cs_hat = new Array1C(MTEX.odf2fc(csabg.getXYZ().toDoubleArray(),
                Array1D.ones(csabg.size()).toDoubleArray(),
                Array1D.ones(A.size()).toDoubleArray()));
        Vec3 ssabg = ss.euler("nfft");

        Array1C ss_hat = new Array1C(MTEX.odf2fc(ssabg.getXYZ().toDoubleArray(),
                Array1D.ones(ssabg.size()).toDoubleArray(),
                Array1D.ones(A.size()).toDoubleArray()));

        int lA = A.size();
        for (int l = 0; l < lA; l++) {
            int ndx[] = Array1D.fill(FourierComponent.deg2dim(l), FourierComponent.deg2dim(l + 1) - 1, 1).toIntArray();
            c_hat.set(ndx, ss_hat.get(ndx).matrixMultiply(c_hat.get(ndx).matrixMultiply(cs_hat.get(ndx), 2 * l + 1, 2 * l + 1), 2 * l + 1, 2 * l + 1));
        }

        return c_hat;
    }

}
