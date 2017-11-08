/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.geom.grid;

import com.jtex.arrays.Array1D;
import com.jtex.geom.Symmetry;
import com.jtex.geom.Quaternion;
import com.jtex.geom.Vec3;

/**
 *
 * @author hios
 */
public class SO3Grid {

    public static boolean[] fundamental_region(Quaternion q, Symmetry cs) {
        return fundamental_region(q, cs, new Symmetry());
    }

    public static boolean[] fundamental_region(Quaternion q, Symmetry cs, Symmetry ss) {
        Vec3 v = new Vec3();
        Array1D h = new Array1D();

        if (cs.isLaue("m-3m", "m-3")) {
            v = Vec3.concat(v, new Vec3(new Array1D(1, 1, 1, 1, -1, -1, -1, -1),
                    new Array1D(1, 1, -1, -1, 1, 1, -1, -1),
                    new Array1D(1, -1, 1, -1, 1, -1, 1, -1)));

            h = Array1D.concat(h, Array1D.fill(8, Math.sqrt(3) / 3));
            if (cs.isLaue("m-3m")) {
                v = Vec3.concat(v, new Vec3(new Array1D(1, -1, 0, 0, 0, 0), new Array1D(0, 0, 1, -1, 0, 0), new Array1D(0, 0, 0, 0, 1, -1)));
                h = Array1D.concat(h, Array1D.fill(6, Math.sqrt(2) - 1));
            }
        }

        if (ss.isLaue("mmm")) {
            v = Vec3.concat(v, new Vec3(new Array1D(-1, 0), new Array1D(0, -1), new Array1D(0, 0)));
            h = Array1D.concat(h, new Array1D(0, 0));
        }

        Vec3 r = q.rodrigues();
        v = v.normalize();

        Array1D b = Array1D.zeros(r.size());
        for (int i = 0; i < v.size(); i++) {
            b.plusd(r.dot(v.get(i)).g(h.get(i)));
        }
        return b.isNull().toBooleanArray();
    }

    public static SO3Grid equispacedSO3Grid(double res, Symmetry cs, Symmetry ss) {

        Array1D range = Symmetry.symmetry2Euler(cs, ss);
        double maxalpha = range.get(0);
        double maxbeta = range.get(1);
        double maxgamma = range.get(2) / 2;

        // seems to be number of points not res
        if (res > 1) {
            if (cs.isLaue("m-3")) {
                res *= 3;
            } else if (cs.isLaue("m-3m")) {
                res *= 2;
            }
            res = 2 / (res / Math.pow(maxbeta * maxgamma, 1 / 3));
        }

        S2Grid alphabeta = S2Grid.equispacedS2Grid(res, 0, maxbeta, 0, maxalpha, true);

        alphabeta.setPeriod(-1, maxalpha);

        int ap22 = (int) Math.round(2 * maxgamma / res);
        Array1D gamma = Array1D.fill(0, ap22 - 1, 1).multiply(2 * maxgamma / ap22).minus(maxgamma);

        S1Grid gammas[][] = new S1Grid[alphabeta.getThetaSize()][];
        double[] th = alphabeta.getTheta().x();
        for (int i = 0; i < alphabeta.getThetaSize(); i++) {

            double cb = Math.cos(th[i]);
            Array1D alpha = new Array1D(alphabeta.getRho(i).x());

            Array1D ca = alpha.cos();

            Array1D re = (ca.multiply(cb)).plus(ca);
            Array1D im = (alpha.sin().multiply(cb + 1)).multiply(-1);

            Array1D dgamma = im.atan2(re);

            gammas[i] = new S1Grid[alpha.size()];
            for (int j = 0; j < alpha.size(); j++) {
                double[] xg = gamma.plus(dgamma.get(j)).toDoubleArray();
                gammas[i][j] = new S1Grid(xg, xg[0], maxgamma);
            }
        }
        SO3Grid SO3 = new SO3Grid(alphabeta, gammas);

        return SO3.subgrid(fundamental_region(SO3.toQuat(), cs));
    }

    private SO3Grid subgrid(boolean[] take) {

        int offset = 0;
        int lb = 0;
        int[] la = new int[alphabeta.getThetaSize()];
        int[][] lg = new int[alphabeta.getThetaSize()][];

        int i = 0;
        int j;
        for (S1Grid[] gi : gamma) {
            lg[i] = new int[alphabeta.getRhoSize(i)];
            j = 0;
            for (S1Grid gj : gi) {
                for (double g : gj.x()) {
                    if (take[offset++]) {
                        lg[i][j]++;
                    }
                }
                if (lg[i][j] > 0) {
                    la[i]++;
                }
                j++;
            }

            if (la[i] > 0) {
                lb++;
            }

            i++;
        }
        i = 0;
        int ii = 0;
        int jj = 0;
        int kk = 0;
        offset = 0;

        double[] beta = new double[lb];
        S1Grid nalpha[] = new S1Grid[lb];
        S1Grid ngamma[][] = new S1Grid[lb][];

        for (double b : alphabeta.getTheta().x()) {
            if (la[i] > 0) {
                j = 0;
                jj = 0;
                beta[ii] = b;
                double alpha[] = new double[la[i]];
                ngamma[ii] = new S1Grid[la[i]];
                for (double a : alphabeta.getRho(i).x()) {
                    if (lg[i][j] > 0) {
                        kk = 0;
                        alpha[jj] = a;
                        double gammas[] = new double[lg[i][j]];
                        for (double g : gamma[i][j].x()) {
                            if (take[offset++]) {
                                gammas[kk++] = g;
                            }
                        }
                        ngamma[ii][jj++] = new S1Grid(gammas, gamma[i][j].getMin(), gamma[i][j].getPeriod());
                    } else {
                        offset += gamma[i][j].size();
                    }
                    j++;
                }
                nalpha[ii++] = new S1Grid(alpha, alphabeta.getRhoMin(i), alphabeta.getRhoPeriod(i));
            } else {
                for (S1Grid gg : gamma[i]) {
                    offset += gg.size();
                }
            }
            i++;
        }
        S1Grid nbeta = new S1Grid(beta, alphabeta.getThetaMin(), alphabeta.getThetaPeriod());
        return new SO3Grid(new S2Grid(nbeta, nalpha), ngamma);

    }

    public static void main(String[] args) {
        Symmetry cs = new Symmetry("m-3m");

        SO3Grid g = SO3Grid.equispacedSO3Grid(Math.toRadians(2.5), cs, new Symmetry());

        System.out.println("sz " + g.size());

        double t1, t2;

        Quaternion qr = Quaternion.rand(1000);
        long t = System.nanoTime();
        Quaternion q = g.toQuat().inverse();

        double[] csa = cs.a();
        double[] csb = cs.b();
        double[] csc = cs.c();
        double[] csd = cs.d();
        double cos = Math.cos(cs.getMaxAngel(new Symmetry()) / 2);
        double aa, bb, cc, dd, di;

        double epsi = .5;

        for (int i = 0; i < qr.size(); i++) {

            Quaternion mm = q.multiply(qr.get(i));

            double[] a = mm.a();
            double[] b = mm.b();
            double[] c = mm.c();
            double[] d = mm.d();
            double dist[] = new double[q.size()];

            for (int j = 0; j < q.size(); j++) {
                aa = a[j];
                bb = b[j];
                cc = c[j];
                dd = d[j];

                for (int k = 0; k < csa.length; k++) {

                    di = Math.abs(aa * csa[k] + bb * csb[k] + cc * csc[k] + dd * csd[k]);

                    if (di > cos) {
                        dist[j] = di;
                        break;
                    } else if (di > dist[j]) {
                        dist[j] = di;
                    }
                }

            }
        }
        System.out.println("ex " + (t1 = (System.nanoTime() - t) / 1e9));

        t = System.nanoTime();
        Vec3 qre = qr.euler("ZYZ");

        Quaternion qcs = cs.rotation_special();

        for (int i = 0; i < qr.size(); i++) {
            double[] dist_region = new double[g.getSize()];
            Vec3 euler = qr.get(i).multiply(qcs).euler("ZYZ");

            for (int j = 0; j < qcs.size(); j++) {
                double[] tmp = g.find_dist_region(euler.x()[j], euler.y()[j], euler.z()[j], .5);
                for (int k = 0; k < g.size; k++) {
                    dist_region[k] = Math.max(tmp[k], dist_region[k]);
                }
            }
//            System.out.println(new Array1D(dist_region));
        }
        System.out.println("ex " + (t2 = (System.nanoTime() - t) / 1e9));
        System.out.println("     GTT  " + g.tt);

        System.out.println(t1 / t2);
    }

    public int size() {
        int sz = 0;
        for (S1Grid[] g1 : gamma) {
            for (S1Grid g2 : g1) {
                sz += g2.size();
            }
        }
        return sz;
    }

    public int getSize() {
        return size;
    }

    public Vec3 expand() {
        double xalpha[] = new double[size];
        double xbeta[] = new double[size];
        double xgamma[] = new double[size];

        int csg = 0;
        int ib[] = new int[alphabeta.getThetaSize() + 1];
        int ia[] = new int[size + 1];
        int i = 1;
        int j = 1;
        for (S1Grid[] g1 : gamma) {
            for (S1Grid g2 : g1) {
                System.arraycopy(g2.x(), 0, xgamma, csg, g2.size());
                csg += g2.size();
                ia[j++] = csg;
            }
            ib[i++] = csg;
        }

        double th[] = alphabeta.getTheta().x();
        for (i = 0; i < alphabeta.getThetaSize(); i++) {
            double beta = th[i];
            for (j = ib[i]; j < ib[i + 1]; j++) {
                xbeta[j] = beta;
            }
        }

        int jj = 0;
        for (i = 0; i < alphabeta.getThetaSize(); i++) {
            double[] alpha = alphabeta.getRho(i).x();
            for (j = 0; j < alphabeta.getRhoSize(i); j++) {
                double a = alpha[j];
                for (int k = 0; k < gamma[i][j].size(); k++) {
                    xalpha[jj++] = a;
                }
            }

        }
        return new Vec3(xalpha, xbeta, xgamma);
    }

    public Quaternion toQuat() {
        Vec3 zyz = expand();
        return Quaternion.rotZYZ(zyz.getX(), zyz.getY(), zyz.getZ());
    }

    private static double MOD(double A, double B) {
        return A - B * Math.floor(A / B);
    }

    private static double MOD0(double A, double B) {
        return A - B * Math.round(A / B);
    }

    private static double MOD3(double A, double B, double C) {
        return A - B * Math.floor((A - C) / B);
    }

    S2Grid alphabeta;
    S1Grid gamma[][];
    int size;

    double palpha = 0;
    double pgamma = 0;

    public SO3Grid(S2Grid alphabeta, S1Grid[][] gamma) {
        this.alphabeta = alphabeta;
        this.gamma = gamma;

        size = size();
    }

    double tt = 0;

    public double[] find_dist_region(double xalpha, double xbeta, double xgamma, double epsilon) {
        double cxb, cxg, cyb, syb, sxb, sxg, cda, sda;
        double a, b, c, d, e, cc, ss, re, im, dg, dg2;

        xalpha = MOD(xalpha, alphabeta.getRhoPeriod(0));

        /* calculate sin and cos */
        cxb = StrictMath.cos(xbeta);
        sxb = StrictMath.sin(xbeta);

        boolean[][] find_region = alphabeta.find_region(xbeta, xalpha, epsilon);

        int offset = 0;
        double[] dist = new double[size];
        for (int i = 0; i < alphabeta.getThetaSize(); i++) {
            if (find_region[i] != null) {
                double ybeta = alphabeta.getTheta(i);
                cyb = StrictMath.cos(ybeta);
                syb = StrictMath.sin(ybeta);

                boolean[] bs = find_region[i];
                for (int j = 0; j < alphabeta.getRhoSize(i); j++) {
                    if (bs[j]) {

                        double yalpha = alphabeta.getRho(i, j);

                        double dalpha = MOD0(xalpha - yalpha, alphabeta.getRhoPeriod(i));
                        double gmma = MOD3(xgamma, gamma[i][j].getPeriod(), gamma[i][j].getMin());

                        cxg = StrictMath.cos(gmma);
                        sxg = StrictMath.sin(gmma);

                        cda = StrictMath.cos(dalpha);
                        sda = StrictMath.sin(dalpha);

                        cc = cxb * cyb;
                        ss = sxb * syb;
                        a = cc + ss * cda;
                        d = (cc + 1D) * cda + ss;
                        e = (cxb + cyb) * sda;

                        b = StrictMath.sqrt(d * d + e * e);
                        c = (1D + 2D * StrictMath.cos(epsilon) - a) / b;

                        re = d * cxg - e * sxg;
                        im = -d * sxg - e * cxg;
                        dg = -StrictMath.atan2(im, re);
                        long t = System.nanoTime();
                        if (c > 1D) {
                            continue;
                        } else if (c < -1.0 + 1e-10) {
                            c = Math.PI;
                        } else {
                            c = StrictMath.acos(c);
                        }

                        boolean[] gamma_region = gamma[i][j].find_region(dg, c);
                        for (int m = 0; m < gamma_region.length; m++) {
                            if (gamma_region[m]) {
//                                dist[offset] = 1;

                                dg2 = MOD0(gamma[i][j].x(m) - dg, gamma[i][j].getPeriod());
                                dist[offset] = StrictMath.cos(.5 * StrictMath.acos(StrictMath.min(1D, StrictMath.max(-1D, .5D * (a + b * StrictMath.cos(dg2) - 1D)))));
                            }
                            offset++;
                        }

                        tt += (System.nanoTime() - t) / 1e9;
                    } else {
                        offset += gamma[i][j].size();
                    }
                }
            } else {
                for (S1Grid g : gamma[i]) {
                    offset += g.size();
                }
            }
        }

        return dist;

    }

}
