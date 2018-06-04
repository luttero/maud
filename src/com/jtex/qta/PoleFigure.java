/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.qta;

import com.jtex.arrays.Array1D;
import com.jtex.geom.Miller;
import com.jtex.geom.Symmetry;
import com.jtex.geom.Vec3;
import com.jtex.qta.kernel.Kernel;
import com.jtex.qta.file.PoleFigureLoader;
import com.jtex.qta.file.BrukerGpolLoader;
import com.jtex.qta.file.DubnaLoader;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Formatter;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author hios
 */
public class PoleFigure {

    public static void main(String args[]) {
        File f = new File("C:\\myThinkpad\\workspace\\mtex\\mtex-3.5.0\\data\\PoleFigure\\dubna\\Q(10-10)_amp.cnv");
//        File f = new File("C:\\myThinkpad\\workspace\\mtex\\mtex-3.5.0\\data\\PoleFigure\\gpol\\Al_311.gpol");
        PoleFigure.load(f);
    }

    public static PoleFigure load(File... file) {
        if (file.length < 1) {
            return null;
        }

        PoleFigureLoader loaders[] = new PoleFigureLoader[]{
            new BrukerGpolLoader(),
            new DubnaLoader()};

        PoleFigureLoader ld = null;
        for (PoleFigureLoader loader : loaders) {
            try {
                loader.load(file[0]);
                ld = loader;
            } catch (Exception ex) {
            }
        }

        if (ld != null) {
            PoleFigure pf = new PoleFigure();
            for (int i = 0; i < file.length; i++) {
                try {
                    PoleFigure pfs = ld.load(file[i]);
                    System.out.println(pfs);
                    pf.add(pfs);
                } catch (IOException ex) {
                    Logger.getLogger(PoleFigure.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
            return pf;
        }
        return null;

    }

    public static PoleFigure union(PoleFigure[] toArray) {
        return union(Arrays.asList(toArray));
    }

    public static PoleFigure union(List<PoleFigure> measuredPFs) {

        PoleFigure npf = new PoleFigure();
        for (PoleFigure pf : measuredPFs) {

            npf.cs = pf.getCS();
            for (int i = 0; i < pf.size(); i++) {
                boolean match = false;
                for (int j = 0; j < npf.size(); j++) {
                    if (pf.h.get(i).angle(npf.h.get(j)).le(1e-4).any()) {
                        npf.data.set(j, Array1D.concat(npf.data.get(j), pf.data.get(i)));
                        npf.r.set(j, Vec3.concat(npf.r.get(j), pf.r.get(i)));
                        match = true;
                    }
                }

                if (!match) {
                    npf.add(pf.get(i));
                }

            }
        }
        return npf;
    }

    private Symmetry cs, ss;
    private List<Miller> h;
    private List<Vec3> r;
    private List<Array1D> data;
    private List<Array1D> c;

    public PoleFigure() {
        h = new ArrayList<Miller>();
        data = new ArrayList<Array1D>();
        r = new ArrayList<Vec3>();
        c = new ArrayList<Array1D>();
        cs = new Symmetry();
        ss = new Symmetry();
    }

    public PoleFigure(Miller h, Vec3 r, Array1D data) {
        this(h, r, data, 1);
    }

    public PoleFigure(Miller h, Vec3 r, Array1D data, double c) {
        this(h, r, data, new Array1D(c));
    }

    public PoleFigure(Miller h, Vec3 r, Array1D data, Array1D c) {
        this();
        this.cs = h.getCS();
        this.h.add(h);
        this.r.add(r);
        this.data.add(data);
        this.c.add(c);
    }

    public PoleFigure(Miller h, Symmetry ss, Vec3 r, Array1D data, Array1D c) {
        this();
        this.cs = h.getCS();
        this.ss = ss;
        this.h.add(h);
        this.r.add(r);
        this.data.add(data);
        this.c.add(c);
    }

    public void add(PoleFigure pf) {

        if (this.cs == null) {
            this.cs = pf.cs;
        }

        // !! check for symmetry
        this.h.addAll(pf.h);
        this.r.addAll(pf.r);
        this.data.addAll(pf.data);
        this.c.addAll(pf.c);
    }

    public PoleFigure get(int... ndx) {
        PoleFigure pf = new PoleFigure();
        for (int i = 0; i < ndx.length; i++) {
            int j = ndx[i];
            pf.h.add(this.h.get(j));
            pf.r.add(this.r.get(j));
            pf.data.add(this.data.get(j));
            pf.c.add(this.c.get(j));
        }
        return pf;
    }

    @Override
    public String toString() {
        return getClass().getName() + "[" + paramString() + "]";
    }

    protected String paramString() {

        String str = "size=" + h.size();
        str += ",symmetry=" + cs.getGroup().toString();

        Formatter formatter = new Formatter(Locale.US);
        for (int i = 0; i < h.size(); i++) {
            Miller hh = h.get(i);
            formatter.format("(h=");
            for (int j = 0; j < hh.size(); j++) {
//                double[] hkl = hh.getHKL(j);
                formatter.format("%s", hh.get(j).toHKL());
            }
            formatter.format(",data=%d)", data.get(i).size());

            if (i < h.size() - 1) {
                formatter.format(",");
            }
        }

        str += ",[" + formatter.toString() + "]";
        return str;
    }

    public void setCS(Symmetry cs) {
        this.cs = cs;
        for (Miller hh : h) {
            hh.setCS(cs);
        }
    }

    public void setH(int i, Miller hh) {
        this.h.set(i, hh);
        this.c.set(i, Array1D.fill(hh.size(), 1));
    }

    public void setH(Miller hh) {
        for (int i = 0; i < this.h.size(); i++) {
            this.h.set(i, hh);
            this.c.set(i, Array1D.fill(hh.size(), 1));
        }
    }

    public void setC(Array1D c) {
        for (int i = 0; i < this.h.size(); i++) {
            this.c.set(i, c);
        }
    }

    public void setC(int i, Array1D c) {
        this.c.set(i, c);
    }

    public Symmetry getCS() {
        return cs;
    }

    public Symmetry getSS() {
        return ss;
    }

    public Miller getH() {
        return Miller.concat(h.toArray(new Miller[h.size()]));
    }

    public Vec3 getR() {
        return Vec3.concat(r.toArray(new Vec3[r.size()]));
    }

    public Array1D getRhoTheta() {
        return getR().getRhoTheta();
    }

    public Array1D getData() {
        return Array1D.concat(data.toArray(new Array1D[data.size()]));
    }

    public Array1D getSuperposition() {
        return Array1D.concat(c.toArray(new Array1D[c.size()]));
    }

    public int size() {
        return h.size();
    }

    public Array1D getQuadratureWeights() {

        Array1D w[] = new Array1D[size()];
        for (int i = 0; i < size(); i++) {
            int n = r.get(i).size();
            w[i] = Array1D.fill(n, 1D / n);
        }

        return Array1D.concat(w);
    }

    public Array1D getQuadratureWeights(Kernel psi) {

        Array1D w[] = new Array1D[size()];
        for (int i = 0; i < size(); i++) {
            Array1D qw = Vec3.kernelDensity(r.get(i), r.get(i), psi);
            double upper = qw.mean() + qw.std();
            double qwmax = qw.get(qw.lb(upper)).max();
            qw.set(qw.gb(qwmax), qwmax);
            qw = qw.ldivide(qw.sum());
            w[i] = qw.divided(qw.sum());
        }

        return Array1D.concat(w);
    }

    public Array1D calcNormalization(PoleFigure pf2) {
        double alpha[] = new double[size()];
        for (int i = 0; i < size(); i++) {
            Array1D w = get(i).getQuadratureWeights();

            System.out.println(data.get(i).size());
            System.out.println(w.size());

            System.out.println(pf2.data.get(i).size());

            alpha[i] = data.get(i).max(0).dot(w) / pf2.data.get(i).max(0).dot(w);
        }
        return new Array1D(alpha);
    }

    public Array1D getGridSize() {
        double[] sz = new double[data.size()];
        int i = 0;
        for (Array1D d : data) {
            sz[i++] = d.size();
        }
        return new Array1D(sz);
    }

    public Array1D getMultiplicity() {
        double[] sz = new double[data.size()];
        int i = 0;
        for (Miller d : h) {
            sz[i++] = ss.size() * d.size() * cs.size();
        }
        return new Array1D(sz);
    }

    public void setData(Array1D d) {
        int i = 0;
        int[] sz = new int[data.size()];
        for (Array1D dd : data) {
            sz[i++] = dd.size();
        }
        Array1D ddd[] = d.split(sz);

        for (i = 0; i < size(); i++) {
            data.set(i, ddd[i]);
        }

    }

    public void delete(int[] ndx) {
        for (int i = ndx.length - 1; i >= 0; i--) {
            int j = ndx[i];
            h.remove(j);
            r.remove(j);
            data.remove(j);
            c.remove(j);
        }
    }

    void setSS(Symmetry ss) {
        this.ss = ss;
    }

}
