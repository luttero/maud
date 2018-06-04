/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.geom;

import com.jtex.arrays.Array1C;
import com.jtex.arrays.Array1D;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Formatter;
import java.util.List;
import java.util.Locale;
import java.util.Scanner;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

/**
 *
 * @author hios
 */
public class Miller extends Vec3 {

    public static void main(String[] args) throws IOException, ScriptException {

        
//        File f = new File("C:\\myWorkstation\\dev\\tbx\\gt\\gtdata\\cif\\Mg.cif");
        //        File f = new File("C:\\myWorkstation\\dev\\tbx\\gt\\gtdata\\cif\\olivin.cif");
        File f = new File("C:\\myWorkstation\\dev\\tbx\\gt\\gtpref\\cif\\Iron-alpha.cif");
        ScriptEngineManager mgr = new ScriptEngineManager();
        ScriptEngine engine = mgr.getEngineByName("JavaScript");
        System.out.println(engine);
        String foo = "importPackage(com.jtex.geom);"
                + "importPackage(com.jtex.arrays);"
                + "importPackage(com.jtex.plot);"
                + "r = Vec3.rand(103);"
                + "Plotter.show(Plotter.plot(r));";
        System.out.println(engine.eval(foo));

        System.out.println(hklFromCIF(f));
    }

    public static Miller hklFromCIF(File file) throws FileNotFoundException, IOException {
        InputStream in = new FileInputStream(file);
        Miller cs = hklFromCIF(new BufferedInputStream(in));
        in.close();
        return cs;
    }

    public static Miller hklFromCIF(BufferedInputStream in) throws IOException {

        in.mark(0);
        Symmetry CS = Symmetry.loadCIF(in);
        in.reset();

        Scanner scanner = new Scanner(in);
        String line = null;

        ScriptEngineManager mgr = new ScriptEngineManager();
        ScriptEngine engine = mgr.getEngineByName("JavaScript");
        List<String> symPos = new ArrayList<String>();
        List<Array1D> syms = new ArrayList<Array1D>();
        while (scanner.hasNext()) {

            line = scanner.nextLine();
            if (line.startsWith("_symmetry_equiv_pos_as_xyz") || line.startsWith("_space_group_symop_operation_xyz")) {
                while (!((line = scanner.nextLine()).startsWith("loop_") || line.startsWith("_"))) {
                    line = "[" + line.replace("'", "").trim() + "]";
                    symPos.add(line);
                }
            } else if (line.startsWith("_atom_site_label")) {
                int pi = 0, px = 0, py = 0, pz = 0;
                while ((line = scanner.nextLine()).startsWith("_")) {
                    if (line.startsWith("_atom_site_fract_x")) {
                        px = pi + 1;
                    } else if (line.startsWith("_atom_site_fract_y")) {
                        py = pi + 1;
                    } else if (line.startsWith("_atom_site_fract_z")) {
                        pz = pi + 1;
                    }
                    pi++;
                };
                do {
                    String[] split = line.trim().split("[ ]+");

                    engine.put("x", Double.valueOf(split[px]));
                    engine.put("y", Double.valueOf(split[py]));
                    engine.put("z", Double.valueOf(split[pz]));

                    double spos[] = new double[symPos.size() * 3];
                    for (int p = 0; p < symPos.size(); p++) {
                        try {
                            List eval = (List) engine.eval(symPos.get(p));

                            for (int i = 0; i < 3; i++) {
                                spos[p * 3 + i] = (Double) eval.get(i);
                            }
                        } catch (ScriptException ex) {
                            Logger.getLogger(Miller.class.getName()).log(Level.SEVERE, null, ex);
                        }

                    }
                    syms.add(new Array1D(spos));
                } while ((scanner.hasNext() && !((line = scanner.nextLine()).startsWith("loop_") || line.startsWith("_"))));

            }
        }
        Array1D atomPositions = Array1D.concat(syms.toArray(new Array1D[syms.size()]));
        int n = atomPositions.size();
        Array1D x = atomPositions.get(Array1D.fill(0, n - 2, 3).toIntArray());
        Array1D y = atomPositions.get(Array1D.fill(1, n - 1, 3).toIntArray());
        Array1D z = atomPositions.get(Array1D.fill(2, n, 3).toIntArray());

        TreeSet<Miller> m = new TreeSet<Miller>(new Comparator<Miller>() {

            @Override
            public int compare(Miller o1, Miller o2) {

                double d2 = o2.dspacing().get(0);
                double d1 = o1.dspacing().get(0);

                double eps = 1e7;
                d2 = Math.round(d2 * eps) / eps;
                d1 = Math.round(d1 * eps) / eps;

                int cmp = Double.compare(d2, d1);
                if (cmp == 0) {
                    if (o1.symmetrise().minus(o2).norm().abs().le(1e-3).any()) {
                        return 0;
                    } else {
                        return -1;
                    }
                } else {
                    return cmp;
                }
            }
        });

        int hmax = 7;
        for (double k = 0; k < hmax; k++) {
            for (double h = 0; h < hmax; h++) {
                for (double l = 0; l < hmax; l++) {
                    if (h + k + l != 0) {
                        Array1C F = new Array1C(x.multiply(h).plus(y.multiply(k)).plus(z.multiply(l)), Array1D.zeros(n / 3)).multiply(0, -2 * Math.PI).exp().sum();
                        if (F.multiply(F.conjugate()).real().get(0) > 1e-2) {
                            m.add(new Miller(h, k, l, CS));
                        }
                    }
                }
            }
        }

        return Miller.concat(m.toArray(new Miller[m.size()]));
    }

    public static Miller fromString(String hkl, Symmetry cs) {

        int sep = hkl.lastIndexOf("/");
        if (sep > 0) {
            hkl = hkl.substring(sep + 1);
        }
        int dot = hkl.lastIndexOf(".");
        if (dot > 0) {
            hkl = hkl.substring(0, dot);
        }

        Pattern p = Pattern.compile("((\\-\\d)|(\\d)){4}|((\\-\\d)|(\\d)){3}");
        Matcher m = p.matcher(hkl);

        List<Miller> h = new ArrayList<Miller>();
        while (m.find()) {
            h.add(fromString0(m.group(), cs));
        }

        return Miller.concat(h.toArray(new Miller[h.size()]));

    }

    private static Miller fromString0(String hkl, Symmetry cs) {

        double[] hs = new double[4];
        int k = 0;
        for (int i = 0; i < hkl.length(); i++) {
            if (hkl.charAt(i) == '-') {
                hs[k] = Double.parseDouble(hkl.substring(i, i + 2));
                i += 1;
            } else {
                hs[k] = Double.parseDouble(hkl.substring(i, i + 1));
            }
            k++;
        }
        if (k == 3) {
            return new Miller(hs[0], hs[1], hs[2], cs);
        } else if (k == 4) {
            return new Miller(hs[0], hs[1], hs[3], cs);
        }
        return null;

    }

    public static Miller concat(Miller... vs) {

        int sz = 0;
        for (Vec3 v : vs) {
            sz += v.size();
        }

        Miller vn = new Miller(sz, vs[0].symmetry, vs[0].asUVW);
        sz = 0;
        int n;
        for (Vec3 v : vs) {
            n = v.size();
            System.arraycopy(v.x(), 0, vn.x(), sz, n);
            System.arraycopy(v.y(), 0, vn.y(), sz, n);
            System.arraycopy(v.z(), 0, vn.z(), sz, n);
            sz += n;
        }
        return vn;
    }

    public static Vec3 v2d(Miller h) {
        Vec3 v = Mat3.fromVec3(h.symmetry.basisStar()).transpose().multiply(h);
        if (h.symmetry.isLaue("-3", "-3m", "6/m", "6/mmm")) {
            Array1D x = v.getX();
            Array1D y = v.getY();
            v.setX((x.multiply(2).minus(y)).divide(3));
            v.setY((y.multiply(2).minus(x)).divide(3));
        }
        return v;
    }

    public static Vec3 v2m(Miller h) {
        return Mat3.fromVec3(h.symmetry.basis()).transpose().multiply(h);
    }

    //reciprocal space
    public static Vec3 m2v(Array1D h, Array1D k, Array1D l, Symmetry s) {
        return Mat3.fromVec3(s.basisStar()).multiply(new Vec3(h, k, l));
    }

    //direct space
    public static Vec3 d2v(Array1D u, Array1D v, Array1D w, Symmetry s) {
        if (s.isLaue("-3", "-3m", "6/m", "6/mmm")) {
            Array1D x = u.getCopy();
            Array1D y = v.getCopy();
            u = x.multiply(2).plus(y);
            v = y.multiply(2).plus(x);
        }
        return Mat3.fromVec3(s.basis()).multiply(new Vec3(u, v, w));
    }

    Symmetry symmetry = new Symmetry();
    boolean asUVW = false;

    public Miller(int n) {
        super(n);
    }

    public Miller(Symmetry s, boolean asuvw) {
        super(0);
        this.symmetry = s;
        this.asUVW = asuvw;
    }

    public Miller(int n, Symmetry s, boolean asuvw) {
        super(n);
        this.symmetry = s;
        this.asUVW = asuvw;
    }

    public Miller() {
        this(0);
    }

    public Miller(Vec3 v, Symmetry s, String... args) {
        set(v);
        this.symmetry = s;

//        this.asUVW = false;
//        for (String arg : args) {
//            if (arg.equals("uvw") || arg.equals("uvwt")) {
//                this.asUVW = true;
//            }
//        }
    }

    public Miller(Array1D h, Array1D k, Array1D l, Symmetry s, boolean asuvw) {
        this(h, k, l, s, (asuvw ? "uwv" : ""));
    }

    public Miller(double h, double k, double l, Symmetry s, String... args) {
        this(new Array1D(h), new Array1D(k), new Array1D(l), s, args);
    }

    public Miller(Array1D h, Array1D k, Array1D l, Symmetry s, String... args) {
        this.symmetry = s;

        this.asUVW = false;
        for (String arg : args) {
            if (arg.equals("uvw") || arg.equals("uvwt")) {
                this.asUVW = true;
            }
        }

        Vec3 v = this.asUVW ? d2v(h, k, l, s) : m2v(h, k, l, s);

        set(v);
    }

    public Miller symmetrise() {
        Vec3[] vs = new Vec3[symmetry.size()];
        for (int i = 0; i < symmetry.size(); i++) {
            vs[i] = symmetry.get(i).rotate(this);
        }
        return new Miller(Vec3.concat(vs), symmetry);
    }

    public Miller get(int... ndx) {
        return new Miller(super.get(ndx), this.symmetry);
    }

    public Miller get(boolean[] ndx) {
        return new Miller(super.get(ndx), this.symmetry);
    }

    public Vec3 toVec3() {
        return new Vec3(this);
    }

    @Override
    public Miller normalize() {
        Vec3 vn = super.normalize();

        return new Miller(vn, this.symmetry); //To change body of generated methods, choose Tools | Templates.
    }

    public Array1D dspacing() {
        return this.norm().ldivide(1);
    }

    public String toHKL(int i) {
        DecimalFormat df = new DecimalFormat("#");
        double[] hkl = getHKL(i);
        return (df.format(hkl[0]) + df.format(hkl[1]) + df.format(hkl[2]));
    }

    public String toHKL() {
        DecimalFormat df = new DecimalFormat("#");
        String str = "";
        for (int i = 0; i < size(); i++) {
            double[] hkl = getHKL(i);

//             if any(strcmp(Laue(m.CS),{'-3','-3m','6/m','6/mmm'}))
            if (symmetry.isLaue("-3", "-3m", "6/m", "6/mmm")) {
                hkl = new double[]{hkl[0], hkl[1], -hkl[0] - hkl[1], hkl[2]};
            }

            str += "{";
            for (double d : hkl) {
                if (Math.abs(d) < 1e-4) {
                    str += "0";
                } else if (d < 0) {
                    str += "\u0305" + df.format(-d);
                } else {
                    str += df.format(d);
                }
            }
            str += "}";
            if (i < size() - 1) {
                str += "\n";
            }

        }
        return str;
    }

    @Override
    public String toString() {
        return getClass().getName() + "[" + paramString() + "]";
    }

    public static void maind(String[] args) {

        System.out.println(new Miller());

//        Symmetry cs = new Symmetry("-1", 3, 4, 5, Math.toRadians(85), Math.toRadians(92), Math.toRadians(122));
        Symmetry cs = new Symmetry("6/mmm");
        cs.basis().print();
        cs.basisStar().print();

        cs.elements().euler().toDegrees().print();

        Array1D u = new Array1D(1, 2, -3, 4, 5);
        Array1D v = new Array1D(2, -2, 3, 4, 5);
        Array1D w = new Array1D(6, 2, 3, 4, 5);

        Miller h = new Miller(1, 2, 3, cs);

        h.print();
        h.symmetrise().print();
    }

    @Override
    public void print() {
        Vec3 v = this.asUVW ? v2d(this) : v2m(this);

        v.print();
    }

    public double[] getHKL(int... ndx) {
        return v2m(this.get(ndx)).getDouble();
    }

    public double[] getUWV() {
        return v2d(this).getDouble();
    }

    @Override
    protected String paramString() {

        Vec3 v = this.asUVW ? v2d(this) : v2m(this);
        String ochar = this.asUVW ? "<" : "{";
        String cchar = this.asUVW ? ">" : "}";
        Formatter formatter = new Formatter(Locale.US);
        for (int i = 0; i < this.size(); i++) {
            formatter.format("%s%.2f,%.2f,%.2f%s", ochar, v.x()[i], v.y()[i], v.z()[i], cchar);
        }

        String str = formatter.toString();
        str += ",symmetry={" + symmetry.getGroup() + "}";

        return str;
    }

    public Miller setCS(Symmetry cs) {

        if (size() > 0) {
            Vec3 v = v2m(this);
            set(m2v(v.getX(), v.getY(), v.getZ(), cs));
        }
        this.symmetry = cs;

        return this;
    }

    public Symmetry getCS() {
        return this.symmetry;
    }
}
