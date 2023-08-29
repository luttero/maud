/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.geom;

import com.jtex.arrays.Array1D;
import com.jtex.qta.file.BrukerGpolLoader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.Scanner;

/**
 *
 * @author hios
 */
public class Symmetry extends Quaternion {

    public static enum Alignment {

        X_a("X||a"),
        X_astar("X||a*"),
        X_b("X||b"),
        X_bstar("X||b*"),
        Y_a("Y||a"),
        Y_astar("Y||a*"),
        Y_b("Y||b"),
        Y_bstar("Y||b*"),
        Z_c("Z||c"),
        Z_cstar("Z||c*");

        String alignment;

        private Alignment(String s) {
            this.alignment = s;
        }

        @Override
        public String toString() {
            return this.alignment;
        }

        public boolean isX() {
            return alignment.startsWith("X");
        }

        public boolean isY() {
            return alignment.startsWith("Y");
        }

        public boolean isZ() {
            return alignment.startsWith("Z");
        }

        public boolean isa() {
            return alignment.endsWith("a");
        }

        public boolean isastar() {
            return alignment.endsWith("a*");
        }

        public boolean isb() {
            return alignment.endsWith("b");
        }

        public boolean isbstar() {
            return alignment.endsWith("b*");
        }

        public boolean isc() {
            return alignment.endsWith("c");
        }

        public boolean iscstar() {
            return alignment.endsWith("c*");
        }

        public boolean is(String align) {
            return alignment.equals(align);
        }

        public static Alignment get(String align) {
            EnumSet<Alignment> allOf = EnumSet.allOf(Alignment.class);
            Iterator<Alignment> it = allOf.iterator();
            while (it.hasNext()) {
                Alignment next = it.next();
                if (next.is(align)) {
                    return next;
                }
            }
            return null;
        }
    }

//    Alignment alignX, alignY, alignZ;
    private static String hms2point(String s) {

        s = s.replaceAll("[ABCFIPR]", "");
        s = s.replaceAll("[abcdn]", "m");
        for (int i = 0; i < s.length() - 1; i++) {
            char ch = s.charAt(i);
            if (Character.isDigit(ch) && Character.isDigit(s.charAt(i + 1))) {
                s = s.substring(0, i + 1).concat(s.substring(i + 2));
            }
        }
        if (s.matches("[m2-6]")) {
            s = s.replace("1", "");
        }
        s = s.toLowerCase();

        return s;
    }

    public static Symmetry loadSampleCIF() {
        InputStream in = BrukerGpolLoader.class.getResourceAsStream("/com/jtex/data/cif/Mg-Magnesium.cif");
        return loadCIF(in);
    }

    public static Symmetry loadCIF(File cif) throws IOException {
        InputStream in = new FileInputStream(cif);
        Symmetry cs = loadCIF(in);
        in.close();
        return cs;
    }

    private static double parseDouble(String in) {

        in = in.trim();
        int ndx = in.indexOf("(");
        if (ndx > 0) {
            in = in.substring(0, ndx);
        }
        return Double.parseDouble(in);
    }

    public static Symmetry loadCIF(InputStream in) {
        Scanner scanner = new Scanner(in);
        double a = 0, b = 0, c = 0, alpha = 0, beta = 0, gamma = 0;
        String group = null;
        String line = null;
        while (scanner.hasNext()) {
            line = scanner.nextLine();
            if (line.startsWith("_cell_angle_alpha")) {
                alpha = Math.toRadians(parseDouble(line.substring(17)));
            } else if (line.startsWith("_cell_angle_beta")) {
                beta = Math.toRadians(parseDouble(line.substring(16)));
            } else if (line.startsWith("_cell_angle_gamma")) {
                gamma = Math.toRadians(parseDouble(line.substring(18)));
            } else if (line.startsWith("_cell_length_a")) {
                a = parseDouble(line.substring(14));
            } else if (line.startsWith("_cell_length_b")) {
                b = parseDouble(line.substring(14));
            } else if (line.startsWith("_cell_length_c")) {
                c = parseDouble(line.substring(14));
            } else if (line.startsWith("_symmetry_space_group_name_Hall")) {
                group = line.substring(31).trim();
            } else if (line.startsWith("_symmetry_space_group_name_H-M")) {
                group = line.substring(30).trim();
            } else if (line.startsWith("_symmetry_point_group_name_H-M")) {
                group = line.substring(30).trim();
            } else if (line.startsWith("_space_group_name_H-M_alt")) {
	            group = line.substring(25).trim();
            } else if (line.startsWith("_space_group_name_Hall")) {
	            group = line.substring(22).trim();
            } else if (line.startsWith("_space_group_IT_number")) {
	            group = line.substring(22).trim();
            } else if (line.startsWith("_symmetry_Int_Tables_number")) {
	            group = line.substring(27).trim();
            }
        }

        group = group.replaceAll("[ '/]", "");
        return new Symmetry(group, false, a, b, c, alpha, beta, gamma);
    }

    public static Vec3 calcAxis(double a, double b, double c, double alpha, double beta, double gamma, Alignment[] align) {
        double ca = Math.cos(alpha), cb = Math.cos(beta), cg = Math.cos(gamma), sg = Math.sin(gamma);
        double ys = (ca - cb * cg) / sg;
        double zs = Math.sqrt(1 + 2 * ca * cb * cg - (ca * ca + cb * cb + cg * cg)) / sg;

        Vec3 aa = Vec3.xvector();
        Vec3 bb = new Vec3(cg, sg, 0);
        Vec3 cc = new Vec3(cb, ys, zs).normalize();

        Vec3 astar = bb.cross(cc).normalize();
        Vec3 bstar = cc.cross(aa).normalize();
        Vec3 cstar = aa.cross(bb).normalize();

        Vec3 xyzNew[] = new Vec3[3];
        for (int i = 0; i < align.length; i++) {
            Vec3 other = null;
            Alignment ali = align[i];
            if (ali == null) {
            } else if (ali.isa()) {
                xyzNew[i] = aa;
                other = cstar;
            } else if (ali.isb()) {
                xyzNew[i] = bb;
            } else if (ali.isc()) {
                xyzNew[i] = cc;
                other = astar;
            } else if (ali.isastar()) {
                xyzNew[i] = astar;
                other = cc;
            } else if (ali.isbstar()) {
                xyzNew[i] = bstar;
            } else if (ali.iscstar()) {
                xyzNew[i] = cstar;
                other = aa;
            }
            boolean otherAx = (i % 2) == 0 && (align[2 - i] == null) && (align[1] == null);

            if (other != null && otherAx) {
                xyzNew[2 - i] = other;
            }

        }

        for (int i = 0; i < 3; i++) {
            if (xyzNew[i] == null) {
                xyzNew[i] = xyzNew[(i + 1) % 3].cross(xyzNew[(i + 2) % 3]).normalize();
            }
        }

        for (int i = 0; i < 3; i++) {
            xyzNew[i] = xyzNew[i].normalize();
        }

        Mat3 M = Mat3.fromVec3(xyzNew[0], xyzNew[1], xyzNew[2]);
        Mat3 Mo = Mat3.fromVec3(aa, bb, cc);
        Mat3 D = Mat3.fromDiag(a, b, c);
        Mat3 B = M.transpose().multiply(Mo).multiply(D);

        return B.toVec3();
    }

    public static Quaternion Axis(Vec3 ax, int nfold) {
        return Quaternion.rotAxisAngle(ax, Array1D.fill(0, nfold - 1, 1).multiply(2 * Math.PI / nfold));
    }

    public static Quaternion calcQuat(PointGroup schon, Vec3 axis) {

        Quaternion rot = null;

        Vec3 a = axis.get(0);
        Vec3 b = axis.get(1);
        Vec3 c = axis.get(2);
        Vec3 lllaxis = new Vec3(1, 1, 1);
        Vec3 ll0axis = new Vec3(1, 1, 0);

        switch (schon) {
            case C1:
            case Ci:
                rot = Quaternion.identity();
                break;
            case C2:
            case Cs:
            case C2h:
                rot = Axis(b, 2);
                break;
            case D2:
            case C2v:
            case D2h:
                rot = Axis(a, 2).prod(Axis(c, 2));
                break;
            case C3:
            case C3i:
                rot = Axis(c, 3);
                break;
            case D3:
            case C3v:
            case D3d:
                rot = Axis(a, 2).prod(Axis(c, 3));
                break;
            case C4:
            case S4:
            case C4h:
                rot = Axis(c, 4);
                break;
            case D4:
            case C4v:
            case D2d:
            case D4h:
                rot = Axis(a, 2).prod(Axis(c, 4));
                break;
            case C6:
            case C3h:
            case C6h:
                rot = Axis(c, 6);
                break;
            case D6:
            case C6v:
            case D3h:
            case D6h:
                rot = Axis(a, 2).prod(Axis(c, 6));
                break;
            case T:
            case Th:
                rot = Axis(lllaxis, 3).prod(Axis(a, 2).prod(Axis(c, 2)));
                break;
            case O:
            case Td:
            case Oh:
                rot = Axis(lllaxis, 3).prod(Axis(ll0axis, 2).prod(Axis(c, 4)));
                break;
	         case Fi:
		         rot = Axis(c, 72);
		        break;
	        case Fib:
		        rot = Axis(a, 2).prod(Axis(c, 72));
		        break;
        }
        return rot;
    }

    public static Array1D symmetry2Euler(Symmetry cs, Symmetry ss) {
        return symmetry2Euler(cs, ss, true);
    }

    public static Array1D symmetry2Euler(Symmetry cs, Symmetry ss, boolean isSO3) {

        double maxalpha = (cs.rotangle_max_y() == Math.PI && ss.rotangle_max_y() == Math.PI)
                ? Math.PI / 2 : ss.rotangle_max_z();
        double maxbeta = Math.min(cs.rotangle_max_y(), ss.rotangle_max_y()) / 2;
        double maxgamma = cs.rotangle_max_z();
        if (isSO3) {
            if (cs.schoen.isLaue("m-3")) {
                //maxalpha = 2*pi;maxgamma=2*pi;maxbeta = pi;
            } else if (cs.schoen.isLaue("m-3m")) {
                maxbeta = Math.PI / 3;
            }
        }
        return new Array1D(maxalpha, maxbeta, maxgamma);
    }

    public double rotangle_max_y() {
        return rotangle_max_y(false);
    }

    public double rotangle_max_y(boolean antipodal) {

        double theta = Math.PI;
        if (schoen.isLaue("-1", "-3", "4/m", "6/m", "72/m")) {
            theta = 2 * Math.PI / (antipodal ? 2 : 1);
        } /*else if (schoen.isLaue("mmm", "-3m", "4/mmm", "m-3m", "6/mmm", "m-3")) {
         theta = Math.PI;
         } */ else if (schoen.isLaue("2/m")) {
            if (!antipodal) { // Luca && !get(2).axis().dot(Vec3.zvector()).isNull().any()) {
// Luca                theta = 2 * Math.PI;
            }
        }
        return theta;

    }

    public double rotangle_max_z() {
        return rotangle_max_z(true);
    }

    public double rotangle_max_z(boolean antipodal) {
        return rotangle_max_z(antipodal, false);
    }

    public double rotangle_max_z(boolean antipodal, boolean alpha) {
        double rho = Math.PI;
        if (schoen.isLaue("-1")) {
            rho *= 2.0;
        } else if (schoen.isLaue("2/m")) {
            if (!antipodal && !get(2).axis().dot(Vec3.zvector()).isNull().any()) {
                rho *= 2.0;
            }
        } else if (schoen.isLaue("mmm", "m-3")) {
            rho /= (alpha ? 2 : 1);
        } else if (schoen.isLaue("-3")) {
            rho *= 2.0 / 3.0;
        } else if (schoen.isLaue("-3m")) {

 //           System.out.println(rho);
            rho *= 2.0 / 3.0 / (antipodal ? 2 : 1);
        } else if (schoen.isLaue("4/m", "4/mmm", "m-3m")) {
            rho /= 2.0;
        } else if (schoen.isLaue("6/m", "6/mmm")) {
            rho /= 3.0;
        } else if (schoen.isLaue("72/m", "72/mmm")) {
	        rho /= 36.0;
        }
        return rho;
    }

    public Quaternion rotation_special() {
        if (schoen.isLaue("-1", "2/m")) {
            return this;
        } else if (schoen.isLaue("mmm", "-3m", "4/mmm", "6/mmm", "72/mmm")) {
            return get(0, 1);
        } else if (schoen.isLaue("-3", "4/m", "6/m", "72/m")) {
            return get(0);
        } else if (schoen.isLaue("m-3", "m-3m")) {
            return get(0, 1, 2, 3, 4, 5);
        }
        return null;
    }

    public int nfold() {
        if (schoen.isLaue("2/m", "mmm")) {
            return 2;
        } else if (schoen.isLaue("m-3", "-3", "-3m")) {
            return 3;
        } else if (schoen.isLaue("4/m", "4/mmm", "m-3m")) {
            return 4;
        } else if (schoen.isLaue("6/m", "6/mmm")) {
            return 6;
        } else if (schoen.isLaue("72/m", "72/mmm")) {
	        return 72;
        } else {
            return 1;
        }
    }

    public double getMaxAngel(Symmetry ss) {
        if (ss.nfold() == 1) {
            return Math.PI / nfold();
        }
        return Double.NEGATIVE_INFINITY;
    }

    private PointGroup schoen;
    private Vec3 axis;

    public Symmetry() {
        this(PointGroup.C1.getSchoenflies(), false);
    }

    public Symmetry(String group, Alignment... ali) {
        this(group, false, 1, ali);
    }

	public Symmetry(String group, boolean rotationOnly, Alignment... ali) {
		this(group, rotationOnly, 1, ali);
	}

	public Symmetry(String group, boolean rotationOnly, double a, Alignment... ali) {
        this(group, rotationOnly, new Array1D(a, a, a), null, ali);
    }

    public Symmetry(String group, boolean rotationOnly, double a, double b, double c, Alignment... ali) {
        this(group, rotationOnly, new Array1D(a, b, c), null, ali);
    }

    public Symmetry(String group, boolean rotationOnly, double a, double b, double c, double alpha, double beta, double gamma, Alignment... ali) {
        this(group, rotationOnly, new Array1D(a, b, c), new Array1D(alpha, beta, gamma), ali);
    }

    public Symmetry(String group, boolean rotationOnly, Array1D axlength, Array1D angles) {
        this(group, rotationOnly, axlength, angles, Alignment.X_astar, Alignment.Z_c);
    }

    public Symmetry(String group, boolean rotationOnly, Array1D axlength, Array1D angles, Alignment... align) {
        this.schoen = null;
		  if (rotationOnly)
			  this.schoen = PointGroup.getRotationBase(group);
		  else
			  this.schoen = PointGroup.get(group);

	    if (this.schoen == null) {
            this.schoen = PointGroup.get(hms2point(group));
        }

        if (this.schoen == null) {
            this.schoen = PointGroup.C1;
        }

        if (angles != null) {
        } else if (schoen.is("trigonal") || schoen.is("hexagonal")) {
            angles = new Array1D(Math.PI / 2, Math.PI / 2, 2 * Math.PI / 3);
        } else if (angles == null) {
            angles = Array1D.fill(3, Math.PI / 2);
        }

        double[] ax = axlength.toDoubleArray();
        double[] an = angles.toDoubleArray();

        Alignment[] alignment = new Alignment[3];
        for (Alignment ali : align) {
            if (ali.isX()) {
                alignment[0] = ali;
            }

            if (ali.isY()) {
                alignment[1] = ali;
            }

            if (ali.isZ()) {
                alignment[2] = ali;
            }
        }

        if (alignment[0] == null) {
            alignment[0] = Alignment.X_astar;
        }

        if (alignment[2] == null) {
            alignment[2] = Alignment.Z_c;
        }

        this.axis = calcAxis(ax[0], ax[1], ax[2], an[0], an[1], an[2], alignment);

        set(calcQuat(this.schoen, this.axis));

    }

	public Alignment[] getAlignment() {
        if (this.schoen.isLaue("-1", "2/m", "-3", "-3m", "6/m", "6/mmm", "72/m", "72/mmm")) {
            Vec3 base = Vec3.concat(basis(), basisStar()).normalize();
            double eps = 1 - 1e-5;

            String abc[] = new String[]{"a", "b", "c", "a*", "b*", "c*"};

            String a = abc[base.dot(Vec3.xvector()).abs().g(eps).find()[0]];
            String b = abc[base.dot(Vec3.yvector()).abs().g(eps).find()[0]];
            String c = abc[base.dot(Vec3.zvector()).abs().g(eps).find()[0]];
            return new Alignment[]{Alignment.get("X||" + a),
                Alignment.get("Y||" + b),
                Alignment.get("Z||" + c)};

        } else {
            return null;
        }

    }

    @Override
    public String toString() {
        return getClass().getName() + "[" + paramString() + "]";
    }

    protected String paramString() {

        String str = "group=" + schoen.toString();
        str += ",size=" + size();

        Alignment[] ali = getAlignment();
        if (ali != null) {
            str += ",alignment=";
            str += ali[0] + "," + ali[1] + "," + ali[2];
        }

        str += ",basis={" + basis().paramString() + "}";

        return str;
    }

    public boolean isLaue(String... m3) {
        return this.schoen.isLaue(m3);
    }

    public Vec3 basis() {
        return this.axis;
    }

    public Vec3 basisStar() {
        return axis.get(1, 2, 0).cross(axis.get(2, 0, 1)).divide(volume());
    }

    public Array1D basisLength() {
        return basis().norm();
    }

    public Array1D basisAngles() {
        double[] w = new double[3];
        for (int i = 0; i < 3; i++) {
            w[i] = axis.get((i + 1) % 3).angle(axis.get((i + 2) % 3)).get(0);
        }
        return new Array1D(w);
    }

    public Quaternion elements() {
        return new Quaternion(a(), b(), c(), d());
    }

    public double volume() {
        return axis.get(0).dot(axis.get(1).cross(axis.get(2))).get(0);
    }

    public PointGroup getGroup() {
        return this.schoen;
    }

}
