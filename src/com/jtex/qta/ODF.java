/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.qta;

import com.jtex.arrays.Array1C;
import com.jtex.arrays.Array1D;
import com.jtex.external.MTEX;
import com.jtex.geom.Miller;
import com.jtex.geom.Quaternion;
import com.jtex.geom.Symmetry;
import com.jtex.geom.Vec3;
import com.jtex.geom.grid.SO3Grid;
import com.jtex.qta.kernel.DeLaValleePoussin;
import com.jtex.qta.kernel.Kernel;
import com.jtex.qta.odf.FourierComponent;
import com.jtex.qta.odf.ODFComponent;
import com.jtex.qta.odf.UniformComponent;
import com.jtex.qta.odf.UnimodalComponent;
import it.unitn.ing.rista.util.MaudPreferences;
import it.unitn.ing.rista.util.Misc;

import java.io.BufferedReader;
import java.io.Reader;
import java.util.*;

/**
 *
 * @author flb
 */
public class ODF extends ODFComponent {

    protected PoleFigure pfsource = null;
    protected Symmetry cs = null;
    protected Symmetry ss = null;

    protected ArrayList<ODFComponent> odfs = new ArrayList<ODFComponent>();

    public ODF() {}

    public ODF(Symmetry cs, Symmetry ss, final PoleFigure source) {
//        this.odfs = new ArrayList<ODFComponent>();
        this.cs = cs;
        this.ss = ss;
        this.pfsource = source;

    }

    public void add(ODFComponent cmp) {
        this.odfs.add(cmp);
    }

    public void add(double portion, ODFComponent cmp) {
        cmp.setPortion(portion);
        this.odfs.add(cmp);
    }

    public void remove(ODFComponent cmp) {
        this.odfs.remove(cmp);
    }

    public void remove(int ndx) {
        this.odfs.remove(ndx);
    }

    public int componentsNumber() {
    	return odfs.size();
    }

    @Override
    public String toString() {
        return getClass().getName() + "[" + paramString() + "]";
    }

    protected String paramString() {
        Formatter formatter = new Formatter(Locale.US);

        String str = "cs=" + cs.getGroup().getLaue() + ",";
//        String str = "hw=" + formatter.format("%.1f", Math.toDegrees(p2hw())).toString();
//        str += ",data=[" + formatter.toString() + "]";
        return str;
    }

    public Symmetry getCS() {
        return cs;
    }

    public Symmetry getSS() {
        return ss;
    }

    public PoleFigure getPFsource() {
        return pfsource;
    }

	public ODF estimate(PoleFigure pf) {
		return estimate(pf, 10);
	}

	public ODF estimate(PoleFigure pf, double dghalfwidth) {
		double halfwidth = Math.toRadians(dghalfwidth);
		return estimate(pf, new ODFOptions(pf.getCS(), pf.getSS(), halfwidth, new DeLaValleePoussin(halfwidth)));
	}

	public ODF estimate(PoleFigure pf, ODFOptions options) {

		Symmetry cs = options.getCS();
		Symmetry ss = options.getSS();

		Quaternion q = options.getGrid().toQuat();

		Quaternion sgrid = q.inverse().prod(ss.inverse()).inverse().prod(cs);

		Miller h = pf.getH();
		Array1D[] ghRhoTheta = new Array1D[h.size()];
		for (int i = 0; i < h.size(); i++) {
			Vec3 gh = sgrid.rotate(h.get(i));
			ghRhoTheta[i] = gh.getRhoTheta();
		}
		Array1D gh = Array1D.concat(ghRhoTheta);
		sgrid = null;

		Array1D r = pf.getRhoTheta();
		Array1D P = pf.getData();
		Array1D lP = pf.getGridSize();
		Array1D lh = pf.getMultiplicity();
		Array1D refl = pf.getSuperposition();

		// todooooo
		Array1D w = null;

		if (options.wFilename.length() > 0) {
			BufferedReader reader = Misc.getReader(options.wFilename);
			if (reader != null) {
				try {
					Vector angular = new Vector(0, 100);
					String token = new String("");
					StringTokenizer st = null;
					String linedata = null;
					boolean endoffile = false;
					boolean found = false;

					while (!endoffile) {
						linedata = reader.readLine();
						if (linedata == null || linedata.length() == 0) {
							endoffile = true;
							break;
						}

						st = new StringTokenizer(linedata, "' ,\t\r\n");

						while (st.hasMoreTokens()) {
							token = st.nextToken();
//          	System.out.println(token);
							angular.addElement(token);
						}
					}
					w = new Array1D(angular);

				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		} else
			w = pf.getQuadratureWeights(options.getPsi());

		Array1D A = options.getPsi().A();
		double[] A2 = null;
		if (MaudPreferences.getBoolean("MTEX.usesFlorianA", true)) {
			A.set(Array1D.linspace(1, A.size() - 1, 2).toIntArray(), 0);
			A2 = A.toDoubleArray();
		} else {
			double[] A1 = A.toDoubleArray();
			A2 = new double[A1.length];
			for (int i = 0; i < A1.length; i+=2)
				A2[i] = A1[i];
		}

		Array1D RM = new Array1D(0, 0, 0, 0);

		double[] result = MTEX.pf2odf(lP.toIntArray(), lh.toIntArray(), refl.toDoubleArray(), options.getIterMax(), options.getIterMin(), options.getFlags(),
				P.toDoubleArray(), r.toDoubleArray(), gh.toDoubleArray(),
				A2, options.getC0().toDoubleArray(), w.toDoubleArray(), RM.toDoubleArray(), 0, 0);

		UnimodalComponent cmp = new UnimodalComponent(q, new Array1D(result), options.getPsi(), cs, ss);

		ODF odf = new ODF(cs, ss, pf);
		odf.add(cmp);

		if (!options.doGhostCorrection) {
			return odf;
		} else {
			double phon = 1D;
			Array1D[] alpha = new Array1D[pf.size()];

			for (int i = 0; i < pf.size(); i++) {
				PoleFigure pfi = pf.get(i);
				PoleFigure cpf = odf.calcPoleFigure(pfi.getH(), pfi.getR());
				double alph = pfi.getData().norm() / cpf.getData().norm();

				phon = Math.min(phon, pfi.getData().quantile(0.01) / alph);

				alpha[i] = Array1D.fill(pfi.getData().size(), alph);
			}
			Array1D alphas = Array1D.concat(alpha);

			if (phon > 0.1) {
				odf.remove(cmp);

				System.out.println("APPLYING GHOSTCORRECTION (phon:" + phon + ")");

				// correct intensities
				Array1D Pc = P.minus(alphas.multiplyd(phon)).max(0);

				double[] ghresult = MTEX.pf2odf(lP.toIntArray(), lh.toIntArray(), refl.toDoubleArray(), options.getIterMax(), options.getIterMin(), options.getFlags(),
						Pc.toDoubleArray(), r.toDoubleArray(), gh.toDoubleArray(),
						A2, options.getC0().toDoubleArray(), w.toDoubleArray(), RM.toDoubleArray(), 0, 0);

				odf.add(phon, new UniformComponent(cs, ss));
				odf.add(1D - phon, new UnimodalComponent(q, new Array1D(ghresult), options.getPsi(), cs, ss));
			}
		}
		return odf;
	}

	//
//
    public double textureindex() {
        return textureindex(Math.toRadians(2.5));
    }
//

    public double textureindex(double res) {

        if (this.odfs.size() == 1 & this.odfs.get(0) instanceof FourierComponent) {
            return ((FourierComponent) this.odfs.get(0)).textureindex();
        }

        SO3Grid grid = SO3Grid.equispacedSO3Grid(res, cs, ss);

        Array1D f = eval(grid.toQuat());
        double t_index = f.divided(f.sum()).multiplyd(f.size()).squared().sum() / grid.size();

        return t_index;
    }

    public double entropy() {
        return entropy(Math.toRadians(2.5));
    }

    public double entropy(double res) {

        SO3Grid grid = SO3Grid.equispacedSO3Grid(res, cs, ss);

        Array1D f = eval(grid.toQuat());
        double[] a = f.toDoubleArray();
        double e = 0;
        for (double v : a) {
            if (v != 0D) {
                e += v * Math.log(Math.abs(v));
            }
        }

        e /= -grid.size();

        return e;
    }

    public PoleFigure calcPoleFigure(Miller h, Vec3 r) {
        PoleFigure pf = new PoleFigure();
        pf.setCS(cs);
        pf.setSS(ss);
        for (int i = 0; i < h.size(); i++) {
            pf.add(new PoleFigure(h.get(i), ss, r, pdf(h.get(i), r), new Array1D(1D)));
        }
        return pf;
    }

    public PoleFigure calcErrorPF(PoleFigure source) {
        return calcErrorPF(source, null);
    }

    public PoleFigure calcErrorPF(PoleFigure source, String etype) {

        PoleFigure pfcalc = new PoleFigure();
        pfcalc.setCS(cs);
        pfcalc.setSS(ss);

        for (int i = 0; i < source.size(); i++) {

            PoleFigure pf1 = source.get(i);
            PoleFigure pf2 = calcPoleFigure(pf1.getH(), pf1.getR());

            System.out.println(pf2.size());
            if (pf2.size() > 1) {
                Array1D coef = pf1.getSuperposition();
                Array1D data = Array1D.zeros(pf1.getData().size());
                for (int j = 0; j < pf2.size(); j++) {
                    data.plusd(pf2.get(j).getData().multiply(coef.get(j)));
                }
                pf2 = new PoleFigure(pf1.getH(), ss, pf1.getR(), data, pf1.getSuperposition());
            }

            double alpha = pf1.calcNormalization(pf2).get(0);

            Array1D d1 = pf1.getData();
            Array1D d2 = pf2.getData().multiplyd(alpha);
            Array1D d;
            if (etype == null || etype.length() == 0) {
                double epsilon = 0.5;
                d = d1.minus(d2).abs().divide(d1.plus(epsilon * alpha).max(d2.plus(epsilon * alpha)));
            } else if ("l1".equals(etype.toLowerCase())) {
                d = d1.minus(d2).abs();
            } else if ("l2".equals(etype.toLowerCase())) {
                d = d1.minus(d2).squared();
            } else /*if ("rp".equals(etype.toLowerCase())) {*/ {
                d = d1.minus(d2).abs().divide(d2);
            }

            System.out.println(d1);
            System.out.println(d2);
            pf2.setData(d);
            pfcalc.add(pf2);
        }
        return pfcalc;

    }

    @Override
    public Array1D pdf(Miller h, Vec3 r) {

        Array1D P = Array1D.zeros(h.size() * r.size());
        for (ODFComponent cmp : odfs) {
            P = P.plus(cmp.pdf(h, r).multiplyd(cmp.getPortion()));
        }
        return P;
    }

    @Override
    public Array1D eval(Quaternion qr) {
        Array1D f = Array1D.zeros(qr.size());
        for (ODFComponent cmp : odfs) {
            f.plusd(cmp.eval(qr).multiplyd(cmp.getPortion()));
        }
        return f;
    }

    @Override
    public Array1C calcFourier(int L) {
        Array1C C = Array1C.zeros(FourierComponent.deg2dim(L));
        for (ODFComponent odf : odfs) {
            C = C.plus(odf.calcFourier(L).multiply(odf.getPortion()));
        }
        return C;
    }

    public Array1D powerSpectrum() {
        FourierComponent fourierComponent = new FourierComponent(calcFourier(this.bandwidth() + 1), this.cs, this.ss);
        return fourierComponent.powerSpectrum();
    }

    public ODF fourierODF(int L) {
        ODF odf = new ODF(cs, ss, pfsource);
        odf.add(new FourierComponent(calcFourier(L), this.cs, this.ss));
        return odf;
    }

    @Override
    public int bandwidth() {
        int b = 0;
        for (ODFComponent odf : odfs) {
            b = Math.max(b, odf.bandwidth());
        }
        return b;
    }

    @Override
    public double volume(Quaternion q, double radius) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
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
        double p = 0D;
        for (ODFComponent odf : odfs) {
            p += odf.getPortion();
        }
        return p;
    }

    @Override
    public void setPortion(double p) {
        p /= this.getPortion();

        for (ODFComponent odf : odfs) {
            odf.setPortion(p * odf.getPortion());
        }

    }

/*Luca    public static void main(String args[]) {
        Quaternion q = Quaternion.rotZXZ(1, 2, 3);

        Symmetry cs = new Symmetry("m-3m", 4, 4, 4);

        Miller h = new Miller(1, 1, 1, cs);
        h.print();
        System.out.println(h.toVec3());
        System.out.println(q.prod(cs).rotate(h));

        UnimodalComponent odf = new UnimodalComponent(Quaternion.rotY(Math.PI / 5), new Array1D(1D), new DeLaValleePoussin(Math.toRadians(5)), cs, new Symmetry());
        FourierComponent fodf = new FourierComponent(odf.calcFourier(32), cs, new Symmetry());

        System.out.println(fodf.bandwidth());
        fodf.powerSpectrum().print();

//        System.out.println(odf.textureindex());
//        System.out.println(fodf.textureindex());
//        System.out.println("J-index " + odf.textureindex());
//        System.out.println("J-index " + fodf.textureindex());
//        System.out.println("entropy " + odf.entropy());
//        
//        System.out.println("vol(h,r)" + odf.fibreVolume(h, new Vec3(0,0,1), Math.toRadians(10)));
    }*/

}

