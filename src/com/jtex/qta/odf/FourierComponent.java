/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.qta.odf;

import com.jtex.arrays.Array1C;
import com.jtex.arrays.Array1D;
import com.jtex.external.MTEX;
import com.jtex.geom.Miller;
import com.jtex.geom.Symmetry;
import com.jtex.geom.Quaternion;
import com.jtex.geom.Vec3;
import com.jtex.poly.SphericalHarmonics;
import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.util.Constants;

/**
 *
 * @author flb
 */
public class FourierComponent extends ODFComponent {

    Array1C C;
//    Symmetry cs, ss;
    double weight;

    public static int deg2dim(int l) {
        return l * (2 * l - 1) * (2 * l + 1) / 3;
    }

    public static int dim2deg(int dim) {
        int l = 0;
        while (deg2dim(l + 1) < dim) {
            l++;
        }
        return l;
    }

    public FourierComponent(Array1C coef, Symmetry cs, Symmetry ss) {
        C = coef;
//        this.cs = cs;
//        this.ss = ss;
        weight = 1;

//        setFourierCoefficients(C);
    }

    @Override
    public Array1D pdf(Miller h, Vec3 r) {

        Vec3 in, out;
        if (h.size() == 1) {
            in = h;
            out = r;
        } else {
            in = r;
            out = h;
        }

        int L = dim2deg(C.size());

        double theta = in.getTheta().get(0);
        double rho = in.getRho().get(0);

//        Array1C Y = new Array1C(JMTEX.sphericalY(L, in.getRhoTheta().toDoubleArray()));
        int lpdf = 0;

        Array1C P_hat = Array1C.zeros((L + 1) * (L + 1));
        for (int l = 0; l <= L; l++) {
            Array1C Y = SphericalHarmonics.Y(l, theta, rho);

            Array1C c = C.get(Array1D.fill(deg2dim(l), deg2dim(l + 1) - 1, 1).toIntArray());
            c = c.divide(Math.sqrt(2 * l + 1));  // normalization
            int[] ndx = Array1D.fill(lpdf, (lpdf += 2 * l + 1) - 1, 1).toIntArray();
            if (h.size() == 1) {
                P_hat.set(ndx, c.matrixMultiply(Y.multiply(2 * Math.sqrt(Math.PI)), 2 * l + 1, 1));
            } else {
                P_hat.set(ndx, c.matrixMultiplyTransposed(Y.multiply(2 * Math.sqrt(Math.PI)), 2 * l + 1, 1));
            }
        }

        P_hat = P_hat.conjugate();

        return new Array1D(MTEX.pdf2pf(out.getRhoTheta().toDoubleArray(), P_hat.toDoubleArray()));

    }

    @Override
    public Array1D eval(Quaternion g) {
        return new Array1D(MTEX.fc2odf(FourierComponent.dim2deg(C.size()), C.toDoubleArray(), g.euler("nfft").getXYZ().toDoubleArray()));
    }

    @Override
    public Array1C calcFourier(int L) {
	    if (FourierComponent.deg2dim(L) == C.size()) {
		    return C;
	    }

//        System.out.println("uhahadfadf" + FourierComponent.deg2dim(L) + "  " + C.size());
	    Array1C c_hat = Array1C.zeros(FourierComponent.deg2dim(L));

	    if (c_hat.size() > C.size()) {
		    c_hat = C.plus(c_hat);
	    } else {
		    c_hat = c_hat.plus(C);
	    }
	    C = c_hat; // Luca
//	    setFourierCoefficients(C);

	    return c_hat;
    }

    @Override
    public double volume(Quaternion q, double radius) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    public Array1D powerSpectrum() {
        int L = bandwidth();
        double power[] = new double[L + 1];
        for (int l = 0; l <= L; l++) {
            Array1C c = C.get(Array1D.fill(deg2dim(l), deg2dim(l + 1) - 1, 1).toIntArray());
            c = c.divide(Math.sqrt(2 * l + 1));  // normalization

            power[l] = Math.sqrt(c.multiply(c.conjugate()).real().sum());
        }

        return new Array1D(power);
    }

//    @Override
    public double textureindex() {

        int L = dim2deg(C.size());

        Array1C coef = Array1C.zeros(C.size());
        for (int l = 0; l <= L; l++) {
            int[] ndx = Array1D.fill(deg2dim(l), deg2dim(l + 1) - 1, 1).toIntArray();
            coef.set(ndx, C.get(ndx).divide(Math.sqrt(2 * l + 1)));
        }

        double t = coef.multiply(coef.conjugate()).real().sum();

        return t;

    }

    @Override
    public int bandwidth() {
        return dim2deg(C.size());
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
        return weight;
    }

    @Override
    public void setPortion(double p) {
	    setWeight(p);
    }

    // Maud interface by Luca Lutterotti

	public static String[] diclistc = {"_mtex_fourier_component_coefficients", "_mtex_fourier_component_weight"};
	public static String[] diclistcrm = {"Fourier component coefficients", "Fourier component weight"};

	public static String[] classlistcs = {};
	public static String[] classlistc = {};

	public static String modelID = "MTex Fourier Component";

	public FourierComponent(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		identifier = modelID;
		IDlabel = modelID;
		description = modelID;
	}

	public FourierComponent(XRDcat aobj) {
		this(aobj, modelID);
	}

	public FourierComponent() {
		identifier = modelID;
		IDlabel = modelID;
		description = modelID;
	}

	public void initConstant() {
		Nstring = 6;
		Nstringloop = 0;
		Nparameter = 0;
		Nparameterloop = 0;
		Nsubordinate = 0;
		Nsubordinateloop = 0;
	}

	public void initDictionary() {
		System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
		System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
		System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
		System.arraycopy(classlistcs, 0, classlists, 0, totsubordinate - totparameterloop);
	}

	public void initParameters() {
		super.initParameters();
		setWeight(1.0);
	}

	public String getFourierCoefficients() {
		return stringField[0];
	}

	public void setFourierCoefficients(Array1C value) {
		stringField[1] = value.toSaveString();
	}

	public String getWeight() {
		return stringField[1];
	}

	public void setWeight(double value) {
		weight = value;
		stringField[1] = Double.toString(value);
	}

	public void refreshForNotificationDown(XRDcat source, int reason) {
		if (!getFilePar().isComputingDerivate() || (source == this ||
				(reason == Constants.SAMPLE_ORIENTATION_CHANGED || (source == getParent() &&
						(reason == Constants.STRAIN_CHANGED || reason == Constants.CELL_CHANGED))))) {
			refreshComputation = true;
			//     System.out.println("Reason " + reason + " Source " + source.toXRDcatString());
		}
	}

	public void updateStringtoDoubleBuffering(boolean firstLoading) {
		super.updateStringtoDoubleBuffering(false);
		weight = Double.parseDouble(getWeight());
		C = Array1C.parse(getFourierCoefficients());
	}


}
