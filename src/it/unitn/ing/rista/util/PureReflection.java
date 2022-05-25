package it.unitn.ing.rista.util;

/**
 * Created by luca on 27/06/2017, Casalino
 */

public class PureReflection {

	public int[] h;
	public int[] k;
	public int[] l;
	public int multiplicity;
	public int mates;
	public double dspace;

	public PureReflection(int[] ih, int[] ik, int[] il, int mult, int mat, double d) {
		h = ih;
		k = ik;
		l = il;
		multiplicity = mult;
		mates = mat;
		dspace = d;
	}

}
