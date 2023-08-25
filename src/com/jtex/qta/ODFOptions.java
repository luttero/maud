package com.jtex.qta;

import com.jtex.arrays.Array1D;
import com.jtex.geom.Symmetry;
import com.jtex.geom.grid.SO3Grid;
import com.jtex.qta.kernel.DeLaValleePoussin;
import com.jtex.qta.kernel.Kernel;

public class ODFOptions {

	Symmetry cs;
	Symmetry ss;
	double resolution;
	Kernel psi;
	int iterMax;
	int iterMin;
	int flags;
	boolean doGhostCorrection = true;
	SO3Grid grid;
	Array1D c0;

	public String wFilename = "";

	public ODFOptions(PoleFigure pf) {
		this(pf, Math.toRadians(7.5));
	}

	public ODFOptions(PoleFigure pf, double halfwidth) {
		this(pf, halfwidth, new DeLaValleePoussin(halfwidth));
	}

	public ODFOptions(PoleFigure pf, double resolution, Kernel psi) {
		this(pf.getCS(), pf.getSS(), resolution, psi);
	}

	public ODFOptions(Symmetry cs) {
		this(cs, Math.toRadians(7.5));
	}

	public ODFOptions(Symmetry cs, double halfwidth) {
		this(cs, halfwidth, halfwidth);
	}

	public ODFOptions(Symmetry cs, double halfwidth, double resolution) {
		this(cs, new Symmetry(), resolution, new DeLaValleePoussin(halfwidth));
	}

	public ODFOptions(Symmetry cs, Symmetry ss, double resolution, Kernel psi) {
		this.cs = cs;
		this.resolution = resolution;
		this.psi = psi;
		this.ss = ss;
		this.flags = 1;
		this.iterMax = 15;
		this.iterMin = 10;

		this.grid = SO3Grid.equispacedSO3Grid(resolution, cs, ss);
		this.c0 = Array1D.fill(grid.size(), 1.0 / grid.size());

	}

	public SO3Grid getGrid() {
		return grid;
	}

	public void setGrid(SO3Grid grid) {
		this.grid = grid;
	}

	public Array1D getC0() {
		return c0;
	}

	public Symmetry getCS() {
		return cs;
	}

	public void setC0(Array1D c0) {
		this.c0 = c0;
	}

	public void setIterMax(int iterMax) {
		this.iterMax = iterMax;
	}

	public void setIterMin(int iterMin) {
		this.iterMin = iterMin;
	}

	public void setGhostCorrection(boolean doGhostCorrection) {
		this.doGhostCorrection = doGhostCorrection;
	}

	public boolean getGhostCorrection() {
		return doGhostCorrection;
	}

	public double getHalfwidth() {
		return psi.p2hw();
	}

	public Kernel getPsi() {
		return psi;
	}

	public double getResolution() {
		return resolution;
	}

	public Symmetry getSS() {
		return ss;
	}

	public int getIterMax() {
		return iterMax;
	}

	public int getIterMin() {
		return iterMin;
	}

	public int getFlags() {
		return flags;
	}

	public void setwFilename(String wFilename) {
		this.wFilename = wFilename;
	}

	@Override
	public String toString() {
		return getClass().getName() + "[" + paramString() + "]";
	}

	protected String paramString() {
		String str = "resolution=" + Math.toDegrees(resolution) + "°";
		str += ",kernel=" + psi.toString();
		str += ",iterMax=" + iterMax;
		str += ",iterMin=" + iterMin;
		str += ",flags=" + flags;
		str += ",grid=" + grid;
		return str;
	}

}

