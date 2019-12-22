package it.unitn.ing.rista.diffr;

public class ReflectionPeak {
	public double energy = 0.0;
//	public double dspace;
	public double position = 0.0;
	public double lorentzPolarization = 1.0;
	public double absShapeFactor = 1.0;
	public double expTextureFactor = 1.0;
	public double calcTextureFactor = 1.0;
	public double expStrainFactor = 0.0;
	public double calcStrainFactor = 0.0;
	public double[] sizestrain = null;
	public double[][] instBroadFactor = null;
  public double[][] instBroadFactor_en = null;
	public double[] broadFactorTotal = null;
  public double[] broadFactorTotal_en = null;
  public double broadFactorHWHM = 0.0;
  public double broadFactorEta = 0.0;
  public double broadFactorHWHM_en = 0.0;
  public double broadFactorEta_en = 0.0;
//	public double instBroadFactorHWHM_ang = 0.0;
//	public double instBroadFactorEta_ang = 0.0;
//	public int min;
//	public int max;

	public void storeComputedOverExperimentalTextureFactors() {
		expTextureFactor = calcTextureFactor;
	}
	public void storeExperimentalOverComputedTextureFactors() {
		calcTextureFactor = expTextureFactor;
	}

}

