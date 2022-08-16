package it.unitn.ing.rista.diffr;

import com.github.tschoonj.xraylib.Xraylib;
import it.unitn.ing.rista.util.Constants;
import java.util.Vector;

public class AtomProperties {

	protected static Vector<AtomProperties> atom_table = new Vector(Constants.ZMAX_LIMITED + 1, 1);

	public static int insertAtomScatterer(int Z_number) {
		int index = checkAlreadyPresent(Z_number);
		if (index < 0) {
//			System.out.println("Add new atom properties for: " + Z_number);
			AtomProperties newAtomP = new AtomProperties(Z_number);
			atom_table.add(newAtomP);
			return atom_table.size() - 1;
		}
//		System.out.println("Already present for: " + Z_number);
		return index;
	}

	public static int checkAlreadyPresent(int Z_number) {
		for (int i = 0; i < atom_table.size(); i++)
			if (getAtomPropertiesFor(i).Z == Z_number)
				return i;
		return -1;
	}

	public static AtomProperties getAtomPropertiesFor(int index) {
		return atom_table.get(index);
	}

	public static int energyKeVToInt(double energyInKeV) {
		int index = (int) ((energyInKeV  - Constants.BASE_ENERGY_IN_KEV) * Constants.MULTIPLE_ENERGY_TO_INT + 0.499);
		if (index >= Constants.energiesMaxNumber)
			return Constants.energiesMaxNumber - 1;
		if (index > 0)
			return index;
		return 0;
	}

	public int Z = -1;
	public Vector<Double> fprime_vec = null;
	public Vector<Double> fsecond_vec = null;
	public Vector<Double> mass_abs_vec = null;
	public Vector<Double> compton_vec = null;

	public AtomProperties(int Z_number) {
		Z = Z_number;
	}

	public void reset() {
		fprime_vec = null;
		fsecond_vec = null;
		mass_abs_vec = null;
		compton_vec = null;
	}

	protected void initFPrime() {
		fprime_vec = new Vector(Constants.energiesMaxNumber, 1);
		double energyInKeV = Constants.BASE_ENERGY_IN_KEV;
		for (int i = 0; i < Constants.energiesMaxNumber; i++) {
			fprime_vec.add(i, Xraylib.Fi(Z, energyInKeV));
			energyInKeV += Constants.INV_MULTIPLE_ENERGY_TO_INT;
		}
	}

	protected void initFSecond() {
		fsecond_vec = new Vector(Constants.energiesMaxNumber, 1);
		double energyInKeV = Constants.BASE_ENERGY_IN_KEV;
		for (int i = 0; i < Constants.energiesMaxNumber; i++) {
			fsecond_vec.add(i, -Xraylib.Fii(Z, energyInKeV));
			energyInKeV += Constants.INV_MULTIPLE_ENERGY_TO_INT;
		}
	}

	protected void initMassAbsorption() {
		mass_abs_vec = new Vector(Constants.energiesMaxNumber, 1);
		double energyInKeV = Constants.BASE_ENERGY_IN_KEV;
		for (int i = 0; i < Constants.energiesMaxNumber; i++) {
			mass_abs_vec.add(i, Xraylib.CS_Total(Z, energyInKeV));
			energyInKeV += Constants.INV_MULTIPLE_ENERGY_TO_INT;
		}
	}

	protected void initCompton() {
		compton_vec = new Vector(Constants.energiesMaxNumber, 1);
		double energyInKeV = Constants.BASE_ENERGY_IN_KEV;
		for (int i = 0; i < Constants.energiesMaxNumber; i++) {
			compton_vec.add(i, Xraylib.CS_Compt(Z, energyInKeV));
			energyInKeV += Constants.INV_MULTIPLE_ENERGY_TO_INT;
		}
	}

	public double getFPrime(double energyInKeV) {
		if (fprime_vec == null)
			initFPrime();
		return fprime_vec.get(energyKeVToInt(energyInKeV));
	}

	public double getFSecond(double energyInKeV) {
		if (fsecond_vec == null)
			initFSecond();
		return fsecond_vec.get(energyKeVToInt(energyInKeV));
	}

	public double getMassAbsorption(double energyInKeV) {
		if (mass_abs_vec == null)
			initMassAbsorption();
		return mass_abs_vec.get(energyKeVToInt(energyInKeV));
	}

	public double getCompton(double energyInKeV) {
		if (compton_vec == null)
			initCompton();
		return compton_vec.get(energyKeVToInt(energyInKeV));
	}

}
