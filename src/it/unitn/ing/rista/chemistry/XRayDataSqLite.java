package it.unitn.ing.rista.chemistry;

import it.unitn.ing.rista.io.StringNumber;
import it.unitn.ing.rista.util.*;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.*;
import com.github.tschoonj.xraylib.*;
//import org.apache.commons.math3.complex.Complex;

public class XRayDataSqLite {

	public static int atomsNumber = 94;

	public static String[] shellIDs = {"K", "L1", "L2", "L3", "M1", "M2", "M3", "M4", "M5",
			"N1", "N2", "N3", "N4", "N5", "N6", "N7", "O1", "O2", "O3", "O4", "O5", "O6", "O7",
			"P1", "P2", "P3", "P4", "P5", "Q1", "Q2", "Q3"};

	public static final int K = 0, L1 = 1, L2 = 2, L3 = 3, M1 = 4, M2 = 5, M3 = 6, M4 = 7, M5 = 8,
			N1 = 9, N2 = 10, N3 = 11, N4 = 12, N5 = 13, N6 = 14, N7 = 15, O1 = 16, O2 = 17, O3 = 18, O4 = 19, O5 = 20, O6 = 21,
			O7 = 22, P1 = 23, P2 = 24, P3 = 25, P4 = 26, P5 = 27, Q1 = 28, Q2 = 29, Q3 = 30;

	public static int[] mainShellNumber = {0, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6};
	public static int[] mainShellIndex = {K, L1, L1, L1, M1, M1, M1, M1, M1, N1, N1, N1, N1, N1, N1, N1,
			O1, O1, O1, O1, O1, O1, O1, P1, P1, P1, P1, P1, Q1, Q1, Q1};

	public static int[] numberInnerShells = {1, 3, 5, 7, 7, 5, 3};

	public static void main(String[] args) throws ClassNotFoundException
	{
		// load the sqlite-JDBC driver using the current class loader
		Class.forName("org.sqlite.JDBC");

		Connection connection = null;
		try
		{
			// create a database connection
//			connection = DriverManager.getConnection("jdbc:sqlite:" + Constants.filesfolder + Constants.pathSeparator + "xraydata.db");
//			connection = DriverManager.getConnection("jdbc:sqlite:" + "/Volumes/xspider/Users/luca/Applications/Maud/files/xraydata.db");
			connection = DriverManager.getConnection("jdbc:sqlite:" + "files/xraydata.db");
			Statement statement = connection.createStatement();
			statement.setQueryTimeout(30);  // set timeout to 30 sec.

/*			for (int i = 1; i <= atomsNumber; i++) {
				String elementNumberAsString = Integer.toString(i);
				ResultSet rs = statement.executeQuery("SELECT * FROM xraydata_ebel_elastic WHERE Z_id = " + elementNumberAsString);
				while(rs.next())
				{
					// read the result set
					System.out.println("z_id = " + rs.getString("z_id"));
					System.out.print("Coeffs = ");
					for (int j = 2; j < 8; j++) {
						System.out.print(rs.getDouble(j) + " ");
					}
					System.out.println();
				}
			}*/
			for (int i = 1; i <= atomsNumber; i++) {
				String elementNumberAsString = Integer.toString(i);
				String queryString = "SELECT * FROM xraydata_ebel_tau_shell WHERE Z_id = " + elementNumberAsString;
				ResultSet rs = statement.executeQuery(queryString);
				while (rs.next()) {
					String shellID = rs.getString("shell_id");
					double[] coeffs = new double[6];
					System.out.print(i + " " + shellID + " ");
					for (int k = 1; k < 7; k++) {
						coeffs[k-1] = rs.getDouble("coeff" + Integer.toString(k));
						System.out.print(coeffs[k-1] + " ");
					}
					System.out.println();
				}
			}
		}
		catch(SQLException e)
		{
			// if the error message is "out of memory",
			// it probably means no database file is found
			e.printStackTrace();
			System.err.println(e.getMessage());
		}
		finally
		{
			try
			{
				if(connection != null)
					connection.close();
			}
			catch(SQLException e)
			{
				// connection close failed.
				System.err.println(e);
			}
		}
		loadEbelAndShellTables(false);
	}

	// Ebel table

	private static String[] coefficientIDs = {"coeff1", "coeff2", "coeff3", "coeff4", "coeff5", "coeff6"};
	private static String[] a_rho = {"A", "rho"};
	private static String[] energyLevels = {"energy_eV", "jump", "level_width_eV"};
	private static double[] trasformEL = {0.001, 1.0, 0.001};
	private static String[] yield = {"fluo_yield"};
	private static String[] costerKronigLabels = {"f1", "f12", "f13", /*"fp13", */"f23"};
	private static String[] costerKronigLabels_m = {"fM12", "fM13", "fM14", "fM15", "fM23", "fM24", "fM25", "fM34", "fM35", "fM45"};
   private static int costerKronigMax_k = 1;
	private static int costerKronigMax_l = 2;
	private static int costerKronigMax_m = 4;
	private static String[] transitionLabels = {"transition_id", "energy_eV", "probability"};
	private static String[] augerLabels = {"outer_shell1_id", "outer_shell2_id", "probability", "rate"};
	private static double[] trasformL = {1.0, 0.001, 1.0};
	private static String[] henkef1f2Labels = {"energy_eV", "f1", "f2"};
	private static double[] trasformH = {0.001, 1.0, 1.0};
	public static Vector<double[]> ebelElastic = null;
	public static Vector<double[]> ebelInelastic = null;
	public static Vector<double[]> aRho = null;
	public static Vector<Vector<double[]>> ebelTauShell = null;
	public static Vector<Vector<double[]>> ebelTauRange = null;
	public static Vector<AtomShellData> shellData = null;
	public static Vector<Vector<double[]>> yieldData = null;
	public static Hashtable<Integer, Hashtable<Integer, double[][]>> costerKronigData = new Hashtable<>();
	public static Vector<Vector<int[]>> transitionShellIDs = null;
	public static Vector<Vector<String>> transitionShellIDlabels = null;
	public static Vector<Vector<double[]>> transitionEnergies = null;
	public static Vector<double[][]> henkeEnergyf1f2 = null;
	public static double linesMinimumEnergy = 0.1;
   public static Vector<Vector<Vector<double[]>>> auger_transition = null;
	public static boolean ebelAndShellLoaded = false;
	public static boolean useCascadeForSensitivity = true;
	public static String xray_database = null;


	public static boolean loadEbelAndShellTables(boolean forceReload) {

		if (ebelElastic != null && !forceReload)
			return true;

		linesMinimumEnergy = MaudPreferences.getDouble("fluorescenceLines.minimum_keV", linesMinimumEnergy);
		useCascadeForSensitivity = MaudPreferences.getBoolean("fluorescenceLines.use_cascade_for_sensitivity",
				useCascadeForSensitivity);

		// load the sqlite-JDBC driver using the current class loader
		try {
			Class.forName("org.sqlite.JDBC");
		} catch (ClassNotFoundException e) {
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}

		Connection connection = null;
		try {
			// create a database connection
			xray_database = MaudPreferences.getPref("fluorescence.database", Constants.documentsDirectory + "xraydata.db");
			connection = DriverManager.getConnection("jdbc:sqlite:" + xray_database);
//			connection = DriverManager.getConnection("jdbc:sqlite:" + xray_database);
			Statement statement = connection.createStatement();
			statement.setQueryTimeout(30);  // set timeout to 30 sec.

			ebelElastic = new Vector<>(atomsNumber, 1);
			ResultSet rs = statement.executeQuery("SELECT * FROM xraydata_ebel_elastic");
			while(rs.next()) {
				double[] coeffs = new double[coefficientIDs.length];
				for (int k = 0; k < coefficientIDs.length; k++)
					coeffs[k] = rs.getDouble(coefficientIDs[k]);
				ebelElastic.add(coeffs);
			}

			ebelInelastic = new Vector<>(atomsNumber, 1);
			rs = statement.executeQuery("SELECT * FROM xraydata_ebel_inelastic");
			while(rs.next()) {
				double[] coeffs = new double[coefficientIDs.length];
				for (int k = 0; k < coefficientIDs.length; k++)
					coeffs[k] = rs.getDouble(coefficientIDs[k]);
				ebelInelastic.add(coeffs);
			}


			Hashtable<Integer, double[][]> costerKronigData_k = new Hashtable<>(atomsNumber, 1);
			for (int j = 0; j < atomsNumber; j++) {
            double[][] coeffs = new double[costerKronigMax_k][costerKronigMax_k];
            costerKronigData_k.put(j, coeffs);
			}
         costerKronigData.put(K, costerKronigData_k);

			Hashtable<Integer, double[][]> costerKronigData_l = new Hashtable<>(atomsNumber, 1);
			rs = statement.executeQuery("SELECT * FROM xraydata_coster_kronig_l");
			while(rs.next()) {
				int z_id = rs.getInt("Z_id");
				double[][] coeffs = new double[costerKronigMax_l][costerKronigMax_l];
				for (int k = 0; k < costerKronigMax_l; k++)
               for (int l = 0; l < k + 1; l++) {
                  StringBuffer tmp = new StringBuffer("f");
                  tmp.append(String.valueOf(l + 1));
                  tmp.append(String.valueOf(k + 2));
                  coeffs[l][k] = rs.getDouble(tmp.toString());
               }
				costerKronigData_l.put(z_id - 1, coeffs);
			}
         costerKronigData.put(L1, costerKronigData_l);

			Hashtable<Integer, double[][]> costerKronigData_m = new Hashtable<>(atomsNumber, 1);
			rs = statement.executeQuery("SELECT * FROM xraydata_coster_kronig_m");
//         System.out.println("Reading xraydata_coster_kronig_m:");
			while(rs.next()) {
				int z_id = rs.getInt("Z_id");
//            System.out.println("AtomSite: " + z_id);
				double[][] coeffs = new double[costerKronigMax_m][costerKronigMax_m];
//            System.out.print("Coeff: ");
				for (int k = 0; k < costerKronigMax_m; k++)
               for (int l = 0; l < k + 1; l++)  {
                  StringBuffer tmp = new StringBuffer("fM");
                  tmp.append(String.valueOf(l + 1));
                  tmp.append(String.valueOf(k + 2));
                  coeffs[l][k] = rs.getDouble(tmp.toString());
//                  System.out.print(" " + tmp.toString() + "=" + coeffs[l][k] + " ");
               }
				costerKronigData_m.put(z_id - 1, coeffs);
//            System.out.println();
			}
         costerKronigData.put(M1, costerKronigData_m);

			aRho = new Vector<>(atomsNumber, 1);
			ebelTauShell = new Vector<>(atomsNumber, 1);
			ebelTauRange = new Vector<>(atomsNumber, 1);
			shellData = new Vector<>(atomsNumber, 1);
			yieldData = new Vector<>(atomsNumber, 1);
			transitionShellIDs = new Vector<>(atomsNumber, 1);
			transitionEnergies = new Vector<>(atomsNumber, 1);
			auger_transition = new Vector<>(atomsNumber, 1);
			transitionShellIDlabels = new Vector<>(atomsNumber, 1);
			henkeEnergyf1f2 = new Vector<>(atomsNumber, 1);
			for (int i = 0; i < atomsNumber; i++) {
				aRho.add(new double[0]);
				ebelTauShell.add(null);
				ebelTauRange.add(null);
				shellData.add(null);
				yieldData.add(null);
				transitionEnergies.add(null);
				auger_transition.add(null);
				transitionShellIDs.add(null);
				transitionShellIDlabels.add(null);
				henkeEnergyf1f2.add(null);
			}

			Vector<double[]> oneElement = new Vector<>();
			Vector<String> idElement = new Vector<>();
			double[] coeffs = new double[yield.length];
			oneElement.add(coeffs);
			yieldData.setElementAt(oneElement, 0); // we set 0 for the H and He that are not in the database
			yieldData.setElementAt(oneElement, 1);

			oneElement = new Vector<>();
			for (int i = 0; i < 4; i++) // no transitions for the first 4 elements
				transitionEnergies.setElementAt(oneElement, i);
			Vector<int[]> secondElement = new Vector<>();
			for (int i = 0; i < 4; i++) // no transitions for the first 4 elements
				transitionShellIDs.setElementAt(secondElement, i);
			for (int i = 0; i < 4; i++) // no transitions for the first 4 elements
				transitionShellIDlabels.setElementAt(idElement, i);

			Vector<Vector<double[]>> doubleElement = new Vector<>();
			for (int i = 0; i < 5; i++) // no transitions for the first 5 elements
				auger_transition.setElementAt(doubleElement, i);


			rs = statement.executeQuery("SELECT * FROM xraydata_ebel_a_rho");
			while(rs.next()) {
				int z_id = rs.getInt("z_id");
				coeffs = new double[a_rho.length];
				for (int k = 0; k < a_rho.length; k++)
					coeffs[k] = rs.getDouble(a_rho[k]);
				aRho.setElementAt(coeffs, z_id - 1);
//				System.out.println("Setting rho for element: " + z_id);
			}

			for (int z_id = 0; z_id < aRho.size(); z_id++) {

				oneElement = new Vector<>();
				String elementNumberAsString = Integer.toString(z_id + 1);
				ResultSet rsr = statement.executeQuery("SELECT * FROM xraydata_ebel_tau_shell WHERE Z_id = " +
						elementNumberAsString);
				while (rsr.next()) {
//					String shellID = rsr.getString("shell_id");
					coeffs = new double[coefficientIDs.length];
					for (int k = 0; k < coefficientIDs.length; k++)
						coeffs[k] = rsr.getDouble(coefficientIDs[k]);
					oneElement.add(coeffs);
				}
				ebelTauShell.setElementAt(oneElement, z_id);

				oneElement = new Vector<>();
				rsr = statement.executeQuery("SELECT * FROM xraydata_ebel_tau_range WHERE Z_id = " +
						elementNumberAsString);
				while (rsr.next()) {
//					String shellID = rsr.getString("gen_shell_id");
					coeffs = new double[coefficientIDs.length];
					for (int k = 0; k < coefficientIDs.length; k++)
						coeffs[k] = rsr.getDouble(coefficientIDs[k]);
					oneElement.add(coeffs);
				}
				ebelTauRange.setElementAt(oneElement, z_id);

				AtomShellData dataElement = new AtomShellData(z_id);
				rsr = statement.executeQuery("SELECT * FROM xraydata_shell_data WHERE Z_id = " +
						elementNumberAsString);
				while (rsr.next()) {
					String shellID = rsr.getString("shell_id");
					ShellDataAndID newData = new ShellDataAndID(shellID);
					coeffs = new double[energyLevels.length];
					for (int k = 0; k < energyLevels.length; k++)
						coeffs[k] = rsr.getDouble(energyLevels[k]) * trasformEL[k];  // in KeV
					newData.addData(coeffs);
					dataElement.addData(newData);
				}
//				System.out.println("Setting shellData for element: " + z_id + ", number of shells: " + oneElement.size());
				shellData.setElementAt(dataElement, z_id);
/*				System.out.println(z_id + " ");
				for (int i = 0; i < oneElement.size(); i++)
					System.out.print(oneElement.elementAt(i)[0] + " ");
				System.out.println();*/

				oneElement = new Vector<>();
				rsr = statement.executeQuery("SELECT * FROM xraydata_fluo_yield WHERE Z_id = " +
						elementNumberAsString);
				while (rsr.next()) {
//					String shellID = rsr.getString("shell_id");
					coeffs = new double[yield.length];
					for (int k = 0; k < yield.length; k++)
						coeffs[k] = rsr.getDouble(yield[k]);
					oneElement.add(coeffs);
				}
				yieldData.setElementAt(oneElement, z_id);

				oneElement = new Vector<>();
				secondElement = new Vector<>();
				idElement = new Vector<>();
				rsr = statement.executeQuery("SELECT * FROM xraydata_transition_data WHERE Z_id = " +
						elementNumberAsString);
				while (rsr.next()) {
//					String shellID = rsr.getString("shell_id");
					String transitionID = rsr.getString(transitionLabels[0]);
					boolean intrashell = isIntraShell(transitionID);
					if (!intrashell) {
						coeffs = new double[transitionLabels.length - 1];
						for (int k = 1; k < transitionLabels.length; k++)
							coeffs[k - 1] = rsr.getDouble(transitionLabels[k]) * trasformL[k];
						oneElement.add(coeffs);
						int[] shellNumbers = new int[2];
						idElement.add(transitionID);
						if (transitionID.startsWith("K")) {
							shellNumbers[0] = K;
							shellNumbers[1] = getShellNumberFromLabel(transitionID.substring(1));
						} else {
							shellNumbers[0] = getShellNumberFromLabel(transitionID.substring(0, 2));
							shellNumbers[1] = getShellNumberFromLabel(transitionID.substring(2, 4));
						}
						secondElement.add(shellNumbers);
					}
				}
				transitionEnergies.setElementAt(oneElement, z_id);
				transitionShellIDs.setElementAt(secondElement, z_id);
				transitionShellIDlabels.setElementAt(idElement, z_id);

				oneElement = new Vector<>();
				rsr = statement.executeQuery("SELECT * FROM xraydata_henke_f1f2 WHERE Z_id = " +
						elementNumberAsString);
				while (rsr.next()) {
					coeffs = new double[henkef1f2Labels.length];
					for (int k = 0; k < henkef1f2Labels.length; k++)
						coeffs[k] = rsr.getDouble(henkef1f2Labels[k]) * trasformH[k];
					oneElement.add(coeffs);
				}
				double[][] anElement = new double[henkef1f2Labels.length][oneElement.size()];
				for (int i = 0; i < oneElement.size(); i++) {
					coeffs = oneElement.elementAt(i);
					for (int j = 0; j < henkef1f2Labels.length; j++)
						anElement[j][i] = coeffs[j];
				}
				henkeEnergyf1f2.setElementAt(anElement, z_id);

/*				doubleElement = new Vector<>();
				for (int i = 0; i < shellIDs.length; i++) {
					rsr = statement.executeQuery("SELECT * FROM xraydata_auger_transitions WHERE (Z_id = " +
							elementNumberAsString + " AND inner_shell_id LIKE " + shellIDs[i] + ")");
					oneElement = new Vector<>();
					while (rsr.next()) {
						String outer_shell1_ID = rsr.getString(augerLabels[0]);
						int outer_shell1 = getShellNumberFromLabel(outer_shell1_ID);
						String outer_shell2_ID = rsr.getString(augerLabels[1]);
						int outer_shell2 = getShellNumberFromLabel(outer_shell2_ID);
						double probability = rsr.getDouble(augerLabels[2]);

						if (!intrashell) {
							coeffs = new double[transitionLabels.length - 1];
							for (int k = 1; k < transitionLabels.length; k++)
								coeffs[k - 1] = rsr.getDouble(transitionLabels[k]) * trasformL[k];
							oneElement.add(coeffs);
							int[] shellNumbers = new int[2];
							idElement.add(transitionID);
							if (transitionID.startsWith("K")) {
								shellNumbers[0] = K;
								shellNumbers[1] = getShellNumberFromLabel(transitionID.substring(1));
							} else {
								shellNumbers[0] = getShellNumberFromLabel(transitionID.substring(0, 2));
								shellNumbers[1] = getShellNumberFromLabel(transitionID.substring(2, 4));
							}
							secondElement.add(shellNumbers);
						}
					}
				}
				auger_transition.setElementAt(doubleElement, z_id);*/

			}
		} catch(SQLException e) {
			// if the error message is "out of memory",
			// it probably means no database file is found
			e.printStackTrace();
			System.err.println(e.getMessage());
		} finally {
			try {
				if(connection != null)
					connection.close();
			} catch(SQLException e) {
				// connection close failed.
				e.printStackTrace();
				System.err.println(e);
			}
		}
/*		System.out.println("Sensitivity: Fe K L1");
		for (double energy = 1.0; energy <= 20.0; energy += 1.0)
			System.out.println(energy + " " + getSensitivity(25, K, energy) * getFluorescenceYield(25, K) + " " + getSensitivity(25, L1, energy) * getFluorescenceYield(25, L1));
		System.out.println("Sensitivity: Pb M2 M5");
		for (double energy = 1.0; energy <= 20.0; energy += 1.0)
			System.out.println(energy + " " + getSensitivity(81, M2, energy) * getFluorescenceYield(81, M2) + " " + getSensitivity(81, M5, energy) * getFluorescenceYield(81, M5));
		System.out.println("Tau: Fe K L1");
		for (double energy = 1.0; energy <= 20.0; energy += 1.0)
			System.out.println(energy + " " + getTauShell(25, K, energy) + " " + getTauShell(25, L1, energy));
		System.out.println("Tau: Pb M2 M5");
		for (double energy = 1.0; energy <= 20.0; energy += 1.0)
			System.out.println(energy + " " + getTauShell(81, M2, energy) + " " + getTauShell(81, M5, energy));*/
		return true;
	}

	public static boolean isIntraShell(String transitionID) {
		if (getFirstLetter(transitionID) == getSecondLetter(transitionID))
			return true;
		return false;
	}

	public static char getFirstLetter(String transitionID) {
		if (transitionID.length() > 0)
			return transitionID.toUpperCase().charAt(0);
		return 'X';
	}

	public static char getSecondLetter(String transitionID) {
		int index = 1;
		while (index < transitionID.length()) {
			char letter = transitionID.toUpperCase().charAt(index++);
			if (!StringNumber.is0to9(letter))
				return letter;
		}
		return 'X';
	}

	public static int getShellNumberFromLabel(String label) {
		for (int i = 0; i < shellIDs.length; i++)
			if (label.equalsIgnoreCase(shellIDs[i]))
				return i;
		return -1; // not recognized
	}

	public static double getTotalAbsorptionForAtomAndEnergy(int atomNumber, double energyInKeV) {
		atomNumber--; // start from 0
		if (atomNumber < 0 || atomNumber >= ebelElastic.size())
			return 0.0;
		loadEbelAndShellTables(false);
		return getCoherentScatteringForAtomAndEnergy(atomNumber, energyInKeV) +
				getIncoherentScatteringForAtomAndEnergy(atomNumber, energyInKeV) +
				getPhotoAbsorptionForAtomAndEnergy(atomNumber, energyInKeV);
	}

	public static double[] getF1F2FromHenkeForAtomAndEnergy(int atomNumber, double energyInKeV) {
		atomNumber--; // start from 0
		loadEbelAndShellTables(false);
		double[] f1f2 = new double[2];
		if (atomNumber >= 0 || atomNumber < henkeEnergyf1f2.size()) {
			double[][] energyf1f2 = henkeEnergyf1f2.elementAt(atomNumber);
			int index = 1;
			while (index < energyf1f2[0].length-1 && energyInKeV > energyf1f2[0][index]) {
				index++;
			}
			int lowindex = index - 1;
			double x_part = (energyInKeV - energyf1f2[0][lowindex]) / (energyf1f2[0][index] - energyf1f2[0][lowindex]);
			f1f2[0] = energyf1f2[1][lowindex] + (energyf1f2[1][index] - energyf1f2[1][lowindex]) * x_part;
			f1f2[1] = energyf1f2[2][lowindex] + (energyf1f2[2][index] - energyf1f2[2][lowindex]) * x_part;
		}
//		System.out.println("AtomSite #:" + (atomNumber + 1) + " " + energyInKeV + " " + f1f2[0] + " " + f1f2[1]);
		return f1f2;
	}

	public static double getCoherentScatteringForAtomAndEnergy(int atomNumber_1, double energyInKeV) {
		return MoreMath.getEbelLogarithmicInterpolation(ebelElastic.elementAt(atomNumber_1), energyInKeV);
	}

	public static double getIncoherentScatteringForAtomAndEnergy(int atomNumber_1, double energyInKeV) {
		return MoreMath.getEbelLogarithmicInterpolation(ebelInelastic.elementAt(atomNumber_1), energyInKeV);
	}

	public static double getPhotoAbsorptionForAtomAndEnergyDiv(int atomNumber_1, double energyInKeV) {
		int shellNumber = getHighestShellIdForAtomAndEnergy(atomNumber_1, energyInKeV);
		if (shellNumber < 0)
			return -1;
		int mainShell = mainShellNumber[shellNumber];
		Vector<double[]> elementData = ebelTauRange.elementAt(atomNumber_1);
		if (mainShell >= elementData.size())
			mainShell = elementData.size() - 1;
//		System.out.println((atomNumber_1 + 1) + " " + energyInKeV + " " + shellNumber + " " + mainShellNumber);
		double result = MoreMath.getEbelLogarithmicInterpolation(elementData.elementAt(mainShell), energyInKeV);
		// now we divide by the jump ratio for certain shellIDs
/*		if (shellNumber > L1 && shellNumber < M1) {
			AtomShellData atomData = shellData.elementAt(atomNumber_1);
//			Vector<double[]> shellEnergiesAndJumps = shellData.elementAt(atomNumber_1);
//			System.out.print("Lx, Dividing by: ");
			for (int i = shellNumber; i > L1; i--) {
				result /= atomData.data.elementAt(i - 1).data[1];
//				System.out.print(atomData.data.elementAt(i - 1).data[1] + " ");
			}
//			System.out.println();
		} else if (shellNumber > M1 && shellNumber < N1) {
			AtomShellData atomData = shellData.elementAt(atomNumber_1);
			// Vector<double[]> shellEnergiesAndJumps = shellData.elementAt(atomNumber_1);
//			System.out.print("Mx, Dividing by: ");
			for (int i = shellNumber; i > M1; i--) {
				result /= atomData.data.elementAt(i - 1).data[1];
//				System.out.print(atomData.data.elementAt(i - 1).data[1] + " ");
			}
//			System.out.println();
		}*/
		return result;
	}

	public static double getPhotoAbsorptionForAtomAndEnergy(int atomNumber_1, double energyInKeV) {
		int shellNumber = getHighestShellIdForAtomAndEnergy(atomNumber_1, energyInKeV);
		if (shellNumber < 0)
			return 0;
		int mainShell = mainShellNumber[shellNumber];
		Vector<double[]> elementData = ebelTauRange.elementAt(atomNumber_1);
		if (mainShell >= elementData.size())
			return 0;
//		System.out.println((atomNumber_1 + 1) + " " + energyInKeV + " " + shellNumber + " " + mainShellNumber);
		double result = MoreMath.getEbelLogarithmicInterpolation(elementData.elementAt(mainShell), energyInKeV);
		// now we divide by the jump ratio for certain shellIDs
		if (shellNumber > L1 && shellNumber < M1) {
			if (mainShell + 1 < elementData.size()) {
				AtomShellData atomData = shellData.elementAt(atomNumber_1);
				double result1 = MoreMath.getEbelLogarithmicInterpolation(elementData.elementAt(mainShell + 1), energyInKeV);
				double delta = result - result1;
				for (int i = shellNumber; i > L1; i--)
					result -= delta / atomData.data.elementAt(i - 1).data[1];
			} else {
//				System.out.println((atomNumber_1 + 1) + " " + energyInKeV + " " + elementData.size() + " " + mainShell + " " + shellNumber);
			}
		} else if (shellNumber > M1 && shellNumber < N1) {
			if (mainShell + 1 < elementData.size()) {
				AtomShellData atomData = shellData.elementAt(atomNumber_1);
				double result1 = MoreMath.getEbelLogarithmicInterpolation(elementData.elementAt(mainShell + 1), energyInKeV);
				double delta = result - result1;
				for (int i = shellNumber; i > M1; i--)
					result -= delta / atomData.data.elementAt(i - 1).data[1];
			} else {
//				System.out.println((atomNumber_1 + 1) + " " + energyInKeV + " " + elementData.size() + " " + mainShell + " " + shellNumber);
			}
		}
		return result;
	}

	public static int getHighestShellIdForAtomAndEnergy(int atomNumber_1, double energyInKeV) {
		try {
			AtomShellData atomData = shellData.elementAt(atomNumber_1);
			if (atomData == null) {
				System.out.println("Warning: problem with atom number(-1) " + atomNumber_1);
			} else {
//			System.out.println("Finding shell for energy: " + energyIneV + " and atom number: " + atomNumber_1);
				for (int i = 0; i < atomData.data.size(); i++) {
//				System.out.println("Check energy: " + shellEnergies.elementAt(i)[0]);
					if (atomData.data.elementAt(i).data[0] <= energyInKeV)
						return i;
				}
			}
		} catch (Exception e) {
			System.out.println("Exception retrieving photo absorption data for atom number: " + atomNumber_1 + ", at energy: " + energyInKeV + " KeV");
			e.printStackTrace(System.out);
		}
		return -1;
	}

	public static double getFluorescenceYield(int atomNumber_1, int shellID) {
		if (shellID < 0 || shellID >= yieldData.elementAt(atomNumber_1).size())
			return 0;
		return yieldData.elementAt(atomNumber_1).elementAt(shellID)[0];
	}

	public static double getJumpRatio(int atomNumber_1, int shellID) {
		ShellDataAndID data = shellData.elementAt(atomNumber_1).getDataFor(shellID);
		if (data != null)
			return data.data[1];
		return 0;
	}

	public static double getAbsorptionEdge(int atomNumber_1, int shellID) {
		ShellDataAndID data = shellData.elementAt(atomNumber_1).getDataFor(shellID);
		if (data != null)
			return data.data[0];
		return 0;
	}

	public static double getTauShell(int atomNumber_1, int shellID, double energyInKeV) {
//		System.out.println(atomNumber_1 + " " + shellID + " " + energyInKeV + " " + getAbsorptionEdge(atomNumber_1, shellID));
		if (shellID < 0) // || getAbsorptionEdge(atomNumber_1, shellID) > energyInKeV)
			return 0;
		if (shellID >= ebelTauShell.elementAt(atomNumber_1).size())
			shellID = ebelTauShell.elementAt(atomNumber_1).size() - 1;
		return MoreMath.getEbelLogarithmicInterpolation(ebelTauShell.elementAt(atomNumber_1).elementAt(shellID),
				energyInKeV);
	}

	public static void loadAugerDataForAtomm(int atom_number) {

		Connection connection = null;
		try {
			// create a database connection
			xray_database = MaudPreferences.getPref("fluorescence.database", Constants.documentsDirectory + "xraydata.db");
			connection = DriverManager.getConnection("jdbc:sqlite:" + xray_database);
//			connection = DriverManager.getConnection("jdbc:sqlite:" + xray_database);
			Statement statement = connection.createStatement();
			statement.setQueryTimeout(30);  // set timeout to 30 sec.

			ResultSet rs = statement.executeQuery("SELECT * FROM xraydata_auger_transitions WHERE (Z_id = " +
					atom_number + ")");
			while(rs.next()) {
				double[] coeffs = new double[2];
				for (int k = 0; k < coefficientIDs.length; k++)
					coeffs[k] = rs.getDouble(coefficientIDs[k]);
				ebelElastic.add(coeffs);
			}

			Hashtable<Integer, double[][]> costerKronigData_k = new Hashtable<>(atomsNumber, 1);
			for (int j = 0; j < atomsNumber; j++) {
				double[][] coeffs = new double[costerKronigMax_k][costerKronigMax_k];
				costerKronigData_k.put(j, coeffs);
			}
			costerKronigData.put(K, costerKronigData_k);

		} catch(SQLException e) {
			// if the error message is "out of memory",
			// it probably means no database file is found
			e.printStackTrace();
			System.err.println(e.getMessage());
		} finally {
			try {
				if(connection != null)
					connection.close();
			} catch(SQLException e) {
				// connection close failed.
				e.printStackTrace();
				System.err.println(e);
			}
		}
	   /*
        if outer_shell_1 is not None and outer_shell_2 is not None:
            SELECT1 = '''SELECT * FROM xraydata_auger_transitions WHERE
                      (Z_id = %d
                      AND inner_shell_id LIKE '%s'
                      AND outer_shell1_id LIKE '%s'
                      AND outer_shell2_id LIKE '%s')''' \
                      % (self.Z, init_exc_shell, outer_shell_1, outer_shell_2)
        elif outer_shell_1 is None and outer_shell_2 is not None:
            SELECT1='''SELECT * FROM xraydata_auger_transitions WHERE
                    (Z_id = %d
                    AND inner_shell_id LIKE '%s'
                    AND outer_shell2_id LIKE '%s')''' \
                    % (self.Z, init_exc_shell, outer_shell_2)
        elif outer_shell_1 is not None and outer_shell_2 is None:
            SELECT1='''SELECT * FROM xraydata_auger_transitions WHERE
                    (Z_id = %d
                    AND inner_shell_id LIKE '%s'
                    AND outer_shell1_id LIKE '%s')''' \
                    % (self.Z, init_exc_shell, outer_shell_1)
        elif outer_shell_1 is None and outer_shell_2 is None:
            SELECT1='''SELECT * FROM xraydata_auger_transitions WHERE
                    (Z_id = %d
                    AND inner_shell_id LIKE '%s')''' \
                    % (self.Z, init_exc_shell)
        cur1 = xdb.get_new_cursor()
        cur1.execute(SELECT1)
        data = cur1.fetchall()
        return data

 */

	}
        
   public static double getAugerProbability(int atom_number_1, int innerShell, int outerShell1, int outerShell2) {

//	   auger_transition.get();




      return 0.0;
   }

	public static double getSensitivityWithCascade(int atomNumber, int shellID, int xrl_line_number, double energyInKeV, double fluorescenceYield) {

		double sensitivity = 0;
		if (xrl_line_number != 0) {
			try {
		//		double sensitivity_nocs = Xraylib.CS_FluorLine_Kissel_no_Cascade(atomNumber, xrl_line_number, energyInKeV);
				sensitivity = Xraylib.CS_FluorLine_Kissel_Cascade(atomNumber, xrl_line_number, energyInKeV);
	//			System.out.println("Sensitivity difference for: " + atomNumber + " " + xrl_line_number + " " + energyInKeV + " " + (sensitivity - sensitivity_nocs));
			} catch (XraylibException xe) {
				return getSensitivityNoXrl(atomNumber, shellID, energyInKeV, fluorescenceYield);
			}
		}
		if (fluorescenceYield == 0) fluorescenceYield = 1;
		return sensitivity / fluorescenceYield;

	}

	public static double getSensitivity(int atomNumber, int shellID, int xrl_line_number, double energyInKeV, double fluorescenceYield) {
		if (shellID == -1)
			return 0;
		if (useCascadeForSensitivity)
			return getSensitivityWithCascade(atomNumber, shellID, xrl_line_number, energyInKeV, fluorescenceYield);
		else
			return getSensitivityNoCascade(atomNumber, shellID, xrl_line_number, energyInKeV, fluorescenceYield);
	}

	public static double getSensitivityNoCascade(int atomNumber, int shellID, int xrl_line_number, double energyInKeV, double fluorescenceYield) {
		double sensitivity = 0;
		if (xrl_line_number != 0) {
			try {
				sensitivity = Xraylib.CS_FluorLine_Kissel_no_Cascade(atomNumber, xrl_line_number, energyInKeV);
//				sensitivity = Xraylib.CS_FluorLine_Kissel(atomNumber, xrl_line_number, energyInKeV);
				//			System.out.println("Sensitivity difference for: " + atomNumber + " " + xrl_line_number + " " + energyInKeV + " " + (sensitivity - sensitivity_nocs));
			} catch (XraylibException xe) {
				return getSensitivityNoXrl(atomNumber, shellID, energyInKeV, fluorescenceYield);
			}
		}
		if (fluorescenceYield == 0) fluorescenceYield = 1;
		return sensitivity / fluorescenceYield;
  	}

	public static double getSensitivityNoXrl(int atomNumber, int shellID, double energyInKeV, double fluorescenceYield) {
		int msi = mainShellIndex[shellID];
      if (msi > 2) return 0;
		Hashtable<Integer, double[][]> ckv = costerKronigData.get(msi);
		if (ckv == null) return 0;
		double[][] ck_coeff = ckv.get(atomNumber - 1);
		if (ck_coeff == null) return 0;
      double[] sensitivityTr = new double[shellID - msi + 1];
      for (int i = msi; i <= shellID; i++) {
         sensitivityTr[i - msi] = getTauShell(atomNumber - 1, i, energyInKeV);
//	      if (atomNumber == 82)
//		      System.out.println(energyInKeV + " " + getTauShell(atomNumber - 1, i, energyInKeV));
         for (int j = msi; j < i; j++)
	         sensitivityTr[i - msi] += sensitivityTr[j - msi] * ck_coeff[j - msi][i - msi - 1];
      }
 		return sensitivityTr[shellID - msi]; // * fluorescenceYield;
	}

	public static Vector<FluorescenceLine> getFluorescenceLinesFor(int atomNumber, double energyInKeV) {
		return getFluorescenceLinesFor(atomNumber, energyInKeV, linesMinimumEnergy);
	}

	public static void checkMinimumEnergy() {
		linesMinimumEnergy = MaudPreferences.getDouble("fluorescenceLines.minimum_keV", linesMinimumEnergy);
	}

	public static Vector<FluorescenceLine> getFluorescenceLinesFor(int atomNumber, double energyInKeV,
	                                                               double minimumEnergyInKeV) {
		atomNumber--;
		Vector<FluorescenceLine> linesForAtom = new Vector(0, 10);
		loadEbelAndShellTables(false);

		Vector<int[]> shellIDsData = transitionShellIDs.elementAt(atomNumber);
		Vector<double[]> shellEnergies = transitionEnergies.elementAt(atomNumber);
		Vector<String> idLabels = transitionShellIDlabels.elementAt(atomNumber);
		for (int i = 0; i < shellIDsData.size(); i++) {
			double[] transitionEnergy = shellEnergies.elementAt(i);
			int innerShell = shellIDsData.elementAt(i)[0];
			String id = idLabels.elementAt(i);
			if (innerShell >= 0 && transitionEnergy[0] > minimumEnergyInKeV && energyInKeV > transitionEnergy[0]) {
				double fluorescenceYield = getFluorescenceYield(atomNumber, innerShell);
				FluorescenceLine aLine = new FluorescenceLine(transitionEnergy[0], innerShell, getAbsorptionEdge(atomNumber, innerShell), id);
				aLine.setFluorescenceYield(fluorescenceYield);
				aLine.setTransitionProbability(transitionEnergy[1]);
				double sensitivity = getSensitivity(atomNumber, innerShell, aLine.xrl_line_number, energyInKeV, fluorescenceYield * transitionEnergy[1]);
//				if (atomNumber == 39)
//					System.out.println(energyInKeV + " " + transitionEnergy[0] + " " + sensitivity + " " +
//							innerShell + " " + transitionEnergy[1] + " " + fluorescenceYield + " " + getAbsorptionEdge(atomNumber, innerShell));
				if (sensitivity < 0)
					sensitivity = 0;
				aLine.setIntensity(fluorescenceYield * sensitivity * transitionEnergy[1]); // this is the probability
				linesForAtom.addElement(aLine);
			}
		}
		return linesForAtom;
	}

	public static Vector<FluorescenceLine> getFluorescenceLinesNoSensitivityFor(int atomNumber, double energyInKeV) {
//		linesMinimumEnergy = MaudPreferences.getDouble("fluorescenceLines.minimum_keV", linesMinimumEnergy);
		return getFluorescenceLinesNoSensitivityFor(atomNumber, energyInKeV, linesMinimumEnergy);
	}

	public static Vector<FluorescenceLine> getFluorescenceLinesNoSensitivityFor(int atomNumber, double energyInKeV,
	                                                               double minimumEnergyInKeV) {
		atomNumber--;
		Vector<FluorescenceLine> linesForAtom = new Vector(0, 10);
		loadEbelAndShellTables(false);

		Vector<int[]> shellIDsData = transitionShellIDs.elementAt(atomNumber);
		Vector<double[]> shellEnergies = transitionEnergies.elementAt(atomNumber);
		Vector<String> idLabels = transitionShellIDlabels.elementAt(atomNumber);
		for (int i = 0; i < shellIDsData.size(); i++) {
			double[] transitionEnergy = shellEnergies.elementAt(i);
			int innerShell = shellIDsData.elementAt(i)[0];
			String id = idLabels.elementAt(i);
			if (innerShell >= 0 && transitionEnergy[0] > minimumEnergyInKeV && energyInKeV > transitionEnergy[0]) {
				double fluorescenceYield = getFluorescenceYield(atomNumber, innerShell);
				FluorescenceLine aLine = new FluorescenceLine(transitionEnergy[0], innerShell, getAbsorptionEdge(atomNumber, innerShell), id);
				aLine.setFluorescenceYield(fluorescenceYield);
				aLine.setIntensity(fluorescenceYield * transitionEnergy[1]); // this is the probability without sensitivity
				aLine.setTransitionProbability(transitionEnergy[1]);
				linesForAtom.addElement(aLine);
/*				if (atomNumber > 80)
					System.out.println(atomNumber + ", Line: " + energyInKeV + " " + transitionEnergy[0] + " " +
							innerShell + " " + transitionEnergy[1] + " " + fluorescenceYield + " " + getAbsorptionEdge(atomNumber, innerShell));*/
			}
		}
		return linesForAtom;
	}

}

class AtomShellData {
	int atomNumber = -1;
	Vector<ShellDataAndID> data;

	public AtomShellData(int anAtomNumber) {
		atomNumber = anAtomNumber;
		data = new Vector<ShellDataAndID>();
	}

	public void addData(ShellDataAndID newData) {
		data.add(newData);
	}

	public ShellDataAndID getDataFor(int shellID) {
		for (int i = 0; i < data.size(); i++)
			if (data.elementAt(i).shellID == shellID)
				return data.elementAt(i);
		return null;
	}
}

class ShellDataAndID {
	int shellID = -1;
	double[] data = null;

	public ShellDataAndID(String shellIDs) {
		shellID = XRayDataSqLite.getShellNumberFromLabel(shellIDs);
	}

	public void addData(double[] newData) {
		data = newData;
	}
}

/*Copyright (c) 2010, Tom Schoonjans
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
    * The names of the contributors may not be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY Tom Schoonjans ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL Tom Schoonjans BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
/*
class xrlexample7 {
	public static void main(String argv[]) {
		//Xraylib.SetErrorMessages(0);
//		Xraylib.SetHardExit(1);
		System.out.println("Example of java program using Xraylib");
		System.out.println("Atomic weight of Fe: "+Xraylib.AtomicWeight(26)+" g/mol");
		System.out.println("Density of pure Al: "+Xraylib.ElementDensity(13)+" g/cm3");
		System.out.println("Photoionization cs of Fe at 10.0 keV: "+Xraylib.CS_Photo(26, 10.0)+" cm2/g");
		System.out.println("Rayleigh scattering cs of Fe at 10.0 keV: "+Xraylib.CS_Rayl(26, 10.0)+" cm2/g");
		System.out.println("Compton scattering cs of Fe at 10.0 keV: "+Xraylib.CS_Compt(26, 10.0)+" cm2/g");
		System.out.println("Total cs of Fe at 10.0 keV: "+Xraylib.CS_Total(26, 10.0)+" cm2/g");
		System.out.println("Total cs (Kissel) of Fe at 10.0 keV: "+Xraylib.CS_Total_Kissel(26, 10.0)+" cm2/g");
		System.out.println("Al mass energy-absorption cs at 20.0 keV: "+ Xraylib.CS_Energy(13, 20.0));
		System.out.println("K atomic level width for Fe: "+Xraylib.AtomicLevelWidth(26,Xraylib.K_SHELL) + " keV");
		System.out.println("K fluorescence yield for Fe: "+Xraylib.FluorYield(26,Xraylib.K_SHELL));
		System.out.println("K jumpfactor for Fe: "+Xraylib.JumpFactor(26,Xraylib.K_SHELL));
		System.out.println("M1->M5 Coster-Kronig transition probability for Au : "+Xraylib.CosKronTransProb(79,Xraylib.FM15_TRANS));
		System.out.println("L1->L3 Coster-Kronig transition probability for Fe : "+Xraylib.CosKronTransProb(26,Xraylib.FL13_TRANS));
		System.out.println("Bi M1N2 radiative rate: "+Xraylib.RadRate(83,Xraylib.M1N2_LINE));
		System.out.println("Zr L1 edge energy: " + Xraylib.EdgeEnergy(40, Xraylib.L1_SHELL) + " keV");
		System.out.println("Fe atomic form factor: " + Xraylib.FF_Rayl(26, 1.0));
		System.out.println("Ni scattering form factor: " + Xraylib.SF_Compt(28, 1.0));
		System.out.println("Differential Thomson cross section at 45 deg: " + Xraylib.DCS_Thoms(45.0*Math.PI/180.0) + " cm2/g");
		System.out.println("Differential Klein-Nishina cross section at 10 keV and 45 deg: " + Xraylib.DCS_KN(10.0, 45.0*Math.PI/180.0) + " cm2/g");
		System.out.println("Differential Rayleigh cross section for Zn at 10 keV and 45 deg: " + Xraylib.DCS_Rayl(30, 10.0, 45.0*Math.PI/180.0) + " cm2/g");
		System.out.println("Differential Compton cross section for Zn at 10 keV and 45 deg: " + Xraylib.DCS_Compt(30, 10.0, 45.0*Math.PI/180.0) + " cm2/g");
		System.out.println("Moment transfer function at 10 keV and 45 deg: " + Xraylib.MomentTransf(10.0, 45.0*Math.PI/180.0));
		System.out.println("Klein-Nishina cross section at 10 keV: " + Xraylib.CS_KN(10.0) + " cm2/g");
		System.out.println("Photon energy after Compton scattering at 10 keV and 45 deg angle: " + Xraylib.ComptonEnergy(10.0, 45.0*Math.PI/180.0));
		System.out.println("Photoionization cs of Fe (Kissel) at 10.0 keV: "+Xraylib.CS_Photo_Total(26, 10.0)+" cm2/g");
		System.out.println("Fe partial photoionization cs of L3 at 6.0 keV: "+Xraylib.CS_Photo_Partial(26,Xraylib.L3_SHELL, 6.0));
		System.out.println("ElectronConfig (Kissel) of Fe L3-shell: " + Xraylib.ElectronConfig(26, Xraylib.L3_SHELL));
		System.out.println("ElectronConfig (Biggs) of Fe L3-shell: " + Xraylib.ElectronConfig_Biggs(26, Xraylib.L3_SHELL));
		System.out.println("Compton profile for Fe at pz = 1.1: "+Xraylib.ComptonProfile(26,(float) 1.1));
		System.out.println("M5 Partial Compton profile for Fe at pz = 1.1: "+Xraylib.ComptonProfile_Partial(26,Xraylib.M5_SHELL, 1.1));
		System.out.println("Bi L2-M5M5 Auger non-radiative rate: "+Xraylib.AugerRate(86,Xraylib.L2_M5M5_AUGER));
		System.out.println("Bi L3 Auger yield: "+Xraylib.AugerYield(86, Xraylib.L3_SHELL));
		System.out.println("Ca K-alpha Fluorescence Line Energy: "+Xraylib.LineEnergy(20,Xraylib.KA_LINE));
		System.out.println("U M3O3 Fluorescence Line Energy: "+Xraylib.LineEnergy(92,Xraylib.M3O3_LINE));
		System.out.println("Pb Lalpha XRF production cs at 20.0 keV (jump approx): "+Xraylib.CS_FluorLine(82, Xraylib.LA_LINE, 20.0));
		System.out.println("Pb Lalpha XRF production cs at 20.0 keV (Kissel): "+Xraylib.CS_FluorLine_Kissel(82, Xraylib.LA_LINE, 20.0));
		System.out.println("Au Ma1 XRF production cs at 10.0 keV (Kissel): "+Xraylib.CS_FluorLine_Kissel(79,Xraylib.MA1_LINE,(float) 10.0));
		System.out.println("Au Mb XRF production cs at 10.0 keV (Kissel): "+Xraylib.CS_FluorLine_Kissel(79,Xraylib.MB_LINE,(float) 10.0));
		System.out.println("Au Mg XRF production cs at 10.0 keV (Kissel): "+Xraylib.CS_FluorLine_Kissel(79,Xraylib.MG_LINE,(float) 10.0));
		System.out.println("Pb Malpha XRF production cs at 20.0 keV with cascade effect: "+Xraylib.CS_FluorLine_Kissel(82,Xraylib.MA1_LINE,(float) 20.0));
		System.out.println("Pb Malpha XRF production cs at 20.0 keV with radiative cascade effect: "+Xraylib.CS_FluorLine_Kissel_Radiative_Cascade(82,Xraylib.MA1_LINE,(float) 20.0));
		System.out.println("Pb Malpha XRF production cs at 20.0 keV with non-radiative cascade effect: "+Xraylib.CS_FluorLine_Kissel_Nonradiative_Cascade(82,Xraylib.MA1_LINE,(float) 20.0));
		System.out.println("Pb Malpha XRF production cs at 20.0 keV without cascade effect: "+Xraylib.CS_FluorLine_Kissel_no_Cascade(82,Xraylib.MA1_LINE,(float) 20.0));
		System.out.println("Sr anomalous scattering factor Fi at 10.0 keV: " + Xraylib.Fi(38, 10.0));
		System.out.println("Sr anomalous scattering factor Fii at 10.0 keV: " + Xraylib.Fii(38, 10.0));
		System.out.println("Symbol of element 26 is: " + Xraylib.AtomicNumberToSymbol(26));
		System.out.println("Number of element Fe is: " + Xraylib.SymbolToAtomicNumber("Fe"));
		System.out.println(Xraylib.CompoundParser("Ca(HCO3)2"));
		System.out.println(Xraylib.CompoundParser("SiO2"));
		System.out.println(Xraylib.CompoundParser("Ca5(PO4)OH"));
		System.out.println(Xraylib.CompoundParser("Fe0.6Mn0.4SiO3"));
		System.out.println("Total cs of SiO2 at 10.0 keV: "+Xraylib.CS_Total_CP("SiO2", 10.0)+" cm2/g");
		System.out.println("Total cs of SiO2 at 10.0 keV: "+Xraylib.CSb_Total_CP("SiO2", 10.0)+" barns/atom");
		System.out.println("Rayleigh cs of SiO2 at 10.0 keV: "+Xraylib.CS_Rayl_CP("SiO2", 10.0)+" cm2/g");
		System.out.println("Rayleigh cs of SiO2 at 10.0 keV: "+Xraylib.CSb_Rayl_CP("SiO2", 10.0)+" barns/atom");
		System.out.println("Compton cs of SiO2 at 10.0 keV: "+Xraylib.CS_Compt_CP("SiO2", 10.0)+" cm2/g");
		System.out.println("Compton cs of SiO2 at 10.0 keV: "+Xraylib.CSb_Compt_CP("SiO2", 10.0)+" barns/atom");
		System.out.println("Photoionization cs of SiO2 at 10.0 keV: "+Xraylib.CS_Photo_CP("SiO2", 10.0)+" cm2/g");
		System.out.println("Photoionization cs of SiO2 at 10.0 keV: "+Xraylib.CSb_Photo_CP("SiO2", 10.0)+" barns/atom");
		System.out.println("Differential Rayleigh cs of SiO2 at 10.0 keV and 45 deg theta: "+Xraylib.DCS_Rayl_CP("SiO2", 10.0, Math.PI/4.0)+" cm2/g/sterad");
		System.out.println("Differential Rayleigh cs of SiO2 at 10.0 keV and 45 deg theta: "+Xraylib.DCSb_Rayl_CP("SiO2", 10.0, Math.PI/4.0)+" barns/atom/sterad");
		System.out.println("Differential Compton cs of SiO2 at 10.0 keV and 45 deg theta: "+Xraylib.DCS_Compt_CP("SiO2", 10.0, Math.PI/4.0)+" cm2/g/sterad");
		System.out.println("Differential Compton cs of SiO2 at 10.0 keV and 45 deg theta: "+Xraylib.DCSb_Compt_CP("SiO2", 10.0, Math.PI/4.0)+" barns/atom/sterad");
		System.out.println("Polarized differential Rayleigh cs of SiO2 at 10.0 keV and 45 deg theta and 90 deg phi: "+Xraylib.DCSP_Rayl_CP("SiO2", 10.0, Math.PI/4.0, Math.PI/2.0)+" cm2/g/sterad");
		System.out.println("Polarized differential Rayleigh cs of SiO2 at 10.0 keV and 45 deg theta and 90 deg phi: "+Xraylib.DCSPb_Rayl_CP("SiO2", 10.0, Math.PI/4.0, Math.PI/2.0)+" barns/atom/sterad");
		System.out.println("Polarized differential Compton cs of SiO2 at 10.0 keV and 45 deg theta and 90 deg phi: "+Xraylib.DCSP_Compt_CP("SiO2", 10.0, Math.PI/4.0, Math.PI/2.0)+" cm2/g/sterad");
		System.out.println("Polarized differential Compton cs of SiO2 at 10.0 keV and 45 deg theta and 90 deg phi: "+Xraylib.DCSPb_Compt_CP("SiO2", 10.0, Math.PI/4.0, Math.PI/2.0)+" barns/atom/sterad");
		System.out.println("Total cs of Polymethyl Methacralate (Lucite, Perspex) at 10.0 keV: "+Xraylib.CS_Total_CP("Polymethyl Methacralate (Lucite, Perspex)", 10.0)+" cm2/g");

		try {
			// the following line should throw an exception
			double testvalue = Xraylib.DCSb_Compt_CP("SiO2)", 10.0, Math.PI/4.0);
			System.exit(1);
		}
		catch (XraylibException e) {}

		double energy = 8;
		double debye_temp_factor = 1.0;
		double rel_angle = 1.0;
		int i;

		double bragg, q, dw;
		double f0 = 0.0, fp = 0.0, fpp = 0.0;
		double[] factors;
		Complex FH, F0;
		Complex FHbar;
		String[] crystalNames;

		// Si Crystal structure
		Crystal_Struct cryst = Xraylib.Crystal_GetCrystal("Si");
		System.out.format("Si unit cell dimensions are %f %f %f%n", cryst.a, cryst.b, cryst.c);
		System.out.format("Si unit cell angles are %f %f %f%n", cryst.alpha, cryst.beta, cryst.gamma);
		System.out.format("Si unit cell volume is %f%n", cryst.volume);
		System.out.format("Si atoms at:%n");
		System.out.format("   Z  fraction    X        Y        Z%n");
		for (i = 0; i < cryst.n_atom; i++) {
			Crystal_Atom atom = cryst.atom[i];
			System.out.format("  %3d %f %f %f %f%n", atom.Zatom, atom.fraction, atom.x, atom.y, atom.z);
		}

  		// Si diffraction parameters

		System.out.format("%nSi111 at 8 KeV. Incidence at the Bragg angle:%n");

		bragg = Xraylib.Bragg_angle(cryst, energy, 1, 1, 1);
		System.out.format("  Bragg angle: Rad: %f Deg: %f%n", bragg, bragg*180/Math.PI);

		q = Xraylib.Q_scattering_amplitude (cryst, energy, 1, 1, 1, rel_angle);
		System.out.format("  Q Scattering amplitude: %f%n", q);

		factors = Xraylib.Atomic_Factors(14, energy, q, debye_temp_factor);
		f0 = factors[0];
		fp = factors[1];
		fpp = factors[2];
		System.out.format("  Atomic factors (Z = 14) f0, fp, fpp: %f, %f, i*%f%n", f0, fp, fpp);

		FH = Xraylib.Crystal_F_H_StructureFactor(cryst, energy, 1, 1, 1, debye_temp_factor, rel_angle);
		System.out.format("  FH(1,1,1) structure factor: (%f, %f)%n", FH.getReal(), FH.getImaginary());

		F0 = Xraylib.Crystal_F_H_StructureFactor(cryst, energy, 0, 0, 0, debye_temp_factor, rel_angle);
		System.out.format("  F0=FH(0,0,0) structure factor: (%f, %f)%n", F0.getReal(), F0.getImaginary());


		// Diamond diffraction parameters

		cryst = Xraylib.Crystal_GetCrystal("Diamond");

		System.out.format("%nDiamond 111 at 8 KeV. Incidence at the Bragg angle:%n");

		bragg = Xraylib.Bragg_angle(cryst, energy, 1, 1, 1);
		System.out.format("  Bragg angle: Rad: %f Deg: %f%n", bragg, bragg*180/Math.PI);

		q = Xraylib.Q_scattering_amplitude (cryst, energy, 1, 1, 1, rel_angle);
		System.out.format("  Q Scattering amplitude: %f%n", q);

		factors = Xraylib.Atomic_Factors (6, energy, q, debye_temp_factor);
		f0 = factors[0];
		fp = factors[1];
		fpp = factors[2];
		System.out.format("  Atomic factors (Z = 6) f0, fp, fpp: %f, %f, i*%f%n", f0, fp, fpp);

		FH = Xraylib.Crystal_F_H_StructureFactor(cryst, energy, 1, 1, 1, debye_temp_factor, rel_angle);
		System.out.format("  FH(1,1,1) structure factor: (%f, %f)%n", FH.getReal(), FH.getImaginary());

		F0 = Xraylib.Crystal_F_H_StructureFactor(cryst, energy, 0, 0, 0, debye_temp_factor, rel_angle);
		System.out.format("  F0=FH(0,0,0) structure factor: (%f, %f)%n", F0.getReal(), F0.getImaginary());

		FHbar = Xraylib.Crystal_F_H_StructureFactor(cryst, energy, -1, -1, -1, debye_temp_factor, rel_angle);
		dw = 1e10 * 2 * (Xraylib.R_E / cryst.volume) * (Xraylib.KEV2ANGST * Xraylib.KEV2ANGST/ (energy * energy)) * Math.sqrt(FH.multiply(FHbar).abs()) / Math.PI / Math.sin(2.0*bragg);
		System.out.format("  Darwin width: %f micro-radians%n", 1e6*dw);

		// Alpha Quartz diffraction parameters
		// Object methods here

		cryst = Xraylib.Crystal_GetCrystal("AlphaQuartz");

		System.out.format("%nAlpha Quartz 020 at 8 KeV. Incidence at the Bragg angle:%n");

		bragg = cryst.Bragg_angle(energy, 0, 2, 0);
		System.out.format("  Bragg angle: Rad: %f Deg: %f%n", bragg, bragg*180/Math.PI);

		q = cryst.Q_scattering_amplitude (energy, 0, 2, 0, rel_angle);
		System.out.format("  Q Scattering amplitude: %f%n", q);

		factors = Xraylib.Atomic_Factors(8, energy, q, debye_temp_factor);
		f0 = factors[0];
		fp = factors[1];
		fpp = factors[2];
		System.out.format("  Atomic factors (Z = 8) f0, fp, fpp: %f, %f, i*%f%n", f0, fp, fpp);

		FH = cryst.Crystal_F_H_StructureFactor(energy, 0, 2, 0, debye_temp_factor, rel_angle);
		System.out.format("  FH(0,2,0) structure factor: (%f, %f)%n", FH.getReal(), FH.getImaginary());

		F0 = cryst.Crystal_F_H_StructureFactor(energy, 0, 0, 0, debye_temp_factor, rel_angle);
		System.out.format("  F0=FH(0,0,0) structure factor: (%f, %f)%n", F0.getReal(), F0.getImaginary());

		// Muscovite diffraction parameters

		cryst = Xraylib.Crystal_GetCrystal("Muscovite");

		System.out.format("%nMuscovite 331 at 8 KeV. Incidence at the Bragg angle:%n");

		bragg = Xraylib.Bragg_angle(cryst, energy, 3, 3, 1);
		System.out.format("  Bragg angle: Rad: %f Deg: %f%n", bragg, bragg*180/Math.PI);

		q = Xraylib.Q_scattering_amplitude(cryst, energy, 3, 3, 1, rel_angle);
		System.out.format("  Q Scattering amplitude: %f%n", q);

		factors = Xraylib.Atomic_Factors(19, energy, q, debye_temp_factor);
		f0 = factors[0];
		fp = factors[1];
		fpp = factors[2];
		System.out.format("  Atomic factors (Z = 19) f0, fp, fpp: %f, %f, i*%f%n", f0, fp, fpp);

		FH = Xraylib.Crystal_F_H_StructureFactor(cryst, energy, 3, 3, 1, debye_temp_factor, rel_angle);
		System.out.format("  FH(3,3,1) structure factor: (%f, %f)\n", FH.getReal(), FH.getImaginary());

		F0 = Xraylib.Crystal_F_H_StructureFactor(cryst, energy, 0, 0, 0, debye_temp_factor, rel_angle);
		System.out.format("  F0=FH(0,0,0) structure factor: (%f, %f)%n", F0.getReal(), F0.getImaginary());

		crystalNames = Xraylib.Crystal_GetCrystalsList();
		System.out.format("List of available crystals:%n");
		for (i = 0 ; i < crystalNames.length  ; i++) {
			System.out.format("  Crystal %d: %s%n", i, crystalNames[i]);
		}

		System.out.format("%n");

		System.out.println("%n" + Xraylib.GetCompoundDataNISTByName("Uranium Monocarbide"));
		System.out.println(Xraylib.GetCompoundDataNISTByIndex(Xraylib.NIST_COMPOUND_BRAIN_ICRP));
		String[] nistCompounds = Xraylib.GetCompoundDataNISTList();

		System.out.println("List of available NIST compounds:");
		for (i = 0 ; i < nistCompounds.length ; i++) {
			System.out.format("  Compound %d: %s%n", i, nistCompounds[i]);
		}

		System.out.println("%n" + Xraylib.GetRadioNuclideDataByName("109Cd"));
		System.out.println(Xraylib.GetRadioNuclideDataByIndex(Xraylib.RADIO_NUCLIDE_125I));
		String[] radioNuclides = Xraylib.GetRadioNuclideDataList();

		System.out.println("List of available radionuclides:");
		for (i = 0 ; i < radioNuclides.length ; i++) {
			System.out.format("  Radionuclide %d: %s%n", i, radioNuclides[i]);
		}

		System.out.println("CS2 Refractive Index at 10.0 keV : "+Xraylib.Refractive_Index_Re("CS2", 10.0, 1.261)+" - "+Xraylib.Refractive_Index_Im("CS2", 10.0, 1.261)+" i");
		System.out.println("C16H14O3 Refractive Index at 1 keV : "+Xraylib.Refractive_Index_Re("C16H14O3", 1.0, 1.2)+" - "+Xraylib.Refractive_Index_Im("C16H14O3", 1.0, 1.2)+" i");
		System.out.println("SiO2 Refractive Index at 5.0 keV : "+Xraylib.Refractive_Index_Re("SiO2", 5.0, 2.65)+" - "+Xraylib.Refractive_Index_Im("SiO2", 5.0, 2.65)+" i");
		Complex refr = Xraylib.Refractive_Index("CS2", 10.0, 1.261);
		System.out.println("CS2 Refractive Index at 10.0 keV : "+ refr.getReal()+" - "+ refr.getImaginary()+" i");


//		System.out.println("Ca(HCO3)2 Rayleigh cs at 10.0 keV: "+Xraylib.CS_Rayl_CP("Ca(HCO3)2",(float) 10.0) );
//		System.out.println("Al mass energy-absorption cs at 20.0 keV: "+ Xraylib.CS_Energy(13, (float) 20.0));
//		System.out.println("Pb mass energy-absorption cs at 40.0 keV: "+ Xraylib.CS_Energy(82, (float) 40.0));
//		System.out.println("CdTe mass energy-absorption cs at 40.0 keV: "+ Xraylib.CS_Energy_CP("CdTe", (float) 40.0));

		System.out.println("");
		System.out.println("--------------------------- END OF XRLEXAMPLE7 -------------------------------");
		System.out.println("");
		System.exit(0);
	}
}*/