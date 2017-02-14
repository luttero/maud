package it.unitn.ing.wizard.HIPPOWizard;

import it.unitn.ing.rista.util.MaudPreferences;
import it.unitn.ing.wizard.HIPPOWizard.HIPPOdata;

import java.io.*;
import java.util.ArrayList;
import java.util.StringTokenizer;

public class IPRFileReader {
	
	private BufferedReader input = null;

	public IPRFileReader(BufferedReader input) {
		this.input = input;
	}
	
	public void read(HIPPOdata data) throws IOException {
		double maxBankToleranceTheta = MaudPreferences.getDouble("hippoWizard.maxDelta2ThetaForBankGrouping", 2.0);
		String line;
		int lineCounter = 0;
		while ((line = input.readLine()) != null) {
			lineCounter++;
			if (line.length() > 12 && line.substring(6, 12).equals("BNKPAR")) {
				StringTokenizer tokenizer = new StringTokenizer(line);
				if (tokenizer.countTokens() < 4)
					throw(new IOException("Error while reading instrument file at line " + lineCounter));
				if (line.charAt(3) == ' ' || line.charAt(3) == '\t')
					tokenizer.nextToken();
				tokenizer.nextToken();
				tokenizer.nextToken();
				String theta2string = tokenizer.nextToken();
				double theta2 = Double.parseDouble(theta2string);
				int roundedForName = (int) (theta2 + 0.5);
				String nameAndTof_theta = "Bank" + roundedForName;
        boolean found = false;
        for (int i = 0; i < data.mbank.size(); i++) {
          if (Math.abs(((HIPPOBank) data.mbank.elementAt(i)).theta2 - theta2) < maxBankToleranceTheta) {
            ((HIPPOBank) data.mbank.elementAt(i)).available = true;
            found = true;
          }
        }
        if (!found) {
          String number = line.substring(3, 6);
          number = number.trim();
//	        System.out.println(number);
          data.mbank.add(new HIPPOBank(nameAndTof_theta, theta2, Integer.parseInt(number), true));
//	        System.out.println("Number of banks read: " + data.mbank.size() + " " + nameAndTof_theta + " " + number);
        }
      }

		}
		
	}

}
