package it.unitn.ing.rista.util;

import java.util.StringTokenizer;
import java.util.Vector;

/**
 * Created by luca on 09/05/15.
 */
public class Symop {

//	public Symop() {}
	public Symop(int value) {number = value;}

	public Symop(int value, String stringvalue) {
		this(value);
		setSymopFromString(stringvalue);
	}

	public int number;
	public Vector<String> coordinates = null;

	public void setSymopFromString(String value) {
		StringTokenizer stoken = new StringTokenizer(value, ",;");
		coordinates = new Vector<String>(3);
		while (stoken.hasMoreTokens())
			coordinates.addElement(stoken.nextToken());
	}

	public String getSymopAsString() {
		String theString = "";
		int i;
		for (i = 0; i < coordinates.size() - 1; i++)
			theString.concat(coordinates.elementAt(i).concat(", "));
		theString.concat(coordinates.elementAt(i));
		return theString;
	}
}
