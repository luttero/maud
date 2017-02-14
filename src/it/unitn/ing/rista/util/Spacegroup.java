package it.unitn.ing.rista.util;

import java.util.Vector;

/**
 * Created by luca on 09/05/15.
 */

public class Spacegroup {

	public String hermann_mauguin;
	public int number;
	public String hall;
	public String schoenflies;
	public String centering_type;
	public String symmetry;
	public boolean centrosymmetric;
	public int laueGroup;
	public int pointGroup;

	public Vector<Symop> symops = new Vector<Symop>();

	public Spacegroup() {
	}

	public Spacegroup(int n, String sf, String hm, String h, String c, String sym, boolean cs) {
		number = n;
		schoenflies = sf;
		hermann_mauguin = hm;
		hall = h;
		centering_type = c;
		symmetry = sym;
		centrosymmetric = cs;
	}

	public void setHermann_mauguin(String hermann_mauguinSymbol) {
		hermann_mauguin = hermann_mauguinSymbol;
	}
	public void setCentering_type(String value) {centering_type = value;}
	public void setSymmetry(String value) {symmetry = value;}
	public void setNumber(int n) {number = n;}
	public void setHall(String hallSymbol) {hall = hallSymbol;}
	public void setSchoenflies(String schoenfliesSymbol) {
		schoenflies = schoenfliesSymbol;
	}

	public void addSymops(String nsymop) {
		int next_number = symops.size() + 1;
		Symop newsymop = new Symop(next_number);
		newsymop.setSymopFromString(nsymop);
		symops.addElement(newsymop);
	}
}
