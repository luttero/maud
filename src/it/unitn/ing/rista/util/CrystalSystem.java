package it.unitn.ing.rista.util;

import java.util.Vector;

/**
 * Created by luca on 09/05/15.
 */
public class CrystalSystem {

	public CrystalSystem() {}

	public CrystalSystem(String name) {symmetry = name;}

	public String symmetry;

	public Vector<Spacegroup> space_groups = new Vector<Spacegroup>();

	public void addSpaceGroupsObject(Spacegroup value) {
//		System.out.println(symmetry + ": adding: " + value.hermann_mauguin);
		space_groups.addElement(value);
	}

}
