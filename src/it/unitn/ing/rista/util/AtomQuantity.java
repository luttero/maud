package it.unitn.ing.rista.util;

import java.util.Vector;

public class AtomQuantity {
	public String label = "";
	public double quantity = 0;
	public double quantity_weight = 0;
	public double mass = 0;

	public AtomQuantity(String label, double mass, double quantity, double weight) {
		this.label = label;
		this.mass = mass;
		this.quantity = quantity;
		this.quantity_weight = weight;
	}

	public AtomQuantity(String label, double mass, double quantity) {
		this.label = label;
		this.mass = mass;
		this.quantity = quantity;
		this.quantity_weight = mass * quantity;
	}

	public int getPositionIn(Vector<AtomQuantity> composition) {
		for (int i = 0; i < composition.size(); i++)
			if (composition.elementAt(i).label.equalsIgnoreCase(label))
				return i;
		return -1;
	}


}
