package it.unitn.ing.rista.diffr.sfm;

import it.unitn.ing.rista.diffr.*;

public class ExtinctionModel extends XRDcat {
	public ExtinctionModel(XRDcat aobj, String alabel) {
		super(aobj, alabel);
	}

	public ExtinctionModel(XRDcat aobj) {
		this(aobj, "Extinction model none");
	}

	public ExtinctionModel() {
	}

	public void computeExtinction(Phase aphase) {
	}

	public void computeStructureFactors(Sample asample, DataFileSet adataset) {
	}

}
