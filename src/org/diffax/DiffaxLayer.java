package org.diffax;

import java.util.Vector;

public class DiffaxLayer {

  public boolean centroSymmetric = false;
  public DiffaxLayer equalTo = null;
  public Vector<DiffaxAtom> atoms = null;

  public DiffaxLayer(boolean centroSymmetric, DiffaxLayer equalTo) {
    this.centroSymmetric = centroSymmetric;
    this.equalTo = equalTo;
  }

  public DiffaxLayer(boolean centroSymmetric) {
    this.centroSymmetric = centroSymmetric;
    this.equalTo = null;
  }

  public DiffaxLayer() {
    this.centroSymmetric = false;
    this.equalTo = null;
  }

  public void addAtom(DiffaxAtom atom) {
    if (atoms == null)
      atoms = new Vector<>();
    atoms.add(atom);
  }

  public void setAtomList(Vector<DiffaxAtom> atomList) {
    atoms = atomList;
  }

  public void removeAtom(DiffaxAtom atom) {
    if (atoms != null)
      atoms.remove(atom);
  }

  public void removeAllAtoms() {
    if (atoms != null)
      atoms.clear();
  }

  public void removeAtomAt(int index) {
    if (atoms != null && index >= 0 && index < atoms.size())
      atoms.remove(index);
  }

  public DiffaxAtom getAtom(int i) {
    if (atoms == null)
      return atoms.get(i);
    else
      return null;
  }

}
