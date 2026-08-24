package org.diffax;

public class DiffaxAtom {
  public String name = "C   ";
  public double x = 0;
  public double y = 0;
  public double z = 0;
  public double bIso = 0;
  public double occupancy = 1;

  public DiffaxAtom(String label, double x, double y, double z, double bIso, double occupancy) {
    try {
      setAtomLabel(label);
      setParameters(x, y, z, bIso, occupancy);
    }  catch (Exception e) {
      e.printStackTrace();
    }
  }

  public void setAtomLabel(String label) throws Exception {
    if (label == null)
      throw new NullPointerException("label");
    if (label.length() < 1)
      throw new IllegalArgumentException("Not a valid atom label");
    name = label;
    while (name.length() < 4)
      name = name + " ";
    if (name.length() > 4)
      name = name.substring(0, 4);
  }

  public void setParameters(double x, double y, double z, double bIso, double occupancy) {
    this.x = x;
    this.y = y;
    this.z = z;
    this.bIso = bIso;
    this.occupancy = occupancy;
  }
}
