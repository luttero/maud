package it.unitn.ing.rista.chemistry;

import java.util.*;

public class FluorescenceElement {
  String symbol;
  int atomicNumber;
  double atomicWeight;
  double density;
  Vector<FluorescenceEdge> edgeLinesGroup;
  Vector<double[]> photoAbsorption;
  Vector<double[]> scatter;

  public FluorescenceElement() {
    edgeLinesGroup = new Vector<FluorescenceEdge>(0, 1);
    photoAbsorption = new Vector<double[]>(0, 10);
    scatter = new Vector<double[]>(0, 10);
  }

  public FluorescenceElement(String symbol) {
    this();
    setSymbol(symbol);
  }

  //get
  public String getSymbol() {
    return symbol;
  }

  public int getAtomicNumber() {
    return atomicNumber;
  }

  public double getAtomicWeight() {
    return atomicWeight;
  }

  public double getDensity() {
    return density;
  }

  public Vector<FluorescenceEdge> getEdgeLinesGroup() {
    return edgeLinesGroup;
  }

  public int getEdgeLinesGroupCount() {
    return edgeLinesGroup.size();
  }

  public FluorescenceEdge getEdgeLinesGroup(int index) {
    return edgeLinesGroup.elementAt(index);
  }

  public Vector<double[]> getPhotoAbsorption() {
    return photoAbsorption;
  }

  public int getPhotoAbsorptionCount() {
    return photoAbsorption.size();
  }

  public double[] getPhotoElement(int index) {
    return photoAbsorption.elementAt(index);
  }

  public Vector<double[]> getScatter() {
    return scatter;
  }

  public int getScatterCount() {
    return scatter.size();
  }

  public double[] getScatterElement(int index) {
    return scatter.elementAt(index);
  }

  //set
  public void setSymbol(String symbol) {
    this.symbol = symbol;
  }

  public void setAtomicNumber(int atomicNumber) {
    this.atomicNumber = atomicNumber;
  }

  public void setAtomicWeight(double atomicWeight) {
    this.atomicWeight = atomicWeight;
  }

  public void setDensity(double density) {
    this.density = density;
  }

  public void addEdgeLinesGroup(FluorescenceEdge group) {
    edgeLinesGroup.addElement(group);
  }

  public void addPhoto(double[] group) {
    photoAbsorption.addElement(group);
  }

  public void addScatter(double[] group) {
    scatter.addElement(group);
  }


}



    
    
    

