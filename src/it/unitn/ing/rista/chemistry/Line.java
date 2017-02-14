package it.unitn.ing.rista.chemistry;

import java.util.Vector;
//import javax.swing.*;
//import java.io.*;
//import java.lang.*;
//import java.*;
//import javax.*;
import java.util.*;
//import java.lang.String;


public class Line {
  String lineSymbol1;
  String lineSymbol2;
  double[] valueLine = new double[2];

  public Line() {
  }

  public Line(String lineSymbol) {
    this();
    setLineSymbol1(lineSymbol);
  }

  public Line(String lineSymbol1, String lineSymbol2, String value1, String value2) {
    this(lineSymbol1);
    setLineSymbol2(lineSymbol2);
    setValueLine(0, value1);
    setValueLine(1, value2);
  }

  //get
  public String getLineSymbol1() {
    return lineSymbol1;
  }

  public String getLineSymbol2() {
    return lineSymbol2;
  }

  public double getValueLine(int index) {
    return valueLine[index];
  }

  public int getValueLineCount() {
    return valueLine.length;
  }

  //set
  public void setLineSymbol1(String lineSymbol) {
    this.lineSymbol1 = lineSymbol;
  }

  public void setLineSymbol2(String lineSymbol) {
    this.lineSymbol2 = lineSymbol;
  }

  //settaggio valori della line
  //bisogna decidere se passa solo i valori o tutta la striga letta
  public void setValueLine(int index, String stri) { //passaggio della stringa solo letta
    valueLine[index] = Double.parseDouble(stri);
  }

  public double getEnergy() {
    return getValueLine(0);
  }

  public double getIntensity() {
    return getValueLine(1);
  }

}
