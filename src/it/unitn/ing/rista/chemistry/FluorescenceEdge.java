package it.unitn.ing.rista.chemistry;

import java.util.Vector;

public class FluorescenceEdge
{
    String edgeSymbol;
    double energy;
    double fluorescenceYield;
    double jumpRatio;
    Vector<Line> line;
    Vector<Ck> ck;
    Vector<Ck> totalCk;
    
    public FluorescenceEdge() {
        line = new Vector<Line>(0, 1);
        ck = new Vector<Ck>(0, 1);
        totalCk= new Vector<Ck>(0, 1);
    }
    public FluorescenceEdge(String edgeSymbol) {
        this();
        setSymbol(edgeSymbol);
    }
    
//get    
    public String getEdgeSymbol(){
        return edgeSymbol;
    }
    public double getEnergy(){
        return energy;
    }
    public double getFluorescenceYield(){
        return fluorescenceYield;
    }
    public double getJumpRatio(){
        return jumpRatio;
    }
    public Line getLine(int index){
        return line.elementAt(index);
    }
    public Vector<Line> getLine(){
        return line;
    }
    public int getLineCount(){
        return line.size();
    }
    public Vector<Ck> getck(){
        return ck;
    }
    public int getCkCount(){
        return ck.size();
    }
    public Vector<Ck> getTotalCk(){
        return totalCk;
    }
    public int getTotalCkCount(){
        return totalCk.size();
    }
   
    
//set
    
    public void setSymbol(String edgeSymbol) {
        this.edgeSymbol=edgeSymbol;
    }
    public void setEnergy(double energy){
        this.energy=energy;
    }
    public void setFluorescenceYield(double fluorescenceYield){
        this.fluorescenceYield=fluorescenceYield;
    }
    public void setJumpRatio(double jumpRatio){
        this.jumpRatio=jumpRatio;
    }
    public void addLineGroup(Line group){
        line.addElement(group);
    }
    public void addCkGroup(Ck group){
        ck.addElement(group);
    }
    public void addTotalCkGroup(Ck group){
        totalCk.addElement(group);
    }
}
