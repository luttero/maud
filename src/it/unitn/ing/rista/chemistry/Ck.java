package it.unitn.ing.rista.chemistry;


public class Ck
{
    String ckSymbol;
    double valueCk;

    public Ck() {
    }

  public Ck(String totalCkSymbol) {
    this();
     setCkSymbol(totalCkSymbol);
 }

  public Ck(String totalCkSymbol, double valueTotalCk) {
     this(totalCkSymbol);
     setValueCk(valueTotalCk);
 }

//get
    public String getCkSymbol(){
        return ckSymbol;    
    }
    public double getValueCk(){
        return valueCk;
    }    
//set
    public void setCkSymbol(String ckSymbol){
        this.ckSymbol=ckSymbol;
    } 
    
//settaggio valori della line 
    //bisogna decidere se passa solo i valori o tutta la striga letta  
    public void setValueCk(double value){
        this.valueCk=value;
    }
    
    
}