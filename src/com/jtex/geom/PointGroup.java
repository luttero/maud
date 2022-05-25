/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.geom;

import java.util.EnumSet;
import java.util.Iterator;
import java.util.TreeSet;

/**
 *
 * @author hios
 */
public enum PointGroup {

    C1("triclinic", "C1", "1", "-1", "1"),
    Ci("triclinic", "Ci", "-1", "-1", "1"),
    C2("monoclinic", "C2", "2", "2/m", "2"),
    Cs("monoclinic", "Cs", "m", "2/m", "2"),
    C2h("monoclinic", "C2h", "2/m", "2/m", "2"),
    D2("orthorhombic", "D2", "222", "mmm", "222"),
    C2v("orthorhombic", "C2v", "mm2", "mmm", "222"),
    D2h("orthorhombic", "D2h", "mmm", "mmm", "222"),
    C4("tetragonal", "C4", "4", "4/m", "4"),
    S4("tetragonal", "S4", "-4", "4/m", "4"),
    C4h("tetragonal", "C4h", "4/m", "4/m", "4"),
    D4("tetragonal", "D4", "422", "4/mmm", "422"),
    C4v("tetragonal", "C4v", "4mm", "4/mmm", "422"),
    D2d("tetragonal", "D2d", "-42m", "4/mmm", "422"),
    D4h("tetragonal", "D4h", "4/mmm", "4/mmm", "422"),
    C3("trigonal", "C3", "3", "-3", "3"),
    C3i("trigonal", "C3i", "-3", "-3", "3"),
    D3("trigonal", "D3", "32", "-3m", "32"),
    C3v("trigonal", "C3v", "3m", "-3m", "32"),
    D3d("trigonal", "D3d", "-3m", "-3m", "32"),
    C6("hexagonal", "C6", "6", "6/m", "6"),
    C3h("hexagonal", "C3h", "-6", "6/m", "6"),
    C6h("hexagonal", "C6h", "6/m", "6/m", "6"),
    D6("hexagonal", "D6", "622", "6/mmm", "622"),
    C6v("hexagonal", "C6v", "6mm", "6/mmm", "622"),
    D3h("hexagonal", "D3h", "-6m2", "6/mmm", "622"),
    D6h("hexagonal", "D6h", "6/mmm", "6/mmm", "622"),
    T("cubic", "T", "23", "m-3", "23"),
    Th("cubic", "Th", "m-3", "m-3", "23"),
    O("cubic", "O", "432", "m-3m", "432"),
    Td("cubic", "Td", "-43m", "m-3m", "432"),
    Oh("cubic", "Oh", "m-3m", "m-3m", "432"),
    Fi("fiber", "C72h", "72/m", "72/m", "72"),
	 Fib("fiber", "D72h", "72/mmm", "72/mmm", "7222");

    String system;
    String schoenflies;
    String international;
    String laue;
    String rotaxis;

    private PointGroup(String system, String schoenflies, String international, String laue, String rotaxis) {
        this.system = system;
        this.schoenflies = schoenflies;
        this.international = international;
        this.laue = laue;
        this.rotaxis = rotaxis;
    }

    public static PointGroup get(String sym) {
        TreeSet<PointGroup> schoen = new TreeSet<PointGroup>(EnumSet.allOf(PointGroup.class));
        Iterator<PointGroup> it = schoen.descendingIterator();
        while (it.hasNext()) {
            PointGroup next = it.next();
            if (next.is(sym)) {
                return next;
            }
        }
        return null;
    }

    public String getSystem() {
        return system;
    }

    public String getInternational() {
        return international;
    }

    public String getLaue() {
        return laue;
    }

    public String getSchoenflies() {
        return schoenflies;
    }

    public boolean is(String... group) {
        for (String g : group) {
            g = g.replace("/", "");
            if (international.replace("/", "").equals(g)
                    || laue.replace("/", "").equals(g)
                    || rotaxis.equals(g)
                    || schoenflies.equals(g)
                    || system.equals(g)) {
                return true;
            }
        }
        return false;
    }

    public boolean isLaue(String... group) {
        for (String g : group) {
            if (laue.equals(g)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public String toString() {
        return laue + " (" + schoenflies + ")";
    }

}
