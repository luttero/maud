/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.qta.kernel;

import java.util.Formatter;
import java.util.Locale;

/**
 *
 * @author flb
 */
public abstract class Kernel implements KernelK {


    /* public Kernel() {

     double hw = Math.toRadians(5);
     double p = 0.5 * Math.log(0.5) / Math.log(Math.cos(hw / 2));

     int L = Math.min(1000, Math.max(10, (int) p + 1));

     System.out.println(L);

     Array1D A = Array1D.fill(L, 1.0);
     A.set(1, p / (p + 2));

     for (int i = 0; i < L - 2; i++) {
     double l = i + 1;
     A.set(i + 2, ((p - l + 1) * A.get(i) - (2 * l + 1) * A.get(i + 1)) / (p + l + 2));
     }

     for (int i = 0; i < L; i++) {
     double l = (double) i;
     A.set(i, (2 * l + 1) * A.get(i));
     }

     System.out.println(A.get(A.ge(1e-3).find()));

     System.out.println(A);

     double sqrtpi = Math.sqrt(Math.PI);
     //        sqrt(pi).*p.^(3/2) + 9.*sqrt(pi).*sqrt(p)./8 + 17./128.*sqrt(pi).*sqrt(1./p)

     double C = sqrtpi * Math.pow(p, 3.0 / 2.0) + 9 * sqrtpi * Math.sqrt(p) / 8 + 17 / 128 * sqrtpi * Math.sqrt(1 / p);

     double co2 = 1;
     double K = C * Math.pow(co2, 2 * p);

     //            RK  = @(dmatrix) (1+p) * ((1+dmatrix)/2).^p;
     double dmatrix = 0;
     double RK = (1 + p) * Math.pow((1 + dmatrix) / 2, p);

     // approx
     //        (Math.PI / 2) //          C = beta(1.5,0.5)/beta(1.5,p+0.5);
     //    K   = @(co2)  C * co2.^(2*p);
     }

     //    public Array1D evalK(quat g1, quat g2, Symmetry CS, Symmetry SS) {
     //    }
     //
     //    public Array1D evalRK(quat g, vec3 h, vec3 r, Symmetry CS, Symmetry SS) {
     //    }
     public static void main(String[] args) {

     //        Kernel k = new Kernel();
     KernelK kern = new DeLaValleePoussin(Math.toRadians(5));

     System.out.println(kern.K(Array1D.linspace(1, 0, 1000000)));
     System.out.println(kern.RK(Array1D.linspace(1, 0, 1000000)));

     }*/
    @Override
    public String toString() {
        return getClass().getName() + "[" + paramString() + "]";
    }

    protected String paramString() {
        Formatter formatter = new Formatter(Locale.US);

        String str = "hw=" + formatter.format("%.1f°", Math.toDegrees(p2hw())).toString();
//        str += ",data=[" + formatter.toString() + "]";
        return str;
    }

    public abstract int bandwidth();
}
