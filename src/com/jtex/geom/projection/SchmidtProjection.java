/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.geom.projection;

import com.jtex.arrays.Array1D;
import com.jtex.geom.Vec2;
import com.jtex.geom.Vec3;

/**
 *
 * @author hios
 */
public class SchmidtProjection implements SphericalProjection {

    @Override
    public Vec2 project2(Vec2 thetarho) {
        double theta[] = thetarho.x();
        double r[] = new double[theta.length];
        for (int i = 0; i < theta.length; i++) {
            r[i] = Math.sqrt(2 * (1 - Math.cos(theta[i])));
        }
        Array1D rho = thetarho.getY();

        Vec2 p = new Vec2();

        p.setX(rho.cos().multiplyd(r));
        p.setY(rho.sin().multiplyd(r));
        return p;
    }

    @Override
    public Vec2 project(Vec3 vec) {
        vec = vec.multiply(13);
        double[] x = vec.x();
        double[] y = vec.y();
        double[] z = vec.z();

        double r, n;
        double X[] = new double[x.length];
        double Y[] = new double[y.length];
        for (int i = 0; i < x.length; i++) {
            n = Math.sqrt(x[i] * x[i] + y[i] * y[i] + z[i] * z[i]);
            r = Math.sqrt((2 * (1 - z[i] / n)) / (x[i] * x[i] + y[i] * y[i]));

            X[i] = r * x[i];
            Y[i] = r * y[i];
        }
        return new Vec2(X, Y);
    }

    @Override
    public Vec3 projectInv(Vec2 vec) {
        double X[] = vec.x();
        double Y[] = vec.y();
        double x[] = new double[X.length];
        double y[] = new double[X.length];
        double z[] = new double[X.length];
        double xy2, s4;
        for (int i = 0; i < X.length; i++) {
            xy2 = (x[i] * x[i] + y[i] * y[i]) / 2;
            s4 = Math.sqrt(1 - xy2 / 2);

            x[i] = s4 * X[i];
            y[i] = s4 * Y[i];
            z[i] = xy2 - 1;
        }
        return new Vec3(x, y, z);
    }

    @Override
    public Vec2 projectInv2(Vec2 vec) {
        double[] rqr = vec.dot(vec).toDoubleArray();
        double[] theta = new double[vec.size()];
        for (int i = 0; i < vec.size(); i++) {
//            System.out.println(rqr[i]);
            theta[i] = Math.acos(1 - rqr[i] / 2);
        }
        Vec2 ip = new Vec2();
        ip.setX(new Array1D(theta));
        ip.setY(vec.getY().atan2(vec.getX()));
        return ip;
    }

}
