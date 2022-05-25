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
public class StereographicProjection implements SphericalProjection {

    @Override
    public Vec2 project2(Vec2 thetarho) {
        double theta[] = thetarho.x();
        double r[] = new double[theta.length];
        for (int i = 0; i < theta.length; i++) {
            r[i] = Math.tan(theta[i] / 2);
        }
        Array1D rho = thetarho.getY();

        Vec2 p = new Vec2();
        p.setX(rho.cos().multiplyd(r));
        p.setY(rho.sin().multiplyd(r));
        return p;
    }

    @Override
    public Vec2 project(Vec3 vec) {
        Array1D rho = vec.getRho();
        Array1D r = vec.getTheta().divided(2).tan();

        Vec2 p = new Vec2();
        p.setX(rho.cos().multiplyd(r));
        p.setY(rho.sin().multiplyd(r));
        return p;
    }

    @Override
    public Vec3 projectInv(Vec2 vec) {
        double[] r = vec.norm().toDoubleArray();
        Array1D rho = vec.getY().atan2(vec.getX());

        double[] theta = new double[vec.size()];
        for (int i = 0; i < vec.size(); i++) {
            theta[i] = Math.atan(r[i]) * 2;
        }
        return new Vec3(new Array1D(theta), rho);
    }

    @Override
    public Vec2 projectInv2(Vec2 vec) {
        double[] r = vec.norm().toDoubleArray();
        double[] theta = new double[vec.size()];
        for (int i = 0; i < vec.size(); i++) {
            theta[i] = Math.atan(r[i]) * 2;
        }
        Vec2 ip = new Vec2();
        ip.setX(new Array1D(theta));
        ip.setY(vec.getY().atan2(vec.getX()));
        return ip;
    }

}
