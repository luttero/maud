/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.geom.projection;

import com.jtex.geom.Vec2;
import com.jtex.geom.Vec3;

/**
 *
 * @author hios
 */

//?????
public class PlainProjection implements Projector {

    @Override
    public Vec2 project2(Vec2 thetarho) {
        return thetarho.copy();
    }

    @Override
    public Vec2 project(Vec3 vec) {
        return new Vec2(vec.getRho().plusd(4*Math.PI).mod(2*Math.PI), vec.getTheta());
    }

    @Override
    public Vec3 projectInv(Vec2 vec) {
        return new Vec3(vec.x(), vec.y());
    }

    @Override
    public Vec2 projectInv2(Vec2 vec) {
        return  new Vec2(vec.y(), vec.x());
    }

}
