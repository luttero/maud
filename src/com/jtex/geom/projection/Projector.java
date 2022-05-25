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
public interface Projector {

    public Vec2 project2(Vec2 thetarho);

//    public vec2 project(Array1D theta, Array1D rho, double scale);
    public Vec2 project(Vec3 vec);

//    public vec3 projectInv(double x, double y, double scale);
//    public vec3 projectInv(Array1D x, Array1D y, double scale);
    public Vec3 projectInv(Vec2 vec);

    public Vec2 projectInv2(Vec2 vec);

}
