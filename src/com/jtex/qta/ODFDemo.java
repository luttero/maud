/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.qta;

import com.jtex.qta.odf.UnimodalComponent;
import com.jtex.qta.odf.UniformComponent;
import com.jtex.arrays.Array1D;
import com.jtex.geom.Miller;
import com.jtex.geom.Quaternion;
import com.jtex.geom.Symmetry;
import com.jtex.geom.Vec3;
import com.jtex.geom.grid.S2Grid;
import com.jtex.plot.Plotter;
import com.jtex.qta.file.BrukerGpolLoader;
import com.jtex.qta.file.DubnaLoader;
import com.jtex.qta.kernel.VonMisesFisher;

/**
 *
 * @author hios
 */
public class ODFDemo {

    public static ODF SantaFe() {

        Symmetry cs = new Symmetry("cubic", false);
        Symmetry ss = new Symmetry("-1", false);

        ODF odf = new ODF(cs, ss, null);
        UniformComponent uniformCmp = new UniformComponent(cs, ss);
        uniformCmp.setPortion(.73);
        odf.add(uniformCmp);

        Quaternion g0 = Quaternion.rotZXZ(Math.toRadians(26.5651), Math.toRadians(48.1897), Math.toRadians(26.5651));
        UnimodalComponent unimodalCmp = new UnimodalComponent(g0, new Array1D(new double[]{1}), new VonMisesFisher(Math.toRadians(10)), cs, ss);
        unimodalCmp.setPortion(.27);
        odf.add(unimodalCmp);

        return odf;
    }

    public static PoleFigure SantaFePF() {

        ODF SantaFe = ODFDemo.SantaFe();

        Miller h = Miller.concat(
                new Miller(0, 0, 1, SantaFe.cs),
                new Miller(0, 1, 1, SantaFe.cs),
                new Miller(1, 1, 1, SantaFe.cs),
                new Miller(2, 1, 1, SantaFe.cs));
        Vec3 r = S2Grid.equispacedS2Grid(Math.toRadians(5), 0, Math.PI, 0, 2 * Math.PI, true).toVec3();
        PoleFigure pf = SantaFe.calcPoleFigure(h, r);
        return pf;
    }

    public static void SantaFeDemo() {

        ODF SantaFe = ODFDemo.SantaFe();

        Miller h = Miller.concat(
                new Miller(0, 0, 1, SantaFe.cs),
                new Miller(0, 1, 1, SantaFe.cs),
                new Miller(1, 1, 1, SantaFe.cs),
                new Miller(2, 1, 1, SantaFe.cs));
        Plotter.show(Plotter.plotpdf(SantaFe, h));

//
        Vec3 r = S2Grid.equispacedS2Grid(Math.toRadians(5), 0, Math.PI, 0, 2 * Math.PI, true).toVec3();
        PoleFigure pf = SantaFe.calcPoleFigure(h, r);

        Array1D data = pf.getData();
        pf.setData(data.multiply(100));

	    System.out.println("Checking crystal symmetry in MTEX: " + pf.getCS().getGroup().toString());
	    System.out.println("Checking sample symmetry in MTEX: " + pf.getSS().getGroup().toString());
	    Plotter.show(Plotter.plot(pf));
        ODF rec = new ODF();
	     rec = rec.estimate(pf, 5);
	    System.out.println("SanteFe ODF1 components number: " + rec.componentsNumber());

//        ODFOptions odfOptions = new ODFOptions(pf, Math.toRadians(5), new VonMisesFisher(Math.toRadians(5)));
//        odfOptions.setGhostCorrection(false);

//        ODF rec2 = rec.estimate(pf, odfOptions);
//	    System.out.println("SanteFe ODF2 components number: " + rec2.componentsNumber());

//        Plotter.show(Plotter.plotpdf(rec2, h));
        Plotter.show(Plotter.plotpdf(rec, h));

 //       SantaFe.powerSpectrum().print();
 //       rec.powerSpectrum().print();
 //       rec2.powerSpectrum().print();

//        Plotter.show(Plotter.plotphi2(SantaFe));
//        Plotter.show(Plotter.plotphi2(rec2));

//        Plotter.show(Plotter.plotphi2(rec));

    }

    public static void BrukerGPolDemo() {

        PoleFigure pf = BrukerGpolLoader.loadExample();

        ODF odf = new ODF();
        odf = odf.estimate(pf, 4);
	     System.out.println("BrukerGPol ODF components number: " + odf.componentsNumber());
        Plotter.show(Plotter.plot(pf));

        Plotter.show(Plotter.plotpdf(odf, pf.getH()));

        Plotter.show(Plotter.plotphi2(odf));
        Plotter.show(Plotter.plotphi2full(odf));

        Plotter.show(Plotter.plotphi1(odf));
        Plotter.show(Plotter.plotsigma(odf));
    }

    public static void DubnaDemo() {

        PoleFigure pf = DubnaLoader.loadExample();

        Plotter.show(Plotter.plot(pf));

        ODF odf = new ODF();
        odf = odf.estimate(pf);

	     System.out.println("Dubna ODF components number: " + odf.componentsNumber());

        Plotter.show(Plotter.plotpdf(odf, pf.getH()));

        Plotter.show(Plotter.plotDiff(odf, pf));

    }

    public static void main(String[] args) {

        DubnaDemo();

    }

}
