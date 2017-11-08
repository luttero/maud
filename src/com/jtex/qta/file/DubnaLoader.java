/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.qta.file;

import com.jtex.arrays.Array1D;
import com.jtex.external.Native;
import com.jtex.geom.Miller;
import com.jtex.geom.Symmetry;
import com.jtex.geom.Vec3;
import com.jtex.plot.ColorMap;
import com.jtex.plot.Plotter;
import com.jtex.plot.Shapes;
import com.jtex.plot.SimpleMultiPlotComponent;
import com.jtex.qta.ODF;
import com.jtex.qta.PoleFigure;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JComponent;

/**
 *
 * @author hios
 */
public class DubnaLoader implements PoleFigureLoader {
    
    public static void main(String[] args) {
        
        PoleFigure pf = loadExample();
        
        SimpleMultiPlotComponent plot = Plotter.plot(pf);
        plot.getScatterOptions().setMarker(Shapes.Named.CROSS);
        plot.getScatterOptions().setMarkerSize(1.7);
//        plot.setColorRangeEqual(true);
        Plotter.show(plot);
        
        ODF.ODFOptions opts = new ODF.ODFOptions(pf.getCS(), Math.toRadians(5), Math.toRadians(3.5));
        ODF odf = ODF.estimate(pf, opts);
        
        SimpleMultiPlotComponent plot1 = Plotter.plotpdf(odf,pf.getH());
        Plotter.show(plot1);
        plot1.setColorMap(ColorMap.getColormap(ColorMap.Name.LABOTEX, 200));
//        plot1.setc
    }
    
    public static Vec3 DubnaGrid(int ntheta) {
        
        int[] ndxtheta = Array1D.fill(0, ntheta - 1, 1).toIntArray();
        Array1D theta = Array1D.linspace(0, Math.PI / 2, 19);
        theta = theta.get(ndxtheta);
        theta = theta.expand(72);
        
        Array1D rho = Array1D.linspace(0, 2 * Math.PI, 72);
        rho = rho.repeat(ntheta);
        Array1D rhostart = new Array1D(360.00, 336.40, 327.05, 320.11, 314.44, 309.57, 305.26, 301.37, 297.80,
                294.47, 291.34, 288.38, 285.54, 282.81, 280.16, 277.57, 275.02, 272.50, 270.00).multiplyd(Math.PI / 180).reverse();
        
        rho.plusd(rhostart.get(ndxtheta).expand(72));
        
        return new Vec3(theta, rho);
        
    }
    
    public PoleFigure load(InputStream in) {
        System.out.println(in);
        Scanner s = new Scanner(in);
        
        List<Double> d = new ArrayList<Double>();
        while (s.hasNext()) {
            d.add(Double.parseDouble(s.next()));
        }
        double[] data = new double[d.size()];
        for (int i = 0; i < data.length; i++) {
            data[i] = d.get(i);
        }
        
        return new PoleFigure(new Miller(), DubnaGrid(data.length / 72), new Array1D(data));

//        System.out.println(":" + );
//        System.out.println(in);
//        return null;
    }
    
    @Override
    public PoleFigure load(File datafiles) throws IOException {
        return load(Arrays.asList(new File[]{datafiles}));
    }
    
    @Override
    public PoleFigure load(List<File> datafiles) throws IOException {
        PoleFigure pf = new PoleFigure();
        for (File datafile : datafiles) {
            if (datafile.exists()) {
                FileInputStream fis = new FileInputStream(datafile);
                PoleFigure npf = load(fis);
                fis.close();
                npf.setH(0, Miller.fromString(datafile.getName(), new Symmetry()));
                pf.add(npf);
            }
        }
        return pf;
    }
    
    public static PoleFigure loadExample() {
        
        Symmetry cs = new Symmetry("-3m", 1.4, 1.4, 1.5);
        
        System.out.println(cs.euler("ZXZ").toDegrees());
        String names[] = new String[]{"Q(02-21)_amp.cnv", "Q(10-10)_amp.cnv",
            "Q(10-11)(01-11)_amp.cnv", "Q(10-12)_amp.cnv",
            "Q(11-20)_amp.cnv", "Q(11-21)_amp.cnv", "Q(11-22)_amp.cnv"};
        
        PoleFigure pf = new PoleFigure();
        pf.setCS(cs);
        for (int i = 0; i < names.length; i++) {
            try {
                InputStream in = DubnaLoader.class.getResourceAsStream("/com/jtex/data/polefigure/dubna/" + names[i]);
                
                PoleFigure ps = new DubnaLoader().load(in);
                in.close();
                ps.setCS(cs);
                ps.setH(Miller.fromString(names[i], cs));
                pf.add(ps);
            } catch (IOException ex) {
                Logger.getLogger(BrukerGpolLoader.class.getName()).log(Level.SEVERE, null, ex);
            }
            
        }
        pf.setC(2, new Array1D(0.52, 1.23));
        
        return pf;
        
    }
    
}
