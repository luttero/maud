/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.qta.file;

import com.jtex.arrays.Array1D;
import com.jtex.geom.Miller;
import com.jtex.geom.Symmetry;
import com.jtex.geom.Vec3;
import com.jtex.plot.Plotter;
import com.jtex.plot.SimpleMultiPlotComponent;
import com.jtex.qta.PoleFigure;
import it.unitn.ing.rista.util.Constants;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author flb
 */
public class BrukerGpolLoader implements PoleFigureLoader {
    
    public static void main(String[] args) {
        
        PoleFigure pf = loadExample();
        
        SimpleMultiPlotComponent plot = Plotter.plot(pf);
        
        Plotter.show(plot);
    }
    
    public PoleFigure load(InputStream input) throws IOException {

        BufferedInputStream in = new BufferedInputStream(input);
        int nrows = 0;
        int ncols = 0;
        int nover = 0;
        int headersz = 96;
        int byt = 1;
        byte[] buf = new byte[80];

        int cline = 0;
        while (cline < headersz) {
            in.read(buf);
            String line = new String(buf);
            if (line.startsWith("HDRBLKS")) {
                headersz = 512 * Integer.valueOf(line.substring(9).trim()) / 80;
            } else if (line.startsWith("NROWS")) {
                nrows = Integer.valueOf(line.substring(9).trim());
            } else if (line.startsWith("NCOLS")) {
                ncols = Integer.valueOf(line.substring(9).trim());
            } else if (line.startsWith("NPIXELB")) {
                byt = Integer.valueOf(line.substring(9).trim());
            } else if (line.startsWith("NOVERFL")) {
                nover = Integer.valueOf(line.substring(9).trim());
            }
            cline++;
        }
        byte[] datab = new byte[nrows * ncols * byt];
        in.read(datab, 0, datab.length);

        int ch1, ch2, ch3, ch4;
        int data[] = new int[nrows * ncols];
        if (byt == 1) {
            for (int i = 0; i < nrows * ncols; i++) {
                data[i] = datab[i] & 0xff;
            }
        } else if (byt == 2) {
            for (int i = 0; i < nrows * ncols; i++) {
                ch1 = datab[2 * i] & 0xff;
                ch2 = datab[2 * i + 1] & 0xff;
                data[i] = (ch2 << 8) + (ch1);
            }
        } else if (byt == 4) {
            for (int i = 0; i < nrows * ncols; i++) {
                ch1 = datab[2 * i] & 0xff;
                ch2 = datab[2 * i + 1] & 0xff;
                ch3 = datab[2 * i + 2] & 0xff;
                ch4 = datab[2 * i + 3] & 0xff;
                data[i] = (ch4 << 32) + (ch3 << 16) + (ch2 << 8) + (ch1);
            }
        }
        byte[] bcount = new byte[9];
        byte[] bindex = new byte[7];
        for (int i = 0; i < nover; i++) {
            in.read(bcount, 0, 9);
            in.read(bindex, 0, 7);
            int count = Integer.parseInt(new String(bcount).trim());
            int index = Integer.parseInt(new String(bindex).trim());
            data[index] = count;
        }

        Array1D d = new Array1D(data);
        Array1D ndx = new Array1D(d.g(0).find());
        d = d.get(ndx.toIntArray());

        Array1D iy = ndx.mod(nrows);
        Array1D ix = (ndx.minus(iy).plusd(1)).divided(nrows);

        ix.minusd(nrows / 2 - .5);
        iy.minusd(ncols / 2 - .5);

        Array1D rho = ix.atan2(iy).multiplyd(-1);
        Array1D theta = (ix.squared().plusd(iy.squared()).sqrt().divided(nrows / 2 - 1)).atan().multiplyd(2);

		  double[] r1 = rho.toDoubleArray();
		  double[] t1 = theta.toDoubleArray();
		  for (int i = 0; i < r1.length && i < t1.length; i++) {
			  System.out.println((r1[i] * Constants.PITODEG) + " " + (t1[i] * Constants.PITODEG));
		  }

        Vec3 r = new Vec3(theta, rho);

        return new PoleFigure(new Miller(0, 0, 1, new Symmetry()), r, d);
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

        Symmetry cs = new Symmetry("m-3m", 3.54);

//        Miller h = Miller.concat(new Miller(1, 1, 1, cs), new Miller(2, 0, 0, cs), new Miller(2, 2, 0, cs), new Miller(3, 1, 1, cs));
//        String names[] = new String[]{"P62h_grob_111.gpol", "P62h_grob_200.gpol", "P62h_grob_220.gpol", "P62h_grob_311.gpol"};
//        String names[] = new String[]{"Al_111.gpol", "Al_200.gpol", "Al_220.gpol", "Al_311.gpol"};
        String names[] = new String[]{"Wieland_42_111.gpol", "Wieland_42_200.gpol", "Wieland_42_220.gpol", "Wieland_42_311.gpol"};

        PoleFigure pf = new PoleFigure();
        pf.setCS(cs);
        for (int i = 0; i < names.length; i++) {
            try {
                InputStream in = BrukerGpolLoader.class.getResourceAsStream("/com/jtex/data/polefigure/gpol/" + names[i]);

                PoleFigure ps = new BrukerGpolLoader().load(in);
                in.close();
                ps.setCS(cs);
                ps.setH(Miller.fromString(names[i], cs));
                pf.add(ps);
            } catch (IOException ex) {
                Logger.getLogger(BrukerGpolLoader.class.getName()).log(Level.SEVERE, null, ex);
            }

        }
        return pf;

    }

}
