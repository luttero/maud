/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.qta.file;

import com.jtex.qta.PoleFigure;
import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 *
 * @author flb
 */
public interface PoleFigureLoader {

    public PoleFigure load(File datafiles) throws IOException;

    public PoleFigure load(List<File> datafiles) throws IOException;

//
//    public PoleFigure load(Symmetry cs, Miller h, File datafiles) throws IOException;
//
//    public PoleFigure load(Symmetry cs, List<Miller> h, List<File> datafiles) throws IOException;
//    public PoleFigure load(InputStream in) throws IOException;
}
