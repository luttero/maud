/*
 * @(#)HDFDataFile.java created Nov 15, 2011 Caen
 *
 * Copyright (c) 2011 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with the author.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */
package it.unitn.ing.rista.diffr.data;

import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.diffr.DiffrDataFile;
import it.unitn.ing.rista.util.Misc;
//import ncsa.hdf.object.*;     // include the common HDF object package
//import ncsa.hdf.object.h5.*;  // include the HDF5 object package
//import ncsa.hdf.hdf5lib.*;

import java.util.StringTokenizer;    // include the Java HDF5 interface

/**
 * The HDFDataFile is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Nov 15, 2011 8:29:36 PM $
 * @since JDK1.1
 */
public class HDFDataFile extends it.unitn.ing.rista.diffr.MultDiffrDataFile {

  public HDFDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".hdf";
  }

  public HDFDataFile() {
    identifier = ".hdf";
  }

  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;

    double chi = 0.0;
    double omega = 0.0;
    double wave = 1.0;
    double sampleDetectorDistance = 0;
    double temperature = 0;

    // create an H5File object
    String directory = getFolder(); //od.getDirectory();
    String name = getLabel(); //od.getFileName();
    System.out.println("Opening file: " + directory + name);

//    String filename = filterfilename(this.toXRDcatString());
/*    H5File h5file = new H5File(directory + name, HDF5Constants.H5F_ACC_RDONLY);

    try
    {
        // open file and retrieve the file structure
      h5file.open();

      javax.swing.tree.TreeNode rootNode = h5file.getRootNode();
      if (rootNode != null)
      {
        while (rootNode.getChildCount() == 1)
          rootNode = rootNode.getChildAt(0);
        Group root = (Group)((javax.swing.tree.DefaultMutableTreeNode)rootNode).getUserObject();
        if (root.getMemberList().size() > 1) {
          for (int i = 0; i < root.getMemberList().size(); i++) {
            Dataset dataset = (Dataset)root.getMemberList().get(i);

            if (dataset.getName().equals("DATA")) {
              String prefix = dataset.getFullName().substring(0, dataset.getFullName().length() - 4);
              Dataset parDataset = (Dataset)h5file.get(prefix + "PARAMETERS");
              String[] params = (String[]) parDataset.getData();

              String token = null;
              StringTokenizer st = new StringTokenizer(params[0], ":= \t\r\n");
              while (st.hasMoreTokens()) {
                token = st.nextToken();
                if (token.equals("Chi")) {
                  chi = Double.parseDouble(st.nextToken());
                } else if (token.equals("Omega")) {
                  omega = Double.parseDouble(st.nextToken());
                } else if (token.equals("Sample-Detector")) {
                  token = st.nextToken();
                  if (token.equals("Distance"))
                    sampleDetectorDistance = Double.parseDouble(st.nextToken());
                } else if (token.equals("Temperature")) {
                  temperature = Double.parseDouble(st.nextToken());
                  if (temperature > 9999.98)
                    temperature = 295;
                } else if (token.equals("Wave")) {
                  token = st.nextToken();
                  if (token.equals("lenght"))
                    sampleDetectorDistance = Double.parseDouble(st.nextToken());
                }

              }

              Dataset xDataset = (Dataset)h5file.get(prefix + "X");
              float[] xVector = (float[]) xDataset.getData();
              Dataset yDataset = (Dataset)h5file.get(prefix + "Y");
              float[] yVector = (float[]) yDataset.getData();
              Dataset zDataset = (Dataset)h5file.get(prefix + "Z");
              float[] zVector = (float[]) zDataset.getData();
//              System.out.println("Dataset " + dataset.getFullName() + ", datatype: " + dataset.getDatatype().getDatatypeDescription());
              dataset.init();

              int rank = dataset.getRank();

              int[] selectedIndex = dataset.getSelectedIndex();
 //             System.out.println("Selected index: ");
 //             for (int j = 0; selectedIndex != null && j < selectedIndex.length; j++)
 //               System.out.print(selectedIndex[j] + ", ");
 //             System.out.println();
              long[] dims = dataset.getDims();
 //             System.out.println("Dims: ");
 //             for (int j = 0; dims != null && j < dims.length; j++)
 //               System.out.print(dims[j] + ", ");
 //             System.out.println();
              long[] selectedDims = dataset.getSelectedDims();
 //             System.out.println("Selected dims: ");
 //             for (int j = 0; selectedDims != null && j < selectedDims.length; j++)
 //               System.out.print(selectedDims[j] + ", ");
 //             System.out.println();
              long[] startDims = dataset.getStartDims();
 //             System.out.println("Start dims: ");
 //             for (int j = 0; startDims != null && j < startDims.length; j++)
 //               System.out.print(startDims[j] + ", ");
 //             System.out.println();
              long[] stride = dataset.getStride();
 //             System.out.println("Stride: ");
 //             for (int j = 0; stride != null && j < stride.length; j++)
 //               System.out.print(stride[j] + ", ");
 //             System.out.println();

              // select dim1 and dim2 as 2D data for display and slice through dim0
              selectedIndex[0] = 1;
              selectedIndex[1] = 2;
              selectedIndex[2] = 0;

              // reset the selection arrays
              for (int ij = 0; ij < rank; ij++) {
                startDims[ij] = 0;
                stride[ij] = 1;
              }

              selectedDims[0] = 1;
              selectedDims[1] = dims[1] / stride[1];
              selectedDims[2] = dims[2] / stride[2];

              for (int phiSlice = 0; phiSlice < dims[0]; phiSlice++) {
                startDims[0] = phiSlice;
                DiffrDataFile datafile = addDiffrDatafile(token);
                boolean atmpB = datafile.isAbilitatetoRefresh;
                datafile.isAbilitatetoRefresh = false;
                datafile.setOmega(token);
                datafile.setChi(token);
                datafile.setPhi(token);

                float[] data = (float[]) dataset.getData();

                float[][] intensity = new float[(int)dims[1]][(int)dims[2]];
                int k = 0;
                for (int i2 = 0; i2 < dims[1]; i2++)
                  for (int i3 = 0; i3 < dims[2]; i3++)
                    intensity[i2][i3] = data[k++];

                // now we add a new datafile


              }

            }
          }
        }

//      printNode(rootNode, "    ");
      }

      h5file.close();

    }
    catch (Exception ex)
    {
        System.out.println(ex);
    }
*/

    return loadSuccessfull;
  }


}
