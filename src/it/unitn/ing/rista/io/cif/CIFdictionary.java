/*
 * @(#)CIFdictionary.java created 2/07/2001 Casalino
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.io.cif;

import java.io.*;
import java.util.*;
import java.net.*;

import it.unitn.ing.rista.util.*;

/**
 *  The CIFdictionary is a utility class to store CIF words
 *
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:58 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class CIFdictionary {

  public static final String loopDecl = "loop_";
  public static final String dataDecl = "data_";
  public static final String refln_h = "_refln_index_h";
  public static final String refln_k = "_refln_index_k";
  public static final String refln_l = "_refln_index_l";
  public static final String refln_FsquaredMeas = "_refln_F_squared_meas";
  public static final String refln_FsquaredCalc = "_refln_F_squared_calc";
  public static final String refln_FsquaredEsd = "_refln_F_squared_sigma";
  public static final String refln_FMeas = "_refln_F_meas";
  public static final String refln_FCalc = "_refln_F_calc";
  public static final String refln_FEsd = "_refln_F_sigma";
  public static final String refln_multiplicity = "_refln_symmetry_multiplicity";
  public static final String refln_wavelength = "_refln_wavelength";
  public static final String refln_dspacing = "_refln_d_spacing";
  public static final String odf_values = "_rita_wimv_odf_values";
  public static final String atomMap_values = "_rita_electron_map_values";
	public static final String texture_factor_meas = "_rita_texture_factor_meas";
	public static final String texture_factor_calc = "_rita_texture_factor_calc";
	public static final String texture_points_number = "_rita_texture_points_number";
	public static final String texture_factor_point = "_rita_texture_point_index";

}
