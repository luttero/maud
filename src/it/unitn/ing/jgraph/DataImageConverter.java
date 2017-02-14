/*
 * @(#)DataImageConverter.java created 7/11/2000 Mesiano
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.jgraph;

import java.awt.*;
import java.awt.image.*;

/**
 * The DataImageConverter is a ....
 *
 * @version $Revision: 1.2 $, $Date: 1970/01/04 19:14:41 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class DataImageConverter {

  public static IndexColorModel makeDefaultColorModel() {
    byte[] rLUT = new byte[256];
    byte[] gLUT = new byte[256];
    byte[] bLUT = new byte[256];
    for (int i = 0; i < 256; i++) {
      rLUT[i] = (byte) i;
      gLUT[i] = (byte) i;
      bLUT[i] = (byte) i;
    }
    return new IndexColorModel(8, 256, rLUT, gLUT, bLUT);
  }

  /** Get the pixels from 3 byte (reg, green, blue). */
  public static int getRGB(int R, int G, int B) {
    return 0xff000000 | ((R & 0xff) << 16) | ((G & 0xff) << 8) | B & 0xff;
  }

  public static Color getPixelColor(int pixels) {
    int r = (pixels & 0xff0000) >> 16;
    int g = (pixels & 0xff00) >> 8;
    int b = pixels & 0xff;
    return new Color(r, g, b);
  }

  public static int[] defineColorByIntensity(float z, float min, float max, boolean grayScale) {

    if (min < 0) {
      z += Math.abs(min);
      max += Math.abs(min);
      min = 0;
    }

    float zredblue = (max + min) / 2f;
    float zgreen1 = (max - min) / 6f + min;
    float zgreen2 = 5f * (max - min) / 6f + min;

    float[] p_color = new float[3];
    if (z < min)
      z = min;
    if (z > max)
      z = max;
    if (grayScale) {
      float value = (z - min) / (max - min);
      p_color[0] = value;						// red
      p_color[1] = value;						// green
      p_color[2] = value;						// blue
    } else {
      if (z < zgreen2 && z >= zredblue) {
        p_color[1] = Math.abs((z - zredblue) / (zgreen2 - zredblue));
        p_color[0] = (1.f - p_color[1]);
        p_color[2] = 0f;
      } else if (z > zgreen1 && z < zredblue) {
        p_color[2] = 1f - Math.abs((z - zgreen1) / (zredblue - zgreen1));
        p_color[1] = 0f;
        p_color[0] = (1.f - p_color[2]);
      } else if (z >= zgreen2) {
        p_color[1] = 1f;
        p_color[0] = Math.abs((z - zgreen2) / (max - zgreen2));
        p_color[2] = Math.abs((z - zgreen2) / (max - zgreen2));
      } else if (z <= zgreen1) {
        p_color[2] = Math.abs((z - min) / (zgreen1 - min));
        p_color[1] = 0f;
        p_color[0] = 0f;
      }
    }

    int[] i_color = new int[3];
    for (int ki = 0; ki < 3; ki++) {
      i_color[ki] = (int) (p_color[ki] * 255);
      if (i_color[ki] > 255)
        i_color[ki] = 255;
      if (i_color[ki] < 0)
        i_color[ki] = 0;
    }

    return i_color;
  }

}

