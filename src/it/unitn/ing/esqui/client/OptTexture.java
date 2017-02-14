package it.unitn.ing.esqui.client;

/** OptTexture.java
 * <br>
 * Title:			<b>ESQUI Client Hexagonal Texture Analysis</b>
 * </br>
 * Description:		Methods to set up an hexagonal optimized texture
 *					analysis for the ESQUI client
 * @author:			Leonardo Cont, December 2000
 * @revision:		February 2001
 * @comment:		none
 */

public class OptTexture extends ClientAnalysis {

  public OptTexture(String title) {
    super(title);
//		setDefaultParameters();
  }

  public String getAnalysisFile() {
    StringBuffer tmpBuffer = new StringBuffer("US " + Client.getUser());
    setNewLine(tmpBuffer);
    tmpBuffer.append("KV 40.0");
    setNewLine(tmpBuffer);
    tmpBuffer.append("MA 30.0");
    setNewLine(tmpBuffer);
    tmpBuffer.append("MF 0");
    setNewLine(tmpBuffer);
    tmpBuffer.append("CT 10");
    setNewLine(tmpBuffer);
    tmpBuffer.append("NC 4096");
    setNewLine(tmpBuffer);
    tmpBuffer.append("MT Aquire");
    setNewLine(tmpBuffer);
    tmpBuffer.append("MV 10.0, 10.0, 0.0, 0.0, -, -, -");
    setNewLine(tmpBuffer);
    tmpBuffer.append("SA");
    setNewLine(tmpBuffer);
    tmpBuffer.append("MV 10.0, 10.0, 5.0, 0.0, -, -, -");
    setNewLine(tmpBuffer);
    tmpBuffer.append("SA");
    setNewLine(tmpBuffer);
    tmpBuffer.append("MV 10.0, 100.0, 10.0, 0.0, -, -, -");
    setNewLine(tmpBuffer);
    tmpBuffer.append("SA");
    setNewLine(tmpBuffer);
    tmpBuffer.append("MV 10.0, 100.0, 15.0, 0.0, -, -, -");
    setNewLine(tmpBuffer);
    tmpBuffer.append("SA");
    setNewLine(tmpBuffer);

    return tmpBuffer.toString();
  }
}
