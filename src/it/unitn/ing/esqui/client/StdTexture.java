package it.unitn.ing.esqui.client;

/** StdTexture.java
 * <br>
 * Title:			<b>ESQUI Client Standard Texture</b>
 * </br>
 * Description:		Methods to set up the stadanrd texture analysis
 for the ESQUI client
 * @author:			Leonardo Cont, December 2000
 * @revision:		January 2001
 * @comment:		none
 */

public class StdTexture extends ClientAnalysis {

  final float[] zDEFAULT_StdText = {noMove, noMove, noMove};
  final float[] yDEFAULT_StdText = {noMove, noMove, noMove};
  final float[] xDEFAULT_StdText = {noMove, noMove, noMove};
  final float[] phiDEFAULT_StdText = {0, 355, 5};
  final float[] chiDEFAULT_StdText = {0, 50, 5};
  final float[] omegaDEFAULT_StdText = {10, 10, noMove};
  final float[] twothetaDEFAULT_StdText = {10, 10, noMove};
  final int ctDEFAULT_StdText = 120;
  final float kVDEFAULT_StdText = 40;
  final float mADEFAULT_StdText = 30;
  final int channelsDEFAULT_StdText = 4096;
  final int movefilterDEFAULT_StdText = 0;
  final String measuretypeDEFAULT_StdText = "Stereographic_Scan";

  public StdTexture(String title) {
    super(title);
    setDefaultParameters();
  }

  public void setDefaultParameters() {
    setParameters(z, zDEFAULT_StdText);
    setParameters(y, yDEFAULT_StdText);
    setParameters(x, xDEFAULT_StdText);
    setParameters(phi, phiDEFAULT_StdText);
    setParameters(chi, chiDEFAULT_StdText);
    setParameters(omega, omegaDEFAULT_StdText);
    setParameters(twotheta, twothetaDEFAULT_StdText);
    ct = ctDEFAULT_StdText;
    kV = kVDEFAULT_StdText;
    mA = mADEFAULT_StdText;
    channels = channelsDEFAULT_StdText;
    movefilter = movefilterDEFAULT_StdText;
    measuretype = measuretypeDEFAULT_StdText;
  }

  public String getAnalysisFile() {
    super.getAnalysisFile();
    StringBuffer tmpBuffer = new StringBuffer("US " + Client.getUser());
    setNewLine(tmpBuffer);
    for (int i = 0; i < separator; i++) {
      tmpBuffer.append(esquigoCommands[i]);
      setBlankSpace(tmpBuffer);
      tmpBuffer.append(allOptionsVector.elementAt(i));
      setNewLine(tmpBuffer);
    }

    int j = 0;
    while (j < 3) {
      tmpBuffer.append(esquigoCommands[separator]);
      for (int i = j++; i < allMovementsVector.size(); i = i + 3) {
        setBlankSpace(tmpBuffer);
        if (((Float) allMovementsVector.elementAt(i)).intValue() == noMove) {
          tmpBuffer.append("-" + ",");
        } else {
          tmpBuffer.append(allMovementsVector.elementAt(i) + ",");
        }
      }
      setNewLine(tmpBuffer);
      separator++;
    }

    for (int i = separator; i < esquigoCommands.length; i++) {
      tmpBuffer.append(esquigoCommands[i]);
      setNewLine(tmpBuffer);
    }

    return tmpBuffer.toString();
  }
}
