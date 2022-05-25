package it.unitn.ing.wizard.HIPPOWizard;

import it.unitn.ing.rista.util.MaudPreferences;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

public class DetectorPanel extends JPanel {

	private JCheckBox checkGroupDatasets;

  JTextField[] minmaxDTF = null;
  JCheckBox[] banksCB = null;
  static double[] minmax_std = {0.0,0.0};

  public static final String BANK_ENABLED = " enabled";

  public static final String[] BANK_DMIN_DMAX = {", d min", ", d max"};

  HIPPOdata data;
  int maxBanks = 0;

  /**
	 * Create the panel
   * @param data the data where to store the temporary information on banks
	 */
	public DetectorPanel(HIPPOdata data) {
		super();
    this.data = data;
    maxBanks = data.mbank.size();
    initGUI();
		resetToDefaults();
    for (int i = 0; i < maxBanks; i++)
      banksCB[i].setSelected(ConfigData.getPropertyValue(
          ((HIPPOBank) data.mbank.elementAt(i)).name + ConfigData.BANK_ENABLED, "true").equalsIgnoreCase("true"));
    int tmpIndex = 0;
    for (int i = 0; i < maxBanks; i++)
      for (int j = 0; j < 2; j++)
        minmaxDTF[tmpIndex++].setText(ConfigData.getPropertyValue(
          ((HIPPOBank) data.mbank.elementAt(i)).name + ConfigData.BANK_DMIN_DMAX[j],
            Float.toString((float) minmax_std[j])));
	}

	public void addActionListener(ActionListener listener) {
    for (int i = 0; i < maxBanks; i++)
      banksCB[i].addActionListener(listener);
	}
	
	private void initGUI() {

		setLayout(new BorderLayout());
		final JPanel panel_11 = new JPanel();
		panel_11.setLayout(new BorderLayout());
		panel_11.setBorder(new TitledBorder(null, "", TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION,
        null, null));
		add(panel_11, BorderLayout.SOUTH);

		checkGroupDatasets = new JCheckBox();
		checkGroupDatasets.setText("Treat rotations as separate datasets/instruments");
		panel_11.add(checkGroupDatasets);
    checkGroupDatasets.setEnabled(true);
    checkGroupDatasets.setToolTipText(
        "Check this if it is possible that different datafiles have different intensities or errors");
    checkGroupDatasets.setSelected(MaudPreferences.getBoolean("hippoWizard.datafiles_different_datasets", false));

    final JPanel panelBanks = new JPanel();
		panelBanks.setBorder(new TitledBorder(null, "Select banks to use", TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION, null, null));
		panelBanks.setLayout(new BoxLayout(panelBanks, BoxLayout.Y_AXIS));
		add(panelBanks);

    banksCB = new JCheckBox[maxBanks];
    minmaxDTF = new JTextField[maxBanks * 2];

    for (int i = 0; i < maxBanks; i++) {
      JPanel panel_6 = new JPanel();
//		  panel_6.setPreferredSize(new Dimension(460, 50));
		  panelBanks.add(panel_6);

		  banksCB[i] = new JCheckBox(((HIPPOBank) data.mbank.elementAt(i)).name + BANK_ENABLED);
//		  banksCB[i].setPreferredSize(new Dimension(150, 24));
		  panel_6.add(banksCB[i]);

//		  JPanel panel = new JPanel();
//		  panel_6.add(panel);

		  panel_6.add(new JLabel(BANK_DMIN_DMAX[0]));
		  minmaxDTF[i * 2] = new JTextField(6);
//		  minmaxDTF[i * 2].setPreferredSize(new Dimension(60, 20));
		  panel_6.add(minmaxDTF[i * 2]);

      panel_6.add(new JLabel(BANK_DMIN_DMAX[1]));
      minmaxDTF[i * 2 + 1] = new JTextField(6);
//      minmaxDTF[i * 2 + 1].setPreferredSize(new Dimension(60, 20));
      panel_6.add(minmaxDTF[i * 2 + 1]);
    }

		final JPanel panel_2 = new JPanel();
		panelBanks.add(panel_2);

		final JButton resetToDefaultsButton = new JButton("Reset to defaults");
		resetToDefaultsButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				resetToDefaults();
			}
		});
		panel_2.add(resetToDefaultsButton);
	}
	
	public boolean enabledToContinue() {
    boolean isSelected = false;
    for (int i = 0; i < maxBanks; i++)
      if (!((HIPPOBank) data.mbank.elementAt(i)).available) {
        banksCB[i].setEnabled(false);
        minmaxDTF[i*2].setEnabled(false);
        minmaxDTF[i*2+1].setEnabled(false);
      } else
        isSelected = isSelected || banksCB[i].isSelected();
    return isSelected;
  }
	
	private void resetToDefaults() {
    for (int i = 0; i < maxBanks; i++)
      banksCB[i].setSelected(true);
    for (int i = 0; i < maxBanks; i++)
      for (int j = 0; j < 2; j++)
        minmaxDTF[i*2+j].setText(Float.toString((float) minmax_std[j]));
	}
	
	public void saveData() {
    for (int i = 0; i < maxBanks; i++) {
		  ((HIPPOBank) data.mbank.elementAt(i)).enabled = banksCB[i].isSelected();
      ((HIPPOBank) data.mbank.elementAt(i)).dSpacingMin = minmaxDTF[i*2].getText();
      ((HIPPOBank) data.mbank.elementAt(i)).dSpacingMax = minmaxDTF[i*2+1].getText();
      ConfigData.setPropertyValue(((HIPPOBank) data.mbank.elementAt(i)).name + ConfigData.BANK_ENABLED,
          Boolean.toString(banksCB[i].isSelected()));
    }
		data.groupDatasetsByRotation = checkGroupDatasets.isSelected();
    MaudPreferences.setPref("hippoWizard.datafiles_different_datasets", data.groupDatasetsByRotation);
    for (int i = 0; i < maxBanks; i++)
      for (int j = 0; j < 2; j++)
        ConfigData.setPropertyValue(((HIPPOBank) data.mbank.elementAt(i)).name + ConfigData.BANK_DMIN_DMAX[j],
            minmaxDTF[i*2+j].getText());
	}

}
