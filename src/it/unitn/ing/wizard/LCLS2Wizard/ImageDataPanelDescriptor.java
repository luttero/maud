package it.unitn.ing.wizard.LCLS2Wizard;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import it.unitn.ing.wizard.WizardPanelDescriptor;

public class ImageDataPanelDescriptor extends WizardPanelDescriptor implements PropertyChangeListener  {

	public static final String IDENTIFIER = "Data files";
	private ImagesPanel panel;

	public ImageDataPanelDescriptor(LCLS2data data) {
		panel = new ImagesPanel(data);
		panel.addPropertyChangeListener(this);
		setPanelDescriptorIdentifier(IDENTIFIER);
		setPanelComponent(panel);
	}

	public Object getNextPanelDescriptor() {
		return FINISH;
	}

	public Object getBackPanelDescriptor() {
		return null;
	}

	public void aboutToDisplayPanel() {
		checkPanelData();
	}

	public void aboutToHidePanel() {
		panel.saveData();
	}

	public void propertyChange(PropertyChangeEvent evt) {
		checkPanelData();
	}

	private void checkPanelData() {
		if (panel.enabledToContinue())
			getWizard().setNextFinishButtonEnabled(true);
		else
			getWizard().setNextFinishButtonEnabled(false);
	}


}
