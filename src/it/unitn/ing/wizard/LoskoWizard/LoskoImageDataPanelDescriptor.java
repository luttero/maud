package it.unitn.ing.wizard.LoskoWizard;

import it.unitn.ing.wizard.WizardPanelDescriptor;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class LoskoImageDataPanelDescriptor extends WizardPanelDescriptor implements PropertyChangeListener {

	public static final String IDENTIFIER = "Data files";
	private LoskoImagePanel panel;

	public LoskoImageDataPanelDescriptor(LoskoData data) {
		panel = new LoskoImagePanel(data);
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
